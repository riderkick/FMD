unit FoOlSlide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, Cloudflare, RegExpr, synautil;

implementation

var
  yomangalockget: TRTLCriticalSection;
  yomangacookies: String;

const
  dirurl = '/directory/';
  yomangadirurl = '/reader/directory/';

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  if Module.Website = 'YoManga' then
    Result := Cloudflare.GETCF(AHTTP, AURL, yomangacookies, yomangalockget)
  else
    Result := AHTTP.GET(AURL);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if (Module.Website = 'YoManga') or
    (Module.Website = 'GoManga') then
    s := yomangadirurl
  else
    s := dirurl;
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + s, Module) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//div[@class="next"]/a[contains(text(),"Last")]/@href');
        if s <> '' then begin
          s := ReplaceRegExpr('.*/(\d+)/$', s, '$1', True);
          Page := StrToIntDef(s, 1);
        end;
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if (Module.Website = 'YoManga') or
    (Module.Website = 'GoManga') then
    s := yomangadirurl
  else
    s := dirurl;
  s := Module.RootURL + s;
  if AURL <> '0' then s += IncStr(AURL) + '/';
  if GETWithCookie(MangaInfo.FHTTP, s, Module) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="list series"]/div/div[@class="title"]/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do
  begin
    if GETWithCookie(MangaInfo.FHTTP, FillHost(Module.RootURL, AURL), Module) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="thumbnail"]/img/@src');
          if title = '' then title := XPathString('//h1[@class="title"]');
          authors := TrimLeftChar(XPathString(
            '//div[@class="info"]/*[contains(text(),"Author")]/following-sibling::text()[1]'), [':', ' ']);
          artists := TrimLeftChar(XPathString(
            '//div[@class="info"]/*[contains(text(),"Artist")]/following-sibling::text()[1]'), [':', ' ']);
          summary := TrimLeftChar(XPathString(
            '//div[@class="info"]/*[contains(text(),"Synopsis")]/following-sibling::text()[1]'), [':', ' ']);
          for v in XPath('//div[@class="list"]//div[@class="title"]/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            if v.toNode.getAttribute('title') <> '' then
              chapterName.Add(v.toNode.getAttribute('title'))
            else chapterName.Add(v.toString);
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL), Module) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('//div[@class="topbar_right"]//ul[@class="dropdown"]/li').Count;
          s := XPathString('//script[contains(.,"var pages")]');
          if s <> '' then begin
            s := GetBetween('var pages = ', ';', s);
            try
              ParseHTML(s);
              for v in XPath('json(*)()("url")') do
                PageLinks.Add(v.toString);
            except
            end;
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do
  begin
    s := AURL;
    if DownloadThread.workCounter > 0 then
      s := AppendURLDelim(s) + 'page/' + IncStr(DownloadThread.workCounter);
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, s), Module) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.workCounter] := XPathString('//div[@id="page"]//img/@src');
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;

  procedure AddWebsiteModule(AWebsite, ARootURL: String);
  begin
    with AddModule do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('Shoujosense', 'http://reader.shoujosense.com');
  AddWebsiteModule('YoManga', 'http://yomanga.co');
  AddWebsiteModule('RawYoManga', 'http://raws.yomanga.co');
  AddWebsiteModule('GoManga', 'http://gomanga.co');
end;

initialization
  InitCriticalSection(yomangalockget);
  RegisterModule;

finalization
  DoneCriticalsection(yomangalockget);

end.
