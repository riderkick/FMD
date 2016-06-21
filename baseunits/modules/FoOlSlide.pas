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
  dirurlreader = '/reader/directory/';
  dirurlfoolslide = '/foolslide/directory/';
  dirurlslide = '/slide/directory/';
  dirurlonline = '/online/directory/';

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  if Module.Website = 'YoManga' then
    Result := Cloudflare.GETCF(AHTTP, AURL, yomangacookies, yomangalockget)
  else
    Result := AHTTP.GET(AURL);
end;

function GetDirUrl(const AWebsite: String): String;
begin
  if (AWebsite = 'YoManga') or
     (AWebsite = 'GoManga') then
    Result := dirurlreader
  else
  if AWebsite = 'OneTimeScans' then
    Result := dirurlfoolslide
  else
  if (AWebsite = 'DejameProbar') or
     (AWebsite = 'MenudoFansub') or
     (AWebsite = 'NeoProjectScan') or
     (AWebsite = 'SantosScan') or
     (AWebsite = 'SolitarioNoFansub') then
    Result := dirurlslide
  else
  if (AWebsite = 'Pzykosis666HFansub') or
     (AWebsite = 'SeinagiFansub') then
    Result := dirurlonline
  else
    Result := dirurl;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
  p: Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + GetDirUrl(Module.Website), Module) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        with TRegExpr.Create do
          try
            Expression := '/(\d+)/$';
            for v in XPath('//*[@class="next"]/a/@href') do
            begin
              s := v.toString;
              Exec(s);
              if SubExprMatchCount > 0 then
              begin
                p := StrToIntDef(Match[1], -1);
                if p > Page then
                  Page := p;
              end;
            end;
          finally
            Free;
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
  s := Module.RootURL + GetDirUrl(Module.Website);
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
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
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    s := AURL;
    if DownloadThread.WorkId > 0 then
      s := AppendURLDelim(s) + 'page/' + IncStr(DownloadThread.WorkId);
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, s), Module) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] := XPathString('//div[@id="page"]//img/@src');
        finally
          Free;
        end;
    end;
  end;
end;

function DownloadImageWithCookie(const DownloadThread: TDownloadThread;
  const AURL, APath, AName: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  if GETWithCookie(DownloadThread.FHTTP, AURL, Module) then
    SaveImageStreamToFile(DownloadThread.FHTTP, APath, AName);
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
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
  with AddWebsiteModule('YoManga', 'http://yomanga.co') do
  begin
    MaxTaskLimit := 1;
    MaxConnectionLimit := 4;
    OnDownloadImage := @DownloadImageWithCookie;
  end;
  AddWebsiteModule('RawYoManga', 'http://raws.yomanga.co');
  AddWebsiteModule('GoManga', 'http://gomanga.co');
  AddWebsiteModule('OneTimeScans', 'http://otscans.com');
  AddWebsiteModule('SenseScans', 'http://reader.sensescans.com');

  //es-san
  AddWebsiteModule('DangoOnlineNoFansub', 'http://lector.dangolinenofansub.com');
  AddWebsiteModule('DejameProbar', 'http://dejameprobar.es');
  AddWebsiteModule('HoshinoFansub', 'http://manga.animefrontline.com');
  AddWebsiteModule('MangaWorksFansub', 'http://lector.mangaworksfansub.net');
  AddWebsiteModule('MasterPieceScans', 'http://reader.manga2me.net');
  AddWebsiteModule('MenudoFansub', 'http://www.menudo-fansub.com');
  AddWebsiteModule('NeoProjectScan', 'http://npscan.scans-es.com');
  AddWebsiteModule('Pzykosis666HFansub', 'http://pzykosis666hfansub.com');
  AddWebsiteModule('R15TeamScanlation', 'http://www.r15team.com');
  AddWebsiteModule('SantosScan', 'http://santosfansub.com');
  AddWebsiteModule('SeinagiFansub', 'http://seinagi.org');
  AddWebsiteModule('SeinagiAdultoFansub', 'http://adulto.seinagi.org');
  AddWebsiteModule('SolitarioNoFansub', 'http://snf.mangaea.net');
end;

initialization
  InitCriticalSection(yomangalockget);
  RegisterModule;

finalization
  DoneCriticalsection(yomangalockget);

end.
