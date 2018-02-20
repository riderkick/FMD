unit SenManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, URIParser;

implementation

const
  dirurl = '/directory';

var
  cookie: String;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(SeparateRight(
          XPathString('//*[@id="Navigation"]//ul/li[last()]/a/@href'), '/page/'), 1);
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
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s += '/page/' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@id="search-results"]/*[contains(@class, "media_box")]/*[contains(@class, "media-body")]/a') do
        begin
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
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="thumbnail"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@class="title"]');
          genres := XPathStringAll('//ul[@class="series-info"]/li[contains(., "Categories")]/a');
          authors := XPathStringAll('//ul[@class="series-info"]/li[contains(., "Author")]/a');
          artists := XPathStringAll('//ul[@class="series-info"]/li[contains(., "Artist")]/a');
          status := MangaInfoStatusIfPos(XPathString('//ul[@class="series-info"]/li[contains(., "Status")]/a'));
          summary := XPathString('//*[@itemprop="description"]');
          XPathHREFAll('//div[@class="title" and contains(., "Chapters")]/following-sibling::div/div/a', chapterLinks, chapterName);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
  uri: TURI;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      cookie := Cookies.Text;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('//select[@name="page"]/option').Count;
          if PageNumber > 0 then begin
            s := MaybeFillHost(Module.RootURL, XPathString('//img[@id="picture"]/@src'));
            uri := ParseURI(s);
            if (Pos('/raw-viewer.php', LowerCase(uri.Path)) > 0) and (Pos('page=1', LowerCase(uri.Params)) > 0) then
              while PageLinks.Count < PageNumber do begin
                uri.Params := ReplaceString(uri.Params, 'page=1', 'page=' + IncStr(PageLinks.Count));
                PageLinks.Add(EncodeURI(uri));
              end
            else if (Pos('/viewer/', LowerCase(uri.Path)) > 0) and (uri.Document = '1') then
              while PageLinks.Count < PageNumber do begin
                uri.Document := IncStr(PageLinks.Count);
                PageLinks.Add(EncodeURI(uri));
              end;
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function BeforeDownloadImage(const DownloadThread: TDownloadThread;
  var AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container do
    if CurrentDownloadChapterPtr < ChapterLinks.Count then begin
      DownloadThread.FHTTP.Headers.Values['Referer'] :=
        ' ' + FillHost(Module.RootURL, ChapterLinks[CurrentDownloadChapterPtr]);
      DownloadThread.FHTTP.Cookies.Text := cookie;
      Result := True;
    end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
  uri: TURI;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    uri := ParseURI(FillHost(Module.RootURL, AURL));
    uri.Document := IncStr(DownloadThread.WorkId);
    if GET(EncodeURI(uri)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := MaybeFillHost(Module.RootURL, XPathString('//img[@id="picture"]/@src'));
          if s <> '' then
            PageLinks[DownloadThread.WorkId] := s;
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'SenManga';
    RootURL := 'http://www.senmanga.com';
    Category := 'English';
    Settings.MaxTaskLimit := 1;
    Settings.MaxConnectionLimit := 4;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.

