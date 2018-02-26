unit Mangakakalot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga_list?type=newest&category=all&state=all&page=';

function GetRedirectUrl(Document: TMemoryStream): String;
var s: String;
begin
  Result := '';
  s := XPathString('//script[contains(., "window.location.assign")]', Document);
  if s <> '' then
    Result := GetBetween('("', '")', s);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1') then begin
    Result := NO_ERROR;
    s := XPathString('//div[@class="group-page"]/a[contains(., "Last")]/@href', MangaInfo.FHTTP.Document);
    Page := StrToInt(RegExprGetMatch('page\=(\d+)', s, 1));
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL)) then begin
    Result := NO_ERROR;
    XPathHREFAll('//div[@class="truyen-list"]/div[@class="list-truyen-item-wrap"]/h3/a',
      MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      s := GetRedirectUrl(Document);
      if s <> '' then begin
        url := s;
        if not GET(url) then Exit;
      end;
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="manga-info-pic"]/img/@src'));
          if title = '' then title := XPathString('//ul[@class="manga-info-text"]/li/h1');
          if (Pos('email', title) > 0) and (Pos('protected', title) > 0) then
            title := Trim(XPathString('//title/substring-after(substring-before(., "Manga Online"), "Read")'));
          authors := XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Author")]/a');
          genres := XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Genre")]/a');
          status := MangaInfoStatusIfPos(XPathString('//ul[@class="manga-info-text"]/li[contains(., "Status")]'));
          summary := XPathStringAll('//div[@id="noidungm"]/text()', '');
          XPathHREFAll('//div[@class="chapter-list"]/div[@class="row"]/span/a', chapterLinks, chapterName);
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var s, url, path, host: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      s := GetRedirectUrl(Document);
      if s <> '' then begin
        SplitURL(s, @host, nil);
        SplitURL(url, nil, @path);
        if not GET(host + path) then Exit;
      end;
      Result := True;
      XPathStringAll('//div[@id="vungdoc"]/img/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := 'English';
      SortedList := True;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    end;
  end;

begin
  AddWebsiteModule('Mangakakalot', 'http://mangakakalot.com');
  AddWebsiteModule('Manganelo', 'http://manganelo.com');
end;

initialization
  RegisterModule;

end.

