unit MangaTube;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, synautil;

implementation

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if MangaInfo.FHTTP.GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
        try
          if title = '' then title := XPathString('//h1[@class="series-title"]');
          coverLink := XPathString('//div[contains(@class, "cover")]//img/@data-original');
          coverLink := MaybeFillHost(Module.RootURL, coverLink);
          summary := XPathString('//h4[text()="Beschreibung"]/following-sibling::p');
          if summary = '' then summary := XPathString('//h4[text()="Beschreibung"]/following-sibling::text()[1]');
          genres := XPathStringAll('//ul[contains(@class, "genre-list")]/li/a');
          authors := XPathStringAll('//ul[contains(@class, "series-details")]/li[contains(., "Autor")]/a');
          artists := XPathStringAll('//ul[contains(@class, "series-details")]/li[contains(., "Artist")]/a');
          status := Trim(XPathString('//ul[contains(@class, "series-details")]/li[contains(., "Status (Offiziell):")]/text()'));
          status := MangaInfoStatusIfPos(status, 'laufend', 'abgeschlossen');
          for v in XPath('//ul[contains(@class, "chapter-list")]/li/a[contains(@href, "read/")]') do
          begin
            chapterLinks.Add(v.toNode().getAttribute('href'));
            chapterName.Add(XPathString('concat(b, " ", span[1])', v));
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  imgpath, s: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//script[contains(., "img_path: ")]');
          imgpath := GetBetween('img_path: ''', ''',', s);
          s := GetBetween('pages: ', '}],', s) + '}]';
          ParseHTML(s);
          for v in XPath('json(*)().file_name') do begin
            s := v.toString;
            PageLinks.Add(imgpath + s);
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s:string;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/series/?filter=alphabetic') then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('//div[@id="series_list"]/@data-series-pages', MangaInfo.FHTTP.Document), 0);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  data: String;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  data := 'action=load_series_list_entries&parameter%5Bpage%5D=' + IncStr(AURL) +
       '&parameter%5Bletter%5D=&parameter%5Bsortby%5D=alphabetic&parameter%5Border%5D=asc';
  if MangaInfo.FHTTP.POST(Module.RootURL + '/ajax', data) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
    try
      XPathStringAll('json(*).success().manga_title', ANames);
      for v in XPath('json(*).success().manga_slug') do
          ALinks.Add(Module.RootURL + '/series/' + v.toString());
    finally
      Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaTube';
    RootURL := 'https://manga-tube.me';
    Category := 'German';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
