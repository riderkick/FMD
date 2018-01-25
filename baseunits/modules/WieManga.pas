unit WieManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, MangaFoxWatermark, FMDVars;

implementation

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
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
          title := XPathString('//div[@class="bookmessagebox"]/h1/substring-before(., " Manga")');
          coverLink := XPathString('//div[@class="bookfrontpage"]/a/img/@src');
          summary := XPathString('//h4[text()="Beschreibung"]/following-sibling::text()');
          genres := XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Genre")]/a');
          authors := XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Autor")]/a');
          artists := XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Zeichner")]/a');
          status := MangaInfoStatusIfPos(XPathString('//div[@class="bookmessgae"]//dd[contains(span/text(), "Status")]/a'), 'ongoing', 'finished');
          XPathHREFAll('//div[@class="chapterlist"]/table//td[@class="col1"]/a', chapterLinks, chapterName);
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
  url: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  url := MaybeFillHost(Module.RootURL, AURL);
  url := TrimRightChar(url, ['/']) + '-1.html';
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(url) then begin
      Result := True;
      PageNumber := XPathCount('(//select[@id="page"])[1]/option', Document);
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  baseurl, url, s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  baseurl := MaybeFillHost(Module.RootURL, AURL);
  url := TrimRightChar(baseurl, ['/']) + '-' + IncStr(DownloadThread.WorkId) + '.html';
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    Headers.Values['Referer'] := baseurl;
    if GET(url) then begin
      Result := True;
      s := XPathString('//img[@id="comicpic"]/@src', Document);
      PageLinks[DownloadThread.WorkId] := s;
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
  v: IXQValue;
  i, x: Integer;
begin
  Result := NET_PROBLEM;
  if Module.CurrentDirectoryIndex = 0 then
    s := '0-9'
  else
    s := ALPHA_LIST_UP[Module.CurrentDirectoryIndex + 1];
  if MangaInfo.FHTTP.GET(Module.RootURL + '/category/' + s + '_'+ IncStr(AURL) + '.html') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do try
      i := 1;
      for v in XPath('//*[@class="booklist"]//span[@class="pagetor"]//text()') do begin
        x := StrToIntDef(v.toString, 1);
        if x > i then i := x;
      end;
      updateList.CurrentDirectoryPageNumber := i;
      XPathHREFtitleAll('//*[@class="booklist"]/table//dl/dd/a[1]', ALinks, ANames);
    finally
      Free;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'WieManga';
    RootURL := 'https://www.wiemanga.com';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    TotalDirectory := Length(ALPHA_LIST_UP);
  end;
end;

initialization
  RegisterModule;

end.
