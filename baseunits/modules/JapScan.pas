unit JapScan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/mangas/') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//*[@id="liste_mangas"]/div[@class="row"]/div[@class="cell"]/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          title := XPathString('//title');
          if Pos(' | Japscan.Com', title) <> 0 then
            title := GetString(title, 'Lecture En Ligne Des Chapitres ', ' | Japscan.Com');
          authors := XPathString('//div[@class="table"]/div[@class="row"]/div[1]');
          genres := XPathString('//div[@class="table"]/div[@class="row"]/div[4]');
          summary := XPathString('//div[@id="synopsis"]/string-join(text(),codepoints-to-string(10))');
          status := MangaInfoStatusIfPos(XPathString('//div[@class="table"]/div[@class="row"]/div[6]'), 'En Cours', 'Terminé');
          for v in XPath('//*[@id="liste_chapitres"]/ul/li/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            s := v.toString;
            if Pos('[email protected]', s) <> 0 then
              s := title + ' ' + XPathString('text()[2]', v)
            else
            if Pos('Scan ', s) = 1 then
              Delete(s, 1, 5);
            chapterName.Add(s);
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
  dataimg, imgurl: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      XPathStringAll('//select[@id="pages"]/option/@data-img', Document, PageLinks);
    end;
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  with DownloadThread, DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    if GET(AppendURLDelim(FillHost(Module.RootURL, ChapterLinks[CurrentDownloadChapterPtr])) + IncStr(WorkId) + '.html') then
      Result := GET(XPathString('//img[@id="image"]/@src', Document));
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Japscan';
    RootURL := 'http://www.japscan.com';
    Category := 'French';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
