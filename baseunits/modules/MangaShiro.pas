unit MangaShiro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/daftar-manga/') then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="azindex"]//li/a', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="imganime"]//img/substring-before(substring-after(@style,"(''"),"'')")'));
          if title = '' then title := XPathString('//*[@class="infos9" and starts-with(.,"Judul")]/text()');
          authors := XPathString('//*[@class="infos9" and starts-with(.,"Produser")]/text()');
          genres := XPathString('//*[@class="infos9" and starts-with(.,"Genre")]/text()');
          status := MangaInfoStatusIfPos(XPathString('//*[@class="infos9" and starts-with(.,"Status")]/text()'));
          summary := XPathString('//*[@class="deskripsi"]/string-join(text(),"")');
          XPathHREFtitleAll('//*[@class="chapter-list"]//li[@class="anilist"]/div[2]/a', chapterLinks, chapterName);
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
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
      XPathStringAll('//*[@class="readmanga"]//img/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaShiro';
    RootURL := 'http://mangashiro.net';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
