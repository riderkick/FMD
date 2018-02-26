unit MangaIndo;

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
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga-list/') then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//li[@class="manga-list"]/a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@id="m-cover"]/img/@src'));
          if title = '' then title := XPathString('//*[@class="title"]/h2');
          authors := XPathString('//*[@id="m-author"]');
          artists := XPathString('//*[@id="m-artist"]');
          genres := XPathString('//*[@id="m-genre"]/string-join(a," ")');
          status := MangaInfoStatusIfPos(XPathString('//*[@id="m-status"]'));
          summary := XPathString('//*[@id="m-synopsis"]');
          XPathHREFAll('//ul[@class="lcp_catlist"]/li/a', chapterLinks, chapterName);
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
      XPathStringAll('//*[@class="entry-content"]//img/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaIndo';
    RootURL := 'http://mangaindo.web.id';
    Category := 'Indonesian';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
