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
var
  dirurl: String;
begin
  Result := NET_PROBLEM;
  dirurl := '/manga-list/';
  if Module.Website = 'MangaShiro' then dirurl := '/daftar-manga/';
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="soralist"]//a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="imgdesc"]/img/@src'));
          if title = '' then title := XPathString('//h1[@itemprop="name"]');
          authors := XPathString('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")');
          genres := XPathString('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")');
          status := MangaInfoStatusIfPos(XPathString('//div[@class="listinfo"]//li[starts-with(.,"Status")]'));
          summary := XPathString('//*[@class="desc"]/string-join(.//text(),"")');
          XPathHREFAll('//div[@class="cl"]//li/span[1]/a', chapterLinks, chapterName);
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
      XPathStringAll('//*[@id="readerarea"]//img/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(const AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := 'Indonesian';
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaShiro', 'http://mangashiro.net');
  AddWebsiteModule('Subapics', 'http://subapics.com');
  AddWebsiteModule('MangaKita', 'http://www.mangakita.net');
  AddWebsiteModule('Mangavy', 'https://mangavy.com');
end;

initialization
  RegisterModule;

end.
