unit GoodManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga-list';
  imagepath = '//div[@id="manga_viewer"]/a/img/@src';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//td/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  s: String;
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
          coverLink := XPathString('//img[@id="series_image"]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@class="right_col"]/h1');
          authors := XPathString('//div[@id="series_details"]/div/span[starts-with(.,"Authors:")]/following-sibling::text()');
          summary := XPathString('//div[@id="series_details"]/div//span[@id="full_notes"]');
          if summary = '' then
            summary := XPathString('//div[@id="series_details"]/div/span[starts-with(.,"Synopsis:")]/following-sibling::*');
          status := MangaInfoStatusIfPos(XPathString('//div[@id="series_details"]/div/span[starts-with(.,"Status:")]/following-sibling::text()'));
          genres := XPathString('//div[@id="series_details"]/div/span[starts-with(.,"Genres:")]/string-join(following-sibling::*,", ")');
          while true do
          begin
            XPathHREFAll('//div[@id="chapters"]/ul/li/a', chapterLinks, chapterName);
            if Thread.IsTerminated then Break;
            s := XPathString('//ul[@class="pagination"]/li/a[.="Next"]/@href');
            if (s <> '') and GET(MaybeFillHost(Module.RootURL, s)) then
              ParseHTML(Document)
            else
              Break;
          end;
          InvertStrings([chapterLinks,chapterName]);
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
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
      try
        PageNumber := XPath('//div[@id="asset_2"]/select[@class="page_select"]/option').Count;
      finally
        Free;
      end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    if GET(AppendURLDelim(FillHost(Module.RootURL, AURL)) +  IncStr(DownloadThread.WorkId)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] :=
            XPathString(imagepath);
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
      Category := 'English';
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('GoodManga', 'http://www.goodmanga.net');
  AddWebsiteModule('MangaBB', 'http://mangabb.co');
end;

initialization
  RegisterModule;

end.
