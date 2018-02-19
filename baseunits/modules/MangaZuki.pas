unit MangaZuki;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga-list';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('(//ul[contains(@class,"pagination")]//a)[last()-1]/replace(@href,"^.*page=(\d+)$","$1")'), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '?page=' + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//*[@class="row"]//a[@class="chart-title"]', ALinks, ANames);
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
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//meta[@itemprop="photo"]/@content');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//div[@class="container"]/div[@class="row"]/div/h2');
          summary := XPathString('//h5[text()="Summary"]/following-sibling::*');
          XPathHREFAll('//ul[@class="chapters"]/li/h3/a', chapterLinks, chapterName);
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
begin
  Result := False;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//div[@id="all"]/img/@data-src') do
            PageLinks.Add(MaybeFillHost(Module.RootURL, Trim(v.toString)));
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL, ACategory: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := ACategory;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaZuki', 'https://mangazuki.co', 'English-Scanlation');
  AddWebsiteModule('MangaZukiRaws', 'https://raws.mangazuki.co', 'Raw');
end;

initialization
  RegisterModule;

end.
