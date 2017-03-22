unit myMangaReaderCMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/changeMangaList?type=text') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//li/a', ALinks, ANames);
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
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="boxed"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h2[@class="widget-title"]');
          if Module.Website = 'MangaDenizi' then
            status := MangaInfoStatusIfPos(XPathString('//dt[.="Durum:"]/following-sibling::dd[1]'), 'Devam Ediyor', 'Tamamlandı')
          else
            status := MangaInfoStatusIfPos(XPathString('//dt[.=("Status","Estado")]/following-sibling::dd[1]'));
          authors := XPathStringAll('//dt[.=("Author(s)","Yazar & Çizer:","Autor(es)")]/following-sibling::dd[1]/string-join(*,", ")');
          artists := XPathStringAll('//dt[.="Artist(s)"]/following-sibling::dd[1]/string-join(*,", ")');
          genres := XPathStringAll('//dt[.=("Categories","Kategoriler:","Categorías")]/following-sibling::dd[1]/string-join(*,", ")');
          summary := XPathString('//div[@class="well"]/p');
          for v in XPath('//ul[@class="chapters"]/li/*[self::h5 or self::h3]') do
          begin
            chapterLinks.Add(XPathString('a/@href', v));
            chapterName.Add(XPathString('normalize-space(.)', v));
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
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//div[@id="all"]/img/@data-src', PageLinks);
        finally
          Free;
        end;
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
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('Komikid', 'http://www.komikid.com');
  AddWebsiteModule('MangaDenizi', 'http://www.mangadenizi.com');
  AddWebsiteModule('MangaDoor', 'http://mangadoor.com');
  AddWebsiteModule('MangaID', 'http://mangaid.co');
  AddWebsiteModule('FallenAngelsScans','http://manga.fascans.com');
end;

initialization
  RegisterModule;

end.
