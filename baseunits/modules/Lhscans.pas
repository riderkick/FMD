unit Lhscans;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//img[@class="thumbnail"]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := Trim(SeparateLeft(XPathString('//title'), '- Raw'));
          authors := XPathString('//ul[@class="manga-info"]/li[contains(., "Author")]//a');
          genres := XPathStringAll('//ul[@class="manga-info"]/li[contains(., "Genre")]//a');
          status := MangaInfoStatusIfPos(XPathString('//ul[@class="manga-info"]/li[contains(., "Status")]//a'));
          summary := XPathString('//h3[text()="Description"]/following-sibling::p');
          XPathHREFAll('//div[@id="tab-chapper"]//table/tbody/tr/td/a', chapterLinks, chapterName);
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
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      XPathStringAll('//img[@class="chapter-img"]/@src', Document, PageLinks);
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga-list.html?listType=allABC') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//span[@manga-slug]//a') do begin
          ANames.Add(Trim(SeparateLeft(v.toString, '- Raw')));
          ALinks.Add(v.toNode.getAttribute('href'));
        end;
      finally
        Free;
      end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Lhscans';
    RootURL := 'http://lhscans.com';
    Category := 'Raw';
    TotalDirectory := 1;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
  end;
end;

initialization
  RegisterModule;

end.

