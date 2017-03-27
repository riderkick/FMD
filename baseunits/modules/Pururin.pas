unit Pururin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, RegExpr;

implementation

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL) then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('//*[contains(@class,"pager")]/a[last()]', MangaInfo.FHTTP.Document), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/browse/search/1/' + IncStr(AURL) + '.html') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="gallery-list"]/li//a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('.//*[@class="title"]//h2/text()[1]', v));
        end;
      finally
        Free;
      end;
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="gallery-cover"]//img/@src'));
          if title = '' then title := XPathString('//*[@class="gallery-info"]/h1');
          artists := XPathString('//*[@class="table-info"]//tr/td[starts-with(.,"Artist")]/following-sibling::td[1]/string-join(.//a,", ")');
          genres := XPathString('//*[@class="table-info"]/string-join(.//tr/td//a,", ")');
          XPathHREFAll('//*[@class="table-data"]//tr/td[2]/a', chapterLinks, chapterName);
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
      XPathStringAll('json(//script[contains(.,"chapter_page")]/substring-before(substring-after(.,"= "),";"))//chapter_image', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Pururin';
    RootURL := 'https://pururin.us';
    SortedList := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
