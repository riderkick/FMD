unit MangaHubRU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirurl = '/explore?search[sort]=date';

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then
    begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create(Document);
      try
        if title = '' then title := query.XPathString('//div[@class="b-group-head__desc"]/h1');
        coverLink := MaybeFillHost(Module.RootURL, query.XPathString('//div[@class="manga-section-image__img"]/img/@src'));
        authors := query.XPathString('//a[@itemprop="author"]');
        summary:= query.XPathString('//div[@itemprop="description"]');
        genres := query.XPathStringAll('//div[@class="b-dtl-desc__labels"]/a');
        query.XPathHREFAll('//div[@class="b-ovf-table"]/div[@class="b-ovf-table__elem"]/a', chapterLinks, chapterName);
        InvertStrings([chapterLinks, chapterName]);
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//div[contains(@class, "b-reader__full")]/@data-js-scans');
          s := ReplaceString(s, '\/', '/');
          if s <> '' then begin
            ParseHTML(s);
            for v in XPath('json(*)()("src")') do begin
              s := ReplaceString(v.toString, '\/', '/');
              PageLinks.Add(MaybeFillHost(Module.RootURL, s));
            end;
          end;
        finally
          Free;
        end;
  end;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//div[@class="pagination"]/ul/li[last()-1]/a');
        Page := StrToIntDef(s, 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '&page=' + IncStr(AURL)) then begin
    Result := NO_ERROR;
    XPathHREFAll('//div[@class="list-element__name"]/a',
      MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaHubRU';
    RootURL := 'https://mangahub.ru';
    Category := 'Russian';
    SortedList := True;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
  end;
end;

initialization
  RegisterModule;

end.

