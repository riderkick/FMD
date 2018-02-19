unit Mangaf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, FMDVars;

implementation

const
  dirurl: String = '/api/v1/explore/state/';
  dirstates: array[0..2] of String = ('stopped', 'ongoing', 'completed');

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  s, p: String;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := dirstates[Module.CurrentDirectoryIndex];
  p := IncStr(AURL);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + s + '?page=' + p) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        if AURL = '0' then begin
          s := XPathString('json(*).last_page');
          updateList.CurrentDirectoryPageNumber := StrToIntDef(XPathString('json(*).last_page'), 0);
        end;
        for v in XPath('json(*).data()') do begin
          s := v.getProperty('slug').toString;
          p := v.getProperty('name').toString;
          ALinks.Add(Module.RootURL + '/m/' + v.getProperty('slug').toString);
          ANames.Add(v.getProperty('name').toString);
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          v := XPath('//div[@id="content"]/noscript/div[@class="container"]');
          if title = '' then title := XPathString('h2', v);
          coverLink := XPathString('img/@src', v);
          authors := XPathString('dl[dt="الكاتب"]/dd', v);
          artists := XPathString('dl[dt="الراسم"]/dd', v);
          genres := XPathStringAll('dl[dt="التصنيفات"]/dd/a', ', ', v);
          status := MangaInfoStatusIfPos(XPathString('dl[dt="الحالة"]/dd', v),
            'مستمرة',
            'مكتملة');
          summary := XPathString('dl[dt="القصة"]/dd', v);
          XPathHREFAll('div[@class="Chapters"]/ul/li/a', chapterLinks, chapterName, v);
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//noscript/div[@class="container"]/ul/li/img/@src') do
            PageLinks.Add(v.toString());
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Mangaf';
    RootURL := 'http://mangaf.co';
    Category := 'Arabic';
    TotalDirectory := 3;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.

