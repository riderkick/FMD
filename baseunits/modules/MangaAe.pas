unit MangaAe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, strutils;

implementation

const
  dirurl = '/manga';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    Page := XPathCount('//div[@class="pagination"]/a', MangaInfo.FHTTP.Document);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '/page:' + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//div[@id="mangadirectory"]/div[@class="mangacontainer"]/a[2]', MangaInfo.FHTTP.Document, ALinks, ANames);
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
      with TXQueryEngineHTML.Create(Document) do try
        coverLink := XPathString('//img[@class="manga-cover"]/resolve-uri(@src)');
        if title = '' then title := TrimChar(XPathString('//h1[@class="EnglishName"]'), ['(', ')']);
        authors := XPathString('//div[@class="manga-details-author"]/h4[1]');
        genres := XPathString('//div[@class="manga-details-extended"]/ul/string-join(./li/a,", ")');
        summary := XPathString('//div[@class="manga-details-extended"]/h4[last()]');
        status := MangaInfoStatusIfPos(XPathString('//div[@class="manga-details-extended"]/h4[2]'),
          'مستمرة',
          'مكتملة');
        XPathHREFAll('//ul[@class="new-manga-chapters"]/li/a', chapterLinks, chapterName);
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
  u: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    u := RemoveURLDelim(MaybeFillHost(Module.RootURL, AURL));
    if AnsiEndsStr('/1', u) then u := AnsiLeftStr(u, Length(u)-2);
    u += '/0/full';
    if GET(u) then
    begin
      Result := True;
      XPathStringAll('//*[@id="showchaptercontainer"]//img/resolve-uri(@src)', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaAe';
    RootURL := 'https://www.manga.ae';
    Category := 'Arabic';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
