unit ReadMangaToday;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

const
  dirurls = ' abcdefghijklmnopqrstuvwxyz';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := Length(dirurls);
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga-list/' + dirurls[StrToIntDef(AURL, 0) + 1]) then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="manga-item"]//a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="panel-body"]//img/@src'));
          if title = '' then title := XPathString('//h1');
          authors := XPathString('//li[.="Author"]/preceding-sibling::li');
          artists := XPathString('//li[.="Artist"]/preceding-sibling::li');
          genres := XPathString('//*[@class="dl-horizontal"]/dt[starts-with(.,"Categories")]/following-sibling::dd[1]/string-join(*,", ")');
          status := MangaInfoStatusIfPos(XPathString('//*[@class="dl-horizontal"]/dt[starts-with(.,"Status")]/following-sibling::dd[1]'));
          summary := XPathString('//*[contains(@class,"movie-detail")]');
          for v in XPath('//ul[@class="chp_lst"]/li/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(XPathString('span[1]', v));
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
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    s := MaybeFillHost(Module.RootURL, AURL);
    if Pos('/all-pages', LowerCase(s)) = 0 then
      s := AppendURLDelim(s) + 'all-pages';
    if GET(s) then
    begin
      Result := True;
      XPathStringAll('//*[contains(@class,"content-list")]//img/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'ReadMangaToday';
    RootURL := 'https://www.readmng.com';
    Category := 'English';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
