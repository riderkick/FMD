unit FunManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga-list';
  diralpha = '#abcdefghijklmnopqrstuvwxyz';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := Length(diralpha);
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s := s + '/' + diralpha[StrToIntDef(AURL, 0) + 1] ;
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="content"]/div/div[@class="row"]//li/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="content"]//img/@src'));
          if title = '' then title := XPathString('//div[@class="content"]//h5');
          authors := XPathStringAll('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Author")]/following-sibling::dd[1]/a');
          artists := XPathStringAll('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Artist")]/following-sibling::dd[1]/a');
          genres := XPathStringAll('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Categories")]/following-sibling::dd[1]/a');
          status := MangaInfoStatusIfPos(XPathString(
            '//dl[@class="dl-horizontal"]/dt[starts-with(.,"Status")]/following-sibling::dd[1]'),
            'Ongoing',
            'Completed');
          summary := XPathString('//div[@class="content"]/div/div[contains(@class,"note")]');
          for v in XPath('//ul[@class="chapter-list"]/li/a') do
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
    if Pos('/all-pages', s) = 0 then
      s := s + '/all-pages';
    if GET(s) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//div[contains(@class,"content-inner")]/img/@src', PageLinks);
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
    Website := 'FunManga';
    RootURL := 'http://www.funmanga.com';
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
