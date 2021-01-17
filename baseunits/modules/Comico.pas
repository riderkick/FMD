unit Comico;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil,Dialogs;

implementation

const
  apiUrl = 'http://www.comico.jp/api';
  getChaptersApiUrl = apiUrl + '/getArticleList.nhn';
  dirurl = '/official';
  dirpages: array[0..7] of String = ('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun', 'finish');

// Get Series Name
function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl + '/' + dirpages[Module.CurrentDirectoryIndex];
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//ul[contains(@class, "resizeTitleList")]/li/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('div/p[1]/text()', v));
        end;
      finally
        Free;
      end;
  end;
end;

// Get Chapter List
function GetChapterPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  url: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
  if MangaInfo.FHTTP.GET(url) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//*[@class="m-pager__item"]/a[last()-1]'), 1);
      finally
        Free;
      end;
  end;
end;

// Get Series Info
function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  s: String;
  v: IXQValue;
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
          coverLink := XPathString('//meta[@property="og:image"]/@content');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then begin
            title := XPathString('//meta[@property="og:title"]/@content');
            title := Trim(SeparateLeft(title, '|'));
          end;
          status := XPathString('//div[contains(@class, "meta")]/p[1]');
          if status = '完結作品' then status := '0'
          else status := '1';
          authors := XPathStringAll('//*[contains(@class, "author")]/a');
	  summary := XPathString('//meta[@property="og:description"]/@content');
          genres := XPathStringAll('//div[contains(@class, "meta")]/p[2]/a');
        finally
          Free;
        end;

      s := RegExprGetMatch('titleNo\=\d+', url, 0);
      Reset;
      if POST(getChaptersApiUrl, s) then
        with TXQueryEngineHTML.Create(Document) do
          try
            for v in XPath('json(*).result.list()') do begin
              chapterLinks.Add(v.getProperty('articleDetailUrl').toString);
              s := v.getProperty('freeFlg').toString;
              if s = 'N' then s += ' [unavailable]'
              else s := '';
              chapterName.Add(v.getProperty('subtitle').toString + s);
            end;
          finally
            Free;
          end;
      Reset;
      Headers.Values['Referer'] := ' ' + url;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      XPathStringAll('//img[contains(@class, "comic-image")]/@src', Document, PageLinks);
    end;
  end;
end;

function BeforeDownloadImage(const DownloadThread: TDownloadThread;
  var AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container do
    if CurrentDownloadChapterPtr < ChapterLinks.Count then begin
      DownloadThread.FHTTP.Headers.Values['Referer'] :=
        ' ' + FillHost(Module.RootURL, ChapterLinks[CurrentDownloadChapterPtr]);
      Result := True;
    end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Comico';
    RootURL := 'http://www.comico.jp';
    Category := 'Raw';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
    TotalDirectory := Length(dirpages);
  end;
end;

initialization
  RegisterModule;

end.
