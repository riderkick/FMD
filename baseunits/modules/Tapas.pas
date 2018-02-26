unit Tapas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr;

implementation

const
  dirurl = '/comics?sortType=TITLE&browse=ALL';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('//div[@class="global-pagination-wrap"]/a[last()-1]'), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s := s + '&pageNumber=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//ul[@class="content-list-wrap"]//li//a[@class="preferred title"]') do begin
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
  locked: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    with MangaInfo.mangaInfo, TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        coverLink := XPathString('//a[@id="series-thumb"]/img/@src');
        if coverLink = '' then coverLink := XPathString('//script[contains(.,"has-thumb")]/substring-before(substring-after(.,"src="""),"""")');
        if title = '' then title := XPathString('//a[@class="series-header-title"]/text()');
        genres := XPathString('//div[@class="tags"]/string-join(./*,", ")');
        authors := XPathString('//a[@class="name"]/span/text()');
        summary := XPathString('//span[@id="series-desc-body"]');
        while Pos('  ', summary) <> 0 do
          summary := StringReplace(summary, '  ', ' ', [rfReplaceAll]);
        for v in XPath('json(//script[contains(.,"var _data")]/concat(substring-before(substring-after(.,"episodeList :"),"]"),"]"))()') do
        begin
          chapterLinks.Add(Module.RootURL + '/episode/' + XPathString('./id', v));
          locked := '';
          if XPathString('./locked', v) = 'true' then locked := ' [locked]';
          chapterName.Add(XPathString('./title', v) + locked);
        end;
      finally
        Free;
      end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          with TRegExpr.Create do
            try
              Expression := '[\?\&]type=q\d+';
              for v in XPath('//img[@class="art-image"]/@src') do
                PageLinks.Add(Replace(v.toString, '', False));
            finally
              Free;
            end;
        finally
          Free;
        end;
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
    Website := 'Tapas';
    RootURL := 'https://tapas.io';
    Category := 'Webcomics';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
