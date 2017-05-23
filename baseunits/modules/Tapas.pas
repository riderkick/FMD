unit Tapas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, Cloudflare, synautil, RegExpr;

implementation

const
  dirurl = '/comics?sortType=TITLE&browse=ALL';
  
var
  tapascf: TCFProps;

function Split(const str: string; const separator: string): TStringList;
var
  strline, strfield: string;
  list: TStringList;
begin
  strline := str;
  list := TStringList.Create;
  repeat
    if Pos(separator, strline) > 0 then
    begin
      strfield := Copy(strline, 1, Pos(separator, strline) - 1);
      strline := Copy(strline, Pos(separator, strline) + 1,
Length(strline) - pos(separator,strline));
    end
    else
    begin
      strfield := strline;
      strline := '';
    end;
    list.Add(strfield);
  until strline = '';
  Result := list;
end;

function SubString(const str: string; const delimFrom: string; const delimTo:
  string): string;
var
  s: string;
begin
  s := copy(str, Pos(delimFrom, str) + Length(delimFrom));
  Result := copy(s, 0, Pos(delimTo, s) - 1);
end;

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String):
 Boolean;
begin
  Result := Cloudflare.GETCF(AHTTP, AURL, tapascf)
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
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
    s := s + '?pageNumber=' + IncStr(AURL);
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
  s: String;
  caps: TStringList;
  cap: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP, FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    with MangaInfo.mangaInfo, TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        coverLink := XPathString('//a[@id="series-thumb"]/img/@src');
        if coverLink = '' then
        begin
          s := SubString(XPathString('//body'), '<div class="inner has-thumb">', '</div>');
          coverLink := SubString(s, '<img src="', '"');
        end;

        if title = '' then
        begin
          title := XPathString('//a[@class="series-header-title"]/text()');
        end;
        genres := XPathString('//div[@class="tags"]');
        authors := XPathString('//a[@class="name"]/span/text()');
        summary := XPathString('//span[@id="series-desc-body"]');
        summary := StringReplace(summary, 
                '                                                    ', '', [rfReplaceAll]);
        
        s := XPathString('//body');
        s := SubString(s, 'var _data', '}],');
        caps := TStringList.Create;
        caps := Split(s, '},{');
        for cap in caps do begin
          chapterLinks.Add('https://tapas.io/episode/' + SubString(cap, '"id":', ','));
          chapterName.Add(SubString(cap, '"title":"', '","thumbUrl":"'));
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
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
  end;
end;

initialization
  tapascf := TCFProps.Create;
  RegisterModule;

finalization
  tapascf.Free;

end.
