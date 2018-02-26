unit MangaChanRU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirurl = '/manga/new';
  perpage = 20;

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
        s := XPathString('//*[@id="pagination"]/a[last()]/@href');
        if s <> '' then Page := StrToIntDef(SeparateRight(s, 'offset='), 1);
        if Page > 1 then
          Page := ceil(Page / perpage) + 1;
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if AURL <> '0' then s += '?offset=' + IntToStr(StrToInt(AURL) * perpage);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="content_row"]//a[@class="title_link"]') do begin
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
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do
  begin
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//img[@id="cover"]/@src'));
          if title = '' then title := XPathString('//*[@class="name_row"]/h1');
          authors := XPathString('//*[@class="item" and contains(.,"Автор")]/following-sibling::*[1]');
          genres := XPathString('//*[@class="item" and contains(.,"Тэги")]/following-sibling::*[1]');
          status := MangaInfoStatusIfPos(
            XPathString('//*[@class="item" and contains(.,"Загружено")]/following-sibling::*[1]'),
            'продолжается',
            '');
          summary := XPathString('//*[@id="description"]/text()');
          for v in XPath('//*[@class="table_cha"]//*[@class="manga"]/a') do begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
          end;
          if chapterLinks.Count = 0 then
            for v in XPath('//a[.="Читать онлайн"]') do
            begin
              chapterLinks.Add(v.toNode.getAttribute('href'));
              chapterName.Add(title);
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
  Source: TStringList;
  i, j: Integer;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      Source := TStringList.Create;
      try
        Source.LoadFromStream(Document);
        if Source.Count > 0 then
          for i := 0 to Source.Count - 1 do
            if Pos('"fullimg":', Source[i]) > 0 then begin
              s := SeparateRight(Source[i], ':');
              s := TrimChar(s, ['[', ']', ',']);
              PageLinks.CommaText := s;
              if PageLinks.Count > 0 then
                with TRegExpr.Create('(?i)//im(\d*\.)') do
                  try
                    for j := 0 to PageLinks.Count - 1 do
                      PageLinks[j] := Replace(PageLinks[j], '//img$1', True);
                  finally
                    Free;
                  end;
              Break;
            end;
      finally
        Source.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;

  procedure AddWebsiteModule(const AWebsite, ARootURL, ACategory: String);
  begin
    with AddModule do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := ACategory;
      SortedList := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaChanRU', 'http://mangachan.me', 'Russian');
  AddWebsiteModule('HentaiChanRU', 'http://hentai-chan.me', 'H-Sites');
  AddWebsiteModule('YaoiChanRU', 'http://yaoichan.me', 'H-Sites');
end;

initialization
  RegisterModule;

end.
