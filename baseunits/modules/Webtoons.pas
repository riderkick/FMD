unit Webtoons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  dirurl = '/en/genre';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//ul[@class="card_lst"]/li/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('div/p[@class="subj"]', v.toNode));
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  p, i: Integer;

  procedure getchapters;
  var
    v: IXQValue;
  begin
    for v in query.XPath('//ul[@id="_listUl"]/li/a') do begin
      MangaInfo.mangaInfo.chapterLinks.Add(v.toNode.getAttribute('href'));
      MangaInfo.mangaInfo.chapterName.Add(query.XPathString('span[@class="subj"]/span', v.toNode));
    end;
  end;

  procedure getp;
  begin
    p := StrToIntDef(SeparateRight(
      query.XPathString('//div[@class="detail_lst"]/div[@class="paginate"]/a[last()]/@href'), '&page='), 1);
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create(Document);
      with query do
        try
          coverLink := XPathString('//meta[@name="twitter:image"]/@content');
          if title = '' then title := XPathString('//div[@class="info"]/h1/text()');
          authors := XPathString('//div[@class="info"]/a/text()');
          genres := XPathString('//div[@class="info"]/h2');
          summary := XPathString('//p[@class="summary"]');
          getchapters;
          getp;
          if p > 1 then begin
            i := 2;
            while (i <= p) and (Thread.IsTerminated = False) do begin
              if GET(url + '&page=' + IntToStr(i)) then begin
                ParseHTML(Document);
                getchapters;
                getp;
              end;
              Inc(i);
            end;
          end;
          InvertStrings([chapterLinks, chapterName]);
          Reset;
          Headers.Values['Referer'] := ' ' + url;
        finally
          query.Free;
        end;
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
              for v in XPath('//div[@id="_imageList"]/img[@class="_images"]/@data-url') do
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
    Website := 'Webtoons';
    RootURL := 'http://www.webtoons.com';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
