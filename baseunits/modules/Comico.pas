unit Comico;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil,Dialogs;

implementation

const
  dirurl = '/official';

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
  s := Module.RootURL + dirurl;
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//p[@itemprop="name"]/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
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
  v: IXQValue;
  s: String;
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
  query: TXQueryEngineHTML;
  p, i: Integer;
  new_url: String;

  procedure getchapters;
  var
    v: IXQValue;
  begin
    for v in query.XPath('//a[@class="m-thumb-episode__inner"]') do begin
      MangaInfo.mangaInfo.chapterLinks.Add(v.toNode.getAttribute('href'));
      MangaInfo.mangaInfo.chapterName.Add(v.toString);
    end;
  end;

  procedure getp;
  begin
    p :=  StrToIntDef(SeparateRight(query.XPathString('//ul[@class="m-pager__list"]/li[last()]/a/@href'), '&page='), 1);
  end;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create(Document);
      with query do
        try
          coverLink := XPathString('//*[@class="m-title-hero m-title-hero__line o-mt-30"]@style/@background-image');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@class="m-title-hero__title--manga"]');
          status := MangaInfoStatusIfPos(
            XPathString('//*[@class="info"]/p/*[@class="data"][starts-with(.,"Status:")]/following-sibling::*[@class="desc"][1]'),
            'Ongoing',
            'Complete');
          artists := XPathStringAll('//a[itemprop="author"]/a');
          authors := XPathStringAll('//*[@class="info"]/p/*[@class="data"][starts-with(.,"Author:")]/following-sibling::*[@class="desc"][1]/a');
	  summary := XPathString('//div[@class="m-title-hero__description"]');
          getchapters;
          getp;
          if p > 1 then begin
            i := 2;
            while (i <= p) and (Thread.IsTerminated = False) do begin
              new_url :=  url + '&page=' + IntToStr(i);
              if GET(new_url) then begin
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
  ViewerURL: String;
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
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//img[@itemprop="primaryImageOfPage"]') do
            PageLinks.Add(v.toNode.getAttribute('src'));
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
    Website := 'Comico';
    RootURL := 'http://www.comico.jp';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
