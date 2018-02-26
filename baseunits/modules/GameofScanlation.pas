unit GameofScanlation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr, synautil;

implementation

const
  dirurl = '/projects/';

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
        for v in XPath('//div[@class="info"]/a') do begin
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
  query: TXQueryEngineHTML;
  v: IXQValue;
  i, p: Integer;
  s: String;

  procedure GetChapters;
  begin
    for v in query.XPath('//div[@class="list_press_text"]/p[@class="text_work"]/a') do begin
      MangaInfo.mangaInfo.chapterLinks.Add(v.toNode.getAttribute('href'));
      MangaInfo.mangaInfo.chapterName.Add(v.toString);
    end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    url := AppendURLDelim(FillHost(Module.RootURL, AURL));
    if MangaInfo.FHTTP.GET(url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create(Document);
      try
        if title = '' then title := query.XPathString('//div[@class="con"]/h2');
        summary := query.XPathString('//dd[@class="dsc"]');
        s := query.XPathString('//div[@class="con"]/dl/span[@class="aln"]');
        if s <> '' then begin
          s := LowerCase(s);
          if Pos('ongoing', s) > 0 then
            status := '1'
          else if Pos('completed', s) > 0 then
            status := '0';
        end;
        GetChapters;
        p := StrToIntDef(query.XPathString('//nav/a[last()-1]'), 1);
        if p > 1 then
          for i := 2 to p do
            if GET(AppendURLDelim(url) + 'page-' + IntToStr(i)) then begin
              query.ParseHTML(Document);
              GetChapters;
            end;
        InvertStrings([chapterLinks, chapterName]);
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
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    s := ReplaceRegExpr('/\?\w+.*$', AURL, '/', False);
    s := AppendURLDelim(FillHost(Module.RootURL, s)) + '?chapter_view=fullstrip';
    if DownloadThread.FHTTP.GET(s) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//div[@class="chapterPages"]//img/@src') do
          begin
            s := v.toString;
            if SaveImageBase64StringToFile(s,
              DownloadThread.Task.CurrentWorkingDir,
              DownloadThread.Task.GetFileName(PageLinks.Count)) then
              PageLinks.Add('D')
            else
              PageLinks.Add(MaybeFillHost(Module.RootURL, s));
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
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
    if CurrentDownloadChapterPtr < ChapterLinks.Count then begin
      Headers.Values['Referer'] := ' ' + FillHost(Module.RootURL, ChapterLinks[CurrentDownloadChapterPtr]);
    end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'GameofScanlation';
    RootURL := 'https://gameofscanlation.moe';
    Category := 'English-Scanlation';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnBeforeDownloadImage := @BeforeDownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
