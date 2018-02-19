unit HeyMangaXyz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga-series/new/';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1') then
  begin
    Result := NO_ERROR;
    Page := XPathCount('//div/a[@class="btn btn-sm btn-icon"]', MangaInfo.FHTTP.Document);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//div[@class="row"]/div/div/a', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="mangas"]/div[@class="manga"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if (coverLink <> '') and (LeftStr(coverLink, 2) = '//') then
            coverLink := 'https:' + coverLink;
          if title = '' then title := SeparateRight(XPathString('//ul[@class="lead"]/li[starts-with(.,"Name: ")]'), ': ');
          s := XPathString('//ul[@class="lead"]/li[starts-with(.,"Status: ")]');
          if Pos('Ongoing', s) > 0 then
            status := '1'
          else if Pos('Completed', s) > 0 then
            status := '0';
          genres := XPathStringAll('//ul[@class="lead"]/li[starts-with(.,"Genre: ")]/a');
          summary := SeparateRight(XPathString('//ul[@class="lead"]/li[starts-with(.,"Plot: ")]'), ': ');
          for v in XPath('//div[@id="chapters"]/ul//li//a') do
          begin
            s := v.toNode.getAttribute('href');
            if RightStr(s, 2) = '/1' then
              SetLength(s, Length(s) - 1);
            chapterLinks.Add(s);
            chapterName.Add(v.toString);
          end;
          if chapterLinks.Count > 0 then
          begin
            s := XPathString('//div[@id="chapters"]/ul/p[starts-with(.,"Next Chapter:")]/following-sibling::li//a/@href');
            if s <> '' then
            begin
              if RightStr(s, 2) = '/1' then
                SetLength(s, Length(s) - 1);
              if SameText(s, chapterLinks[chapterLinks.Count-1]) then
              begin
                chapterLinks.Delete(chapterLinks.Count - 1);
                chapterName.Delete(chapterName.Count - 1);
              end;
            end;
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
begin
  with DownloadThread, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    Result := FHTTP.GET(FillHost(Module.RootURL, AURL) + '1');
    if not Result then Exit;
    XPathStringAll('//select[@id="fuzetsu_list"]/option[@value]/concat('+ QuotedStr(AURL) +',@value)', FHTTP.Document, PageLinks);
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;

  function downloadandsave(u: String): Boolean;
  begin
    if u = '' then Exit(False);
    if LeftStr(u, 2) = '//' then u := 'https:' + u;
    Result := DownloadThread.FHTTP.GET(u);
  end;

begin
  Result := False;
  if not DownloadThread.FHTTP.GET(FillHost(Module.RootURL, AURL)) then Exit;
  with TXQueryEngineHTML.Create(DownloadThread.FHTTP.Document) do
  try
    Result := downloadandsave(XPathString('//img[@id="img-content"]/@src'));
    if (Result = False) and (DownloadThread.IsTerminated = False)  then
      Result := downloadandsave(XPathString('//img[@id="img-content"]/@onerror/substring-before(substring-after(.,"''"),"''")'));
  finally
    Free;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'HeyManga';
    RootURL := 'https://www.heymanga.me';
    Category := 'English';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
