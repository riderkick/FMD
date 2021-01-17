unit RawSenManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr, synautil, URIParser;

implementation

var
  cookie: String;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/directory/text_version') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//table//tr/td[2]/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  cl, m: String;
  cu: Boolean;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  m := RemoveHostFromURL(AURL);
  m := RemoveURLDelim(m);
  cl := '';
  with TRegExpr.Create do
    try
      Expression := '(.+)/.+/\d+?$';
      cu := Exec(m);
      if cu then begin
        cl := m;
        m := Replace(m, '$1', True);
      end;
    finally
      Free;
    end;
  m := AppendURLDelim(m);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    if cl <> '' then url := FillHost(Module.RootURL, cl)
    else url := FillHost(Module.RootURL, m);
    if GET(FillHost(Module.RootURL, m)) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//div[@class="thumbnail"]/img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@class="title"]');
          genres := XPathStringAll('//ul[@class="series-info"]/li[contains(., "Categories")]/a');
          authors := XPathStringAll('//ul[@class="series-info"]/li[contains(., "Author")]/a');
          artists := XPathStringAll('//ul[@class="series-info"]/li[contains(., "Artist")]/a');
          status := MangaInfoStatusIfPos(XPathString('//ul[@class="series-info"]/li[contains(., "Status")]/a'));
          summary := XPathString('//*[@itemprop="description"]');
          XPathHREFAll('//div[@class="title" and contains(., "Chapters")]/following-sibling::div/div/a', chapterLinks, chapterName);
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
  uri: TURI;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    s := RemoveURLDelim(ChapterLinks[CurrentDownloadChapterPtr]);
    with TRegExpr.Create do
      try
        Expression := '(.+)/.+/\d+?$';
        if Exec(s) then begin
          Expression := '/\d+$';
          s := Replace(s, '', False);
        end;
        ChapterLinks[CurrentDownloadChapterPtr] := s;
      finally
        Free;
      end;
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, s + '/1')) then begin
      Result := True;
      cookie := Cookies.Text;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('//select[@name="page"]/option').Count;
          if PageNumber > 0 then begin
            s := MaybeFillHost(Module.RootURL, XPathString('//img[@id="picture"]/@src'));
            uri := ParseURI(s);
            if (Pos('/raw-viewer.php', LowerCase(uri.Path)) > 0) and (Pos('page=1', LowerCase(uri.Params)) > 0) then
              while PageLinks.Count < PageNumber do begin
                uri.Params := ReplaceString(uri.Params, 'page=1', 'page=' + IncStr(PageLinks.Count));
                PageLinks.Add(EncodeURI(uri));
              end
            else if (Pos('/viewer/', LowerCase(uri.Path)) > 0) and (uri.Document = '1') then
              while PageLinks.Count < PageNumber do begin
                uri.Document := IncStr(PageLinks.Count);
                PageLinks.Add(EncodeURI(uri));
              end;
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
  uri: TURI;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    uri := ParseURI(FillHost(Module.RootURL, AURL));
    uri.Document := IncStr(DownloadThread.WorkId);
    if GET(EncodeURI(uri)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := MaybeFillHost(Module.RootURL, XPathString('//img[@id="picture"]/@src'));
          if s <> '' then
            PageLinks[DownloadThread.WorkId] := s;
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
      DownloadThread.FHTTP.Cookies.Text := cookie;
      Result := True;
    end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'RawSenManga';
    RootURL := 'http://raw.senmanga.com';
    Category := 'Raw';
    MaxTaskLimit := 1;
    MaxConnectionLimit := 4;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    OnBeforeDownloadImage := @BeforeDownloadImage;
  end;
end;

initialization
  RegisterModule;

end.
