unit RawSenManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr, synautil;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/Manga/?order=text-version') then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table//tr/td[2]/a') do begin
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
  i: Integer;
  s, cl, m: String;
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
          coverLink := XPathString('//img[@class="series-cover"]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@itemprop="name"]');
          v := XPath('//div[@class="series_desc"]/*');
          if v.Count > 0 then begin
            i := 0;
            while i < v.Count - 2 do begin
              s := v.get(i).toString;
              if Pos('Categorize in:', s) = 1 then genres := v.get(i + 1).toString else
              if Pos('Author:', s) = 1 then authors := v.get(i + 1).toString else
              if Pos('Artist:', s) = 1 then artists := Trim(SeparateRight(v.get(i).toString, ':')) else
              if Pos('Status:', s) = 1 then if Pos('ongoing', LowerCase(v.get(i).toString)) > 0 then
                  status := '1' else status := '0';
              Inc(i);
            end;
          end;
          summary := XPathString('//div[@class="series_desc"]//div[@itemprop="description"]');
          if not cu then
            for v in XPath('//*[@id="content"]/*[@id="post"]//tr[@class="even" or @class="odd"]/td[2]/a') do
            begin
              chapterLinks.Add(v.toNode.getAttribute('href'));
              chapterName.Add(v.toString);
            end
          else if cl <> '' then
            if GET(FillHost(Module.RootURL, cl)) then
            begin
              ParseHTML(Document);
              for v in XPath('//select[@name="chapter"]/option') do begin
                chapterLinks.Add(m + v.toNode.getAttribute('value'));
                chapterName.Add(v.toString);
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
var
  s: String;
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
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('//select[@name="page"]/option').Count;
          if PageNumber > 0 then begin
            s := MaybeFillHost(Module.RootURL, XPathString('//img[@id="picture"]/@src'));
            if ((Pos('/raw-viewer.php?', LowerCase(s)) > 0) and (LowerCase(RightStr(s, 7)) = '&page=1')) or
              ((Pos('/viewer/', AnsiLowerCase(s)) > 0) and (RightStr(s, 2) = '/1')) then
            begin
              SetLength(s, Length(s) - 1);
              while PageLinks.Count < PageNumber do
                PageLinks.Add(s + IncStr(PageLinks.Count));
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
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    if GET(FillHost(Module.RootURL, AURL) + '/' + IncStr(DownloadThread.WorkId)) then begin
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
      Result := True;
    end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'RawSenManga';
    RootURL := 'http://raw.senmanga.com';
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
