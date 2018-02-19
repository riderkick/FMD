unit HentaiCafe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

uses
  synautil;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      Page := StrToIntDef(query.XPathString('//a[@class="last"]'), 1);
    finally
      query.Free;
    end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/page/' + AURL + '/') then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//h2[@class="entry-title"]/a') do
      begin
        ANames.Add(v.toString);
        ALinks.Add(v.toNode.getAttribute('href'));
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  MangaInfo.mangaInfo.website := Module.Website;
  if MangaInfo.FHTTP.GET(FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      with MangaInfo.mangaInfo, query do begin
        coverLink := XPathString('//div[@class="entry-content content"]//img/@src');
        title := XPathString('//div[@class="entry-content content"]//h3');
        for v in XPath('//div[@class="entry-content content"]//a') do begin
          s := v.toNode.getAttribute('href');
          if Pos('/artist/', s) > 0 then AddCommaString(artists, v.toString)
          else if Pos('/read/', s) > 0 then begin
            if title <> '' then begin
              chapterLinks.Add(s);
              chapterName.Add(title);
            end;
          end
          else AddCommaString(genres, v.toString);
        end;
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  source: TStringList;
  i: Integer;
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      source := TStringList.Create;
      try
        source.LoadFromStream(DownloadThread.FHTTP.Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do
            if Pos('var pages = ', source[i]) > 0 then begin
              s := SeparateRight(source[i], 'var pages = ');
              s := Trim(TrimRightChar(s, [';']));
              ParseJSONArray(s, 'url', PageLinks);
              Break;
            end;
        PageNumber := PageLinks.Count;
      finally
        source.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'HentaiCafe';
    RootURL := 'https://hentai.cafe';
    Category := 'H-Sites';
    SortedList := True;
    FavoriteAvailable := False;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
