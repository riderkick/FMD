unit HitomiLa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  source: TStringList;
  i: Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL) then begin
    Result := NO_ERROR;
    source := TStringList.Create;
    try
      source.LoadFromStream(MangaInfo.FHTTP.Document);
      if source.Count > 0 then
        for i := 0 to source.Count - 1 do begin
          if Pos('insert_paging(', source[i]) > 0 then begin
            Page := StrToIntDef(GetString(source[i], ', 1, ', ');'), 1);
            Break;
          end;
        end;
    finally
      source.Free;
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
  s := Module.RootURL;
  if AURL <> '0' then
    s := s + '/index-all-' + AURL + '.html';
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="gallery-content"]/div/h1/a') do
        begin
          ANames.Add(v.toString);
          ALinks.Add(v.toNode.getAttribute('href'));
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
        coverLink := FillURLProtocol('https://', XPathString('//div[@class="cover"]//img/@src'));
        title := XPathString('//div/h1');
        for v in XPath('//div[@class="gallery-info"]/table//tr/td[2]//a') do
          AddCommaString(genres, v.toString);
        s := XPathString('//div[@class="cover-column"]/a/@href');
        if (s <> '') and (title <> '') then begin
          chapterLinks.Add(s);
          chapterName.Add(title);
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
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        for v in query.XPath('//div[@class="img-url"]') do
          PageLinks.Add(FillURLProtocol('https://', v.toString));
        PageNumber := PageLinks.Count
      finally
        query.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'HitomiLa';
    RootURL := 'https://hitomi.la';
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
