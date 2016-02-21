unit MangaTr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager;

implementation

var
  mangatrcookie: String = '';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga-list.html') then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//span[starts-with(@class, "manga")]//a') do
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
  i: Integer;
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
        coverLink := XPathString('//img[@class="thumbnail"]/@src');
        if coverLink <> '' then coverLink := FillHost(Module.RootURL, coverLink);
        if title = '' then title := XPathString('//h1');
        authors := XPathString('//table[2]/tbody/tr[2]/td[1]');
        artists := XPathString('//table[2]/tbody/tr[2]/td[2]');
        genres := TrimRightChar(Trim(XPathString('//table[2]/tbody/tr[2]/td[3]')), [',']);
        summary := XPathString('//div[@class="well"]/p');
        v := XPath('//table[4]/tbody/tr/td/a');
        if v.Count = 0 then v := XPath('//table[3]/tbody/tr/td/a');
        if v.Count > 0 then
          for i := 1 to v.Count do begin
            chapterLinks.Add(v.get(i).toNode.getAttribute('href'));
            chapterName.Add(v.get(i).toString);
          end;
      end;
    finally
      query.Free;
    end;
  end;
end;

function GETWithCookie(const AHTTP: THTTPSendThread; AURL: String): Boolean;
var
  s: String;
begin
  Result := False;
  if mangatrcookie <> '' then begin
    AHTTP.Cookies.Text := mangatrcookie;
    if AHTTP.GET(AURL) then begin
      s := StreamToString(AHTTP.Document);
      if (Pos('class="chapter-content"', s) > 0) or (Pos('class=''chapter-content''', s) > 0) then
        Result := True;
    end;
  end;
  if not Result then begin
    AHTTP.Reset;
    if AHTTP.GET(AURL) then begin
      if AHTTP.Cookies.Values['PHPSESSID'] <> '' then begin
        mangatrcookie := AHTTP.GetCookies;
        AHTTP.Reset;
        AHTTP.Headers.Values['Referer'] := ' ' + AURL;
        Result := AHTTP.GET(AURL);
      end;
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
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        for v in query.XPath('//div[@class="chapter-content"]/select[2]/option') do
          if StrToIntDef(v.toString, 0) > 0 then
            PageContainerLinks.Add(v.toNode.getAttribute('value'));
        PageNumber := PageContainerLinks.Count;
      finally
        query.Free;
      end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do begin
    Headers.Values['Referer'] := ' ' + AURL;
    if DownloadThread.workCounter > PageContainerLinks.Count then Exit;
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, PageContainerLinks[DownloadThread.workCounter])) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        PageLinks[DownloadThread.workCounter] := query.XPathString('//div[@class="chapter-content"]//img/@src');
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
    Website := 'Manga-Tr';
    RootURL := 'http://manga-tr.com';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
