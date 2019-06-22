unit MangaTr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

var
  mangatrcookie: String = '';
  puzzmoscookie: String = '';

const
  mangatrdirurl = '/manga-list.html?listType=allABC';
  puzzmosdirurl = '/directory?type=text';

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
  if Module.Website = 'Manga-Tr' then
    s := s + mangatrdirurl
  else if Module.Website = 'Puzzmos' then
    s := s + puzzmosdirurl;
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//span[starts-with(@class, "manga")]//a') do
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
  v: IXQValue;
  i: Integer;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  MangaInfo.mangaInfo.website := Module.Website;
  if MangaInfo.FHTTP.GET(FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    with MangaInfo.mangaInfo, TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        coverLink := XPathString('//img[starts-with(@class,"thumbnail")]/@src');
        if coverLink <> '' then coverLink := FillHost(Module.RootURL, coverLink);
        if title = '' then title := XPathString('//title');
        if title <> '' then
        begin
          if Pos(' Manga - Oku ', title) > 0 then
            title := SeparateLeft(title, ' Manga - Oku ')
          else if Pos(' Mangasını Oku ', title) > 0 then
            title := SeparateLeft(title, ' Mangasını Oku ');
        end;
        if title = '' then title := XPathString('//h1');
        if Pos('Yazar', XPathString('//table[1]/tbody/tr[1]/td[1]')) > 0 then
          s := '//table[1]/tbody/tr[2]'
        else
          s := '//table[2]/tbody/tr[2]/';
        authors := XPathString(s + '/td[1]');
        artists := XPathString(s + '/td[2]');
        genres := TrimRightChar(Trim(XPathString(s + '/td[3]')), [',']);
        summary := XPathString('//div[@class="well"]/p');
        v := XPath('//table[4]/tbody/tr/td/a');
        if v.Count = 0 then v := XPath('//table[3]/tbody/tr/td/a');
        if v.Count = 0 then begin
          s := XPathString('//*[@slug]/@slug');
          if s <> '' then begin
            MangaInfo.FHTTP.Reset;
            MangaInfo.FHTTP.Headers.Add('X-Requested-With: XMLHttpRequest');
            if MangaInfo.FHTTP.GET(Module.RootURL+'/cek/fetch_pages_manga.php?manga_cek='+s) then begin
              ParseHTML(MangaInfo.FHTTP.Document);
              v := XPath('//tr/td[1]/a');
            end;
          end;
        end;
        if v.Count <> 0 then
        begin
          for i := 1 to v.Count do begin
            chapterLinks.Add(v.get(i).toNode.getAttribute('href'));
            chapterName.Add(v.get(i).toString);
          end;
          InvertStrings([chapterLinks, chapterName]);
        end;
      finally
        Free;
      end;
  end;
end;

function GETWithCookie(const AHTTP: THTTPSendThread;
  const AURL: String;
  const Module: TModuleContainer;
  const usePOST: Boolean = False;
  const POSTData: String = ''): Boolean;
var
  iurl, s: String;
  ccookie: PString;
begin
  Result := False;
  if Module.Website = 'Manga-Tr' then
    ccookie := @mangatrcookie
  else if Module.Website = 'Puzzmos' then
    ccookie := @puzzmoscookie;
  iurl := MaybeFillHost(Module.RootURL, AURL);
  if ccookie^ <> '' then
  begin
    AHTTP.Cookies.Text := ccookie^;
    if AHTTP.GET(iurl) then
    begin
      s := StreamToString(AHTTP.Document);
      if (Pos('class="chapter-content"', s) > 0) or (Pos('class=''chapter-content''', s) > 0) then
        Result := True;
    end;
  end;
  if not Result then
  begin
    AHTTP.Reset;
    if usePOST then
      Result := AHTTP.POST(iurl, POSTData)
    else
      Result := AHTTP.GET(iurl);
    if Result then
    begin
      if AHTTP.Cookies.Values['PHPSESSID'] <> '' then
      begin
        // manga-tr allpage = 1; perpage = 2
        if Module.Website = 'Manga-Tr' then
          AHTTP.Cookies.Values['read_type'] := '1';
        ccookie^ := AHTTP.GetCookies;
        AHTTP.Reset;
        AHTTP.Headers.Values['Referer'] := ' ' + iurl;
        Result := AHTTP.GET(iurl);
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
    PageContainerLinks.Clear;
    PageNumber := 0;
    //if Module.Website = 'Manga-Tr' then
    //  Result := GETWithCookie(DownloadThread.FHTTP, AURL, Module)
    // puzzmos POST perpage = "sayfadansayfa:" allpage = "altalta:"
    //else if Module.Website = 'Puzzmos' then
    //  Result := GETWithCookie(DownloadThread.FHTTP, AURL, Module, True, 'altalta:');
    Result := GETWithCookie(DownloadThread.FHTTP, AURL, Module);
    if Result then
    begin
      with TXQueryEngineHTML.Create(Document) do
        try
          //perpage
          if Module.Website = 'Manga-Tr' then
            s := '//div[@class="chapter-content"]/select[2]/option'
          else if Module.Website = 'Puzzmos' then
            s := '(//select)[2]/option';
          for v in XPath(s) do
            if Pos('Yorumlar', v.toString) = 0 then
              PageContainerLinks.Add(v.toNode.getAttribute('value'));
          PageNumber := PageContainerLinks.Count;
          //allpage
          for v in XPath('//div[@class="chapter-content"]//img[@class="chapter-img"]/@src') do
            PageLinks.Add(MaybeFillHost(Module.RootURL, v.toString));
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    Headers.Values['Referer'] := ' ' + AURL;
    if DownloadThread.WorkId > PageContainerLinks.Count then Exit;
    if GETWithCookie(DownloadThread.FHTTP, PageContainerLinks[DownloadThread.WorkId], Module) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] :=
            MaybeFillHost(Module.RootURL, XPathString('//div[@class="chapter-content"]//img/@src'));
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;

  procedure AddWebsiteModule(const AWebsite, ARootURL: String);
  begin
    with AddModule do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := 'Turkish';
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('Manga-Tr', 'http://manga-tr.com');
  AddWebsiteModule('Puzzmos', 'http://puzzmos.com');
end;

initialization
  RegisterModule;

end.
