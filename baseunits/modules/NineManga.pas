unit NineManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, RegExpr;

implementation

uses FMDVars;

var
  cookie: String;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  i, x: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/category/index_' + IncStr(AURL) + '.html') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do try
      i := 1;
      for v in XPath('//ul[@class="pagelist"]/li') do begin
        x := StrToIntDef(v.toString, 1);
        if x > i then i := x;
      end;
      updateList.CurrentDirectoryPageNumber := i;
      XPathHREFAll('//dl[@class="bookinfo"]//dd/a[@class="bookname"]', ALinks, ANames);
    finally
      Free;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;

  function GetWithWarning(var url: String; const MangaInfo: TMangaInformation): Boolean;
  var
    w: String;
  begin
    Result := False;
    if MangaInfo.FHTTP.GET(url) then begin
      w := XPathString('//script[contains(., ''is_warning = "1"'')]', MangaInfo.FHTTP.Document);
      if (w = '') or (Pos('waring=1', url) > 0) then
        Result := True
      else begin
        url += '?waring=1';
        Result := MangaInfo.FHTTP.GET(url);
      end;
    end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GetWithWarning(url, MangaInfo) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do try
        coverLink := XPathString('//*[@class="bookface"]/img/@src');
        if title = '' then title := XPathString('//*[@class="manga"]/*[@class="ttline"]/h1/substring-before(.," Manga")');
        authors := XPathString('//ul[@class="message"]/li[starts-with(.,"Author")]/string-join(a,", ")');
        genres := XPathString('//ul[@class="message"]/li[starts-with(.,"Genre")]/string-join(a,", ")');
        status := MangaInfoStatusIfPos(XPathString('//ul[@class="message"]/li[starts-with(.,"Status")]'));
        summary := Trim(XPathString('//*[@class="bookintro"]/p[starts-with(.,"Summary")]/substring-after(.,":")'));
        XPathHREFAll('//*[@class="chapterbox"]//li/a', chapterLinks, chapterName);
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
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      PageNumber := XPathCount('(//select[@id="page"])[1]/option', Document);
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s, ref: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;

  s := AURL;
  s := ReplaceRegExpr('\.html?$', s, '', False);
  if DownloadThread.WorkId = 0 then
    ref := AURL
  else
    ref := s + '-' + IntToStr(DownloadThread.WorkId) + '.html';
  s := s + '-' + IncStr(DownloadThread.WorkId) + '.html';
  ref := MaybeFillHost(module.RootURL, ref);

  if cookie = '' then
  begin
    if DownloadThread.FHTTP.GET(Module.RootURL) then
       cookie := DownloadThread.FHTTP.Cookies.Text;
  end;

  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    Cookies.Text := cookie;
    Headers.Values['Referer'] := ref;
    if GET(MaybeFillHost(Module.RootURL, s)) then
    begin
      Result := True;
      PageLinks[DownloadThread.WorkId] := XPathString('//img[contains(@class,"manga_pic")]/@src', Document);
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL, ACategory: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      Category := ACategory;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('NineManga', 'http://www.ninemanga.com', 'English');
  AddWebsiteModule('NineMangaES', 'http://es.ninemanga.com', 'Spanish');
  AddWebsiteModule('NineMangaRU', 'http://ru.ninemanga.com', 'Russian');
  AddWebsiteModule('NineMangaDE', 'http://de.ninemanga.com', 'German');
  AddWebsiteModule('NineMangaIT', 'http://it.ninemanga.com', 'Italian');
  AddWebsiteModule('NineMangaBR', 'http://br.ninemanga.com', 'Portugues');
end;

initialization
  cookie := '';
  RegisterModule;

end.
