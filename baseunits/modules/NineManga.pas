unit NineManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses FMDVars;

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
var
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    s := url;
    if Pos('waring=1', s) = 0 then s += '?waring=1';
    if GET(s) then begin
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
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do begin
    if GET(AppendURLDelim(MaybeFillHost(Module.RootURL, AURL)) + 'page-' + IncStr(DownloadThread.WorkId)) then
    begin
      Result := True;
      PageLinks[DownloadThread.WorkId] := XPathString('//img[contains(@class,"manga_pic")]/@src', Document);
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('NineManga', 'http://www.ninemanga.com');
  AddWebsiteModule('NineMangaES', 'http://es.ninemanga.com');
  AddWebsiteModule('NineMangaRU', 'http://ru.ninemanga.com');
  AddWebsiteModule('NineMangaDE', 'http://de.ninemanga.com');
  AddWebsiteModule('NineMangaIT', 'http://it.ninemanga.com');
  AddWebsiteModule('NineMangaBR', 'http://br.ninemanga.com');
end;

initialization
  RegisterModule;

end.
