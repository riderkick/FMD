unit TruyenTranhTuan;

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
  s: String;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL;
  if AURL <> '0' then
    s += '/page/' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do try
      i := 1;
      for v in XPath('//*[@id="page-nav"]//li') do begin
        x := StrToIntDef(v.toString, 1);
        if x > i then i := x;
      end;
      updateList.CurrentDirectoryPageNumber := i;
      XPathHREFAll('//*[@id="story-list"]/div/span[1]/a', ALinks, ANames);
    finally
      Free;
    end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do try
        coverLink := XPathString('//*[@class="manga-cover"]/img/@src');
        if title = '' then title := XPathString('//h1[@itemprop="name"]');
        authors := XPathString('//p[@class="misc-infor" and starts-with(.,"Tác giả")]/string-join(./a,", ")');
        genres := XPathString('//p[@class="misc-infor" and starts-with(.,"Thể loại")]/string-join(./a,", ")');
        summary := XPathString('//*[@id="manga-summary"]/p');
        XPathHREFAll('//*[@id="manga-chapter"]//*[@class="chapter-name"]/a', chapterLinks, chapterName);
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
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      s := XPathString('//script[contains(.,"var slides_page_path")]/substring-before(substring-after(substring-after(.,"var slides_page_path"),"["),"]")', Document);
      if s = '' then
      s := XPathString('//script[contains(.,"var slides_page_url_path")]/substring-before(substring-after(substring-after(.,"var slides_page_url_path"),"["),"]")', Document);
      if s <> '' then begin
        s := StringReplace(s, '"', '', [rfReplaceAll]);
        PageLinks.CommaText := s;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'TruyenTranhTuan';
    RootURL := 'http://truyentranhtuan.com';
    Category := 'Vietnamese';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
