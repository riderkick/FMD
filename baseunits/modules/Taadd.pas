unit Taadd;

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
  s: String;
  v: IXQValue;
  i, x: Integer;
begin
  Result := NET_PROBLEM;
  if Module.CurrentDirectoryIndex = 0 then
    s := '0-9'
  else
    s := ALPHA_LIST_UP[Module.CurrentDirectoryIndex + 1];
  if MangaInfo.FHTTP.GET(Module.RootURL + '/category/' + s + '_views_'+ IncStr(AURL) + '.html') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do try
      i := 1;
      for v in XPath('//*[@class="clistChr"]//span[@class="pagetor"]//text()') do begin
        x := StrToIntDef(v.toString, 1);
        if x > i then i := x;
      end;
      updateList.CurrentDirectoryPageNumber := i;
      XPathHREFtitleAll('//*[@class="clistChr"]/ul/li/div/h2/a', ALinks, ANames);
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
        coverLink := XPathString('//table//td/a/img/@src');
        if title = '' then title := XPathString('//title/substring-before(.," - Read ")');
        authors := XPathString('//table//table//td[starts-with(.,"Author:")]/string-join(./a,", ")');
        genres := XPathString('//table//table//td[starts-with(.,"Categories:")]/string-join(./a,", ")');
        status := MangaInfoStatusIfPos(XPathString('//table//table//td[starts-with(.,"Status:")]/a'),'Updated','Completed');
        summary := XPathString('//table//table//td[contains(.," Manga Summary ")]/substring-after(.,"Manga Summary ")');
        XPathHREFAll('//*[@class="chapter_list"]/table//tr/td[1]/a', chapterLinks, chapterName);
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
      PageNumber := StrToIntDef(XPathString('//select[@id="page"]/count(./option)', Document),0);
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
      PageLinks[DownloadThread.WorkId] := XPathString('//img[@id="comicpic"]/@src', Document);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Taadd';
    RootURL := 'http://www.taadd.com';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    TotalDirectory := Length(ALPHA_LIST_UP);
  end;
end;

initialization
  RegisterModule;

end.
