unit MyReadingManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML;

implementation

uses Cloudflare, httpsendthread;

var
  myreadingmangacf: TCFProps;

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
begin
  Result := Cloudflare.GETCF(AHTTP, AURL, myreadingmangacf)
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL) then begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('//*[contains(@class,"archive-pagination")]/ul/li[last()-1]', MangaInfo.FHTTP.Document), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + '/page/' + IncStr(AURL) + '/') then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//h2[@class="entry-title"]/a', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GETWithCookie(MangaInfo.FHTTP, url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do try
        title := XPathString('//*[contains(@class,"entry-content")]/h2');
        if title = '' then title := Trim(XPathString('//*[contains(@class,"entry-content")]/p[starts-with(.,"Title")]/substring-after(.,":")'));
        if title = '' then title := XPathString('//*[contains(@class,"entry-content")]/p[1]/strong');
        if title = '' then title := XPathString('//h1[@class="entry-title"]');
        genres := XPathString('//header[@class="entry-header"]/string-join(./p[position()>1]//a,", ")');
        authors := Trim(XPathString('//*[contains(@class,"entry-content")]/p[starts-with(.,"Author")]/substring-after(.,":")'));
        chapterLinks.Add(url);
        chapterName.Add(title);
        for v in XPath('//*[contains(@class,"entry-pagination")]/a') do
        begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          chapterName.Add(title + ' - ' + v.toString);
        end;
        if chapterName.Count > 1 then
          chapterName[0] := chapterName[0] + ' - 1';
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
    if GETWithCookie(DownloadThread.FHTTP, MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      XPathStringAll('//*[contains(@class,"entry-content")]//div//img/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MyReadingManga';
    RootURL := 'https://myreadingmanga.info';
    SortedList := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  myreadingmangacf := TCFProps.Create;
  RegisterModule;

finalization
  myreadingmangacf.Free;

end.
