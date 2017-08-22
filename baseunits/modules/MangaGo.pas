unit MangaGo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, Cloudflare;

implementation

var
  mangagocf: TCFProps;

const
  dirurl= '/list/directory/all/';

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
begin
  Result := Cloudflare.GETCF(AHTTP, AURL, mangagocf)
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + dirurl + '1/') then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('count(//div[@class="pagination"]//ol/li//select/option)', MangaInfo.FHTTP.Document), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + dirurl + IncStr(AURL) + '/') then
  begin
    Result := NO_ERROR;
    XPathHREFtitleAll('//div[@class="directory_left"]//li/h3/a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
    if GETWithCookie(MangaInfo.FHTTP, url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="left cover"]/img/@src'));
          title := XPathString('//h1');
          if RightStr(title, 6) = ' manga' then SetLength(title, Length(title) - 6);
          status := MangaInfoStatusIfPos(XPathString('//div[@class="manga_right"]//td/label[.="Status:"]/following-sibling::*/text()'));
          authors := XPathString('//div[@class="manga_right"]//td/label[.="Author:"]/string-join(following-sibling::*/text(),", ")');
          genres := XPathString('//div[@class="manga_right"]//td/label[.="Genre(s):"]/string-join(following-sibling::*/text(),", ")');
          summary := XPathString('//div[@class="manga_summary"]/string-join(text(),codepoints-to-string(10))');
          XPathHREFAll('//table[@id="chapter_table"]//td//a', chapterLinks, chapterName);
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
    if GETWithCookie(DownloadThread.FHTTP, MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      s := XPathString('//script[contains(.,"imgsrcs")]', Document);
      if s <> '' then begin
        if Pos('imgsrcs = new Array', s) <> 0 then
          s := GetString(s, '(', ')')
        else
          s := GetString(s, ' = ''', ''';');
        s:=StringReplace(s, '''', '', [rfReplaceAll]);
        PageLinks.CommaText := s;
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaGo';
    RootURL := 'http://www.mangago.me';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  mangagocf := TCFProps.Create;
  RegisterModule;

finalization
  mangagocf.Free;

end.
