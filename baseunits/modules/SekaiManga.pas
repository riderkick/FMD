unit SekaiManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/Sekai/manga-list.html';
  urllead = '/Sekai/';

function FixURLLead(const S: String): String;
begin
  Result := S;
  if Result = '' then Exit;
  if Pos(urllead, Result) = 0 then
  begin
    if Result[1] = '/' then
      Delete(Result, 1, 1);
    Result := urllead + Result;
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
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in CSS('div.char>span a') do
        begin
          s := Trim(v.toString);
          if s <> '' then
          begin
            ALinks.Add(FixURLLead(v.toNode.getAttribute('href')));
            ANames.Add(s);
          end;
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v, x: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := CSS('img.img-rounded').toNode.getAttribute('src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := SeparateLeft(XPathString('//title'), ' - '#10);
          authors := XPathString('//div[@id="info"]//table/tbody/tr/td[.="Autor(s):"]/following-sibling::td');
          artists := XPathString('//div[@id="info"]//table/tbody/tr/td[.="Artista(s):"]/following-sibling::td');
          status := MangaInfoStatusIfPos(XPathString('//div[@id="info"]//table/tbody/tr/td[.="Estado en SekaiManga:"]/following-sibling::td'), 'En curso', 'Terminado');
          genres := XPathString('//div[@id="info"]//table/tbody/tr/td[.="Género(s):"]/following-sibling::td');
          for v in XPath('//table[@id="example"]/tbody/tr') do
          begin
            x := XPath('td[2]//a', v.toNode);
            s := XPathString('td[3]', v.toNode);
            if s <> '' then s := ' ' + s;
            chapterLinks.Add(FixURLLead(x.toNode.getAttribute('href')));
            chapterName.Add(x.toString + s);
          end;
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
    Cookies.Add('read_type=1');
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//div[@class="chapter-content"]//img/@src', PageLinks);
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'SekaiManga';
    RootURL := 'http://www.sekaimanga.net';
    Category := 'Spanish';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
