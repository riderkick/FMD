unit UnionMangas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, RegExpr, synautil;

implementation

const
  dirurl = '/mangas';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('//ul[@class="pagination"]/li[last()]/a/substring-before(substring-after(@href,"a-z/"),"/")', MangaInfo.FHTTP.Document), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if AURL <> '0' then s += '/a-z/' + IncStr(AURL) + '/*';
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="row"]/div/a[2]', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//img[@class="img-thumbnail"]/@src'));
          title := XPathString('//title/substring-before(.," - Union Mangás")');
          genres := XPathString('//h4[starts-with(./label,"Gênero")]/substring-after(.,":")');
          authors := XPathString('//h4[starts-with(./label,"Autor")]/substring-after(.,":")');
          artists := XPathString('//h4[starts-with(./label,"Artista")]/substring-after(.,":")');
          status := MangaInfoStatusIfPos(XPathString('//h4[starts-with(./label,"Status")]/substring-after(.,":")'), 'Ativo', 'Completo');
          summary := XPathString('//*[@class="panel-body"]');
          XPathHREFAll('//*[contains(@class,"lancamento-linha")]/div[1]/a', chapterLinks, chapterName);
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      XPathStringAll('//img[contains(@class, "img-manga") and contains(@src, "/leitor/")]/@src', Document, PageLinks);
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'UnionMangas';
    RootURL := 'http://unionmangas.net';
    Category := 'Portugues';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
