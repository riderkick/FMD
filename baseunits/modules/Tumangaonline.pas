unit Tumangaonline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, Cloudflare, synautil, RegExpr;

implementation

const
  apiurl = '/api/v1/';
  apiurlmangas = apiurl + 'mangas';
  apiurlimagenes = apiurl + 'imagenes';
  dirurl = apiurlmangas + '?searchBy=nombre&sortDir=asc&sortedBy=nombre&itemsPerPage=';
  perpage = '1000';
  mangaurl = '/biblioteca/mangas/';
  imgurl = 'https://img1.tumangaonline.com';

var
  tumangacf: TCFProps;

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String):
 Boolean;
begin
  Result := Cloudflare.GETCF(AHTTP, AURL, tumangacf);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + dirurl + '1&page=1') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := ceil(StrToIntDef(XPathString('json(*).total'), 1) / StrToInt(perpage));
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + dirurl + perpage + '&page=' + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('json(*).data()/string-join((id,"/",nombreUrl,codepoints-to-string(10),nombre),"")') do
        begin
          s := v.toString;
          ALinks.Add(mangaurl + SeparateLeft(s, #10));
          ANames.Add(Trim(SeparateRight(s, #10)));
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s, mangaid, purl: String;
  p, i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := FillHost(Module.RootURL, AURL);
    mangaid := RegExprGetMatch('/\w+/(\d+)/\w+', url, 1);
    if mangaid = '' then Exit;
    if GETWithCookie(MangaInfo.FHTTP, Module.RootURL + apiurlmangas + '/' + mangaid) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('json(*).imageUrl');
          if coverLink <> '' then coverLink := MaybeFillHost(imgurl, coverLink);
          if title = '' then title := XPathString('json(*).nombre');
          status := MangaInfoStatusIfPos(XPathString('json(*).estado'), 'Activo', 'Finalizado');
          summary := XPathString('json(*).info.sinopsis');
          genres := XPathStringAll('json(*).generos().genero');
          AddCommaString(genres, XPathStringAll('json(*).categorias().categoria'));
          artists := XPathStringAll('json(*).artistas().artista');
          authors := XPathStringAll('json(*).autores().autor');
          purl := Module.RootURL + apiurlmangas + '/' + mangaid + '/capitulos?tomo=-1&page=';
          if GET(purl + '1') then
          begin
            ParseHTML(Document);
            p := StrToIntDef(XPathString('json(*).last_page'), 1);
            for i := 1 to p do
            begin
              if i > 1 then
                if GET(purl + IntToStr(i)) then
                  ParseHTML(Document)
                else
                  Break;
              for v in XPath('json(*).data()') do
              begin
                chapterLinks.Add(apiurlimagenes + XPathString('"?idManga="||tomo/idManga||"&idScanlation="||subidas/idScan||"&numeroCapitulo="||numCapitulo||"&visto=true"', v));
                chapterName.Add(XPathString('string-join((numCapitulo,nombre)," ")', v));
              end;
            end;
            InvertStrings([chapterLinks, chapterName]);
          end;
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := imgurl + '/subidas/' +
            XPathString('json(*)/string-join((capitulo/tomo/idManga,capitulo/numCapitulo,idScan),"/")') + '/';
          for v in XPath('json((json(*).imagenes))()') do
            PageLinks.Add(s + v.toString);
        finally
          Free;
        end;
    end;
  end;
end;

function DownloadImageWithCookie(const DownloadThread: TDownloadThread;
  const AURL, APath, AName: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  if GETWithCookie(DownloadThread.FHTTP, AURL) then
    result := SaveImageStreamToFile(DownloadThread.FHTTP, APath, AName) <> '';
end;

procedure RegisterModule;
begin
  with AddModule do begin
    Website := 'Tumangaonline';
    RootURL := 'https://www.tumangaonline.com';
    MaxTaskLimit := 1;
    MaxConnectionLimit := 1;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImageWithCookie;
  end;
end;

initialization
  tumangacf := TCFProps.Create;
  RegisterModule;

finalization
  tumangacf.Free;

end.
