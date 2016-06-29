unit Tumangaonline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, RegExpr;

implementation

const
  apiurl = '/api/v1/';
  apiurlmangas = apiurl + 'mangas';
  apiurlimagenes = apiurl + 'imagenes';
  dirurl = apiurlmangas + '?searchBy=nombre&sortDir=asc&sortedBy=nombre&itemsPerPage=';
  perpage = '1000';
  mangaurl = '/biblioteca/mangas/';
  imgurl = 'http://img1.tumangaonline.com';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1&page=1') then
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
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + perpage + '&page=' + IncStr(AURL)) then
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
  v, x: IXQValue;
  mangaid, purl: String;
  s, cl, cn, vs: String;
  p, i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := FillHost(Module.RootURL, AURL);
    mangaid := RegExprGetMatch('/mangas/(\d+)/', url, 1);
    if mangaid = '' then Exit;
    if GET(Module.RootURL + apiurlmangas + '/' + mangaid) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('json(*).imageUrl');
          if coverLink <> '' then coverLink := MaybeFillHost(imgurl, coverLink);
          if title = '' then title := XPathString('json(*).nombre');
          s := XPathString('json(*).estado');
          if Pos('Activo', s) <> 0 then
            status := '1'
          else if Pos('Finalizado', s) <> 0 then
            status := '0';
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
                cl := apiurlimagenes + XPathString('string-join(("?idManga=",idTomo,"&numeroCapitulo=",numCapitulo),"")', v);
                vs := '&visto=' + XPathString('visto', v);
                cn := Trim(XPathString('string-join((numCapitulo,nombre)," ")', v));
                if RightStr(cn, 5) = ' null' then
                  SetLength(cn, Length(cn) - 5);
                for x in XPath('(subidas)()', v) do
                begin
                  chapterLinks.Add(cl + '&idScanlation='+ XPathString('idScan', x) + vs);
                  s := XPathString('scanlation/nombre', x);
                  if s <> '' then chapterName.Add(cn + ' [' + s + ']')
                  else chapterName.Add(s);
                end;
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
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := imgurl + '/subidas/' +
            XPathString('json(*)/concat(capitulo/idTomo,"/",capitulo/numCapitulo,"/",idScan)') + '/';
          for v in XPath('json((json(*).imagenes))()') do
            PageLinks.Add(s + v.toString);
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do begin
    Website := 'Tumangaonline';
    RootURL := 'http://www.tumangaonline.com';
    MaxTaskLimit := 1;
    MaxConnectionLimit := 1;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
