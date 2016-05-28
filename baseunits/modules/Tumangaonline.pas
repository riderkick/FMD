unit Tumangaonline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, synacode, RegExpr;

implementation

const
  apiurl = '/api/v1/';
  apiurlmangas = apiurl + 'mangas';
  apiurlimagenes = apiurl + 'imagenes';
  dirurl = apiurlmangas + '?searchBy=nombre&sortDir=asc&sortedBy=nombre&itemsPerPage=1000&page=';
  mangaurl = '/#!/biblioteca/mangas/';
  imgurl = 'http://img1.tumangaonline.com';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + '1') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString('json(*).last_page'), 1);
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
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('json(*).data()/concat(id,"/",nombreUrl," ",nombre)') do
        begin
          s := v.toString;
          ALinks.Add(mangaurl + SeparateLeft(s, ' '));
          ANames.Add(SeparateRight(s, ' '));
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
  mangaid, purl, s: String;
  p, i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    if Pos(mangaurl, AURL) <> 0 then
    begin
      mangaid := SeparateRight(AURL, mangaurl);
      if Pos('/', mangaid) <> 0 then
        mangaid := SeparateLeft(mangaid, '/');
    end
    else
      Exit;
    url := FillHost(Module.RootURL, AURL);
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
              for v in XPath('json(*).data()/concat("?idManga=",idTomo,"&idScanlation=",subidas/idScan,"&numeroCapitulo=",numCapitulo,"&visto=true")') do
                chapterLinks.Add(apiurlimagenes + v.toString);
              for v in XPath('json(*).data()/concat(numCapitulo," ",nombre)') do
              begin
                s := v.toString;
                if RightStr(s, 5) = ' null' then
                  SetLength(s, Length(s) - 5);
                chapterName.Add(s);
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
          s := imgurl + '/subidas/' + XPathString('json(*)/concat(capitulo/idTomo,"/",capitulo/numCapitulo,"/",idScan)') + '/';
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
