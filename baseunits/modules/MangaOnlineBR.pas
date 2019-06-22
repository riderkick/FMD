unit MangaOnlineBR;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, synacode, RegExpr;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/lista-titulos.php') then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="manga"]/p/a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@id="page-mangas"]//*[@class="image"]/img/@src'));
          title := XPathString('//h1');
          authors := XPathString('//*[@class="texto-left"]/ul/li[starts-with(.,"História")]/substring-after(.,":")');
          artists := XPathString('//*[@class="texto-left"]/ul/li[starts-with(.,"Ilustração")]/substring-after(.,":")');
          genres := XPathString('string-join(//*[@class="generos"]/a,", ")');
          status := MangaInfoStatusIfPos(XPathString('//*[@class="texto-left"]/ul/li[starts-with(.,"Status")]'), 'Em Publicação', '');
          summary := XPathString('//p[.="Sinopse"]/following-sibling::p');
          for v in XPath('//*[@id="volumes-capitulos"]//*[@class="texto"]') do
          begin
            s := XPathString('p[1]', v);
            for x in XPath('p[2]//a', v) do
            begin
              chapterLinks.Add(x.toNode.getAttribute('href'));
              chapterName.Add(s + ' Capitulo ' + x.toString);
            end;
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
var
  a, c: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    a := '';
    c := '';
    with TRegExpr.Create('(?i)/([^/]+)/capitulo/(\d+)') do
      try
        if Exec(AURL) then
        begin
          a := Match[1];
          c := Match[2];
        end;
      finally
        Free;
      end;
    if (a = '') or (c ='') then Exit;
    if GET(Module.RootURL + '/capitulo.php?act=getImg&anime=' + EncodeURLElement(a) + '&capitulo=' + c + '&src=1&view=2') then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//*[@id="imgAvancadoVisualizacao"]/img') do
            PageLinks.Add(MaybeFillHost(Module.RootURL, v.toNode.getAttribute('src')));
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
    Website := 'MangaOnlineBR';
    RootURL := 'http://mangaonline.com.br';
    Category := 'Portugues';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
