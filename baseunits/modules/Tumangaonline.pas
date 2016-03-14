unit Tumangaonline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, synacode, RegExpr;

implementation

const
  dirurls: array[0..5] of String = (
    '/listado-mangas/mangas?tipo=4&filter=Webtoon',
    '/listado-mangas/mangas?tipo=4&filter=Manga',
    '/listado-mangas/mangas?tipo=4&filter=OneShot',
    '/listado-mangas/mangas?tipo=4&filter=Manhwa',
    '/listado-mangas/mangas?tipo=4&filter=Manhua',
    '/listado-mangas/mangas?tipo=4&filter=Novela'
    );

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation; var Page: Integer;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurls[Module.CurrentDirectoryIndex]) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//ul[@class="pagination"]/li[starts-with(.,"Página 1 de")]');
        if s <> '' then begin
          s := SeparateRight(s, 'de ');
          Page := StrToIntDef(s, 1);
        end;
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
  s := Module.RootURL + dirurls[Module.CurrentDirectoryIndex];
  if AURL <> '0' then
    s := s + '&pag=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//div[@class="row text-center"]/div/a') do begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
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
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//img[@itemprop="image"]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@class="panel-title"]');
          authors := XPathString('//table[@class="tbl table-hover"]/tbody/tr/td[@property="name"]/a[1]');
          artists := XPathString('//table[@class="tbl table-hover"]/tbody/tr/td[@property="name"]/a[2]');
          genres := XPathString('//table[@class="tbl table-hover"]/tbody/tr/td[@colspan="2"]');
          summary := XPathString('//div[@id="descripcion"]');
          MangaInfo.RemoveHostFromChapterLinks := False;
          for v in XPath('//table[@class="tbl table-hover table-striped"]/tbody/tr/td/h5/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('onClick'));
            chapterName.Add(v.toString);
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function TaskStart(const Task: TTaskContainer; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if Task = nil then Exit;
  with Task.PageContainerLinks do
  begin
    NameValueSeparator := '=';
    Delimiter := '&';
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s, idManga, idCapitulo: String;
  source: TStringList;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    s := '';

    if Pos('listacapitulos', LowerCase(AURL)) > 0 then
    begin
      with TRegExpr.Create do
        try
          Expression := '(?i)listaCapitulos\((\d+),(\d+)[^d]*.*$';
          idManga := Replace(AURL, '$1', True);
          idCapitulo := Replace(AURL, '$2', True);
        finally
          Free;
        end;

      Headers.Values['Referer'] := ' ' + MaybeFillHost(Module.RootURL, DownloadInfo.Link);
      if POST(Module.RootURL + '/index.php?option=com_controlmanga&view=capitulos&format=raw',
        QueryString([KeyVal('idManga', idManga), KeyVal('idCapitulo', idCapitulo)])) then
      begin
        with TXQueryEngineHTML.Create(Document) do
          try
            s := XPathString('//a[@class="toViewer"]/@data-enlace');
            if s <> '' then
              ChapterLinks[CurrentDownloadChapterPtr] := s;
          finally
            Free;
          end;
      end;
    end
    else if Pos('/visor/', AURL) > 0 then
      s := AURL
    else
      Exit;
    if s = '' then Exit;

    Headers.Values['Referer'] := ' ' + MaybeFillHost(Module.RootURL, DownloadInfo.Link);
    if GET(MaybeFillHost(Module.RootURL, s)) then begin
      Result := True;
      with PageContainerLinks do begin
        source := TStringList.Create;
        try
          source.LoadFromStream(Document);
          if source.Count > 0 then
            for i := 0 to source.Count - 1 do begin
              if Pos('var tipo ', source[i]) > 0 then
                Values['tipo'] := TrimChar(SeparateRight(source[i], '='), [';', ' '])
              else if Pos('var tam ', source[i]) > 0 then
                Values['tam'] := TrimChar(SeparateRight(source[i], '='), [';', ' '])
              else if Pos('var sep ', source[i]) > 0 then
                Values['sep'] := TrimChar(SeparateRight(source[i], '='), [';', ' '])
              else if Pos('function toggleFullScreen()', source[i]) > 0 then
                Break;
            end;
        finally
          source.Free;
        end;
        with TXQueryEngineHTML.Create(Document) do
          try
            Values['ruta'] := EncodeURL(XPathString('//input[@hidden=true]/@value'));
            PageNumber := XPath('//select[@id="pageNumber"]/option').Count;
          finally
            Free;
          end;
      end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container, DownloadThread.FHTTP do begin
    if PageContainerLinks.Count = 0 then Exit;
    PageContainerLinks.Values['pagina'] := IncStr(DownloadThread.workCounter);
    Headers.Values['Referer'] :=
      ' ' + MaybeFillHost(Module.RootURL, ChapterLinks[CurrentDownloadChapterPtr]);
    if POST(Module.RootURL + '/visor/x.php', PageContainerLinks.DelimitedText) then begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.workCounter] :=
            XPathString('//img[@class="img-responsive center-block"]/@src');
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
    TotalDirectory := Length(dirurls);
    OnTaskStart := @TaskStart;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
