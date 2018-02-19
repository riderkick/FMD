unit KuManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, URIParser;

implementation

const
  dirurl = '/backend/ajax/searchengine.php';
  dirperpage = '15';
  dirpostdata = 'contentType=manga&retrieveCategories=true&retrieveAuthors=true&perPage=' +
    dirperpage + '&page=';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.POST(Module.RootURL + dirurl, dirpostdata + '1') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('json(*).totalContents');
        Page := ceil(StrToIntDef(s, 1) / StrToInt(dirperpage));
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  c, i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.POST(Module.RootURL + dirurl, dirpostdata + IncStr(AURL)) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        c := XPath('json(*).contents()').Count;
        for i := 1 to c do
        begin
          ANames.Add(XPathString('json(*).contents(' + IntToStr(i) + ').name'));
          ALinks.Add(XPathString('json(*).contents(' + IntToStr(i) + ')/concat("/manga/",id,"/",slug)'));
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  lastPage, i, totalChapters, perPage: Integer;
  s: String;
  tmp: TStringList;
  uri: TURI;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo do
  begin
    url := FillHost(Module.RootURL, AURL);
    if MangaInfo.FHTTP.GET(url) then begin
      Result := NO_ERROR;
      tmp := TStringList.Create;
      lastPage := 0;
      with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
        try
          coverLink := XPathString('//div[@class="row"]//img/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1');
          summary := XPathString('//*[@class="infom"]/div/div[1]/p');
          authors := XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Autor") and not(contains(.,"No Disponible"))]/substring-after(normalize-space(.),": ")');
          artists := XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Artist") and not(contains(.,"No Disponible"))]/substring-after(normalize-space(.),": ")');
          status := MangaInfoStatusIfPos(XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Estado")]'), 'Activo', 'Finalizado');
          genres := XPathString('//*[@class="panel-footer" and contains(.,"Géneros")]/string-join(.//a,", ")');
          XPathHREFAll('//table[contains(@class, "table")]//h4[@class="title"]/a', chapterLinks, chapterName);

          s := XPathString('//script[contains(., "php_pagination")]');
          if s <> '' then begin
            s := GetBetween('php_pagination(', ');', s);
            tmp.Delimiter := ',';
            tmp.DelimitedText := s;
            totalChapters := StrToIntDef(tmp[4], 0);
            perPage := StrToIntDef(tmp[5], 1);
            lastPage := Math.ceil(float(totalChapters) / float(perPage));
          end;
        finally
          tmp.Free;
          Free;
        end;

      for i := 2 to lastPage do begin
        uri := ParseURI(url);
        uri.Path := uri.Path + 'p/' + IntToStr(i);
        s := EncodeURI(uri);
        if MangaInfo.FHTTP.GET(s) then begin
           with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
           try
             XPathHREFAll('//table[contains(@class, "table")]//h4[@class="title"]/a', chapterLinks, chapterName);
           finally
             Free;
           end;
        end;
      end;

      InvertStrings([chapterLinks, chapterName]);
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  pages: Integer;
  pageFormat, pageFormat2: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL.Replace('/c/','/leer/'))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do try
        pageFormat := XPathString('//script[contains(.,"konekomangareader")]/substring-before(substring-after(substring-after(.,"setup"),","),");")');
        if pageFormat <> '' then begin
          pageFormat := ReplaceString(pageFormat, ':remote', ':''remote''');
          ParseHTML(pageFormat);
          pages := StrToIntDef(XPathString('json(*)//pages'), 0);
          pageFormat := XPathString('substring-before(json(*)//pageFormat,"{pnumber}")');
          pageFormat2 := XPathString('substring-after(json(*)//pageFormat,"{pnumber}")');
          if pageFormat <> '' then
            for pages := 1 to pages do
              PageLinks.Add(pageFormat + IntToStr(pages) + pageFormat2);
        end;
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
    Website := 'KuManga';
    RootURL := 'http://www.kumanga.com';
    Category := 'Spanish';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
