unit LeoManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/directorio-manga?orden=alfabetico';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(SeparateRight(
          XPathString('//ul[@class="pagination"]/li[last()-1]/a/@href'), 'pagina='), 1);
      finally
        Free;
      end;
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
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s += '&pagina=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//ul[@class="list-inline reset-floats"]/li/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(Trim(XPathString('div//h2/text()[1]', v.toNode)));
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
  s, t: String;
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//img[@class="manga-picture"]/@src'));
          if title = '' then
          begin
            title := XPathString('//*[@id="page-manga"]/h1');
            if RightStr(title, 6) = ' Manga' then
              SetLength(title, Length(title) - 6);
          end;
          summary := XPathString('//*[@id="page-manga"]/h2[.="Sinopsis"]/following-sibling::p/text()');
          authors := XPathStringAll('//*[@id="page-manga"]/div[@class="row"]/div/strong[.="Autor:"]/following-sibling::a');
          genres := XPathStringAll('//*[@id="page-manga"]/div[@class="row"]/div/strong[.="Géneros:"]/following-sibling::a');
          for v in XPath('//ul[@class="list-unstyled caps-list"]/li') do
          begin
            s := Trim(XPathString('div[1]/h3/text()[2]', v.toNode));
            for x in XPath('div[2]/ul/li/a', v.toNode) do
            begin
              chapterLinks.Add(x.toNode.getAttribute('href'));
              t := x.toString;
              if s <> '' then
                t := s + ' ' + t;
              chapterName.Add(t);
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
  s, p: String;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//a[contains(@class,"btn")][.="Online"]/@href');
          if s = '' then Exit;
          if GET(MaybeFillHost(Module.RootURL, s)) then
          begin
            ParseHTML(Document);
            s := XPathString('//*[@id="read-chapter"]/@name');
            if s <> '' then
              s := AppendURLDelim(MaybeFillHost(Module.RootURL, s));
            p := XPathString('//*[@id="read-chapter"]/@pos');
            if p <> '' then
            begin
              ExtractStrings([';'],[], PChar(p), PageLinks, False);
              if (s <> '') and (PageLinks.Count > 0) then
                for i := 0 to PageLinks.Count - 1 do
                  PageLinks[i] := s + PageLinks[i];
            end;
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
    Website := 'LeoManga';
    RootURL := 'http://www.leomanga.com';
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
