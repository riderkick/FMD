unit HentaiFox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, RegExpr;

implementation

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL) then
  begin
    Result := NO_ERROR;
    Page := StrToIntDef(XPathString('//*[@class="pagination"]/a[last()-1]', MangaInfo.FHTTP.Document), 1);
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/pag/' + IncStr(AURL) + '/') then
  begin
    Result := NO_ERROR;
    XPathHREFAll('//*[@class="galleries_overview"]//*[contains(@class,"item")]/a', MangaInfo.FHTTP.Document, ALinks, ANames);
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
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="cover"]/img/@src'));
          if title = '' then title := XPathString('//*[@class="info"]/h1');
          artists := XPathString('//*[@class="info"]/span[starts-with(.,"Artist")]/substring-after(.,": ")');
          genres := XPathString('//*[@class="info"]/string-join(./span/a,", ")');
          chapterLinks.Add(url);
          chapterName.Add(title);
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
      XPathStringAll('//*[@class="gallery"]//img/@data-src', Document, PageLinks);
      if PageLinks.Count <> 0 then
      begin
        for i := 0 to PageLinks.Count - 1 do
        begin
          if LeftStr(PageLinks[i], 2) = '//' then
            PageLinks[i] := 'https:' + PageLinks[i];
        end;
        PageLinks.Text := ReplaceRegExpr('(?i)(/\d+)t\.', PageLinks.Text, '$1.', True);
      end;
    end;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'HentaiFox';
    RootURL := 'https://hentaifox.com';
    Category := 'H-Sites';
    SortedList := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
