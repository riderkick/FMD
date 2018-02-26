unit Mangacan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + '/daftar-komik-manga-bahasa-indonesia.html') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        XPathHREFAll('//*[@class="series_col"]//li/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          if title = '' then title := XPathString('//h1/substring-before(.," Indonesia|Baca")');
          XPathHREFAll('//table[@class="updates"]//td/a', chapterLinks, chapterName);
          if chapterLinks.Count <> 0 then
            chapterLinks.Text := StringReplace(chapterLinks.Text, '-1.htm', '.htm', [rfIgnoreCase, rfReplaceAll]);
          InvertStrings([chapterLinks,chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
      try
        for v in XPath('//*[@id="imgholder"]//img/@src') do
          PageLinks.Add(MaybeFillHost(Module.RootURL, v.toString));
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
    Website := 'Mangacan';
    RootURL := 'http://www.mangacanblog.com';
    Category := 'Indonesian';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
