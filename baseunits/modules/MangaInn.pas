unit MangaInn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, synautil, FMDVars;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  s := ALPHA_LIST[Module.CurrentDirectoryIndex + 1];
  if MangaInfo.FHTTP.GET(Module.RootURL + '/manga-list/' + s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        updateList.CurrentDirectoryPageNumber := 1;
        XPathHREFAll('//ul[contains(@class, "manga-list")]/li/a', ALinks, ANames);
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
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
          coverLink := XPathString('//div[contains(@class, "manga-info")]//img[@class="img-responsive mobile-img"]/resolve-uri(@src)');
          title := XPathString('//title');
          if Pos(' - Read ', title) <> 0 then
            title := Trim(GetBetween('- Read ', ' Online at', title));
          status := MangaInfoStatusIfPos(
            XPathString('//dt[starts-with(.,"Status")]/following-sibling::dd[1]'),
            'Ongoing',
            'Complete');
          authors := XPathString('//dt[starts-with(.,"Author")]/following-sibling::dd[1]');
          artists := XPathString('//dt[starts-with(.,"Artist")]/following-sibling::dd[1]');
          genres := XPathStringAll('//dt[starts-with(.,"Categories")]/following-sibling::dd[1]/a');
          summary := Trim(XPathString('//div[contains(@class, "manga-info")]//div[contains(@class, "note")]'));
          for v in XPath('//div[@id="chapter_list"]/ul/li/a') do
          begin
            chapterLinks.Add(v.toNode().getAttribute('href'));
            chapterName.Add(XPathString('span[1]', v));
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
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL + '/all-pages')) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//script[contains(.,"var images = [")]');
          if s <> '' then
          begin
            s := '[' + GetBetween('var images = [', '];', s) + ']';
            ParseHTML(s);
            XPathStringAll('json(*)().url', PageLinks);
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
    Website := 'MangaInn';
    RootURL := 'http://www.mangainn.net';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    TotalDirectory := Length(ALPHA_LIST);
  end;
end;

initialization
  RegisterModule;

end.
