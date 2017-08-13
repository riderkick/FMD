unit MangaInn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, synautil;

implementation

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + '/MangaList') then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="mangalistItems"]/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(v.toString);
        end;
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
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//img[@itemprop="image"]/resolve-uri(@src)');
          title := XPathString('//title');
          if Pos(' - Read ', title) <> 0 then
            title := Trim(GetBetween(' - Read ', ' Online For Free', title));
          status := MangaInfoStatusIfPos(
            XPathString('//*[@class="RedHeadLabel"][starts-with(.,"Status")]/following-sibling::*[1]'),
            'Ongoing',
            'Complete');
          authors := XPathString('//*[@class="RedHeadLabel"][starts-with(.,"Author(s)")]/following-sibling::*[1]');
          artists := XPathString('//*[@class="RedHeadLabel"][starts-with(.,"Artist(s)")]/following-sibling::*[1]');
          genres := XPathString('//*[@class="RedHeadLabel"][starts-with(.,"Genre(s)")]/following-sibling::*[1]');
          summary := Trim(XPathString('//*[@class="RedHeadLabel"][starts-with(.,"Summary")]/following-sibling::*'));
          XPathHREFAll('//div[@class="content"]/div[@class="divThickBorder"][3]/table//td[1]/span/a', chapterLinks, chapterName);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
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
          PageNumber := XPath('//select[@id="cmbpages"]/option').Count;
          PageLinks.Add(XPathString('//*[@id="divimgPage"]/img/@src'));
        finally
          Free;
        end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    s := MaybeFillHost(Module.RootURL, AURL);
    if DownloadThread.WorkId > 0 then
      s += '/page_' + IntToStr(DownloadThread.WorkId + 1);
    if GET(s) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[DownloadThread.WorkId] := XPathString('//*[@id="divimgPage"]/img/@src');
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
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
