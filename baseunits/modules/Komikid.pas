unit Komikid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/changeMangaList?type=text';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//ul[@class="manga-list"]/li/a') do
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="content"]//img/@src'));
          if title = '' then title := XPathString('//div[@class="content"]//h5');
          authors := XPathStringAll('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Author")]/following-sibling::dd[1]/*');
          artists := XPathString('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Artist")]/following-sibling::dd[1]');
          genres := XPathStringAll('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Categories")]/following-sibling::dd[1]/*');
          status := MangaInfoStatusIfPos(XPathString(
            '//dl[@class="dl-horizontal"]/dt[starts-with(.,"Status")]/following-sibling::dd[1]'),
            'Ongoing',
            'Completed');
          summary := XPathString('//*[@class="well"]/p');
          for v in XPath('//ul[@class="chapters"]/li/h5') do
          begin
            chapterLinks.Add(XPathString('a/@href', v));
            chapterName.Add(XPathString('a||": "||em', v));
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
          XPathStringAll('//*[@id="all"]/img/@data-src', PageLinks);
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
    Website := 'Komikid';
    RootURL := 'http://www.komikid.com';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
