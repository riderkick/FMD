unit MangaDoor;

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
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//li/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('//h6',v.toNode));
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
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//*[@class="cover"]/img/@src');
          if coverLink <> '' then coverLink := coverLink;
          if title = '' then title := XPathString('//h2[@class="widget-title"]');
          status := MangaInfoStatusIfPos(
            XPathString('//dd[2]'),
            'Ongoing',
            'Complete');
          artists := XPathStringAll('//dd[2]/a');
          authors := XPathStringAll('//dd[3]');
          for v in XPath('//ul[@class="chapters"]/li/*/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
          end;
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
    Cookies.Values['viewer'] := '1';
    if GET(RemoveURLDelim(FillHost(Module.RootURL, AURL))) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//script[contains(.,"var pages = [")]');
          if s <> '' then
          begin
            s := '[' + GetBetween('var pages = [', '];', s) + ']';
            ParseHTML(s);
            XPathStringAll('json(*)().page_image', PageLinks);
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
    Website := 'MangaDoor';
    RootURL := 'http://mangadoor.com';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
