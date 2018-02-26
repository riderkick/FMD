unit ReadComics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil,Dialogs;

implementation

const
  dirurl = '/comic-list';

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
        for v in XPath('//div[@class="serie-box"]/*/li/a') do
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
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if GET(url) then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := XPathString('//img[@id="series_image"]/@src');
          if coverLink <> '' then coverLink := MaybeFillHost(Module.RootURL, coverLink);
          if title = '' then title := XPathString('//h1[@class="manga-title"]');
          status := MangaInfoStatusIfPos(
            XPathString('//*[@class="info"]/p/*[@class="data"][starts-with(.,"Status:")]/following-sibling::*[@class="desc"][1]'),
            'Ongoing',
            'Complete');
          artists := XPathStringAll('//*[@class="info"]/p/*[@class="data"][starts-with(.,"Artist:")]/following-sibling::*[@class="desc"][1]/a');
          authors := XPathStringAll('//*[@class="info"]/p/*[@class="data"][starts-with(.,"Author:")]/following-sibling::*[@class="desc"][1]/a');
          for v in XPath('//ul[@class="basic-list"]/li/a') do
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
  s,url: String;
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    url := AppendURLDelim(MaybeFillHost(Module.RootURL, AURL))+'full';
    if GET(url) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
      try
        for v in XPath('//img[@class="chapter_img"]') do
          PageLinks.Add(v.toNode.getAttribute('src'));
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
    Website := 'ReadComics';
    RootURL := 'http://readcomics.tv';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
