unit MangaOnlineTo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread;

implementation

const
  dirurl = '/manga-list.html';

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
        for v in XPath('//div[@class="content-l"]//li/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('text()[last()]', v));
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@class="image-info"]/img/@src'));
          if title = '' then title := XPathString('//div[@class="info"]/h2/text()[last()]');
          authors := XPathString('//div[@class="info"]/div[@class="row-info"]/p/span[starts-with(.,"Author")]/following-sibling::*');
          artists := XPathString('//div[@class="info"]/div[@class="row-info"]/p/span[starts-with(.,"Artist")]/following-sibling::*');
          genres := XPathString('//div[@class="info"]/div[@class="row-info"]/p/span[starts-with(.,"Genre")]/string-join(following-sibling::*//a,", ")');
          status := MangaInfoStatusIfPos(XPathString(
            '//div[@class="info"]/div[@class="row-info"]/p/span[starts-with(.,"Status")]/following-sibling::*'),
            'Ongoing',
            'Completed');
          summary := XPathString('//div[@class="info"]/div[@class="row-info"]/p[starts-with(.,"Plot Summary")]/following-sibling::p/text()');
          for v in XPath('//div[@class="list-chapter"]//li/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(XPathString('string-join(text(),"")', v));
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
          XPathStringAll('//*[@id="list-img"]/img/@src', PageLinks);
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
    Website := 'MangaOnlineTo';
    RootURL := 'http://mangaon.net';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
