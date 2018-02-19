unit MangaHere;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/mangalist/';
  imagepath = '//*[@id="viewer"]//img[@id="image"]/@src';

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  Result := NET_PROBLEM;
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//a[@class="manga_info"]') do
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
    if GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="manga_detail"]//img[@class="img"]/@src'));
          if title = '' then title := XPathString('//meta[@property="og:title"]/@content');
          authors := SeparateRight(XPathString('//*[@class="detail_topText"]/li[starts-with(.,"Author")]'), ':');
          artists := SeparateRight(XPathString('//*[@class="detail_topText"]/li[starts-with(.,"Artist")]'), ':');
          genres := SeparateRight(XPathString('//*[@class="detail_topText"]/li[starts-with(.,"Genre")]'), ':');
          status := MangaInfoStatusIfPos(XPathString(
            '//*[@class="detail_topText"]/li[starts-with(.,"Status")]'),
            'Ongoing',
            'Completed');
          summary := XPathString('//*[@class="detail_topText"]/li/p[@id="show"]/text()');
          for v in XPath('//*[@class="detail_list"]/ul/li/span[@class="left"]') do
          begin
            chapterLinks.Add(XPathString('a/@href', v));
            chapterName.Add(XPathString('string-join((a,span,text()[3])," ")', v));
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
  with DownloadThread, FHTTP, Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(MaybeFillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageNumber := XPath('(//span[@class="right"]/select)[1]/option[not(.="Featured")]').Count;
          PageLinks.Add(XPathString(imagepath));
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
  with DownloadThread, FHTTP, Task.Container do
  begin
    if (WorkId = PageLinks.Count - 1) and (Pos('/featured.', AURL) <> 0) then
    begin
      PageLinks.Delete(WorkId);
      Exit;
    end;

    s := AppendURLDelim(MaybeFillHost(Module.RootURL, AURL));
    if WorkId > 0 then
      s := s + IncStr(WorkId) + '.html';
    if GET(s) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          PageLinks[WorkId] := XPathString(imagepath);
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
    Website := 'MangaHere';
    RootURL := 'http://www.mangahere.cc';
    Category := 'English';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
