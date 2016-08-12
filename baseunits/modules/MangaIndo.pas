unit MangaIndo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

implementation

const
  dirurl = '/manga-list2/';

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
        for v in XPath('//div[@class="entry-content"]/div/ul/li/a') do
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
  query: TXQueryEngineHTML;
  p: LongInt;
  i: Integer;

  procedure ScanChapters;
  var
    v: IXQValue;
  begin
    with MangaInfo.mangaInfo, query do
      for v in XPath('//ul[@class="lcp_catlist"]/li/a') do
      begin
        chapterLinks.Add(v.toNode.getAttribute('href'));
        chapterName.Add(v.toString);
      end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := MaybeFillHost(Module.RootURL, AURL);
    if GET(url) then begin
      Result := NO_ERROR;
      query := TXQueryEngineHTML.Create(Document);
      with query do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//div[@id="main"]//img/@src'));
          if title = '' then title := XPathString('//div[@class="title"]/h2');
          artists := Trim(SeparateRight(XPathString('//div[@id="main"]//div[@class="entry-content"]/p/text()[starts-with(.,"Artist:")]'),':'));
          authors := Trim(SeparateRight(XPathString('//div[@id="main"]//div[@class="entry-content"]/p/text()[starts-with(.,"Author:")]'),':'));
          genres := Trim(SeparateRight(XPathString('//div[@id="main"]//div[@class="entry-content"]/p/text()[starts-with(.,"Genre:")]'),':'));
          status := MangaInfoStatusIfPos(XPathString(
            '//div[@id="main"]//div[@class="entry-content"]/p/text()[starts-with(.,"Genre:")]'),
            'Ongoing',
            'Completed');
          summary := Trim(SeparateRight(XPathString('//div[@id="main"]//div[@class="entry-content"]/p[starts-with(.,"Synopsis:")]'), ':'));
          ScanChapters;
          p := StrToIntDef(XPathString('(//ul[@class="lcp_paginator"]/li/a[.!=">>"])[last()]') , 1);
          if p > 1 then
            for i := 2 to p do
            begin
              if Thread.IsTerminated then Break;
              if GET(AppendURLDelim(url) + '?lcp_page0=' + IntToStr(i)) then
              begin
                ParseHTML(Document);
                ScanChapters;
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
          XPathStringAll('//div[@id="main"]//div[@class="entry-content"]//img/@src', PageLinks);
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
    Website := 'MangaIndo';
    RootURL := 'http://mangaindo.web.id';
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
