unit Hentai2Read;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil;

const
  dirurl = '/hentai-list/all/any/all/last-added';
  cdnurl = 'http://static.hentaicdn.com/hentai';

implementation

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then
    Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(XPathString(
          '//ul[starts-with(@class,"pagination")]/li[last()-1]/a'), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation; const ANames, ALinks: TStringList;
  const AURL: String; const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then
    Exit(UNKNOWN_ERROR);
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s := s + '/' + IncStr(AURL) + '/';
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="img-overlay text-center"]/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(XPathString('h2/@data-title', v));
        end;
      finally
        Free;
      end;
  end;
end;

function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then
    Exit(UNKNOWN_ERROR);
  with MangaInfo.FHTTP, MangaInfo.mangaInfo do
  begin
    url := FillHost(Module.RootURL, AURL);
    if GET(url) then
    begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(cdnurl, XPathString('//img[@class="img-responsive border-black-op"]/@src'));
          title := XPathString('//h3[@class="block-title"]/a/text()');
          status := MangaInfoStatusIfPos(XPathString('//ul[contains(@class,"list-simple-mini")]/li[starts-with(.,"Status")]'));
          authors := XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li[starts-with(.,"Author")]/a');
          artists := XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li[starts-with(.,"Artist")]/a');
          genres := XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li/a');
          summary := XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li[starts-with(.,"Storyline")]/*[position()>1]');
          for v in XPath('//ul[contains(@class,"nav-chapters")]/li/div/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(XPathString('string-join(text()," ")', v.toNode));
          end;
          InvertStrings([chapterLinks, chapterName]);
        finally
          Free;
        end;
    end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then
    Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      if PageLinks.Count = 0 then
        with TXQueryEngineHTML.Create(Document) do
          try
            for v in XPath('json(//script[contains(.,"images'' :")]/substring-before(substring-after(.,"images'' : "),"]")||"]")()') do
              PageLinks.Add(cdnurl + v.toString);
            if PageLinks.Count = 0 then
              PageNumber := XPath('(//li[contains(@class, "pageDropdown")])[1]/ul/li').Count;
          finally
            Free;
          end;
    end;
  end;
end;

function GetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  s: String;
begin
  Result := False;
  if DownloadThread = nil then
    Exit;
  with DownloadThread.Task.Container, DownloadThread.FHTTP do
  begin
    s := FillHost(Module.RootURL, AURL);
    if DownloadThread.WorkId > 0 then
      s := AppendURLDelim(s) + IncStr(DownloadThread.WorkId) + '/';
    if GET(s) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          s := XPathString('//img[@id="arf-reader"]/@src');
          if s <> '' then
          begin
            s := TrimLeftChar(s, ['/']);
            PageLinks[DownloadThread.WorkId] := MaybeFillHost(cdnurl, s);
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
    Website := 'Hentai2Read';
    RootURL := 'http://hentai2read.com';
    Category := 'H-Sites';
    SortedList := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
