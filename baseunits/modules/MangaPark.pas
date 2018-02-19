unit MangaPark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synautil, strutils;

implementation

const
  dirurl = '/search?orderby=add';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(Module.RootURL + dirurl) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        Page := StrToIntDef(SeparateRight(
          XPathString('//ul[@class="paging full"]/li[last()-2]/a/@href'), 'page='), 1);
      finally
        Free;
      end;
  end;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL + dirurl;
  if AURL <> '0' then
    s += '&page=' + IncStr(AURL);
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@class="manga-list"]/div//td/h2/a') do
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
  v, x: IXQValue;
  s, t: String;
  i: Integer;
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
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//*[@class="cover"]/img/@src'));
          if title = '' then title := Trim(XPathString('//*[@class="content"]//h1'));
          if AnsiEndsStr(' Manga', title) then title := LeftStr(title, Length(title) - 6)
          else if AnsiEndsStr(' Manhwa', title) then title := LeftStr(title, Length(title) - 7);
          authors := XPathStringAll('//table[@class="attr"]//tr/th[.="Author(s)"]/following-sibling::td/a');
          artists := XPathStringAll('//table[@class="attr"]//tr/th[.="Artist(s)"]/following-sibling::td/a');
          genres := XPathStringAll('//table[@class="attr"]//tr/th[.="Genre(s)"]/following-sibling::td/a');
          status := MangaInfoStatusIfPos(XPathString(
            '//table[@class="attr"]//tr/th[.="Status"]/following-sibling::td'),
            'Ongoing',
            'Completed');
          summary := XPathString('//*[@class="content"]/p[@class="summary"]');
          for v in XPath('//*[@id="list"]/*[contains(@id,"stream")]') do
          begin
            s := Trim(XPathString('h3', v.toNode));
            x := XPath('div/ul/li/span', v.toNode);
            for i := x.Count downto 1 do
            begin
              t := XPathString('a/@href', x.get(i).toNode);
              if RightStr(t, 2) = '/1' then
                SetLength(t, Length(t) - 2);
              chapterLinks.Add(t);
              t := x.get(i).toString;
              if s <> '' then
                t := s + ' ' + t;
              chapterName.Add(t);
            end;
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
    s := MaybeFillHost(Module.RootURL, AURL);
    if RightStr(s, 2) = '/1' then
      SetLength(s, Length(s) - 2);
    if GET(s) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//img[@class="img"]/@src', PageLinks);
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
    Website := 'MangaPark';
    RootURL := 'http://mangapark.me';
    Category := 'English';
    SortedList := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
