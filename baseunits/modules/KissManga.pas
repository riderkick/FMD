unit KissManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, Cloudflare, RegExpr, synautil;

implementation

const
  kissmangadirurl = '/MangaList/Newest';
  readcomiconlinedirurl = '/ComicList/Newest';

var
  kissmangacookies: String = '';
  kissmangalockget: TRTLCriticalSection;
  readcomiconlinecookies: String = '';
  readcomiconlinelockget: TRTLCriticalSection;

function GETWithCookie(const AHTTP: THTTPSendThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
begin
  if Module.Website = 'KissManga' then
    Result := Cloudflare.GETCF(AHTTP, AURL, kissmangacookies, kissmangalockget)
  else if Module.Website = 'ReadComicOnline' then
    Result := Cloudflare.GETCF(AHTTP, AURL, readcomiconlinecookies, readcomiconlinelockget);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
var
  s: String;
begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if Module.Website = 'KissManga' then
    s := s + kissmangadirurl
  else if Module.Website = 'ReadComicOnline' then
    s := s + readcomiconlinedirurl;
  if GETWithCookie(MangaInfo.FHTTP, s, Module) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        s := XPathString('//ul[@class="pager"]/li[last()]/a/@href');
        if s <> '' then begin
          s := ReplaceRegExpr('^.*=(\d+)$', s, '$1', True);
          Page := StrToIntDef(s, 1);
        end;
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
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  s := Module.RootURL;
  if Module.Website = 'KissManga' then
    s := s + kissmangadirurl
  else if Module.Website = 'ReadComicOnline' then
    s := s + readcomiconlinedirurl;
  if AURL <> '0' then
    s := s + '?page=' + IncStr(AURL);
  if GETWithCookie(MangaInfo.FHTTP, s, Module) then begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[@class="listing"]/tbody/tr/td[1]/a') do begin
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
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if GETWithCookie(MangaInfo.FHTTP, FillHost(Module.RootURL, AURL), Module) then begin
    Result := NO_ERROR;
    with MangaInfo.mangaInfo, TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        coverLink := XPathString('//div[@id="rightside"]//img/@src');
        if title = '' then
        begin
          title := XPathString('//title');
          if title <> '' then
          begin
            if Pos('manga | Read', title) <> 0 then
              title := SeparateLeft(title, 'manga | Read')
            else if Pos('comic | Read', title) <> 0 then
              title := SeparateLeft(title, 'comic | Read');
          end;
        end;
        genres := SeparateRight(XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Genres:")]'), ':');
        authors := SeparateRight(XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Author:")]'), ':');
        artists := SeparateRight(XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Artist:")]'), ':');
        status := MangaInfoStatusIfPos(XPathString(
          '//div[@class="barContent"]/div/p[starts-with(.,"Status:")]'),
          'Ongoing',
          'Completed');
        summary := XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Summary:")]//following-sibling::p[1]');
        for v in XPath('//table[@class="listing"]/tbody/tr/td/a') do begin
          chapterLinks.Add(v.toNode.getAttribute('href'));
          s := v.toNode.getAttribute('title');
          if LeftStr(s, 5) = 'Read ' then
            Delete(s, 1, 5);
          if RightStr(s, 7) = ' online' then
            SetLength(s, Length(s) - 7)
          else if RightStr(s, 29) = ' comic online in high quality' then
            SetLength(s, Length(s) - 29);
          chapterName.Add(s);
        end;
        InvertStrings([chapterLinks, chapterName]);
      finally
        Free;
      end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  source: TStringList;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GETWithCookie(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL), Module) then begin
      Result := True;
      source := TStringList.Create;
      try
        source.LoadFromStream(Document);
        if source.Count > 0 then
          for i := 0 to source.Count - 1 do begin
            if Pos('lstImages.push', source[i]) > 0 then
              PageLinks.Add(GetBetween('.push("', '");', source[i]));
          end;
      finally
        source.Free;
      end;
    end;
  end;
end;

procedure RegisterModule;

  procedure AddWebsiteModule(AWebsite, ARootURL: String);
  begin
    with AddModule do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      SortedList := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('KissManga', 'http://kissmanga.com');
  AddWebsiteModule('ReadComicOnline', 'http://readcomiconline.to');
end;

initialization
  InitCriticalSection(kissmangalockget);
  InitCriticalSection(readcomiconlinelockget);
  RegisterModule;

finalization
  DoneCriticalsection(kissmangalockget);
  DoneCriticalsection(readcomiconlinelockget);

end.
