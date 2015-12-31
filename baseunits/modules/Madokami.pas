unit Madokami;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, synacode, RegExpr, fpjson;

implementation

const
  modulename = 'Madokami';
  urlroot = 'https://manga.madokami.com';
  madokamidirlist: array [0..12] of string = (
    '/Manga/%23%20-%20F',
    '/Manga/G%20-%20M',
    '/Manga/N%20-%20Z',
    '/Manga/_Autouploads/AutoUploaded%20from%20Assorted%20Sources',
    '/Manga/_Autouploads/ComicWalker',
    '/Manga/Non-English/Bahasa%20Indonesia',
    '/Manga/Non-English/Brazilian%20Portuguese',
    '/Manga/Non-English/Fran%C3%A7ais',
    '/Manga/Non-English/Italian',
    '/Manga/Non-English/Russian',
    '/Manga/Non-English/Spanish',
    '/Manga/_Doujinshi',
    '/Raws'
    );

var
  locklogin: TRTLCriticalSection;
  onlogin: Boolean = False;

function Login(var AHTTP: THTTPSendThread): Boolean;
begin
  Result := False;
  if Account.Enabled[modulename] = False then Exit;
  if Account.Username[modulename] = '' then Exit;
  if TryEnterCriticalsection(locklogin) > 0 then
    try
      onlogin := True;
      Account.Status[modulename] := asChecking;
      AHTTP.Reset;
      AHTTP.Cookies.Clear;
      AHTTP.Headers.Values['Authorization'] :=
        ' Basic ' + Base64Encode(Account.Username[modulename] +
        ':' + Account.Password[modulename]);
      if AHTTP.GET(urlroot + '/login') then begin
        Result := AHTTP.Cookies.Values['laravel_session'] <> '';
        if Result then begin
          Account.Cookies[modulename] := AHTTP.GetCookies;
          Account.Status[modulename] := asValid;
        end
        else begin
          Account.Cookies[modulename] := '';
          Account.Status[modulename] := asInvalid;
        end;
        Account.Save;
      end;
    finally
      onlogin := False;
      LeaveCriticalsection(locklogin);
    end
  else
    while onlogin do Sleep(1000);
  AHTTP.Reset;
  if Result then begin
    AHTTP.Cookies.Text := Account.Cookies[modulename];
  end;
end;

function GETWithLogin(var AHTTP: THTTPSendThread; AURL: String): Boolean;
begin
  Result := False;
  AHTTP.Cookies.Text := Account.Cookies[modulename];
  AHTTP.FollowRedirection := False;
  Result := AHTTP.GET(AURL);
  if (AHTTP.ResultCode > 400) and (AHTTP.Headers.Values['WWW-Authenticate'] = ' Basic') then
  begin
    if Login(AHTTP) then
      Result := AHTTP.GET(AURL);
  end;
end;

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := Length(madokamidirlist);
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  currentdir: Integer;
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  currentdir := StrToIntDef(AURL, 0);
  if currentdir > Length(madokamidirlist) then Exit;
  if MangaInfo.FHTTP.GET(Module.RootURL + madokamidirlist[currentdir]) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      for v in query.XPath('//table[@id="index-table"]/tbody/tr/td[1]/a') do
      begin
        s := v.toString;
        if Length(s) > 1 then begin
          if s[Length(s)] = '/' then SetLength(s, Length(s) - 1);
          ANames.Add(v.toString);
          ALinks.Add(v.toNode.getAttribute('href'));
        end;
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  query: TXQueryEngineHTML;
  v: IXQValue;
  i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    query := TXQueryEngineHTML.Create;
    try
      query.ParseHTML(StreamToString(MangaInfo.FHTTP.Document));
      with MangaInfo.mangaInfo, query do begin
        coverLink := XPathString('//img[@itemprop="image"]/@src');
        if title = '' then title := XPathString('//*[@class="title"]');
        authors := XPathString('//*[@itemprop="author"]');
        v := XPath('//div[@class="genres"]/a');
        if v.Count > 0 then begin
          genres := '';
          for i := 0 to v.Count - 1 do
            AddCommaString(genres, v.get(i).toString);
        end;
        for v in XPath('//table[@id="index-table"]/tbody/tr') do begin
          chapterLinks.Add(XPathString('td/a[contains(text(),"Read")]/@href', v.toNode));
          chapterName.Add(XPathString('td[1]/a', v.toNode));
        end;
        if chapterName.Count > 0 then
          with TRegExpr.Create do
            try
              Expression := '\.\w+';
              for i := 0 to chapterName.Count - 1 do
                chapterName[i] := Replace(chapterName[i], '', False);
            finally
              Free;
            end;
      end;
    finally
      query.Free;
    end;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const AURL: String;
  Module: TModuleContainer): Boolean;
var
  query: TXQueryEngineHTML;
  datapath, datafiles: String;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageContainerLinks.Clear;
    PageNumber := 0;
    if GETWithLogin(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      query := TXQueryEngineHTML.Create;
      try
        query.ParseHTML(StreamToString(Document));
        datapath := query.XPathString('//div[@id="reader"]/@data-path');
        datapath := EncodeURLElement(datapath);
        datafiles := query.XPathString('//div[@id="reader"]/@data-files');
        datafiles := Trim(TrimChar(datafiles, ['[', ']']));
        datafiles := JSONStringToString(datafiles);
        PageLinks.Delimiter := ',';
        PageLinks.DelimitedText := datafiles;
        if PageLinks.Count > 0 then
          for i := 0 to PageLinks.Count - 1 do
            PageLinks[i] := Module.RootURL + '/reader/image?path=' +
              datapath + '&file=' + EncodeURLElement(PageLinks[i]);
      finally
        query.Free;
      end;
    end;
  end;
end;

function DownloadImage(var DownloadThread: TDownloadThread;
    const AURL, APath, AName, APrefix: String; Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  if GETWithLogin(DownloadThread.FHTTP, AURL) then begin
    SaveImageStreamToFile(DownloadThread.FHTTP.Document, APath, AName + APrefix);
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := modulename;
    RootURL := urlroot;
    AccountSupport := True;
    MaxTaskLimit := 1;
    MaxConnectionLimit := 4;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnDownloadImage := @DownloadImage;
    OnLogin := @Login;
  end;
end;

initialization
  InitCriticalSection(locklogin);
  RegisterModule;

finalization
  DoneCriticalsection(locklogin);

end.
