unit Madokami;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, XQueryEngineHTML, httpsendthread, synacode, RegExpr, fpjson;

implementation

const
  modulename = 'Madokami';
  urlroot = 'https://manga.madokami.com';
  madokamidirlist: array [0..12] of String = (
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

function Login(const AHTTP: THTTPSendThread): Boolean;
begin
  Result := False;
  if Account.Enabled[modulename] = False then Exit;
  if Account.Username[modulename] = '' then Exit;
  if TryEnterCriticalsection(locklogin) > 0 then
    try
      Account.Status[modulename] := asChecking;
      AHTTP.Reset;
      AHTTP.Cookies.Clear;
      AHTTP.Headers.Values['Authorization'] :=
        ' Basic ' + Base64Encode(Account.Username[modulename] +
        ':' + Account.Password[modulename]);
      if AHTTP.GET(urlroot + '/login') then begin
        //Result := AHTTP.Cookies.Values['laravel_session'] <> '';
        Result := (AHTTP.ResultCode < 400) and (AHTTP.Headers.Values['WWW-Authenticate'] = '');
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
      LeaveCriticalsection(locklogin);
    end
  else begin
    EnterCriticalsection(locklogin);
    try
      if Result then
        AHTTP.Cookies.Text := Account.Cookies[modulename];
    finally
      LeaveCriticalsection(locklogin);
    end;
  end;
  AHTTP.Reset;
end;

function GETWithLogin(const AHTTP: THTTPSendThread; AURL: String): Boolean;
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

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := Length(madokamidirlist);
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
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
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//table[@id="index-table"]/tbody/tr/td[1]/a') do
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          s := v.toString;
          if Length(s) > 1 then
            if s[Length(s)] = '/' then
              SetLength(s, Length(s) - 1);
          ANames.Add(s);
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
  i: Integer;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  if MangaInfo.FHTTP.GET(FillHost(Module.RootURL, AURL)) then begin
    Result := NO_ERROR;
    with MangaInfo.mangaInfo, TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        coverLink := XPathString('//img[@itemprop="image"]/@src');
        if title = '' then title := XPathString('//*[@class="title"]');
        if title = '' then title := XPathString('(//h1//span[@itemprop="title"])[last()]');
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
      finally
        Free;
      end;
  end;
end;

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  datapath, datafiles: String;
  i: Integer;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.manager.container do begin
    PageLinks.Clear;
    PageNumber := 0;
    if GETWithLogin(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL)) then begin
      Result := True;
      with TXQueryEngineHTML.Create(DownloadThread.FHTTP.Document) do
        try
          datapath := XPathString('//div[@id="reader"]/@data-path');
          datapath := EncodeURLElement(datapath);
          datafiles := XPathString('//div[@id="reader"]/@data-files');
          datafiles := Trim(TrimChar(datafiles, ['[', ']']));
          datafiles := JSONStringToString(datafiles);
          PageLinks.Delimiter := ',';
          PageLinks.DelimitedText := datafiles;
          if PageLinks.Count > 0 then
            for i := 0 to PageLinks.Count - 1 do
              PageLinks[i] := Module.RootURL + '/reader/image?path=' +
                datapath + '&file=' + EncodeURLElement(PageLinks[i]);
        finally
          Free;
        end;
    end;
  end;
end;

function DownloadImage(const DownloadThread: TDownloadThread;
  const AURL, APath, AName: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  if GETWithLogin(DownloadThread.FHTTP, AURL) then begin
    SaveImageStreamToFile(DownloadThread.FHTTP.Document, APath, AName);
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
