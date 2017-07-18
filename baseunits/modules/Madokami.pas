unit Madokami;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  accountmanagerdb, XQueryEngineHTML, httpsendthread, synacode, RegExpr, fpjson;

implementation

uses FMDVars;

const
  modulename = 'Madokami';
  urlroot = 'https://manga.madokami.al';
  madokamilist = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  madokamiotherlist: array [0..9] of String = (
    '/Manga/_Autouploads/AutoUploaded%20from%20Assorted%20Sources',
    '/Manga/_Autouploads/ComicWalker',
    '/Manga/Oneshots',
    '/Manga/Non-English/Bahasa%20Indonesia',
    '/Manga/Non-English/Brazilian%20Portuguese',
    '/Manga/Non-English/Deutsch',
    '/Manga/Non-English/Fran%C3%A7ais',
    '/Manga/Non-English/Italian',
    '/Manga/Non-English/Russian',
    '/Manga/Non-English/Spanish'
    );

var
  madokamiauth: String = '';
  locklogin: TRTLCriticalSection;
  madokamiulist: array of TStrings;

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
      madokamiauth := 'Authorization: Basic ' + Base64Encode(Account.Username[modulename] + ':' + Account.Password[modulename]);
      AHTTP.Headers.Add(madokamiauth);
      if AHTTP.GET(urlroot) then begin
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

procedure SetAuth(const AHTTP: THTTPSendThread);
begin
  AHTTP.Cookies.Text := Account.Cookies[modulename];
  if AHTTP.Cookies.Count <> 0 then
  begin
    if madokamiauth = '' then
      madokamiauth := 'Authorization: Basic ' + Base64Encode(Account.Username[modulename] + ':' + Account.Password[modulename]);
    AHTTP.Headers.Add(madokamiauth);
  end;
end;

function GETWithLogin(const AHTTP: THTTPSendThread; AURL: String): Boolean;
begin
  Result := False;
  SetAuth(AHTTP);
  AHTTP.FollowRedirection := False;
  Result := AHTTP.GET(AURL);
  if (AHTTP.ResultCode > 400) and (AHTTP.Headers.Values['WWW-Authenticate'] = ' Basic') then
  begin
    if Login(AHTTP) then
      Result := AHTTP.GET(AURL);
  end;
end;

procedure ClearMadokamiUlist;
var
  i: Integer;
begin
  if Length(madokamiulist) <> 0 then begin
    for i := Low(madokamiulist) to High(madokamiulist) do
      FreeAndNil(madokamiulist[i]);
    SetLength(madokamiulist, 0);
  end;
end;

function BeforeUpdateList(const Module: TModuleContainer): Boolean;
var
  i: Integer;
begin
  Result := True;
  ClearMadokamiUlist;
  SetLength(madokamiulist, Length(madokamilist));
  for i := Low(madokamiulist) to High(madokamiulist) do
    madokamiulist[i] := TStringList.Create;
end;

function AfterUpdateList(const Module: TModuleContainer): Boolean;
begin
  Result := True;
  ClearMadokamiUlist;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  if  workPtr < Length(madokamilist) then begin
    if GETWithLogin(MangaInfo.FHTTP, Module.RootURL + '/Manga/' + madokamilist[WorkPtr+1]) then begin
      XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a/@href', MangaInfo.FHTTP.Document, madokamiulist[WorkPtr]);
      Page := madokamiulist[WorkPtr].Count;
    end;
  end
  else
    Page := 1;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;

  procedure GetList;
  var
    v: IXQValue;
    s: String;
  begin
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do try
      for v in XPath('//table[@id="index-table"]/tbody/tr/td[1]/a') do
      begin
        s := v.toString;
        if Length(s) > 1 then
        begin
          if s[Length(s)] = '/' then
            SetLength(s, Length(s) - 1);
          if LowerCase(LeftStr(s, 4)) = '.txt' then
            s := '';
        end;
        if s <> '' then
        begin
          ALinks.Add(v.toNode.getAttribute('href'));
          ANames.Add(s);
        end;
      end;
    finally
      Free;
    end;
  end;

var
  workPtr, i: Integer;
  u: String;
  l1: TStrings;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  workPtr := StrToIntDef(AURL, 0);
  if Module.CurrentDirectoryIndex < Length(madokamilist) then
    u := MaybeFillHost(Module.RootURL, madokamiulist[Module.CurrentDirectoryIndex][workPtr])
  else
    u := Module.RootURL + madokamiotherlist[Module.CurrentDirectoryIndex-Length(madokamilist)];
  if GETWithLogin(MangaInfo.FHTTP, u) then begin
    Result := NO_ERROR;
    if Module.CurrentDirectoryIndex < Length(madokamilist) then begin
      l1 := TStringList.Create;
      try
        XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a/@href', MangaInfo.FHTTP.Document, l1);
        for i := 0 to l1.Count - 1 do begin
          if MangaInfo.Thread.IsTerminated then Break;
          if GETWithLogin(MangaInfo.FHTTP, MaybeFillHost(Module.RootURL, l1[i]))  then
            GetList;
        end;
      finally
        l1.Free;
      end;
    end
    else
      GetList;
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
  if GETWithLogin(MangaInfo.FHTTP, MaybeFillHost(Module.RootURL, AURL)) then begin
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
  with DownloadThread.Task.Container do begin
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
    SaveImageStreamToFile(DownloadThread.FHTTP, APath, AName);
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := modulename;
    RootURL := urlroot;
    AccountSupport := True;
    //MaxTaskLimit := 1;
    //MaxConnectionLimit := 4;
    TotalDirectory := Length(madokamilist) + Length(madokamiotherlist);
    OnBeforeUpdateList := @BeforeUpdateList;
    OnAfterUpdateList := @AfterUpdateList;
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
  ClearMadokamiUlist;
  DoneCriticalsection(locklogin);

end.
