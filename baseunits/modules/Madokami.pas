unit Madokami;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, httpsendthread, synacode, RegExpr, fpjson;

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
  accountexist: Boolean = False;

function Login(const AHTTP: THTTPSendThread; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if Module.Account.Enabled = False then Exit;
  if (Module.Account.Username = '') or (Module.Account.Password = '') then Exit;
  if TryEnterCriticalsection(locklogin) > 0 then
    try
      Module.Account.Status := asChecking;
      AHTTP.Reset;
      AHTTP.Cookies.Clear;
      madokamiauth := 'Authorization: Basic ' +
        Base64Encode(Module.Account.Username + ':' + Module.Account.Password);
      AHTTP.Headers.Add(madokamiauth);
      if AHTTP.GET(urlroot) then begin
        //Result := AHTTP.Cookies.Values['laravel_session'] <> '';
        Result := (AHTTP.ResultCode < 400) and (AHTTP.Headers.Values['WWW-Authenticate'] = '');
        if Result then
          Module.Account.Status := asValid
        else
          Module.Account.Status := asInvalid;
      end;
    finally
      LeaveCriticalsection(locklogin);
    end
  else begin
    EnterCriticalsection(locklogin);
    try
    finally
      LeaveCriticalsection(locklogin);
    end;
  end;
  AHTTP.Reset;
end;

procedure SetAuth(const AHTTP: THTTPSendThread; const Module: TModuleContainer);
begin
  if AHTTP.Cookies.Count <> 0 then
  begin
    if madokamiauth = '' then
      madokamiauth := 'Authorization: Basic ' + Base64Encode(Module.Account.Username);
    AHTTP.Headers.Add(madokamiauth);
  end;
end;

function GETWithLogin(const AHTTP: THTTPSendThread; AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  SetAuth(AHTTP, Module);
  AHTTP.FollowRedirection := False;
  Result := AHTTP.GET(AURL);
  if ((AHTTP.ResultCode > 400) and (AHTTP.Headers.Values['WWW-Authenticate'] = ' Basic')) or
    (Pos('/login',AHTTP.Headers.Values['Location']) <> 0) then
  begin
    if Login(AHTTP, Module) then
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
  accountexist := Module.Account.Username <> '';
  if not accountexist then Exit;
  ClearMadokamiUlist;
  SetLength(madokamiulist, Length(madokamilist));
  for i := Low(madokamiulist) to High(madokamiulist) do
    madokamiulist[i] := TStringList.Create;
end;

function AfterUpdateList(const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if not accountexist then Exit;
  ClearMadokamiUlist;
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  if not accountexist then Exit;
  if  workPtr < Length(madokamilist) then begin
    if GETWithLogin(MangaInfo.FHTTP, Module.RootURL + '/Manga/' + madokamilist[WorkPtr+1], Module) then begin
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
  begin
    XPathHREFAll('//table[@id="index-table"]/tbody/tr/td[1]/a[not(ends-with(.,".txt") or ends-with(.,".zip") or ends-with(.,".rar"))]', MangaInfo.FHTTP.Document, ALinks, ANames);
  end;

var
  workPtr, i: Integer;
  u: String;
  l1: TStrings;
begin
  Result := NET_PROBLEM;
  if not accountexist then Exit;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  workPtr := StrToIntDef(AURL, 0);
  u := '';
  if Module.CurrentDirectoryIndex < Length(madokamilist) then
  begin
    if workPtr < madokamiulist[Module.CurrentDirectoryIndex].Count then
      u := MaybeFillHost(Module.RootURL, madokamiulist[Module.CurrentDirectoryIndex][workPtr]);
  end
  else
    u := Module.RootURL + madokamiotherlist[Module.CurrentDirectoryIndex-Length(madokamilist)];
  if u = '' then Exit;
  if GETWithLogin(MangaInfo.FHTTP, u, Module) then begin
    Result := NO_ERROR;
    if Module.CurrentDirectoryIndex < Length(madokamilist) then begin
      l1 := TStringList.Create;
      try
        XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a/@href', MangaInfo.FHTTP.Document, l1);
        for i := 0 to l1.Count - 1 do begin
          if MangaInfo.Thread.IsTerminated then Break;
          if GETWithLogin(MangaInfo.FHTTP, MaybeFillHost(Module.RootURL, l1[i]), Module)  then
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
  if GETWithLogin(MangaInfo.FHTTP, MaybeFillHost(Module.RootURL, AURL), Module) then begin
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
              Expression := '\.\w+\s*$';
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
    if GETWithLogin(DownloadThread.FHTTP, FillHost(Module.RootURL, AURL), Module) then begin
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
  const AURL: String; const Module: TModuleContainer): Boolean;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  Result := GETWithLogin(DownloadThread.FHTTP, AURL, Module);
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := modulename;
    RootURL := urlroot;
    Category := 'English';
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
