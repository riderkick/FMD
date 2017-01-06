unit MangaLife;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  XQueryEngineHTML, accountmanagerdb, httpsendthread, synacode;

implementation

uses
  synautil;

const
  dirURL = '/directory/';
  diralpha = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  dirURLmangasee = '/directory.php';
var
  MMangaSee,
  MMangaTraders: TModuleContainer;
  MangaTradersLockLogin: TRTLCriticalSection;
  MangaTradersOnLogin: Boolean;

function MangaTradersLogin(const AHTTP: THTTPSendThread): Boolean;
var
  s: String;
begin
  Result := False;
  if Account.Enabled[MMangaTraders.Website] = False then Exit;
  if TryEnterCriticalsection(MangaTradersLockLogin) > 0 then
    try
      MangaTradersOnLogin := True;
      Account.Status[MMangaTraders.Website] := asChecking;
      SendLog('MangaTraders: post login');
      s :=
        'EmailAddress=' + EncodeURLElement(Account.Username[MMangaTraders.Website]) +
        '&Password=' + EncodeURLElement(Account.Password[MMangaTraders.Website]) +
        '&RememberMe=1';
      AHTTP.Headers.Clear;
      if AHTTP.POST(MMangaTraders.RootURL + '/auth/process.login.php', s) then
      begin
        s := StreamToString(AHTTP.Document);
        SendLog('MangaTraders: login result = ' + s);
        if LowerCase(s) = 'ok' then
        begin
          Account.Status[MMangaTraders.Website] := asValid;
          Account.Cookies[MMangaTraders.Website] := AHTTP.Cookies.Text;
        end
        else
          Account.Status[MMangaTraders.Website] := asInvalid;
      end
      else
        Account.Status[MMangaTraders.Website] := asUnknown;
      Result := Account.Status[MMangaTraders.Website] = asValid;
    finally
      MangaTradersOnLogin := False;
      LeaveCriticalsection(MangaTradersLockLogin);
    end
  else
  begin
    while MangaTradersOnLogin do Sleep(1000);
    Result := Account.Status[MMangaTraders.Website] = asValid;
    if Result then
      AHTTP.Cookies.Text := Account.Cookies[MMangaTraders.Website];
  end;
end;

function GETMangaTraders(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
var
  accstat: TAccountStatus;
begin
  Result := False;
  if Account.Enabled[MMangaTraders.Website] then
  begin
    accstat := Account.Status[MMangaTraders.Website];
    if accstat = asValid then
      AHTTP.Cookies.AddText(Account.Cookies[MMangaTraders.Website])
    else
    if accstat in [asChecking, asUnknown] then
      Result := MangaTradersLogin(AHTTP);
  end;
  Result := AHTTP.GET(AURL);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  if (Module = MMangaSee) or
     (Module = MMangaTraders) then
    Page := Length(diralpha)
  else
    Page := 1;
end;

function fixcleanurl(const u: string): string;
begin
  Result := u;
  if Result = '' then Exit;
  while Result[1] = '.' do
    Delete(Result, 1, 1);
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  v: IXQValue;
  s: String;
begin
  Result := NET_PROBLEM;
  s := Module.RootURL;
  if Module = MMangaSee then
    s += dirURLmangasee
  else
    s+=dirURL;
  if AURL <> '0' then
  begin
    if Module = MMangaSee then
      s += '?c='
    else
    if Module = MMangaTraders then
      s += '?q=';
    s += diralpha[StrToIntDef(AURL, 0) + 1]
  end;
  if MangaInfo.FHTTP.GET(s) then
  begin
    Result := NO_ERROR;
    with TXQueryEngineHTML.Create(MangaInfo.FHTTP.Document) do
      try
        for v in XPath('//*[@id="content"]//a') do
        begin
          ALinks.Add(fixcleanurl(v.toNode.getAttribute('href')));
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
  r: Boolean;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if Module = MMangaTraders then
      r := GETMangaTraders(MangaInfo.FHTTP, url)
    else
      r := GET(url);
    if r then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//meta[@property="og:image"]/@content'));
          if title = '' then title := XPathString('//*[@class="row"]//h1');
          authors := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Author")]'), ':');
          artists := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Artist")]'), ':');
          status := XPathString('//*[@class="row"][starts-with(.,"Scanlation Status")]');
          if status = '' then
            status := XPathString('//*[@class="row"][starts-with(.,"Status")]');
          if status <> '' then
            status := MangaInfoStatusIfPos(status);
          genres := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Genre")]'), ':');
          summary := Trim(SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Description")]'), ':'));
          for v in XPath('//div[@class="list chapter-list"]/a') do
          begin
            chapterLinks.Add(v.toNode.getAttribute('href'));
            chapterName.Add(v.toString);
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
  v: IXQValue;
begin
  Result := False;
  if DownloadThread = nil then Exit;
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          for v in XPath('//img[@class="CurImage"]') do
            PageLinks.Add(v.toNode.getAttribute('src'));
        finally
          Free;
        end;
    end;
  end;
end;

procedure RegisterModule;

  function AddWebsiteModule(AWebsite, ARootURL: String): TModuleContainer;
  begin
    Result := AddModule;
    with Result do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaLife', 'http://mangalife.org');
  MMangaSee := AddWebsiteModule('MangaSee', 'http://mangaseeonline.net');
  MMangaTraders := AddWebsiteModule('MangaTraders', 'http://mangatraders.biz');
  MMangaTraders.AccountSupport := True;
  MMangaTraders.OnLogin := @MangaTradersLogin;
end;

initialization
  InitCriticalSection(MangaTradersLockLogin);
  RegisterModule;

finalization
  DoneCriticalsection(MangaTradersLockLogin);

end.
