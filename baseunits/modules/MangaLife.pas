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

var
  MMangaTraders: TModuleContainer;
  MangaTradersLockLogin: TRTLCriticalSection;
  MangaTradersOnLogin: Boolean;

function MangaTradersLogin(const AHTTP: THTTPSendThread; const Module: TModuleContainer): Boolean;
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

function GETMangaTraders(const AHTTP: THTTPSendThread; const AURL: String; const Module: TModuleContainer): Boolean;
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
      Result := MangaTradersLogin(AHTTP, Module);
  end;
  Result := AHTTP.GET(AURL);
end;

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  if Module = MMangaTraders then
    Page := Length(diralpha)
  else
    page := 1;
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
  s := Module.RootURL + dirURL;
  if AURL <> '0' then
  begin
    if Module = MMangaTraders then
      s += '?q=';
    s += diralpha[StrToIntDef(AURL, 0) + 1];
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
  s: String;
begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit(UNKNOWN_ERROR);
  with MangaInfo.mangaInfo, MangaInfo.FHTTP do
  begin
    url := RemoveURLDelim(FillHost(Module.RootURL, AURL));
    if Module = MMangaTraders then
      r := GETMangaTraders(MangaInfo.FHTTP, url, Module)
    else
      r := GET(url);
    if r then begin
      Result := NO_ERROR;
      with TXQueryEngineHTML.Create(Document) do
        try
          title := XPathString('//*[@class="row"]//h1');
          if ResultCode = 404 then
          begin
            status := '-1';
            Exit;
          end;
          if (Module = MMangaTraders) and (Pos('/series/', url) <> 0) then
          begin
            s := XPathString('//div[@class="alert alert-success startReading"]/a/@href');
            if Pos('/manga/', s) <> 0 then
            begin
              s := MaybeFillHost(Module.RootURL, s);
              if GET(s) then
                ParseHTML(Document)
              else
                r := False;
            end
            else
              r := False;
          end;
          coverLink := MaybeFillHost(Module.RootURL, XPathString('//meta[@property="og:image"]/@content'));
          authors := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Author")]'), ':');
          artists := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Artist")]'), ':');
          status := XPathString('//*[@class="row"][starts-with(.,"Scanlation Status")]');
          if status = '' then
            status := XPathString('//*[@class="row"][starts-with(.,"Status")]');
          if status <> '' then
            status := MangaInfoStatusIfPos(status);
          genres := SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Genre")]'), ':');
          summary := Trim(SeparateRight(XPathString('//*[@class="row"][starts-with(.,"Description")]'), ':'));
          if r then
          begin
            for v in XPath('//div[@class="list chapter-list"]//a') do
            begin
              s := v.toNode.getAttribute('href');
              if Pos('-page-1', s) <> 0 then
                s := StringReplace(s, '-page-1', '', []);
              chapterLinks.Add(s);
              chapterName.Add(XPathString('span[@class="chapterLabel"]', v));
            end;
            InvertStrings([chapterLinks, chapterName]);
          end;
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
  with DownloadThread.FHTTP, DownloadThread.Task.Container do
  begin
    PageLinks.Clear;
    PageNumber := 0;
    if GET(FillHost(Module.RootURL, AURL)) then
    begin
      Result := True;
      with TXQueryEngineHTML.Create(Document) do
        try
          XPathStringAll('//*[contains(@class,"image-container")]//img/@src', PageLinks);
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
      Category := 'English';
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
    end;
  end;

begin
  AddWebsiteModule('MangaLife', 'http://mangalife.us');
  AddWebsiteModule('MangaSee', 'http://mangaseeonline.us');
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
