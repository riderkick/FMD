{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit WebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uData, uDownloadsManager, FMDOptions, httpsendthread,
  WebsiteModulesSettings, Process, Multilog, LazLogger, Cloudflare, RegExpr, fpjson, jsonparser,
  jsonscanner, fpjsonrtti, uBaseUnit, httpcookiemanager, syncobjs;

const
  MODULE_NOT_FOUND = -1;
  NO_ERROR = 0;
  NET_PROBLEM = 1;
  INFORMATION_NOT_FOUND = 2;

type

  TModuleContainer = class;

  TOnBeforeUpdateList = function(const Module: TModuleContainer): Boolean;
  TOnAfterUpdateList = function(const Module: TModuleContainer): Boolean;
  TOnGetDirectoryPageNumber = function(const MangaInfo: TMangaInformation;
    var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
  TOnGetNameAndLink = function(const MangaInfo: TMangaInformation;
    const ANames, ALinks: TStringList; const AURL: String;
    const Module: TModuleContainer): Integer;
  TOnGetInfo = function(const MangaInfo: TMangaInformation; const AURL: String;
    const Module: TModuleContainer): Integer;

  TOnTaskStart = function(const Task: TTaskContainer; const Module: TModuleContainer): Boolean;
  TOnGetPageNumber = function(const DownloadThread: TDownloadThread;
    const AURL: String; const Module: TModuleContainer): Boolean;
  TOnGetImageURL = function(const DownloadThread: TDownloadThread;
    const AURL: String; const Module: TModuleContainer): Boolean;

  TOnBeforeDownloadImage = function(const DownloadThread: TDownloadThread;
    var AURL: String; const Module: TModuleContainer): Boolean;

  TOnDownloadImage = function(const DownloadThread: TDownloadThread;
    const AURL: String; const Module: TModuleContainer): Boolean;

  TOnSaveImage = function(const AHTTP: THTTPSendThread;
    const APath, AName: String; const Module: TModuleContainer): String;

  TOnAfterImageSaved = function(const AFilename: String; const Module: TModuleContainer): Boolean;

  TOnLogin = function(const AHTTP: THTTPSendThread; const Module: TModuleContainer): Boolean;

  TModuleMethod = (MMGetDirectoryPageNumber, MMGetNameAndLink, MMGetInfo,
    MMTaskStart, MMGetPageNumber, MMGetImageURL, MMBeforeDownloadImage,
    MMDownloadImage, MMSaveImage, MMAfterImageSaved, MMLogin);

  TWebsiteOptionType = (woCheckBox, woEdit, woSpinEdit, woComboBox);

  TWebsiteOptionItem = record
    OptionType: TWebsiteOptionType;
    Name: String;
    Caption: PString;
    BindValue: Pointer;
    Items: PString;
  end;

  TAccountStatus = (asUnknown, asChecking, asValid, asInvalid);

  { TWebsiteModuleAccount }

  TWebsiteModuleAccount = class
  private
    FEnabled: Boolean;
    FPassword: String;
    FStatus: TAccountStatus;
    FUsername: String;
  public
    Guardian: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;
    property Status: TAccountStatus read FStatus write FStatus;
  end;

  PModuleContainer = ^TModuleContainer;

  { TModuleContainer }

  TModuleContainer = class
  private
    FAccount: TWebsiteModuleAccount;
    FAccountSupport: Boolean;
    FID: Integer;
    FSettings: TWebsiteModuleSettings;
    FTotalDirectory: Integer;
    FCloudflareCF: TCFProps;
    FCloudflareEnabled: Boolean;
    FCookieManager: THTTPCookieManager;
    procedure SetAccountSupport(AValue: Boolean);
    procedure SetCloudflareEnabled(AValue: Boolean);
    procedure CheckCloudflareEnabled(const AHTTP: THTTPSendThread);
    function CloudflareHTTPRequest(const AHTTP: THTTPSendThread; const Method, URL: String; const Response: TObject = nil): Boolean;
    procedure SetTotalDirectory(AValue: Integer);
    procedure AddOption(const AOptionType: TWebsiteOptionType;
      const ABindValue: Pointer; const AName: String; const ACaption: PString; const AItems: PString = nil);
  public
    Guardian: TCriticalSection;
    Tag: Integer;
    TagPtr: Pointer;
    Website: String;
    RootURL: String;
    Category: String;
    ActiveTaskCount: Integer;
    ActiveConnectionCount: Integer;
    SortedList: Boolean;
    InformationAvailable: Boolean;
    FavoriteAvailable: Boolean;
    DynamicPageLink: Boolean;
    TotalDirectoryPage: array of Integer;
    CurrentDirectoryIndex: Integer;
    MaxTaskLimit: Integer;
    MaxThreadPerTaskLimit: Integer;
    MaxConnectionLimit: Integer;
    OptionList: array of TWebsiteOptionItem;
    OnBeforeUpdateList: TOnBeforeUpdateList;
    OnAfterUpdateList: TOnAfterUpdateList;
    OnGetDirectoryPageNumber: TOnGetDirectoryPageNumber;
    OnGetNameAndLink: TOnGetNameAndLink;
    OnGetInfo: TOnGetInfo;
    OnTaskStart: TOnTaskStart;
    OnGetPageNumber: TOnGetPageNumber;
    OnGetImageURL: TOnGetImageURL;
    OnBeforeDownloadImage: TOnBeforeDownloadImage;
    OnDownloadImage: TOnDownloadImage;
    OnSaveImage: TOnSaveImage;
    OnAfterImageSaved: TOnAfterImageSaved;
    OnLogin: TOnLogin;
    constructor Create;
    destructor Destroy; override;
  public
    property ID: Integer read FID;
    property TotalDirectory: Integer read FTotalDirectory write SetTotalDirectory;
    procedure AddOptionCheckBox(const ABindValue: PBoolean; const AName: String;
      const ACaption: PString);
    procedure AddOptionEdit(const ABindValue: PString; const AName: String;
      const ACaption: PString);
    procedure AddOptionSpinEdit(const ABindValue: PInteger; const AName: String;
      const ACaption: PString);
    procedure AddOptionComboBox(const ABindValue: PInteger; const AName: String;
      const ACaption, AItems: PString);
    property CloudflareEnabled: Boolean read FCloudflareEnabled write SetCloudflareEnabled;
    procedure PrepareHTTP(const AHTTP: THTTPSendThread);

    procedure IncActiveTaskCount; inline;
    procedure DecActiveTaskCount; inline;
    procedure IncActiveConnectionCount; inline;
    procedure DecActiveConnectionCount; inline;

    function GetMaxConnectionLimit: Integer;
    function GetMaxTaskLimit: Integer;
    function GetMaxThreadPerTaskLimit: Integer;

    property Settings: TWebsiteModuleSettings read FSettings write FSettings;
    property AccountSupport: Boolean read FAccountSupport write SetAccountSupport;
    property Account: TWebsiteModuleAccount read FAccount write FAccount;
    property CookieManager: THTTPCookieManager read FCookieManager;
  end;

  TModuleContainers = specialize TFPGList<TModuleContainer>;

  { TWebsiteModules }

  TWebsiteModules = class
  private
    FCSModules: TRTLCriticalSection;
    FModuleList: TModuleContainers;
    function ModuleExist(const ModuleId: Integer): Boolean; inline;
    function GetModule(const ModuleId: Integer): TModuleContainer;
    function GetCount: Integer;
    function GetMaxTaskLimit(const ModuleId: Integer): Integer;
    function GetMaxThreadPerTaskLimit(const ModuleId: Integer): Integer;
    function GetMaxConnectionLimit(const ModuleId: Integer): Integer;
    function GetActiveTaskCount(const ModuleId: Integer): Integer;
    function GetActiveConnectionLimit(const ModuleId: Integer): Integer;
    function GetWebsite(const ModuleId: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;

    function AddModule: TModuleContainer;
    function LocateModule(const AWebsite: String): Integer;
    function LocateModule(const AWebsite: String; var M: TModuleContainer): Integer;
    function LocateModuleByHost(const AHost: String): Integer;
    function ModuleAvailable(const ModuleId: Integer;
      const ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const AWebsite: String;
      const ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const AWebsite: String;
      const ModuleMethod: TModuleMethod;
      var OutIndex: Integer): Boolean; overload;
    function ModuleAvailable(const AWebsite: String): Boolean; overload;
    function ModuleAvailable(const AWebsite: String;
      var OutIndex: Integer): Boolean; overload;
    function ModuleAvailable(const AWebsite: String; var M: TModuleContainer): Boolean; overload;

    function BeforeUpdateList(const ModuleId: Integer): Boolean;
    function AfterUpdateList(const ModuleId: Integer): Boolean;
    function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
      var Page: Integer; const WorkPtr: Integer; const ModuleId: Integer): Integer; overload;
    function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
      var Page: Integer; const WorkPtr: Integer; const AWebsite: String): Integer; overload; inline;

    function GetNameAndLink(const MangaInfo: TMangaInformation;
      const ANames, ALinks: TStringList; const AURL: String;
      const ModuleId: Integer): Integer; overload;
    function GetNameAndLink(const MangaInfo: TMangaInformation;
      const ANames, ALinks: TStringList;
      const AURL, AWebsite: String): Integer; overload; inline;

    function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
      const ModuleId: Integer): Integer; overload;
    function GetInfo(const MangaInfo: TMangaInformation;
      const AURL, AWebsite: String): Integer; overload; inline;

    function TaskStart(const Task: TTaskContainer;
      const ModuleId: Integer): Boolean; overload;
    function TaskStart(const Task: TTaskContainer;
      const AWebsite: String): Boolean; overload; inline;

    function GetPageNumber(const DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function GetPageNumber(const DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean; overload; inline;

    function GetImageURL(const DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function GetImageURL(const DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean; overload; inline;

    function BeforeDownloadImage(const DownloadThread: TDownloadThread;
      var AURL: String; const ModuleId: Integer): Boolean; overload;
    function BeforeDownloadImage(const DownloadThread: TDownloadThread;
      var AURL, AWebsite: String): Boolean; overload; inline;

    function DownloadImage(const DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function DownloadImage(const DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean; overload; inline;

    function SaveImage(const AHTTP: THTTPSendThread;
      const APath, AName: String; const ModuleId: Integer): String; overload;
    function SaveImage(const AHTTP: THTTPSendThread;
      const APath, AName, AWebsite: String): String; overload; inline;

    function AfterImageSaved(const AFilename: String; const ModuleId: Integer): Boolean; overload;
    function AfterImageSaved(const AFilename, AWebsite: String): Boolean; overload; inline;

    function Login(const AHTTP: THTTPSendThread; const ModuleId: Integer): Boolean; overload;
    function Login(const AHTTP: THTTPSendThread; const AWebsite: String): Boolean; overload; inline;

    procedure LockModules;
    procedure UnlockModules;

    property Module[const ModuleId: Integer]: TModuleContainer read GetModule; default;
    property Count: Integer read GetCount;
    property Website[const ModuleId: Integer]: String read GetWebsite;

    property MaxTaskLimit[const ModuleId: Integer]: Integer read GetMaxTaskLimit;
    property MaxThreadPerTaskLimit[const ModuleId: Integer]: Integer read GetMaxThreadPerTaskLimit;
    property MaxConnectionLimit[const ModuleId: Integer]: Integer read GetMaxConnectionLimit;
    property ActiveTaskCount[const ModuleId: Integer]: Integer read GetActiveTaskCount;
    property ActiveConnectionCount[const ModuleId: Integer]: Integer read GetActiveConnectionLimit;
    procedure IncActiveTaskCount(ModuleId: Integer);
    procedure DecActiveTaskCount(ModuleId: Integer);
    function CanCreateTask(ModuleId: Integer): Boolean;
    procedure IncActiveConnectionCount(ModuleId: Integer);
    procedure DecActiveConnectionCount(ModuleId: Integer);
    function CanCreateConnection(ModuleId: Integer): Boolean;

    procedure LoadFromFile;
    procedure SaveToFile;
  end;

var
  Modules: TWebsiteModules;

procedure doInitialize;
function AddModule: TModuleContainer;

procedure LockCreateConnection;
procedure UnlockCreateConnection;

function CleanOptionName(const S: String): String;


implementation

uses
{$I ModuleList.inc}

var
  CS_Connection: TRTLCriticalSection;

function CleanOptionName(const S: String): String;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  Num = ['0'..'9'];
  AlphaNum = Alpha + Num;
var
  i: Integer;
begin
  Result := Trim(S);
  if Result = '' then Exit;
  while (Length(Result) > 0) and (Result[1] in Num) do
    Delete(Result, 1, 1);
  i := 1;
  while i <= Length(Result) do
    if not (Result[i] in AlphaNum) then
      Delete(Result, i, 1)
    else
      Inc(i);
end;

{ TWebsiteModuleAccount }

constructor TWebsiteModuleAccount.Create;
begin
  Guardian := TCriticalSection.Create;
end;

destructor TWebsiteModuleAccount.Destroy;
begin
  Guardian.Free;
  inherited Destroy;
end;

{ TModuleContainer }

procedure TModuleContainer.SetCloudflareEnabled(AValue: Boolean);
begin
  if FCloudflareEnabled = AValue then Exit;
  FCloudflareEnabled := AValue;
  if FCloudflareEnabled then
    FCloudflareCF := TCFProps.Create(self)
  else
  begin
    FCloudflareCF.Free;
    FCloudflareCF := nil;
  end;
end;

procedure TModuleContainer.SetAccountSupport(AValue: Boolean);
begin
  if FAccountSupport = AValue then Exit;
  FAccountSupport := AValue;
  if FAccountSupport then
  begin
    if FAccount = nil then
      FAccount := TWebsiteModuleAccount.Create;
  end
  else
  if FAccount<>nil then
    FAccount.Free;
end;

procedure TModuleContainer.CheckCloudflareEnabled(const AHTTP: THTTPSendThread);
begin
  if FCloudflareEnabled then
    if AHTTP.OnHTTPRequest <> @CloudflareHTTPRequest then
      AHTTP.OnHTTPRequest := @CloudflareHTTPRequest;
end;

function TModuleContainer.CloudflareHTTPRequest(const AHTTP: THTTPSendThread;
  const Method, URL: String; const Response: TObject): Boolean;
begin
  Result := Cloudflare.CFRequest(AHTTP, Method, URL, Response, FCloudflareCF);
end;

procedure TModuleContainer.SetTotalDirectory(AValue: Integer);
var
  i: Integer;
begin
  if FTotalDirectory = AValue then Exit;
  FTotalDirectory := AValue;
  SetLength(TotalDirectoryPage, FTotalDirectory);
  if Length(TotalDirectoryPage) > 0 then
    for i := Low(TotalDirectoryPage) to High(TotalDirectoryPage) do
      TotalDirectoryPage[i] := 1;
end;

constructor TModuleContainer.Create;
begin
  Guardian := TCriticalSection.Create;
  FSettings := TWebsiteModuleSettings.Create;
  FID := -1;
  ActiveTaskCount := 0;
  ActiveConnectionCount := 0;
  AccountSupport := False;
  SortedList := False;
  InformationAvailable := True;
  FavoriteAvailable := True;
  DynamicPageLink := False;
  TotalDirectory := 1;
  CurrentDirectoryIndex := 0;
  CloudflareEnabled := True;
  FCookieManager := THTTPCookieManager.Create;
end;

destructor TModuleContainer.Destroy;
begin
  SetLength(TotalDirectoryPage, 0);
  SetLength(OptionList,0);
  if Assigned(FCloudflareCF) then
    FCloudflareCF.Free;
  if Assigned(FAccount) then
    FAccount.Free;
  FSettings.Free;
  Guardian.Free;
  FCookieManager.Free;
  inherited Destroy;
end;

procedure TModuleContainer.AddOptionCheckBox(const ABindValue: PBoolean;
  const AName: String; const ACaption: PString);
begin
  AddOption(woCheckBox, ABindValue, AName, ACaption);
end;

procedure TModuleContainer.AddOptionEdit(const ABindValue: PString; const AName: String;
  const ACaption: PString);
begin
  AddOption(woEdit, ABindValue, AName, ACaption);
end;

procedure TModuleContainer.AddOptionSpinEdit(const ABindValue: PInteger;
  const AName: String; const ACaption: PString);
begin
  AddOption(woSpinEdit, ABindValue, AName, ACaption);
end;

procedure TModuleContainer.AddOptionComboBox(const ABindValue: PInteger;
  const AName: String; const ACaption, AItems: PString);
begin
  AddOption(woComboBox, ABindValue, AName, ACaption, AItems);
end;

procedure TModuleContainer.PrepareHTTP(const AHTTP: THTTPSendThread);
var
  s: String;
begin
  AHTTP.CookieManager := FCookieManager;
  //todo: replace it with website challenges, there is more than cloudflare
  CheckCloudflareEnabled(AHTTP);
  if not Settings.Enabled then exit;
  with Settings.HTTP do
  begin
    if Cookies<>'' then
      AHTTP.MergeCookies(Cookies);
    if UserAgent<>'' then
      AHTTP.UserAgent:=UserAgent;
    with Proxy do
    begin
      s:='';
      case Proxy.ProxyType of
        ptDefault,ptDirect:AHTTP.SetNoProxy;
        ptHTTP:s:='HTTP';
        ptSOCKS4:s:='SOCKS4';
        ptSOCKS5:s:='SOCKS5';
      end;
      if s<>'' then
        AHTTP.SetProxy(s,ProxyHost,ProxyPort,ProxyUsername,ProxyPassword);
    end;
  end;
end;

procedure TModuleContainer.IncActiveTaskCount;
begin
  ActiveTaskCount := InterLockedIncrement(ActiveTaskCount);
end;

procedure TModuleContainer.DecActiveTaskCount;
begin
  ActiveTaskCount := InterLockedDecrement(ActiveTaskCount);
end;

procedure TModuleContainer.IncActiveConnectionCount;
begin
  ActiveConnectionCount := InterLockedIncrement(ActiveConnectionCount);
end;

procedure TModuleContainer.DecActiveConnectionCount;
begin
  ActiveConnectionCount := InterLockedDecrement(ActiveConnectionCount);
end;

function TModuleContainer.GetMaxConnectionLimit: Integer;
begin
  if (Settings.Enabled) and (Settings.MaxConnectionLimit <> 0)  then
    Result:=Settings.MaxConnectionLimit
  else
    Result:=MaxConnectionLimit;
end;

function TModuleContainer.GetMaxTaskLimit: Integer;
begin
  if (Settings.Enabled) and (Settings.MaxTaskLimit <> 0)  then
    Result:=Settings.MaxTaskLimit
  else
    Result:=MaxTaskLimit;
end;

function TModuleContainer.GetMaxThreadPerTaskLimit: Integer;
begin
  if (Settings.Enabled) and (Settings.MaxThreadPerTaskLimit <> 0)  then
    Result:=Settings.MaxThreadPerTaskLimit
  else
    Result:=MaxThreadPerTaskLimit;
end;

procedure TModuleContainer.AddOption(const AOptionType: TWebsiteOptionType;
  const ABindValue: Pointer; const AName: String; const ACaption: PString;
  const AItems: PString);
begin
  if ABindValue = nil then Exit;
  if AName = '' then Exit;
  SetLength(OptionList, Length(OptionList) + 1);
  with OptionList[High(OptionList)] do
  begin
    OptionType := AOptionType;
    BindValue := ABindValue;
    Name := CleanOptionName(AName);
    Caption := ACaption;
    Items := AItems;
  end;
end;

{ TWebsiteModules }

constructor TWebsiteModules.Create;
begin
  InitCriticalSection(FCSModules);
  FModuleList := TModuleContainers.Create;
end;

destructor TWebsiteModules.Destroy;
var
  i: Integer;
begin
  if FModuleList.Count > 0 then
    for i := FModuleList.Count - 1 downto 0 do
      FModuleList[i].Free;
  FModuleList.Free;
  DoneCriticalsection(FCSModules);
  inherited Destroy;
end;

function TWebsiteModules.AddModule: TModuleContainer;
begin
  EnterCriticalsection(FCSModules);
  try
    Result := TModuleContainer.Create;
    Result.FID := FModuleList.Add(Result);
  finally
    LeaveCriticalsection(FCSModules);
  end;
end;

function TWebsiteModules.LocateModule(const AWebsite: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if FModuleList.Count > 0 then
    for i := FModuleList.Count - 1 downto 0 do
      if SameText(FModuleList[i].Website, AWebsite) then
      begin
        Result := i;
        Break;
      end;
end;

function TWebsiteModules.LocateModule(const AWebsite: String;
  var M: TModuleContainer): Integer;
begin
  Result := LocateModule(AWebsite);
  if Result <> -1 then
  M := FModuleList[Result];
end;

function TWebsiteModules.LocateModuleByHost(const AHost: String): Integer;

  function PosModule(const s: String): Integer;
  var
    i: Integer;
  begin
    for i := FModuleList.Count - 1 downto 0 do
      if Pos(s, LowerCase(FModuleList[i].RootURL)) <> 0 then
        Exit(i);
    Result := -1;
  end;
var
  h: String;
begin
  Result := -1;
  if FModuleList.Count = 0 then Exit;
  h := LowerCase(AHost);
  Result := PosModule(h);
  if Result = -1 then
  begin
    SplitURL(h, @h, nil, False, False);
    if h = '' then Exit;
    Result := PosModule(h);
  end;
end;

function TWebsiteModules.ModuleExist(const ModuleId: Integer): Boolean;
begin
  Result := (ModuleId >= 0) and (ModuleId < FModuleList.Count);
end;

function TWebsiteModules.ModuleAvailable(const ModuleId: Integer;
  const ModuleMethod: TModuleMethod): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    case ModuleMethod of
      MMGetDirectoryPageNumber: Result := Assigned(OnGetDirectoryPageNumber);
      MMGetNameAndLink: Result := Assigned(OnGetNameAndLink);
      MMGetInfo: Result := Assigned(OnGetInfo);
      MMGetPageNumber: Result := Assigned(OnGetPageNumber);
      MMGetImageURL: Result := Assigned(OnGetImageURL);
      MMBeforeDownloadImage: Result := Assigned(OnBeforeDownloadImage);
      MMDownloadImage: Result := Assigned(OnDownloadImage);
      MMAfterImageSaved: Result := Assigned(OnAfterImageSaved);
      MMLogin: Result := Assigned(OnLogin);
      else
        Result := False;
    end;
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String;
  const ModuleMethod: TModuleMethod): Boolean;
begin
  Result := ModuleAvailable(LocateModule(AWebsite), ModuleMethod);
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String;
  const ModuleMethod: TModuleMethod; var OutIndex: Integer): Boolean;
begin
  Result := False;
  OutIndex := LocateModule(AWebsite);
  Result := ModuleAvailable(OutIndex, ModuleMethod);
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String): Boolean;
begin
  Result := (LocateModule(AWebsite) > -1);
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String;
  var OutIndex: Integer): Boolean;
begin
  OutIndex := LocateModule(AWebsite);
  Result := OutIndex > -1;
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String;
  var M: TModuleContainer): Boolean;
begin
  M := GetModule(LocateModule(AWebsite));
  Result := M <> nil;
end;

function TWebsiteModules.BeforeUpdateList(const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnBeforeUpdateList) then
      Result := OnBeforeUpdateList(FModuleList[ModuleId]);
end;

function TWebsiteModules.AfterUpdateList(const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with TModuleContainer(FModuleList[ModuleId]) do
    if Assigned(OnAfterUpdateList) then
      Result := OnAfterUpdateList(FModuleList[ModuleId]);
end;

function TWebsiteModules.GetDirectoryPageNumber(
  const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const ModuleId: Integer): Integer;
begin
  Page := 1;
  Result := MODULE_NOT_FOUND;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnGetDirectoryPageNumber) then
      Result := OnGetDirectoryPageNumber(MangaInfo, Page, WorkPtr, FModuleList[ModuleId]);
end;

function TWebsiteModules.GetDirectoryPageNumber(
  const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const AWebsite: String): Integer;
begin
  Result := GetDirectoryPageNumber(MangaInfo, Page, WorkPtr, LocateModule(AWebsite));
end;

function TWebsiteModules.GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; const ModuleId: Integer
  ): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
  if Assigned(OnGetNameAndLink) then
    Result := OnGetNameAndLink(MangaInfo, ANames, ALinks, AURL, FModuleList[ModuleId]);
end;

function TWebsiteModules.GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL, AWebsite: String): Integer;
begin
  Result := GetNameAndLink(MangaInfo, ANames, ALinks, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const ModuleId: Integer): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnGetInfo) then
      Result := OnGetInfo(MangaInfo, AURL, FModuleList[ModuleId]);
end;

function TWebsiteModules.GetInfo(const MangaInfo: TMangaInformation;
  const AURL, AWebsite: String): Integer;
begin
  Result := GetInfo(MangaInfo, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.TaskStart(const Task: TTaskContainer;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnTaskStart) then
      Result := FModuleList[ModuleId].OnTaskStart(
        Task, FModuleList[ModuleId]);
end;

function TWebsiteModules.TaskStart(const Task: TTaskContainer;
  const AWebsite: String): Boolean;
begin
  Result := TaskStart(Task, LocateModule(AWebsite));
end;

function TWebsiteModules.GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnGetPageNumber) then
      Result := OnGetPageNumber(DownloadThread, AURL, FModuleList[ModuleId]);
end;

function TWebsiteModules.GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL, AWebsite: String): Boolean;
begin
  Result := GetPageNumber(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnGetImageURL) then
      Result := OnGetImageURL(DownloadThread, AURL, FModuleList[ModuleId]);
end;

function TWebsiteModules.GetImageURL(const DownloadThread: TDownloadThread;
  const AURL, AWebsite: String): Boolean;
begin
  Result := GetImageURL(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.BeforeDownloadImage(
  const DownloadThread: TDownloadThread; var AURL: String;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnBeforeDownloadImage) then
      Result := OnBeforeDownloadImage(DownloadThread, AURL, FModuleList[ModuleId]);
end;

function TWebsiteModules.BeforeDownloadImage(
  const DownloadThread: TDownloadThread; var AURL, AWebsite: String): Boolean;
begin
  Result := BeforeDownloadImage(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.DownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnDownloadImage) then
      Result := OnDownloadImage(DownloadThread, AURL, FModuleList[ModuleId]);
end;

function TWebsiteModules.DownloadImage(const DownloadThread: TDownloadThread;
  const AURL, AWebsite: String): Boolean;
begin
  Result := DownloadImage(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.SaveImage(const AHTTP: THTTPSendThread;
  const APath, AName: String; const ModuleId: Integer): String;
begin
  Result := '';
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnSaveImage) then
      Result := OnSaveImage(AHTTP, APath, AName, FModuleList[ModuleId]);
end;

function TWebsiteModules.SaveImage(const AHTTP: THTTPSendThread;
  const APath, AName, AWebsite: String): String;
begin
  Result := SaveImage(AHTTP, APath, AName, LocateModule(AWebsite));
end;

function TWebsiteModules.AfterImageSaved(const AFilename: String;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnAfterImageSaved) then
      Result := OnAfterImageSaved(AFilename, FModuleList[ModuleId]);
end;

function TWebsiteModules.AfterImageSaved(const AFilename, AWebsite: String
  ): Boolean;
begin
  Result := AfterImageSaved(AFilename, LocateModule(AWebsite));
end;

function TWebsiteModules.Login(const AHTTP: THTTPSendThread;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if Assigned(OnLogin) then
    begin
      PrepareHTTP(AHTTP);
      Result := OnLogin(AHTTP, FModuleList[ModuleId]);
    end;
end;

function TWebsiteModules.Login(const AHTTP: THTTPSendThread;
  const AWebsite: String): Boolean;
begin
  Result := Login(AHTTP, LocateModule(AWebsite));
end;

procedure TWebsiteModules.LockModules;
begin
  EnterCriticalsection(FCSModules);
end;

procedure TWebsiteModules.UnlockModules;
begin
  LeaveCriticalsection(FCSModules);
end;

procedure TWebsiteModules.IncActiveTaskCount(ModuleId: Integer);
begin
  if ModuleExist(ModuleId) then
  FModuleList[ModuleId].IncActiveTaskCount;
end;

procedure TWebsiteModules.DecActiveTaskCount(ModuleId: Integer);
begin
  if ModuleExist(ModuleId) then
  FModuleList[ModuleId].DecActiveTaskCount;
end;

function TWebsiteModules.CanCreateTask(ModuleId: Integer): Boolean;
begin
  Result := True;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if GetMaxTaskLimit > 0 then
      Result := ActiveTaskCount < GetMaxTaskLimit;
end;

procedure TWebsiteModules.IncActiveConnectionCount(ModuleId: Integer);
begin
  if ModuleExist(ModuleId) then
  FModuleList[ModuleId].IncActiveConnectionCount;
end;

procedure TWebsiteModules.DecActiveConnectionCount(ModuleId: Integer);
begin
  if ModuleExist(ModuleId) then
  FModuleList[ModuleId].DecActiveConnectionCount;
end;

function TWebsiteModules.CanCreateConnection(ModuleId: Integer): Boolean;
begin
  Result := True;
  if ModuleExist(ModuleId) then
  with FModuleList[ModuleId] do
    if GetMaxConnectionLimit > 0 then
      Result := ActiveConnectionCount < GetMaxConnectionLimit;
end;

procedure TWebsiteModules.LoadFromFile;
var
  i, j, k: Integer;
  jd: TJSONDeStreamer;
  ja: TJSONArray;
  fs: TFileStream;
  jp: TJSONParser;
  jo, jo2: TJSONObject;
  j_cookies: TJSONArray;
  c: THTTPCookie;
begin
  if FModuleList.Count=0 then Exit;
  if not FileExists(MODULES_FILE) then Exit;

  ja:=nil;
  try
    fs:=TFileStream.Create(MODULES_FILE,fmOpenRead or fmShareDenyWrite);
    try
      jp:=TJSONParser.Create(fs,[joUTF8]);
      ja:=TJSONArray(jp.Parse);
    finally
      jp.Free;
    end;
  finally
    fs.Free;
  end;

  if (ja<>nil) and (ja.Count<>0) then
    try
      jd:=TJSONDeStreamer.Create(nil);
      jd.Options:=jd.Options+[jdoIgnorePropertyErrors];
      for i:=FModuleList.Count-1 downto 0 do
        with FModuleList[i] do
        begin
          jo:=nil;
          for j:=ja.Count-1 downto 0 do
            if ja.Objects[j].Get('Website','')=Website then
            begin
              jo:=ja.Objects[j];
              Break;
            end;
          if jo<>nil then
          begin
            jo2:=jo.Get('Settings',TJSONObject(nil));
            if jo2<>nil then
              jd.JSONToObject(jo2,Settings);
            if Length(OptionList)<>0 then
            begin
              jo2:=jo.Get('Options',TJSONObject(nil));
              if jo2<>nil then
                for k:=Low(OptionList) to High(OptionList) do
                  with OptionList[k],jo2 do
                    case OptionType of
                      woCheckBox:PBoolean(BindValue)^:=Get(Name,PBoolean(BindValue)^);
                      woEdit:PString(BindValue)^:=Get(Name,PString(BindValue)^);
                      woSpinEdit,woComboBox:PInteger(BindValue)^:=Get(Name,PInteger(BindValue)^);
                    end;
            end;
            if Account<>nil then
            begin
              jo2:=jo.Get('Account',TJSONObject(nil));
              if jo2<>nil then
              begin
                jd.JSONToObject(jo2,Account);
                if Account.Username<>'' then Account.Username := DecryptString(Account.Username);
                if Account.Password<>'' then Account.Password := DecryptString(Account.Password);
                if Account.Status=asChecking then
                  Account.Status:=asUnknown;
              end;
            end;
            j_cookies:=jo.Get('Cookies',TJSONArray(nil));
            if Assigned(j_cookies) then
            begin
              for k:=0 to j_cookies.Count-1 do
              begin
                c:=THTTPCookie.Create;
                CookieManager.Cookies.Add(c);
                jd.JSONToObject(TJSONObject(j_cookies.Items[k]), c);
              end;
            end;
            ja.Delete(j);
          end;
        end;
    finally
      jd.Free;
      ja.Free;
    end;
end;

procedure TWebsiteModules.SaveToFile;
var
  i, j: Integer;
  js: TJSONStreamer;
  ja, j_cookies: TJSONArray;
  fs: TMemoryStream;
  jo: TJSONObject;
  jo2: TJSONObject;
begin
  if FModuleList.Count=0 then Exit;
  ja:=TJSONArray.Create;
  js:=TJSONStreamer.Create(nil);
  js.Options:=js.Options+[jsoDateTimeAsString];
  try
    for i:=0 to FModuleList.Count-1 do
      with FModuleList[i] do
      begin
        jo:=TJSONObject.Create;
        ja.Add(jo);
        jo.Add('Website',Website);
        jo.Add('Settings',js.ObjectToJSON(Settings));
        if Length(OptionList) <> 0 then
        begin
          jo2:=TJSONObject.Create;
          jo.Add('Options',jo2);
          for j:=Low(OptionList) to High(OptionList) do
            with OptionList[j],jo2 do
              case OptionType of
                woCheckBox:Add(Name,PBoolean(BindValue)^);
                woEdit:Add(Name,PString(BindValue)^);
                woSpinEdit,woComboBox:Add(Name,PInteger(BindValue)^);
              end;
        end;
        if Account<>nil then
        begin
          jo2:=js.ObjectToJSON(Account);
          jo2.Strings['Username']:=EncryptString(Account.Username);
          jo2.Strings['Password']:=EncryptString(Account.Password);
          jo.Add('Account',jo2);
        end;
        j_cookies:=TJSONArray.Create;
        for j:=0 to CookieManager.Cookies.Count-1 do
        begin
          if CookieManager.Cookies[j].Persistent then
            j_cookies.Add(js.ObjectToJSON(CookieManager.Cookies[j]));
        end;
        jo.Add('Cookies', j_cookies);
      end;
    fs:=TMemoryStream.Create;
    try
      ja.DumpJSON(fs);
      fs.SaveToFile(MODULES_FILE);
    finally
      fs.Free;
    end;
  finally
    ja.Free;
    js.Free;
  end;
end;

function TWebsiteModules.GetModule(const ModuleId: Integer): TModuleContainer;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit(nil);
  Result := FModuleList[ModuleId];
end;

function TWebsiteModules.GetCount: Integer;
begin
  Result := FModuleList.Count;
end;

function TWebsiteModules.GetMaxTaskLimit(const ModuleId: Integer): Integer;
begin
  if not ModuleExist(ModuleId) then Exit(0);
  Result := FModuleList[ModuleId].GetMaxTaskLimit;
end;

function TWebsiteModules.GetMaxThreadPerTaskLimit(const ModuleId: Integer
  ): Integer;
begin
  if not ModuleExist(ModuleId) then Exit(0);
  Result := FModuleList[ModuleId].Settings.MaxThreadPerTaskLimit;
end;

function TWebsiteModules.GetMaxConnectionLimit(const ModuleId: Integer): Integer;
begin
  if not ModuleExist(ModuleId) then Exit(0);
  Result := FModuleList[ModuleId].GetMaxConnectionLimit;
end;

function TWebsiteModules.GetActiveTaskCount(const ModuleId: Integer): Integer;
begin
  if not ModuleExist(ModuleId) then Exit(0);
  Result := FModuleList[ModuleId].ActiveTaskCount;
end;

function TWebsiteModules.GetActiveConnectionLimit(const ModuleId: Integer): Integer;
begin
  if not ModuleExist(ModuleId) then Exit(0);
  Result := FModuleList[ModuleId].ActiveConnectionCount;
end;

function TWebsiteModules.GetWebsite(const ModuleId: Integer): String;
begin
  if not ModuleExist(ModuleId) then Exit('');
  Result := FModuleList[ModuleId].Website;
end;

procedure doInitialize;
begin
  if Modules = nil then
    Modules := TWebsiteModules.Create;
end;

function AddModule: TModuleContainer;
begin
  if Modules = nil then
    doInitialize;
  Result := Modules.AddModule;
end;

procedure LockCreateConnection;
begin
  EnterCriticalsection(CS_Connection);
end;

procedure UnlockCreateConnection;
begin
  LeaveCriticalsection(CS_Connection);
end;

initialization
  InitCriticalSection(CS_Connection);
  doInitialize;

finalization
  if Assigned(Modules) then
    FreeAndNil(Modules);
  DoneCriticalsection(CS_Connection);

end.
