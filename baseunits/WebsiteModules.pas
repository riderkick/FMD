unit WebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uDownloadsManager, uBaseUnit, RegExpr;

const
  MODULE_NOT_FOUND = -1;
  NO_ERROR = 0;
  NET_PROBLEM = 1;
  INFORMATION_NOT_FOUND = 2;

  MAX_TASKLIMIT = 8;
  MAX_CONNECTIONPERHOSTLIMIT = 32;

type

  TModuleContainer = class;

  TOnGetDirectoryPageNumber = function(var MangaInfo: TMangaInformation;
    var Page: Integer; Module: TModuleContainer): Integer;
  TOnGetNameAndLink = function(var MangaInfo: TMangaInformation;
    const ANames, ALinks: TStringList; const AURL: String;
    Module: TModuleContainer): Integer;
  TOnGetInfo = function(var MangaInfo: TMangaInformation; const AURL: String;
    const Reconnect: Integer; Module: TModuleContainer): Integer;

  TOnTaskStart = function(var Task: TTaskContainer; Module: TModuleContainer): Boolean;
  TOnGetPageNumber = function(var DownloadThread: TDownloadThread;
    const AURL: String; Module: TModuleContainer): Boolean;
  TOnGetImageURL = function(var DownloadThread: TDownloadThread;
    const AURL: String; Module: TModuleContainer): Boolean;

  TOnBeforeDownloadImage = function(var DownloadThread: TDownloadThread;
    AURL: String; Module: TModuleContainer): Boolean;

  TOnDownloadImage = function(var DownloadThread: TDownloadThread;
    const AURL, APath, AName, APrefix: String; Module: TModuleContainer): Boolean;

  TOnLogin = function(var AHTTP: THTTPSendThread): Boolean;

  TModuleMethod = (MMGetDirectoryPageNumber, MMGetNameAndLink, MMGetInfo,
    MMTaskStart, MMGetPageNumber, MMGetImageURL, MMBeforeDownloadImage,
    MMDownloadImage, MMLogin);

  { TModuleContainer }

  TModuleContainer = class
  private
    FTotalDirectory: Integer;
    procedure SetTotalDirectory(AValue: Integer);
  public
    Website: String;
    RootURL: String;
    MaxTaskLimit: Integer;
    MaxConnectionLimit: Integer;
    ActiveTaskCount: Integer;
    ActiveConnectionCount: Integer;
    AccountSupport: Boolean;
    SortedList: Boolean;
    InformationAvailable: Boolean;
    FavoriteAvailable: Boolean;
    DynamicPageLink: Boolean;
    TotalDirectoryPage: array of Integer;
    CurrentDirectoryIndex: Integer;
    OnGetDirectoryPageNumber: TOnGetDirectoryPageNumber;
    OnGetNameAndLink: TOnGetNameAndLink;
    OnGetInfo: TOnGetInfo;
    OnTaskStart: TOnTaskStart;
    OnGetPageNumber: TOnGetPageNumber;
    OnGetImageURL: TOnGetImageURL;
    OnBeforeDownloadImage: TOnBeforeDownloadImage;
    OnDownloadImage: TOnDownloadImage;
    OnLogin: TOnLogin;
    constructor Create;
    destructor Destroy; override;
  public
    property TotalDirectory: Integer read FTotalDirectory write SetTotalDirectory;
  end;

  { TWebsiteModules }

  TWebsiteModules = class
  private
    FCSModules: TRTLCriticalSection;
    FModuleList: TFPList;
    function GetModule(const ModuleId: Integer): TModuleContainer;
    function GetCount: Integer;
    function GetMaxTaskLimit(const ModuleId: Integer): Integer;
    function GetMaxConnectionLimit(const ModuleId: Integer): Integer;
    function GetActiveTaskCount(const ModuleId: Integer): Integer;
    function GetActiveConnectionLimit(const ModuleId: Integer): Integer;
    function GetWebsite(const ModuleId: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;

    function AddModule: TModuleContainer;
    function LocateModule(const AWebsite: String): Integer;
    function LocateModuleByHost(const AHost: String): Integer;
    function ModuleAvailable(const ModuleId: Integer;
      ModuleMethod: TModuleMethod): Boolean;
      overload;
    function ModuleAvailable(const AWebsite: String;
      ModuleMethod: TModuleMethod): Boolean;
      overload;
    function ModuleAvailable(const AWebsite: String; ModuleMethod: TModuleMethod;
      var OutIndex: Integer): Boolean; overload;
    function ModuleAvailable(const AWebsite: String): Boolean; overload;
    function ModuleAvailable(const AWebsite: String; var OutIndex: Integer): Boolean;
      overload;

    function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
      var Page: Integer; const ModuleId: Integer): Integer; overload;
    function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
      var Page: Integer; const AWebsite: String): Integer; overload;

    function GetNameAndLink(var MangaInfo: TMangaInformation;
      const ANames, ALinks: TStringList; const AURL: String;
      const ModuleId: Integer): Integer;
      overload;
    function GetNameAndLink(var MangaInfo: TMangaInformation;
      const ANames, ALinks: TStringList; const AURL, AWebsite: String): Integer; overload;

    function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
      const Reconnect: Integer; const ModuleId: Integer): Integer; overload;
    function GetInfo(var MangaInfo: TMangaInformation; const AURL: String;
      const Reconnect: Integer; const AWebsite: String): Integer; overload;

    function TaskStart(var Task: TTaskContainer; const ModuleId: Integer): Boolean;
      overload;
    function TaskStart(var Task: TTaskContainer; const AWebsite: String): Boolean;
      overload;

    function GetPageNumber(var DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function GetPageNumber(var DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean;
      overload;

    function GetImageURL(var DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function GetImageURL(var DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean;
      overload;

    function BeforeDownloadImage(var DownloadThread: TDownloadThread;
      AURL: String; const ModuleId: Integer): Boolean; overload;
    function BeforeDownloadImage(var DownloadThread: TDownloadThread;
      AURL: String; const AWebsite: String): Boolean; overload;

    function DownloadImage(var DownloadThread: TDownloadThread;
      const AURL, APath, AName, APrefix: String; ModuleId: Integer): Boolean; overload;
    function DownloadImage(var DownloadThread: TDownloadThread;
      const AURL, APath, AName, APrefix, AWebsite: String): Boolean; overload;

    function Login(var AHTTP: THTTPSendThread; const ModuleId: Integer): Boolean; overload;
    function Login(var AHTTP: THTTPSendThread; const AWebsite: String): Boolean; overload;

    procedure LockModules;
    procedure UnlockModules;

    property Module[const ModuleId: Integer]: TModuleContainer read GetModule;
    property Count: Integer read GetCount;
    property Website[const ModuleId: Integer]: String read GetWebsite;

    property MaxTaskLimit[const ModuleId: Integer]: Integer read GetMaxTaskLimit;
    property MaxConnectionLimit[const ModuleId: Integer]: Integer
      read GetMaxConnectionLimit;
    property ActiveTaskCount[const ModuleId: Integer]: Integer read GetActiveTaskCount;
    property ActiveConnectionCount[const ModuleId: Integer]: Integer
      read GetActiveConnectionLimit;
    procedure IncActiveTaskCount(ModuleId: Integer);
    procedure DecActiveTaskCount(ModuleId: Integer);
    function CanCreateTask(ModuleId: Integer): Boolean;
    procedure IncActiveConnectionCount(ModuleId: Integer);
    procedure DecActiveConnectionCount(ModuleId: Integer);
    function CanCreateConnection(ModuleId: Integer): Boolean;
  end;

var
  Modules: TWebsiteModules;

procedure doInitialize;
function AddModule: TModuleContainer;

procedure LockCreateConnection;
procedure UnlockCreateConnection;

implementation

{$I ModuleList.inc}

const
  REGEX_HOST = '(?ig)^(\w+://)?([^/]*\.\w+)?(\:\d+)?(/?.*)$';

var
  CS_Connection: TRTLCriticalSection;

{ TModuleContainer }

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
  MaxTaskLimit := 0;
  MaxConnectionLimit := 0;
  ActiveTaskCount := 0;
  ActiveConnectionCount := 0;
  AccountSupport := False;
  SortedList := False;
  InformationAvailable := True;
  FavoriteAvailable := True;
  DynamicPageLink := False;
  TotalDirectory := 1;
  CurrentDirectoryIndex := 0;
end;

destructor TModuleContainer.Destroy;
begin
  SetLength(TotalDirectoryPage, 0);
  inherited Destroy;
end;

{ TWebsiteModules }

constructor TWebsiteModules.Create;
begin
  InitCriticalSection(FCSModules);
  FModuleList := TFPList.Create;
end;

destructor TWebsiteModules.Destroy;
var
  i: Integer;
begin
  if FModuleList.Count > 0 then
    for i := 0 to FModuleList.Count - 1 do
      TModuleContainer(FModuleList[i]).Free;
  FModuleList.Free;
  DoneCriticalsection(FCSModules);
  inherited Destroy;
end;

function TWebsiteModules.AddModule: TModuleContainer;
begin
  EnterCriticalsection(FCSModules);
  try
    FModuleList.Add(TModuleContainer.Create);
    Result := TModuleContainer(FModuleList.Last);
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
    for i := 0 to FModuleList.Count - 1 do
      if SameText(TModuleContainer(FModuleList[i]).Website, AWebsite) then
      begin
        Result := i;
        Break;
      end;
end;

function TWebsiteModules.LocateModuleByHost(const AHost: String): Integer;
var
  i: Integer;
  h: String;
begin
  Result := -1;
  if FModuleList.Count > 0 then
  begin
    h := LowerCase(AHost);
    for i := 0 to FModuleList.Count - 1 do
      if Pos(h, LowerCase(TModuleContainer(FModuleList[i]).RootURL)) <> 0 then
      begin
        Result := i;
        Break;
      end;
    if Result = -1 then
      with TRegExpr.Create do
        try
          Expression := REGEX_HOST;
          for i := 0 to FModuleList.Count - 1 do
            if Pos(LowerCase(Replace(TModuleContainer(FModuleList[i]).RootURL,
              '$2', True)), h) <> 0 then
            begin
              Result := i;
              Break;
            end;
        finally
          Free;
        end;
  end;
end;

function TWebsiteModules.ModuleAvailable(const ModuleId: Integer;
  ModuleMethod: TModuleMethod): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    case ModuleMethod of
      MMGetDirectoryPageNumber: Result := Assigned(OnGetDirectoryPageNumber);
      MMGetNameAndLink: Result := Assigned(OnGetNameAndLink);
      MMGetInfo: Result := Assigned(OnGetInfo);
      MMGetPageNumber: Result := Assigned(OnGetPageNumber);
      MMGetImageURL: Result := Assigned(OnGetImageURL);
      MMBeforeDownloadImage: Result := Assigned(OnBeforeDownloadImage);
      MMDownloadImage: Result := Assigned(OnDownloadImage);
      MMLogin: Result := Assigned(OnLogin);
      else
        Result := False;
    end;
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String;
  ModuleMethod: TModuleMethod): Boolean;
begin
  Result := ModuleAvailable(LocateModule(AWebsite), ModuleMethod);
end;

function TWebsiteModules.ModuleAvailable(const AWebsite: String;
  ModuleMethod: TModuleMethod; var OutIndex: Integer): Boolean;
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

function TWebsiteModules.GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; const ModuleId: Integer): Integer;
begin
  Page := 1;
  Result := MODULE_NOT_FOUND;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetDirectoryPageNumber) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetDirectoryPageNumber(
      MangaInfo, Page, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; const AWebsite: String): Integer;
begin
  Result := GetDirectoryPageNumber(MangaInfo, Page, LocateModule(AWebsite));
end;

function TWebsiteModules.GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String; const ModuleId: Integer): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetNameAndLink) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetNameAndLink(
      MangaInfo, ANames, ALinks, AURL, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.GetNameAndLink(var MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL, AWebsite: String): Integer;
begin
  Result := GetNameAndLink(MangaInfo, ANames, ALinks, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.GetInfo(var MangaInfo: TMangaInformation;
  const AURL: String; const Reconnect: Integer; const ModuleId: Integer): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetInfo) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetInfo(
      MangaInfo, AURL, Reconnect, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.GetInfo(var MangaInfo: TMangaInformation;
  const AURL: String; const Reconnect: Integer; const AWebsite: String): Integer;
begin
  Result := GetInfo(MangaInfo, AURL, Reconnect, LocateModule(AWebsite));
end;

function TWebsiteModules.TaskStart(var Task: TTaskContainer;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnTaskStart) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnTaskStart(
      Task, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.TaskStart(var Task: TTaskContainer;
  const AWebsite: String): Boolean;
begin
  Result := TaskStart(Task, LocateModule(AWebsite));
end;

function TWebsiteModules.GetPageNumber(var DownloadThread: TDownloadThread;
  const AURL: String; const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetPageNumber) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetPageNumber(
      DownloadThread, AURL, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.GetPageNumber(var DownloadThread: TDownloadThread;
  const AURL, AWebsite: String): Boolean;
begin
  Result := GetPageNumber(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.GetImageURL(var DownloadThread: TDownloadThread;
  const AURL: String; const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetImageURL) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetImageURL(
      DownloadThread, AURL, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.GetImageURL(var DownloadThread: TDownloadThread;
  const AURL, AWebsite: String): Boolean;
begin
  Result := GetImageURL(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.BeforeDownloadImage(
  var DownloadThread: TDownloadThread; AURL: String; const ModuleId: Integer
  ): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnBeforeDownloadImage) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnBeforeDownloadImage(
      DownloadThread, AURL, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.BeforeDownloadImage(
  var DownloadThread: TDownloadThread; AURL: String; const AWebsite: String
  ): Boolean;
begin
  Result := BeforeDownloadImage(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.DownloadImage(var DownloadThread: TDownloadThread;
  const AURL, APath, AName, APrefix: String; ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnDownloadImage) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnDownloadImage(
      DownloadThread, AURL, APath, AName, APrefix, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.DownloadImage(var DownloadThread: TDownloadThread;
  const AURL, APath, AName, APrefix, AWebsite: String): Boolean;
begin
  Result := DownloadImage(DownloadThread, AURL, APath, AName, APrefix,
    LocateModule(AWebsite));
end;

function TWebsiteModules.Login(var AHTTP: THTTPSendThread;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnLogin) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnLogin(AHTTP);
end;

function TWebsiteModules.Login(var AHTTP: THTTPSendThread; const AWebsite: String
  ): Boolean;
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if MaxTaskLimit > 0 then
      ActiveTaskCount := InterLockedIncrement(ActiveTaskCount);
end;

procedure TWebsiteModules.DecActiveTaskCount(ModuleId: Integer);
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if ActiveTaskCount > 0 then
      ActiveTaskCount := InterLockedDecrement(ActiveTaskCount);
end;

function TWebsiteModules.CanCreateTask(ModuleId: Integer): Boolean;
begin
  Result := True;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if MaxTaskLimit > 0 then
      Result := ActiveTaskCount < MaxTaskLimit;
end;

procedure TWebsiteModules.IncActiveConnectionCount(ModuleId: Integer);
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if MaxConnectionLimit > 0 then
      ActiveConnectionCount := InterLockedIncrement(ActiveConnectionCount);
end;

procedure TWebsiteModules.DecActiveConnectionCount(ModuleId: Integer);
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if ActiveConnectionCount > 0 then
      ActiveConnectionCount := InterLockedDecrement(ActiveConnectionCount);
end;

function TWebsiteModules.CanCreateConnection(ModuleId: Integer): Boolean;
begin
  Result := True;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if MaxConnectionLimit > 0 then
      Result := ActiveConnectionCount < MaxConnectionLimit;
end;

function TWebsiteModules.GetModule(const ModuleId: Integer): TModuleContainer;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit(nil);
  Result := TModuleContainer(FModuleList[ModuleId]);
end;

function TWebsiteModules.GetCount: Integer;
begin
  Result := FModuleList.Count;
end;

function TWebsiteModules.GetMaxTaskLimit(const ModuleId: Integer): Integer;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit(0);
  Result := TModuleContainer(FModuleList[ModuleId]).MaxTaskLimit;
end;

function TWebsiteModules.GetMaxConnectionLimit(const ModuleId: Integer): Integer;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit(0);
  Result := TModuleContainer(FModuleList[ModuleId]).MaxConnectionLimit;
end;

function TWebsiteModules.GetActiveTaskCount(const ModuleId: Integer): Integer;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit(0);
  Result := TModuleContainer(FModuleList[ModuleId]).ActiveTaskCount;
end;

function TWebsiteModules.GetActiveConnectionLimit(const ModuleId: Integer): Integer;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit(0);
  Result := TModuleContainer(FModuleList[ModuleId]).ActiveConnectionCount;
end;

function TWebsiteModules.GetWebsite(const ModuleId: Integer): String;
begin
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit('');
  Result := TModuleContainer(FModuleList[ModuleId]).Website;
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
