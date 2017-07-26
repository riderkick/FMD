{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit WebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uDownloadsManager, FMDOptions, httpsendthread,
  RegExpr;

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
    const AURL, APath, AName: String; const Module: TModuleContainer): Boolean;

  TOnAfterImageSaved = function(const AFilename: String; const Module: TModuleContainer): Boolean;

  TOnLogin = function(const AHTTP: THTTPSendThread): Boolean;

  TModuleMethod = (MMGetDirectoryPageNumber, MMGetNameAndLink, MMGetInfo,
    MMTaskStart, MMGetPageNumber, MMGetImageURL, MMBeforeDownloadImage,
    MMDownloadImage, MMAfterImageSaved, MMLogin);

  TWebsiteOptionType = (woCheckBox, woEdit, woSpinEdit, woComboBox);

  TWebsiteOptionItem = record
    OptionType: TWebsiteOptionType;
    Name: String;
    Caption: PString;
    BindValue: Pointer;
    Items: PString;
  end;

  { TModuleContainer }

  TModuleContainer = class
  private
    FID: Integer;
    FTotalDirectory: Integer;
    procedure SetTotalDirectory(AValue: Integer);
    procedure AddOption(const AOptionType: TWebsiteOptionType;
      const ABindValue: Pointer; const AName: String; const ACaption: PString);
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
      const ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const AWebsite: String;
      const ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const AWebsite: String;
      const ModuleMethod: TModuleMethod;
      var OutIndex: Integer): Boolean; overload;
    function ModuleAvailable(const AWebsite: String): Boolean; overload;
    function ModuleAvailable(const AWebsite: String;
      var OutIndex: Integer): Boolean; overload;

    function BeforeUpdateList(const ModuleId: Integer): Boolean;
    function AfterUpdateList(const ModuleId: Integer): Boolean;
    function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
      var Page: Integer; const WorkPtr: Integer; const ModuleId: Integer): Integer; overload;
    function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
      var Page: Integer; const WorkPtr: Integer; const AWebsite: String): Integer; overload;

    function GetNameAndLink(const MangaInfo: TMangaInformation;
      const ANames, ALinks: TStringList; const AURL: String;
      const ModuleId: Integer): Integer; overload;
    function GetNameAndLink(const MangaInfo: TMangaInformation;
      const ANames, ALinks: TStringList;
      const AURL, AWebsite: String): Integer; overload;

    function GetInfo(const MangaInfo: TMangaInformation; const AURL: String;
      const ModuleId: Integer): Integer; overload;
    function GetInfo(const MangaInfo: TMangaInformation;
      const AURL, AWebsite: String): Integer; overload;

    function TaskStart(const Task: TTaskContainer;
      const ModuleId: Integer): Boolean; overload;
    function TaskStart(const Task: TTaskContainer;
      const AWebsite: String): Boolean; overload;

    function GetPageNumber(const DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function GetPageNumber(const DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean; overload;

    function GetImageURL(const DownloadThread: TDownloadThread;
      const AURL: String; const ModuleId: Integer): Boolean; overload;
    function GetImageURL(const DownloadThread: TDownloadThread;
      const AURL, AWebsite: String): Boolean; overload;

    function BeforeDownloadImage(const DownloadThread: TDownloadThread;
      var AURL: String; const ModuleId: Integer): Boolean; overload;
    function BeforeDownloadImage(const DownloadThread: TDownloadThread;
      var AURL, AWebsite: String): Boolean; overload;

    function DownloadImage(const DownloadThread: TDownloadThread;
      const AURL, APath, AName: String; const ModuleId: Integer): Boolean; overload;
    function DownloadImage(const DownloadThread: TDownloadThread;
      const AURL, APath, AName, AWebsite: String): Boolean; overload;

    function AfterImageSaved(const AFilename: String; const ModuleId: Integer): Boolean; overload;
    function AfterImageSaved(const AFilename, AWebsite: String): Boolean; overload;

    function Login(const AHTTP: THTTPSendThread; const ModuleId: Integer): Boolean; overload;
    function Login(const AHTTP: THTTPSendThread; const AWebsite: String): Boolean; overload;

    procedure LockModules;
    procedure UnlockModules;

    property Module[const ModuleId: Integer]: TModuleContainer read GetModule; default;
    property Count: Integer read GetCount;
    property Website[const ModuleId: Integer]: String read GetWebsite;

    property MaxTaskLimit[const ModuleId: Integer]: Integer read GetMaxTaskLimit;
    property MaxConnectionLimit[const ModuleId: Integer]: Integer read GetMaxConnectionLimit;
    property ActiveTaskCount[const ModuleId: Integer]: Integer read GetActiveTaskCount;
    property ActiveConnectionCount[const ModuleId: Integer]: Integer read GetActiveConnectionLimit;
    procedure IncActiveTaskCount(ModuleId: Integer);
    procedure DecActiveTaskCount(ModuleId: Integer);
    function CanCreateTask(ModuleId: Integer): Boolean;
    procedure IncActiveConnectionCount(ModuleId: Integer);
    procedure DecActiveConnectionCount(ModuleId: Integer);
    function CanCreateConnection(ModuleId: Integer): Boolean;

    procedure LoadWebsiteOption;
    procedure SaveWebsiteOption;
  end;

var
  Modules: TWebsiteModules;

procedure doInitialize;
function AddModule: TModuleContainer;

procedure LockCreateConnection;
procedure UnlockCreateConnection;

function CleanOptionName(const S: String): String;


implementation

{$I ModuleList.inc}

var
  CS_Connection: TRTLCriticalSection;

{ TModuleContainer }

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
  FID := -1;
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
  SetLength(OptionList,0);
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
  AddOption(woComboBox, ABindValue, AName, ACaption);
  with OptionList[High(OptionList)] do
    Items := AItems;
end;

procedure TModuleContainer.AddOption(const AOptionType: TWebsiteOptionType;
  const ABindValue: Pointer; const AName: String; const ACaption: PString);
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
  end;
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
    for i := 0 to FModuleList.Count - 1 do
      if SameText(TModuleContainer(FModuleList[i]).Website, AWebsite) then
      begin
        Result := i;
        Break;
      end;
end;

function TWebsiteModules.LocateModuleByHost(const AHost: String): Integer;

  function PosModule(const s: String): Integer;
  var
    i: Integer;
  begin
    for i := 0 to FModuleList.Count - 1 do
      if Pos(s, LowerCase(TModuleContainer(FModuleList[i]).RootURL)) <> 0 then
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

function TWebsiteModules.ModuleAvailable(const ModuleId: Integer;
  const ModuleMethod: TModuleMethod): Boolean;
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

function TWebsiteModules.BeforeUpdateList(const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if Assigned(OnBeforeUpdateList) then
      Result := OnBeforeUpdateList(TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.AfterUpdateList(const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if Assigned(OnAfterUpdateList) then
      Result := OnAfterUpdateList(TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.GetDirectoryPageNumber(
  const MangaInfo: TMangaInformation; var Page: Integer; const WorkPtr: Integer;
  const ModuleId: Integer): Integer;
begin
  Page := 1;
  Result := MODULE_NOT_FOUND;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  with TModuleContainer(FModuleList[ModuleId]) do
    if Assigned(OnGetDirectoryPageNumber) then
      Result := OnGetDirectoryPageNumber(MangaInfo, Page, WorkPtr,TModuleContainer(FModuleList[ModuleId]));
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetNameAndLink) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetNameAndLink(
      MangaInfo, ANames, ALinks, AURL, TModuleContainer(FModuleList[ModuleId]));
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetInfo) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetInfo(
      MangaInfo, AURL, TModuleContainer(FModuleList[ModuleId]));
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnTaskStart) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnTaskStart(
      Task, TModuleContainer(FModuleList[ModuleId]));
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetPageNumber) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetPageNumber(
      DownloadThread, AURL, TModuleContainer(FModuleList[ModuleId]));
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnGetImageURL) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnGetImageURL(
      DownloadThread, AURL, TModuleContainer(FModuleList[ModuleId]));
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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnBeforeDownloadImage) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnBeforeDownloadImage(
      DownloadThread, AURL, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.BeforeDownloadImage(
  const DownloadThread: TDownloadThread; var AURL, AWebsite: String): Boolean;
begin
  Result := BeforeDownloadImage(DownloadThread, AURL, LocateModule(AWebsite));
end;

function TWebsiteModules.DownloadImage(const DownloadThread: TDownloadThread;
  const AURL, APath, AName: String; const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnDownloadImage) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnDownloadImage(
      DownloadThread, AURL, APath, AName, TModuleContainer(FModuleList[ModuleId]));
end;

function TWebsiteModules.DownloadImage(const DownloadThread: TDownloadThread;
  const AURL, APath, AName, AWebsite: String): Boolean;
begin
  Result := DownloadImage(DownloadThread, AURL, APath, AName, LocateModule(AWebsite));
end;

function TWebsiteModules.AfterImageSaved(const AFilename: String;
  const ModuleId: Integer): Boolean;
begin
  Result := False;
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnAfterImageSaved) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnAfterImageSaved(AFilename,
      TModuleContainer(FModuleList[ModuleId]));

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
  if (ModuleId < 0) or (ModuleId >= FModuleList.Count) then Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleId]).OnLogin) then
    Result := TModuleContainer(FModuleList[ModuleId]).OnLogin(AHTTP);
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

procedure TWebsiteModules.LoadWebsiteOption;
var
  i, j: Integer;
begin
  if FModuleList.Count = 0 then Exit;
  for i := 0 to FModuleList.Count - 1 do
    with TModuleContainer(FModuleList[i]) do
      if Length(OptionList) > 0 then
        for j := Low(OptionList) to High(OptionList) do
          with OptionList[j], configfile do
          begin
            case OptionType of
              woCheckBox: PBoolean(BindValue)^ := ReadBool(Website, Name, PBoolean(BindValue)^);
              woEdit: PString(BindValue)^ := ReadString(Website, Name, PString(BindValue)^);
              woSpinEdit, woComboBox: PInteger(BindValue)^ := ReadInteger(Website, Name, PInteger(BindValue)^);
            end;
          end;
end;

procedure TWebsiteModules.SaveWebsiteOption;
var
  i, j: Integer;
begin
  if FModuleList.Count = 0 then Exit;
  for i := 0 to FModuleList.Count - 1 do
    with TModuleContainer(FModuleList[i]) do
      if Length(OptionList) > 0 then
        for j := Low(OptionList) to High(OptionList) do
          with OptionList[j], configfile do
          begin
            case OptionType of
              woCheckBox: WriteBool(Website, Name, PBoolean(BindValue)^);
              woEdit: WriteString(Website, Name, PString(BindValue)^);
              woSpinEdit, woComboBox: WriteInteger(Website, Name, PInteger(BindValue)^);
            end;
          end;
  configfile.UpdateFile;
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
