unit WebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uDownloadsManager;

const
  MODULE_NOT_FOUND = -1;
  NO_ERROR = 0;
  NET_PROBLEM = 1;
  INFORMATION_NOT_FOUND = 2;

type

  TModuleContainer = class;

  TOnGetDirectoryPageNumber = function(var MangaInfo: TMangaInformation;
    var Page: Cardinal; Module: TModuleContainer): Integer;
  TOnGetNameAndLink = function(var MangaInfo: TMangaInformation;
    const Names, Links: TStringList; const URL: String;
    Module: TModuleContainer): Integer;
  TOnGetInfo = function(var MangaInfo: TMangaInformation; const URL: String;
    const Reconnect: Cardinal; Module: TModuleContainer): Integer;

  TOnGetPageNumber = function(var DownloadThread: TDownloadThread;
    const URL: String; Module: TModuleContainer): Boolean;
  TOnGetImageURL = function(var DownloadThread: TDownloadThread;
    const URL: String; Module: TModuleContainer): Boolean;

  TModuleMethod = (MMGetDirectoryPageNumber, MMGetNameAndLink, MMGetInfo,
    MMGetPageNumber, MMGetImageURL);

  { TModuleContainer }

  TModuleContainer = class
  public
    Website, RootURL: String;
    SortedList, InformationAvailable: Boolean;
    OnGetDirectoryPageNumber: TOnGetDirectoryPageNumber;
    OnGetNameAndLink: TOnGetNameAndLink;
    OnGetInfo: TOnGetInfo;
    OnGetPageNumber: TOnGetPageNumber;
    OnGetImageURL: TOnGetImageURL;
    constructor Create;
  end;

  { TWebsiteModules }

  TWebsiteModules = class(TObject)
  private
    FCSModules: TRTLCriticalSection;
    FModuleList: TFPList;
  public
    constructor Create;
    destructor Destroy; override;

    function AddModule: TModuleContainer;
    function LocateModule(const Website: String): Integer;
    function ModuleAvailable(const ModuleIndex: Integer;
      ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const Website: String;
      ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const Website: String; ModuleMethod: TModuleMethod;
      var OutIndex: Integer): Boolean; overload;

    function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
      var Page: Cardinal; const ModuleIndex: Integer): Integer; overload;
    function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
      var Page: Cardinal; const Website: String): Integer; overload;

    function GetNameAndLink(var MangaInfo: TMangaInformation;
      const Names, Links: TStringList; const URL: String;
      const ModuleIndex: Integer): Integer;
      overload;
    function GetNameAndLink(var MangaInfo: TMangaInformation;
      const Names, Links: TStringList; const URL, Website: String): Integer; overload;

    function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
      const Reconnect: Cardinal; const ModuleIndex: Integer): Integer; overload;
    function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
      const Reconnect: Cardinal; const Website: String): Integer; overload;

    function GetPageNumber(var DownloadThread: TDownloadThread;
      const URL: String; const ModuleIndex: Integer): Boolean; overload;
    function GetPageNumber(var DownloadThread: TDownloadThread;
      const URL, Website: String): Boolean; overload;

    function GetImageURL(var DownloadThread: TDownloadThread;
      const URL: String; const ModuleIndex: Integer): Boolean; overload;
    function GetImageURL(var DownloadThread: TDownloadThread;
      const URL, Website: String): Boolean; overload;

    procedure LockModules;
    procedure UnlockModules;

    function Items(const Index: Integer): TModuleContainer;
  end;

var
  Modules: TWebsiteModules;

procedure doInitialize;
function AddModule: TModuleContainer;

implementation

{$I ModuleList.inc}

{ TModuleContainer }

constructor TModuleContainer.Create;
begin
  SortedList := False;
  InformationAvailable := True;
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

function TWebsiteModules.LocateModule(const Website: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if FModuleList.Count > 0 then
  begin
    EnterCriticalsection(FCSModules);
    try
      for i := 0 to FModuleList.Count - 1 do
      begin
        if SameText(TModuleContainer(FModuleList[i]).Website, Website) then
        begin
          Result := i;
          Break;
        end;
      end;
    finally
      LeaveCriticalsection(FCSModules);
    end;
  end;
end;

function TWebsiteModules.ModuleAvailable(const ModuleIndex: Integer;
  ModuleMethod: TModuleMethod): Boolean;
begin
  Result := False;
  if (ModuleIndex < 0) or (FModuleList.Count = 0) or
    (ModuleIndex >= FModuleList.Count) then
    Exit;
  with TModuleContainer(FModuleList[ModuleIndex]) do
  begin
    case ModuleMethod of
      MMGetDirectoryPageNumber: Result := Assigned(OnGetDirectoryPageNumber);
      MMGetNameAndLink: Result := Assigned(OnGetNameAndLink);
      MMGetInfo: Result := Assigned(OnGetInfo);
      MMGetPageNumber: Result := Assigned(OnGetPageNumber);
      MMGetImageURL: Result := Assigned(OnGetImageURL);
      else
        Result := False;
    end;
  end;
end;

function TWebsiteModules.ModuleAvailable(const Website: String;
  ModuleMethod: TModuleMethod): Boolean;
begin
  Result := ModuleAvailable(LocateModule(Website), ModuleMethod);
end;

function TWebsiteModules.ModuleAvailable(const Website: String;
  ModuleMethod: TModuleMethod; var OutIndex: Integer): Boolean;
begin
  Result := False;
  OutIndex := LocateModule(Website);
  Result := ModuleAvailable(OutIndex, ModuleMethod);
end;

function TWebsiteModules.GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Cardinal; const ModuleIndex: Integer): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if (ModuleIndex < 0) or (FModuleList.Count = 0) or
    (ModuleIndex >= FModuleList.Count) then
    Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleIndex]).OnGetDirectoryPageNumber) then
    Result := TModuleContainer(FModuleList[ModuleIndex]).OnGetDirectoryPageNumber(
      MangaInfo, Page, TModuleContainer(FModuleList[ModuleIndex]));
end;

function TWebsiteModules.GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Cardinal; const Website: String): Integer;
begin
  Result := GetDirectoryPageNumber(MangaInfo, Page, LocateModule(Website));
end;

function TWebsiteModules.GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String;
  const ModuleIndex: Integer): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if (ModuleIndex < 0) or (FModuleList.Count = 0) or
    (ModuleIndex >= FModuleList.Count) then
    Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleIndex]).OnGetNameAndLink) then
    Result := TModuleContainer(FModuleList[ModuleIndex]).OnGetNameAndLink(
      MangaInfo, Names, Links, URL, TModuleContainer(FModuleList[ModuleIndex]));
end;

function TWebsiteModules.GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL, Website: String): Integer;
begin
  Result := GetNameAndLink(MangaInfo, Names, Links, URL, LocateModule(Website));
end;

function TWebsiteModules.GetInfo(var MangaInfo: TMangaInformation;
  const URL: String; const Reconnect: Cardinal; const ModuleIndex: Integer): Integer;
begin
  Result := MODULE_NOT_FOUND;
  if (ModuleIndex < 0) or (FModuleList.Count = 0) or
    (ModuleIndex >= FModuleList.Count) then
    Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleIndex]).OnGetInfo) then
    Result := TModuleContainer(FModuleList[ModuleIndex]).OnGetInfo(
      MangaInfo, URL, Reconnect, TModuleContainer(FModuleList[ModuleIndex]));
end;

function TWebsiteModules.GetInfo(var MangaInfo: TMangaInformation;
  const URL: String; const Reconnect: Cardinal; const Website: String): Integer;
begin
  Result := GetInfo(MangaInfo, URL, Reconnect, LocateModule(Website));
end;

function TWebsiteModules.GetPageNumber(var DownloadThread: TDownloadThread;
  const URL: String; const ModuleIndex: Integer): Boolean;
begin
  Result := False;
  if (ModuleIndex < 0) or (FModuleList.Count = 0) or
    (ModuleIndex >= FModuleList.Count) then
    Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleIndex]).OnGetPageNumber) then
    Result := TModuleContainer(FModuleList[ModuleIndex]).OnGetPageNumber(
      DownloadThread, URL, TModuleContainer(FModuleList[ModuleIndex]));
end;

function TWebsiteModules.GetPageNumber(var DownloadThread: TDownloadThread;
  const URL, Website: String): Boolean;
begin
  Result := GetPageNumber(DownloadThread, URL, LocateModule(Website));
end;

function TWebsiteModules.GetImageURL(var DownloadThread: TDownloadThread;
  const URL: String; const ModuleIndex: Integer): Boolean;
begin
  Result := False;
  if (ModuleIndex < 0) or (FModuleList.Count = 0) or
    (ModuleIndex >= FModuleList.Count) then
    Exit;
  if Assigned(TModuleContainer(FModuleList[ModuleIndex]).OnGetImageURL) then
    Result := TModuleContainer(FModuleList[ModuleIndex]).OnGetImageURL(
      DownloadThread, URL, TModuleContainer(FModuleList[ModuleIndex]));
end;

function TWebsiteModules.GetImageURL(var DownloadThread: TDownloadThread;
  const URL, Website: String): Boolean;
begin
  Result := GetImageURL(DownloadThread, URL, LocateModule(Website));
end;

procedure TWebsiteModules.LockModules;
begin
  EnterCriticalsection(FCSModules);
end;

procedure TWebsiteModules.UnlockModules;
begin
  LeaveCriticalsection(FCSModules);
end;

function TWebsiteModules.Items(const Index: Integer): TModuleContainer;
begin
  Result := TModuleContainer(FModuleList[Index]);
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

initialization
  doInitialize;

finalization
  if Assigned(Modules) then
    FreeAndNil(Modules);

end.
