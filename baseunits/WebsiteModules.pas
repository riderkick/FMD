unit WebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uDownloadsManager, RegExpr;

const
  MODULE_NOT_FOUND = -1;
  NO_ERROR = 0;
  NET_PROBLEM = 1;
  INFORMATION_NOT_FOUND = 2;

type

  TModuleContainer = class;

  TOnGetDirectoryPageNumber = function(var MangaInfo: TMangaInformation;
    var Page: Integer; Module: TModuleContainer): Integer;
  TOnGetNameAndLink = function(var MangaInfo: TMangaInformation;
    const Names, Links: TStringList; const URL: String;
    Module: TModuleContainer): Integer;
  TOnGetInfo = function(var MangaInfo: TMangaInformation; const URL: String;
    const Reconnect: Integer; Module: TModuleContainer): Integer;

  TOnGetPageNumber = function(var DownloadThread: TDownloadThread;
    const URL: String; Module: TModuleContainer): Boolean;
  TOnGetImageURL = function(var DownloadThread: TDownloadThread;
    const URL: String; Module: TModuleContainer): Boolean;

  TModuleMethod = (MMGetDirectoryPageNumber, MMGetNameAndLink, MMGetInfo,
    MMGetPageNumber, MMGetImageURL);

  { TModuleContainer }

  TModuleContainer = class
  private
    FTotalDirectory: Integer;
    procedure SetTotalDirectory(AValue: Integer);
  public
    Website, RootURL: String;
    SortedList, InformationAvailable, FavoriteAvailable: Boolean;
    TotalDirectoryPage: array of Integer;
    CurrentDirectoryIndex: Integer;
    OnGetDirectoryPageNumber: TOnGetDirectoryPageNumber;
    OnGetNameAndLink: TOnGetNameAndLink;
    OnGetInfo: TOnGetInfo;
    OnGetPageNumber: TOnGetPageNumber;
    OnGetImageURL: TOnGetImageURL;
    constructor Create;
    destructor Destroy; override;
  public
    property TotalDirectory: Integer read FTotalDirectory write SetTotalDirectory;
  end;

  { TWebsiteModules }

  TWebsiteModules = class(TObject)
  private
    FCSModules: TRTLCriticalSection;
    FModuleList: TFPList;
  protected
    function GetModule(const Index: Integer): TModuleContainer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function AddModule: TModuleContainer;
    function LocateModule(const Website: String): Integer;
    function LocateModuleByHost(const Host: String): Integer;
    function ModuleAvailable(const ModuleIndex: Integer;
      ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const Website: String;
      ModuleMethod: TModuleMethod): Boolean; overload;
    function ModuleAvailable(const Website: String; ModuleMethod: TModuleMethod;
      var OutIndex: Integer): Boolean; overload;
    function ModuleAvailable(const Website: String): Boolean; overload;
    function ModuleAvailable(const Website: String; var OutIndex: Integer): Boolean;
      overload;

    function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
      var Page: Integer; const ModuleIndex: Integer): Integer; overload;
    function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
      var Page: Integer; const Website: String): Integer; overload;

    function GetNameAndLink(var MangaInfo: TMangaInformation;
      const Names, Links: TStringList; const URL: String;
      const ModuleIndex: Integer): Integer;
      overload;
    function GetNameAndLink(var MangaInfo: TMangaInformation;
      const Names, Links: TStringList; const URL, Website: String): Integer; overload;

    function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
      const Reconnect: Integer; const ModuleIndex: Integer): Integer; overload;
    function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
      const Reconnect: Integer; const Website: String): Integer; overload;

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

    property Module[const Index: Integer]: TModuleContainer read GetModule;
    property Count: Integer read GetCount;
  end;

var
  Modules: TWebsiteModules;

procedure doInitialize;
function AddModule: TModuleContainer;

implementation

{$I ModuleList.inc}

const
  REGEX_HOST = '(?ig)^(\w+://)?([^/]*\.\w+)?(\:\d+)?(/?.*)$';

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
  SortedList := False;
  InformationAvailable := True;
  FavoriteAvailable := True;
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

function TWebsiteModules.LocateModule(const Website: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if FModuleList.Count > 0 then
    for i := 0 to FModuleList.Count - 1 do
      if SameText(TModuleContainer(FModuleList[i]).Website, Website) then
      begin
        Result := i;
        Break;
      end;
end;

function TWebsiteModules.LocateModuleByHost(const Host: String): Integer;
var
  i: Integer;
  h: String;
begin
  Result := -1;
  if FModuleList.Count > 0 then
  begin
    h := LowerCase(Host);
    for i := 0 to FModuleList.Count - 1 do
      if Pos(h, LowerCase(TModuleContainer(FModuleList[i]).RootURL)) <> 0 then
      begin
        Result := i;
        Break;
      end;
    if Result = -1 then
    begin
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

function TWebsiteModules.ModuleAvailable(const Website: String): Boolean;
begin
  Result := (LocateModule(Website) > -1);
end;

function TWebsiteModules.ModuleAvailable(const Website: String;
  var OutIndex: Integer): Boolean;
begin
  OutIndex := LocateModule(Website);
  Result := OutIndex > -1;
end;

function TWebsiteModules.GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; const ModuleIndex: Integer): Integer;
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
  var Page: Integer; const Website: String): Integer;
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
  const URL: String; const Reconnect: Integer; const ModuleIndex: Integer): Integer;
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
  const URL: String; const Reconnect: Integer; const Website: String): Integer;
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

function TWebsiteModules.GetModule(const Index: Integer): TModuleContainer;
begin
  if (Index < 0) or (Index >= FModuleList.Count) then
    Exit(nil);
  Result := TModuleContainer(FModuleList[Index]);
end;

function TWebsiteModules.GetCount: Integer;
begin
  Result := FModuleList.Count;
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
