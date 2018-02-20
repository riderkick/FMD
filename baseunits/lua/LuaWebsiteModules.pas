unit LuaWebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, lua53, LuaStringsStorage, WebsiteModules;

type
  TLuaWebsiteModulesContainer = class;

  { TLuaWebsiteModule }

  TLuaWebsiteModule = class
  private
  public
    Module: TModuleContainer;
    OnBeforeUpdateList: String;
    OnAfterUpdateList: String;
    OnGetDirectoryPageNumber: String;
    OnGetNameAndLink: String;
    OnGetInfo: String;
    OnTaskStart: String;
    OnGetPageNumber: String;
    OnGetImageURL: String;
    OnBeforeDownloadImage: String;
    OnDownloadImage: String;
    OnSaveImage: String;
    OnAfterImageSaved: String;
    OnLogin: String;
    Storage: TStringsStorage;
    LastUpdated: String;
    Container: TLuaWebsiteModulesContainer;

    Options: TStringList;

    constructor Create;
    destructor Destroy; override;

    function AddOption(const AName, ACaption: String; const AClass: TClass): Integer;
    procedure AddOptionCheckBox(const AName, ACaption: String; const ADefault: Boolean);
    procedure AddOptionEdit(const AName, ACaption: String; const ADefault: String);
    procedure AddOptionSpinEdit(const AName, ACaption: String; const ADefault: Integer);
    procedure AddOptionComboBox(const AName, ACaption, AItems: String; const ADefault: Integer);

    procedure LuaPushMe(L: Plua_State);
    function LuaDoMe(L: Plua_State): Integer;
  end;

  TLuaWebsiteModules = specialize TFPGList<TLuaWebsiteModule>;

  { TLuaWebsiteModulesContainer }

  TLuaWebsiteModulesContainer = class
  public
    Modules: TLuaWebsiteModules;
    FileName: String;
    ByteCode: TMemoryStream;
    constructor Create;
    destructor Destroy; override;
  end;

  TLuaWebsiteModulesContainers = specialize TFPGList<TLuaWebsiteModulesContainer>;

  { TLuaWebsiteModulesManager }

  TLuaWebsiteModulesManager = class
  public
    Containers: TLuaWebsiteModulesContainers;
    TempModuleList: TLuaWebsiteModules;
    constructor Create;
    destructor Destroy; override;
  end;

  TOptionItem = class
    Caption: String;
  end;

  TOptionItemCheckBox = class(TOptionItem)
    Value: Boolean;
  end;

  TOptionItemEdit = class(TOptionItem)
    Value: String;
  end;

  TOptionItemSpinEdit = class(TOptionItem)
    Value: Integer;
  end;

  TOptionItemComboBox = class(TOptionItem)
    Items: String;
    Value: Integer;
  end;

procedure ScanLuaWebsiteModulesFile;

procedure luaWebsiteModuleAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);

var
  LuaWebsiteModulesManager: TLuaWebsiteModulesManager;
  AlwaysLoadLuaFromFile: Boolean = {$ifdef DEVBUILD}True{$else}False{$endif};

implementation

uses
  FMDOptions, FileUtil, MultiLog, LuaClass, LuaBase, LuaMangaInfo, LuaHTTPSend,
  LuaXQuery, LuaUtils, LuaDownloadTask, LuaUpdateListManager, luaStrings, uData,
  uDownloadsManager, xquery, httpsendthread, FMDVars;

function DoBeforeUpdateList(const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(l, updateList, 'updatelist', @luaUpdateListManagerAddMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnBeforeUpdateList) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoAfterUpdateList(const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(l, updateList, 'updatelist', @luaUpdateListManagerAddMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnAfterUpdateList) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoGetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const WorkPtr: Integer; const Module: TModuleContainer): Integer;
var
  l: Plua_State;
begin
  Result := NO_ERROR;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushIntegerGlobal(l, 'page', Page);
      luaPushIntegerGlobal(l, 'workptr', WorkPtr);
      luaPushObject(l, MangaInfo.mangaInfo, 'mangainfo', @luaMangaInfoAddMetaTable);
      luaPushObject(l, MangaInfo.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushObject(l, updateList, 'updatelist', @luaUpdateListManagerAddMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnGetDirectoryPageNumber) then
      begin
        Result := lua_tointeger(l, -1);
        if lua_getglobal(l, 'page') <> 0 then
          Page := lua_tointeger(l, -1);
      end;
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoGetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  l: Plua_State;
begin
  Result := NO_ERROR;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(l, MangaInfo.mangaInfo, 'mangainfo', @luaMangaInfoAddMetaTable);
      luaPushObject(l, MangaInfo.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(L, 'url', AURL);
      luaPushObject(l, ANames, 'names', @luaStringsAddMetaTable);
      luaPushObject(l, ALinks, 'links', @luaStringsAddMetaTable);
      luaPushObject(l, updateList, 'updatelist', @luaUpdateListManagerAddMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnGetNameAndLink) then
        Result := lua_tointeger(L, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoGetInfo(const MangaInfo: TMangaInformation; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  l: Plua_State;
begin
  Result := NO_ERROR;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushStringGlobal(l, 'url', AURL);
      luaPushObject(l, MangaInfo.mangaInfo, 'mangainfo', @luaMangaInfoAddMetaTable);
      luaPushObject(l, MangaInfo.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      LuaCallFunction(l, OnGetInfo);
    except
      Logger.SendError(lua_tostring(L, -1));
    end;
    lua_close(l);
  end;
end;

function DoTaskStart(const Task: TTaskContainer; const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(l, Task, 'task', @luaDownloadTaskMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnTaskStart) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoGetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(L, DownloadThread.Task.Container, 'task', @luaDownloadTaskMetaTable);
      luaPushObject(l, DownloadThread.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(l, 'url', AURL);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnGetPageNumber) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoGetImageURL(const DownloadThread: TDownloadThread; const AURL: String;
  const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(L, DownloadThread.Task.Container, 'task', @luaDownloadTaskMetaTable);
      luaPushObject(l, DownloadThread.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushIntegerGlobal(l, 'workid', DownloadThread.WorkId);
      luaPushStringGlobal(l, 'url', AURL);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnGetImageURL) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoBeforeDownloadImage(const DownloadThread: TDownloadThread;
  var AURL: String; const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(L, DownloadThread.Task.Container, 'task', @luaDownloadTaskMetaTable);
      luaPushObject(l, DownloadThread.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(l, 'url', AURL);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnBeforeDownloadImage) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoDownloadImage(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(L, DownloadThread.Task.Container, 'task', @luaDownloadTaskMetaTable);
      luaPushObject(l, DownloadThread.FHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(l, 'url', AURL);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnDownloadImage) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoSaveImage(const AHTTP: THTTPSendThread; const APath, AName: String;
  const Module: TModuleContainer): String;
var
  l: Plua_State;
begin
  Result := '';
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(l, AHTTP, 'http', @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(l, 'path', APath);
      luaPushStringGlobal(l, 'name', AName);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnSaveImage) then
        Result := lua_tostring(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoAfterImageSaved(const AFilename: String; const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushStringGlobal(l, 'filename', AFilename);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnAfterImageSaved) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function DoLogin(const AHTTP: THTTPSendThread; const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModule(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      LuaPushMe(l);
      luaPushObject(l, AHTTP, 'http', @luaHTTPSendThreadAddMetaTable);

      if LuaDoMe(l) <> 0 then
        raise Exception.Create('');
      if LuaCallFunction(l, OnTaskStart) then
        Result := lua_toboolean(l, -1);
    except
      Logger.SendError(lua_tostring(l, -1));
    end;
    lua_close(l);
  end;
end;

function LoadLuaToWebsiteModules(AFilename: String): Boolean;
var
  l: Plua_State;
  c: TLuaWebsiteModulesContainer;
  m: TMemoryStream;
  i: Integer;
  s: String;
begin
  Result := False;
  Logger.Send('Load lua website module', AFilename);
  try
    l := LuaNewBaseState;
    try
      m := LuaDumpFileToStream(l, AFilename);
      if m <> nil then
      begin
        if lua_pcall(l, 0, 0, 0) <> 0 then
          raise Exception.Create('');
        LuaCallFunction(l, 'Init');
      end;
    except
      Logger.SendError('Error load lua website module. ' + lua_tostring(L, -1));
    end;
  finally
    lua_close(l);
  end;

  if LuaWebsiteModulesManager.TempModuleList.Count <> 0 then
    with LuaWebsiteModulesManager do
    begin
      c := TLuaWebsiteModulesContainer.Create;
      c.FileName := AFilename;
      c.ByteCode := m;
      m := nil;
      s := '';
      Containers.Add(c);
      for i := 0 to TempModuleList.Count - 1 do
        with TempModuleList[i] do
        begin
          s += Module.Website + ', ';
          c.Modules.Add(TempModuleList[i]);
          Container := c;
          if OnBeforeUpdateList <> '' then
            Module.OnBeforeUpdateList := @DoBeforeUpdateList;
          if OnAfterUpdateList <> '' then
            Module.OnAfterUpdateList := @DoAfterUpdateList;
          if OnGetDirectoryPageNumber <> '' then
            Module.OnGetDirectoryPageNumber := @DoGetDirectoryPageNumber;
          if OnGetNameAndLink <> '' then
            Module.OnGetNameAndLink := @DoGetNameAndLink;
          if OnGetInfo <> '' then
            Module.OnGetInfo := @DoGetInfo;
          if OnTaskStart <> '' then
            Module.OnTaskStart := @DoTaskStart;
          if OnGetPageNumber <> '' then
            Module.OnGetPageNumber := @DoGetPageNumber;
          if OnGetImageURL <> '' then
            Module.OnGetImageURL := @DoGetImageURL;
          if OnBeforeDownloadImage <> '' then
            Module.OnBeforeDownloadImage := @DoBeforeDownloadImage;
          if OnDownloadImage <> '' then
            Module.OnDownloadImage := @DoDownloadImage;
          if OnSaveImage <> '' then
            Module.OnSaveImage := @DoSaveImage;
          if OnAfterImageSaved <> '' then
            Module.OnAfterImageSaved := @DoAfterImageSaved;
          if OnLogin <> '' then
            Module.OnLogin := @DoLogin;
        end;
      TempModuleList.Clear;
      SetLength(s, Length(s) - 2);
      Logger.Send('Loaded modules from ' + ExtractFileName(AFilename), s);
      s := '';
    end;
  if m <> nil then
    m.Free;
end;

procedure ScanLuaWebsiteModulesFile;
var
  d: String;
  f: TStringList;
  i: Integer;
begin
  d := LUA_WEBSITEMODULE_FOLDER;
  try
    f := FindAllFiles(d, '*.lua', False, faAnyFile);
    if f.Count > 0 then
      for i := 0 to f.Count - 1 do
        LoadLuaToWebsiteModules(f[i]);
  finally
    f.Free;
  end;
end;

{ TLuaWebsiteModulesManager }

constructor TLuaWebsiteModulesManager.Create;
begin
  Containers := TLuaWebsiteModulesContainers.Create;
  TempModuleList := TLuaWebsiteModules.Create;
end;

destructor TLuaWebsiteModulesManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to TempModuleList.Count - 1 do
    TempModuleList[i].Free;
  TempModuleList.Free;
  for i := 0 to Containers.Count - 1 do
    Containers[i].Free;
  Containers.Free;
  inherited Destroy;
end;

{ TLuaWebsiteModulesContainer }

constructor TLuaWebsiteModulesContainer.Create;
begin
  Modules := TLuaWebsiteModules.Create;
  ByteCode := nil;
end;

destructor TLuaWebsiteModulesContainer.Destroy;
var
  i: Integer;
begin
  if Assigned(ByteCode) then
    ByteCode.Free;
  for i := 0 to Modules.Count - 1 do
    Modules[i].Free;
  Modules.Free;
  inherited Destroy;
end;


{ TLuaWebsiteModule }

constructor TLuaWebsiteModule.Create;
begin
  LuaWebsiteModulesManager.TempModuleList.Add(Self);
  Storage := TStringsStorage.Create;
  Options := TStringList.Create;
  Options.OwnsObjects := True;
  Options.Duplicates := dupIgnore;
  Options.Sorted := True;
  Module := Modules.AddModule;
  Module.TagPtr := Self;
end;

destructor TLuaWebsiteModule.Destroy;
begin
  Options.Free;
  Storage.Free;
  inherited Destroy;
end;

function TLuaWebsiteModule.AddOption(const AName, ACaption: String; const AClass: TClass): Integer;
var
  o: TOptionItem;
begin
  Result := Options.Add(AName);
  if Result = -1 then Exit;
  o := TOptionItem(AClass.Create);
  o.Caption := ACaption;
  Options.Objects[Result] := o;
end;

procedure TLuaWebsiteModule.AddOptionCheckBox(const AName, ACaption: String;
  const ADefault: Boolean);
var
  o: TOptionItemCheckBox;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemCheckBox);
  if i = -1 then Exit;
  o := TOptionItemCheckBox(Options.Objects[i]);
  o.Value := ADefault;
  Module.AddOptionCheckBox(@o.Value, Options[i], @o.Caption);
end;

procedure TLuaWebsiteModule.AddOptionEdit(const AName, ACaption: String; const ADefault: String);
var
  o: TOptionItemEdit;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemEdit);
  if i = -1 then Exit;
  o := TOptionItemEdit(Options.Objects[i]);
  o.Value := ADefault;
  Module.AddOptionEdit(@o.Value, Options[i], @o.Caption);
end;

procedure TLuaWebsiteModule.AddOptionSpinEdit(const AName, ACaption: String;
  const ADefault: Integer);
var
  o: TOptionItemSpinEdit;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemSpinEdit);
  if i = -1 then Exit;
  o := TOptionItemSpinEdit(Options.Objects[i]);
  o.Value := ADefault;
  Module.AddOptionSpinEdit(@o.Value, Options[i], @o.Caption);
end;

procedure TLuaWebsiteModule.AddOptionComboBox(const AName, ACaption, AItems: String;
  const ADefault: Integer);
var
  o: TOptionItemComboBox;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemComboBox);
  if i = -1 then Exit;
  o := TOptionItemComboBox(Options.Objects[i]);
  o.Items := AItems;
  o.Value := ADefault;
  Module.AddOptionComboBox(@o.Value, Options[i], @o.Caption, @o.Items);
end;

procedure TLuaWebsiteModule.LuaPushMe(L: Plua_State);
begin
  luaPushObject(L, Self, 'module', @luaWebsiteModuleAddMetaTable);
  luaPushIntegerGlobal(L, 'no_error', NO_ERROR);
  luaPushIntegerGlobal(L, 'net_problem', NET_PROBLEM);
  luaPushIntegerGlobal(L, 'information_not_found', INFORMATION_NOT_FOUND);
end;

function TLuaWebsiteModule.LuaDoMe(L: Plua_State): Integer;
begin
  if AlwaysLoadLuaFromFile then
    Result := luaL_loadfile(L, PChar(Container.FileName))
  else
    Result := LuaLoadFromStream(L, Container.ByteCode, PChar(Container.FileName));
  if Result = 0 then
    Result := lua_pcall(L, 0, 0, 0);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function lua_addoptioncheckbox(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionCheckBox(
    lua_tostring(L, 1), lua_tostring(L, 2), lua_toboolean(L, 3));
end;

function lua_addoptionedit(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionEdit(
    lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3));
end;

function lua_addoptionspinedit(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionSpinEdit(
    lua_tostring(L, 1), lua_tostring(L, 2), lua_tointeger(L, 3));
end;

function lua_addoptioncombobox(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionComboBox(
    lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3), lua_tointeger(L, 4));
end;

function lua_gettotaldirectory(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TLuaWebsiteModule(luaClassGetObject(L)).Module.TotalDirectory);
  Result := 1;
end;

function lua_settotaldirectory(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).Module.TotalDirectory := lua_tointeger(L, 1);
end;

function lua_getoption(L: Plua_State): Integer; cdecl;
var
  m: TLuaWebsiteModule;
  i: Integer;
  o: TObject;
begin
  m := TLuaWebsiteModule(luaClassGetObject(L));
  i:=m.Options.IndexOf(lua_tostring(L, 1));
  if i = -1 then
    Result := 0
  else
  begin
    o := m.Options.Objects[i];
    if o is TOptionItemCheckBox then
      lua_pushboolean(L, TOptionItemCheckBox(o).Value)
    else
    if o is TOptionItemEdit then
      lua_pushstring(L, TOptionItemEdit(o).Value)
    else
    if o is TOptionItemSpinEdit then
      lua_pushinteger(L, TOptionItemSpinEdit(o).Value)
    else
    if o is TOptionItemComboBox then
      lua_pushinteger(L, TOptionItemComboBox(o).Value);
    Result := 1;
  end;
end;

const
  methods: packed array [0..5] of luaL_Reg = (
    (name: 'AddOptionCheckBox'; func: @lua_addoptioncheckbox),
    (name: 'AddOptionEdit'; func: @lua_addoptionedit),
    (name: 'AddOptionSpinEdit'; func: @lua_addoptionspinedit),
    (name: 'AddOptionCombobox'; func: @lua_addoptioncombobox),
    (name: 'GetOption'; func: @lua_getoption),
    (name: nil; func: nil)
    );

procedure luaWebsiteModuleAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TLuaWebsiteModule(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'Website', @Module.Website);
    luaClassAddStringProperty(L, MetaTable, 'RootURL', @Module.RootURL);
    luaClassAddStringProperty(L, MetaTable, 'Category', @Module.Category);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxTaskLimit', @Module.Settings.MaxTaskLimit);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxConnectionLimit',
      @Module.Settings.MaxConnectionLimit);
    luaClassAddIntegerProperty(L, MetaTable, 'ActiveTaskCount', @Module.ActiveTaskCount);
    luaClassAddIntegerProperty(L, MetaTable, 'ActiveConnectionCount',
      @Module.ActiveConnectionCount);
    luaClassAddBooleanProperty(L, MetaTable, 'AccountSupport', @Module.AccountSupport);
    luaClassAddBooleanProperty(L, MetaTable, 'SortedList', @Module.SortedList);
    luaClassAddBooleanProperty(L, MetaTable, 'InformationAvailable',
      @Module.InformationAvailable);
    luaClassAddBooleanProperty(L, MetaTable, 'FavoriteAvailable',
      @Module.FavoriteAvailable);
    luaClassAddBooleanProperty(L, MetaTable, 'DynamicPageLink', @Module.DynamicPageLink);
    luaClassAddBooleanProperty(L, MetaTable, 'DynamicPageLink',
      @Module.CloudflareEnabled);
    luaClassAddStringProperty(L, MetaTable, 'OnBeforeUpdateList', @OnBeforeUpdateList);
    luaClassAddStringProperty(L, MetaTable, 'OnAfterUpdateList', @OnAfterUpdateList);
    luaClassAddStringProperty(L, MetaTable, 'OnGetDirectoryPageNumber',
      @OnGetDirectoryPageNumber);
    luaClassAddStringProperty(L, MetaTable, 'OnGetNameAndLink', @OnGetNameAndLink);
    luaClassAddStringProperty(L, MetaTable, 'OnGetInfo', @OnGetInfo);
    luaClassAddStringProperty(L, MetaTable, 'OnTaskStart', @OnTaskStart);
    luaClassAddStringProperty(L, MetaTable, 'OnGetPageNumber', @OnGetPageNumber);
    luaClassAddStringProperty(L, MetaTable, 'OnGetImageURL', @OnGetImageURL);
    luaClassAddStringProperty(L, MetaTable, 'OnBeforeDownloadImage',
      @OnBeforeDownloadImage);
    luaClassAddStringProperty(L, MetaTable, 'OnDownloadImage', @OnDownloadImage);
    luaClassAddStringProperty(L, MetaTable, 'OnSaveImage', @OnSaveImage);
    luaClassAddStringProperty(L, MetaTable, 'OnAfterImageSaved', @OnAfterImageSaved);
    luaClassAddStringProperty(L, MetaTable, 'OnLogin', @OnLogin);
    luaClassAddStringProperty(L, MetaTable, 'LastUpdated', @LastUpdated);
    luaClassAddIntegerProperty(L, MetaTable, 'CurrentDirectoryIndex', @Module.CurrentDirectoryIndex);

    luaClassAddProperty(L, MetaTable, UserData, 'TotalDirectory', @lua_gettotaldirectory, @lua_settotaldirectory);

    luaClassAddFunction(L, MetaTable, UserData, methods);

    luaClassAddObject(L, MetaTable, Storage, 'Storage', @luaStringsStorageAddMetaTable);
  end;
end;

function _create(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TLuaWebsiteModule.Create, '', False,
    @luaWebsiteModuleAddMetaTable);
  Result := 1;
end;

procedure luaWebsiteModuleRegister(L: Plua_State);
begin
  lua_register(L, 'NewModule', @_create);
end;

initialization
  luaClassRegister(TLuaWebsiteModule, @luaWebsiteModuleAddMetaTable,
    @luaWebsiteModuleRegister);
  LuaWebsiteModulesManager := TLuaWebsiteModulesManager.Create;

finalization
  LuaWebsiteModulesManager.Free;

end.
