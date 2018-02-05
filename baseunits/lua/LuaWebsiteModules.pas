unit LuaWebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, LuaStringsStorage, WebsiteModules, uData,
  uDownloadsManager, xquery, httpsendthread;

type

  { TLuaWebsiteModuleContainer }

  TLuaWebsiteModuleContainer = class
  private
    FModule: TModuleContainer;
  public
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
    Filename: String;
    LastUpdated: String;
    constructor Create;
    destructor Destroy; override;
    procedure luaPushMe(L: Plua_State);
  end;

procedure ScanLuaWebsiteModulesFile;

implementation

uses
  FMDOptions, FileUtil, MultiLog, LuaClass, LuaBase, LuaMangaInfo, LuaHTTPSend,
  LuaXQuery, LuaUtils, LuaDownloadTask;

var
  luawebsitemodulelist: TFPList;

function DoBeforeUpdateList(const Module: TModuleContainer): Boolean;
var
  l: Plua_State;
begin
  Result := False;
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushIntegerGlobal(l, 'page', Page);
      luaPushIntegerGlobal(l, 'workptr', WorkPtr);
      luaPushObject(l, MangaInfo.mangaInfo, 'mangainfo');
      luaPushObject(l, MangaInfo.FHTTP, 'http');

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, MangaInfo.mangaInfo, 'mangainfo');
      luaPushObject(l, MangaInfo.FHTTP, 'http');
      luaPushStringGlobal(L, 'url', AURL);
      luaPushObject(l, ANames, 'names');
      luaPushObject(l, ANames, 'links');

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushStringGlobal(l, 'url', AURL);
      luaPushObject(l, MangaInfo.mangaInfo, 'mangainfo');
      luaPushObject(l, MangaInfo.FHTTP, 'http');

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, Task, 'task');

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, DownloadThread.FHTTP, 'http');
      luaPushStringGlobal(l, 'url', AURL);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, DownloadThread.FHTTP, 'http');
      luaPushStringGlobal(l, 'url', AURL);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, DownloadThread.FHTTP, 'http');
      luaPushStringGlobal(l, 'url', AURL);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, DownloadThread.FHTTP, 'http');
      luaPushStringGlobal(l, 'url', AURL);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, AHTTP, 'http');
      luaPushStringGlobal(l, 'path', APath);
      luaPushStringGlobal(l, 'name', AName);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushStringGlobal(l, 'filename', AFilename);

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  with TLuaWebsiteModuleContainer(Module.TagPtr) do
  begin
    l := LuaNewBaseState;
    try
      luaPushMe(l);
      luaPushObject(l, AHTTP, 'http');

      if luaL_dofile(l, PChar(Filename)) <> 0 then
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
  m: Pointer;
  i: Integer;

  procedure setcurrent;
  begin
    with TLuaWebsiteModuleContainer(m) do
    begin
      Filename := AFilename;
      if OnBeforeUpdateList <> '' then
        FModule.OnBeforeUpdateList := @DoBeforeUpdateList;
      if OnAfterUpdateList <> '' then
        FModule.OnAfterUpdateList := @DoAfterUpdateList;
      if OnGetDirectoryPageNumber <> '' then
        FModule.OnGetDirectoryPageNumber := @DoGetDirectoryPageNumber;
      if OnGetNameAndLink <> '' then
        FModule.OnGetNameAndLink := @DoGetNameAndLink;
      if OnGetInfo <> '' then
        FModule.OnGetInfo := @DoGetInfo;
      if OnTaskStart <> '' then
        FModule.OnTaskStart := @DoTaskStart;
      if OnGetPageNumber <> '' then
        FModule.OnGetPageNumber := @DoGetPageNumber;
      if OnGetImageURL <> '' then
        FModule.OnGetImageURL := @DoGetImageURL;
      if OnBeforeDownloadImage <> '' then
        FModule.OnBeforeDownloadImage := @DoBeforeDownloadImage;
      if OnDownloadImage <> '' then
        FModule.OnDownloadImage := @DoDownloadImage;
      if OnSaveImage <> '' then
        FModule.OnSaveImage := @DoSaveImage;
      if OnAfterImageSaved <> '' then
        FModule.OnAfterImageSaved := @DoAfterImageSaved;
      if OnLogin <> '' then
        FModule.OnLogin := @DoLogin;

      Logger.EnterMethod('lualoadmodule' + IntToStr(i), ' ');
      Logger.Send('File', Filename);
      Logger.Send('Website', FModule.Website);
      Logger.Send('RootURL', FModule.RootURL);
      Logger.Send('LastUpdated', LastUpdated);
      Logger.ExitMethod('lualoadmodule' + IntToStr(i), ' ');
    end;
  end;

begin
  Result := False;
  Logger.Send('Load lua website module', AFilename);
  try
    l := LuaDoFile(AFilename, 'Init');
  except
    Logger.SendError('Error load lua website module');
  end;
  if Assigned(l) then
  begin
    logger.Send('Read returned data from lua website module');
    Result := True;
    try
      for i := 1 to lua_gettop(L) do
        if lua_isuserdata(L, i) then
        begin
          m := lua_touserdata(L, i);
          if TObject(m) is TLuaWebsiteModuleContainer then
            setcurrent
          else
          begin
            m := PPointer(m)^;
            if TObject(m) is TLuaWebsiteModuleContainer then
              setcurrent;
          end;
        end
        else
          Logger.SendWarning('Lua module doesn''t return any data!');
    finally
      lua_close(l);
    end;
  end;
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


{ TLuaWebsiteModuleContainer }

constructor TLuaWebsiteModuleContainer.Create;
begin
  luawebsitemodulelist.Add(Self);
  FModule := Modules.AddModule;
  FModule.TagPtr := Self;
  Storage := TStringsStorage.Create;
end;

destructor TLuaWebsiteModuleContainer.Destroy;
begin
  Storage.Free;
  inherited Destroy;
end;

procedure TLuaWebsiteModuleContainer.luaPushMe(L: Plua_State);
begin
  luaPushObject(L, Self, 'module');
  luaPushIntegerGlobal(L, 'no_error', NO_ERROR);
  luaPushIntegerGlobal(L, 'net_problem', NET_PROBLEM);
  luaPushIntegerGlobal(L, 'information_not_found', INFORMATION_NOT_FOUND);
end;

procedure luaWebsiteModuleAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TLuaWebsiteModuleContainer(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'Website', @FModule.Website);
    luaClassAddStringProperty(L, MetaTable, 'RootURL', @FModule.RootURL);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxTaskLimit', @FModule.MaxTaskLimit);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxConnectionLimit',
      @FModule.MaxConnectionLimit);
    luaClassAddIntegerProperty(L, MetaTable, 'ActiveTaskCount',
      @FModule.ActiveTaskCount);
    luaClassAddIntegerProperty(L, MetaTable, 'ActiveConnectionCount',
      @FModule.ActiveConnectionCount);
    luaClassAddBooleanProperty(L, MetaTable, 'AccountSupport', @FModule.AccountSupport);
    luaClassAddBooleanProperty(L, MetaTable, 'SortedList', @FModule.SortedList);
    luaClassAddBooleanProperty(L, MetaTable, 'InformationAvailable',
      @FModule.InformationAvailable);
    luaClassAddBooleanProperty(L, MetaTable, 'FavoriteAvailable',
      @FModule.FavoriteAvailable);
    luaClassAddBooleanProperty(L, MetaTable, 'DynamicPageLink',
      @FModule.DynamicPageLink);
    luaClassAddBooleanProperty(L, MetaTable, 'DynamicPageLink',
      @FModule.CloudflareEnabled);
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
    luaClassAddObject(L, Storage, MetaTable, 'Storage');
  end;
end;

function _create(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TLuaWebsiteModuleContainer.Create, '', False,
    @luaWebsiteModuleAddMetaTable);
  Result := 1;
end;

procedure luaWebsiteModuleRegister(L: Plua_State);
begin
  lua_register(L, 'NewModule', @_create);
end;

procedure dofinalization;
var
  i: Integer;
begin
  for i := 0 to luawebsitemodulelist.Count - 1 do
    TObject(luawebsitemodulelist[i]).Free;
  luawebsitemodulelist.Free;
end;

initialization
  luawebsitemodulelist := TFPList.Create;
  luaClassRegister(TLuaWebsiteModuleContainer, @luaWebsiteModuleAddMetaTable,
    @luaWebsiteModuleRegister);

finalization
  dofinalization;

end.
