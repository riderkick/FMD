unit LuaBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53, LuaClass;

procedure LuaBaseRegister(L: Plua_State);
procedure luaPushObject(L: Plua_State; Obj: TObject; Name: String;
  AddMetaTable: TluaClassAddMetaTable = nil; AutoFree: Boolean = False); inline;

function LuaDoFile(AFilename: String; AFuncName: String = ''): Plua_State;
function LuaNewBaseState: Plua_State;
function LuaCallFunction(L: Plua_State; AFuncName: String): Boolean;

function LuaDumpFileToStream(L: Plua_State; AFilename: String;
  AStripDebug: Boolean = False): TMemoryStream;
function LuaLoadFromStream(L: Plua_State; AStream: TMemoryStream; AName: PAnsiChar): Integer;

implementation

uses
  luaStrings, LuaBaseUnit, LuaRegExpr, LuaSynaUtil, LuaSynaCode, MultiLog;

function luabase_print(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to lua_gettop(L) do
    Logger.Send(lua_tostring(L, i));
end;

procedure LuaBaseRegister(L: Plua_State);
begin
  lua_register(L, 'print', @luabase_print);

  luaBaseUnitRegister(L);
  luaRegExprRegister(L);
  luaSynaUtilRegister(L);
  luaSynaCodeRegister(L);

  luaClassRegisterAll(L);
end;

procedure luaPushObject(L: Plua_State; Obj: TObject; Name: String;
  AddMetaTable: TluaClassAddMetaTable; AutoFree: Boolean);
begin
  luaClassPushObject(L, Obj, Name, AutoFree, AddMetaTable);
end;

function LuaDoFile(AFilename: String; AFuncName: String): Plua_State;
begin
  Result := nil;
  if not FileExists(AFilename) then
    Exit;
  Result := luaL_newstate;
  try
    luaL_openlibs(Result);
    LuaBaseRegister(Result);
    if luaL_dofile(Result, PChar(AFilename)) <> 0 then
      raise Exception.Create('');
    LuaCallFunction(Result, AFuncName);
  except
    Logger.SendError('LuaDoFile.Error ' + lua_tostring(Result, -1));
  end;
end;

function LuaNewBaseState: Plua_State;
begin
  Result := nil;
  Result := luaL_newstate;
  try
    luaL_openlibs(Result);
    LuaBaseRegister(Result);
  except
    Logger.SendError(lua_tostring(Result, -1));
  end;
end;

function LuaCallFunction(L: Plua_State; AFuncName: String): Boolean;
begin
  Result := False;
  if lua_getglobal(L, PChar(AFuncName)) = 0 then
    raise Exception.Create('No function name ' + QuotedStr(AFuncName));
  if lua_pcall(L, 0, -1, 0) <> 0 then
    raise Exception.Create('');
  Result := True;
end;

function _luawriter(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;
begin
  if TMemoryStream(ud).Write(p^, sz) <> sz then
    Result := 1
  else
    Result := 0;
end;

function LuaDumpFileToStream(L: Plua_State; AFilename: String;
  AStripDebug: Boolean): TMemoryStream;
var
  strip: Integer;
begin
  if not FileExists(AFilename) then
    Exit;
  Result := TMemoryStream.Create;
  try
    if luaL_loadfile(L, PChar(AFilename)) <> 0 then
      raise Exception.Create('');
    if AStripDebug then
      strip := 0
    else
      strip := 1;
    if lua_dump(L, @_luawriter, Result, strip) <> 0 then
      raise Exception.Create('');
  except
    Result.Free;
    Result := nil;
    Logger.SendError(lua_tostring(L, -1));
  end;
end;

function _luareader(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
var
  m: TMemoryStream;
begin
  m := TMemoryStream(ud);
  if m.Size = 0 then
  begin
    Result := nil;
    Exit;
  end;
  Result := PAnsiChar(m.Memory);
  sz^ := m.Size;
end;

function LuaLoadFromStream(L: Plua_State; AStream: TMemoryStream; AName: PAnsiChar): Integer;
begin
  Result := lua_load(L, @_luareader, AStream, AName, 'b');
end;

end.
