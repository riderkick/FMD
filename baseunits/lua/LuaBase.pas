unit LuaBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53;

procedure LuaBaseRegister(L: Plua_State);
procedure luaPushObject(L: Plua_State; Obj: TObject; Name: String;
  AutoFree: Boolean = False); inline;

function LuaDoFile(AFilename: String; AFuncName: String = ''): Plua_State;
function LuaNewBaseState: Plua_State;
function LuaCallFunction(L: Plua_State; AFuncName: String): Boolean;

implementation

uses
  LuaClass, luaStrings, LuaBaseUnit, LuaRegex, MultiLog;

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
  luaRegexRegister(L);
  luaBaseUnitRegister(L);
  luaClassRegisterAll(L);
end;

procedure luaPushObject(L: Plua_State; Obj: TObject; Name: String; AutoFree: Boolean);
begin
  luaClassPushObject(L, Obj, Name, AutoFree);
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

end.
