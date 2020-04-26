unit LuaUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaAddCFunctionToTable(L: Plua_State; Table: Integer;
  Name: PAnsiChar; Func: lua_CFunction);
procedure luaAddCClosureToTable(L: Plua_State; Table, Value: Integer;
  Name: PAnsiChar; Func: lua_CFunction); overload;
procedure luaAddCClosureToTable(L: Plua_State; Table: Integer;
  Value: Pointer; Name: PAnsiChar; Func: lua_CFunction); overload;
procedure luaAddStringToTable(L: Plua_State; Table: Integer; Name, Value: PAnsiChar);
procedure luaAddIntegerToTable(L: Plua_State; Table: Integer;
  Name: PAnsiChar; Value: lua_Integer);
procedure luaAddBooleanToTable(L: Plua_State; Table: Integer;
  Name: PAnsiChar; Value: Boolean);

procedure luaPushFunctionGlobal(L: Plua_State; Name: PAnsiChar; Func: lua_CFunction);

procedure luaPushStringGlobal(L: Plua_State; Name: PAnsiChar; S: String);
procedure luaPushIntegerGlobal(L: Plua_State; Name: PAnsiChar; I: Integer);
procedure luaPushBooleanGlobal(L: Plua_State; Name: PAnsiChar; B: Boolean);

procedure luaPushUserData(L: Plua_State; U: Pointer); overload; inline;
procedure luaPushUserData(L: Plua_State; U: Pointer; var UIndex: Integer); overload; inline;
function luaGetUserData(L: Plua_State; idx: Integer): Pointer; inline;
function luaGetString(L: Plua_State; idx: Integer): String; inline;

function LuaToString(L: Plua_State; Idx: Integer): String;
function LuaStackToString(L: Plua_State): String;

procedure luaL_newlib(L: Plua_State; n: PAnsiChar; lr: PluaL_Reg); overload; inline;

// deprecated since 5.2
procedure luaL_openlib(L: Plua_State; n: PansiChar; lr: PluaL_Reg;
  {%H-}nup: Integer); inline;
procedure luaL_register(L: Plua_State; n: PAnsiChar; lr: PluaL_Reg); inline;

implementation

procedure luaAddCFunctionToTable(L: Plua_State; Table: Integer;
  Name: PAnsiChar; Func: lua_CFunction);
begin
  lua_pushstring(L, Name);
  lua_pushcfunction(L, Func);
  lua_rawset(L, Table);
end;

procedure luaAddCClosureToTable(L: Plua_State; Table, Value: Integer;
  Name: PAnsiChar; Func: lua_CFunction);
begin
  lua_pushstring(L, Name);
  lua_pushvalue(L, Value);
  lua_pushcclosure(L, Func, 1);
  lua_rawset(L, Table);
end;

procedure luaAddCClosureToTable(L: Plua_State; Table: Integer;
  Value: Pointer; Name: PAnsiChar; Func: lua_CFunction);
begin
  lua_pushstring(L, Name);
  lua_pushlightuserdata(L, Value);
  lua_pushcclosure(L, Func, 1);
  lua_rawset(L, Table);
end;

procedure luaAddStringToTable(L: Plua_State; Table: Integer; Name, Value: PAnsiChar);
begin
  lua_pushstring(L, Name);
  lua_pushstring(L, Value);
  lua_rawset(L, Table);
end;

procedure luaAddIntegerToTable(L: Plua_State; Table: Integer;
  Name: PAnsiChar; Value: lua_Integer);
begin
  lua_pushstring(L, Name);
  lua_pushinteger(L, Value);
  lua_rawset(L, Table);
end;

procedure luaAddBooleanToTable(L: Plua_State; Table: Integer;
  Name: PAnsiChar; Value: Boolean);
begin
  lua_pushstring(L, Name);
  lua_pushboolean(L, Value);
  lua_rawset(L, Table);
end;

procedure luaPushFunctionGlobal(L: Plua_State; Name: PAnsiChar; Func: lua_CFunction);
begin
  lua_pushcfunction(L, Func);
  lua_setglobal(L, Name);
end;

procedure luaPushStringGlobal(L: Plua_State; Name: PAnsiChar; S: String);
begin
  lua_pushstring(L, S);
  lua_setglobal(L, Name);
end;

procedure luaPushIntegerGlobal(L: Plua_State; Name: PAnsiChar; I: Integer);
begin
  lua_pushinteger(L, I);
  lua_setglobal(L, Name);
end;

procedure luaPushBooleanGlobal(L: Plua_State; Name: PAnsiChar; B: Boolean);
begin
  lua_pushboolean(L, B);
  lua_setglobal(L, Name);
end;

procedure luaPushUserData(L: Plua_State; U: Pointer);
begin
  PPointer(lua_newuserdata(L, SizeOf(PPointer)))^ := U;
end;

procedure luaPushUserData(L: Plua_State; U: Pointer; var UIndex: Integer);
begin
  luaPushUserData(L, U);
  UIndex := lua_gettop(L);
end;

function luaGetUserData(L: Plua_State; idx: Integer): Pointer;
begin
  Result := PPointer(lua_touserdata(L, idx))^;
end;

function luaGetString(L: Plua_State; idx: Integer): String;
var
  slen: size_t;
begin
  Result := lua_tolstring(L, idx, @slen);
  SetLength(Result, slen);
end;

function LuaToString(L: Plua_State; Idx: Integer): String;
begin
  if lua_isuserdata(L, Idx) then
    Result := 'userdata: ' + hexStr(lua_touserdata(L, Idx))
  else
  if lua_isstring(L, Idx) then
    Result := 'string: ' + luaGetString(L, Idx)
  else
  if lua_isinteger(L, Idx) then
    Result := 'integer: ' + IntToStr(lua_tointeger(L, Idx))
  else
  if lua_iscfunction(L, Idx) then
    Result := 'cfunc: ' + hexStr(lua_topointer(L, Idx))
  else
  if lua_isfunction(L, Idx) then
    Result := 'func: ' + hexStr(lua_topointer(L, Idx))
  else
  if lua_isnoneornil(L, Idx) then
    Result := 'nil'
  else
  if lua_isboolean(L, Idx) then
    Result := 'boolean: ' + BoolToStr(lua_toboolean(L, Idx), True)
  else
  if lua_isnumber(L, Idx) then
    Result := 'number: ' + FloatToStr(lua_tonumber(L, Idx))
  else
  if lua_istable(L, Idx) then
    Result := 'table: ' + hexStr(lua_topointer(L, Idx))
  else
  if lua_islightuserdata(L, Idx) then
    Result := 'ligthuserdata: ' + hexStr(lua_topointer(L, Idx))
  else
    Result := 'unknown: ' + hexStr(lua_topointer(L, Idx));
end;

function LuaStackToString(L: Plua_State): String;
var
  i: Integer;
begin
  Result := '';
  i := lua_gettop(L);
  if i = 0 then
    Exit;
  for i := 1 to i do
    Result := Result + IntToStr(i) + '=' + LuaToString(L, i) + LineEnding;
  SetLength(Result, Length(Result) - Length(LineEnding));
end;

procedure luaL_newlib(L: Plua_State; n: PAnsiChar; lr: PluaL_Reg);
begin
  luaL_newlib(L, lr);
  if n <> '' then
    lua_setglobal(L, n);
end;

procedure luaL_openlib(L: Plua_State; n: PansiChar; lr: PluaL_Reg; nup: Integer);
begin
  luaL_setfuncs(L, lr, 0);
  if n <> '' then
    lua_setglobal(L, n);
end;

procedure luaL_register(L: Plua_State; n: PAnsiChar; lr: PluaL_Reg);
begin
  luaL_openlib(L, n, lr, 0);
end;

end.
