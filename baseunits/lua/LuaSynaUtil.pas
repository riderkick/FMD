unit LuaSynaUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, synautil;

procedure luaSynaUtilRegister(L: Plua_State);

implementation

uses
  LuaUtils;

function lua_getbetween(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, GetBetween(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3)));
  Result := 1;
end;

function lua_separateleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SeparateLeft(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function lua_separateright(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SeparateRight(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function lua_replacestring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ReplaceString(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3)));
  Result := 1;
end;

procedure luaSynaUtilRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'GetBetween', @lua_getbetween);
  luaPushFunctionGlobal(L, 'SeparateLeft', @lua_separateleft);
  luaPushFunctionGlobal(L, 'SeparateRight', @lua_separateright);
  luaPushFunctionGlobal(L, 'ReplaceString', @lua_replacestring);
end;

end.

