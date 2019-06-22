unit LuaRegExpr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, RegExpr;

procedure luaRegExprRegister(L: Plua_State);

implementation

uses
  LuaUtils;

function re_exec(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, ExecRegExpr(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function re_replace(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ReplaceRegExpr(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3), True));
  Result := 1;
end;

procedure luaRegExprRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'ExecRegExpr', @re_exec);
  luaPushFunctionGlobal(L, 'ReplaceRegExpr', @re_replace);
end;

end.
