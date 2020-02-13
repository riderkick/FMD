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
  lua_pushboolean(L, ExecRegExpr(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function re_replace(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ReplaceRegExpr(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3), True));
  Result := 1;
end;

procedure luaRegExprRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'ExecRegExpr', @re_exec);
  luaPushFunctionGlobal(L, 'ReplaceRegExpr', @re_replace);
end;

end.
