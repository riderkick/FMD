unit LuaRegex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, RegExpr;

procedure luaRegexRegister(L: Plua_State);

implementation

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

const
  methods: packed array [0..2] of luaL_Reg = (
    (name: 'Exec'; func: @re_exec),
    (name: 'Replace'; func: @re_replace),
    (name: nil; func: nil)
    );

procedure luaRegexRegister(L: Plua_State);
begin
  lua_newtable(L);
  luaL_setfuncs(L, @methods, 0);
  lua_setglobal(L, 'RegExpr');
end;

end.
