unit LuaPCRE2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaPCRE2Register(L: Plua_State);

implementation

uses
  pcre2, LuaClass, LuaUtils;

function re_match(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, PCRE2Match(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function re_replace(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L)=4 then
    lua_pushstring(L, PCRE2Replace(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3),lua_toboolean(L,4)))
  else
    lua_pushstring(L, PCRE2Replace(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3)));
  Result := 1;
end;

const
  methods: packed array [0..2] of luaL_Reg = (
    (name: 'match'; func: @re_match),
    (name: 'replace'; func: @re_replace),
    (name: nil; func: nil)
    );

procedure luaPCRE2Register(L: Plua_State);
begin
  luaClassNewLib(L,'re',methods);
end;

end.
