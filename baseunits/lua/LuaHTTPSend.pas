unit LuaHTTPSend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

implementation

uses
  uBaseUnit, httpsendthread, LuaClass;

type
  TUserData = THTTPSendThread;

function http_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).GET(lua_tostring(L, 1)));
  Result := 1;
end;

function http_post(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).POST(lua_tostring(L, 1),
    lua_tostring(L, 2)));
  Result := 1;
end;

function http_head(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).head(lua_tostring(L, 1)));
  Result := 1;
end;

function http_xhr(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).XHR(lua_tostring(L, 1)));
  Result := 1;
end;

function http_reset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Reset;
end;

function http_getcookies(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).GetCookies;
end;

function http_threadterminated(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).ThreadTerminated);
  Result := 1;
end;

function http_document(L: Plua_State): Integer; cdecl;
begin
  lua_pushlightuserdata(L, TUserData(luaClassGetObject(L)).Document);
  Result := 1;
end;

const
  methods: packed array [0..6] of luaL_Reg = (
    (name: 'GET'; func: @http_get),
    (name: 'POST'; func: @http_post),
    (name: 'HEAD'; func: @http_head),
    (name: 'XHR'; func: @http_xhr),
    (name: 'Reset'; func: @http_reset),
    (name: 'GetCookies'; func: @http_getcookies),
    (name: nil; func: nil)
    );
  props: packed array[0..2] of luaL_Reg_prop = (
    (name: 'Document'; funcget: @http_document; funcset: nil),
    (name: 'ThreadTerminated'; funcget: @http_threadterminated; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaHTTPSendThreadAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TUserData(Obj) do
  begin
    luaClassAddFunction(L, MetaTable, UserData, methods);
    luaClassAddProperty(L, MetaTable, UserData, props);
    luaClassAddObject(L, Headers, MetaTable, 'Headers');
    luaClassAddObject(L, Cookies, MetaTable, 'Cookies');
  end;
end;

initialization
  luaClassRegister(THTTPSendThread, @luaHTTPSendThreadAddMetaTable);

end.
