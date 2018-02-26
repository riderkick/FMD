unit LuaHTTPSend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaHTTPSendThreadAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean = False);

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

function http_setproxy(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SetProxy(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tostring(L, 3), lua_tostring(L, 4), lua_tostring(L, 5));
end;

const
  methods: packed array [0..7] of luaL_Reg = (
    (name: 'GET'; func: @http_get),
    (name: 'POST'; func: @http_post),
    (name: 'HEAD'; func: @http_head),
    (name: 'XHR'; func: @http_xhr),
    (name: 'Reset'; func: @http_reset),
    (name: 'GetCookies'; func: @http_getcookies),
    (name: 'SetProxy'; func: @http_setproxy),
    (name: nil; func: nil)
    );
  props: packed array[0..1] of luaL_Reg_prop = (
    (name: 'Terminated'; funcget: @http_threadterminated; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaHTTPSendThreadAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean = False);
begin
  with TUserData(Obj) do
  begin
    luaClassAddFunction(L, MetaTable, UserData, methods);
    luaClassAddProperty(L, MetaTable, UserData, props);
    luaClassAddObject(L, MetaTable, Headers, 'Headers');
    luaClassAddObject(L, MetaTable, Cookies, 'Cookies');
    luaClassAddStringProperty(L, MetaTable, 'MimeType', @TUserData(Obj).MimeType);
    luaClassAddStringProperty(L, MetaTable, 'UserAgent', @TUserData(Obj).UserAgent);
    luaClassAddUserData(L, MetaTable, TUserData(Obj).Document, 'Document');
  end;
end;

initialization
  luaClassRegister(THTTPSendThread, @luaHTTPSendThreadAddMetaTable);

end.
