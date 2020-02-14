unit LuaHTTPSend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaHTTPSendThreadAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  uBaseUnit, httpsendthread, LuaClass, LuaUtils;

type
  TUserData = THTTPSendThread;

function http_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).GET(luaGetString(L, 1)));
  Result := 1;
end;

function http_post(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).POST(luaGetString(L, 1),
    luaGetString(L, 2)));
  Result := 1;
end;

function http_head(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).head(luaGetString(L, 1)));
  Result := 1;
end;

function http_xhr(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).XHR(luaGetString(L, 1)));
  Result := 1;
end;

function http_reset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Reset;
end;

function http_getcookies(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).GetCookies);
  Result := 1;
end;

function http_threadterminated(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).ThreadTerminated);
  Result := 1;
end;

function http_setproxy(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SetProxy(luaGetString(L, 1), luaGetString(L, 2),
    luaGetString(L, 3), luaGetString(L, 4), luaGetString(L, 5));
end;

function http_threadlasturl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).LastURL);
  Result := 1;
end;

function http_threadresultcode(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).ResultCode);
  Result := 1;
end;

function http_threadresultstring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).ResultString);
  Result := 1;
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
  props: packed array[0..4] of luaL_Reg_prop = (
    (name: 'Terminated'; funcget: @http_threadterminated; funcset: nil),
    (name: 'LastURL'; funcget: @http_threadlasturl; funcset: nil),
    (name: 'ResultCode'; funcget: @http_threadresultcode; funcset: nil),
    (name: 'ResultString'; funcget: @http_threadresultstring; funcset: nil),
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
    luaClassAddObject(L, MetaTable, TUserData(Obj).Document, 'Document');
  end;
end;

initialization
  luaClassRegister(THTTPSendThread, @luaHTTPSendThreadAddMetaTable);

end.
