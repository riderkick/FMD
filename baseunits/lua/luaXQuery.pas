unit LuaXQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, XQueryEngineHTML, xquery;

procedure luaXQueryPush(L: Plua_State; Obj: TXQueryEngineHTML;
  Name: String = ''; AutoFree: Boolean = False); inline;

implementation

uses
  LuaClass, LuaIXQValue, LuaUtils;

type
  TUserData = TXQueryEngineHTML;

function xquery_create(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) = 1 then
  begin
    if lua_isstring(L, 1) then
      luaXQueryPush(L, TXQueryEngineHTML.Create(lua_tostring(L, 1)), '', True)
    else
    if lua_isuserdata(L, 1) then
      luaXQueryPush(L, TXQueryEngineHTML.Create(TStream(lua_touserdata(L, 1))),
        '', True);
  end
  else
    luaXQueryPush(L, TXQueryEngineHTML.Create, '', True);
  Result := 1;
end;

function xquery_parsehtml(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  Result := 0;
  u := TUserData(luaClassGetObject(L));
  if lua_isstring(L, 1) then
    u.ParseHTML(lua_tostring(L, 1))
  else
  if lua_isuserdata(L, 1) then
    u.ParseHTML(lua_tostring(L, 1));
end;

function xquery_xpath(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  x: IXQValue;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    x := u.XPath(lua_tostring(L, 1), IXQValue(PPointer(lua_touserdata(L, 2))^))
  else
    x := u.XPath(lua_tostring(L, 1));
  luaIXQValuePush(L, TLuaIXQValue.Create(x));
  Result := 1;
end;

function xquery_xpathstring(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    lua_pushstring(L, u.XPathString(lua_tostring(L, 1),
      TLuaIXQValue(PPointer(lua_touserdata(L, 2))^).FIXQValue))
  else
    lua_pushstring(L, u.XPathString(lua_tostring(L, 1)));
  Result := 1;
end;

function xquery_xpathstringall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    2: u.XPathStringAll(lua_tostring(L, 1), TStrings(PPointer(lua_touserdata(L, 2))^));
    3: u.XPathStringAll(lua_tostring(L, 1), TStrings(PPointer(lua_touserdata(L, 2))^),
        IXQValue(PPointer(lua_touserdata(L, 2))^));
  end;
  Result := 0;
end;

function xquery_xpathhrefall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    3: u.XPathHREFAll(lua_tostring(L, 1), TStrings(PPointer(lua_touserdata(L, 2))^),
        TStrings(PPointer(lua_touserdata(L, 3))^));
    4: u.XPathHREFAll(lua_tostring(L, 1), TStrings(PPointer(lua_touserdata(L, 2))^),
        TStrings(PPointer(lua_touserdata(L, 3))^),
        IXQValue(PPointer(lua_touserdata(L, 4))^))
  end;
  Result := 0;
end;

function xquery_xpathhreftitleall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    3: u.XPathHREFtitleAll(lua_tostring(L, 1), TStrings(PPointer(lua_touserdata(L, 2))^),
        TStrings(PPointer(lua_touserdata(L, 3))^));
    4: u.XPathHREFtitleAll(lua_tostring(L, 1), TStrings(PPointer(lua_touserdata(L, 2))^),
        TStrings(PPointer(lua_touserdata(L, 3))^),
        IXQValue(PPointer(lua_touserdata(L, 4))^))
  end;
  Result := 0;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @xquery_create),
    (name: 'Create'; func: @xquery_create),
    (name: nil; func: nil)
    );
  methods: packed array [0..6] of luaL_Reg = (
    (name: 'ParseHTML'; func: @xquery_parsehtml),
    (name: 'XPath'; func: @xquery_xpath),
    (name: 'XPathString'; func: @xquery_xpathstring),
    (name: 'XpathStringAll'; func: @xquery_xpathstringall),
    (name: 'XpathHREFAll'; func: @xquery_xpathhrefall),
    (name: 'XpathHREFTitle'; func: @xquery_xpathhreftitleall),
    (name: nil; func: nil)
    );

procedure luaXQueryAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
end;

procedure luaXQueryPush(L: Plua_State; Obj: TXQueryEngineHTML;
  Name: String; AutoFree: Boolean);
begin
  luaClassPushObject(L, Obj, Name, AutoFree, @luaXQueryAddMetaTable);
end;

procedure luaXQueryRegister(L: Plua_State);
begin
  luaClassNewLib(L, 'TXQuery', constructs);
end;

initialization
  luaClassRegister(TXQueryEngineHTML, @luaXQueryAddMetaTable, @luaXQueryRegister);

end.
