unit LuaXQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, XQueryEngineHTML, xquery;

procedure luaXQueryPush(L: Plua_State; Obj: TXQueryEngineHTML; Name: String = '';
  AutoFree: Boolean = False); inline;

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
      luaXQueryPush(L, TXQueryEngineHTML.Create(luaGetString(L, 1)), '', True)
    else
    if lua_isuserdata(L, 1) then
      luaXQueryPush(L, TXQueryEngineHTML.Create(TStream(luaGetUserData(L, 1))),
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
    u.ParseHTML(luaGetString(L, 1))
  else
  if lua_isuserdata(L, 1) then
    u.ParseHTML(TStream(luaGetUserData(L, 1)));
end;

function xquery_xpath(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  x: IXQValue;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    x := u.XPath(luaGetString(L, 1), TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue)
  else
    x := u.XPath(luaGetString(L, 1));
  luaIXQValuePush(L, TLuaIXQValue.Create(x));
  Result := 1;
end;

function xquery_xpathi(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  x: IXQValue;
  t, i: Integer;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    x := u.XPath(luaGetString(L, 1), TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue)
  else
    x := u.XPath(luaGetString(L, 1));
  lua_newtable(L);
  t := lua_gettop(L);
  for i := 1 to x.Count do
  begin
    luaIXQValuePush(L, TLuaIXQValue.Create(x.get(i)));
    lua_rawseti(L, t, i);
  end;
  Result := 1;
end;

function xquery_xpathstring(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    lua_pushstring(L, u.XPathString(luaGetString(L, 1),
      TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue))
  else
    lua_pushstring(L, u.XPathString(luaGetString(L, 1)));
  Result := 1;
end;

function xquery_xpathstringall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  Result := 0;
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    1: begin
         lua_pushstring(L, u.XPathStringAll(luaGetString(L, 1)));
         Result := 1;
       end;
    2: begin
         if lua_isstring(L, 2) then
         begin
           lua_pushstring(L, u.XPathStringAll(luaGetString(L, 1), luaGetString(L, 2)));
           Result := 1;
         end
         else
         if lua_isuserdata(L, 2) then
         begin
           u.XPathStringAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)));
           Result := 0;
         end;
       end;
    3: begin
         if lua_isstring(L, 2) then
         begin
           lua_pushstring(L, u.XPathStringAll(luaGetString(L, 1), luaGetString(L, 2),
             TLuaIXQValue(luaGetUserData(L, 3)).FIXQValue));
           Result := 1;
         end
         else
         if lua_isuserdata(L, 2) then
         begin
           u.XPathStringAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
             TLuaIXQValue(luaGetUserData(L, 3)).FIXQValue);
           Result := 0;
         end;
       end;
    end;
end;

function xquery_xpathhrefall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    3: u.XPathHREFAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)));
    4: u.XPathHREFAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)), TLuaIXQValue(luaGetUserData(L, 4)).FIXQValue)
  end;
  Result := 0;
end;

function xquery_xpathhreftitleall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    3: u.XPathHREFtitleAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)));
    4: u.XPathHREFtitleAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)), TLuaIXQValue(luaGetUserData(L, 4)).FIXQValue)
  end;
  Result := 0;
end;

function xquery_xpathcount(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    lua_pushinteger(L, u.XPathCount(luaGetString(L, 1),
      TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue))
  else
    lua_pushinteger(L, u.XPathCount(luaGetString(L, 1)));
  Result := 1;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @xquery_create),
    (name: 'Create'; func: @xquery_create),
    (name: nil; func: nil)
    );
  methods: packed array [0..8] of luaL_Reg = (
    (name: 'ParseHTML'; func: @xquery_parsehtml),
    (name: 'XPath'; func: @xquery_xpath),
    (name: 'XPathI'; func: @xquery_xpathi),
    (name: 'XPathString'; func: @xquery_xpathstring),
    (name: 'XpathStringAll'; func: @xquery_xpathstringall),
    (name: 'XpathHREFAll'; func: @xquery_xpathhrefall),
    (name: 'XpathHREFTitleAll'; func: @xquery_xpathhreftitleall),
    (name: 'XPathCount'; func: @xquery_xpathcount),
    (name: nil; func: nil)
    );

procedure luaXQueryAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
end;

procedure luaXQueryPush(L: Plua_State; Obj: TXQueryEngineHTML; Name: String;
  AutoFree: Boolean);
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
