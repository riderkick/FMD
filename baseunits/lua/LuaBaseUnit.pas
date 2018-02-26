unit LuaBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, uBaseUnit;

procedure luaBaseUnitRegister(L: Plua_State);

implementation

uses
  LuaUtils, MultiLog;

function lua_pos(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Pos(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function lua_trim(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, Trim(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_maybefillhost(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MaybeFillHost(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function lua_invertstrings(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to lua_gettop(L) do
    InvertStrings(TStringList(luaGetUserData(L, i)));
end;

function lua_mangainfostatusifpos(L: Plua_State): Integer; cdecl;
begin
  case lua_gettop(L) of
    3: lua_pushstring(L, MangaInfoStatusIfPos(lua_tostring(L, 1),
        lua_tostring(L, 2), lua_tostring(L, 3)));
    2: lua_pushstring(L, MangaInfoStatusIfPos(lua_tostring(L, 1), lua_tostring(L, 2)));
    1: lua_pushstring(L, MangaInfoStatusIfPos(lua_tostring(L, 1)));
    else
      Exit(0);
  end;
  Result := 1;
end;

function lua_appendurldelim(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AppendURLDelim(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_removeurldelim(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, RemoveURLDelim(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_appendurldelimleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AppendURLDelimLeft(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_removeurldelimleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, RemoveURLDelimLeft(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_regexprgetmatch(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, RegExprGetMatch(lua_tostring(L, 1), lua_tostring(L, 2), lua_tointeger(L, 3)));
  Result := 1;
end;

function lua_htmldecode(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HTMLDecode(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_urldecode(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, URLDecode(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_incstr(L: Plua_State): Integer; cdecl;
var
  n: Integer;
begin
  n := 1;
  if (lua_gettop(L) = 2) and lua_isinteger(L, 2) then
    n := lua_tointeger(L, 2);
  if lua_isinteger(L, 1) then
    lua_pushstring(L, IncStr(lua_tointeger(L, 1), n))
  else
    lua_pushstring(L, IncStr(lua_tostring(L, 1), n));
  Result := 1;
end;

procedure luaBaseUnitRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'Pos', @lua_pos);
  luaPushFunctionGlobal(L, 'Trim', @lua_trim);
  luaPushFunctionGlobal(L, 'MaybeFillHost', @lua_maybefillhost);
  luaPushFunctionGlobal(L, 'InvertStrings', @lua_invertstrings);
  luaPushFunctionGlobal(L, 'MangaInfoStatusIfPos', @lua_mangainfostatusifpos);
  luaPushFunctionGlobal(L, 'AppendURLDelim', @lua_appendurldelim);
  luaPushFunctionGlobal(L, 'AppendURLDelimleft', @lua_appendurldelimleft);
  luaPushFunctionGlobal(L, 'RemoveURLDelim', @lua_removeurldelim);
  luaPushFunctionGlobal(L, 'RemoveURLDelimLeft', @lua_removeurldelimleft);
  luaPushFunctionGlobal(L, 'RegExprGetMatch', @lua_regexprgetmatch);
  luaPushFunctionGlobal(L, 'HTMLDecode', @lua_htmldecode);
  luaPushFunctionGlobal(L, 'URLDecode', @lua_urldecode);
  luaPushFunctionGlobal(L, 'IncStr', @lua_incstr);
end;

end.
