unit LuaStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaStringsPush(L: Plua_State; Obj: TStrings; Name: String = '';
  AutoFree: Boolean = False); inline;

procedure luaStringsAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);

implementation

uses LuaClass, LuaUtils;

type
  TUserData = TStrings;

function strings_create(L: Plua_State): Integer; cdecl;
begin
  luaStringsPush(L, TStringList.Create, '', True);
  Result := 1;
end;

function strings_loadfromfile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).LoadFromFile(luaGetString(L, 1));
end;

function strings_loadfromstream(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).LoadFromStream(TStream(luaGetUserData(L, 1)));
end;

function strings_settext(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Text := luaGetString(L, 1);
end;

function strings_gettext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).Text);
  Result := 1;
end;

function strings_setcommatext(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).CommaText := luaGetString(L, 1);
  Result := 0;
end;

function strings_getcommatext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).CommaText);
  Result := 1;
end;

function strings_add(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).Add(luaGetString(L, 1));
  Result := 0;
end;

function strings_addtext(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).AddText(luaGetString(L, 1));
  Result := 0;
end;

function strings_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, PAnsiChar(
    TUserData(luaClassGetObject(L)).Strings[lua_tointeger(L, 1)]));
  Result := 1;
end;

function strings_set(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Strings[lua_tointeger(L, 1)] := luaGetString(L, 2);
end;

function strings_getdelimitedtext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).DelimitedText);
  Result := 1;
end;

function strings_setdelimitedtext(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).DelimitedText := luaGetString(L, 1);
  Result := 0;
end;

function strings_getdelimiter(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, PAnsiChar(
    String(TUserData(luaClassGetObject(L)).Delimiter)));
  Result := 1;
end;

function strings_setdelimiter(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Delimiter := String(luaGetString(L, 1))[1];
end;

function strings_namevalueseparatorget(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, PAnsiChar(
    String(TUserData(luaClassGetObject(L)).NameValueSeparator)));
  Result := 1;
end;

function strings_namevalueseparatorset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).NameValueSeparator := String(luaGetString(L, 1))[1];
end;

function strings_valuesget(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, PAnsiChar(
    TUserData(luaClassGetObject(L)).Values[luaGetString(L, 1)]));
  Result := 1;
end;

function strings_valuesset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Values[luaGetString(L, 1)] := luaGetString(L, 2);
end;

function strings_getcount(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Count);
  Result := 1;
end;

function strings_sort(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TStringList(TUserData(luaClassGetObject(L))).Sort;
end;

function strings_clear(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Clear;
end;

function strings_delete(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Delete(lua_tointeger(L, 1));
end;

function strings_indexof(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).IndexOf(luaGetString(L, 1)));
  Result := 1;
end;

function strings_indexofname(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).IndexOfName(luaGetString(L, 1)));
  Result := 1;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @strings_create),
    (name: 'Create'; func: @strings_create),
    (name: nil; func: nil)
    );
  methods: packed array [0..14] of luaL_Reg = (
    (name: 'LoadFromFile'; func: @strings_loadfromfile),
    (name: 'LoadFromStream'; func: @strings_loadfromstream),
    (name: 'SetText'; func: @strings_settext),
    (name: 'GetText'; func: @strings_gettext),
    (name: 'Add'; func: @strings_add),
    (name: 'AddText'; func: @strings_addtext),
    (name: 'Get'; func: @strings_get),
    (name: 'Set'; func: @strings_set),
    (name: 'GetCount'; func: @strings_getcount),
    (name: 'Sort'; func: @strings_sort),
    (name: 'Clear'; func: @strings_clear),
    (name: 'Delete'; func: @strings_delete),
    (name: 'IndexOf'; func: @strings_indexof),
    (name: 'IndexOfName'; func: @strings_indexofname),
    (name: nil; func: nil)
    );
  props: packed array[0..6] of lual_Reg_prop = (
    (name: 'Text'; funcget: @strings_gettext; funcset: @strings_settext),
    (name: 'CommaText'; funcget: @strings_getcommatext; funcset: @strings_setcommatext),
    (name: 'Count'; funcget: @strings_getcount; funcset: nil),
    (name: 'DelimitedText'; funcget: @strings_getdelimitedtext; funcset: @strings_setdelimitedtext),
    (name: 'Delimiter'; funcget: @strings_getdelimiter; funcset: @strings_setdelimiter),
    (name: 'NameValueSeparator'; funcget: @strings_namevalueseparatorget; funcset: @strings_namevalueseparatorset),
    (name: nil; funcget: nil; funcset: nil)
    );
  arrprops: packed array[0..2] of lual_Reg_prop = (
    (name: 'Strings'; funcget: @strings_get; funcset: @strings_set),
    (name: 'Values'; funcget: @strings_valuesget; funcset: @strings_valuesset),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaStringsAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
  luaClassAddArrayProperty(L, MetaTable, UserData, arrprops);
  luaClassAddDefaultArrayProperty(L, MetaTable, UserData, @strings_get, @strings_set);
end;

procedure luaStringsPush(L: Plua_State; Obj: TStrings; Name: String; AutoFree: Boolean);
begin
  luaClassPushObject(L, Obj, Name, AutoFree, @luaStringsAddMetaTable);
end;

procedure luaStringsRegister(L: Plua_State);
begin
  luaClassNewLib(L, PAnsiChar(String(TUserData.ClassName)), constructs);
end;

initialization
  luaClassRegister(TUserData, @luaStringsAddMetaTable, @luaStringsRegister);

end.
