unit LuaMemoryStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaMemoryStreamAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  uBaseUnit, LuaClass, LuaUtils;

type
  TUserData = TMemoryStream;

function mem_toString(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, StreamToString(TUserData(luaClassGetObject(L))));
  Result := 1;
end;

function mem_readAnsiString(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).ReadAnsiString);
  Result := 1;
end;

function mem_writeAnsiString(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).WriteAnsiString(luaGetString(L, 1));
end;

function mem_loadFromFile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).LoadFromFile(luaGetString(L, 1));
end;

function mem_saveToFile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SaveToFile(luaGetString(L, 1));
end;

function mem_getSize(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Size);
  Result := 1;
end;

function mem_setSize(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Size := lua_tointeger(L, 1);
end;

const
  methods: packed array [0..5] of luaL_Reg = (
    (name: 'ToString'; func: @mem_tostring),
    (name: 'ReadAnsiString'; func: @mem_readAnsiString),
    (name: 'WriteAnsiString'; func: @mem_writeAnsiString),
    (name: 'LoadFromFile'; func: @mem_loadFromFile),
    (name: 'SaveToFile'; func: @mem_saveToFile),
    (name: nil; func: nil)
    );
  props: packed array[0..1] of luaL_Reg_prop = (
    (name: 'Size'; funcget: @mem_getSize; funcset: @mem_setSize),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaMemoryStreamAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean);
begin
  with TUserData(Obj) do
  begin
    luaClassAddFunction(L, MetaTable, UserData, methods);
    luaClassAddProperty(L, MetaTable, UserData, props);
  end;
end;

initialization
  luaClassRegister(TMemoryStream, @luaMemoryStreamAddMetaTable);

end.
