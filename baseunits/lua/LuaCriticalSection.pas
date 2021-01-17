unit LuaCriticalSection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaCriticalSectionAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  syncobjs, LuaClass;

type
  TUserData = TCriticalSection;

function lua_tryenter(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).TryEnter);
  Result := 1;
end;

function lua_enter(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Enter;
end;

function lua_Leave(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Leave;
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'TryEnter'; func: @lua_tryenter),
    (name: 'Enter'; func: @lua_enter),
    (name: 'Leave'; func: @lua_Leave),
    (name: nil; func: nil)
    );

procedure luaCriticalSectionAddMetaTable(L: Plua_State; Obj: Pointer; MetaTable,
  UserData: Integer; AutoFree: Boolean = False);
begin
  with TUserData(Obj) do
  begin
    luaClassAddFunction(L, MetaTable, UserData, methods);
  end;
end;

initialization
  luaClassRegister(TCriticalSection, @luaCriticalSectionAddMetaTable);

end.
