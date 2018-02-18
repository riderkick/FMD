unit LuaUpdateListManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaUpdateListManagerAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  LuaClass, uUpdateThread;

function lua_GetCurrentDirectoryPageNumber(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUpdateListManagerThread(luaClassGetObject(L)).CurrentDirectoryPageNumber);
  Result := 1;
end;

function lua_SetCurrentDirectoryPageNumber(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUpdateListManagerThread(luaClassGetObject(L)).CurrentDirectoryPageNumber := lua_tointeger(L, 1);
end;

procedure luaUpdateListManagerAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  luaClassAddProperty(L, MetaTable, UserData, 'CurrentDirectoryPageNumber', @lua_GetCurrentDirectoryPageNumber, @lua_SetCurrentDirectoryPageNumber);
end;

initialization
  luaClassRegister(TUpdateListManagerThread, @luaUpdateListManagerAddMetaTable);

end.

