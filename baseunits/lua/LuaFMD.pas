unit LuaFMD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaFMDRegister(L: Plua_State);

implementation

uses
  LuaClass, LuaUtils, FMDVars, FMDOptions;

procedure luaFMDRegister(L: Plua_State);
var
  t: Integer;
begin
  t := luaNewTable(L);
  luaAddStringToTable(L, t, 'directory', PAnsiChar(FMD_DIRECTORY));
  luaAddStringToTable(L, t, 'exe_Name', PAnsiChar(FMD_EXENAME));
  luaAddStringToTable(L, t, 'version', PAnsiChar(FMD_VERSION_STRING));
  luaAddStringToTable(L, t, 'revision', PAnsiChar(REVISION_NUMBER));
  luaAddStringToTable(L, t, 'lua_directory', PAnsiChar(LUA_REPO_FOLDER));
  lua_setglobal(L, 'fmd');
end;

end.
