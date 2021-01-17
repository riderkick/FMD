unit LuaDuktape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, uBaseUnit;

procedure luaDuktapeRegister(L: Plua_State);

implementation

uses JSUtils, LuaUtils;

function lua_execjs(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ExecJS(luaGetString(L, 1)));
  Result := 1;
end;

procedure luaDuktapeRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'ExecJS', @lua_execjs);
end;

end.

