unit LuaCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, BaseCrypto;

procedure luaCryptoRegister(L: Plua_State);

implementation

uses
  LuaUtils;

function crypto_hextostr(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HexToStr(luaGetString(L, 1)));
  Result := 1;
end;

function crypto_strtohexstr(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, StrToHexStr(luaGetString(L, 1)));
  Result := 1;
end;

function crypto_md5hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5Hex(luaGetString(L, 1)));
  Result := 1;
end;

function crypto_aesdecryptcbc(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AESDecryptCBC(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3)));
  Result := 1;
end;

procedure luaCryptoRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'HexToStr', @crypto_hextostr);
  luaPushFunctionGlobal(L, 'StrToHexStr', @crypto_strtohexstr);
  luaPushFunctionGlobal(L, 'MD5Hex', @crypto_md5hex);
  luaPushFunctionGlobal(L, 'AESDecryptCBC', @crypto_aesdecryptcbc);
end;

end.

