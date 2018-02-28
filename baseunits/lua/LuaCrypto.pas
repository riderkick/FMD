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
  lua_pushstring(L, HexToStr(lua_tostring(L, 1)));
  Result := 1;
end;

function crypto_strtohexstr(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, StrToHexStr(lua_tostring(L, 1)));
  Result := 1;
end;

function crypto_md5hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5Hex(lua_tostring(L, 1)));
  Result := 1;
end;

function crypto_aesdecryptcbc(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AESDecryptCBC(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3)));
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

