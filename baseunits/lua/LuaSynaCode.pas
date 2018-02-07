unit LuaSynaCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, synacode;

procedure luaSynaCodeRegister(L: Plua_State);

implementation

uses
  LuaUtils;

function lua_decodeurl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecodeURL(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_encodeurl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeURL(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_decodeuu(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecodeUU(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_encodeuu(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeUU(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_encodeurlelement(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeURLElement(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_decodebase64(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecodeBase64(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_encodebase64(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeBase64(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_crc16(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Crc16(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_crc32(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Crc32(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_md4(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD4(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_md5(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_hmac_md5(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HMAC_MD5(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function lua_md5longhash(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5LongHash(lua_tostring(L, 1), lua_tointeger(L, 2)));
  Result := 1;
end;

function lua_sha1(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SHA1(lua_tostring(L, 1)));
  Result := 1;
end;

function lua_hmac_sha1(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HMAC_SHA1(lua_tostring(L, 1), lua_tostring(L, 2)));
  Result := 1;
end;

function lua_sha1longhash(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SHA1LongHash(lua_tostring(L, 1), lua_tointeger(L, 2)));
  Result := 1;
end;

procedure luaSynaCodeRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'DecodeURL', @lua_decodeurl);
  luaPushFunctionGlobal(L, 'EncodeURL', @lua_encodeurl);
  luaPushFunctionGlobal(L, 'DecodeUU', @lua_decodeuu);
  luaPushFunctionGlobal(L, 'EncodeUU', @lua_encodeuu);
  luaPushFunctionGlobal(L, 'EncodeURLElement', @lua_encodeurlelement);
  luaPushFunctionGlobal(L, 'DecodeBase64', @lua_decodebase64);
  luaPushFunctionGlobal(L, 'EncodeBase64', @lua_encodebase64);
  luaPushFunctionGlobal(L, 'CRC16', @lua_crc16);
  luaPushFunctionGlobal(L, 'CRC32', @lua_crc32);
  luaPushFunctionGlobal(L, 'MD4', @lua_md4);
  luaPushFunctionGlobal(L, 'MD5', @lua_md5);
  luaPushFunctionGlobal(L, 'HMAC_MD5', @lua_hmac_md5);
  luaPushFunctionGlobal(L, 'MD5LongHash', @lua_md5longhash);
  luaPushFunctionGlobal(L, 'SHA1', @lua_sha1);
  luaPushFunctionGlobal(L, 'HMAC_SHA1', @lua_hmac_sha1);
  luaPushFunctionGlobal(L, 'SHA1LongHash', @lua_sha1longhash);
end;

end.

