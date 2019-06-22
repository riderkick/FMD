unit LuaImagePuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, ImagePuzzle;

procedure luaImagePuzzlePush(L: Plua_State; Obj: TImagePuzzle; Name: String = '';
  AutoFree: Boolean = False); inline;

implementation

uses LuaClass, LuaUtils;

type
  TUserData = TImagePuzzle;

function imagepuzzle_create(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) = 2 then
    if lua_isinteger(L, 1) and lua_isinteger(L, 2) then
      luaImagePuzzlePush(L, TImagePuzzle.Create(lua_tointeger(L, 1), lua_tointeger(L, 2)), '', True);
  Result := 1;
end;

function imagepuzzle_descramble(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).DeScramble(
    TStream(luaGetUserData(L, 1)),
    TStream(luaGetUserData(L, 2)));
end;

function imagepuzzle_matrixget(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Matrix[lua_tointeger(L, 1)]);
  Result := 1;
end;

function imagepuzzle_matrixset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Matrix[lua_tointeger(L, 1)] := lua_tointeger(L, 2);
end;

function imagepuzzle_gethorblock(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).HorBlock);
  Result := 1;
end;

function imagepuzzle_getverblock(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).VerBlock);
  Result := 1;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @imagepuzzle_create),
    (name: 'Create'; func: @imagepuzzle_create),
    (name: nil; func: nil)
    );
  props: packed array[0..2] of lual_Reg_prop = (
    (name: 'HorBlock'; funcget: @imagepuzzle_gethorblock; funcset: nil),
    (name: 'VerBlock'; funcget: @imagepuzzle_getverblock; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );
  arrprops: packed array[0..1] of luaL_Reg_prop = (
    (name: 'Matrix'; funcget: @imagepuzzle_matrixget; funcset: @imagepuzzle_matrixset),
    (name: nil; funcget: nil; funcset: nil)
    );
  methods: packed array [0..1] of luaL_Reg = (
    (name: 'DeScramble'; func: @imagepuzzle_descramble),
    (name: nil; func: nil)
    );

procedure luaImagePuzzleAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
  luaClassAddArrayProperty(L, MetaTable, UserData, arrprops);
  luaClassAddIntegerProperty(L, MetaTable, 'Multiply', @TUserData(Obj).Multiply);
end;

procedure luaImagePuzzlePush(L: Plua_State; Obj: TImagePuzzle; Name: String;
  AutoFree: Boolean);
begin
  luaClassPushObject(L, Obj, Name, AutoFree, @luaImagePuzzleAddMetaTable);
end;

procedure luaImagePuzzleRegister(L: Plua_State);
begin
  luaClassNewLib(L, 'TImagePuzzle', constructs);
end;

initialization
  luaClassRegister(TUserData, @luaImagePuzzleAddMetaTable, @luaImagePuzzleRegister);

end.

