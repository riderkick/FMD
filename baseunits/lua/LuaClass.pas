unit LuaClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

type
  PObject = ^TObject;

  luaL_Reg_prop = packed record
    name: PAnsiChar;
    funcget: lua_CFunction;
    funcset: lua_CFunction;
  end;
  PluaL_Reg_prop = ^luaL_Reg_prop;

  TluaClassAddMetaTable = procedure(L: Plua_State; Obj: Pointer;
    MetaTable, UserData: Integer; AutoFree: Boolean = False);
  TLuaClassRegisterLib = procedure(L: Plua_State);

procedure luaClassNewLib(L: Plua_State; n: PAnsiChar; lr: PluaL_Reg);

procedure luaClassRegisterAll(L: Plua_State);

procedure luaClassRegister(C: TClass; AddMetaTable: TluaClassAddMetaTable;
  AddLib: TLuaClassRegisterLib = nil);

procedure luaClassNewUserData(L: Plua_State; var MetaTable, UserData: Integer;
  Obj: Pointer; AutoFree: Boolean = False);

function luaClassGetClosure(L: Plua_State): Pointer;
function luaClassGetObject(L: Plua_State): Pointer; inline;

procedure luaClassPushUserData(L: Plua_State; Obj: Pointer; Name: String;
  AutoFree: Boolean; AddMetaTable: TluaClassAddMetaTable);
procedure luaClassPushObject(L: Plua_State; Obj: TObject; Name: String;
  AutoFree: Boolean = False; AddMetaTable: TluaClassAddMetaTable = nil);

procedure luaClassAddFunction(L: Plua_State; MetaTable, UserData: Integer;
  Name: PAnsiChar; Func: lua_CFunction); overload; inline;
procedure luaClassAddFunction(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg); overload;
procedure luaClassAddProperty(L: Plua_State; MetaTable, UserData: Integer;
  Name: PAnsiChar; FuncGet, FuncSet: lua_CFunction); overload;
procedure luaClassAddProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg_prop); overload;
procedure luaClassAddArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  Name: PAnsiChar; FuncGet, FuncSet: lua_CFunction); overload;
procedure luaClassAddArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg_prop); overload;
procedure luaClassAddDefaultArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncGet, FuncSet: lua_CFunction); overload;
procedure luaClassAddDefaultArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg_prop); overload; inline;
procedure luaClassAddStringProperty(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer); overload;
procedure luaClassAddIntegerProperty(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer); overload;
procedure luaClassAddBooleanProperty(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer); overload;
procedure luaClassAddObject(L: Plua_State; MetaTable: Integer; Obj: TObject;
  Name: String; AddMetaTable: TluaClassAddMetaTable = nil);
procedure luaClassAddUserData(L: Plua_State; MetaTable: Integer; Obj: TObject;
  Name: String);

implementation

uses LuaUtils;

type

  { TluaClassList }

  TluaClassList = class
    FClassList, FAddMetaTableList, FClassLibList: TFPList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(C: TClass; AddMetaTable: TluaClassAddMetaTable;
      AddLib: TLuaClassRegisterLib);
    function FindAddMetaTable(C: TClass): TluaClassAddMetaTable;
    property Libs: TFPList read FClassLibList;
  end;

var
  classlist: TluaClassList;

function __index(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  if lua_gettop(L) < 2 then
    Exit;

  lua_getmetatable(L, 1); // 1 should be userdata
  lua_pushstring(L, AnsiLowerCase(luaGetString(L, 2)));  // 2 should be the key
  lua_rawget(L, -2); // get metatable[key]

  if lua_istable(L, -1) then
  begin
    lua_getfield(L, -1, '__get');
    if lua_iscfunction(L, -1) then
      lua_call(L, 0, 1);
  end
  else
  if lua_isnil(L, -1) then
  begin
    lua_pop(L, 1);
    lua_getfield(L, -1, '__defaultget'); // default get[] from metatable
    if lua_iscfunction(L, -1) then
    begin
      lua_pushvalue(L, 2); // key
      lua_call(L, 1, 1);
    end
    else
    if lua_isnil(L, -1) then // no default get
      lua_pop(L, 1);
  end;
  Result := 1;
end;

function __newindex(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) < 2 then
    Exit(0);

  lua_getmetatable(L, 1);
  lua_pushstring(L, AnsiLowerCase(luaGetString(L, 2)));
  lua_rawget(L, -2);

  if lua_istable(L, -1) then
  begin
    lua_getfield(L, -1, '__set');
    if lua_iscfunction(L, -1) then
    begin
      lua_pushvalue(L, 3); // data
      lua_call(L, 1, 0);
    end;
  end
  else
  if lua_isnil(L, -1) then
  begin
    lua_pop(L, 1);
    lua_getfield(L, -1, '__defaultset'); // default get from metatable
    if lua_iscfunction(L, -1) then
    begin
      lua_pushvalue(L, 2); // key
      lua_pushvalue(L, 3); // data
      lua_call(L, 2, 1);
    end
    else
    if lua_isnil(L, -1) then // no default get
      lua_pop(L, 1);
  end;
  Result := 1;
end;

function __indexarray(L: Plua_State): Integer; cdecl;
begin
  Result := 1;

  if luaGetString(L, 2) = '__get' then
  begin
    lua_pushvalue(L, 1);
    Exit;
  end;

  lua_pushvalue(L, lua_upvalueindex(2)); // cfunc
  lua_pushvalue(L, lua_upvalueindex(1)); // userdata
  lua_pushvalue(L, 2);
  lua_call(L, 2, 1);
end;

function __newindexarray(L: Plua_State): Integer; cdecl;
begin
  Result := 1;

  if luaGetString(L, 2) = '__set' then
  begin
    lua_pushvalue(L, 1);
    Exit;
  end;
  lua_pushvalue(L, lua_upvalueindex(2)); // cfunc
  lua_pushvalue(L, lua_upvalueindex(1)); // userdata
  lua_pushvalue(L, 2);
  lua_pushvalue(L, 3);
  lua_call(L, 3, 0);
end;

function __gc(L: Plua_State): Integer; cdecl;
var
  autodestroy: Boolean;
begin
  Result := 0;
  if not lua_isuserdata(L, 1) then
    Exit;

  autodestroy := False;
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, '__autodestroy');
  if lua_isboolean(L, -1) then
    autodestroy := lua_toboolean(L, -1);
  if not autodestroy then
    Exit;

  lua_getfield(L, -2, 'destroy');
  if lua_iscfunction(L, -1) then
  begin
    lua_pushvalue(L, 1);
    lua_call(L, 1, 0);
  end
  else
    try
      PObject(lua_touserdata(L, 1))^.Free;
    except
    end;
end;

function __self(L: Plua_State): Integer; cdecl;
begin
  lua_pushlightuserdata(L, PPointer(luaClassGetObject(L))^);
  Result := 1;
end;

function firstUpcase(s: String): String;
begin
  Result := s;
  if Result <> '' then
    Result[1] := AnsiUpperCase(Result[1])[1];
end;

procedure luaClassNewLib(L: Plua_State; n: PAnsiChar; lr: PluaL_Reg);
var
  t: Integer;
begin
  lua_newtable(L);
  t := lua_gettop(L);
  while lr^.name <> nil do
  begin
    luaAddCFunctionToTable(L, t, PAnsiChar(LowerCase(lr^.name)), lr^.func);
    luaAddCFunctionToTable(L, t, PAnsiChar(firstUpcase(lr^.name)), lr^.func);
    Inc(lr);
  end;
  if n <> '' then
    lua_setglobal(L, n);
end;

procedure luaClassRegisterAll(L: Plua_State);
var
  i: Integer;
begin
  for i := 0 to classlist.Libs.Count - 1 do
    if classlist.Libs[i] <> nil then
      TLuaClassRegisterLib(classlist.Libs[i])(L);
end;

procedure luaClassRegister(C: TClass; AddMetaTable: TluaClassAddMetaTable;
  AddLib: TLuaClassRegisterLib);
begin
  classlist.Add(C, AddMetaTable, AddLib);
end;

procedure luaClassNewUserData(L: Plua_State; var MetaTable, UserData: Integer;
  Obj: Pointer; AutoFree: Boolean);
begin
  luaPushUserData(L, Obj, UserData);
  lua_newtable(L);
  MetaTable := lua_gettop(L);
  luaClassAddFunction(L, MetaTable, UserData, 'self', @__self);
  luaAddCFunctionToTable(L, MetaTable, '__index', @__index);
  luaAddCFunctionToTable(L, MetaTable, '__newindex', @__newindex);
  luaAddCFunctionToTable(L, MetaTable, '__gc', @__gc);
  luaAddBooleanToTable(L, MetaTable, '__autodestroy', AutoFree);
end;

function luaClassGetClosure(L: Plua_State): Pointer;
begin
  Result := nil;
  if lua_isuserdata(L, lua_upvalueindex(1)) then
    Result := lua_touserdata(L, lua_upvalueindex(1))
  else
  if lua_gettop(L) > 0 then
    if lua_isuserdata(L, 1) then
    begin
      Result := lua_touserdata(L, 1);
      lua_remove(L, 1);
    end;
end;

function luaClassGetObject(L: Plua_State): Pointer;
begin
  Result := PPointer(luaClassGetClosure(L))^;
end;

procedure luaClassPushUserData(L: Plua_State; Obj: Pointer; Name: String;
  AutoFree: Boolean; AddMetaTable: TluaClassAddMetaTable);
var
  m, u: Integer;
begin
  if Obj = nil then
    Exit;
  luaClassNewUserData(L, m, u, Obj, AutoFree);
  AddMetaTable(L, Obj, m, u);
  lua_setmetatable(L, u);
  if Name <> '' then
    lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaClassPushObject(L: Plua_State; Obj: TObject; Name: String;
  AutoFree: Boolean; AddMetaTable: TluaClassAddMetaTable);
begin
  if Obj = nil then
    Exit;
  if AddMetaTable = nil then
    AddMetaTable := classlist.FindAddMetaTable(Obj.ClassType);
  if AddMetaTable = nil then
    Exit;
  luaClassPushUserData(L, Obj, Name, AutoFree, AddMetaTable);
end;

procedure luaClassAddFunction(L: Plua_State; MetaTable, UserData: Integer;
  Name: PAnsiChar; Func: lua_CFunction);
begin
  luaAddCClosureToTable(L, MetaTable, UserData, PAnsiChar(AnsiLowerCase(Name)), Func);
end;

procedure luaClassAddFunction(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg);
var
  p: PluaL_Reg;
begin
  p := FuncArr;
  while p^.name <> nil do
  begin
    luaClassAddFunction(L, MetaTable, UserData, p^.name, p^.func);
    Inc(p);
  end;
end;

procedure luaClassAddProperty(L: Plua_State; MetaTable, UserData: Integer;
  Name: PAnsiChar; FuncGet, FuncSet: lua_CFunction);
var
  t: Integer;
begin
  lua_pushstring(L, PAnsiChar(LowerCase(Name)));
  lua_newtable(L);
  t := lua_gettop(L);

  if FuncGet <> nil then
    luaAddCClosureToTable(L, t, UserData, '__get', FuncGet);
  if FuncSet <> nil then
    luaAddCClosureToTable(L, t, UserData, '__set', FuncSet);

  lua_rawset(L, MetaTable);
end;

procedure luaClassAddProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg_prop);
var
  p: PluaL_Reg_prop;
begin
  p := FuncArr;
  while p^.name <> nil do
  begin
    luaClassAddProperty(L, MetaTable, UserData, p^.name, p^.funcget, p^.funcset);
    Inc(p);
  end;
end;

procedure luaClassAddArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  Name: PAnsiChar; FuncGet, FuncSet: lua_CFunction);
var
  t, m: Integer;
begin
  lua_pushstring(L, PAnsiChar(AnsiLowerCase(Name)));
  lua_newtable(L);
  t := lua_gettop(L);

  lua_newtable(L);
  m := lua_gettop(L);

  lua_pushstring(L, '__index');
  lua_pushvalue(L, UserData);
  lua_pushcfunction(L, FuncGet);
  lua_pushcclosure(L, @__indexarray, 2);
  lua_rawset(L, m);

  if FuncSet <> nil then
  begin
    lua_pushstring(L, '__newindex');
    lua_pushvalue(L, UserData);
    lua_pushcfunction(L, FuncSet);
    lua_pushcclosure(L, @__newindexarray, 2);
    lua_rawset(L, m);
  end;

  lua_setmetatable(L, t);
  lua_rawset(L, MetaTable);
end;

procedure luaClassAddArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg_prop);
var
  p: PluaL_Reg_prop;
begin
  p := FuncArr;
  while p^.name <> nil do
  begin
    luaClassAddArrayProperty(L, MetaTable, UserData, p^.name, p^.funcget, p^.funcset);
    Inc(p);
  end;
end;

procedure luaClassAddDefaultArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncGet, FuncSet: lua_CFunction);
begin
  if FuncGet <> nil then
    luaAddCClosureToTable(L, MetaTable, UserData, '__defaultget', FuncGet);
  if FuncSet <> nil then
    luaAddCClosureToTable(L, MetaTable, UserData, '__defaultset', FuncSet);
end;

procedure luaClassAddDefaultArrayProperty(L: Plua_State; MetaTable, UserData: Integer;
  FuncArr: PluaL_Reg_prop);
begin
  luaClassAddDefaultArrayProperty(L, MetaTable, UserData, FuncArr^.funcget,
    FuncArr^.funcset);
end;

function luaclass_string_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, String(luaClassGetClosure(L)^));
  Result := 1;
end;

function luaclass_string_set(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  String(luaClassGetClosure(L)^) := luaGetString(L, -1);
end;

function luaclass_int_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Integer(luaClassGetClosure(L)^));
  Result := 1;
end;

function luaclass_int_set(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Integer(luaClassGetClosure(L)^) := lua_tointeger(L, -1);
end;

function luaclass_bool_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, Boolean(luaClassGetClosure(L)^));
  Result := 1;
end;

function luaclass_bool_set(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Boolean(luaClassGetClosure(L)^) := lua_toboolean(L, -1);
end;

procedure luaClassAddVariable(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer; FuncGet, FuncSet: lua_CFunction);
var
  t: Integer;
begin
  lua_pushstring(L, PAnsiChar(LowerCase(Name)));
  lua_newtable(L);
  t := lua_gettop(L);

  luaAddCClosureToTable(L, t, P, '__get', FuncGet);
  luaAddCClosureToTable(L, t, P, '__set', FuncSet);

  lua_rawset(L, MetaTable);
end;

procedure luaClassAddStringProperty(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer);
begin
  luaClassAddVariable(L, MetaTable, Name, P, @luaclass_string_get, @luaclass_string_set);
end;

procedure luaClassAddIntegerProperty(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer);
begin
  luaClassAddVariable(L, MetaTable, Name, P, @luaclass_int_get, @luaclass_int_set);
end;

procedure luaClassAddBooleanProperty(L: Plua_State; MetaTable: Integer;
  Name: PAnsiChar; P: Pointer);
begin
  luaClassAddVariable(L, MetaTable, Name, P, @luaclass_bool_get, @luaclass_bool_set);
end;

procedure luaClassAddObject(L: Plua_State; MetaTable: Integer; Obj: TObject; Name: String;
  AddMetaTable: TluaClassAddMetaTable);
var
  m, u: Integer;
begin
  if AddMetaTable = nil then
    AddMetaTable := classlist.FindAddMetaTable(Obj.ClassType);
  if AddMetaTable = nil then
    Exit;
  lua_pushstring(L, PAnsiChar(AnsiLowerCase(Name)));
  luaClassNewUserData(L, m, u, Obj, False);
  AddMetaTable(L, Obj, m, u);
  lua_setmetatable(L, u);
  lua_rawset(L, MetaTable);
end;

procedure luaClassAddUserData(L: Plua_State; MetaTable: Integer; Obj: TObject;
  Name: String);
begin
  lua_pushstring(L, PAnsiChar(AnsiLowerCase(Name)));
  luaPushUserData(L, Obj);
  lua_rawset(L, MetaTable);
end;

{ TluaClassList }

constructor TluaClassList.Create;
begin
  FClassList := TFPList.Create;
  FAddMetaTableList := TFPList.Create;
  FClassLibList := TFPList.Create;
end;

destructor TluaClassList.Destroy;
begin
  FClassList.Free;
  FAddMetaTableList.Free;
  FClassLibList.Free;
  inherited Destroy;
end;

procedure TluaClassList.Add(C: TClass; AddMetaTable: TluaClassAddMetaTable;
  AddLib: TLuaClassRegisterLib);
begin
  FClassList.Add(C);
  FAddMetaTableList.Add(AddMetaTable);
  FClassLibList.Add(AddLib);
end;

function TluaClassList.FindAddMetaTable(C: TClass): TluaClassAddMetaTable;
var
  i, p: Integer;
begin
  Result := nil;
  p := FClassList.IndexOf(C);
  if p = -1 then
    for i := 0 to FClassList.Count - 1 do
      if C.InheritsFrom(TClass(FClassList[i])) then
      begin
        p := i;
        Break;
      end;
  if p <> -1 then
    Result := TluaClassAddMetaTable(FAddMetaTableList[p]);
end;

initialization
  classlist := TluaClassList.Create;

finalization
  classlist.Free;

end.
