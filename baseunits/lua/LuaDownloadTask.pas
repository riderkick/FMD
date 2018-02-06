unit LuaDownloadTask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

implementation

uses
  LuaClass, uDownloadsManager;

procedure luaDownloadTaskMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TTaskContainer(Obj) do
  begin
    luaClassAddObject(L, PageLinks, MetaTable, 'PageLinks');
    luaClassAddObject(L, PageContainerLinks, MetaTable, 'PageContainerLinks');
    luaClassAddIntegerProperty(L, MetaTable, 'PageNumber', @PageNumber);
  end;
end;

initialization
  luaClassRegister(TTaskContainer, @luaDownloadTaskMetaTable);

end.

