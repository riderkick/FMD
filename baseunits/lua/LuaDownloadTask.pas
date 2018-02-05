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
    luaClassAddObject(L, ChapterName, MetaTable, 'chapterName');
    luaClassAddObject(L, ChapterLinks, MetaTable, 'chapterLinks');
    luaClassAddObject(L, PageContainerLinks, MetaTable, 'chapterLinks');
  end;
end;

initialization
  luaClassRegister(TTaskContainer, @luaDownloadTaskMetaTable);

end.

