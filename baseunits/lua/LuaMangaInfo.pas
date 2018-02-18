unit LuaMangaInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaMangaInfoAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  uBaseUnit, LuaClass;

procedure luaMangaInfoAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TMangaInfo(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'url', @url);
    luaClassAddStringProperty(L, MetaTable, 'title', @title);
    luaClassAddStringProperty(L, MetaTable, 'link', @link);
    luaClassAddStringProperty(L, MetaTable, 'website', @website);
    luaClassAddStringProperty(L, MetaTable, 'coverLink', @coverLink);
    luaClassAddStringProperty(L, MetaTable, 'authors', @authors);
    luaClassAddStringProperty(L, MetaTable, 'artists', @artists);
    luaClassAddStringProperty(L, MetaTable, 'genres', @genres);
    luaClassAddStringProperty(L, MetaTable, 'status', @status);
    luaClassAddStringProperty(L, MetaTable, 'summary', @summary);
    luaClassAddStringProperty(L, MetaTable, 'summary', @summary);
    luaClassAddIntegerProperty(L, MetaTable, 'numChapter', @numChapter);
    luaClassAddObject(L, MetaTable, chapterName, 'chapterNames');
    luaClassAddObject(L, MetaTable, chapterLinks, 'chapterLinks');
  end;
end;

initialization
  luaClassRegister(TMangaInfo, @luaMangaInfoAddMetaTable);

end.

