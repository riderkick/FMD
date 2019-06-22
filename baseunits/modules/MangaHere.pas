unit MangaHere;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules;

implementation

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaHere';
    RootURL := 'http://www.mangahere.cc';
    Category := 'English';
  end;
end;

initialization
  RegisterModule;

end.
