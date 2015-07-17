unit FoOlSlide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, USimpleLogger, HTMLUtil,
  RegExpr;

implementation

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Cardinal; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;
  i: Integer;
begin
  Result := INFORMATION_NOT_FOUND;
  Page := 1;
  if MangaInfo = nil then
    Exit;
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), Module.RootURL + '/directory/', 3) then
    begin
      Result := NO_ERROR;
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        if Parse.Count > 0 then
        begin
          for i := 0 to Parse.Count - 1 do
          begin
            if GetVal(Parse[i], 'class') = 'next' then
              if GetTagName(Parse[i + 2]) = 'a' then
              begin
                Page := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$',
                  GetVal(Parse[i + 2], 'href'), '$1', True), 1);
                Break;
              end;
          end;
        end;
      finally
        Parse.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;
  i: Integer;
begin
  Result := INFORMATION_NOT_FOUND;
  if MangaInfo = nil then
    Exit;
  if MangaInfo = nil then
    Exit;
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), Module.RootURL + '/directory/' +
      IncStr(URL), 3) then
    begin
      Result := NO_ERROR;
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        if Parse.Count > 0 then
        begin
          for i := 0 to Parse.Count - 1 do
          begin
            if (GetTagName(Parse[i]) = 'a') and (Pos('/series/', Parse[i]) > 0) then
            begin
              Links.Add(GetVal(Parse[i], 'href'));
              Names.Add(CommonStringFilter(Parse[i + 1]));
            end;
          end;
        end;
      finally
        Parse.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Shoujosense';
    RootURL := 'http://reader.shoujosense.com';
    SortedList := False;
    InformationAvailable := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
  end;
end;

initialization
  RegisterModule;

end.
