unit Mangacan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  HTMLUtil, synautil;

implementation

const
  dirURL = '/daftar-komik-manga-bahasa-indonesia.html';

function GetDirectoryPageNumber(var {%H-}MangaInfo: TMangaInformation;
  var Page: Integer; {%H-}Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := 1;
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const {%H-}URL: String;
  Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'a') and (Pos('baca-komik-', Parse[i]) <> 0) then
      begin
        Links.Add(GetVal(Parse[i], 'href'));
        Names.Add(CommonStringFilter(Parse[i + 1]));
      end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  Parse := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Parse), Module.RootURL + dirURL, 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      ParseHTML(Parse.Text, Parse);
      if Parse.Count > 0 then
      begin
        Result := NO_ERROR;
        ScanParse;
      end;
    end;
  finally
    Parse.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Integer; Module: TModuleContainer): Integer;
var
  Parse: TStringList;
  info: TMangaInfo;

  procedure ScanChapters(const StartIndex: Integer);
  var
    i: Integer;
    s: String = '';
  begin
    for i := StartIndex to Parse.Count - 1 do
    begin
      if GetTagName(Parse[i]) = '/table' then
        Break
      else
      if GetTagName(parse[i]) = 'a' then
      begin
        s := GetVal(Parse[i], 'href');
        if (Length(s) > 7) and (RightStr(s, 7) = '-1.html') then
          s := SeparateLeft(s, '-1.html') + '.html';
        info.chapterLinks.Add(s);
        info.chapterName.Add(CommonStringFilter(Parse[i + 1]));
      end;
    end;

    //invert chapters
    if info.chapterLinks.Count > 0 then
      InvertStrings([info.chapterLinks, info.chapterName]);
  end;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
    begin
      //title
      if info.title = '' then
        if Pos(' Indonesia|Baca ', Parse[i]) <> 0 then
          info.title := CommonStringFilter(SeparateLeft(Parse[i], ' Indonesia|Baca '));

      //chapters
      if (GetTagName(Parse[i]) = 'table') and
        (GetVal(Parse[i], 'class') = 'updates') then
      begin
        ScanChapters(i);
        Break;
      end;
    end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := FillHost(Module.RootURL, URL);
  Parse := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Parse), info.url, Reconnect) then
    begin
      Result := INFORMATION_NOT_FOUND;
      ParseHTML(Parse.Text, Parse);
      if Parse.Count > 0 then
      begin
        Result := NO_ERROR;
        ScanParse;
      end;
    end;
  finally
    Parse.Free;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Parse: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'img') and
        (GetVal(Parse[i], 'class') = 'picture') then
        Container.PageLinks.Add(MaybeFillHost(Module.RootURL, GetVal(Parse[i], 'src')));
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Parse := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Parse),
      FillHost(Module.RootURL, URL),
      Container.Manager.retryConnect) then
    begin
      ParseHTML(Parse.Text, Parse);
      if Parse.Count > 0 then
      begin
        Result := True;
        ScanParse;
      end;
    end;
  finally
    Parse.Free;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Mangacan';
    RootURL := 'http://mangacanblog.com';
    SortedList := False;
    InformationAvailable := False;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
