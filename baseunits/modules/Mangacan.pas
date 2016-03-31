unit Mangacan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  httpsendthread, HTMLUtil, synautil;

implementation

const
  dirURL = '/daftar-komik-manga-bahasa-indonesia.html';

function GetDirectoryPageNumber(const MangaInfo: TMangaInformation;
  var Page: Integer; const Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := 1;
end;

function GetNameAndLink(const MangaInfo: TMangaInformation;
  const ANames, ALinks: TStringList; const AURL: String;
  const Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'a') and (Pos('baca-komik-', Parse[i]) <> 0) then
      begin
        ALinks.Add(GetVal(Parse[i], 'href'));
        ANames.Add(CommonStringFilter(Parse[i + 1]));
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

function GetInfo(const MangaInfo: TMangaInformation;
  const AURL: String; const Module: TModuleContainer): Integer;
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
  info.url := FillHost(Module.RootURL, AURL);
  Parse := TStringList.Create;
  try
    if MangaInfo.FHTTP.GET(info.url, TObject(Parse)) then
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

function GetPageNumber(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
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
      FillHost(Module.RootURL, AURL),
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
