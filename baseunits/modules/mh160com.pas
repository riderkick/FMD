unit mh160com;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  HTMLUtil, base64, RegExpr;

implementation

const
  diralpha = 'abcdefghijklmnopqrstuvwxyz';

function GetDirectoryPageNumber(var {%H-}MangaInfo: TMangaInformation;
  var Page: Integer; {%H-}Module: TModuleContainer): Integer;
begin
  Result := NO_ERROR;
  Page := Length(diralpha);
end;

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
    begin
      if Parse[i] = '<dt>' then
        if GetTagName(Parse[i + 1]) = 'a' then
        begin
          Links.Add(GetVal(Parse[i + 1], 'href'));
          Names.Add(CommonStringFilter(Parse[i + 2]));
        end;
    end;
  end;

begin
  Result := INFORMATION_NOT_FOUND;
  if MangaInfo = nil then
    Exit;
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), Module.RootURL + '/kanmanhua/' +
      diralpha[StrToInt(URL) + 1] + '.html', 3) then
    begin
      Result := NO_ERROR;
      Source.Text := ConvertCharsetToUTF8(Source.Text);
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        if Parse.Count > 0 then
          ScanParse;
      finally
        Parse.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetInfo(var MangaInfo: TMangaInformation; const URL: String;
  const Reconnect: Cardinal; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;

  procedure ScanChapters(const StartIndex: Integer);
  var
    i: Integer;
    g: String = '';
  begin
    with MangaInfo.mangaInfo do
    begin
      for i := StartIndex to Parse.Count - 1 do
      begin
        if Parse[i] = '</div>' then
          Break;
        if GetTagName(Parse[i]) = 'a' then
        begin
          chapterLinks.Add(GetVal(Parse[i], 'href'));
          chapterName.Add(g + Trim(Parse[i + 1]));
        end;
      end;
      //invert chapters
      if MangaInfo.mangaInfo.chapterLinks.Count > 0 then
        InvertStrings([chapterLinks, chapterName]);
    end;
  end;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      with MangaInfo.mangaInfo do
      begin
        //title
        if title = '' then
          if GetVal(Parse[i], 'class') = 'intro_l' then
            if GetVal(Parse[i + 2], 'class') = 'title' then
              title := CommonStringFilter(Parse[i + 5]);

        //cover
        if coverLink = '' then
          if GetVal(Parse[i], 'class') = 'cover' then
            if GetTagName(Parse[i + 1]) = 'img' then
            begin
              coverLink := GetVal(Parse[i + 1], 'src');
              if Pos('http', coverLink) <> 1 then
                coverLink := Module.RootURL + coverLink;
            end;

        //author
        if (Pos('原著作者：', Parse[i]) <> 0) and
          (Parse[i + 1] = '</em>') then
          authors := CommonStringFilter(Parse[i + 2]);

        //genres
        if (Pos('剧情类别：', Parse[i]) <> 0) and
          (Parse[i + 1] = '</em>') then
          genres := Trim(Parse[i + 3]);

        //summary
        if Pos('class="introduction"', Parse[i]) <> 0 then
          summary := Trim(Parse[i + 2]);

        //chapters
        if GetVal(Parse[i], 'class') = 'plist pnormal' then
          ScanChapters(i);
      end;
  end;

begin
  Result := INFORMATION_NOT_FOUND;
  if MangaInfo = nil then
    Exit;
  MangaInfo.mangaInfo.website := Module.Website;
  MangaInfo.mangaInfo.url := FillHost(Module.RootURL, URL);
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), MangaInfo.mangaInfo.url, Reconnect) then
    begin
      Result := NO_ERROR;
      Source.Text := ConvertCharsetToUTF8(Source.Text);
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        if Parse.Count > 0 then
          ScanParse;
      finally
        Parse.Free;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function GetPageNumber(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    s: String;
  begin
    s := ReplaceRegExpr('(?ig)^.*var\spicTree\s*=[''"](.+?)[''"].*$',
      Source.Text, '$1', True);
    if s <> '' then
    begin
      s := DecodeStringBase64(s);
      s := StringReplace(s, '$qingtiandy$', #13#10, [rfReplaceAll]);
      Container.PageLinks.AddText(s);
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then
    Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Source), FillHost(Module.RootURL, URL),
      Container.Manager.retryConnect) then
    begin
      Result := True;
      ScanParse;
    end;
  finally
    Source.Free;
  end;
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'mh160';
    RootURL := 'http://www.mh160.com';
    SortedList := False;
    InformationAvailable := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
  end;
end;

initialization
  RegisterModule;

end.
