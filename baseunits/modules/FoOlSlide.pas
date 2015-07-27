unit FoOlSlide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  HTMLUtil, RegExpr;

implementation

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if GetVal(Parse[i], 'class') = 'next' then
        if GetTagName(Parse[i + 2]) = 'a' then
        begin
          Page := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$',
            GetVal(Parse[i + 2], 'href'), '$1', True), 1);
          Break;
        end;
  end;

begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit;
  Parse := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Parse), Module.RootURL + '/directory/', 3) then
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

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'a') and (Pos('/series/', Parse[i]) > 0) then
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
    if MangaInfo.GetPage(TObject(Parse), Module.RootURL + '/directory/' +
      IncStr(URL), 3) then
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
  const Reconnect: Cardinal; Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanChapters(const StartIndex: Integer);
  var
    i: Integer;
    g: String = '';
  begin
    with MangaInfo.mangaInfo do
    begin
      for i := StartIndex to Parse.Count - 1 do
      begin
        if Parse[i] = '</article>' then
          Break;
        if GetVal(Parse[i], 'class') = 'group' then
          if GetVal(Parse[i + 1], 'class') = 'title' then
          begin
            g := Trim(Parse[i + 2]);
            if g = 'Chapters' then
              g := ''
            else
              g += ' ';
          end;
        if GetTagName(Parse[i]) = 'a' then
          if GetVal(Parse[i - 1], 'class') = 'title' then
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
    with MangaInfo.mangaInfo do
      for i := 0 to Parse.Count - 1 do
      begin
        //title
        if title = '' then
          if (GetTagName(Parse[i]) = 'h1') and (GetVal(Parse[i], 'class') = 'title') then
            title := CommonStringFilter(Parse[i + 1]);

        //cover
        if coverLink = '' then
          if GetVal(Parse[i], 'class') = 'thumbnail' then
            if GetTagName(Parse[i + 2]) = 'img' then
              coverLink := GetVal(Parse[i + 2], 'src');

        //author
        if (Parse[i] = 'Author') and (Parse[i + 1] = '</b>') then
          authors := Trim(TrimLeftChar(Parse[i + 2], [':']));

        //artist
        if (Parse[i] = 'Artist') and (Parse[i + 1] = '</b>') then
          artists := Trim(TrimLeftChar(Parse[i + 2], [':']));

        //summary
        if (Parse[i] = 'Synopsis') and (Parse[i + 1] = '</b>') then
          summary := Trim(TrimLeftChar(Parse[i + 2], [':']));

        //chapters
        if GetVal(Parse[i], 'class') = 'list' then
        begin
          ScanChapters(i);
          Break;
        end;
      end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  MangaInfo.mangaInfo.website := Module.Website;
  MangaInfo.mangaInfo.url := FillHost(Module.RootURL, URL);
  Parse := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Parse), MangaInfo.mangaInfo.url, Reconnect) then
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
    i, p: Integer;
  begin
    p := -1;
    for i := 0 to Parse.Count - 1 do
      if (GetVal(Parse[i], 'class') = 'dropdown') and (Pos('style=', Parse[i]) > 0) then
      begin
        p := i + 1;
        Break;
      end;
    if p > -1 then
      for i := p to Parse.Count - 1 do
      begin
        if Pos('</ul>', Parse[i]) <> 0 then
          Break;
        if GetTagName(Parse[i]) = 'a' then
          Inc(Container.PageNumber);
      end;
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
    if DownloadThread.GetPage(TObject(Parse), FillHost(Module.RootURL, URL),
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

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Parse: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'img') and (GetVal(Parse[i], 'class') = 'open') then
      begin
        if DownloadThread.workCounter < Container.PageLinks.Count then
          Container.PageLinks[DownloadThread.workCounter] := GetVal(Parse[i], 'src');
        Break;
      end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Parse := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Parse), FillHost(Module.RootURL, URL) +
      'page/' + IntToStr(DownloadThread.workCounter + 1),
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
    Website := 'Shoujosense';
    RootURL := 'http://reader.shoujosense.com';
    SortedList := False;
    InformationAvailable := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
  end;
end;

initialization
  RegisterModule;

end.
