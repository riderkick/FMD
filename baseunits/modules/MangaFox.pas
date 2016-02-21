unit MangaFox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  mangafoxwatermarkremover, HTMLUtil, RegExpr;

implementation

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
      if (GetTagName(Parse[i]) = 'a') and
        (Pos('series_preview manga_', Parse[i]) > 0) then
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
    if MangaInfo.GetPage(TObject(Parse), Module.RootURL + '/manga/', 3) then
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
      if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'tips') then
      begin
        Inc(info.numChapter);
        s := Trim(parse[i + 1]) + ' ' + Trim(parse[i + 5]);
        info.chapterName.Add(CommonStringFilter(s));
        s := GetVal(parse[i], 'href');
        if RightStr(s, 6) = '1.html' then
          SetLength(s, Length(s) - 6);
        info.chapterLinks.Add(s);
      end;
    end;

    //invert chapters
    if info.chapterLinks.Count > 0 then
      InvertStrings([info.chapterLinks, info.chapterName]);
  end;

  procedure ScanParse;
  var
    i, j: Integer;
  begin
    info.authors := '';
    info.artists := '';
    info.genres := '';
    info.summary := '';
    info.status := '';
    for i := 0 to Parse.Count - 1 do
    begin
      //title
      if info.title = '' then
        if GetVal(parse[i], 'id') = 'title' then
          info.title := CommonStringFilter(Parse[i + 3]);

      //cover
      if info.coverLink = '' then
        if (GetTagName(parse[i]) = 'div') and
          (GetVal(parse[i], 'class') = 'cover') then
          info.coverLink := CorrectURL(GetVal(parse[i + 2], 'src'));

      //author
      if info.authors = '' then
        if Pos('/search/author/', parse[i]) > 0 then
          info.authors := Trim(parse[i + 1]);

      //artist
      if info.artists = '' then
        if Pos('/search/artist/', parse[i]) > 0 then
          info.artists := Trim(parse[i + 1]);

      //genres
      if info.genres = '' then
        if (GetTagName(parse[i]) = 'td') and (GetVal(parse[i], 'valign') = 'top') then
          if Pos('/genres/', parse[i + 2]) > 0 then
          begin

            for j := i + 1 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = '/td' then
                Break
              else
              if Pos('<', parse[j]) = 0 then
                info.genres := info.genres + parse[j];
            end;
          end;

      //summary
      if info.summary = '' then
        if (GetTagName(parse[i]) = 'p') and (GetVal(parse[i], 'class') = 'summary') then
        begin
          for j := i + 1 to parse.Count - 1 do
          begin
            if GetTagName(parse[j]) = '/p' then
              Break
            else
            if Pos('<', parse[j]) = 0 then
              info.summary := info.summary + #13#10 + CommonStringFilter(parse[j]);
          end;
        end;

      //status
      if info.status = '' then
        if GetTagName(parse[i]) = 'h5' then
          if UpperCase(Trim(parse[i + 1])) = 'STATUS:' then
          begin
            if Pos('ONGOING', UpperCase(parse[i + 5])) > 0 then
              info.status := '1'   // ongoing
            else
              info.status := '0';  // completed
          end;

      //check if it's licensed
      if GetVal(parse[i], 'class') = 'warning' then
        if Pos('it is not available in', parse[i + 1]) > 0 then
        begin
          info.numChapter := 0;
          info.chapterName.Clear;
          info.chapterLinks.Clear;
          info.summary := Trim(Parse[i + 1]);
          Break;
        end;

      //chapters
      if GetVal(Parse[i], 'id') = 'chapters' then
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
    i, j: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'select') and
        (GetVal(Parse[i], 'onchange') = 'change_page(this)') then
      begin
        for j := i + 1 to Parse.Count - 1 do
        begin
          if GetTagName(Parse[j]) = '/select' then
            Break
          else
          if (GetTagName(Parse[j]) = 'option') and
            (GetVal(Parse[j], 'value') <> '0') then
            Inc(Container.PageNumber);
        end;
        Break;
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
    if DownloadThread.GetPage(TObject(Parse),
      AppendURLDelim(FillHost(Module.RootURL, AURL)) + '1.html',
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

function GetImageURL(const DownloadThread: TDownloadThread;
  const AURL: String; const Module: TModuleContainer): Boolean;
var
  Parse: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'img') and (GetVal(Parse[i], 'id') = 'image') then
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
    if DownloadThread.GetPage(TObject(Parse),
      AppendURLDelim(FillHost(Module.RootURL, AURL)) +
      IncStr(DownloadThread.workCounter) + '.html',
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

function AfterImageSaved(const AFilename: String; const Module: TModuleContainer): Boolean;
begin
  Result := True;
  if OptionMangaFoxRemoveWatermark then
    Result := mangafoxwatermarkremover.RemoveWatermark(AFilename, OptionMangaFoxSaveAsPNG);
end;

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'MangaFox';
    RootURL := 'http://mangafox.me';
    SortedList := False;
    InformationAvailable := True;
    OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
    OnGetNameAndLink := @GetNameAndLink;
    OnGetInfo := @GetInfo;
    OnGetPageNumber := @GetPageNumber;
    OnGetImageURL := @GetImageURL;
    OnAfterImageSaved := @AfterImageSaved;
  end;
end;

initialization
  RegisterModule;

end.
