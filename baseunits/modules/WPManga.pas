unit WPManga;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  HTMLUtil, RegExpr, strutils;

implementation

const
  dirURL = '/manga-list/all/any/last-added/';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := Parse.Count - 1 downto 0 do
      if Pos(dirURL, Parse[i]) <> 0 then
      begin
        Page := StrToIntDef(ReplaceRegExpr('^.*' + dirURL + '(\d+)/$',
          GetVal(Parse[i], 'href'), '$1', True), 1);
        Break;
      end;
  end;

begin
  Result := NET_PROBLEM;
  Page := 1;
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

function GetNameAndLink(var MangaInfo: TMangaInformation;
  const Names, Links: TStringList; const URL: String; Module: TModuleContainer): Integer;
var
  Parse: TStringList;

  procedure ScanNamesEyeOnManga;
  var
    i: Integer;
  begin
    for i := 0 to parse.Count - 1 do
      if GetVal(parse[i], 'class') = 'cvr' then
        try
          if GetTagName(parse[i - 4]) = 'a' then
          begin
            names.Add(CommonStringFilter(parse[i - 3]));
            links.Add(GetVal(parse[i - 4], 'href'));
          end;
        except
        end;
  end;

  procedure ScanNamesWPManga;
  var
    i, j, k: Integer;
    s: String;
  begin
    for i := 0 to parse.Count - 1 do
    begin
      //thumbnail mode
      if GetVal(parse[i], 'id') = 'sct_content' then
      begin
        for j := i + 1 to parse.Count - 1 do
        begin
          s := GetTagName(parse[j]);
          if (s = 'sct_sidebar') or (s = 'sct_wid_bot') then
            Break
          else
          if GetVal(parse[j], 'class') = 'det' then
          begin
            for k := j + 1 to parse.Count - 1 do
            begin
              if GetTagName(parse[k]) = 'a' then
              begin
                links.Add(GetVal(parse[k], 'href'));
                names.Add(CommonStringFilter(parse[k + 1]));
                Break;
              end;
            end;
          end;
        end;
        Break;
      end;
    end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  Parse := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Parse), Module.RootURL + dirURL +
      IncStr(URL) + '/', 3) then
    begin
      Result := INFORMATION_NOT_FOUND;
      ParseHTML(Parse.Text, Parse);
      if Parse.Count > 0 then
      begin
        Result := NO_ERROR;
        if Module.Website = 'EyeOnManga' then
          ScanNamesEyeOnManga
        else
          ScanNamesWPManga;
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
  info: TMangaInfo;
  isInvertChapters: Boolean;

  procedure ScanChapters;
  var
    i, j: Integer;
    s: String;
    StartFindPage: Boolean;
  begin
    StartFindPage := False;
    for i := 0 to parse.Count - 1 do
    begin
      if not StartFindPage then
      begin
        s := LowerCase(GetVal(parse[i], 'class'));
        if (GetTagName(parse[i]) = 'ul') and
          (AnsiIndexText(s, ['lst', 'chp_lst']) > -1) then
        begin
          StartFindPage := True;
          if s = 'chp_lst' then
            isInvertChapters := False;
        end;
      end
      else
      begin
        if GetTagName(parse[i]) = '/ul' then
          Break
        else
        if GetTagName(parse[i]) = 'a' then
        begin
          if Pos('class="c4', parse[i]) = 0 then
          begin
            s := '';
            for j := i + 1 to parse.Count - 1 do
            begin
              s := Trim(parse[j]);
              if (s <> '') and (Pos('<', s) <> 1) then
                Break;
            end;
            info.chapterLinks.Add(GetVal(parse[i], 'href'));
            info.chapterName.Add(CommonStringFilter(s));
          end;
        end;
      end;
    end;
  end;

  procedure ScanParse;
  var
    i, j, pnumber: Integer;
    s: String;
    isInfo: Boolean;
    isSummaryDone: Boolean;
  begin
    info.genres := '';
    info.summary := '';
    isInvertChapters := True;
    isInfo := False;
    isSummaryDone := False;
    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if info.coverLink = '' then
      begin
        if (Pos('<img ', parse[i]) <> 0) and
          (Pos('class="cvr', parse[i]) <> 0) then
          info.coverLink := GetVal(parse[i], 'src');
      end;

      //title
      if info.title = '' then
      begin
        if (Pos('class="ttl"', parse[i]) <> 0) or
          (Pos('itemprop="itemreviewed"', parse[i]) <> 0) then
          info.title := CommonStringFilter(parse[i + 1]);
      end;

      //details
      s := GetVal(parse[i], 'class');
      if (GetTagName(parse[i]) = 'div') and
        (AnsiIndexText(s, ['det', 'lts_chp fr', 'mng_ifo']) > -1) then
        isInfo := True;
      if isInfo then
      begin
        if GetTagName(parse[i]) = 'h2' then
          isInfo := False
        else
        begin
          s := Trim(parse[i]);
          //author
          if s = 'Author' then
            info.authors := CommonStringFilter(
              TrimLeftChar(parse[i + 2], [':', ' ']))
          else
          if Pos('/author/', s) <> 0 then
            info.authors := CommonStringFilter(parse[i + 1])
          //artist
          else
          if s = 'Artist' then
            info.artists := CommonStringFilter(
              TrimLeftChar(parse[i + 2], [':', ' ']))
          //status
          else
          if s = 'Status' then
          begin
            if Pos('completed', LowerCase(parse[i + 2])) <> 0 then
              info.status := '0'
            else
              info.status := '1';
          end
          //genres
          else
          if (s = 'Category') or (s = 'Genres') then
          begin
            for j := i + 3 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = '/p' then
                Break
              else
              if Pos('<', parse[j]) = 0 then
                AddCommaString(info.genres, CommonStringFilter(parse[j]));
            end;
          end
          else
          if s = 'Type' then
            AddCommaString(info.genres,
              CommonStringFilter(TrimLeftChar(parse[i + 2], [':', ' '])))
          else
          //summary
          if (not isSummaryDone) and (s = 'Summary') then
          begin
            isSummaryDone := True;
            for j := i + 2 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = '/p' then
                Break
              else
              if Pos('<', parse[j]) = 0 then
                info.summary := Trim(info.summary + LineEnding +
                  CommonStringFilter(parse[j]));
            end;
          end;
          //summary
          if (not isSummaryDone) and (GetTagName(parse[i]) = 'p') then
          begin
            s := Trim(parse[i + 1]);
            if (s <> '') and (Pos('<', s) = 0) then
            begin
              isSummaryDone := True;
              for j := i + 1 to parse.Count - 1 do
              begin
                if GetTagName(parse[j]) = '/p' then
                  Break
                else
                if Pos('<', parse[j]) = 0 then
                  info.summary := Trim(info.summary + LineEnding +
                    CommonStringFilter(parse[j]));
              end;
            end;
          end;
        end;
      end;
    end;

    //chapters
    ScanChapters;
    pnumber := 1;
    for i := parse.Count - 1 downto 0 do
    begin
      if (Pos('<a', parse[i]) <> 0) and
        (Pos('/chapter-list/', parse[i]) <> 0) then
      begin
        s := GetVal(parse[i], 'href');
        pnumber := StrToIntDef(
          ReplaceRegExpr('^.*/chapter-list/(\d+)/$', s, '$1', True), 1);
        Break;
      end;
    end;
    if pnumber > 1 then
    begin
      for i := 2 to pnumber do
      begin
        if MangaInfo.GetPage(TObject(Parse),
          info.url + 'chapter-list/' + IntToStr(i) + '/', Reconnect) then
        begin
          ParseHTML(Parse.Text, parse);
          if Parse.Count > 0 then
            ScanChapters;
        end;
      end;
    end;

    //invert chapters
    if isInvertChapters then
      InvertStrings([info.chapterName, info.chapterLinks]);
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := FillHost(Module.RootURL, URL);
  Parse := TStringList.Create;
  try
    if Length(info.url) > 0 then
      if RightStr(info.url, 1) <> '/' then
        info.url += '/';
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
  s: String;

  procedure ScanParse;
  var
    i, j: Integer;
    s: String;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(parse[i]) = 'select') and
        (GetVal(parse[i], 'class') = 'cbo_wpm_pag') then
      begin
        for j := i + 1 to parse.Count - 1 do
        begin
          s := GetTagName(parse[j]);
          if s = '/select' then
            Break
          else
          if s = 'option' then
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
    s := FillHost(Module.RootURL, URL);
    if Length(s) > 0 then
      if RightStr(s, 1) <> '/' then
        s += '/';
    s += '1/';
    if DownloadThread.GetPage(TObject(Parse), s, Container.Manager.retryConnect) then
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
  s: String;

  procedure ScanParse;
  var
    i, j: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetVal(Parse[i], 'class') = 'wpm_pag mng_rdr') then
      begin
        for j := i + 1 to Parse.Count - 1 do
          if GetTagName(Parse[j]) = 'img' then
          begin
            Container.PageLinks[DownloadThread.WorkCounter] := GetVal(Parse[j], 'src');
            Break;
          end;
        Break;
      end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Parse := TStringList.Create;
  try
    s := FillHost(Module.RootURL, URL);
    if Length(s) > 0 then
      if RightStr(s, 1) <> '/' then
        s += '/';
    s += IntToStr(DownloadThread.workCounter + 1) + '/';
    if DownloadThread.GetPage(TObject(Parse), s, Container.Manager.retryConnect) then
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

  procedure AddWebsiteModule(AWebsite, ARootURL: String);
  begin
    with AddModule do
    begin
      Website := AWebsite;
      RootURL := ARootURL;
      SortedList := True;
      InformationAvailable := True;
      OnGetDirectoryPageNumber := @GetDirectoryPageNumber;
      OnGetNameAndLink := @GetNameAndLink;
      OnGetInfo := @GetInfo;
      OnGetPageNumber := @GetPageNumber;
      OnGetImageURL := @GetImageURL;
    end;
  end;

begin
  AddWebsiteModule('MangaCap', 'http://www.mangacap.com');
  AddWebsiteModule('MangaBoom', 'http://www.mangaboom.com');
  AddWebsiteModule('Authrone', 'http://www.authrone.com');
  AddWebsiteModule('EyeOnManga', 'http://www.eyeonmanga.com');
end;

initialization
  RegisterModule;

end.
