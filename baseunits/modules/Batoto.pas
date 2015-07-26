unit Batoto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  HTMLUtil, RegExpr;

implementation

const
  dirurls: array [0..1] of String = (
    '/comic/_/sp/',
    '/comic/_/comics/');
  perpage = 50;
  dirparam = '?sort_col=record_saved&sort_order=desc&per_page=';

function GetDirectoryPageNumber(var MangaInfo: TMangaInformation;
  var Page: Integer; Module: TModuleContainer): Integer;
var
  Source, Parse: TStringList;

  procedure ScanParse;
  var
    i: Integer;
    s: String;
  begin
    if Parse.Count > 0 then
      for i := 0 to Parse.Count - 1 do
        if (Pos('Page 1 of ', parse.Strings[i]) > 0) then
        begin
          s := GetString(parse.Strings[i] + '~!@', 'Page 1 of ', '~!@');
          Page := StrToInt(TrimLeft(TrimRight(s)));
          Break;
        end;
  end;

begin
  Result := NET_PROBLEM;
  Page := 1;
  if MangaInfo = nil then Exit;
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source),
      Module.RootURL + dirurls[Module.CurrentDirectoryIndex] +
      dirparam + IntToStr(perpage), 3) then
    begin
      Result := NO_ERROR;
      Parse := TStringList.Create;
      try
        ParseHTML(Source.Text, Parse);
        ScanParse;
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
  s: String;
  p: Integer;

  procedure ScanParse;
  var
    i, j: Integer;
  begin
    j := -1;
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'table') and
        (GetVal(Parse[i], 'class') = 'ipb_table topic_list hover_rows') then
      begin
        j := i;
        Break;
      end;
    if (j > -1) and (j < Parse.Count) then
      for i := j to Parse.Count - 1 do
        if Pos('</table', Parse[i]) <> 0 then
          Break
        else
        if GetTagName(Parse[i]) = 'a' then
        begin
          Links.Add(GetVal(Parse[i], 'href'));
          Names.Add(CommonStringFilter(Parse[i + 1]));
        end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  s := Module.RootURL + dirurls[Module.CurrentDirectoryIndex] +
    dirparam + IntToStr(perpage);
  p := StrToIntDef(URL, 0);
  if p > 0 then
    s += '&st=' + (IntToStr(p * perpage));
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), s, 3) then
    begin
      Result := NO_ERROR;
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
  info: TMangaInfo;

  procedure ScanChapters(const StartIndex: Integer);
  var
    i: Integer;
    addchap: Boolean;
    s: String;
  begin
    for i := StartIndex to Parse.Count - 1 do
    begin
      if Pos('</table', Parse[i]) <> 0 then
        Break
      else
      if (GetTagName(Parse[i]) = 'tr') and (Pos(' chapter_row', Parse[i]) <> 0) then
      begin
        addchap := True;
        s := CommonStringFilter(Parse[i + 6]);
        if (not OptionShowAllLang) then
          addchap := (Pos(' lang_English ', Parse[i]) <> 0)
        else
          s += Format(' [%s]', [Trim(GetVal(Parse[i + 12], 'title'))]);
        if addchap then
        begin
          if OptionShowBatotoSG then
            s += Format(' [%s]', [Trim(Parse[i + 19])]);
          info.chapterLinks.Add(GetVal(Parse[i + 4], 'href'));
          info.chapterName.Add(s);
        end;
      end;
    end;
    if info.chapterLinks.Count > 0 then
      InvertStrings([info.chapterLinks, info.chapterName]);
  end;

  procedure ScanParse;
  var
    i, j: Integer;
  begin
    info := MangaInfo.mangaInfo;
    info.genres := '';
    info.summary := '';
    for i := 0 to Parse.Count - 1 do
    begin
      //title
      if info.title = '' then
        if (Pos('ipsType_pagetitle', Parse[i]) > 0) then
          info.title := CommonStringFilter(Parse[i + 1]);

      //cover
      if info.coverLink = '' then
        if (GetTagName(Parse[i]) = 'img') and
          (Pos('max-height:500px', Parse[i]) > 0) then
          info.coverLink := GetVal(Parse[i], 'src');

      //author
      if (Pos('Author:', Parse[i]) > 0) then
        info.authors := Trim(Parse[i + 5]);

      //artist
      if (Pos('Artist:', parse[i]) > 0) then
        info.artists := Trim(Parse[i + 5]);

      //genres
      if Pos('/search?genres=', Parse[i]) > 0 then
      begin
        if Pos('</span', Parse[i + 4]) > 0 then
          if info.genres = '' then
            info.genres := Trim(Parse[i + 3])
          else
            info.genres := info.genres + ', ' + Parse[i + 3];
      end;

      //summary
      if (Pos('Description:', Parse[i]) <> 0) then
      begin
        info.summary := '';
        for j := i + 3 to Parse.Count - 1 do
        begin
          if Pos('</td>', Parse[j]) <> 0 then
            Break
          else
          if Pos('<', Parse[j]) <> 1 then
          begin
            if info.summary <> '' then
              info.summary += LineEnding;
            info.summary += StringFilter(Parse[j]);
          end;
        end;
      end;

      //status
      if (Pos('Status:', Parse[i]) > 0) then
      begin
        if (i + 4 < Parse.Count - 1) and
          (Pos('Ongoing', Parse[i + 4]) > 0) then
          info.status := '1'   // ongoing
        else
          info.status := '0';  // completed
      end;

      //chapters
      if (GetTagName(Parse[i]) = 'table') and
        (GetVal(Parse[i], 'class') = 'ipb_table chapters_list') then
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
  Source := TStringList.Create;
  try
    if MangaInfo.GetPage(TObject(Source), MangaInfo.mangaInfo.url, Reconnect) then
    begin
      Result := NO_ERROR;
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
  Source, Parse: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    i, j: Integer;
    isWebtoon: Boolean;
  begin
    for i := 0 to Parse.Count - 1 do
      if (Pos('page_select', Parse[i]) > 0) then
      begin
        isWebtoon := False;
        Break;
      end;

    //webtoon
    if isWebtoon then
      for i := 0 to Parse.Count - 1 do
      begin
        if Pos('<img ', Parse[i]) > 0 then
          if Pos('<br/>', Parse[i + 1]) > 0 then
            Container.pageLinks.Add(GetVal(Parse[i], 'src'));
      end
    else
      for i := 0 to Parse.Count - 1 do
      begin
        if Pos('page_select', Parse[i]) <> 0 then
        begin
          j := i + 2;
          while GetTagName(Parse[j]) = 'option' do
          begin
            Inc(Container.PageNumber);
            Inc(j, 3);
          end;
          Break;
        end;
      end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Container.PageLinks.Clear;
  Container.PageContainerLinks.Clear;
  Container.PageNumber := 0;
  Source := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Source), FillHost(Module.RootURL, URL) + '/1',
      Container.Manager.retryConnect) then
    begin
      Result := True;
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

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Source, Parse: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    i: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'img') and (GetVal(Parse[i], 'id') = 'comic_page') then
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
  Source := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Source), FillHost(Module.RootURL, URL) +
      '/' + IntToStr(DownloadThread.workCounter + 1),
      Container.Manager.retryConnect) then
    begin
      Result := True;
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

procedure RegisterModule;
begin
  with AddModule do
  begin
    Website := 'Batoto';
    RootURL := 'http://bato.to';
    SortedList := True;
    InformationAvailable := True;
    TotalDirectory := Length(dirurls);
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
