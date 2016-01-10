unit PecintaKomik;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, uData, uBaseUnit, uDownloadsManager,
  HTMLUtil, synautil;

implementation

const
  dirURL = '/directory/';

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
    i, j: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'section') and
        (GetVal(Parse[i], 'class') = 'cols') then
      begin
        for j := i + 1 to Parse.Count - 1 do
          if GetTagName(Parse[j]) = '/section' then
            Break
          else
          if (GetTagName(Parse[j]) = 'a') and
            (GetVal(Parse[j], 'class') = 'screenshot') then
          begin
            Links.Add(AppendURLDelim(GetVal(Parse[j], 'href')));
            Names.Add(CommonStringFilter(Parse[j + 1]));
          end;
        Break;
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
    s: String;
  begin
    for i := StartIndex + 1 to Parse.Count - 1 do
    begin
      s := GetTagName(Parse[i]);
      if s = '/div' then
        Break
      else
      if s = 'a' then
      begin
        info.chapterLinks.Add(AppendURLDelim(GetVal(Parse[i], 'href')));
        info.chapterName.Add(CommonStringFilter(Parse[i + 1]));
      end;
    end;

    //invert chapters
    if info.chapterLinks.Count > 0 then
      InvertStrings([info.chapterLinks, info.chapterName]);
  end;

  procedure ScanInfo(const StartIndex: Integer);
  var
    i: Integer;
    s: String;
  begin
    for i := StartIndex + 1 to Parse.Count - 1 do
    begin
      s := GetTagName(Parse[i]);
      if s = '/div' then
        Break
      else
      if s = 'li' then
      begin
        //authors
        if Pos('Author(s):', Parse[i + 1]) = 1 then
          info.authors := CommonStringFilter(SeparateRight(Parse[i + 1],
            'Author(s):'))
        else
        //artist
        if Pos('Artist(s):', Parse[i + 1]) = 1 then
          info.artists := CommonStringFilter(SeparateRight(Parse[i + 1],
            'Artist(s):'))
        else
        //genres
        if Pos('Genre:', Parse[i + 1]) = 1 then
          info.genres := CommonStringFilter(SeparateRight(Parse[i + 1], 'Genre:'))
        else
        //summary
        if Pos('Sinopsis:', Parse[i + 1]) = 1 then
          info.summary := CommonStringFilter(SeparateRight(Parse[i + 1], 'Sinopsis:'));
      end;
    end;
  end;

  procedure ScanInfo2(const StartIndex: Integer);
  var
    i: Integer;
    s: String;
  begin
    for i := StartIndex + 1 to Parse.Count - 1 do
    begin
      s := GetTagName(Parse[i]);
      if s = '/div' then
        Break
      else
      if s = 'li' then
      begin
        //authors
        if Pos('Author(s):', Parse[i + 2]) = 1 then
          info.authors := CommonStringFilter(Parse[i + 4])
        else
        //artist
        if Pos('Artist(s):', Parse[i + 2]) = 1 then
          info.artists := CommonStringFilter(Parse[i + 4])
        else
        //genres
        if Pos('Genre:', Parse[i + 2]) = 1 then
          info.genres := CommonStringFilter(Parse[i + 4])
        else
        //summary
        if Pos('Sinopsis:', Parse[i + 2]) = 1 then
          info.summary := CommonStringFilter(Parse[i + 4]);
      end;
    end;
  end;

  //broken layout
  procedure ScanInfo3;
  var
    i, j: Integer;
  begin
    info.genres := '';
    info.summary := '';
    for i := 0 to Parse.Count - 1 do
    begin
      //cover
      if info.coverLink = '' then
        if (GetTagName(Parse[i]) = 'img') and
          (GetVal(Parse[i], 'class') = 'pecintakomik') then
          info.coverLink := MaybeFillHost(Module.RootURL, GetVal(Parse[i], 'src'));
      //title
      if info.title = '' then
        if GetVal(Parse[i], 'class') = 'aname' then
          info.title := CommonStringFilter(Parse[i + 1]);
      if GetVal(Parse[i], 'class') = 'propertytitle' then
      begin
        //author
        if Pos('Autor:', Parse[i + 1]) = 1 then
          info.authors := CommonStringFilter(Parse[i + 6])
        else
        //artist
        if Pos('Artist:', Parse[i + 1]) = 1 then
          info.artists := CommonStringFilter(Parse[i + 6]);
        //summary
        if Pos('Ringkasan Cerita:', Parse[i + 1]) = 1 then
          for j := i + 3 to Parse.Count - 1 do
          begin
            if GetTagName(Parse[j]) = '/td' then
              Break
            else
            if (Parse[j] <> '') and (Parse[j][1] <> '<') then
            begin
              if info.summary <> '' then
                info.summary += LineEnding;
              info.summary += CommonStringFilter(Parse[j]);
            end;
          end;
        //genre
        if GetVal(Parse[i], 'class') = 'genretags' then
          AddCommaString(info.genres, CommonStringFilter(Parse[i + 1]));
      end;
      //chapters
      if (GetTagName(Parse[i]) = 'ul') and
        (GetVal(Parse[i], 'class') = 'series_alpha') then
      begin
        ScanChapters(i);
        Break;
      end;
    end;
  end;

  procedure ScanParse;
  var
    i, j: Integer;
  begin
    if Pos('/manga/', URL) = 0 then
    begin
      for i := 0 to Parse.Count - 1 do
      begin
        if GetVal(Parse[i], 'http-equiv') = 'x-ua-compatible' then
        begin
          ScanInfo3;
          Break;
        end;

        //cover
        if info.coverLink = '' then
          if (GetTagName(Parse[i]) = 'img') and
            (GetVal(Parse[i], 'class') = 'pecintakomik') then
            info.coverLink := MaybeFillHost(Module.RootURL, GetVal(Parse[i], 'src'));

        if GetVal(Parse[i], 'class') = 'post-cnt' then
          if GetTagName(Parse[i + 2]) = 'h2' then
          begin
            //chapters
            if Pos('List Chapter(s)', Parse[i + 3]) <> 0 then
            begin
              ScanChapters(i);
              Break;
            end
            else
            begin
              //title
              if info.title = '' then
                info.title := CommonStringFilter(Parse[i + 3]);
              //info
              if (GetTagName(Parse[i + 8]) = 'li') and
                (GetTagName(Parse[i + 9]) <> 'strong') then
                ScanInfo(i)
              else
                ScanInfo2(i);
            end;
          end;
      end;
    end
    else
      //no info
      for i := 0 to Parse.Count - 1 do
      begin
        //title
        if info.title = '' then
          if (GetTagName(Parse[i]) = 'select') and
            (GetVal(Parse[i], 'name') = 'manga') then
            for j := i + 1 to Parse.Count - 1 do
            begin
              if GetTagName(Parse[j]) = '/select' then
                Break
              else
              if (GetTagName(Parse[j]) = 'option') and
                (GetVal(Parse[j], 'selected') = 'selected') then
              begin
                info.title := CommonStringFilter(Parse[j + 1]);
                Break;
              end;
            end;
        //chapters
        if (GetTagName(Parse[i]) = 'select') and
          (GetVal(Parse[i], 'name') = 'chapter') then
        begin
          for j := i + 1 to Parse.Count - 1 do
          begin
            if GetTagName(Parse[j]) = '/select' then
              Break
            else
            if GetTagName(Parse[j]) = 'option' then
            begin
              Inc(info.numChapter);
              info.chapterLinks.Add(AppendURLDelim(URL) + IntToStr(info.numChapter));
              info.chapterName.Add('Chapter ' + IntToStr(info.numChapter));
            end;
          end;
          Break;
        end;
      end;
  end;

begin
  Result := NET_PROBLEM;
  if MangaInfo = nil then Exit;
  info := MangaInfo.mangaInfo;
  info.website := Module.Website;
  info.url := AppendURLDelim(FillHost(Module.RootURL, URL));
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
    i, j: Integer;
  begin
    for i := 0 to Parse.Count - 1 do
      if (GetTagName(Parse[i]) = 'select') and
        (GetVal(Parse[i], 'name') = 'page') then
      begin
        for j := i + 1 to Parse.Count - 1 do
        begin
          if GetTagName(Parse[j]) = '/select' then
            Break
          else
          if GetTagName(Parse[j]) = 'option' then
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

function GetImageURL(var DownloadThread: TDownloadThread; const URL: String;
  Module: TModuleContainer): Boolean;
var
  Parse: TStringList;
  Container: TTaskContainer;

  procedure ScanParse;
  var
    i: Integer;
    baseurl: String;
  begin
    baseurl := Module.RootURL;
    for i := 0 to Parse.Count - 1 do
    begin
      if GetTagName(Parse[i]) = 'base' then
        baseurl := GetVal(Parse[i], 'href');
      if (GetTagName(Parse[i]) = 'img') and (GetVal(Parse[i], 'class') = 'picture') then
      begin
        if DownloadThread.workCounter < Container.PageLinks.Count then
          Container.PageLinks[DownloadThread.workCounter] :=
            MaybeFillHost(baseurl, GetVal(Parse[i], 'src'));
        Break;
      end;
    end;
  end;

begin
  Result := False;
  if DownloadThread = nil then Exit;
  Container := DownloadThread.manager.container;
  Parse := TStringList.Create;
  try
    if DownloadThread.GetPage(TObject(Parse),
      AppendURLDelim(FillHost(Module.RootURL, URL)) + IncStr(DownloadThread.workCounter),
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
    Website := 'PecintaKomik';
    RootURL := 'http://www.pecintakomik.com';
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
