{
        File: uData.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uData;

{$mode delphi}
{$DEFINE DOWNLOADER}

// This unit contains all necessary functions for data processing

interface

uses
  Classes, SysUtils, uBaseUnit, uFMDThread, DBDataProcess, FileUtil,
  LazFileUtils, SimpleLogger, strutils, dateutils, RegExpr, httpsend;

type

  { TDataProcess }

  TDataProcess = class(TObject)
  private
    function GetInfo(const index: Cardinal): TStringList;
    function GetParam(const index, paramNo: Integer): String;
  public
    website, Filename: String;
    isFilterAllSites, isFiltered: Boolean;

    site, filterMark: TByteList;
    // used by search
    searchPos, filterPos: TCardinalList;
    Data,

    // parts
    Title, Link, Authors, Artists, Genres, Status, Summary: TStringList;
    JDN: TList;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function FirstParam(const index: Cardinal): String;

    // en: Break data into parts... This may be considered as bad coding, but
    //     it's for faster filter
    procedure BreakDataToParts(const i: Cardinal);

    function LoadFromFile(const website: String): Boolean;
    function LoadFromAllFiles(const websiteList: TStringList): Boolean;
    procedure SaveToFile(const website: String); overload;
    procedure SaveToFile; overload;

    function CanFilter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Cardinal; const haveAllChecked, searchNewManga: Boolean): Boolean;

    // en: Filter by genres, title, authors, ...
    function Filter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Cardinal; const haveAllChecked, searchNewManga: Boolean;
      useRegExpr: Boolean = False): Boolean;
    // realtime search
    function Search(AMangaName: String): Boolean;
    // get data position
    function GetPos(const ANodePos: Integer): Integer;

    // en: Remove filter
    procedure RemoveFilter;
    procedure Sort;
    property Info[index: Cardinal]: TStringList read GetInfo;
    property Param[index, paramNo: Integer]: String read GetParam;
  end;

  { TMangaInformation }

  TMangaInformation = class(TObject)
  public
    isGetByUpdater: Boolean;
    mangaInfo: TMangaInfo;
    parse: TStringList;
    isGenerateFolderChapterName: Boolean;
    isRemoveUnicode: Boolean;
    FHTTP: THTTPSendThread;
    ModuleId: Integer;

    procedure OnTag(NoCaseTag, ActualTag: String);
    procedure OnText(Text: String);
    constructor Create(AOwnerThread: TFMDThread = nil; CreateInfo: Boolean = True);
    destructor Destroy; override;
    procedure ClearInfo;
    function GetDirectoryPage(var Page: Integer; const website: String): Byte;
    function GetNameAndLink(const names, links: TStringList;
      const website, URL: String): Byte;
    function GetInfoFromURL(const website, URL: String; const Reconnect: Integer = 0): Byte;
    procedure SyncInfoToData(const DataProcess: TDataProcess;
      const index: Cardinal); overload;
    procedure SyncInfoToData(const DataProcess: TDBDataProcess); overload;
    procedure SyncMinorInfoToData(const DataProcess: TDataProcess;
      const index: Cardinal);

    // Only use this function for getting manga infos for the first time
    procedure AddInfoToDataWithoutBreak(const Name, link: String;
      const DataProcess: TDataProcess);
    // Only use this function for update manga list
    procedure AddInfoToData(const Name, link: String; const DataProcess: TDataProcess);
      overload;
    // to add data to TDBDataProcess
    procedure AddInfoToData(const Title, Link: String;
      const DataProcess: TDBDataProcess); overload;
    //wrapper
    function GetPage(var output: TObject; URL: String;
      const Reconnect: Integer = 0): Boolean; inline;
  end;

var
  options: TStringList;

implementation

uses
  Dialogs, fpJSON, JSONParser, jsonscanner, IniFiles, FastHTMLParser, HTMLUtil,
  SynaCode, uMisc, frmMain, WebsiteModules;

// ----- TDataProcess -----

constructor TDataProcess.Create;
begin
  inherited Create;
  isFilterAllSites := False;
  isFiltered := False;
  Data := TStringList.Create;

  Title := TStringList.Create;
  Link := TStringList.Create;
  Authors := TStringList.Create;
  Artists := TStringList.Create;
  Genres := TStringList.Create;
  Status := TStringList.Create;
  Summary := TStringList.Create;
  JDN := TList.Create;

  site := TByteList.Create;
  filterMark := TByteList.Create;
  filterPos := TCardinalList.Create;
  searchPos := TCardinalList.Create;
end;

destructor TDataProcess.Destroy;
begin
  searchPos.Free;
  filterMark.Free;
  filterPos.Free;
  site.Free;

  Title.Free;
  Link.Free;
  Authors.Free;
  Artists.Free;
  Genres.Free;
  Status.Free;
  Summary.Free;
  JDN.Free;

  Data.Free;
  inherited Destroy;
end;

procedure TDataProcess.Clear;
begin
  isFilterAllSites := False;
  isFiltered := False;

  Data.Clear;
  Title.Clear;
  Link.Clear;
  Authors.Clear;
  Artists.Clear;
  Genres.Clear;
  Status.Clear;
  Summary.Clear;

  JDN.Clear;
  site.Clear;
  filterMark.Clear;
  filterPos.Clear;
  searchPos.Clear;
end;

function TDataProcess.FirstParam(const index: Cardinal): String;
var
  l: Cardinal;
begin
  Result := '';
  l := Pos(SEPERATOR, Data.Strings[index]);
  if l <> 0 then
    Result := LeftStr(Data.Strings[index], l - 1);
end;

function TDataProcess.GetInfo(const index: Cardinal): TStringList;
begin
  GetParams(Result{%H-}, Data.Strings[index]);
end;

function TDataProcess.GetParam(const index, paramNo: Integer): String;
var
  i, p: Integer;
  s: String;
begin
  Result := '';
  if index < Data.Count then
  begin
    s := Data.Strings[index];
    i := 0;
    p := 0;
    repeat
      p := Pos(SEPERATOR, s);
      if p > 0 then
      begin
        Inc(i);
        Result := LeftStr(s, p - 1);
        s := RightStr(s, Length(s) - p - Length(SEPERATOR) + 1);
      end;
    until (p = 0) or (i > paramNo);
    if i <= paramNo then
      Result := '';
  end;
end;

// en: break data - for fast filter
procedure TDataProcess.BreakDataToParts(const i: Cardinal);
begin
  if i < Data.Count then
  begin
    Title.Strings[i] := GetParam(i, DATA_PARAM_TITLE);
    Link.Strings[i] := GetParam(i, DATA_PARAM_LINK);
    Authors.Strings[i] := GetParam(i, DATA_PARAM_AUTHORS);
    Artists.Strings[i] := GetParam(i, DATA_PARAM_ARTISTS);
    Genres.Strings[i] := GetParam(i, DATA_PARAM_GENRES);
    Status.Strings[i] := GetParam(i, DATA_PARAM_STATUS);
    Summary.Strings[i] := GetParam(i, DATA_PARAM_SUMMARY);
    JDN.Items[i] := Pointer(StrToIntDef(GetParam(i, DATA_PARAM_JDN), 0));
  end;
end;

function TDataProcess.LoadFromFile(const website: String): Boolean;
var
  id, i: Cardinal;
  l: TStringList;
  Filename: String;
begin
  Filename := fmdDirectory + DATA_FOLDER + website;

  Data.Clear;
  searchPos.Clear;
  filterMark.Clear;
  filterPos.Clear;
  site.Clear;

  title.Clear;
  authors.Clear;
  artists.Clear;
  genres.Clear;
  status.Clear;
  summary.Clear;
  jdn.Clear;

  if not FileExistsUTF8(Filename + DATA_EXT) then
    Exit(False);
  l := TStringList.Create;
  try
    Self.Filename := Filename;

    Data.LoadFromFile(Filename + DATA_EXT);
    id := GetMangaSiteID(website);

    if Data.Count > 0 then
    begin
      //QuickSortData(data);
      QuickSortNaturalPart(Data, SEPERATOR, DATA_PARAM_TITLE); //Natural Sorting
      for i := 0 to Data.Count - 1 do
      begin
        filterMark.Add(FILTER_SHOW);
        filterPos.Add(i);
        site.Add(id);

        l.Clear;
        try
          GetParams(l, Data.Strings[i]);
          while l.Count < 10 do
            l.Add('');
          title.Add(l.Strings[DATA_PARAM_TITLE]);
          link.Add(l.Strings[DATA_PARAM_LINK]);
          authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
          artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
          genres.Add(l.Strings[DATA_PARAM_GENRES]);
          status.Add(l.Strings[DATA_PARAM_STATUS]);
          summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
          jdn.Add(Pointer(StrToIntDef(l.Strings[DATA_PARAM_JDN], 0)));
        except
        end;
      end;
    end;
  finally
    l.Free;
  end;
  Result := True;
end;

function TDataProcess.LoadFromAllFiles(const websiteList: TStringList): Boolean;
var
  id, j, i: Cardinal;
  l: TStringList;
  Filename: String;
begin
  if websiteList.Count = 0 then
    Exit(False);
  Data.Clear;
  searchPos.Clear;
  filterMark.Clear;
  filterPos.Clear;
  site.Clear;

  title.Clear;
  authors.Clear;
  artists.Clear;
  genres.Clear;
  status.Clear;
  summary.Clear;
  jdn.Clear;

  l := TStringList.Create;
  try
    for i := 0 to websiteList.Count - 1 do
    begin
      Filename := DATA_FOLDER + websiteList.Strings[i];
      id := GetMangaSiteID(websiteList.Strings[i]);
      if not FileExistsUTF8(Filename + DATA_EXT) then
        continue;
      l.Clear;
      l.LoadFromFile(Filename + DATA_EXT);

      if l.Count <> 0 then
      begin
        for j := 0 to l.Count - 1 do
        begin
          site.Add(id);
        end;
        Data.Text := Data.Text + l.Text;
      end;
    end;

    if Data.Count > 0 then
    begin
      QuickSortDataWithWebID(Data, site);
      for i := 0 to Data.Count - 1 do
      begin
        filterMark.Add(FILTER_SHOW);
        filterPos.Add(i);

        l.Clear;
        try
          GetParams(l, Data.Strings[i]);
          while l.Count < 10 do
            l.Add('');
          title.Add(l.Strings[DATA_PARAM_TITLE]);
          link.Add(l.Strings[DATA_PARAM_LINK]);
          authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
          artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
          genres.Add(l.Strings[DATA_PARAM_GENRES]);
          status.Add(l.Strings[DATA_PARAM_STATUS]);
          summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
          jdn.Add(Pointer(StrToIntDef(l.Strings[DATA_PARAM_JDN], 0)));
        except
        end;
      end;
    end;
  finally
    l.Free;
  end;
  Result := True;
end;

procedure TDataProcess.SaveToFile(const website: String);
begin
  if Data.Count = 0 then
    Exit;
  //QuickSortData(Data);
  ForceDirectoriesUTF8(DATA_FOLDER);
  Data.SaveToFile(DATA_FOLDER + website + DATA_EXT);
end;

procedure TDataProcess.SaveToFile;
begin
  if Data.Count = 0 then
    Exit;
  //QuickSortData(Data);
  ForceDirectoriesUTF8(DATA_FOLDER);
  Data.SaveToFile(Filename + DATA_EXT);
end;

// check if we need to filter or not
function TDataProcess.CanFilter(const checkedGenres, uncheckedGenres: TStringList;
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
  const minusDay: Cardinal; const haveAllChecked, searchNewManga: Boolean): Boolean;
begin
  if (filterPos.Count = 0) or
    (Data.Count = 0) or
    ((stTitle = '') and
    (stAuthors = '') and
    (stArtists = '') and
    (stSummary = '') and
    (stStatus = '2') and
    (checkedGenres.Count = 0) and
    (uncheckedGenres.Count = 0)) and
    (not searchNewManga) then
    Result := False
  else
    Result := True;
end;

function TDataProcess.Filter(const checkedGenres, uncheckedGenres: TStringList;
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
  const minusDay: Cardinal; const haveAllChecked, searchNewManga: Boolean;
  useRegExpr: Boolean = False): Boolean;
var
  currentJDN, i, j, k, fpos, Count: Integer;
  s: String;
  regx: TRegExpr;
  fshow: Boolean;
  gen: TStringList;
begin
  regx := TRegExpr.Create;
  regx.ModifierI := True;
  try
    Result := False;
    searchPos.Clear;
    if (filterPos.Count = 0) or
      (Data.Count = 0) or
      ((stTitle = '') and
      (stAuthors = '') and
      (stArtists = '') and
      (stSummary = '') and
      (stStatus = '2') and
      (checkedGenres.Count = 0) and
      (uncheckedGenres.Count = 0)) and
      (not searchNewManga) then
      Exit;

    // ugly filter code but quite fast
    if searchNewManga then
    begin
      currentJDN := GetCurrentJDN;
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if (currentJDN - {%H-}integer(jdn.Items[fpos]) >= minusDay) and
          (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos] := FILTER_HIDE;
      end;
    end;

    // filter title
    if Trim(stTitle) <> '' then
    begin
      s := LowerCase(stTitle);
      if useRegExpr then
        regx.Expression := stTitle;
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if useRegExpr then
          fshow := not regx.Exec(Title[fpos])
        else
          fshow := (Pos(s, LowerCase(Title.Strings[fpos])) = 0);
        if fshow and
          (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos] := FILTER_HIDE;
      end;
    end;

    // filter authors
    if stAuthors <> '' then
    begin
      s := LowerCase(stAuthors);
      if useRegExpr then
        regx.Expression := stAuthors;
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if useRegExpr then
          fshow := not regx.Exec(Authors[fpos])
        else
          fshow := (Pos(s, LowerCase(Authors.Strings[fpos])) = 0);
        if fshow and
          (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos] := FILTER_HIDE;
      end;
    end;

    // filter artist
    if stArtists <> '' then
    begin
      s := LowerCase(stArtists);
      if useRegExpr then
        regx.Expression := stArtists;
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if useRegExpr then
          fshow := not regx.Exec(Artists[fpos])
        else
          fshow := (Pos(s, LowerCase(Artists.Strings[fpos])) = 0);
        if fshow and
          (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos] := FILTER_HIDE;
      end;
    end;

    // filter summary
    if stSummary <> '' then
    begin
      s := LowerCase(stSummary);
      if useRegExpr then
        regx.Expression := stSummary;
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if useRegExpr then
          fshow := not regx.Exec(Summary[fpos])
        else
          fshow := (Pos(s, LowerCase(Summary.Strings[fpos])) = 0);
        if fshow and
          (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos] := FILTER_HIDE;
      end;
    end;

    // filter status
    if stStatus <> '2' then
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if (CompareText(stStatus, Status.Strings[fpos]) <> 0) and
          (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos] := FILTER_HIDE;
      end;

    // filter genres
    if checkedGenres.Count > 0 then
    begin
      for i := 0 to checkedGenres.Count - 1 do
        if useRegExpr then
          checkedGenres[i] := Trim(checkedGenres[i])
        else
          checkedGenres.Strings[i] := Trim(LowerCase(checkedGenres.Strings[i]));
      gen := TStringList.Create;
      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if filterMark.Items[fpos] = FILTER_SHOW then
        begin
          gen.Clear;
          ExtractStrings([','], [], PChar(Trim(LowerCase(Genres[fpos]))), gen);
          TrimStrings(gen);
          if gen.Count > 0 then
          begin
            if haveAllChecked then
            begin
              Count := checkedGenres.Count;
              for j := 0 to checkedGenres.Count - 1 do
              begin
                if useRegExpr then
                  regx.Expression := checkedGenres[j];
                for k := 0 to gen.Count - 1 do
                begin
                  if useRegExpr then
                    fshow := regx.Exec(gen[k])
                  else
                    fshow := SameText(checkedGenres[j], gen[k]);
                  if fshow then
                  begin
                    Dec(Count);
                    Break;
                  end;
                end;
              end;
              if Count > 0 then
                filterMark.Items[fpos] := FILTER_HIDE;
            end
            else
            begin
              filterMark.Items[fpos] := FILTER_HIDE;
              for j := 0 to checkedGenres.Count - 1 do
              begin
                if useRegExpr then
                  regx.Expression := checkedGenres[j];
                for k := 0 to gen.Count - 1 do
                begin
                  if useRegExpr then
                    fshow := regx.Exec(gen[k])
                  else
                    fshow := SameText(checkedGenres[j], gen[k]);
                  if fshow then
                    Break;
                end;
                if fshow then
                begin
                  filterMark.Items[fpos] := FILTER_SHOW;
                  Break;
                end;
              end;
            end;
          end
          else
          begin
            if filterMark.Items[fpos] = FILTER_SHOW then
              filterMark.Items[fpos] := FILTER_HIDE;
          end;
        end;
      end;
      gen.Free;
    end;

    if uncheckedGenres.Count > 0 then
    begin
      for i := 0 to uncheckedGenres.Count - 1 do
        uncheckedGenres.Strings[i] := LowerCase(uncheckedGenres.Strings[i]);

      for i := 0 to filterPos.Count - 1 do
      begin
        fpos := filterPos.Items[i];
        if (filterMark.Items[fpos] = FILTER_SHOW) then
        begin
          s := LowerCase(Genres.Strings[fpos]);
          if haveAllChecked then
          begin
            Count := uncheckedGenres.Count;
            for j := 0 to uncheckedGenres.Count - 1 do
              if Pos((uncheckedGenres.Strings[j] + ','), s) = 0 then
                Dec(Count);
            if Count > 0 then
              filterMark.Items[fpos] := FILTER_HIDE;
          end
          else
          begin
            for j := 0 to uncheckedGenres.Count - 1 do
              if Pos((uncheckedGenres.Strings[j] + ','), s) <> 0 then
              begin
                filterMark.Items[fpos] := FILTER_HIDE;
                Break;
              end;
          end;
        end;
      end;
    end;

    fpos := filterPos.Count;
    filterPos.Clear;
    for i := 0 to Data.Count - 1 do
      if filterMark.Items[i] = FILTER_SHOW then
        filterPos.Add(i);

    if filterPos.Count <> fpos then
    begin
      isFiltered := True;
      Result := True;
    end;
  except
    on E: Exception do
      WriteLog_E('TDataProcess.Filter.Error!', E, Self);
  end;
  regx.Free;
end;

function TDataProcess.Search(AMangaName: String): Boolean;
var
  i: Cardinal;
begin
  searchPos.Clear;
  if filterPos.Count <= 0 then
    Exit;
  AMangaName := Upcase(AMangaName);
  for i := 0 to filterPos.Count - 1 do
  begin
    if Pos(AMangaName, upcase(Title.Strings[filterPos.Items[i]])) > 0 then
    begin
      searchPos.Add(filterPos.Items[i]);
      Result := True;
    end;
  end;
end;

// get data position
function TDataProcess.GetPos(const ANodePos: Integer): Integer;
begin
  if searchPos.Count = 0 then
    Result := filterPos.Items[ANodePos]
  else
    Result := searchPos.Items[ANodePos];
end;

procedure TDataProcess.RemoveFilter;
var
  i: Cardinal;
begin
  searchPos.Clear;
  filterMark.Clear;
  filterPos.Clear;
  if Data.Count > 0 then
  begin
    for i := 0 to Data.Count - 1 do
    begin
      filterMark.Add(FILTER_SHOW);
      filterPos.Add(i);
    end;
  end;
  isFiltered := False;
end;

procedure TDataProcess.Sort;
begin
  //QuickSortData(data);
  uMisc.QuickSortNaturalPart(Data, SEPERATOR, DATA_PARAM_TITLE);
end;

{ TMangaInformation }

constructor TMangaInformation.Create(AOwnerThread: TFMDThread;
  CreateInfo: Boolean);
begin
  inherited Create;
  FHTTP := THTTPSendThread.Create(AOwnerThread);
  FHTTP.Headers.NameValueSeparator := ':';
  parse := TStringList.Create;
  if CreateInfo then
    mangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
  ModuleId := -1;
end;

destructor TMangaInformation.Destroy;
begin
  if Assigned(mangaInfo) then
    mangaInfo.Free;
  if Assigned(parse) then
    parse.Free;
  FHTTP.Free;
  inherited Destroy;
end;

procedure TMangaInformation.ClearInfo;
begin
  mangaInfo.artists := '';
  mangaInfo.authors := '';
  mangaInfo.genres := '';
  mangaInfo.summary := '';
  mangaInfo.coverLink := '';
  mangaInfo.numChapter := 0;
  mangaInfo.status := '';
  mangaInfo.title := '';
  mangaInfo.url := '';
  mangaInfo.website := '';
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;
end;

procedure TMangaInformation.OnTag(NoCaseTag, ActualTag: String);
begin
  parse.Add(ActualTag);
end;

procedure TMangaInformation.OnText(Text: String);
begin
  parse.Add(Text);
end;

function TMangaInformation.GetDirectoryPage(var Page: Integer;
  const website: String): Byte;
var
  s: String;
  p: Integer;
  Source: TStringList;
  Parser: THTMLParser;
  WebsiteID: Cardinal;

  {$I includes/AnimeA/directory_page_number.inc}

  {$I includes/Manga24h/directory_page_number.inc}

  {$I includes/VnSharing/directory_page_number.inc}

  {$I includes/Hentai2Read/directory_page_number.inc}

  {$I includes/Fakku/directory_page_number.inc}

  {$I includes/MangaPark/directory_page_number.inc}

  //{$I includes/MangaTraders/directory_page_number.inc}

  {$I includes/SenManga/directory_page_number.inc}

  {$I includes/MangaGo/directory_page_number.inc}

  {$I includes/MangaEden/directory_page_number.inc}

  {$I includes/BlogTruyen/directory_page_number.inc}

  {$I includes/S2Scans/directory_page_number.inc}

  {$I includes/LectureEnLigne/directory_page_number.inc}

  {$I includes/MangaAe/directory_page_number.inc}

  {$I includes/CentralDeMangas/directory_page_number.inc}

  {$I includes/DM5/directory_page_number.inc}

  {$I includes/NineManga/directory_page_number.inc}

  {$I includes/JapanShin/directory_page_number.inc}

  {$I includes/Mangacow/directory_page_number.inc}

  {$I includes/OneManga/directory_page_number.inc}

  {$I includes/MangaTown/directory_page_number.inc}

  {$I includes/MyReadingMangaInfo/directory_page_number.inc}

  {$I includes/IKomik/directory_page_number.inc}

  {$I includes/NHentai/directory_page_number.inc}

  {$I includes/MangaMint/directory_page_number.inc}

  {$I includes/MangaFrame/directory_page_number.inc}

  {$I includes/MangaHost/directory_page_number.inc}

  {$I includes/PornComix/directory_page_number.inc}

  {$I includes/MangaSee/directory_page_number.inc}

  {$I includes/MangaAt/directory_page_number.inc}

  {$I includes/ReadMangaToday/directory_page_number.inc}

  {$I includes/Dynasty-Scans/directory_page_number.inc}

begin
  Page := 0;

  //load User-Agent from INIAdvanced
  AdvanceLoadHTTPConfig(FHTTP, website);

  //load pagenumber_config if available
  p := INIAdvanced.ReadInteger('UpdateListDirectoryPageNumber', website, -1);

  if p > 0 then
  begin
    Page := p;
    BROWSER_INVERT := True;
  end
  else
  begin
    BROWSER_INVERT := False;
    if ModuleId < 0 then
      ModuleId := Modules.LocateModule(website);
    if Modules.ModuleAvailable(ModuleId, MMGetDirectoryPageNumber) then
      Result := Modules.GetDirectoryPageNumber(Self, Page, ModuleId)
    else
    begin
      WebsiteID := GetMangaSiteID(website);
      Source := TStringList.Create;
      if WebsiteID = ANIMEA_ID then
        Result := GetAnimeADirectoryPageNumber
      else
      if WebsiteID = MANGA24H_ID then
        Result := GetManga24hDirectoryPageNumber
      else
      if WebsiteID = VNSHARING_ID then
        Result := GetVnSharingDirectoryPageNumber
      else
      if WebsiteID = HENTAI2READ_ID then
        Result := GetHentai2ReadDirectoryPageNumber
      else
      if WebsiteID = FAKKU_ID then
        Result := GetFakkuDirectoryPageNumber
      else
      if WebsiteID = MANGAPARK_ID then
        Result := GetMangaParkDirectoryPageNumber
      else
      //if WebsiteID = MANGATRADERS_ID then
      //  Result := GetMangaTradersDirectoryPageNumber
      //else
      if WebsiteID = MANGAGO_ID then
        Result := GetMangaGoDirectoryPageNumber
      else
      if WebsiteID = MANGAEDEN_ID then
        Result := GetMangaEdenDirectoryPageNumber(WebsiteRoots[MANGAEDEN_ID, 1])
      else
      if WebsiteID = PERVEDEN_ID then
        Result := GetMangaEdenDirectoryPageNumber(WebsiteRoots[PERVEDEN_ID, 1])
      else
      if WebsiteID = BLOGTRUYEN_ID then
        Result := GetBlogTruyenDirectoryPageNumber
      else
      if WebsiteID = S2SCAN_ID then
        Result := GetS2ScanDirectoryPageNumber
      else
      if WebsiteID = SENMANGA_ID then
        Result := GetSenMangaDirectoryPageNumber
      else
      if WebsiteID = LECTUREENLIGNE_ID then
        Result := GetLectureEnLigneDirectoryPageNumber
      else
      if WebsiteID = MANGAAE_ID then
        Result := GetMangaAeDirectoryPageNumber
      else
      if WebsiteID = CENTRALDEMANGAS_ID then
        Result := GetCentralDeMangasDirectoryPageNumber
      else
      if WebsiteID = DM5_ID then
        Result := GetDM5DirectoryPageNumber
      else
      if (WebsiteID = NINEMANGA_ID) or
        (WebsiteID = NINEMANGA_ES_ID) or
        (WebsiteID = NINEMANGA_CN_ID) or
        (WebsiteID = NINEMANGA_RU_ID) or
        (WebsiteID = NINEMANGA_DE_ID) or
        (WebsiteID = NINEMANGA_IT_ID) or
        (WebsiteID = NINEMANGA_BR_ID) then
        Result := GetNineMangaDirectoryPageNumber
      else
      if WebsiteID = JAPANSHIN_ID then
        Result := GetJapanShinDirectoryPageNumber
      else
      if WebsiteID = MANGACOW_ID then
        Result := GetMangaCowDirectoryPageNumber
      else
      if WebsiteID = ONEMANGA_ID then
        Result := GetOneMangaDirectoryPageNumber
      else
      if WebsiteID = MANGATOWN_ID then
        Result := GetMangaTownDirectoryPageNumber
      else
      if WebsiteID = MYREADINGMANGAINFO_ID then
        Result := GetMyReadingMangaInfoDirectoryPageNumber
      else
      if WebsiteID = IKOMIK_ID then
        Result := GetIKomikDirectoryPageNumber
      else
      if WebsiteID = NHENTAI_ID then
        Result := GetNHentaiDirectoryPageNumber
      else
      if WebsiteID = MANGAMINT_ID then
        Result := GetMangaMintDirectoryPageNumber
      else
      if WebsiteID = MANGAFRAME_ID then
        Result := GetMangaFrameDirectoryPageNumber
      else
      if WebsiteID = MANGAHOST_ID then
        Result := GetMangaHostDirectoryPageNumber
      else
      if (WebsiteID = PORNCOMIX_ID) or
        (WebsiteID = XXCOMICS_ID) or
        (WebsiteID = XXCOMICSMT_ID) or
        (WebsiteID = XXCOMICS3D_ID) or
        (WebsiteID = PORNCOMIXRE_ID) or
        (WebsiteID = PORNCOMIXIC_ID) or
        (WebsiteID = PORNXXXCOMICS_ID) then
        Result := GetPornComixDirectoryPageNumber(GetMangaSiteID(website))
      else
      if WebsiteID = MANGASEE_ID then
        Result := GetMangaSeeDirectoryPageNumber
      else
      if WebsiteID = MANGAAT_ID then
        Result := GetMangaAtDirectoryPageNumber
      else
      if WebsiteID = READMANGATODAY_ID then
        Result := GetReadMangaTodayDirectoryPageNumber
      else
      if WebsiteID = DYNASTYSCANS_ID then
        Result := GetDynastyScansDirectoryPageNumber
      else
      begin
        Result := NO_ERROR;
        Page := 1;
        Source.Free;
      end;
    end;

    if page < 1 then
      Page := 1;
  end;
end;

function TMangaInformation.GetNameAndLink(const names, links: TStringList;
  const website, URL: String): Byte;
var
  Source: TStringList;
  Parser: THTMLParser;
  WebsiteID: Cardinal;

  {$I includes/AnimeA/names_and_links.inc}

  {$I includes/EsMangaHere/names_and_links.inc}

  {$I includes/AnimExtremist/names_and_links.inc}

  {$I includes/MangaInn/names_and_links.inc}

  {$I includes/Manga24h/names_and_links.inc}

  {$I includes/VnSharing/names_and_links.inc}

  {$I includes/Hentai2Read/names_and_links.inc}

  {$I includes/Fakku/names_and_links.inc}

  {$I includes/MangaPark/names_and_links.inc}

  {$I includes/MangaTraders/names_and_links.inc}

  {$I includes/TruyenTranhTuan/names_and_links.inc}

  {$I includes/SubManga/names_and_links.inc}

  {$I includes/Komikid/names_and_links.inc}

  {$I includes/Mabuns/names_and_links.inc}

  {$I includes/MangaEsta/names_and_links.inc}

  {$I includes/HugeManga/names_and_links.inc}

  {$I includes/AnimeStory/names_and_links.inc}

  {$I includes/LectureEnLigne/names_and_links.inc}

  {$I includes/ScanManga/names_and_links.inc}

  {$I includes/MangaAr/names_and_links.inc}

  {$I includes/MangaAe/names_and_links.inc}

  {$I includes/CentralDeMangas/names_and_links.inc}

  {$I includes/Imanhua/names_and_links.inc}

  {$I includes/Turkcraft/names_and_links.inc}

  {$I includes/MangaVadisi/names_and_links.inc}

  {$I includes/MangaFrame/names_and_links.inc}

  {$I includes/Mangacow/names_and_links.inc}

  {$I includes/SenManga/names_and_links.inc}

  {$I includes/Starkana/names_and_links.inc}

  {$I includes/EatManga/names_and_links.inc}

  {$I includes/MangaGo/names_and_links.inc}

  {$I includes/S2Scans/names_and_links.inc}

  {$I includes/EGScans/names_and_links.inc}

  {$I includes/BlogTruyen/names_and_links.inc}

  {$I includes/MangaEden/names_and_links.inc}

  {$I includes/Kivmanga/names_and_links.inc}

  {$I includes/MeinManga/names_and_links.inc}

  {$I includes/MangasPROJECT/names_and_links.inc}

  {$I includes/MangaREADER_POR/names_and_links.inc}

  {$I includes/NineManga/names_and_links.inc}

  {$I includes/JapanShin/names_and_links.inc}

  {$I includes/Japscan/names_and_links.inc}

  {$I includes/CentrumMangi_PL/names_and_links.inc}

  {$I includes/MangaLib_PL/names_and_links.inc}

  {$I includes/OneManga/names_and_links.inc}

  {$I includes/MangaTown/names_and_links.inc}

  {$I includes/MangaOku/names_and_links.inc}

  {$I includes/MyReadingMangaInfo/names_and_links.inc}

  {$I includes/IKomik/names_and_links.inc}

  {$I includes/NHentai/names_and_links.inc}

  {$I includes/MangaMint/names_and_links.inc}

  {$I includes/UnixManga/names_and_links.inc}

  {$I includes/ExtremeMangas/names_and_links.inc}

  {$I includes/MangaHost/names_and_links.inc}

  {$I includes/PornComix/names_and_links.inc}

  {$I includes/MangaSee/names_and_links.inc}

  {$I includes/MangaKu/names_and_links.inc}

  {$I includes/MangaAt/names_and_links.inc}

  {$I includes/ReadMangaToday/names_and_links.inc}

  {$I includes/LoneManga/names_and_links.inc}

  {$I includes/Dynasty-Scans/names_and_links.inc}

begin
  //load User-Agent from INIAdvanced
  AdvanceLoadHTTPConfig(FHTTP, website);

  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(website);
  if Modules.ModuleAvailable(ModuleId, MMGetNameAndLink) then
    Result := Modules.GetNameAndLink(Self, names, links, URL, ModuleId)
  else
  begin
    WebsiteID := GetMangaSiteID(website);
    Source := TStringList.Create;
    if WebsiteID = ANIMEA_ID then
      Result := AnimeAGetNamesAndLinks
    else
    if WebsiteID = MANGAINN_ID then
      Result := MangaInnGetNamesAndLinks
    else
    if WebsiteID = MANGA24H_ID then
      Result := Manga24hGetNamesAndLinks
    else
    if WebsiteID = VNSHARING_ID then
      Result := VnSharingGetNamesAndLinks
    else
    if WebsiteID = HENTAI2READ_ID then
      Result := Hentai2ReadGetNamesAndLinks
    else
    if WebsiteID = FAKKU_ID then
      Result := FakkuGetNamesAndLinks
    else
    if WebsiteID = MANGAPARK_ID then
      Result := MangaParkGetNamesAndLinks
    else
    if WebsiteID = MANGATRADERS_ID then
      Result := MangaTradersGetNamesAndLinks
    else
    if WebsiteID = STARKANA_ID then
      Result := StarkanaGetNamesAndLinks
    else
    if WebsiteID = EATMANGA_ID then
      Result := EatMangaGetNamesAndLinks
    else
    if WebsiteID = MANGAGO_ID then
      Result := MangaGoGetNamesAndLinks
    else
    if WebsiteID = S2SCAN_ID then
      Result := S2ScanGetNamesAndLinks
    else
    if WebsiteID = EGSCANS_ID then
      Result := EGScansGetNamesAndLinks
    else
    if WebsiteID = MANGAEDEN_ID then
      Result := MangaEdenGetNamesAndLinks(WebsiteRoots[MANGAEDEN_ID, 1])
    else
    if WebsiteID = PERVEDEN_ID then
      Result := MangaEdenGetNamesAndLinks(WebsiteRoots[PERVEDEN_ID, 1])
    else
    if WebsiteID = MEINMANGA_ID then
      Result := MeinMangaGetNamesAndLinks
    else
    if WebsiteID = BLOGTRUYEN_ID then
      Result := BlogTruyenGetNamesAndLinks
    else
    if WebsiteID = TRUYENTRANHTUAN_ID then
      Result := TruyenTranhTuanGetNamesAndLinks
    else
    if WebsiteID = SUBMANGA_ID then
      Result := SubMangaGetNamesAndLinks
    else
    if WebsiteID = ESMANGAHERE_ID then
      Result := EsMangaHereGetNamesAndLinks
    else
    if WebsiteID = ANIMEEXTREMIST_ID then
      Result := AnimeExtremistGetNamesAndLinks
    else
    if WebsiteID = KOMIKID_ID then
      Result := KomikidGetNamesAndLinks
    else
    if WebsiteID = MABUNS_ID then
      Result := MabunsGetNamesAndLinks
    else
    if WebsiteID = MANGAESTA_ID then
      Result := MangaEstaGetNamesAndLinks
    else
    if WebsiteID = HUGEMANGA_ID then
      Result := HugeMangaGetNamesAndLinks
    else
    if WebsiteID = ANIMESTORY_ID then
      Result := AnimeStoryGetNamesAndLinks
    else
    if WebsiteID = LECTUREENLIGNE_ID then
      Result := LectureEnLigneGetNamesAndLinks
    else
    if WebsiteID = SCANMANGA_ID then
      Result := ScanMangaGetNamesAndLinks
    else
    if WebsiteID = MANGAAR_ID then
      Result := MangaArGetNamesAndLinks
    else
    if WebsiteID = MANGAAE_ID then
      Result := MangaAeGetNamesAndLinks
    else
    if WebsiteID = CENTRALDEMANGAS_ID then
      Result := CentralDeMangasGetNamesAndLinks
    else
    if WebsiteID = IMANHUA_ID then
      Result := imanhuaGetNamesAndLinks
    else
    if WebsiteID = TURKCRAFT_ID then
      Result := TurkcraftGetNamesAndLinks
    else
    if WebsiteID = MANGAVADISI_ID then
      Result := MangaVadisiGetNamesAndLinks
    else
    if WebsiteID = MANGAFRAME_ID then
      Result := MangaFrameNamesAndLinks
    else
    if WebsiteID = MANGACOW_ID then
      Result := MangaCowGetNamesAndLinks
    else
    if WebsiteID = SENMANGA_ID then
      Result := SenMangaGetNamesAndLinks
    else
    if WebsiteID = KIVMANGA_ID then
      Result := KivmangaGetNamesAndLinks
    else
    if WebsiteID = MANGASPROJECT_ID then
      Result := MangasPROJECTGetNamesAndLinks
    else
    if WebsiteID = MANGAREADER_POR_ID then
      Result := MangaREADER_PORGetNamesAndLinks
    else
    if (WebsiteID = NINEMANGA_ID) or
      (WebsiteID = NINEMANGA_ES_ID) or
      (WebsiteID = NINEMANGA_CN_ID) or
      (WebsiteID = NINEMANGA_RU_ID) or
      (WebsiteID = NINEMANGA_DE_ID) or
      (WebsiteID = NINEMANGA_IT_ID) or
      (WebsiteID = NINEMANGA_BR_ID) then
      Result := NineMangaGetNamesAndLinks
    else
    if WebsiteID = JAPANSHIN_ID then
      Result := JapanShinGetNamesAndLinks
    else
    if WebsiteID = JAPSCAN_ID then
      Result := JapscanNamesAndLinks
    else
    if WebsiteID = CENTRUMMANGI_PL_ID then
      Result := CentrumMangi_PLGetNamesAndLinks
    else
    if WebsiteID = MANGALIB_PL_ID then
      Result := MangaLib_PLGetNamesAndLinks
    else
    if WebsiteID = ONEMANGA_ID then
      Result := OneMangaGetNamesAndLinks
    else
    if WebsiteID = MANGATOWN_ID then
      Result := MangaTownGetNamesAndLinks
    else
    if WebsiteID = MANGAOKU_ID then
      Result := MangaOkuGetNamesAndLinks
    else
    if WebsiteID = MYREADINGMANGAINFO_ID then
      Result := MyReadingMangaInfoNamesAndLinks
    else
    if WebsiteID = IKOMIK_ID then
      Result := IKomikNamesAndLinks
    else
    if WebsiteID = NHENTAI_ID then
      Result := NHentaiNamesAndLinks
    else
    if WebsiteID = MANGAMINT_ID then
      Result := MangaMintGetNamesAndLinks
    else
    if WebsiteID = UNIXMANGA_ID then
      Result := UnixMangaNamesAndLinks
    else
    if WebsiteID = EXTREMEMANGAS_ID then
      Result := ExtremeMangasNamesAndLinks
    else
    if WebsiteID = MANGAHOST_ID then
      Result := MangaHostGetNamesAndLinks
    else
    if (WebsiteID = PORNCOMIX_ID) or
      (WebsiteID = XXCOMICS_ID) or
      (WebsiteID = XXCOMICSMT_ID) or
      (WebsiteID = XXCOMICS3D_ID) or
      (WebsiteID = PORNCOMIXRE_ID) or
      (WebsiteID = PORNCOMIXIC_ID) or
      (WebsiteID = PORNXXXCOMICS_ID) then
      Result := PornComixGetNamesAndLinks(GetMangaSiteID(website))
    else
    if WebsiteID = MANGASEE_ID then
      Result := MangaSeeGetNamesAndLinks
    else
    if WebsiteID = MANGAKU_ID then
      Result := MangaKuGetNamesAndLinks
    else
    if WebsiteID = MANGAAT_ID then
      Result := MangaAtGetNamesAndLinks
    else
    if WebsiteID = READMANGATODAY_ID then
      Result := ReadMangaTodayGetNamesAndLinks
    else
    if WebsiteID = LONEMANGA_ID then
      Result := LoneMangaGetNamesAndLinks
    else
    if WebsiteID = DYNASTYSCANS_ID then
      Result := DynastyScansGetNamesAndLinks
    else
    begin
      Result := INFORMATION_NOT_FOUND;
      Source.Free;
    end;
  end;

  //remove host from url
  if links.Count > 0 then
    RemoveHostFromURLsPair(links, names);
end;

function TMangaInformation.GetInfoFromURL(const website, URL: String;
  const Reconnect: Integer): Byte;
var
  s, s2: String;
  j, k: Integer;
  del: Boolean;
  Source: TStringList;
  Parser: THTMLParser;
  WebsiteID: Cardinal;

  {$I includes/AnimeA/manga_information.inc}

  // due to its weird designs, this will take a lot of work (and time) for it to
  // work property

  {$I includes/SubManga/manga_information.inc}

  {$I includes/EsMangaHere/manga_information.inc}

  {$I includes/AnimExtremist/manga_information.inc}

  {$I includes/MangaInn/manga_information.inc}

  {$I includes/Manga24h/manga_information.inc}

  {$I includes/VnSharing/manga_information.inc}

  {$I includes/Hentai2Read/manga_information.inc}

  {$I includes/Fakku/manga_information.inc}

  {$I includes/MangaPark/manga_information.inc}

  {$I includes/MangaTraders/manga_information.inc}

  {$I includes/MangaEden/manga_information.inc}

  {$I includes/Starkana/manga_information.inc}

  {$I includes/EatManga/manga_information.inc}

  {$I includes/S2Scans/manga_information.inc}

  {$I includes/EGScans/manga_information.inc}

  {$I includes/MangaGo/manga_information.inc}

  {$I includes/TruyenTranhTuan/manga_information.inc}

  {$I includes/Komikid/manga_information.inc}

  {$I includes/Mabuns/manga_information.inc}

  {$I includes/MangaEsta/manga_information.inc}

  {$I includes/HugeManga/manga_information.inc}

  {$I includes/AnimeStory/manga_information.inc}

  {$I includes/LectureEnLigne/manga_information.inc}

  {$I includes/ScanManga/manga_information.inc}

  {$I includes/Turkcraft/manga_information.inc}

  {$I includes/MangaVadisi/manga_information.inc}

  {$I includes/MangaFrame/manga_information.inc}

  {$I includes/MangaAr/manga_information.inc}

  {$I includes/MangaAe/manga_information.inc}

  {$I includes/CentralDeMangas/manga_information.inc}

  {$I includes/Mangacow/manga_information.inc}

  {$I includes/SenManga/manga_information.inc}

  {$I includes/BlogTruyen/manga_information.inc}

  {$I includes/MeinManga/manga_information.inc}

  {$I includes/KivManga/manga_information.inc}

  {$I includes/MangasPROJECT/manga_information.inc}

  {$I includes/MangaREADER_POR/manga_information.inc}

  {$I includes/NineManga/manga_information.inc}

  {$I includes/JapanShin/manga_information.inc}

  {$I includes/Japscan/manga_information.inc}

  {$I includes/CentrumMangi_PL/manga_information.inc}

  {$I includes/MangaLib_PL/manga_information.inc}

  {$I includes/OneManga/manga_information.inc}

  {$I includes/MangaTown/manga_information.inc}

  {$I includes/MangaOku/manga_information.inc}

  {$I includes/MyReadingMangaInfo/manga_information.inc}

  {$I includes/IKomik/manga_information.inc}

  {$I includes/NHentai/manga_information.inc}

  {$I includes/MangaMint/manga_information.inc}

  {$I includes/UnixManga/manga_information.inc}

  {$I includes/ExtremeMangas/manga_information.inc}

  {$I includes/MangaHost/manga_information.inc}

  {$I includes/PornComix/manga_information.inc}

  {$I includes/MangaSee/manga_information.inc}

  {$I includes/MangaKu/manga_information.inc}

  {$I includes/MangaAt/manga_information.inc}

  {$I includes/ReadMangaToday/manga_information.inc}

  {$I includes/LoneManga/manga_information.inc}

  {$I includes/Dynasty-Scans/manga_information.inc}

begin
  if Trim(URL) = '' then
    Exit(INFORMATION_NOT_FOUND);

  //load User-Agent from INIAdvanced
  AdvanceLoadHTTPConfig(FHTTP, website);

  mangaInfo.website := website;
  if mangaInfo.link = '' then mangaInfo.link := URL;
  mangaInfo.coverLink := '';
  mangaInfo.numChapter := 0;
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;

  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(website);
  if Modules.ModuleAvailable(ModuleId, MMGetInfo) then begin
    mangaInfo.url := FillHost(Modules.Module[ModuleId].RootURL, URL);
    Result := Modules.GetInfo(Self, URL, ModuleId)
  end
  else
  begin
    WebsiteID := GetMangaSiteID(website);
    if WebsiteID > High(WebsiteRoots) then
      Exit(INFORMATION_NOT_FOUND);
    mangaInfo.url := FillMangaSiteHost(WebsiteID, URL);
    Source := TStringList.Create;
    if WebsiteID = ANIMEA_ID then
      Result := GetAnimeAInfoFromURL
    else
    if WebsiteID = MANGAINN_ID then
      Result := GetMangaInnInfoFromURL
    else
    if WebsiteID = MANGA24H_ID then
      Result := GetManga24hInfoFromURL
    else
    if WebsiteID = VNSHARING_ID then
      Result := GetVnSharingInfoFromURL
    else
    if WebsiteID = HENTAI2READ_ID then
      Result := GetHentai2ReadInfoFromURL
    else
    if WebsiteID = FAKKU_ID then
      Result := GetFakkuInfoFromURL
    else
    if WebsiteID = MANGAPARK_ID then
      Result := GetMangaParkInfoFromURL
    else
    if WebsiteID = MANGATRADERS_ID then
      Result := GetMangaTradersInfoFromURL
    else
    if WebsiteID = STARKANA_ID then
      Result := GetStarkanaInfoFromURL
    else
    if WebsiteID = EATMANGA_ID then
      Result := GetEatMangaInfoFromURL
    else
    if WebsiteID = MANGAGO_ID then
      Result := GetMangaGoInfoFromURL
    else
    if WebsiteID = S2SCAN_ID then
      Result := GetS2scanInfoFromURL
    else
    if WebsiteID = EGSCANS_ID then
      Result := GetEGScansInfoFromURL
    else
    if WebsiteID = TRUYENTRANHTUAN_ID then
      Result := GetTruyenTranhTuanInfoFromURL
    else
    if WebsiteID = MANGAEDEN_ID then
      Result := GetMangaEdenInfoFromURL(WebsiteRoots[MANGAEDEN_ID, 1])
    else
    if WebsiteID = PERVEDEN_ID then
      Result := GetMangaEdenInfoFromURL(WebsiteRoots[PERVEDEN_ID, 1])
    else
    if WebsiteID = MEINMANGA_ID then
      Result := GetMeinMangaInfoFromURL
    else
    if WebsiteID = SUBMANGA_ID then
      Result := GetSubMangaInfoFromURL
    else
    if WebsiteID = ESMANGAHERE_ID then
      Result := GetEsMangaHereInfoFromURL
    else
    if WebsiteID = ANIMEEXTREMIST_ID then
      Result := GetAnimeExtremistInfoFromURL
    else
    if WebsiteID = KOMIKID_ID then
      Result := GetKomikidInfoFromURL
    else
    if WebsiteID = MABUNS_ID then
      Result := GetMabunsInfoFromURL
    else
    if WebsiteID = MANGAESTA_ID then
      Result := GetMangaEstaInfoFromURL
    else
    if WebsiteID = HUGEMANGA_ID then
      Result := GetHugeMangaInfoFromURL
    else
    if WebsiteID = ANIMESTORY_ID then
      Result := GetAnimeStoryInfoFromURL
    else
    if WebsiteID = LECTUREENLIGNE_ID then
      Result := GetLectureEnLigneInfoFromURL
    else
    if WebsiteID = SCANMANGA_ID then
      Result := GetScanMangaInfoFromURL
    else
    if WebsiteID = TURKCRAFT_ID then
      Result := GetTurkcraftInfoFromURL
    else
    if WebsiteID = MANGAFRAME_ID then
      Result := GetMangaframeInfoFromURL
    else
    if WebsiteID = MANGAVADISI_ID then
      Result := GetMangaVadisiInfoFromURL
    else
    if WebsiteID = MANGAAR_ID then
      Result := GetMangaArInfoFromURL
    else
    if WebsiteID = MANGAAE_ID then
      Result := GetMangaAeInfoFromURL
    else
    if WebsiteID = CENTRALDEMANGAS_ID then
      Result := GetCentralDeMangasInfoFromURL
    else
    if WebsiteID = MANGACOW_ID then
      Result := GetMangaCowInfoFromURL
    else
    if WebsiteID = SENMANGA_ID then
      Result := GetSenMangaInfoFromURL
    else
    if WebsiteID = BLOGTRUYEN_ID then
      Result := GetBlogTruyenInfoFromURL
    else
    if WebsiteID = KIVMANGA_ID then
      Result := GetKivmangaInfoFromURL
    else
    if WebsiteID = MANGASPROJECT_ID then
      Result := GetMangasPROJECTInfoFromURL
    else
    if WebsiteID = MANGAREADER_POR_ID then
      Result := GetMangaREADER_PORInfoFromURL
    else
    if (WebsiteID = NINEMANGA_ID) or
      (WebsiteID = NINEMANGA_ES_ID) or
      (WebsiteID = NINEMANGA_CN_ID) or
      (WebsiteID = NINEMANGA_RU_ID) or
      (WebsiteID = NINEMANGA_DE_ID) or
      (WebsiteID = NINEMANGA_IT_ID) or
      (WebsiteID = NINEMANGA_BR_ID) then
      Result := GetNineMangaInfoFromURL
    else
    if WebsiteID = JAPANSHIN_ID then
      Result := GetJapanShinInfoFromURL
    else
    if WebsiteID = JAPSCAN_ID then
      Result := GetJapscanInfoFromURL
    else
    if WebsiteID = CENTRUMMANGI_PL_ID then
      Result := GetCentrumMangi_PLInfoFromURL
    else
    if WebsiteID = MANGALIB_PL_ID then
      Result := GetMangaLib_PLInfoFromURL
    else
    if WebsiteID = ONEMANGA_ID then
      Result := GetOneMangaInfoFromURL
    else
    if WebsiteID = MANGATOWN_ID then
      Result := GetMangaTownInfoFromURL
    else
    if WebsiteID = MANGAOKU_ID then
      Result := GetMangaOkuInfoFromURL
    else
    if WebsiteID = MYREADINGMANGAINFO_ID then
      Result := GetMyReadingMangaInfoInfoFromURL
    else
    if WebsiteID = IKOMIK_ID then
      Result := GetIKomikInfoFromURL
    else
    if WebsiteID = NHENTAI_ID then
      Result := GetNHentaiInfoFromURL
    else
    if WebsiteID = MANGAMINT_ID then
      Result := GetMangaMintInfoFromURL
    else
    if WebsiteID = UNIXMANGA_ID then
      Result := GetUnixMangaInfoFromURL
    else
    if WebsiteID = EXTREMEMANGAS_ID then
      Result := GetExtremeMangasInfoFromURL
    else
    if WebsiteID = MANGAHOST_ID then
      Result := GetMangaHostInfoFromURL
    else
    if (WebsiteID = PORNCOMIX_ID) or
      (WebsiteID = XXCOMICS_ID) or
      (WebsiteID = XXCOMICSMT_ID) or
      (WebsiteID = XXCOMICS3D_ID) or
      (WebsiteID = PORNCOMIXRE_ID) or
      (WebsiteID = PORNCOMIXIC_ID) or
      (WebsiteID = PORNXXXCOMICS_ID) then
      Result := GetPornComixInfoFromURL(GetMangaSiteID(website))
    else
    if WebsiteID = MANGASEE_ID then
      Result := GetMangaSeeInfoFromURL
    else
    if WebsiteID = MANGAKU_ID then
      Result := GetMangaKuInfoFromURL
    else
    if WebsiteID = MANGAAT_ID then
      Result := GetMangaAtInfoFromURL
    else
    if WebsiteID = READMANGATODAY_ID then
      Result := GetReadMangaTodayInfoFromURL
    else
    if WebsiteID = LONEMANGA_ID then
      Result := GetLoneMangaInfoFromURL
    else
    if WebsiteID = DYNASTYSCANS_ID then
      Result := GetDynastyScansInfoFromURL
    else
    begin
      Source.Free;
      Result := INFORMATION_NOT_FOUND;
      Exit;
    end;
  end;

  with mangaInfo do begin
    s := artists;
    if (s <> '') and (s[1] = '<') then
      artists := '';
    s := authors;
    if (s <> '') and (s[1] = '<') then
      authors := '';

    // check everything once more
    title := Trim(RemoveStringBreaks(CommonStringFilter(title)));
    authors := Trim(RemoveStringBreaks(Trim(authors)));
    artists := Trim(RemoveStringBreaks(Trim(artists)));
    genres := Trim(RemoveStringBreaks(Trim(genres)));

    authors := TrimRightChar(Trim(authors), [',']);
    artists := TrimRightChar(Trim(artists), [',']);
    genres := TrimRightChar(Trim(genres), [',']);

    summary := CleanMultilinedString(summary);

    // fix info
    if (authors = '-') or (authors = ':') then
      authors := '';
    if (artists = '-') or (artists = ':') then
      artists := '';
    if (summary = '-') or (summary = ':') then
      summary := '';

    // cleanup chapters
    if chapterLinks.Count>0 then begin
      while chapterName.Count<chapterLinks.Count do
        chapterName.Add('');
      while chapterLinks.Count<chapterName.Count do
        chapterName.Delete(chapterName.Count-1);
      for j:=0 to chapterLinks.Count-1 do begin
        chapterLinks[j]:=Trim(chapterLinks[j]);
        chapterName[j]:=Trim(chapterName[j]);
      end;
    end;

    // remove duplicate chapter
    if chapterLinks.Count > 0 then
    begin
      j := 0;
      while j < (chapterLinks.Count - 1) do
      begin
        del := False;
        if (j + 1) < chapterLinks.Count then
          for k := j + 1 to chapterLinks.Count - 1 do
          begin
            if SameText(chapterLinks[j], chapterLinks[k]) then
            begin
              chapterLinks.Delete(j);
              chapterName.Delete(j);
              del := True;
              Break;
            end;
          end;
        if not del then
          Inc(j);
      end;
    end;

    if chapterLinks.Count > 0 then
    begin
      // remove host from chapter links
      RemoveHostFromURLsPair(chapterLinks, chapterName);
      // fixing chapter name
      for j := 0 to chapterName.Count - 1 do
      begin
        chapterName.Strings[j] := Trim(RemoveStringBreaks(
          CommonStringFilter(chapterName[j])));
      end;

      //remove manga name from chapter
      if OptionRemoveMangaNameFromChapter and (title <> '') then
      begin
        s := LowerCase(title);
        j := Length(s);
        for k := 0 to chapterName.Count - 1 do begin
          s2 := LowerCase(chapterName[k]);
          if Length(s2) > j then
            if Pos(s, s2) = 1 then begin
              s2 := chapterName[k];
              Delete(s2, 1, j);
              chapterName[k] := Trim(s2);
            end;
        end;
      end;
    end;

    numChapter := chapterLinks.Count;
  end;
end;

procedure TMangaInformation.SyncMinorInfoToData(const DataProcess: TDataProcess;
  const index: Cardinal);
begin
  // sync info to data
  {$IFDEF DOWNLOADER}
  if not dataProcess.isFilterAllSites then
  {$ENDIF}
  begin
    DataProcess.Data.Strings[index] := SetParams(
      [DataProcess.Param[index, DATA_PARAM_TITLE],
      DataProcess.Param[index, DATA_PARAM_LINK],
      DataProcess.Param[index, DATA_PARAM_AUTHORS],
      DataProcess.Param[index, DATA_PARAM_ARTISTS],
      DataProcess.Param[index, DATA_PARAM_GENRES],
      mangaInfo.status,
      DataProcess.Param[index, DATA_PARAM_SUMMARY],
      IntToStr(mangaInfo.numChapter),
      {$IFDEF DOWNLOADER}
      DataProcess.Param[index, DATA_PARAM_JDN],
      {$ELSE}
      '0',
      {$ENDIF}
      '0']);
  end;
  // then break it into parts
  dataProcess.BreakDataToParts(index);
end;

procedure TMangaInformation.SyncInfoToData(const DataProcess: TDataProcess;
  const index: Cardinal);
begin
  // sync info to data
  {$IFDEF DOWNLOADER}
  if not dataProcess.isFilterAllSites then
  {$ENDIF}
  begin
    if Trim(mangaInfo.title) = '' then
      mangaInfo.title := DataProcess.Param[index, DATA_PARAM_TITLE];
    DataProcess.Data.Strings[index] := SetParams(
      //[DataProcess.Param[index, DATA_PARAM_TITLE],
      //sync title as well, some site possible to change title or when mangainfo script not work
      [mangaInfo.title,
      DataProcess.Param[index, DATA_PARAM_LINK],
      mangaInfo.authors,
      mangaInfo.artists,
      mangaInfo.genres,
      mangaInfo.status,
      StringFilter(mangaInfo.summary),
      IntToStr(mangaInfo.numChapter),
      {$IFDEF DOWNLOADER}
      DataProcess.Param[index, DATA_PARAM_JDN],
      {$ELSE}
      '0',
      {$ENDIF}
      '0']);
  end;
  // then break it into parts
  dataProcess.BreakDataToParts(index);
end;

procedure TMangaInformation.SyncInfoToData(const DataProcess: TDBDataProcess);
begin
  if Assigned(DataProcess) then
    with mangaInfo do
      DataProcess.UpdateData(title, link, authors, artists, genres, status, summary,
        numChapter, website);
end;

procedure TMangaInformation.AddInfoToDataWithoutBreak(const Name, link: String;
  const DataProcess: TDataProcess);
var
  S: String;
begin
  if mangaInfo.title <> '' then
    S := mangaInfo.title
  else
    S := Name;

  DataProcess.Data.Add(RemoveStringBreaks(SetParams([
    S,
    link,
    mangaInfo.authors,
    mangaInfo.artists,
    mangaInfo.genres,
    mangaInfo.status,
    StringFilter(mangaInfo.summary),
    IntToStr(mangaInfo.numChapter),
    {$IFDEF DOWNLOADER}
    IntToStr(GetCurrentJDN),
    {$ELSE}
    '0',
    {$ENDIF}
    '0'
    ])));
end;

procedure TMangaInformation.AddInfoToData(const Name, link: String;
  const DataProcess: TDataProcess);
var
  l: TStringList;
begin
  l := TStringList.Create;
  DataProcess.Data.Add(
    RemoveStringBreaks(
    SetParams(
    [Name,
    link,
    mangaInfo.authors,
    mangaInfo.artists,
    mangaInfo.genres,
    mangaInfo.status,
    StringFilter(mangaInfo.summary),
    IntToStr(mangaInfo.numChapter),
    IntToStr(GetCurrentJDN),
    '0'])));
  GetParams(l, DataProcess.Data.Strings[DataProcess.Data.Count - 1]);
  DataProcess.title.Add(l.Strings[DATA_PARAM_TITLE]);
  DataProcess.link.Add(l.Strings[DATA_PARAM_LINK]);
  DataProcess.authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
  DataProcess.artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
  DataProcess.genres.Add(l.Strings[DATA_PARAM_GENRES]);
  DataProcess.status.Add(l.Strings[DATA_PARAM_STATUS]);
  DataProcess.summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
  {$IFDEF DOWNLOADER}
  DataProcess.jdn.Add(Pointer(StrToInt(l.Strings[DATA_PARAM_JDN])));
  {$ELSE}
  DataProcess.jdn.Add(Pointer(StrToInt('0')));
  {$ENDIF}
  l.Free;
end;

procedure TMangaInformation.AddInfoToData(const Title, Link: String;
  const DataProcess: TDBDataProcess);
begin
  if Assigned(DataProcess) then
  begin
    if (mangaInfo.title = '') and (Title <> '') then mangaInfo.title := Title;
    if (mangaInfo.link = '') and (Link <> '') then mangaInfo.link := Link;
    with mangaInfo do
      DataProcess.AddData(title, link, authors, artists, genres, status,
        StringBreaks(summary), numChapter, Now);
  end;
end;

function TMangaInformation.GetPage(var output: TObject; URL: String;
  const Reconnect: Integer): Boolean;
begin
  Result := uBaseUnit.GetPage(FHTTP, output, URL, Reconnect);
end;

end.
