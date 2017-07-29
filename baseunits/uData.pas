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
  Classes, SysUtils, uBaseUnit, DBDataProcess, FMDOptions, httpsendthread,
  BaseThread, LazFileUtils, strutils, RegExpr, httpsend, MultiLog;

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
  private
    FOwner: TBaseThread;
  public
    isGetByUpdater: Boolean;
    mangaInfo: TMangaInfo;
    parse: TStringList;
    isGenerateFolderChapterName: Boolean;
    isRemoveUnicode: Boolean;
    RemoveHostFromChapterLinks: Boolean;
    FHTTP: THTTPSendThread;
    ModuleId: Integer;

    procedure OnTag(NoCaseTag, ActualTag: String);
    procedure OnText(AText: String);
    constructor Create(AOwnerThread: TBaseThread = nil; ACreateInfo: Boolean = True);
    destructor Destroy; override;
    procedure ClearInfo;
    function GetDirectoryPage(var APage: Integer; const AWebsite: String): Byte;
    function GetNameAndLink(const ANames, ALinks: TStringList; const AWebsite, AURL: String): Byte;
    function GetInfoFromURL(const AWebsite, AURL: String; const AReconnect: Integer = 0): Byte;
    procedure SyncInfoToData(const ADataProcess: TDataProcess; const AIndex: Cardinal); overload;
    procedure SyncInfoToData(const ADataProcess: TDBDataProcess); overload;
    procedure SyncMinorInfoToData(const ADataProcess: TDataProcess; const AIndex: Cardinal);

    // Only use this function for getting manga infos for the first time
    procedure AddInfoToDataWithoutBreak(const AName, ALink: String; const ADataProcess: TDataProcess);
    // Only use this function for update manga list
    procedure AddInfoToData(const AName, Alink: String; const ADataProcess: TDataProcess);
      overload;
    // to add data to TDBDataProcess
    procedure AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess); overload;
    //wrapper
    function GetPage(var AOutput: TObject; AURL: String; const AReconnect: Integer = 0): Boolean; inline;
    property Thread: TBaseThread read FOwner;
  end;

var
  options: TStringList;

implementation

uses
  Dialogs, fpJSON, JSONParser, jsonscanner, FastHTMLParser, HTMLUtil,
  SynaCode, frmMain, WebsiteModules, uUpdateThread;

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
  Filename := DATA_FOLDER + website;

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
          site.Add(id);
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
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String; const minusDay: Cardinal;
  const haveAllChecked, searchNewManga: Boolean): Boolean;
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
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String; const minusDay: Cardinal;
  const haveAllChecked, searchNewManga: Boolean; useRegExpr: Boolean = False): Boolean;
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
        if (currentJDN - {%H-}Integer(jdn.Items[fpos]) >= minusDay) and
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
          if filterMark.Items[fpos] = FILTER_SHOW then
            filterMark.Items[fpos] := FILTER_HIDE;
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
            for j := 0 to uncheckedGenres.Count - 1 do
              if Pos((uncheckedGenres.Strings[j] + ','), s) <> 0 then
              begin
                filterMark.Items[fpos] := FILTER_HIDE;
                Break;
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
      Logger.SendException(Self.ClassName + '.Filter.Error!', E);
  end;
  regx.Free;
end;

function TDataProcess.Search(AMangaName: String): Boolean;
var
  i: Cardinal;
begin
  Result := False;
  searchPos.Clear;
  if filterPos.Count <= 0 then
    Exit;
  AMangaName := Upcase(AMangaName);
  for i := 0 to filterPos.Count - 1 do
    if Pos(AMangaName, upcase(Title.Strings[filterPos.Items[i]])) > 0 then
    begin
      searchPos.Add(filterPos.Items[i]);
      Result := True;
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
    for i := 0 to Data.Count - 1 do
    begin
      filterMark.Add(FILTER_SHOW);
      filterPos.Add(i);
    end;
  isFiltered := False;
end;

procedure TDataProcess.Sort;
begin
  //QuickSortData(data);
  QuickSortNaturalPart(Data, SEPERATOR, DATA_PARAM_TITLE);
end;

{ TMangaInformation }

constructor TMangaInformation.Create(AOwnerThread: TBaseThread; ACreateInfo: Boolean);
begin
  inherited Create;
  FOwner := AOwnerThread;
  FHTTP := THTTPSendThread.Create(AOwnerThread);
  FHTTP.Headers.NameValueSeparator := ':';
  parse := TStringList.Create;
  if ACreateInfo then
    mangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
  ModuleId := -1;
  RemoveHostFromChapterLinks := True;
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

procedure TMangaInformation.OnText(AText: String);
begin
  parse.Add(AText);
end;

function TMangaInformation.GetDirectoryPage(var APage: Integer; const AWebsite: String): Byte;
var
  s: String;
  p: Integer;
  Source: TStringList;
  Parser: THTMLParser;
  MangaSiteID: Integer;

  {$I includes/AnimeA/directory_page_number.inc}

  {$I includes/VnSharing/directory_page_number.inc}

  {$I includes/S2Scans/directory_page_number.inc}

  {$I includes/LectureEnLigne/directory_page_number.inc}

  {$I includes/CentralDeMangas/directory_page_number.inc}

  {$I includes/DM5/directory_page_number.inc}

  {$I includes/JapanShin/directory_page_number.inc}

  {$I includes/OneManga/directory_page_number.inc}

  {$I includes/MangaTown/directory_page_number.inc}

  {$I includes/IKomik/directory_page_number.inc}

  {$I includes/NHentai/directory_page_number.inc}

  {$I includes/MangaHost/directory_page_number.inc}

  {$I includes/MangaAt/directory_page_number.inc}

  {$I includes/Dynasty-Scans/directory_page_number.inc}

begin
  APage := 0;

  //load User-Agent from advancedfile
  AdvanceLoadHTTPConfig(FHTTP, AWebsite);

  //load pagenumber_config if available
  p := advancedfile.ReadInteger('UpdateListDirectoryPageNumber', AWebsite, -1);

  if p > 0 then
  begin
    APage := p;
    BROWSER_INVERT := True;
  end
  else
  begin
    BROWSER_INVERT := False;
    if ModuleId < 0 then
      ModuleId := Modules.LocateModule(AWebsite);
    if Modules.ModuleAvailable(ModuleId, MMGetDirectoryPageNumber) then
      Result := Modules.GetDirectoryPageNumber(Self, APage, TUpdateListThread(Thread).workPtr, ModuleId)
    else
    begin
      MangaSiteID := GetMangaSiteID(AWebsite);
      Source := TStringList.Create;
      if MangaSiteID = ANIMEA_ID then
        Result := GetAnimeADirectoryPageNumber
      else
      if MangaSiteID = VNSHARING_ID then
        Result := GetVnSharingDirectoryPageNumber
      else
      if MangaSiteID = S2SCAN_ID then
        Result := GetS2ScanDirectoryPageNumber
      else
      if MangaSiteID = LECTUREENLIGNE_ID then
        Result := GetLectureEnLigneDirectoryPageNumber
      else
      if MangaSiteID = CENTRALDEMANGAS_ID then
        Result := GetCentralDeMangasDirectoryPageNumber
      else
      if MangaSiteID = DM5_ID then
        Result := GetDM5DirectoryPageNumber
      else
      if MangaSiteID = JAPANSHIN_ID then
        Result := GetJapanShinDirectoryPageNumber
      else
      if MangaSiteID = ONEMANGA_ID then
        Result := GetOneMangaDirectoryPageNumber
      else
      if MangaSiteID = MANGATOWN_ID then
        Result := GetMangaTownDirectoryPageNumber
      else
      if MangaSiteID = IKOMIK_ID then
        Result := GetIKomikDirectoryPageNumber
      else
      if MangaSiteID = NHENTAI_ID then
        Result := GetNHentaiDirectoryPageNumber
      else
      if MangaSiteID = MANGAHOST_ID then
        Result := GetMangaHostDirectoryPageNumber
      else
      if MangaSiteID = MANGAAT_ID then
        Result := GetMangaAtDirectoryPageNumber
      else
      if MangaSiteID = DYNASTYSCANS_ID then
        Result := GetDynastyScansDirectoryPageNumber
      else
      begin
        Result := NO_ERROR;
        APage := 1;
        Source.Free;
      end;
    end;

    if APage < 1 then
      APage := 1;
  end;
end;

function TMangaInformation.GetNameAndLink(const ANames, ALinks: TStringList;
  const AWebsite, AURL: String): Byte;
var
  Source: TStringList;
  Parser: THTMLParser;
  MangaSiteID: Integer;

  {$I includes/AnimeA/names_and_links.inc}

  {$I includes/EsMangaHere/names_and_links.inc}

  {$I includes/AnimExtremist/names_and_links.inc}

  {$I includes/VnSharing/names_and_links.inc}

  {$I includes/AnimeStory/names_and_links.inc}

  {$I includes/LectureEnLigne/names_and_links.inc}

  {$I includes/ScanManga/names_and_links.inc}

  {$I includes/MangaAr/names_and_links.inc}

  {$I includes/CentralDeMangas/names_and_links.inc}

  {$I includes/Imanhua/names_and_links.inc}

  {$I includes/Turkcraft/names_and_links.inc}

  {$I includes/Starkana/names_and_links.inc}

  {$I includes/S2Scans/names_and_links.inc}

  {$I includes/EGScans/names_and_links.inc}

  {$I includes/Kivmanga/names_and_links.inc}

  {$I includes/MeinManga/names_and_links.inc}

  {$I includes/MangasPROJECT/names_and_links.inc}

  {$I includes/MangaREADER_POR/names_and_links.inc}

  {$I includes/JapanShin/names_and_links.inc}

  {$I includes/CentrumMangi_PL/names_and_links.inc}

  {$I includes/MangaLib_PL/names_and_links.inc}

  {$I includes/OneManga/names_and_links.inc}

 {$I includes/MangaTown/names_and_links.inc}

  {$I includes/MangaOku/names_and_links.inc}

  {$I includes/IKomik/names_and_links.inc}

  {$I includes/NHentai/names_and_links.inc}

  {$I includes/UnixManga/names_and_links.inc}

  {$I includes/ExtremeMangas/names_and_links.inc}

  {$I includes/MangaHost/names_and_links.inc}

  {$I includes/MangaKu/names_and_links.inc}

  {$I includes/MangaAt/names_and_links.inc}

  {$I includes/Dynasty-Scans/names_and_links.inc}

begin
  //load User-Agent from advancedfile
  AdvanceLoadHTTPConfig(FHTTP, AWebsite);

  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(AWebsite);
  if Modules.ModuleAvailable(ModuleId, MMGetNameAndLink) then
    Result := Modules.GetNameAndLink(Self, ANames, ALinks, AURL, ModuleId)
  else
  begin
    MangaSiteID := GetMangaSiteID(AWebsite);
    Source := TStringList.Create;
    if MangaSiteID = ANIMEA_ID then
      Result := AnimeAGetNamesAndLinks
    else
    if MangaSiteID = VNSHARING_ID then
      Result := VnSharingGetNamesAndLinks
    else
    if MangaSiteID = STARKANA_ID then
      Result := StarkanaGetNamesAndLinks
    else
    if MangaSiteID = S2SCAN_ID then
      Result := S2ScanGetNamesAndLinks
    else
    if MangaSiteID = EGSCANS_ID then
      Result := EGScansGetNamesAndLinks
    else
    if MangaSiteID = MEINMANGA_ID then
      Result := MeinMangaGetNamesAndLinks
    else
    if MangaSiteID = ESMANGAHERE_ID then
      Result := EsMangaHereGetNamesAndLinks
    else
    if MangaSiteID = ANIMEEXTREMIST_ID then
      Result := AnimeExtremistGetNamesAndLinks
    else
    if MangaSiteID = ANIMESTORY_ID then
      Result := AnimeStoryGetNamesAndLinks
    else
    if MangaSiteID = LECTUREENLIGNE_ID then
      Result := LectureEnLigneGetNamesAndLinks
    else
    if MangaSiteID = SCANMANGA_ID then
      Result := ScanMangaGetNamesAndLinks
    else
    if MangaSiteID = MANGAAR_ID then
      Result := MangaArGetNamesAndLinks
    else
    if MangaSiteID = CENTRALDEMANGAS_ID then
      Result := CentralDeMangasGetNamesAndLinks
    else
    if MangaSiteID = IMANHUA_ID then
      Result := imanhuaGetNamesAndLinks
    else
    if MangaSiteID = TURKCRAFT_ID then
      Result := TurkcraftGetNamesAndLinks
    else
    if MangaSiteID = KIVMANGA_ID then
      Result := KivmangaGetNamesAndLinks
    else
    if MangaSiteID = MANGASPROJECT_ID then
      Result := MangasPROJECTGetNamesAndLinks
    else
    if MangaSiteID = MANGAREADER_POR_ID then
      Result := MangaREADER_PORGetNamesAndLinks
    else
    if MangaSiteID = JAPANSHIN_ID then
      Result := JapanShinGetNamesAndLinks
    else
    if MangaSiteID = CENTRUMMANGI_PL_ID then
      Result := CentrumMangi_PLGetNamesAndLinks
    else
    if MangaSiteID = MANGALIB_PL_ID then
      Result := MangaLib_PLGetNamesAndLinks
    else
    if MangaSiteID = ONEMANGA_ID then
      Result := OneMangaGetNamesAndLinks
    else
   if MangaSiteID = MANGATOWN_ID then
      Result := MangaTownGetNamesAndLinks
    else
    if MangaSiteID = MANGAOKU_ID then
      Result := MangaOkuGetNamesAndLinks
    else
    if MangaSiteID = IKOMIK_ID then
      Result := IKomikNamesAndLinks
    else
    if MangaSiteID = NHENTAI_ID then
      Result := NHentaiNamesAndLinks
    else
    if MangaSiteID = UNIXMANGA_ID then
      Result := UnixMangaNamesAndLinks
    else
    if MangaSiteID = EXTREMEMANGAS_ID then
      Result := ExtremeMangasNamesAndLinks
    else
    if MangaSiteID = MANGAHOST_ID then
      Result := MangaHostGetNamesAndLinks
    else
    if MangaSiteID = MANGAKU_ID then
      Result := MangaKuGetNamesAndLinks
    else
    if MangaSiteID = MANGAAT_ID then
      Result := MangaAtGetNamesAndLinks
    else
    if MangaSiteID = DYNASTYSCANS_ID then
      Result := DynastyScansGetNamesAndLinks
    else
    begin
      Result := INFORMATION_NOT_FOUND;
      Source.Free;
    end;
  end;

  //remove host from AURL
  if ALinks.Count > 0 then
    RemoveHostFromURLsPair(ALinks, ANames);
end;

function TMangaInformation.GetInfoFromURL(const AWebsite, AURL: String; const AReconnect: Integer): Byte;
var
  s, s2: String;
  j, k: Integer;
  del: Boolean;
  Source: TStringList;
  Parser: THTMLParser;
  MangaSiteID: Integer;

  {$I includes/AnimeA/manga_information.inc}

  // due to its weird designs, this will take a lot of work (and time) for it to
  // work property

  {$I includes/EsMangaHere/manga_information.inc}

  {$I includes/AnimExtremist/manga_information.inc}

  {$I includes/VnSharing/manga_information.inc}

  {$I includes/Starkana/manga_information.inc}

  {$I includes/S2Scans/manga_information.inc}

  {$I includes/EGScans/manga_information.inc}

  {$I includes/AnimeStory/manga_information.inc}

  {$I includes/LectureEnLigne/manga_information.inc}

  {$I includes/ScanManga/manga_information.inc}

  {$I includes/Turkcraft/manga_information.inc}

  {$I includes/MangaAr/manga_information.inc}

  {$I includes/CentralDeMangas/manga_information.inc}

  {$I includes/MeinManga/manga_information.inc}

  {$I includes/KivManga/manga_information.inc}

  {$I includes/MangasPROJECT/manga_information.inc}

  {$I includes/MangaREADER_POR/manga_information.inc}

  {$I includes/JapanShin/manga_information.inc}

  {$I includes/CentrumMangi_PL/manga_information.inc}

  {$I includes/MangaLib_PL/manga_information.inc}

  {$I includes/OneManga/manga_information.inc}

 {$I includes/MangaTown/manga_information.inc}

  {$I includes/MangaOku/manga_information.inc}

  {$I includes/IKomik/manga_information.inc}

  {$I includes/NHentai/manga_information.inc}

  {$I includes/UnixManga/manga_information.inc}

  {$I includes/ExtremeMangas/manga_information.inc}

  {$I includes/MangaHost/manga_information.inc}

  {$I includes/MangaKu/manga_information.inc}

  {$I includes/MangaAt/manga_information.inc}

  {$I includes/Dynasty-Scans/manga_information.inc}

begin
  if Trim(AURL) = '' then
    Exit(INFORMATION_NOT_FOUND);

  //load User-Agent from advancedfile
  AdvanceLoadHTTPConfig(FHTTP, AWebsite);

  mangaInfo.website := AWebsite;
  mangaInfo.coverLink := '';
  mangaInfo.numChapter := 0;
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;

  if ModuleId < 0 then
    ModuleId := Modules.LocateModule(AWebsite);
  if Modules.ModuleAvailable(ModuleId, MMGetInfo) then begin
    mangaInfo.url := FillHost(Modules.Module[ModuleId].RootURL, AURL);
    Result := Modules.GetInfo(Self, AURL, ModuleId);
  end
  else
  begin
    MangaSiteID := GetMangaSiteID(AWebsite);
    if MangaSiteID > High(WebsiteRoots) then
      Exit(INFORMATION_NOT_FOUND);
    mangaInfo.url := FillMangaSiteHost(MangaSiteID, AURL);
    Source := TStringList.Create;
    if MangaSiteID = ANIMEA_ID then
      Result := GetAnimeAInfoFromURL
    else
    if MangaSiteID = VNSHARING_ID then
      Result := GetVnSharingInfoFromURL
    else
    if MangaSiteID = STARKANA_ID then
      Result := GetStarkanaInfoFromURL
    else
    if MangaSiteID = S2SCAN_ID then
      Result := GetS2scanInfoFromURL
    else
    if MangaSiteID = EGSCANS_ID then
      Result := GetEGScansInfoFromURL
    else
    if MangaSiteID = MEINMANGA_ID then
      Result := GetMeinMangaInfoFromURL
    else
    if MangaSiteID = ESMANGAHERE_ID then
      Result := GetEsMangaHereInfoFromURL
    else
    if MangaSiteID = ANIMEEXTREMIST_ID then
      Result := GetAnimeExtremistInfoFromURL
    else
    if MangaSiteID = ANIMESTORY_ID then
      Result := GetAnimeStoryInfoFromURL
    else
    if MangaSiteID = LECTUREENLIGNE_ID then
      Result := GetLectureEnLigneInfoFromURL
    else
    if MangaSiteID = SCANMANGA_ID then
      Result := GetScanMangaInfoFromURL
    else
    if MangaSiteID = TURKCRAFT_ID then
      Result := GetTurkcraftInfoFromURL
    else
    if MangaSiteID = MANGAAR_ID then
      Result := GetMangaArInfoFromURL
    else
    if MangaSiteID = CENTRALDEMANGAS_ID then
      Result := GetCentralDeMangasInfoFromURL
    else
    if MangaSiteID = KIVMANGA_ID then
      Result := GetKivmangaInfoFromURL
    else
    if MangaSiteID = MANGASPROJECT_ID then
      Result := GetMangasPROJECTInfoFromURL
    else
    if MangaSiteID = MANGAREADER_POR_ID then
      Result := GetMangaREADER_PORInfoFromURL
    else
    if MangaSiteID = JAPANSHIN_ID then
      Result := GetJapanShinInfoFromURL
    else
    if MangaSiteID = CENTRUMMANGI_PL_ID then
      Result := GetCentrumMangi_PLInfoFromURL
    else
    if MangaSiteID = MANGALIB_PL_ID then
      Result := GetMangaLib_PLInfoFromURL
    else
    if MangaSiteID = ONEMANGA_ID then
      Result := GetOneMangaInfoFromURL
    else
    if MangaSiteID = MANGATOWN_ID then
      Result := GetMangaTownInfoFromURL
    else
    if MangaSiteID = MANGAOKU_ID then
      Result := GetMangaOkuInfoFromURL
    else
    if MangaSiteID = IKOMIK_ID then
      Result := GetIKomikInfoFromURL
    else
    if MangaSiteID = NHENTAI_ID then
      Result := GetNHentaiInfoFromURL
    else
    if MangaSiteID = UNIXMANGA_ID then
      Result := GetUnixMangaInfoFromURL
    else
    if MangaSiteID = EXTREMEMANGAS_ID then
      Result := GetExtremeMangasInfoFromURL
    else
    if MangaSiteID = MANGAHOST_ID then
      Result := GetMangaHostInfoFromURL
    else
    if MangaSiteID = MANGAKU_ID then
      Result := GetMangaKuInfoFromURL
    else
    if MangaSiteID = MANGAAT_ID then
      Result := GetMangaAtInfoFromURL
    else
    if MangaSiteID = DYNASTYSCANS_ID then
      Result := GetDynastyScansInfoFromURL
    else
    begin
      Source.Free;
      Result := INFORMATION_NOT_FOUND;
      Exit;
    end;
  end;

  with mangaInfo do begin
    if link = '' then
      link := RemoveHostFromURL(mangaInfo.url);

    // cleanup info
    coverLink := CleanURL(coverLink);
    title := Trim(FixWhiteSpace(RemoveStringBreaks(CommonStringFilter(title))));
    authors := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(authors))));
    artists := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(artists))));
    genres := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(genres))));

    authors := TrimRightChar(Trim(FixWhiteSpace(authors)), [',']);
    artists := TrimRightChar(Trim(FixWhiteSpace(artists)), [',']);
    genres := TrimRightChar(Trim(FixWhiteSpace(genres)), [',']);

    summary := CleanMultilinedString(FixWhiteSpace(summary));

    // fix info
    if title = '' then
      title := 'N/A';
    if (LeftStr(authors, 1) = '<') or (authors = '-') or (authors = ':') then
      authors := '';
    if (LeftStr(artists, 1) = '<') or (artists = '-') or (artists = ':') then
      artists := '';
    if (summary = '-') or (summary = ':') then
      summary := '';

    // cleanup chapters
    if chapterLinks.Count > 0 then begin
      while chapterName.Count < chapterLinks.Count do
        chapterName.Add('');
      while chapterLinks.Count < chapterName.Count do
        chapterName.Delete(chapterName.Count - 1);
      for j := 0 to chapterLinks.Count - 1 do begin
        chapterLinks[j] := Trim(chapterLinks[j]);
        chapterName[j] := Trim(chapterName[j]);
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
            if SameText(chapterLinks[j], chapterLinks[k]) then
            begin
              chapterLinks.Delete(j);
              chapterName.Delete(j);
              del := True;
              Break;
            end;
        if not del then
          Inc(j);
      end;
    end;

    if chapterLinks.Count > 0 then
    begin
      // remove host from chapter links
      if RemoveHostFromChapterLinks then
        RemoveHostFromURLsPair(chapterLinks, chapterName);
      // fixing chapter name
      for j := 0 to chapterName.Count - 1 do
        chapterName[j] := Trim(CleanString(RemoveStringBreaks(
          CommonStringFilter(chapterName[j]))));

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
              s2 := Trim(s2);
              if LeftStr(s2, 2) = '- ' then
                Delete(s2, 1, 2);
              chapterName[k] := s2;
            end;
        end;
      end;
    end;

    numChapter := chapterLinks.Count;
  end;
end;

procedure TMangaInformation.SyncMinorInfoToData(const ADataProcess: TDataProcess; const AIndex: Cardinal);
begin
  // sync info to data
  {$IFDEF DOWNLOADER}
  if not ADataProcess.isFilterAllSites then
  {$ENDIF}
    ADataProcess.Data.Strings[AIndex] := SetParams(
      [ADataProcess.Param[AIndex, DATA_PARAM_TITLE],
      ADataProcess.Param[AIndex, DATA_PARAM_LINK],
      ADataProcess.Param[AIndex, DATA_PARAM_AUTHORS],
      ADataProcess.Param[AIndex, DATA_PARAM_ARTISTS],
      ADataProcess.Param[AIndex, DATA_PARAM_GENRES],
      mangaInfo.status,
      ADataProcess.Param[AIndex, DATA_PARAM_SUMMARY],
      IntToStr(mangaInfo.numChapter),
      {$IFDEF DOWNLOADER}
      ADataProcess.Param[AIndex, DATA_PARAM_JDN],
      {$ELSE}
      '0',
      {$ENDIF}
      '0']);
  // then break it into parts
  ADataProcess.BreakDataToParts(AIndex);
end;

procedure TMangaInformation.SyncInfoToData(const ADataProcess: TDataProcess; const AIndex: Cardinal);
begin
  // sync info to data
  {$IFDEF DOWNLOADER}
  if not ADataProcess.isFilterAllSites then
  {$ENDIF}
  begin
    if Trim(mangaInfo.title) = '' then
      mangaInfo.title := ADataProcess.Param[AIndex, DATA_PARAM_TITLE];
    ADataProcess.Data.Strings[AIndex] := SetParams(
      //[ADataProcess.Param[AIndex, DATA_PARAM_TITLE],
      //sync title as well, some site possible to change title or when mangainfo script not work
      [mangaInfo.title,
      ADataProcess.Param[AIndex, DATA_PARAM_LINK],
      mangaInfo.authors,
      mangaInfo.artists,
      mangaInfo.genres,
      mangaInfo.status,
      StringFilter(mangaInfo.summary),
      IntToStr(mangaInfo.numChapter),
      {$IFDEF DOWNLOADER}
      ADataProcess.Param[AIndex, DATA_PARAM_JDN],
      {$ELSE}
      '0',
      {$ENDIF}
      '0']);
  end;
  // then break it into parts
  ADataProcess.BreakDataToParts(AIndex);
end;

procedure TMangaInformation.SyncInfoToData(const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
    with mangaInfo do
      ADataProcess.UpdateData(title, link, authors, artists, genres, status, summary,
        numChapter, website);
end;

procedure TMangaInformation.AddInfoToDataWithoutBreak(const AName, ALink: String;
  const ADataProcess: TDataProcess);
var
  S: String;
begin
  if mangaInfo.title <> '' then
    S := mangaInfo.title
  else
    S := AName;

  ADataProcess.Data.Add(RemoveStringBreaks(SetParams([
    S,
    ALink,
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

procedure TMangaInformation.AddInfoToData(const AName, Alink: String; const ADataProcess: TDataProcess);
var
  l: TStringList;
begin
  l := TStringList.Create;
  ADataProcess.Data.Add(
    RemoveStringBreaks(
    SetParams(
    [AName,
    Alink,
    mangaInfo.authors,
    mangaInfo.artists,
    mangaInfo.genres,
    mangaInfo.status,
    StringFilter(mangaInfo.summary),
    IntToStr(mangaInfo.numChapter),
    IntToStr(GetCurrentJDN),
    '0'])));
  GetParams(l, ADataProcess.Data.Strings[ADataProcess.Data.Count - 1]);
  ADataProcess.title.Add(l.Strings[DATA_PARAM_TITLE]);
  ADataProcess.link.Add(l.Strings[DATA_PARAM_LINK]);
  ADataProcess.authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
  ADataProcess.artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
  ADataProcess.genres.Add(l.Strings[DATA_PARAM_GENRES]);
  ADataProcess.status.Add(l.Strings[DATA_PARAM_STATUS]);
  ADataProcess.summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
  {$IFDEF DOWNLOADER}
  ADataProcess.jdn.Add(Pointer(StrToInt(l.Strings[DATA_PARAM_JDN])));
  {$ELSE}
  DataProcess.jdn.Add(Pointer(StrToInt('0')));
  {$ENDIF}
  l.Free;
end;

procedure TMangaInformation.AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
  begin
    if (mangaInfo.title = '') and (ATitle <> '') then mangaInfo.title := ATitle;
    if (mangaInfo.link = '') and (ALink <> '') then mangaInfo.link := ALink;
    with mangaInfo do
      ADataProcess.AddData(title, link, authors, artists, genres, status,
        StringBreaks(summary), numChapter, Now);
  end;
end;

function TMangaInformation.GetPage(var AOutput: TObject; AURL: String; const AReconnect: Integer): Boolean;
begin
  Result := uBaseUnit.GetPage(FHTTP, AOutput, AURL, AReconnect);
end;

end.
