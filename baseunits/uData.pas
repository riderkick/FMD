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
  Classes, SysUtils, uBaseUnit, uFMDThread, sqlite3conn, sqldb, USimpleLogger,
  strutils, dateutils, RegExpr, sqlite3, FileUtil, httpsend;

type

  TSQLite3Connectionx = class(TSQLite3Connection)
  public
    property Handle read GetHandle;
  end;

  { TDBDataProcess }

  TDBDataProcess = class
  private
    FConn: TSQLite3Connectionx;
    FTrans: TSQLTransaction;
    FQuery: TSQLQuery;
    FRegxp: TRegExpr;
    FWebsite: String;
    FTableName: String;
    FDataCount: Integer;
    FFiltered: Boolean;
    FFilterAllSites: Boolean;
    FSitesList: TStringList;
    FSQLSelect: String;
  protected
    function GetConnected: Boolean;
    procedure CreateTable;
    procedure VacuumTable;
    procedure GetDataCount;
    function GetWebsiteName(RecIndex: Integer): String;
    function GetParam(RecIndex, ParamNo: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(AWebsite: String = ''): Boolean;
    procedure Save;
    procedure Refresh;
    procedure AddData(Title, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter, JDN: Integer); overload;
    procedure AddData(Title, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter: Integer; JDN: TDateTime); overload;
    procedure ApplyUpdates;
    function Search(ATitle: String): Boolean;
    function CanFilter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Cardinal; const haveAllChecked, searchNewManga: Boolean): Boolean;
    function Filter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Cardinal; const haveAllChecked, searchNewManga: Boolean;
      useRegExpr: Boolean = False): Boolean;
    procedure RemoveFilter;
    procedure Sort;
    property Website: String read FWebsite write FWebsite;
    property TableName: String read FTableName write FTableName;
    property Connected: Boolean read GetConnected;
    property DataCount: Integer read FDataCount;
    property Filtered: Boolean read FFiltered;
    property FilterAllSites: Boolean read FFilterAllSites write FFilterAllSites;
    property SitesList: TStringList read FSitesList write FSitesList;
    property WebsiteName[RecIndex: Integer]: String read GetWebsiteName;
    property Param[RecIndex, ParamNo: Integer]: String read GetParam;
  end;

  { TDataProcess }

  TDataProcess = class(TObject)
  private
    function GetInfo(const index: Cardinal): TStringList;
    function GetParam(const index, paramNo : Integer) : String;
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
    FHTTP: THTTPSend;

    procedure OnTag(NoCaseTag, ActualTag: string);
    procedure OnText(Text: String);
    constructor Create(AOwnerThread: TFMDThread = nil);
    destructor Destroy; override;
    procedure ClearInfo;
    function GetDirectoryPage(var Page: Cardinal; const website: String): Byte;
    function GetNameAndLink(const names, links: TStringList;
      const website, URL: String): Byte;
    function GetInfoFromURL(const website, URL: String; const Reconnect: Cardinal): Byte;
    procedure SyncInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
    procedure SyncMinorInfoToData(const DataProcess: TDataProcess;
      const index: Cardinal);

    // Only use this function for getting manga infos for the first time
    procedure AddInfoToDataWithoutBreak(const Name, link: String;
      const DataProcess: TDataProcess);
    // Only use this function for update manga list
    procedure AddInfoToData(const Name, link: String; const DataProcess: TDataProcess);

    //wrapper
    function GetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean; overload;
  end;

var
  options: TStringList;

const
  DBDataProcessParam = 'title,link,authors,artists,genres,status,summary,numchapter,jdn';
  DBDataProcessParams: array [0..8] of ShortString =
    ('title', 'link', 'authors', 'artists', 'genres', 'status', 'summary', 'numchapter', 'jdn');
  DBDataProccesCreateParam = '(title TEXT,'+
                              'link TEXT NOT NULL PRIMARY KEY,'+
                              'authors TEXT,'+
                              'artists TEXT,'+
                              'genres TEXT,'+
                              'status TEXT,'+
                              'summary TEXT,'+
                              'numchapter INTEGER,'+
                              'jdn INTEGER);';

  procedure ConvertDataProccessToDB(AWebsite: String; DeleteOriginal: Boolean = False);

implementation

uses
  Dialogs,
  fpJSON, JSONParser, IniFiles,
  jsHTMLUtil,
  FastHTMLParser, HTMLUtil,
  SynaCode,
  frmMain,
  uMisc;

function NaturalCompareCallback({%H-}user: pointer; len1: longint; data1: pointer;
  len2: longint; data2: pointer): longint; cdecl;
var
  s1, s2: String;
begin
  SetString(s1, data1, len1);
  SetString(s2, data2, len2);
  Result := NaturalCompareStr(s1, s2);
end;

procedure RegexCallback(context: PSqlite3_Context; argc: longint;
  argv: PPSqlite3_Value); cdecl;
var
  regexp, Text: PChar;
  regex: TRegExpr;
begin
  if sqlite3_user_data(context) = nil then
  begin
    sqlite3_result_int64(context, 0);
    Exit;
  end;
  if argc <> 2 then
  begin
    sqlite3_result_int64(context, 0);
    Exit;
  end;
  regexp := sqlite3_value_text(argv[0]);
  Text := sqlite3_value_text(argv[1]);
  if (regexp = nil) or (Text = nil) then
  begin
    sqlite3_result_int64(context, 0);
    Exit;
  end;
  try
    regex := TRegExpr(sqlite3_user_data(context));
    regex.Expression := regexp;
    sqlite3_result_int64(context, int64(regex.Exec(Text)));
  except
    sqlite3_result_int64(context, 0);
  end;
end;

procedure ConvertDataProccessToDB(AWebsite: String; DeleteOriginal: Boolean);
var
  filepath: String;
  rawdata: TDataProcess;
  dbdata: TDBDataProcess;
  i: Integer;
begin
  filepath := fmdDirectory + DATA_FOLDER + AWebsite;
  if FileExistsUTF8(filepath + DATA_EXT) then
  begin
    rawdata := TDataProcess.Create;
    dbdata := TDBDataProcess.Create;
    try
      if FileExistsUTF8(filepath + DBDATA_EXT) then
        DeleteFileUTF8(filepath + DBDATA_EXT);
      rawdata.LoadFromFile(AWebsite);
      dbdata.Open(AWebsite);
      if rawdata.Data.Count > 0 then
      with rawdata do
      begin
        for i := 0 to Data.Count-1 do
        begin
          dbdata.AddData(Title[i], Link[i], Authors[i], Artists[i], Genres[i],
            Status[i], StringBreaks(Summary[i]), StrToIntDef(Param[i, DATA_PARAM_NUMCHAPTER], 1),
            {%H-}Integer(JDN[i])-3);
        end;
        dbdata.ApplyUpdates;
      end;
      dbdata.Sort;
    finally
      rawdata.Free;
      dbdata.Free;
    end;
    if DeleteOriginal then
      DeleteFileUTF8(filepath + DATA_EXT);
  end;
end;

{ TDBDataProcess }

function TDBDataProcess.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TDBDataProcess.CreateTable;
begin
  if FConn.Connected then
  begin
    FConn.ExecuteDirect('CREATE TABLE ' + QuotedStr(FTableName) +
      DBDataProccesCreateParam);
    FTrans.Commit;
  end;
end;

procedure TDBDataProcess.VacuumTable;
begin
  if FConn.Connected then
  with FConn do
  begin
    ExecuteDirect('END TRANSACTION');
    ExecuteDirect('VACUUM');
    ExecuteDirect('BEGIN TRANSACTION');
  end;
end;

procedure TDBDataProcess.GetDataCount;
begin
  if FQuery.Active then
  begin
    FQuery.Last;
    FDataCount := FQuery.RecordCount;
    FQuery.Refresh;
  end
  else
    FDataCount := 0;
end;

function TDBDataProcess.GetWebsiteName(RecIndex: Integer): String;
begin
  if FConn.Connected then
  begin
    if FilterAllSites then
    begin

    end
    else
      Result := FWebsite;
  end;
end;

function TDBDataProcess.GetParam(RecIndex, ParamNo: Integer): String;
begin
  Result := '';
  if FQuery.Active and
    (ParamNo < Length(DBDataProcessParams)) and
    (RecIndex < FDataCount) then
  begin
    try
      FQuery.RecNo := RecIndex+1;
      Result:= FQuery.FieldByName(DBDataProcessParams[ParamNo]).AsString;
    except
      on E: Exception do
        WriteLog_E('TDBDataProcess.GetParam.Error: ' + E.Message +
          LineEnding + GetStackTraceInfo);
    end;
  end;
end;

constructor TDBDataProcess.Create;
begin
  FConn := TSQLite3Connectionx.Create(nil);
  FTrans := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FTrans.DataBase := FConn;
  FQuery.Transaction := FTrans;
  FQuery.DataBase := FTrans.DataBase;
  FRegxp := TRegExpr.Create;
  FRegxp.ModifierI := True;
  FSitesList := TStringList.Create;
  FTableName := 'masterlist';
  FSQLSelect := 'SELECT * FROM ' + FTableName;
  FDataCount := 0;
end;

destructor TDBDataProcess.Destroy;
begin
  FSitesList.Free;
  FQuery.Close;
  if FConn.Connected then
  begin
    FTrans.Commit;
    VacuumTable;
  end;
  FTrans.Active := False;
  FConn.Connected := False;
  FQuery.Free;
  FTrans.Free;
  FConn.Free;
  FRegxp.Free;
  inherited Destroy;
end;

function TDBDataProcess.Open(AWebsite: String): Boolean;
var
  ts: TStringList;
  filepath: String;
  i: Integer;
begin
  Result := False;
  if AWebsite <> '' then FWebsite := AWebsite;
  if FWebsite = '' then Exit;
  try
    filepath := fmdDirectory + DATA_FOLDER + FWebsite + DBDATA_EXT;
    if FConn.Connected then
    begin
      FTrans.Commit;
      VacuumTable;
      FConn.Connected := False;
    end;
    FConn.DatabaseName := filepath;
    FConn.Connected := True;
    sqlite3_create_collation(FConn.Handle, PChar('NATCMP'), SQLITE_UTF8, nil,
      NaturalCompareCallback);
    sqlite3_create_function(FConn.Handle, PChar('REGEXP'), 2, SQLITE_UTF8, FRegxp,
      RegexCallback, nil, nil);
    FTrans.Active := True;
    ts := TStringList.Create;
    try
      FConn.GetTableNames(ts);
      ts.Sort;
      if not ts.Find(FTableName, i) then
        CreateTable;
    finally
      ts.Free;
    end;
    FSQLSelect := 'SELECT * FROM ' + FTableName;;
    FQuery.SQL.Text := FSQLSelect;
    FQuery.Open;
    GetDataCount;
    FFiltered := False;
    Result := FQuery.Active;
  except
    on E: Exception do
      WriteLog_E('TDBDataProcess.LoadFromFile.Error: ' + E.Message +
        LineEnding + GetStackTraceInfo);
  end;
end;

procedure TDBDataProcess.Save;
begin
  Self.ApplyUpdates;
end;

procedure TDBDataProcess.Refresh;
begin
  if FQuery.Active then
    FQuery.Refresh;
end;

procedure TDBDataProcess.AddData(Title, Link, Authors, Artists, Genres, Status,
  Summary: String; NumChapter, JDN: Integer);
begin
  if FConn.Connected then
  try
    FConn.ExecuteDirect('INSERT INTO ' + FTableName + '(' + DBDataProcessParam +
      ') VALUES (' +
      QuotedStrd(Title) + ',' +
      QuotedStrd(Link) + ',' +
      QuotedStrd(Authors) + ',' +
      QuotedStrd(Artists) + ',' +
      QuotedStrd(Genres) + ',' +
      QuotedStrd(Status) + ',' +
      QuotedStrd(Summary) + ',' +
      QuotedStrd(NumChapter) + ',' +
      QuotedStrd(JDN) + ');');
  except
    on E: Exception do
      WriteLog_E('TDBDataProcess.AddData.Error: ' + E.Message + LineEnding + GetStackTraceInfo);
  end;
end;

procedure TDBDataProcess.AddData(Title, Link, Authors, Artists, Genres, Status,
  Summary: String; NumChapter: Integer; JDN: TDateTime);
begin
  Self.AddData(Title, Link, Authors, Artists, Genres, Status, Summary,
    NumChapter, DateToJDN(JDN));
end;

procedure TDBDataProcess.ApplyUpdates;
begin
  if FConn.Connected then
  begin
    FQuery.Close;
    FTrans.Commit;
    FQuery.Open;
    GetDataCount;
  end;
end;

function TDBDataProcess.Search(ATitle: String): Boolean;
begin
  Result := False;
  if FConn.Connected then
  begin
    try
      FQuery.Close;
      FQuery.SQL.Text := FSQLSelect;
      if ATitle <> '' then
        FQuery.SQL.Add('WHERE title LIKE ' + QuotedStrd(AnsiQuotedStr(ATitle, '%')));
      FQuery.Open;
      GetDataCount;
    except
      on E: Exception do
        WriteLog_E('TDBDataProcess.Search.Error: ' + E.Message + LineEnding + GetStackTraceInfo);
    end;
  end;
end;

function TDBDataProcess.CanFilter(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists, stStatus,
  stSummary: String; const minusDay: Cardinal; const haveAllChecked,
  searchNewManga: Boolean): Boolean;
begin
  Result := True;
end;

function TDBDataProcess.Filter(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists, stStatus,
  stSummary: String; const minusDay: Cardinal; const haveAllChecked,
  searchNewManga: Boolean; useRegExpr: Boolean): Boolean;
begin
  FFiltered := True;
  Result := FFiltered;
end;

procedure TDBDataProcess.RemoveFilter;
begin
  FFiltered := False;
end;

procedure TDBDataProcess.Sort;
begin
  if FConn.Connected then
  begin
    FQuery.Close;
    with FConn do
    begin
      ExecuteDirect('CREATE TABLE ' + QuotedStrd(FTableName+'_ordered') +
        DBDataProccesCreateParam);
      ExecuteDirect('INSERT INTO '+ QuotedStrd(FTableName+'_ordered') + ' ' +
        BracketStr(DBDataProcessParam) + ' SELECT ' + DBDataProcessParam +
        ' FROM '+ QuotedStrd(FTableName) + 'ORDER BY title COLLATE NATCMP');
      ExecuteDirect('DROP TABLE ' + QuotedStrd(FTableName));
      ExecuteDirect('ALTER TABLE '+ QuotedStrd(FTableName+'_ordered') +
        'RENAME TO ' + QuotedStrd(FTableName));
      FTrans.Commit;
      VacuumTable
    end;
    FQuery.Open;
  end;
end;

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
    Title.Strings[i] := GetParam(i, DATA_PARAM_NAME);
    Link.Strings[i] := GetParam(i, DATA_PARAM_LINK);
    Authors.Strings[i] := GetParam(i, DATA_PARAM_AUTHORS);
    Artists.Strings[i] := GetParam(i, DATA_PARAM_ARTISTS);
    Genres.Strings[i] := GetParam(i, DATA_PARAM_GENRES);
    Status.Strings[i] := GetParam(i, DATA_PARAM_STATUS);
    Summary.Strings[i] := GetParam(i, DATA_PARAM_SUMMARY);
    JDN.Items[i] := Pointer(StrToIntDef(GetParam(i, DATA_PARAM_JDN),0));
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

  if not FileExists(Filename + DATA_EXT) then
    Exit(False);
  l := TStringList.Create;
  try
    Self.Filename := Filename;

    Data.LoadFromFile(Filename + DATA_EXT);
    id := GetMangaSiteID(website);

    if Data.Count > 0 then
    begin
      //QuickSortData(data);
      QuickSortNaturalPart(Data, SEPERATOR, DATA_PARAM_NAME); //Natural Sorting
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
          title.Add(l.Strings[DATA_PARAM_NAME]);
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

// TODO: load from all files - this function is for "Filter all sites"
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
      if not FileExists(Filename + DATA_EXT) then
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
          While l.Count < 10 do
            l.Add('');
          title.Add(l.Strings[DATA_PARAM_NAME]);
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
        if (currentJDN - Integer(jdn.Items[fpos]) >= minusDay) and
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
      MainForm.ExceptionHandler(Self, E);
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
  uMisc.QuickSortNaturalPart(Data, SEPERATOR, DATA_PARAM_NAME);
end;

{ TMangaInformation }

constructor TMangaInformation.Create(AOwnerThread: TFMDThread);
begin
  inherited Create;
  FHTTP := THTTPSendThread.Create(AOwnerThread);
  FHTTP.Headers.NameValueSeparator := ':';
  parse := TStringList.Create;
  mangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
end;

destructor TMangaInformation.Destroy;
begin
  ClearInfo;
  mangaInfo.Free;
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

procedure TMangaInformation.OnTag(NoCaseTag, ActualTag : string);
begin
  parse.Add(ActualTag);
end;

procedure TMangaInformation.OnText(Text: String);
begin
  parse.Add(Text);
end;

function TMangaInformation.GetDirectoryPage(var Page: Cardinal;
  const website: String): Byte;
var
  s: String;
  p: Integer;
  Source: TStringList;
  Parser: THTMLParser;
  WebsiteID: Cardinal;

  {$I includes/AnimeA/directory_page_number.inc}

  {$I includes/KissManga/directory_page_number.inc}

  {$I includes/Batoto/directory_page_number.inc}

  {$I includes/Manga24h/directory_page_number.inc}

  {$I includes/VnSharing/directory_page_number.inc}

  {$I includes/Hentai2Read/directory_page_number.inc}

  {$I includes/Fakku/directory_page_number.inc}

  {$I includes/Pururin/directory_page_number.inc}

  {$I includes/MangaPark/directory_page_number.inc}

  {$I includes/MangaFox/directory_page_number.inc}

  //{$I includes/MangaTraders/directory_page_number.inc}

  {$I includes/SenManga/directory_page_number.inc}

  {$I includes/MangaGo/directory_page_number.inc}

  {$I includes/MangaEden/directory_page_number.inc}

  {$I includes/BlogTruyen/directory_page_number.inc}

  {$I includes/RedHawkScans/directory_page_number.inc}

  {$I includes/S2Scans/directory_page_number.inc}

  {$I includes/LectureEnLigne/directory_page_number.inc}

  {$I includes/MangaAe/directory_page_number.inc}

  {$I includes/CentralDeMangas/directory_page_number.inc}

  {$I includes/Manga2u/directory_page_number.inc}

  {$I includes/DM5/directory_page_number.inc}

  {$I includes/EHentai/directory_page_number.inc}

  {$I includes/NineManga/directory_page_number.inc}

  {$I includes/JapanShin/directory_page_number.inc}

  {$I includes/Mangacow/directory_page_number.inc}

  {$I includes/OneManga/directory_page_number.inc}

  {$I includes/MangaTown/directory_page_number.inc}

  {$I includes/ReadHentaiManga/directory_page_number.inc}

  {$I includes/MyReadingMangaInfo/directory_page_number.inc}

  {$I includes/IKomik/directory_page_number.inc}

  {$I includes/NHentai/directory_page_number.inc}

  {$I includes/UnionMangas/directory_page_number.inc}

  {$I includes/MangaMint/directory_page_number.inc}

  {$I includes/HakiHome/directory_page_number.inc}

  {$I includes/MangaFrame/directory_page_number.inc}

  {$I includes/MangaHost/directory_page_number.inc}

  {$I includes/PornComix/directory_page_number.inc}

  {$I includes/MangaSee/directory_page_number.inc}

  {$I includes/AcademyVN/directory_page_number.inc}

  {$I includes/MangaAt/directory_page_number.inc}

  {$I includes/ReadMangaToday/directory_page_number.inc}

  {$I includes/Dynasty-Scans/directory_page_number.inc}

  {$I includes/Madokami/directory_page_number.inc}

  {$I includes/WPManga/directory_page_number.inc}

begin
  Page := 0;
  WebsiteID := GetMangaSiteID(website);

  //load User-Agent from INIAdvanced
  if website <> '' then
    FHTTP.UserAgent := INIAdvanced.ReadString('UserAgent', website, '');

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

    Source := TStringList.Create;
    if website = WebsiteRoots[ANIMEA_ID, 0] then
      Result := GetAnimeADirectoryPageNumber
    else
    if website = WebsiteRoots[KISSMANGA_ID, 0] then
      Result := GetKissMangaDirectoryPageNumber
    else
    if website = WebsiteRoots[BATOTO_ID, 0] then
      Result := GetBatotoDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGA24H_ID, 0] then
      Result := GetManga24hDirectoryPageNumber
    else
    if website = WebsiteRoots[VNSHARING_ID, 0] then
      Result := GetVnSharingDirectoryPageNumber
    else
    if website = WebsiteRoots[HENTAI2READ_ID, 0] then
      Result := GetHentai2ReadDirectoryPageNumber
    else
    if website = WebsiteRoots[FAKKU_ID, 0] then
      Result := GetFakkuDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAPARK_ID, 0] then
      Result := GetMangaParkDirectoryPageNumber
    else
    //if website = WebsiteRoots[MANGAFOX_ID, 0] then
    //  Result := GetMangaFoxDirectoryPageNumber
    //else
    //if website = WebsiteRoots[MANGATRADERS_ID, 0] then
    //  Result := GetMangaTradersDirectoryPageNumber
    //else
    if website = WebsiteRoots[MANGAGO_ID, 0] then
      Result := GetMangaGoDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAEDEN_ID, 0] then
      Result := GetMangaEdenDirectoryPageNumber(WebsiteRoots[MANGAEDEN_ID, 1])
    else
    if website = WebsiteRoots[PERVEDEN_ID, 0] then
      Result := GetMangaEdenDirectoryPageNumber(WebsiteRoots[PERVEDEN_ID, 1])
    else
    if website = WebsiteRoots[BLOGTRUYEN_ID, 0] then
      Result := GetBlogTruyenDirectoryPageNumber
    else
    if website = WebsiteRoots[REDHAWKSCANS_ID, 0] then
      Result := GetRedHawkScansDirectoryPageNumber
    else
    if website = WebsiteRoots[S2SCAN_ID,0] then
      Result:= GetS2ScanDirectoryPageNumber
    else
    if website = WebsiteRoots[SENMANGA_ID, 0] then
      Result := GetSenMangaDirectoryPageNumber
    else
    if website = WebsiteRoots[LECTUREENLIGNE_ID, 0] then
      Result := GetLectureEnLigneDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAAE_ID, 0] then
      Result := GetMangaAeDirectoryPageNumber
    else
    if website = WebsiteRoots[CENTRALDEMANGAS_ID, 0] then
      Result := GetCentralDeMangasDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGA2U_ID, 0] then
      Result := GetManga2uDirectoryPageNumber
    else
    if website = WebsiteRoots[DM5_ID, 0] then
      Result := GetDM5DirectoryPageNumber
    else
    if website = WebsiteRoots[PURURIN_ID, 0] then
      Result := GetPururinDirectoryPageNumber
    else
    if website = WebsiteRoots[EHENTAI_ID, 0] then
      Result := GetEHentaiDirectoryPageNumber
    else
    if (website = WebsiteRoots[NINEMANGA_ID, 0]) or
      (website = WebsiteRoots[NINEMANGA_ES_ID, 0]) or
      (website = WebsiteRoots[NINEMANGA_CN_ID, 0]) or
      (website = WebsiteRoots[NINEMANGA_RU_ID, 0]) or
      (website = WebsiteRoots[NINEMANGA_DE_ID, 0]) or
      (website = WebsiteRoots[NINEMANGA_IT_ID, 0]) or
      (website = WebsiteRoots[NINEMANGA_BR_ID, 0]) then
      Result := GetNineMangaDirectoryPageNumber
    else
    if website = WebsiteRoots[JAPANSHIN_ID, 0] then
      Result := GetJapanShinDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGACOW_ID, 0] then
      Result := GetMangaCowDirectoryPageNumber
    else
    if website = WebsiteRoots[ONEMANGA_ID, 0] then
      Result := GetOneMangaDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGATOWN_ID, 0] then
      Result := GetMangaTownDirectoryPageNumber
    else
    if website = WebsiteRoots[READHENTAIMANGA_ID, 0] then
      Result := GetReadHentaiMangaDirectoryPageNumber
    else
    if website = WebsiteRoots[MYREADINGMANGAINFO_ID, 0] then
      Result := GetMyReadingMangaInfoDirectoryPageNumber
    else
    if website = WebsiteRoots[IKOMIK_ID, 0] then
      Result := GetIKomikDirectoryPageNumber
    else
    if website = WebsiteRoots[NHENTAI_ID, 0] then
      Result := GetNHentaiDirectoryPageNumber
    else
    if website = GetMangaSiteName(UNIONMANGAS_ID) then
      Result := GetUnionMangasDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAMINT_ID, 0] then
      Result := GetMangaMintDirectoryPageNumber
    else
    if website = WebsiteRoots[HAKIHOME_ID, 0] then
      Result := GetHakiHomeDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAFRAME_ID, 0] then
      Result := GetMangaFrameDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAHOST_ID, 0] then
      Result := GetMangaHostDirectoryPageNumber
    else
    if (website = WebsiteRoots[PORNCOMIX_ID, 0]) or
      (website = WebsiteRoots[XXCOMICS_ID, 0]) or
      (website = WebsiteRoots[XXCOMICSMT_ID, 0]) or
      (website = WebsiteRoots[XXCOMICS3D_ID, 0]) or
      (website = WebsiteRoots[PORNCOMIXRE_ID, 0]) or
      (website = WebsiteRoots[PORNCOMIXIC_ID, 0]) or
      (website = WebsiteRoots[PORNXXXCOMICS_ID, 0]) then
      Result := GetPornComixDirectoryPageNumber(GetMangaSiteID(website))
    else
    if website = WebsiteRoots[MANGASEE_ID, 0] then
      Result := GetMangaSeeDirectoryPageNumber
    else
    if website = WebsiteRoots[ACADEMYVN_ID, 0] then
      Result := GetAcademyVNDirectoryPageNumber
    else
    if website = WebsiteRoots[MANGAAT_ID, 0] then
      Result := GetMangaAtDirectoryPageNumber
    else
    if website = WebsiteRoots[READMANGATODAY_ID, 0] then
      Result := GetReadMangaTodayDirectoryPageNumber
    else
    if website = WebsiteRoots[DYNASTYSCANS_ID, 0] then
      Result := GetDynastyScansDirectoryPageNumber
    else
    if website = WebsiteRoots[MADOKAMI_ID, 0] then
      Result := GetMadokamiDirectoryPageNumber
    else
    if SitesIsWPManga(WebsiteID) then
      Result := GetWPMangaDirectoryPageNumber
    else
    begin
      Result := NO_ERROR;
      Page := 1;
      Source.Free;
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

  {$I includes/Manga2u/names_and_links.inc}

  {$I includes/AnimeA/names_and_links.inc}

  {$I includes/MangaHere/names_and_links.inc}

  {$I includes/EsMangaHere/names_and_links.inc}

  {$I includes/AnimExtremist/names_and_links.inc}

  {$I includes/MangaInn/names_and_links.inc}

  {$I includes/KissManga/names_and_links.inc}

  {$I includes/Batoto/names_and_links.inc}

  {$I includes/Manga24h/names_and_links.inc}

  {$I includes/VnSharing/names_and_links.inc}

  {$I includes/Hentai2Read/names_and_links.inc}

  {$I includes/Fakku/names_and_links.inc}

  {$I includes/MangaReader/names_and_links.inc}

  {$I includes/MangaPark/names_and_links.inc}

  {$I includes/MangaFox/names_and_links.inc}

  {$I includes/MangaTraders/names_and_links.inc}

  {$I includes/TruyenTranhTuan/names_and_links.inc}

  {$I includes/SubManga/names_and_links.inc}

  {$I includes/Komikid/names_and_links.inc}

  {$I includes/PecintaKomik/names_and_links.inc}

  {$I includes/Mabuns/names_and_links.inc}

  {$I includes/MangaEsta/names_and_links.inc}

  {$I includes/Pururin/names_and_links.inc}

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

  {$I includes/MangaPanda/names_and_links.inc}

  {$I includes/MangaGo/names_and_links.inc}

  {$I includes/MangaStream/names_and_links.inc}

  {$I includes/RedHawkScans/names_and_links.inc}

  {$I includes/S2Scans/names_and_links.inc}

  {$I includes/EGScans/names_and_links.inc}

  {$I includes/BlogTruyen/names_and_links.inc}

  {$I includes/MangaEden/names_and_links.inc}

  {$I includes/Kivmanga/names_and_links.inc}

  {$I includes/Mangacan/names_and_links.inc}

  {$I includes/MeinManga/names_and_links.inc}

  {$I includes/MangasPROJECT/names_and_links.inc}

  {$I includes/MangaREADER_POR/names_and_links.inc}

  {$I includes/EHentai/names_and_links.inc}

  {$I includes/MangaStreamTo/names_and_links.inc}

  {$I includes/NineManga/names_and_links.inc}

  {$I includes/JapanShin/names_and_links.inc}

  {$I includes/Japscan/names_and_links.inc}

  {$I includes/CentrumMangi_PL/names_and_links.inc}

  {$I includes/MangaLib_PL/names_and_links.inc}

  {$I includes/OneManga/names_and_links.inc}

  {$I includes/MangaTown/names_and_links.inc}

  {$I includes/ReadHentaiManga/names_and_links.inc}

  {$I includes/MangaOku/names_and_links.inc}

  {$I includes/MyReadingMangaInfo/names_and_links.inc}

  {$I includes/IKomik/names_and_links.inc}

  {$I includes/NHentai/names_and_links.inc}

  {$I includes/UnionMangas/names_and_links.inc}

  {$I includes/MangaMint/names_and_links.inc}

  {$I includes/UnixManga/names_and_links.inc}

  {$I includes/HakiHome/names_and_links.inc}

  {$I includes/ExtremeMangas/names_and_links.inc}

  {$I includes/MangaHost/names_and_links.inc}

  {$I includes/PornComix/names_and_links.inc}

  {$I includes/MangaSee/names_and_links.inc}

  {$I includes/MangaKu/names_and_links.inc}

  {$I includes/AcademyVN/names_and_links.inc}

  {$I includes/MangaAt/names_and_links.inc}

  {$I includes/SenMangaRAW/names_and_links.inc}

  {$I includes/ReadMangaToday/names_and_links.inc}

  {$I includes/LoneManga/names_and_links.inc}

  {$I includes/Dynasty-Scans/names_and_links.inc}

  {$I includes/Madokami/names_and_links.inc}

  {$I includes/WPManga/names_and_links.inc}

begin
  WebsiteID := GetMangaSiteID(website);

  //load User-Agent from INIAdvanced
  if website <> '' then
    FHTTP.UserAgent := INIAdvanced.ReadString('UserAgent', website, '');

  Source := TStringList.Create;
  if website = WebsiteRoots[ANIMEA_ID, 0] then
    Result := AnimeAGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAHERE_ID, 0] then
    Result := MangaHereGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAINN_ID, 0] then
    Result := MangaInnGetNamesAndLinks
  else
  if website = WebsiteRoots[KISSMANGA_ID, 0] then
    Result := KissMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[BATOTO_ID, 0] then
    Result := BatotoGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGA24H_ID, 0] then
    Result := Manga24hGetNamesAndLinks
  else
  if website = WebsiteRoots[VNSHARING_ID, 0] then
    Result := VnSharingGetNamesAndLinks
  else
  if website = WebsiteRoots[HENTAI2READ_ID, 0] then
    Result := Hentai2ReadGetNamesAndLinks
  else
  if website = WebsiteRoots[FAKKU_ID, 0] then
    Result := FakkuGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAREADER_ID, 0] then
    Result := MangaReaderGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAPARK_ID, 0] then
    Result := MangaParkGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAFOX_ID, 0] then
    Result := MangaFoxGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGATRADERS_ID, 0] then
    Result := MangaTradersGetNamesAndLinks
  else
  if website = WebsiteRoots[STARKANA_ID, 0] then
    Result := StarkanaGetNamesAndLinks
  else
  if website = WebsiteRoots[EATMANGA_ID, 0] then
    Result := EatMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAPANDA_ID, 0] then
    Result := MangaPandaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAGO_ID, 0] then
    Result := MangaGoGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGASTREAM_ID, 0] then
    Result := MangaStreamGetNamesAndLinks
  else
  if website = WebsiteRoots[REDHAWKSCANS_ID, 0] then
    Result := RedHawkScansGetNamesAndLinks
  else
  if website = WebsiteRoots[S2SCAN_ID, 0] then
    Result := S2ScanGetNamesAndLinks
  else
  if website = WebsiteRoots[EGSCANS_ID, 0] then
    Result := EGScansGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAEDEN_ID, 0] then
    Result := MangaEdenGetNamesAndLinks(WebsiteRoots[MANGAEDEN_ID, 1])
  else
  if website = WebsiteRoots[PERVEDEN_ID, 0] then
    Result := MangaEdenGetNamesAndLinks(WebsiteRoots[PERVEDEN_ID, 1])
  else
  if website = WebsiteRoots[MEINMANGA_ID, 0] then
    Result := MeinMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[BLOGTRUYEN_ID, 0] then
    Result := BlogTruyenGetNamesAndLinks
  else
  if website = WebsiteRoots[TRUYENTRANHTUAN_ID, 0] then
    Result := TruyenTranhTuanGetNamesAndLinks
  else
  if website = WebsiteRoots[SUBMANGA_ID, 0] then
    Result := SubMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[ESMANGAHERE_ID, 0] then
    Result := EsMangaHereGetNamesAndLinks
  else
  if website = WebsiteRoots[ANIMEEXTREMIST_ID, 0] then
    Result := AnimeExtremistGetNamesAndLinks
  else
  if website = WebsiteRoots[KOMIKID_ID, 0] then
    Result := KomikidGetNamesAndLinks
  else
  if website = WebsiteRoots[PECINTAKOMIK_ID, 0] then
    Result := PecintaKomikGetNamesAndLinks
  else
  if website = WebsiteRoots[MABUNS_ID, 0] then
    Result := MabunsGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAESTA_ID, 0] then
    Result := MangaEstaGetNamesAndLinks
  else
  if website = WebsiteRoots[PURURIN_ID, 0] then
    Result := PururinGetNamesAndLinks
  else
  if website = WebsiteRoots[HUGEMANGA_ID, 0] then
    Result := HugeMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[ANIMESTORY_ID, 0] then
    Result := AnimeStoryGetNamesAndLinks
  else
  if website = WebsiteRoots[LECTUREENLIGNE_ID, 0] then
    Result := LectureEnLigneGetNamesAndLinks
  else
  if website = WebsiteRoots[SCANMANGA_ID, 0] then
    Result := ScanMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAAR_ID, 0] then
    Result := MangaArGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAAE_ID, 0] then
    Result := MangaAeGetNamesAndLinks
  else
  if website = WebsiteRoots[CENTRALDEMANGAS_ID, 0] then
    Result := CentralDeMangasGetNamesAndLinks
  else
  if website = WebsiteRoots[IMANHUA_ID, 0] then
    Result := imanhuaGetNamesAndLinks
  else
  if website = WebsiteRoots[TURKCRAFT_ID, 0] then
    Result := TurkcraftGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAVADISI_ID, 0] then
    Result := MangaVadisiGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAFRAME_ID, 0] then
    Result := MangaFrameNamesAndLinks
  else
  if website = WebsiteRoots[MANGACOW_ID, 0] then
    Result := MangaCowGetNamesAndLinks
  else
  if website = WebsiteRoots[SENMANGA_ID, 0] then
    Result := SenMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[KIVMANGA_ID, 0] then
    Result := KivmangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGACAN_ID, 0] then
    Result := MangacanGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGASPROJECT_ID, 0] then
    Result := MangasPROJECTGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAREADER_POR_ID, 0] then
    Result := MangaREADER_PORGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGA2U_ID, 0] then
    Result := Manga2uGetNamesAndLinks
  else
  if website = WebsiteRoots[EHENTAI_ID, 0] then
    Result := EHentaiGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGASTREAMTO_ID, 0] then
    Result := MangaStreamToGetNamesAndLinks
  else
  if (website = WebsiteRoots[NINEMANGA_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_ES_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_CN_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_RU_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_DE_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_IT_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_BR_ID, 0]) then
    Result := NineMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[JAPANSHIN_ID, 0] then
    Result := JapanShinGetNamesAndLinks
  else
  if website = WebsiteRoots[JAPSCAN_ID, 0] then
    Result := JapscanNamesAndLinks
  else
  if website = WebsiteRoots[CENTRUMMANGI_PL_ID, 0] then
    Result := CentrumMangi_PLGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGALIB_PL_ID, 0] then
    Result := MangaLib_PLGetNamesAndLinks
  else
  if website = WebsiteRoots[ONEMANGA_ID, 0] then
    Result := OneMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGATOWN_ID, 0] then
    Result := MangaTownGetNamesAndLinks
  else
  if website = WebsiteRoots[READHENTAIMANGA_ID, 0] then
    Result := ReadHentaiMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAOKU_ID, 0] then
    Result := MangaOkuGetNamesAndLinks
  else
  if website = WebsiteRoots[MYREADINGMANGAINFO_ID, 0] then
    Result := MyReadingMangaInfoNamesAndLinks
  else
  if website = WebsiteRoots[IKOMIK_ID, 0] then
    Result := IKomikNamesAndLinks
  else
  if website = WebsiteRoots[NHENTAI_ID, 0] then
    Result := NHentaiNamesAndLinks
  else
  if website = WebsiteRoots[UNIONMANGAS_ID, 0] then
    Result := UnionMangasNamesAndLinks
  else
  if website = WebsiteRoots[MANGAMINT_ID, 0] then
    Result := MangaMintGetNamesAndLinks
  else
  if website = WebsiteRoots[UNIXMANGA_ID, 0] then
    Result := UnixMangaNamesAndLinks
  else
  if website = WebsiteRoots[HAKIHOME_ID, 0] then
    Result := HakiHomeNamesAndLinks
  else
  if website = WebsiteRoots[EXTREMEMANGAS_ID, 0] then
    Result := ExtremeMangasNamesAndLinks
  else
  if website = WebsiteRoots[MANGAHOST_ID, 0] then
    Result := MangaHostGetNamesAndLinks
  else
  if (website = WebsiteRoots[PORNCOMIX_ID, 0]) or
    (website = WebsiteRoots[XXCOMICS_ID, 0]) or
    (website = WebsiteRoots[XXCOMICSMT_ID, 0]) or
    (website = WebsiteRoots[XXCOMICS3D_ID, 0]) or
    (website = WebsiteRoots[PORNCOMIXRE_ID, 0]) or
    (website = WebsiteRoots[PORNCOMIXIC_ID, 0]) or
    (website = WebsiteRoots[PORNXXXCOMICS_ID, 0]) then
    Result := PornComixGetNamesAndLinks(GetMangaSiteID(website))
  else
  if website = WebsiteRoots[MANGASEE_ID, 0] then
    Result := MangaSeeGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAKU_ID, 0] then
    Result := MangaKuGetNamesAndLinks
  else
  if website = WebsiteRoots[ACADEMYVN_ID, 0] then
    Result := AcademyVNGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAAT_ID, 0] then
    Result := MangaAtGetNamesAndLinks
  else
  if website = WebsiteRoots[SENMANGARAW_ID, 0] then
    Result := SenMangaRAWGetNamesAndLinks
  else
  if website = WebsiteRoots[READMANGATODAY_ID, 0] then
    Result := ReadMangaTodayGetNamesAndLinks
  else
  if website = WebsiteRoots[LONEMANGA_ID, 0] then
    Result := LoneMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[DYNASTYSCANS_ID, 0] then
    Result := DynastyScansGetNamesAndLinks
  else
  if website = WebsiteRoots[MADOKAMI_ID, 0] then
    Result := MadokamiGetNamesAndLinks
  else
  if SitesIsWPManga(WebsiteID) then
    Result := GetWPMangaNamesAndLinks
  else
  begin
    Result := INFORMATION_NOT_FOUND;
    Source.Free;
  end;

  //remove host from url
  if links.Count > 0 then
    RemoveHostFromURLsPair(links, names);
end;

function TMangaInformation.GetInfoFromURL(const website, URL: String;
  const Reconnect: Cardinal): Byte;
var
  s: String;
  j, k: Integer;
  del: Boolean;
  rex: TRegExpr;
  Source: TStringList;
  Parser: THTMLParser;
  WebsiteID: Cardinal;

  {$I includes/AnimeA/manga_information.inc}

  {$I includes/MangaHere/manga_information.inc}

  // due to its weird designs, this will take a lot of work (and time) for it to
  // work property

  {$I includes/SubManga/manga_information.inc}

  {$I includes/EsMangaHere/manga_information.inc}

  {$I includes/AnimExtremist/manga_information.inc}

  {$I includes/MangaInn/manga_information.inc}

  {$I includes/KissManga/manga_information.inc}

  {$I includes/Batoto/manga_information.inc}

  {$I includes/Manga24h/manga_information.inc}

  {$I includes/VnSharing/manga_information.inc}

  {$I includes/Hentai2Read/manga_information.inc}

  {$I includes/Fakku/manga_information.inc}

  {$I includes/MangaReader/manga_information.inc}

  {$I includes/MangaPark/manga_information.inc}

  {$I includes/MangaFox/manga_information.inc}

  {$I includes/MangaTraders/manga_information.inc}

  {$I includes/MangaStream/manga_information.inc}

  {$I includes/MangaEden/manga_information.inc}

  {$I includes/Starkana/manga_information.inc}

  {$I includes/EatManga/manga_information.inc}

  {$I includes/RedHawkScans/manga_information.inc}

  {$I includes/S2Scans/manga_information.inc}

  {$I includes/EGScans/manga_information.inc}

  {$I includes/MangaPanda/manga_information.inc}

  {$I includes/MangaGo/manga_information.inc}

  {$I includes/TruyenTranhTuan/manga_information.inc}

  {$I includes/Komikid/manga_information.inc}

  // this site have 2 kind of cover page

  {$I includes/PecintaKomik/manga_information.inc}

  {$I includes/Mabuns/manga_information.inc}

  {$I includes/MangaEsta/manga_information.inc}

  {$I includes/Pururin/manga_information.inc}

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

  {$I includes/Manga2u/manga_information.inc}

  {$I includes/KivManga/manga_information.inc}

  {$I includes/Mangacan/manga_information.inc}

  {$I includes/MangasPROJECT/manga_information.inc}

  {$I includes/MangaREADER_POR/manga_information.inc}

  {$I includes/EHentai/manga_information.inc}

  {$I includes/MangaStreamTo/manga_information.inc}

  {$I includes/NineManga/manga_information.inc}

  {$I includes/JapanShin/manga_information.inc}

  {$I includes/Japscan/manga_information.inc}

  {$I includes/CentrumMangi_PL/manga_information.inc}

  {$I includes/MangaLib_PL/manga_information.inc}

  {$I includes/OneManga/manga_information.inc}

  {$I includes/MangaTown/manga_information.inc}

  {$I includes/ReadHentaiManga/manga_information.inc}

  {$I includes/MangaOku/manga_information.inc}

  {$I includes/MyReadingMangaInfo/manga_information.inc}

  {$I includes/IKomik/manga_information.inc}

  {$I includes/NHentai/manga_information.inc}

  {$I includes/UnionMangas/manga_information.inc}

  {$I includes/MangaMint/manga_information.inc}

  {$I includes/UnixManga/manga_information.inc}

  {$I includes/HakiHome/manga_information.inc}

  {$I includes/ExtremeMangas/manga_information.inc}

  {$I includes/MangaHost/manga_information.inc}

  {$I includes/PornComix/manga_information.inc}

  {$I includes/MangaSee/manga_information.inc}

  {$I includes/MangaKu/manga_information.inc}

  {$I includes/AcademyVN/manga_information.inc}

  {$I includes/MangaAt/manga_information.inc}

  {$I includes/SenMangaRAW/manga_information.inc}

  {$I includes/ReadMangaToday/manga_information.inc}

  {$I includes/LoneManga/manga_information.inc}

  {$I includes/Dynasty-Scans/manga_information.inc}

  {$I includes/Madokami/manga_information.inc}

  {$I includes/WPManga/manga_information.inc}

begin
  if Trim(URL) = '' then Exit(INFORMATION_NOT_FOUND);
  WebsiteID := GetMangaSiteID(website);
  if WebsiteID > High(WebsiteRoots) then Exit(INFORMATION_NOT_FOUND);

  //load User-Agent from INIAdvanced
  if website <> '' then
    FHTTP.UserAgent := INIAdvanced.ReadString('UserAgent', website, '');

  Source := TStringList.Create;
  mangaInfo.coverLink := '';
  mangaInfo.numChapter := 0;
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;

  if website = WebsiteRoots[ANIMEA_ID, 0] then
    Result := GetAnimeAInfoFromURL
  else
  if website = WebsiteRoots[MANGAHERE_ID, 0] then
    Result := GetMangaHereInfoFromURL
  else
  if website = WebsiteRoots[MANGAINN_ID, 0] then
    Result := GetMangaInnInfoFromURL
  else
  if website = WebsiteRoots[KISSMANGA_ID, 0] then
    Result := GetKissMangaInfoFromURL
  else
  if website = WebsiteRoots[BATOTO_ID, 0] then
    Result := GetBatotoInfoFromURL
  else
  if website = WebsiteRoots[MANGA24H_ID, 0] then
    Result := GetManga24hInfoFromURL
  else
  if website = WebsiteRoots[VNSHARING_ID, 0] then
    Result := GetVnSharingInfoFromURL
  else
  if website = WebsiteRoots[HENTAI2READ_ID, 0] then
    Result := GetHentai2ReadInfoFromURL
  else
  if website = WebsiteRoots[FAKKU_ID, 0] then
    Result := GetFakkuInfoFromURL
  else
  if website = WebsiteRoots[MANGAREADER_ID, 0] then
    Result := GetMangaReaderInfoFromURL
  else
  if website = WebsiteRoots[MANGAPARK_ID, 0] then
    Result := GetMangaParkInfoFromURL
  else
  if website = WebsiteRoots[MANGAFOX_ID, 0] then
    Result := GetMangaFoxInfoFromURL
  else
  if website = WebsiteRoots[MANGATRADERS_ID, 0] then
    Result := GetMangaTradersInfoFromURL
  else
  if website = WebsiteRoots[STARKANA_ID, 0] then
    Result := GetStarkanaInfoFromURL
  else
  if website = WebsiteRoots[EATMANGA_ID, 0] then
    Result := GetEatMangaInfoFromURL
  else
  if website = WebsiteRoots[MANGAPANDA_ID, 0] then
    Result := GetMangaPandaInfoFromURL
  else
  if website = WebsiteRoots[MANGAGO_ID, 0] then
    Result := GetMangaGoInfoFromURL
  else
  if website = WebsiteRoots[MANGASTREAM_ID, 0] then
    Result := GetMangaStreamInfoFromURL
  else
  if website = WebsiteRoots[REDHAWKSCANS_ID, 0] then
    Result := GetRedHawkScansInfoFromURL
  else
  if website = WebsiteRoots[S2SCAN_ID, 0] then
    Result := GetS2scanInfoFromURL
  else
  if website = WebsiteRoots[EGSCANS_ID, 0] then
    Result := GetEGScansInfoFromURL
  else
  if website = WebsiteRoots[TRUYENTRANHTUAN_ID, 0] then
    Result := GetTruyenTranhTuanInfoFromURL
  else
  if website = WebsiteRoots[MANGAEDEN_ID, 0] then
    Result := GetMangaEdenInfoFromURL(WebsiteRoots[MANGAEDEN_ID, 1])
  else
  if website = WebsiteRoots[PERVEDEN_ID, 0] then
    Result := GetMangaEdenInfoFromURL(WebsiteRoots[PERVEDEN_ID, 1])
  else
  if website = WebsiteRoots[MEINMANGA_ID, 0] then
    Result := GetMeinMangaInfoFromURL
  else
  if website = WebsiteRoots[MANGA2U_ID, 0] then
    Result := GetManga2uInfoFromURL
  else
  if website = WebsiteRoots[SUBMANGA_ID, 0] then
    Result := GetSubMangaInfoFromURL
  else
  if website = WebsiteRoots[ESMANGAHERE_ID, 0] then
    Result := GetEsMangaHereInfoFromURL
  else
  if website = WebsiteRoots[ANIMEEXTREMIST_ID, 0] then
    Result := GetAnimeExtremistInfoFromURL
  else
  if website = WebsiteRoots[KOMIKID_ID, 0] then
    Result := GetKomikidInfoFromURL
  else
  if website = WebsiteRoots[PECINTAKOMIK_ID, 0] then
    Result := GetPecintaKomikInfoFromURL
  else
  if website = WebsiteRoots[MABUNS_ID, 0] then
    Result := GetMabunsInfoFromURL
  else
  if website = WebsiteRoots[MANGAESTA_ID, 0] then
    Result := GetMangaEstaInfoFromURL
  else
  if website = WebsiteRoots[PURURIN_ID, 0] then
    Result := GetPururinInfoFromURL
  else
  if website = WebsiteRoots[HUGEMANGA_ID, 0] then
    Result := GetHugeMangaInfoFromURL
  else
  if website = WebsiteRoots[ANIMESTORY_ID, 0] then
    Result := GetAnimeStoryInfoFromURL
  else
  if website = WebsiteRoots[LECTUREENLIGNE_ID, 0] then
    Result := GetLectureEnLigneInfoFromURL
  else
  if website = WebsiteRoots[SCANMANGA_ID, 0] then
    Result := GetScanMangaInfoFromURL
  else
  if website = WebsiteRoots[TURKCRAFT_ID, 0] then
    Result := GetTurkcraftInfoFromURL
  else
  if website = WebsiteRoots[MANGAFRAME_ID, 0] then
    Result := GetMangaframeInfoFromURL
  else
  if website = WebsiteRoots[MANGAVADISI_ID, 0] then
    Result := GetMangaVadisiInfoFromURL
  else
  if website = WebsiteRoots[MANGAAR_ID, 0] then
    Result := GetMangaArInfoFromURL
  else
  if website = WebsiteRoots[MANGAAE_ID, 0] then
    Result := GetMangaAeInfoFromURL
  else
  if website = WebsiteRoots[CENTRALDEMANGAS_ID, 0] then
    Result := GetCentralDeMangasInfoFromURL
  else
  if website = WebsiteRoots[MANGACOW_ID, 0] then
    Result := GetMangaCowInfoFromURL
  else
  if website = WebsiteRoots[SENMANGA_ID, 0] then
    Result := GetSenMangaInfoFromURL
  else
  if website = WebsiteRoots[BLOGTRUYEN_ID, 0] then
    Result := GetBlogTruyenInfoFromURL
  else
  if website = WebsiteRoots[KIVMANGA_ID, 0] then
    Result := GetKivmangaInfoFromURL
  else
  if website = WebsiteRoots[MANGACAN_ID, 0] then
    Result := GetMangacanInfoFromURL
  else
  if website = WebsiteRoots[MANGASPROJECT_ID, 0] then
    Result := GetMangasPROJECTInfoFromURL
  else
  if website = WebsiteRoots[MANGAREADER_POR_ID, 0] then
    Result := GetMangaREADER_PORInfoFromURL
  else
  if website = WebsiteRoots[EHENTAI_ID, 0] then
    Result := GetEHentaiInfoFromURL
  else
  if website = WebsiteRoots[MANGASTREAMTO_ID, 0] then
    Result := GetMangaStreamToInfoFromURL
  else
  if (website = WebsiteRoots[NINEMANGA_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_ES_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_CN_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_RU_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_DE_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_IT_ID, 0]) or
    (website = WebsiteRoots[NINEMANGA_BR_ID, 0]) then
    Result := GetNineMangaInfoFromURL
  else
  if website = WebsiteRoots[JAPANSHIN_ID, 0] then
    Result := GetJapanShinInfoFromURL
  else
  if website = WebsiteRoots[JAPSCAN_ID, 0] then
    Result := GetJapscanInfoFromURL
  else
  if website = WebsiteRoots[CENTRUMMANGI_PL_ID, 0] then
    Result := GetCentrumMangi_PLInfoFromURL
  else
  if website = WebsiteRoots[MANGALIB_PL_ID, 0] then
    Result := GetMangaLib_PLInfoFromURL
  else
  if website = WebsiteRoots[ONEMANGA_ID, 0] then
    Result := GetOneMangaInfoFromURL
  else
  if website = WebsiteRoots[MANGATOWN_ID, 0] then
    Result := GetMangaTownInfoFromURL
  else
  if website = WebsiteRoots[READHENTAIMANGA_ID, 0] then
    Result := GetReadHentaiMangaInfoFromURL
  else
  if website = WebsiteRoots[MANGAOKU_ID, 0] then
    Result := GetMangaOkuInfoFromURL
  else
  if website = WebsiteRoots[MYREADINGMANGAINFO_ID, 0] then
    Result := GetMyReadingMangaInfoInfoFromURL
  else
  if website = WebsiteRoots[IKOMIK_ID, 0] then
    Result := GetIKomikInfoFromURL
  else
  if website = WebsiteRoots[NHENTAI_ID, 0] then
    Result := GetNHentaiInfoFromURL
  else
  if website = WebsiteRoots[UNIONMANGAS_ID, 0] then
    Result := GetUnionMangasInfoFromURL
  else
  if website = WebsiteRoots[MANGAMINT_ID, 0] then
    Result := GetMangaMintInfoFromURL
  else
  if website = WebsiteRoots[UNIXMANGA_ID, 0] then
    Result := GetUnixMangaInfoFromURL
  else
  if website = WebsiteRoots[HAKIHOME_ID, 0] then
    Result := GetHakiHomeInfoFromURL
  else
  if website = WebsiteRoots[EXTREMEMANGAS_ID, 0] then
    Result := GetExtremeMangasInfoFromURL
  else
  if website = GetMangaSiteName(MANGAHOST_ID) then
    Result := GetMangaHostInfoFromURL
  else
  if (website = GetMangaSiteName(PORNCOMIX_ID)) or
    (website = GetMangaSiteName(XXCOMICS_ID)) or
    (website = GetMangaSiteName(XXCOMICSMT_ID)) or
    (website = GetMangaSiteName(XXCOMICS3D_ID)) or
    (website = GetMangaSiteName(PORNCOMIXRE_ID)) or
    (website = GetMangaSiteName(PORNCOMIXIC_ID)) or
    (website = GetMangaSiteName(PORNXXXCOMICS_ID)) then
    Result := GetPornComixInfoFromURL(GetMangaSiteID(website))
  else
  if website = GetMangaSiteName(MANGASEE_ID) then
    Result := GetMangaSeeInfoFromURL
  else
  if website = GetMangaSiteName(MANGAKU_ID) then
    Result := GetMangaKuInfoFromURL
  else
  if website = GetMangaSiteName(ACADEMYVN_ID) then
    Result := GetAcademyVNInfoFromURL
  else
  if website = GetMangaSiteName(MANGAAT_ID) then
    Result := GetMangaAtInfoFromURL
  else
  if website = GetMangaSiteName(SENMANGARAW_ID) then
    Result := GetSenMangaRAWInfoFromURL
  else
  if website = GetMangaSiteName(READMANGATODAY_ID) then
    Result := GetReadMangaTodayInfoFromURL
  else
  if website = GetMangaSiteName(LONEMANGA_ID) then
    Result := GetLoneMangaInfoFromURL
  else
  if website = GetMangaSiteName(DYNASTYSCANS_ID) then
    Result := GetDynastyScansInfoFromURL
  else
  if website = GetMangaSiteName(MADOKAMI_ID) then
    Result := GetMadokamiInfoFromURL
  else
  if SitesIsWPManga(WebsiteID) then
    Result := GetWPMangaInfoFromURL
  else
  begin
    Source.Free;
    Result := INFORMATION_NOT_FOUND;
    Exit;
  end;

  s := mangaInfo.artists;
  if (s <> '') and (s[1] = '<') then
    mangaInfo.artists := '';
  s := mangaInfo.authors;
  if (s <> '') and (s[1] = '<') then
    mangaInfo.authors := '';

  // check everything once more
  mangaInfo.title := Trim(RemoveStringBreaks(CommonStringFilter(mangaInfo.title)));
  mangaInfo.authors := Trim(RemoveStringBreaks(Trim(mangaInfo.authors)));
  mangaInfo.artists := Trim(RemoveStringBreaks(Trim(mangaInfo.artists)));
  mangaInfo.genres := Trim(RemoveStringBreaks(Trim(mangaInfo.genres)));
  mangaInfo.authors := TrimRightChar(Trim(mangaInfo.authors), [',']);
  mangaInfo.artists := TrimRightChar(Trim(mangaInfo.artists), [',']);
  mangaInfo.genres := TrimRightChar(Trim(mangaInfo.genres), [',']);
  mangaInfo.summary := StringBreaks(mangaInfo.summary);
  mangaInfo.summary := Trim(TrimChar(mangaInfo.summary, [#13, #10]));
  mangaInfo.summary := BreaksString(mangaInfo.summary);
  // strip double CR
  mangaInfo.summary := Trim(StringReplace(mangaInfo.summary, '\n\r\n\r', '\n\r', [rfReplaceAll]));
  mangaInfo.summary := Trim(StringReplace(mangaInfo.summary, '\r\n\r\n', '\r\n', [rfReplaceAll]));

  // fix info
  rex := TRegExpr.Create;
  try
    rex.Expression := '^[\-\:]$';
    if rex.Exec(mangaInfo.authors) then
      mangaInfo.authors := '';
    if rex.Exec(mangaInfo.artists) then
      mangaInfo.artists := '';
    if rex.Exec(mangaInfo.summary) then
      mangaInfo.summary := '';
    rex.Expression := '\<\/?\w\>';
    if rex.Exec(LowerCase(mangaInfo.summary)) then
      mangaInfo.summary := '';
  finally
    rex.Free;
  end;

  // remove duplicate chapter
  if mangaInfo.chapterLinks.Count > 0 then
  begin
    j := 0;
    while j < (mangaInfo.chapterLinks.Count - 1) do
    begin
      del := False;
      if (j + 1) < mangaInfo.chapterLinks.Count then
        for k := j + 1 to mangaInfo.chapterLinks.Count - 1 do
        begin
          if SameText(mangaInfo.chapterLinks[j], mangaInfo.chapterLinks[k]) then
          begin
            mangaInfo.chapterLinks.Delete(j);
            mangaInfo.chapterName.Delete(j);
            del := True;
            Break;
          end;
        end;
      if not del then
        Inc(j);
    end;
  end;

  if mangaInfo.chapterLinks.Count > 0 then
  begin
    // remove host from chapter links
    RemoveHostFromURLsPair(mangaInfo.chapterLinks, mangaInfo.chapterName);
    // fixing chapter name
    for j := 0 to mangaInfo.chapterName.Count - 1 do
    begin
      mangaInfo.chapterName.Strings[j] := Trim(RemoveStringBreaks(
        CommonStringFilter(mangaInfo.chapterName[j])));
    end;
  end;

  mangaInfo.numChapter := mangaInfo.chapterLinks.Count;
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
      [DataProcess.Param[index, DATA_PARAM_NAME],
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
      mangaInfo.title := DataProcess.Param[index, DATA_PARAM_NAME];
    DataProcess.Data.Strings[index] := SetParams(
      //[DataProcess.Param[index, DATA_PARAM_NAME],
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
  DataProcess.title.Add(l.Strings[DATA_PARAM_NAME]);
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

function TMangaInformation.GetPage(var output: TObject; URL: String;
  const Reconnect: Cardinal): Boolean;
begin
  Result := uBaseUnit.GetPage(FHTTP, output, URL, Reconnect);
end;

end.
