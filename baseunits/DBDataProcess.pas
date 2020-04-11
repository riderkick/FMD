{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit DBDataProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, FMDOptions, MultiLog, sqlite3conn,
  sqlite3backup, sqlite3dyn, sqldb, DB, RegExpr;

type

  { TDBDataProcess }

  TDBDataProcess = class(TObject)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FQuery: TSQLQuery;
    FRegxp: TRegExpr;
    FWebsite: String;
    FTableName: String;
    FRecordCount: Integer;
    FFiltered: Boolean;
    FFilterAllSites: Boolean;
    FFilterApplied: Boolean;
    FAllSitesAttached: Boolean;
    FSitesList: TStringList;
    FAttachedSites: TStringList;
    FSQLSelect: String;
    FFilterSQL: String;
    FLinks: TStringList;
    FRecNo: Integer;
    function GetLinkCount: Integer;
    procedure ResetRecNo(Dataset: TDataSet);
    procedure GoToRecNo(const ARecIndex: Integer);
  protected
    procedure CreateTable;
    procedure ConvertNewTable;
    procedure VacuumTable;
    procedure GetRecordCount;
    procedure AddSQLCond(const sqltext: String; useOR: Boolean = False);
    procedure AddSQLSimpleFilter(const fieldname, Value: String;
      useNOT: Boolean = False; useOR: Boolean = False; useRegexp: Boolean = False);
    function GetConnected: Boolean;
    function InternalOpen(const FilePath: String = ''): Boolean;
    function GetWebsiteName(RecIndex: Integer): String;
    function GetValue(RecIndex, FieldIndex: Integer): String;
    function GetValueInt(RecIndex, FieldIndex: Integer): Integer;
    procedure AttachAllSites;
    procedure DetachAllSites;
    function ExecuteDirect(SQL: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(AWebsite: String): Boolean;
    function Open(AWebsite: String = ''): Boolean;
    function OpenTable(const ATableName: String = '';
      CheckRecordCount: Boolean = False): Boolean;
    function TableExist(const ATableName: String): Boolean;
    function Search(ATitle: String): Boolean;
    function CanFilter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const {%H-}minusDay: Integer;
      const haveAllChecked, searchNewManga: Boolean): Boolean;
    function Filter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean;
      useRegExpr: Boolean = False): Boolean;
    function WebsiteLoaded(const AWebsite: String): Boolean;
    function LinkExist(ALink: String): Boolean;

    procedure InitLocateLink;
    procedure DoneLocateLink;
    procedure CreateDatabase(AWebsite: String = '');
    procedure GetFieldNames(List: TStringList);
    procedure Close;
    procedure CloseTable;
    procedure Save;
    procedure Backup(AWebsite: String);
    procedure Refresh(RecheckDataCount: Boolean = False);
    function AddData(Const Title, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter, JDN: Integer): Boolean; overload;
    function AddData(Const Title, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter: Integer; JDN: TDateTime): Boolean; overload; inline;
    function UpdateData(Const Title, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter: Integer; AWebsite: String = ''): Boolean;
    function DeleteData(const RecIndex: Integer): Boolean;
    procedure Commit;
    procedure Rollback;
    procedure RemoveFilter;
    procedure Sort;

    property Website: String read FWebsite write FWebsite;
    property TableName: String read FTableName write FTableName;
    property Connected: Boolean read GetConnected;
    property RecordCount: Integer read FRecordCount;
    property Filtered: Boolean read FFiltered;
    property FilterAllSites: Boolean read FFilterAllSites write FFilterAllSites;
    property SitesList: TStringList read FSitesList write FSitesList;
    property WebsiteName[RecIndex: Integer]: String read GetWebsiteName;
    property Value[RecIndex, FieldIndex: Integer]: String read GetValue; default;
    property ValueInt[RecIndex, FieldIndex: Integer]: Integer read GetValueInt;
    property LinkCount: Integer read GetLinkCount;
    property Connection: TSQLite3Connection read FConn;
    property Transaction: TSQLTransaction read FTrans;
    property Table: TSQLQuery read FQuery;
  end;

const
  DBDataProcessParam = '"link","title","authors","artists","genres","status","summary","numchapter","jdn"';
  DBDataProcessParams: array [0..8] of ShortString =
    ('link', 'title', 'authors', 'artists', 'genres', 'status',
    'summary', 'numchapter', 'jdn');
  DBTempFieldWebsiteIndex = Length(DBDataProcessParams);
  DBDataProccesCreateParam =
    '"link" TEXT NOT NULL PRIMARY KEY,' +
    '"title" TEXT,' +
    '"authors" TEXT,' +
    '"artists" TEXT,' +
    '"genres" TEXT,' +
    '"status" TEXT,' +
    '"summary" TEXT,' +
    '"numchapter" INTEGER,' +
    '"jdn" INTEGER';

function DBDataFilePath(const AWebsite: String): String;
function DataFileExist(const AWebsite: String): Boolean;
procedure CopyDBDataProcess(const AWebsite, NWebsite: String);
function DeleteDBDataProcess(const AWebsite: String): Boolean;
procedure OverwriteDBDataProcess(const AWebsite, NWebsite: String);

implementation

uses
  uBaseUnit;

function NaturalCompareCallback({%H-}user: pointer; len1: longint;
  data1: pointer; len2: longint; data2: pointer): longint; cdecl;
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

function QuotedLike(const S: String): String;
begin
  Result := QuotedStr('%'+S+'%');
end;

function DBDataFilePath(const AWebsite: String): String;
begin
  Result := DATA_FOLDER + AWebsite + DBDATA_EXT;
end;

function DataFileExist(const AWebsite: String): Boolean;
begin
  if AWebsite = '' then
    Exit(False);
  Result := FileExistsUTF8(DATA_FOLDER + AWebsite + DATA_EXT) or
    FileExistsUTF8(DATA_FOLDER + AWebsite + DBDATA_EXT);
end;

procedure CopyDBDataProcess(const AWebsite, NWebsite: String);
begin
  if NWebsite = '' then
    Exit;
  if DataFileExist(AWebsite) then
  begin
    try
      CopyFile(DATA_FOLDER + AWebsite + DBDATA_EXT,
        DATA_FOLDER + NWebsite + DBDATA_EXT,
        [cffPreserveTime, cffOverwriteFile], True);
    except
      on E: Exception do
        Logger.SendException('CopyDBDataProcess.Error!', E);
    end;
  end;
end;

function DeleteDBDataProcess(const AWebsite: String): Boolean;
var
  tryc: Integer;
begin
  Result := not FileExistsUTF8(DATA_FOLDER + AWebsite + DBDATA_EXT);
  if Result = False then
  begin
    tryc := 0;
    while not DeleteFileUTF8(DATA_FOLDER + AWebsite + DBDATA_EXT) do
    begin
      if tryc > 3 then
        Break;
      Inc(tryc);
      Sleep(250);
    end;
    Result := not FileExistsUTF8(DATA_FOLDER + AWebsite + DBDATA_EXT);
  end;
end;

procedure OverwriteDBDataProcess(const AWebsite, NWebsite: String);
begin
  if FileExistsUTF8(DATA_FOLDER + NWebsite + DBDATA_EXT) then
  begin
    if DeleteDBDataProcess(AWebsite) then
      RenameFileUTF8(DATA_FOLDER + NWebsite + DBDATA_EXT,
        DATA_FOLDER + AWebsite + DBDATA_EXT);
  end;
end;

{ TDBDataProcess }

function TDBDataProcess.GetLinkCount: Integer;
begin
  if Assigned(FLinks) then
    Result := FLinks.Count
  else
    Result := 0;
end;

procedure TDBDataProcess.ResetRecNo(Dataset: TDataSet);
begin
  FRecNo := 0;
end;

procedure TDBDataProcess.GoToRecNo(const ARecIndex: Integer);
begin
  if FQuery.RecNo<>(ARecIndex+1) then
  begin
    FRecNo:=ARecIndex;
    FQuery.RecNo:=ARecIndex+1;
  end;
end;

procedure TDBDataProcess.CreateTable;
begin
  if FConn.Connected then
  begin
    FConn.ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrd(FTableName));
    FConn.ExecuteDirect('CREATE TABLE ' + QuotedStrd(FTableName) + ' (' +
      DBDataProccesCreateParam + ');');
    FTrans.Commit;
  end;
end;

procedure TDBDataProcess.ConvertNewTable;
begin
  if FQuery.Active = False then Exit;
  if  (FQuery.Fields[0].FieldName <> 'link') or
      (FieldTypeNames[FQuery.FieldByName('title').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[FQuery.FieldByName('link').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[FQuery.FieldByName('authors').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[FQuery.FieldByName('artists').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[FQuery.FieldByName('genres').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[FQuery.FieldByName('status').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[FQuery.FieldByName('summary').DataType] <> Fieldtypenames[ftMemo]) then
    try
      FQuery.Close;
      with fconn do begin
        ExecuteDirect('DROP TABLE IF EXISTS '+QuotedStr('temp'+FTableName));
        ExecuteDirect('CREATE TABLE '+QuotedStrd('temp'+FTableName)+ ' (' + DBDataProccesCreateParam + ')');
        ExecuteDirect('INSERT INTO '+QuotedStrd('temp'+FTableName)+' ('+DBDataProcessParam+') SELECT ' + DBDataProcessParam + ' FROM '+QuotedStrd(FTableName));
        ExecuteDirect('DROP TABLE '+QuotedStrd(FTableName));
        ExecuteDirect('ALTER TABLE '+QuotedStrd('temp'+FTableName)+' RENAME TO '+QuotedStrd(FTableName));
      end;
      FTrans.Commit;
      VacuumTable;
      FQuery.Open;
    except
      FTrans.Rollback;
    end;
end;

procedure TDBDataProcess.VacuumTable;
var
  queryactive: Boolean;
begin
  if FConn.Connected then
  begin
    queryactive := FQuery.Active;
    FQuery.Close;
    with FConn do
    begin
      ExecuteDirect('END TRANSACTION');
      try
        ExecuteDirect('VACUUM');
      except
        on E: Exception do
          Logger.SendException(Self.ClassName+'['+Website+'].VacuumTable.Error!', E);
      end;
      ExecuteDirect('BEGIN TRANSACTION');
    end;
    if FQuery.Active <> queryactive then
      FQuery.Active := queryactive;
  end;
end;

procedure TDBDataProcess.GetRecordCount;
var
  bsql, s: String;
  queryopen: Boolean;
begin
  FRecordCount := 0;
  queryopen := FQuery.Active;
  if queryopen then FQuery.Close;
  bsql := Trim(FQuery.SQL.Text);
  if UpperCase(LeftStr(bsql, 8)) = 'SELECT *' then
  begin
    s := 'SELECT COUNT("link") as recordcount' + Copy(bsql, 9, Length(bsql));
    FQuery.SQL.Text := s;
    FQuery.Open;
    if FQuery.Active then
    begin
      FRecordCount := FQuery.Fields[0].AsInteger;
      FQuery.Close;
    end;
  end;
  FQuery.SQL.Text := bsql;
  if FQuery.Active <> queryopen then
    FQuery.Active := queryopen;
end;

procedure TDBDataProcess.AddSQLCond(const sqltext: String; useOR: Boolean);
begin
  with FQuery.SQL do
  begin
    if Count > 0 then
      if (Strings[Count - 1] <> '(') and
        (UpCase(Trim(Strings[Count - 1])) <> 'WHERE') then
      begin
        if useOR then
          Add('OR')
        else
          Add('AND');
      end;
    Add(sqltext);
  end;
end;

procedure TDBDataProcess.AddSQLSimpleFilter(const fieldname, Value: String;
  useNOT: Boolean; useOR: Boolean; useRegexp: Boolean);
var
  svalue: String;
  scond: String;
begin
  svalue := LowerCase(Trim(Value));
  if (fieldname = '') or (svalue = '') then
    Exit;
  if useNOT then
    scond := ' NOT'
  else
    scond := '';
  if useRegexp then
    AddSQLCond(QuotedStrd(fieldname) + scond + ' REGEXP ' + QuotedStr(svalue), useOR)
  else
    AddSQLCond(QuotedStrd(fieldname) + scond + ' LIKE ' + QuotedLike(svalue), useOR);
end;

function TDBDataProcess.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TDBDataProcess.InternalOpen(const FilePath: String): Boolean;
begin
  Result := False;
  if FilePath <> '' then
    FConn.DatabaseName := FilePath;
  if FConn.DatabaseName = '' then
    Exit;
  try
    FConn.CharSet := 'UTF8';
    FConn.Connected := True;
    sqlite3_create_collation(FConn.Handle, PChar('NATCMP'), SQLITE_UTF8, nil,
      @NaturalCompareCallback);
    sqlite3_create_function(FConn.Handle, PChar('REGEXP'), 2, SQLITE_UTF8, FRegxp,
      @RegexCallback, nil, nil);
    FTrans.Active := True;
  except
    on E: Exception do
    begin
      Logger.SendException(Self.ClassName+'['+Website+'].InternalOpen.Error!', E);
      Result := False;
    end;
  end;
  Result := FConn.Connected;
end;

function TDBDataProcess.GetWebsiteName(RecIndex: Integer): String;
begin
  Result:=FWebsite;
  if FAllSitesAttached then
    try
      FQuery.RecNo:=RecIndex+1;
      Result:=FQuery.Fields[DBTempFieldWebsiteIndex].AsString;
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].GetWebsiteName Error!'+
        'RecIndex: '+IntToStr(RecIndex), E);
    end;
end;

function TDBDataProcess.GetValue(RecIndex, FieldIndex: Integer): String;
begin
  if FieldIndex in [DATA_PARAM_NUMCHAPTER,DATA_PARAM_JDN] then
    Result:='0'
  else
    Result:='';
  if FQuery.Active=False then Exit;
  try
    GoToRecNo(RecIndex);
    Result:=FQuery.Fields[FieldIndex].AsString;
  except
  end;
end;

function TDBDataProcess.GetValueInt(RecIndex, FieldIndex: Integer): Integer;
begin
  Result:=0;
  if FQuery.Active=False then Exit;
  if not (FieldIndex in [DATA_PARAM_NUMCHAPTER,DATA_PARAM_JDN]) then
    Exit;
  try
    GoToRecNo(RecIndex);
    Result:=FQuery.Fields[FieldIndex].AsInteger;
  except
  end;
end;

procedure TDBDataProcess.AttachAllSites;

  procedure RemoveCurrentSite;
  var
    j: Integer;
  begin
    if SitesList.Count > 0 then
      for j := 0 to SitesList.Count - 1 do
        if SitesList[j] = FWebsite then
        begin
          SitesList.Delete(j);
          Break;
        end;
  end;

var
  i: Integer;
begin
  RemoveCurrentSite;
  if (not FConn.Connected) or (SitesList.Count = 0) then Exit;
  if Trim(SitesList.Text) = Trim(FAttachedSites.Text) then Exit;
  DetachAllSites;
  FConn.ExecuteDirect('END TRANSACTION');
  try
    for i:=0 to SitesList.Count-1 do begin
      //max attached database is 10
      //if FAttachedSites.Count=10 then Break;
      if (FAttachedSites.IndexOf(SitesList[i]) = -1) and
        (FileExistsUTF8(DBDataFilePath(SitesList[i]))) then
      begin
        FConn.ExecuteDirect('ATTACH ' +
          QuotedStr(DBDataFilePath(SitesList[i])) + ' AS ' + QuotedStrd(SitesList[i]));
        FAttachedSites.Add(SitesList[i]);
      end;
    end;
  except
    on E: Exception do
      Logger.SendException(Self.ClassName+'['+Website+'].AttachAllSites.Error!'+
        ' try to attach '+QuotedStr(SitesList[i]), E)
  end;
  FConn.ExecuteDirect('BEGIN TRANSACTION');
  FAllSitesAttached := FAttachedSites.Count > 0;
end;

procedure TDBDataProcess.DetachAllSites;
var
  i: Integer;
  queryactive: Boolean;
begin
  if (not FConn.Connected) or (FAttachedSites.Count = 0) then Exit;
  queryactive := FQuery.Active;
  if FQuery.Active then FQuery.Close;
  FTrans.Commit;
  FConn.ExecuteDirect('END TRANSACTION');
  for i := FAttachedSites.Count - 1 downto 0 do begin
    try
      FConn.ExecuteDirect('DETACH ' + QuotedStrd(FAttachedSites[i]));
      FAttachedSites.Delete(i);
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].DetachAllSites.Error!', E);
    end;
  end;
  FConn.ExecuteDirect('BEGIN TRANSACTION');
  FAllSitesAttached := FAttachedSites.Count > 0;
  if FQuery.Active <> queryactive then FQuery.Active := queryactive;
end;

function TDBDataProcess.ExecuteDirect(SQL: String): Boolean;
begin
  Result := False;
  if FConn.Connected then
    try
      FConn.ExecuteDirect(SQL);
      Result := True;
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].ExecuteDirect.Error!'#13#10 +
          'SQL: ' + SQL, E);
    end;
end;

constructor TDBDataProcess.Create;
begin
  inherited Create;
  FConn := TSQLite3Connection.Create(nil);
  FTrans := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FConn.Transaction := FTrans;
  FQuery.PacketRecords := 25;
  FQuery.DataBase := FTrans.DataBase;
  FQuery.Transaction := FTrans;
  FRegxp := TRegExpr.Create;
  FRegxp.ModifierI := True;
  FSitesList := TStringList.Create;
  FAttachedSites := TStringList.Create;
  FTableName := 'masterlist';
  FSQLSelect := 'SELECT * FROM ' + QuotedStrd(FTableName);
  FRecordCount := 0;
  FFiltered := False;
  FFilterAllSites := False;
  FFilterApplied := False;
  FFilterSQL := '';
  FAllSitesAttached := False;

  ResetRecNo(nil);
  FQuery.AfterOpen := @ResetRecNo;
  FQuery.AfterInsert := @ResetRecNo;
  FQuery.AfterDelete := @ResetRecNo;
  FQuery.AfterEdit := @ResetRecNo;
  FQuery.AfterRefresh := @ResetRecNo;
end;

destructor TDBDataProcess.Destroy;
begin
  try
    if FConn.Connected then
    begin
      FQuery.Close;
      Commit;
      Close;
    end;
  except
    on E: Exception do
      Logger.SendException(Self.ClassName+'['+Website+'].Destroy.Error!', E);
  end;
  DoneLocateLink;
  FAttachedSites.Free;
  FSitesList.Free;
  FQuery.Free;
  FTrans.Free;
  FConn.Free;
  FRegxp.Free;
  inherited Destroy;
end;

function TDBDataProcess.Connect(AWebsite: String): Boolean;
var
  filepath: String;
begin
  Result := False;
  if AWebsite <> '' then
    FWebsite := AWebsite;
  if FWebsite = '' then
    Exit;
  filepath := DATA_FOLDER + FWebsite + DBDATA_EXT;
  if not FileExistsUTF8(filepath) then
    Exit;
  Result := InternalOpen(filepath);
end;

function TDBDataProcess.Open(AWebsite: String): Boolean;
var
  filepath: String;
begin
  Result := False;
  Self.Close;
  if AWebsite <> '' then
    FWebsite := AWebsite;
  if FWebsite = '' then
    Exit;
  filepath := DATA_FOLDER + FWebsite + DBDATA_EXT;
  if not FileExistsUTF8(filepath) then
    Exit;
  try
    if InternalOpen(filepath) then
    begin
      if not TableExist(FTableName) then
        CreateTable;
      OpenTable(FTableName, True);
    end;
    Result := FQuery.Active;
  except
    on E: Exception do
      Logger.SendException(Self.ClassName+'['+Website+'].Open.Error!', E);
  end;
end;

function TDBDataProcess.OpenTable(const ATableName: String;
  CheckRecordCount: Boolean): Boolean;
begin
  Result := False;
  if FConn.Connected then
  begin
    try
      if ATableName <> '' then
        FTableName := ATableName;
      if FTableName = '' then
        Exit;
      if TableExist(FTableName) then
      begin
        if FQuery.Active then
          FQuery.Close;
        if FTrans.Active=False then FTrans.Active:=True;
        FSQLSelect := 'SELECT * FROM ' + QuotedStrd(FTableName);
        FQuery.SQL.Text := FSQLSelect;
        if CheckRecordCount then
          GetRecordCount;
        FQuery.Open;
      end;
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].OpenTable.Error!', E);
    end;
  end;
  Result := FQuery.Active;
  if Result then ConvertNewTable;
end;

function TDBDataProcess.TableExist(const ATableName: String): Boolean;
var
  ts: TStringList;
  i: Integer;
begin
  Result := False;
  if FConn.Connected then
  begin
    ts := TStringList.Create;
    try
      FConn.GetTableNames(ts);
      ts.Sorted := True;
      Result := ts.Find(ATableName, i);
    finally
      ts.Free;
    end;
  end;
end;

procedure TDBDataProcess.Close;
begin
  FRecordCount := 0;
  if FConn.Connected then
    try
      FQuery.Close;
      RemoveFilter;
      FConn.Close;
      FConn.DatabaseName := '';
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].Close.Error!', E);
    end;
end;

procedure TDBDataProcess.CloseTable;
begin
  if FQuery.Active then
  begin
    FRecordCount := 0;
    RemoveFilter;
    FQuery.Close;
  end;
end;

procedure TDBDataProcess.Save;
begin
  Commit;
end;

procedure TDBDataProcess.Backup(AWebsite: String);
begin
  if AWebsite = '' then
    Exit;
  if FConn.Connected then
  begin
    with TSQLite3Backup.Create do
      try
        Backup(FConn, DATA_FOLDER + AWebsite + DBDATA_EXT);
      finally
        Free;
      end;
  end;
end;

procedure TDBDataProcess.Refresh(RecheckDataCount: Boolean);
begin
  if FConn.Connected then
  begin
    if FQuery.Active then begin
      if RecheckDataCount then
        GetRecordCount;
      FQuery.Refresh;
    end
    else
    if Trim(FQuery.SQL.Text) <> '' then
    begin
      if RecheckDataCount then
        GetRecordCount;
      FQuery.Open;
    end;
  end;
end;

function TDBDataProcess.AddData(const Title, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter, JDN: Integer): Boolean;
begin
  Result:=False;
  if Link='' then Exit;
  if FConn.Connected=False then Exit;
  try
    FConn.ExecuteDirect(
      'INSERT INTO '+QuotedStrd(FTableName)+' ('+DBDataProcessParam+') VALUES ('+
      QuotedStr(Link)+', '+
      QuotedStr(Title)+', '+
      QuotedStr(Authors)+', '+
      QuotedStr(Artists)+', '+
      QuotedStr(Genres)+', '+
      QuotedStr(Status)+', '+
      QuotedStr(Summary)+', '+
      QuotedStr(IntToStr(NumChapter))+', '+
      QuotedStr(IntToStr(JDN))+');');
    Result:=True;
  except
  end;
end;

function TDBDataProcess.AddData(const Title, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter: Integer; JDN: TDateTime): Boolean;
begin
  Result := AddData(Title, Link, Authors, Artists, Genres, Status, Summary,
    NumChapter, DateToJDN(JDN));
end;

function TDBDataProcess.UpdateData(const Title, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter: Integer; AWebsite: String): Boolean;
var
  sql: String;
begin
  Result:=False;
  if Link='' then Exit;
  if FConn.Connected=False then Exit;
  try
    sql:='UPDATE ';
    if (AWebsite<>'') and (AWebsite<>FWebsite) and FAllSitesAttached then
      sql+=QuotedStrd(AWebsite)+'.'+QuotedStrd(FTableName)
    else
      sql+=QuotedStrd(FTableName);
    sql+=' SET "title"='+QuotedStr(Title)+
         ', "authors"='+QuotedStr(Authors)+
         ', "artists"='+QuotedStr(Artists)+
         ', "genres"='+QuotedStr(Genres)+
         ', "status"='+QuotedStr(Status)+
         ', "summary"='+QuotedStr(Summary)+
         ', "numchapter"='+QuotedStr(IntToStr(NumChapter))+
         ' WHERE ("link"='+QuotedStr(Link)+');';
    FConn.ExecuteDirect(sql);
    Result:=True;
  except
  end;
end;

function TDBDataProcess.DeleteData(const RecIndex: Integer): Boolean;
begin
  Result := False;
  try
    GoToRecNo(RecIndex);
    FQuery.Delete;
    Dec(FRecordCount);
    Result := True;
  except
    on E: Exception do
      Logger.SendException(ClassName+'['+Website+'].DeleteData.Error!',E);
  end;
end;

procedure TDBDataProcess.Commit;
var
  queryactive: Boolean;
begin
  if FConn.Connected then
    try
      queryactive := FQuery.Active;
      if FQuery.Active then FQuery.Close;
      FTrans.Commit;
      if FQuery.Active <> queryactive then
        FQuery.Active := queryactive;
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].Commit.Error!',E);
    end;
end;

procedure TDBDataProcess.Rollback;
begin
  if FConn.Connected then
    try
      FTrans.Rollback;
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].Rollback.Error!',E);
    end;
end;

function TDBDataProcess.Search(ATitle: String): Boolean;
var
  i: Integer;
begin
  if FQuery.Active then
  begin
    try
      FQuery.Close;
      with FQuery do
      begin
        SQL.Clear;
        if FFilterApplied then
          SQL.AddText(FFilterSQL)
        else
          SQL.Add(FSQLSelect);
        if ATitle <> '' then
        begin
          if not FFilterApplied then
            SQL.Add('WHERE');
          if FAllSitesAttached then
          begin
            if SQL.Count > 0 then
            begin
              i := 0;
              while i < SQL.Count do
              begin
                if (SQL[i] = 'UNION ALL') or (SQL[i] = ')') then
                begin
                  SQL.Insert(i, 'AND');
                  SQL.Insert(i + 1, '"title" LIKE ' + QuotedLike(ATitle));
                  Inc(i, 3);
                end
                else
                  Inc(i);
              end;
            end;
          end
          else
            AddSQLSimpleFilter('title', ATitle);
          FFiltered := True;
        end
        else
          FFiltered := FFilterApplied;
      end;
      GetRecordCount;
      FQuery.Open;
    except
      on E: Exception do
        Logger.SendException(Self.ClassName+'['+Website+'].Search.Error!'#13#10 +
          'SQL:'#13#10 + FQuery.SQL.Text, E);
    end;
  end;
  Result := FQuery.Active;
  if not Result then
  begin
    FFiltered := False;
    FRecordCount := 0;
  end;
end;

function TDBDataProcess.CanFilter(const checkedGenres, uncheckedGenres: TStringList;
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
  const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean): Boolean;
begin
  Result := False;
  if not FQuery.Active then
    Exit;
  if ((stTitle = '') and
    (stAuthors = '') and
    (stArtists = '') and
    (stSummary = '') and
    (stStatus = '2') and
    (checkedGenres.Count = 0) and
    (uncheckedGenres.Count = 0)) and
    (not searchNewManga) and
    haveAllChecked then
    Result := False
  else
    Result := True;
end;

function TDBDataProcess.Filter(const checkedGenres, uncheckedGenres: TStringList;
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
  const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean;
  useRegExpr: Boolean): Boolean;
var
  tsql: String;
  i: Integer;
  filtersingle: Boolean = True;

  procedure GenerateSQLFilter;
  var
    j: Integer;
  begin
    // filter new manga based on date
    if searchNewManga then
      AddSQLCond('"jdn" > ' +
        QuotedStrd(IntToStr(DateToJDN(Now)-minusDay)));

    // filter title
    AddSQLSimpleFilter('title', stTitle, False, False, useRegExpr);

    // filter authors
    AddSQLSimpleFilter('authors', stAuthors, False, False, useRegExpr);

    // filter artists
    AddSQLSimpleFilter('artists', stArtists, False, False, useRegExpr);

    // filter summary
    AddSQLSimpleFilter('summary', stSummary, False, False, useRegExpr);

    // filter status
    if stStatus <> '2' then
      AddSQLCond('"status"=' + QuotedStrd(stStatus));

    //filter checked genres
    if checkedGenres.Count > 0 then
    begin
      AddSQLCond('(');
      for j := 0 to checkedGenres.Count - 1 do
        AddSQLSimpleFilter('genres', checkedGenres[j], False,
          (not haveAllChecked), useRegExpr);
      FQuery.SQL.Add(')');
    end;

    //filter unchecked genres
    if uncheckedGenres.Count > 0 then
    begin
      AddSQLCond('(');
      for j := 0 to uncheckedGenres.Count - 1 do
        AddSQLSimpleFilter('genres', uncheckedGenres[j], True,
          (not haveAllChecked), useRegExpr);
      FQuery.SQL.Add(')');
    end;
  end;

begin
  Result := False;
  if FQuery.Active = False then
    Exit;
  if not CanFilter(checkedGenres, uncheckedGenres, stTitle, stAuthors,
    stArtists, stStatus, stSummary, minusDay, haveAllChecked, searchNewManga) then
    Exit;
  with FQuery do
  begin
    FQuery.Close;
    FRecordCount := 0;
    tsql := SQL.Text;
    SQL.Clear;
    try
      if FFilterAllSites and (FSitesList.Count > 0) then
      begin
        AttachAllSites;
        if FAttachedSites.Count > 0 then
        begin
          SQL.Add('SELECT * FROM');
          SQL.Add('(');
          SQL.Add('SELECT *, ' + QuotedStrd(FWebsite) + ' AS "website" FROM ' +
            QuotedStrd(FTableName));
          SQL.Add('WHERE');
          GenerateSQLFilter;
          for i := 0 to FAttachedSites.Count - 1 do
          begin
            SQL.Add('UNION ALL');
            SQL.Add('SELECT *, ' + QuotedStrd(FAttachedSites[i]) +
              ' AS "website" FROM ' +
              QuotedStrd(FAttachedSites[i]) + '.' + QuotedStrd(FTableName));
            SQL.Add('WHERE');
            GenerateSQLFilter;
          end;
          SQL.Add(')');
          SQL.Add('ORDER BY "title" COLLATE NATCMP');
          filtersingle := False;
        end;
      end;

      if filtersingle then
      begin
        SQL.Add(FSQLSelect);
        SQL.Add('WHERE');
        GenerateSQLFilter;
      end;

      Self.GetRecordCount;
      FQuery.Open;
      FFiltered := Active;
      FFilterApplied := FFiltered;
      if FFilterApplied then
        FFilterSQL := SQL.Text
      else
        FFilterSQL := '';
    except
      on E: Exception do
      begin
        Logger.SendException(Self.ClassName+'['+Website+'].Filter.Error!'#13#10 +
          'SQL:'#13#10 + FQuery.SQL.Text, E);
        FQuery.Close;
        SQL.Text := tsql;
        Self.GetRecordCount;
        FQuery.Open;
        FFilterAllSites := False;
        FFiltered := False;
        FFilterApplied := False;
        FFilterSQL := '';
      end;
    end;
    Result := FFiltered;
    Result := FFiltered;
  end;
end;

procedure TDBDataProcess.CreateDatabase(AWebsite: String);
var
  filepath: String;
begin
  if AWebsite <> '' then FWebsite := AWebsite;
  if FWebsite = '' then Exit;
  Close;
  filepath := DATA_FOLDER + FWebsite + DBDATA_EXT;
  if FileExistsUTF8(filepath) then
    DeleteFileUTF8(filepath);
  if ForceDirectoriesUTF8(DATA_FOLDER) then
  begin
    InternalOpen(filepath);
    CreateTable;
  end;
end;

procedure TDBDataProcess.GetFieldNames(List: TStringList);
begin
  if (List <> nil) and (FQuery.Active) then
    FQuery.GetFieldNames(List);
end;

procedure TDBDataProcess.RemoveFilter;
begin
  if FFiltered then
  begin
    FFilterAllSites := False;
    FFiltered := False;
    FFilterApplied := False;
    FFilterSQL := '';
    FQuery.SQL.Text := FSQLSelect;
    FRecordCount := 0;
    DetachAllSites;
    if FQuery.Active then
      OpenTable(FTableName, True);
  end;
end;

procedure TDBDataProcess.Sort;
var
  queryactive: Boolean;
begin
  if FConn.Connected then
  begin
    queryactive := FQuery.Active;
    FQuery.Close;
    with FConn do
      try
        ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrd(FTableName + '_ordered'));
        ExecuteDirect('CREATE TABLE ' + QuotedStrd(FTableName + '_ordered') + ' (' + DBDataProccesCreateParam +')');
        ExecuteDirect('INSERT INTO '+QuotedStrd(FTableName + '_ordered') + ' (' + DBDataProcessParam + ') SELECT '+ DBDataProcessParam +' FROM ' + QuotedStrd(FTableName) + ' ORDER BY "title" COLLATE NATCMP');
        ExecuteDirect('DROP TABLE ' + QuotedStrd(FTableName));
        ExecuteDirect('ALTER TABLE ' + QuotedStrd(FTableName + '_ordered') + ' RENAME TO ' + QuotedStrd(FTableName));
        FTrans.Commit;
        VacuumTable;
      except
        on E: Exception do
          Logger.SendException(Self.ClassName+'['+Website+'].Sort.Error!', E);
      end;
    if FQuery.Active <> queryactive then
      FQuery.Active := queryactive;
  end;
end;

function TDBDataProcess.WebsiteLoaded(const AWebsite: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FWebsite = AWebsite then
    Exit(True);
  if FAllSitesAttached then
    for i := 0 to FAttachedSites.Count - 1 do
      if FAttachedSites[i] = AWebsite then
      begin
        Result := True;
        Break;
      end;
end;

function TDBDataProcess.LinkExist(ALink: String): Boolean;
var
  i: Integer;
begin
  if Assigned(FLinks) then
    Result := FLinks.Find(ALink, i)
  else
    Result := False;
end;

procedure TDBDataProcess.InitLocateLink;
begin
  if Assigned(FLinks) then
    FLinks.Clear
  else
    FLinks := TStringList.Create;
  FLinks.Sorted := False;
  if FQuery.Active then
  begin
    FQuery.First;
    repeat
      FLinks.Add(FQuery.Fields[1].AsString);
      FQuery.Next;
    until FQuery.EOF;
    if FLinks.Count > 0 then
      FLinks.Sorted := True;
  end;
end;

procedure TDBDataProcess.DoneLocateLink;
begin
  if Assigned(FLinks) then
    FreeAndNil(FLinks);
end;

end.
