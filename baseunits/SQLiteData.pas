unit SQLiteData;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LazFileUtils, strutils, sqlite3conn, sqldb, SQLite3Dyn;

type

  { TSQLite3ConnectionH }

  TSQLite3ConnectionH = class(TSQLite3Connection)
  protected
    procedure DoInternalDisconnect; override;
  public
    property Handle read GetHandle;
    property Statements;
  end;

  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  { TSQliteData }

  TSQliteData = class
  private
    FAutoVacuum: Boolean;
    FConn: TSQLite3ConnectionH;
    FFieldsParams: String;
    FOnError: TExceptionEvent;
    FTrans: TSQLTransaction;
    FQuery: TSQLQuery;
    FFileName: String;
    FTableName: String;
    FCreateParams: String;
    FSelectParams: String;
    FRecordCount: Integer;
    procedure DoOnError(E: Exception);
    function GetAutoApplyUpdates: Boolean;
    procedure SetAutoApplyUpdates(AValue: Boolean);
    procedure SetAutoVacuum(AValue: Boolean);
    procedure SetCreateParams(AValue: String);
    procedure SetFieldsParams(AValue: String);
    procedure SetOnError(AValue: TExceptionEvent);
    procedure SetSelectParams(AValue: String);
  protected
    function OpenDB: Boolean; virtual;
    function CreateDB: Boolean; virtual;
    function ConvertNewTableIF: Boolean; virtual;
    procedure DoConvertNewTable;
    procedure GetRecordCount; virtual;
    procedure SetRecordCount(const AValue: Integer); virtual;
    procedure IncRecordCount(const N: Integer = 1);
    procedure Vacuum; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const AOpenTable: Boolean = True; const AGetRecordCount: Boolean = True): Boolean; virtual;
    function OpenTable(const AGetRecordCount: Boolean = True): Boolean; virtual;
    procedure Close; virtual;
    procedure CloseTable; virtual;
    procedure Refresh(RecheckDataCount: Boolean = False); virtual;
    procedure Commit; virtual;
    procedure CommitRetaining; virtual;
    procedure Save; virtual;
    function Connected: Boolean; inline;
    property Connection: TSQLite3ConnectionH read FConn;
    property Transaction: TSQLTransaction read FTrans;
    property Table: TSQLQuery read FQuery;
    property Filename: String read FFileName write FFileName;
    property TableName: String read FTableName write FTableName;
    property CreateParams: String read FCreateParams write SetCreateParams;
    property SelectParams: String read FSelectParams write SetSelectParams;
    property FieldsParams: String read FFieldsParams write SetFieldsParams;
    property RecordCount: Integer read FRecordCount;
    property AutoApplyUpdates: Boolean read GetAutoApplyUpdates write SetAutoApplyUpdates;
    property AutoVacuum: Boolean read FAutoVacuum write SetAutoVacuum;
    property OnError: TExceptionEvent read FOnError write SetOnError;
  end;

function QuotedStr(const S: Integer): String; overload; inline;
function QuotedStr(const S: Boolean): String; overload; inline;
function QuotedStr(const S: TDateTime): String; overload; inline;
function QuotedStrD(const S: String): String; overload; inline;
function QuotedStrD(const S: Integer): String; overload; inline;

implementation


function QuotedStr(const S: Integer): String;
begin
  Result := AnsiQuotedStr(IntToStr(S), '''');
end;

function QuotedStr(const S: Boolean): String;
begin
  Result := AnsiQuotedStr(BoolToStr(S, '1', '0'), '''');
end;

function ToStrZeroPad(const i, len: Word): String;
begin
  Result:=IntToStr(i);
  if Length(Result)<len then
    Result:=StringOfChar('0',len-Length(Result))+Result;
end;

function DateTimeToSQLiteDateTime(const D: TDateTime): String;
var
  Year, Month, Day, Hour, Minute, Second, MiliSecond: word;
begin
  DecodeDate(D, Year, Month, Day);
  DecodeTime(D, Hour, Minute, Second, MiliSecond);
  Result := ToStrZeroPad(Year,4)+'-'+ToStrZeroPad(Month,2)+'-'+ToStrZeroPad(Day,2)+' '+
            ToStrZeroPad(Hour,2)+':'+ToStrZeroPad(Minute,2)+':'+ToStrZeroPad(Second,2)+'.'+ToStrZeroPad(MiliSecond,3);
end;

function QuotedStr(const S: TDateTime): String;
begin
  Result := AnsiQuotedStr(DateTimeToSQLiteDateTime(S), '"');
end;

function QuotedStrD(const S: String): String;
begin
  Result := AnsiQuotedStr(S, '"');
end;

function QuotedStrD(const S: Integer): String;
begin
  Result := AnsiQuotedStr(IntToStr(S), '"');
end;

{ TSQLite3ConnectionH }

procedure TSQLite3ConnectionH.DoInternalDisconnect;
var
  L: TList;
  i: Integer;
  lhandle: psqlite3;
begin
  L := Statements.LockList;
  try
    for i:=0 to L.Count-1 do
      TCustomSQLStatement(L[i]).Unprepare;
    L.Clear;
  finally
    Statements.UnlockList;
  end;
  lhandle:=Handle;
  if lhandle <> nil then
  begin
    checkerror(sqlite3_close_v2(lhandle));
    ReleaseSQLite;
  end;
end;

{ TSQliteData }

procedure TSQliteData.DoOnError(E: Exception);
begin
  if Assigned(OnError) then
    OnError(Self, E);
end;

function TSQliteData.GetAutoApplyUpdates: Boolean;
begin
  Result := sqoAutoApplyUpdates in FQuery.Options;
end;

procedure TSQliteData.SetAutoApplyUpdates(AValue: Boolean);
begin
  if AValue then
    FQuery.Options := FQuery.Options + [sqoAutoApplyUpdates]
  else
    FQuery.Options := FQuery.Options - [sqoAutoApplyUpdates];
end;

procedure TSQliteData.SetAutoVacuum(AValue: Boolean);
begin
  if FAutoVacuum = AValue then Exit;
  FAutoVacuum := AValue;
end;

procedure TSQliteData.SetCreateParams(AValue: String);
begin
  if FCreateParams = AValue then Exit;
  FCreateParams := TrimSet(Trim(AValue), ['(', ')', ';']);
end;

procedure TSQliteData.SetFieldsParams(AValue: String);
begin
  if FFieldsParams = AValue then Exit;
  FFieldsParams := AValue;
end;

procedure TSQliteData.SetOnError(AValue: TExceptionEvent);
begin
  if FOnError = AValue then Exit;
  FOnError := AValue;
end;

procedure TSQliteData.SetSelectParams(AValue: String);
begin
  if FSelectParams = AValue then Exit;
  FSelectParams := AValue;
end;

function TSQliteData.OpenDB: Boolean;
begin
  Result := False;
  if FFileName = '' then Exit;
  try
    FConn.DatabaseName := FFileName;
    FConn.Connected := True;
    FTrans.Active := True;
  except
  end;
  Result := FConn.Connected;
end;

function TSQliteData.CreateDB: Boolean;
begin
  Result := False;
  if (FTableName = '') or (FCreateParams = '') then Exit;
  if not FConn.Connected then
    if not OpenDB then Exit;
  try
    FConn.ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrD(FTableName));
    FConn.ExecuteDirect('CREATE TABLE ' + QuotedStrD(FTableName) + ' (' + FCreateParams + ')');
    FTrans.Commit;
    Result := True;
  except
  end;
end;

function TSQliteData.ConvertNewTableIF: Boolean;
begin
  Result := False;
end;

procedure TSQliteData.DoConvertNewTable;
var
  qactive: Boolean;
begin
  if not ConvertNewTableIF then Exit;
  if not FConn.Connected then Exit;
  try
    qactive := FQuery.Active;
    if FQuery.Active then FQuery.Close;
    with FConn do
    begin
      ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrD('temp' + FTableName));
      ExecuteDirect('CREATE TABLE ' + QuotedStrD('temp' + FTableName) + ' (' + FCreateParams + ')');
      ExecuteDirect('INSERT INTO ' + QuotedStrD('temp' + FTableName) + ' (' + FieldsParams + ') SELECT ' + FieldsParams + ' FROM "' + FTableName + '"');
      ExecuteDirect('DROP TABLE ' + QuotedStrD(FTableName));
      ExecuteDirect('ALTER TABLE ' + QuotedStrD('temp' + FTableName) + ' RENAME TO ' + QuotedStrD(FTableName));
    end;
    FTrans.Commit;
    if qactive <> FQuery.Active then
      FQuery.Active := qactive;
  except
    FTrans.Rollback;
  end;
end;

procedure TSQliteData.GetRecordCount;
begin
  if FQuery.Active then
  begin
    FQuery.Last;
    FRecordCount := FQuery.RecordCount;
    FQuery.Refresh;
  end
  else FRecordCount := 0;
end;

procedure TSQliteData.SetRecordCount(const AValue: Integer);
begin
  if FRecordCount = AValue then Exit;
  FRecordCount := AValue;
end;

procedure TSQliteData.IncRecordCount(const N: Integer);
begin
  Inc(FRecordCount, N);
end;

procedure TSQliteData.Vacuum;
var
  qactive: Boolean;
begin
  if not FConn.Connected then Exit;
  try
    qactive := FQuery.Active;
    if FQuery.Active then FQuery.Close;
    FConn.ExecuteDirect('END TRANSACTION');
    try
      FConn.ExecuteDirect('VACUUM');
    finally
      FConn.ExecuteDirect('BEGIN TRANSACTION');
      if FQuery.Active <> qactive then
        FQuery.Active := qactive;
    end;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

constructor TSQliteData.Create;
begin
  FConn := TSQLite3ConnectionH.Create(nil);
  FTrans := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FConn.CharSet := 'UTF8';
  FConn.Transaction := FTrans;
  FQuery.DataBase := FTrans.DataBase;
  FQuery.Transaction := FTrans;
  AutoApplyUpdates := True;
  FAutoVacuum := True;
  FRecordCount := 0;
  FFileName := '';
  FTableName := 'maintable';
  FCreateParams := '';
end;

destructor TSQliteData.Destroy;
begin
  Self.Close;
  FConn.Free;
  FQuery.Free;
  FTrans.Free;
  inherited Destroy;
end;

function TSQliteData.Open(const AOpenTable: Boolean;
  const AGetRecordCount: Boolean): Boolean;
begin
  Result := False;
  if (FFileName = '') or (FCreateParams = '') then Exit;
  if FileExistsUTF8(FFileName) then
    Result := OpenDB
  else
    Result := CreateDB;
  if Result and AOpenTable then
    Result := OpenTable(AGetRecordCount);
end;

function TSQliteData.OpenTable(const AGetRecordCount: Boolean): Boolean;
begin
  Result := False;
  if not FConn.Connected then Exit;
  try
    if FQuery.Active then FQuery.Close;
    if FSelectParams <> '' then
      FQuery.SQL.Text := FSelectParams
    else
      FQuery.SQL.Text := 'SELECT * FROM ' + QuotedStrD(FTableName);
    FQuery.Open;
    if AGetRecordCount then
      GetRecordCount
    else
      FRecordCount := FQuery.RecordCount;
  except
    on E: Exception do
      DoOnError(E);
  end;
  Result := FQuery.Active;
  if Result then DoConvertNewTable;
end;

procedure TSQliteData.Close;
begin
  if not FConn.Connected then Exit;
  try
    Save;
    if FAutoVacuum then
      Vacuum;
    CloseTable;
    FTrans.Active := False;
    FConn.Close;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

procedure TSQliteData.CloseTable;
begin
  if not FConn.Connected then Exit;
  if not FQuery.Active then Exit;
  try
    FQuery.Close;
    FRecordCount := 0;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

procedure TSQliteData.Refresh(RecheckDataCount: Boolean);
begin
  if not FConn.Connected then Exit;
  if FQuery.Active then
    FQuery.Refresh
  else
    FQuery.Open;
  if RecheckDataCount then
    GetRecordCount
  else
    FRecordCount := FQuery.RecordCount;
end;

procedure TSQliteData.Commit;
begin
  if not FConn.Connected then Exit;
  try
    Transaction.Commit;
  except
    Transaction.Rollback;
  end;
end;

procedure TSQliteData.CommitRetaining;
begin
  if not FConn.Connected then Exit;
  try
    Transaction.CommitRetaining;
  except
    Transaction.RollbackRetaining;
  end;
end;

procedure TSQliteData.Save;
var
  qactive: Boolean;
begin
  if not FConn.Connected then Exit;
  try
    qactive := FQuery.Active;
    if FQuery.Active then
    begin
      FQuery.ApplyUpdates;
      FQuery.Close;
    end;
    FTrans.Commit;
    if qactive <> qactive then
      FQuery.Active := FQuery.Active;
    if FQuery.Active then
      GetRecordCount;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

function TSQliteData.Connected: Boolean;
begin
  Result := FConn.Connected and FQuery.Active;
end;

end.
