unit accountmanagerdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, USimpleLogger, base64, sqlite3conn,
  sqldb;

type

  { TAccountManager }

  TAccountManager = class
  private
    locklocate: TRTLCriticalSection;
    fconn: TSQLite3Connection;
    ftrans: TSQLTransaction;
    fquery: TSQLQuery;
    ffilename: String;
    frecordcount: Integer;

    function GetUsername(AName: string): string;
    function GetEnabled(AName: string): boolean;
    function GetPassword(AName: string): string;
    function GetCookies(AName: string): string;
    function GetStatus(AName: string): string;
    procedure SetUsername(AName: string; AValue: string);
    procedure SetEnabled(AName: string; AValue: boolean);
    procedure SetPassword(AName: string; AValue: string);
    procedure SetCookies(AName: string; AValue: string);
    procedure SetStatus(AName: string; AValue: string);

    function CreateDB: Boolean;
    function InternalOpenDB: Boolean;
    function CreateDBTable: Boolean;
    function OpenDBTable: Boolean;
    function GetValueString(const AName, AField: string): string;
    function GetValueBoolean(const AName, AField: string): boolean;
    procedure SetValueString(const AName, AField, AValue: string);
    procedure SetValueBoolean(const AName, AField: string; AValue: boolean);
    procedure GetRecordCount;
    procedure Vacuum;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    procedure Save;
    function AddAccount(const AName, AUsername, APassword: string): Boolean;
    function DeleteAccount(const AName: string): Boolean;
    property Enabled[AName: string]: boolean read GetEnabled write SetEnabled;
    property Username[AName: string]: string read GetUsername write SetUsername;
    property Password[AName: string]: string read GetPassword write SetPassword;
    property Cookies[AName: string]: string read GetCookies write SetCookies;
    property Status[AName: string]: string read GetStatus write SetStatus;
    property Count: Integer read frecordcount;
  end;

  procedure InitAccountManager(const AFilename: string);

var
  Account: TAccountManager;

implementation

const
  ctablename = 'accounts';
  ctbaccountscreateparams = '('#13#10 +
    '"aname" VARCHAR NOT NULL PRIMARY KEY,'#13#10 +
    '"enabled" BOOL,'#13#10 +
    '"username" VARCHAR,'#13#10 +
    '"password" VARCHAR,'#13#10 +
    '"cookies" VARCHAR,'#13#10 +
    '"status" VARCHAR'#13#10 +
    ');';

procedure InitAccountManager(const AFilename: string);
begin
  Account := TAccountManager.Create(AFilename);
end;

function encode(const s: string): string;
begin
  if s = '' then Exit('');
  Result := EncodeStringBase64(s);
end;

function decode(const s: string): string;
begin
  if s = '' then Exit('');
  Result := DecodeStringBase64(s);
end;

{ TAccountManager }

function TAccountManager.GetUsername(AName: string): string;
begin
  Result := GetValueString(AName, 'username');
end;

function TAccountManager.GetEnabled(AName: string): boolean;
begin
  Result := GetValueBoolean(AName, 'enabled');
end;

function TAccountManager.GetPassword(AName: string): string;
begin
  Result := decode(GetValueString(AName, 'password'));
end;

function TAccountManager.GetCookies(AName: string): string;
begin
  Result := decode(GetValueString(AName, 'cookies'));
end;

function TAccountManager.GetStatus(AName: string): string;
begin
  Result := GetValueString(AName, 'status');
end;

procedure TAccountManager.SetUsername(AName: string; AValue: string);
begin
  SetValueString(AName, 'username', AValue);
end;

procedure TAccountManager.SetEnabled(AName: string; AValue: boolean);
begin
  SetValueBoolean(AName, 'enabled', AValue);
end;

procedure TAccountManager.SetPassword(AName: string; AValue: string);
begin
  SetValueString(AName, 'password', encode(AValue));
end;

procedure TAccountManager.SetCookies(AName: string; AValue: string);
begin
  SetValueString(AName, 'cookies', encode(AValue));
end;

procedure TAccountManager.SetStatus(AName: string; AValue: string);
begin
  SetValueString(AName, 'status', AValue);
end;

function TAccountManager.CreateDB: Boolean;
begin
  Result := False;
  if ffilename = '' then Exit;
  if FileExistsUTF8(ffilename) then DeleteFileUTF8(ffilename);
  if InternalOpenDB then Result := CreateDBTable;
end;

function TAccountManager.InternalOpenDB: Boolean;
begin
  Result := False;
  if ffilename = '' then Exit;
  try
    fconn.DatabaseName := ffilename;
    fconn.Connected := True;
    ftrans.Active := True;
    Result := fconn.Connected;
  except
    on E: Exception do
      WriteLog_E('TAccountManager.InternalOpenDB.Failed, ' + ffilename, E, Self);
  end;
end;

function TAccountManager.CreateDBTable: Boolean;
begin
  Result := False;
  if InternalOpenDB = False then Exit;
  try
    fconn.ExecuteDirect('DROP TABLE IF EXISTS ' + AnsiQuotedStr(ctablename, '"'));
    fconn.ExecuteDirect('CREATE TABLE ' + AnsiQuotedStr(ctablename, '"') + #13#10 + ctbaccountscreateparams);
    ftrans.Commit;
    OpenDBTable;
    Result := True;
  except
    on E: Exception do
      WriteLog_E('TAccountManager.CreateDBTable.Failed, ' + ffilename, E, Self);
  end;
end;

function TAccountManager.OpenDBTable: Boolean;
begin
  Result := False;
  if InternalOpenDB = False then Exit;
  try
    if fquery.Active then fquery.Close;
    fquery.SQL.Text := 'SELECT * FROM ' + AnsiQuotedStr(ctablename, '"');
    fquery.Open;
    GetRecordCount;
    Result := fquery.Active;
  except
    on E: Exception do
      WriteLog_E('TAccountManager.OpenDBTable.Failed, ' + ffilename, E, Self);
  end;
end;

function TAccountManager.GetValueString(const AName, AField: string): string;
begin
  if fquery.Active = False then Exit('');
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then Result := fquery.FieldByName(AField).AsString
    else Result := '';
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

function TAccountManager.GetValueBoolean(const AName, AField: string): boolean;
begin
  if fquery.Active = False then Exit(False);
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then Result := fquery.FieldByName(AField).AsBoolean
    else Result := False;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.SetValueString(const AName, AField, AValue: string);
begin
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then begin
      fquery.Edit;
      fquery.FieldByName(AField).AsString := AValue;
      fquery.Post;
    end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.SetValueBoolean(const AName, AField: string;
  AValue: boolean);
begin
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then begin
      fquery.Edit;
      fquery.FieldByName(AField).AsBoolean := AValue;
      fquery.Post;
    end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.GetRecordCount;
begin
  if fquery.Active then begin
    fquery.Last;
    frecordcount := fquery.RecordCount;
    fquery.Refresh;
  end
  else frecordcount := 0;
end;

procedure TAccountManager.Vacuum;
begin
  if fconn.Connected = False then Exit;
  fquery.Close;
  fconn.ExecuteDirect('END TRANSACTION');
  try
    fconn.ExecuteDirect('VACUUM');
  except
    on E: Exception do
      WriteLog_E('TAccountManager.Vacuum.Failed!', E, Self);
  end;
  fconn.ExecuteDirect('BEGIN TRANSACTION');
  fquery.Open;
end;

constructor TAccountManager.Create(const AFilename: string);
begin
  InitCriticalSection(locklocate);
  fconn := TSQLite3Connection.Create(nil);
  ftrans := TSQLTransaction.Create(nil);
  fquery := TSQLQuery.Create(nil);
  fconn.CharSet := 'UTF8';
  fconn.Transaction := ftrans;
  fquery.DataBase := ftrans.DataBase;
  fquery.Transaction := ftrans;
  fquery.Options := fquery.Options + [sqoAutoApplyUpdates];
  frecordcount := 0;

  ffilename := AFilename;
  if not FileExistsUTF8(ffilename) then CreateDBTable
  else OpenDBTable;
end;

destructor TAccountManager.Destroy;
begin
  if fconn.Connected then begin
    Save;
    Vacuum;
    fquery.Close;
    fconn.Close;
  end;
  fconn.Free;
  fquery.Free;
  ftrans.Free;
  DoneCriticalsection(locklocate);
  inherited Destroy;
end;

procedure TAccountManager.Save;
begin
  if fconn.Connected = False then Exit;
  try
    if fquery.Active then fquery.Close;
    ftrans.Commit;
    fquery.Open;
    GetRecordCount;
  except
    on E: Exception do
      WriteLog_E('TAccountManager.Save.Failed, ' + ffilename, E, Self);
  end;
end;

function TAccountManager.AddAccount(const AName, AUsername, APassword: string
  ): Boolean;
begin
  Result := False;
  if fconn.Connected = False then Exit;
  try
    with fquery do begin
      Append;
      FieldByName('aname').AsString := AName;
      FieldByName('enabled').AsBoolean := True;
      FieldByName('username').AsString := AUsername;
      FieldByName('password').AsString := encode(APassword);
      FieldByName('status').AsString := 'Unknown';
      Post;
    end;
    GetRecordCount;
    Result := True;
  except
    on E: Exception do
      WriteLog_E('TAccountManager.AddAccount.Failed, ' + AName, E, Self);
  end;
end;

function TAccountManager.DeleteAccount(const AName: string): Boolean;
begin
  Result := False;
  if fconn.Connected = False then Exit;
  try
    with fquery do begin
      if Locate('aname', AName, []) then begin
        Delete;
        GetRecordCount;
        Result := True;
      end;
    end;
  except
    on E: Exception do
      WriteLog_E('TAccountManager.DeleteAccount.Failed, ' + AName, E, Self);
  end;
end;

finalization
  if Assigned(Account) then Account.Free;

end.
