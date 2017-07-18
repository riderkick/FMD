{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit accountmanagerdb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazFileUtils, MultiLog, base64, sqlite3conn,
  sqldb, db;

type

  TAccountStatus = (asUnknown, asChecking, asValid, asInvalid);

  { TAccountManager }

  TAccountManager = class
  private
    locklocate: TRTLCriticalSection;
    fconn: TSQLite3Connection;
    ftrans: TSQLTransaction;
    fquery: TSQLQuery;
    ffilename: String;
    frecordcount: Integer;
    function CreateDB: Boolean;
    function InternalOpenDB: Boolean;
    function CreateDBTable: Boolean;
    function OpenDBTable: Boolean;
    function FixStatus: Boolean;
    procedure ConvertNewTable;
    function GetValueString(const AName, AField: string): string;
    function GetValueBoolean(const AName, AField: string): boolean;
    function GetValueInteger(const AName, AField: string): Integer;
    function GetValueStr(const RecIndex, FieldIndex: Integer): string;
    function GetValueBool(const RecIndex, FieldIndex: Integer): boolean;
    function GetValueInt(const RecIndex, FieldIndex: Integer): Integer;
    function GetUsername(AName: string): string;
    function GetEnabled(AName: string): boolean;
    function GetPassword(AName: string): string;
    function GetCookies(AName: string): string;
    function GetStatus(AName: string): TAccountStatus;
    procedure SetUsername(AName: string; AValue: string);
    procedure SetEnabled(AName: string; AValue: boolean);
    procedure SetPassword(AName: string; AValue: string);
    procedure SetCookies(AName: string; AValue: string);
    procedure SetStatus(AName: string; AValue: TAccountStatus);
    procedure SetValueString(const AName, AField, AValue: string);
    procedure SetValueBoolean(const AName, AField: string; AValue: boolean);
    procedure SetValueInteger(const AName, AField: string; AValue: Integer);
    procedure SetValueStr(const RecIndex, FieldIndex: Integer; AValue: string);
    procedure SetValueBool(const RecIndex, FieldIndex: Integer; AValue: boolean);
    procedure SetValueInt(const RecIndex, FieldIndex: Integer; AValue: Integer);
    procedure GetRecordCount;
    procedure Vacuum;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    procedure Save;
    function AddAccount(const AName, AUsername, APassword: string): Boolean;
    function DeleteAccount(const AName: string): Boolean;
    function DeleteAccount(const RecIndex: Integer): Boolean; overload;
    property Enabled[AName: string]: boolean read GetEnabled write SetEnabled;
    property Username[AName: string]: string read GetUsername write SetUsername;
    property Password[AName: string]: string read GetPassword write SetPassword;
    property Cookies[AName: string]: string read GetCookies write SetCookies;
    property Status[AName: string]: TAccountStatus read GetStatus write SetStatus;
    property ValueStr[const RecIndex, FieldIndex: Integer]: string read GetValueStr write SetValueStr;
    property ValueBool[const RecIndex, FieldIndex: Integer]: boolean read GetValueBool write SetValueBool;
    property ValueInt[const RecIndex, FieldIndex: Integer]: Integer read GetValueInt write SetValueInt;
    property Count: Integer read frecordcount;
  end;

  procedure InitAccountManager(const AFilename: string);

var
  Account: TAccountManager;

implementation

const
  ctablename = 'accounts';
  ctbaccountscreateparams = '('#13#10 +
    '"enabled" BOOL,'#13#10 +
    '"aname" VARCHAR NOT NULL PRIMARY KEY,'#13#10 +
    '"username" TEXT,'#13#10 +
    '"password" TEXT,'#13#10 +
    '"cookies" TEXT,'#13#10 +
    '"status" INTEGER'#13#10 +
    ');';

function QuotedStrd(const S: string): string;
begin
  Result := AnsiQuotedStr(S, '"');
end;

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

function TAccountManager.GetStatus(AName: string): TAccountStatus;
begin
  Result := TAccountStatus(GetValueInteger(AName, 'status'));
end;

function TAccountManager.GetValueStr(const RecIndex, FieldIndex: Integer
  ): string;
begin
  Result := '';
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  try
    fquery.RecNo := RecIndex + 1;
    Result := fquery.Fields[FieldIndex].AsString;
    if (FieldIndex = 3) or (FieldIndex = 4) then Result := decode(Result);
  except
  end;
end;

function TAccountManager.GetValueBool(const RecIndex, FieldIndex: Integer
  ): boolean;
begin
  Result := False;
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  try
    fquery.RecNo := RecIndex + 1;
    Result := fquery.Fields[FieldIndex].AsBoolean;
  except
  end;
end;

function TAccountManager.GetValueInt(const RecIndex, FieldIndex: Integer
  ): Integer;
begin
  Result := 0;
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  try
    fquery.RecNo := RecIndex + 1;
    Result := fquery.Fields[FieldIndex].AsInteger;
  except
  end;
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

procedure TAccountManager.SetStatus(AName: string; AValue: TAccountStatus);
begin
  SetValueString(AName, 'status', IntToStr(Integer(AValue)));
end;

function TAccountManager.CreateDB: Boolean;
begin
  Result := False;
  if ffilename = '' then Exit;
  if FileExistsUTF8(ffilename) then DeleteFileUTF8(ffilename);
  CreateDBTable;
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
      Logger.SendException('TAccountManager.InternalOpenDB.Failed, ' + ffilename, E);
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
      Logger.SendException('TAccountManager.CreateDBTable.Failed, ' + ffilename, E);
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
    FixStatus;
    GetRecordCount;
    Result := fquery.Active;
  except
    on E: Exception do
      Logger.SendException('TAccountManager.OpenDBTable.Failed, ' + ffilename, E);
  end;
  if Result then ConvertNewTable;
end;

function TAccountManager.FixStatus: Boolean;
begin
  Result := False;
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if fquery.RecordCount > 0 then begin
      fquery.First;
      while not fquery.EOF do begin
        if fquery.FieldByName('status').AsInteger = Integer(asChecking) then begin
          fquery.Edit;
          fquery.FieldByName('status').AsInteger := Integer(asUnknown);
          fquery.Post;
        end;
        fquery.Next;
      end;
    end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.ConvertNewTable;
begin
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if (FieldTypeNames[fquery.FieldByName('username').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[fquery.FieldByName('password').DataType] <> Fieldtypenames[ftMemo]) or
      (FieldTypeNames[fquery.FieldByName('cookies').DataType] <> Fieldtypenames[ftMemo]) then
      try
        fquery.Close;
        with fconn do begin
          ExecuteDirect('DROP TABLE IF EXISTS '+QuotedStr('temp'+ctablename));
          ExecuteDirect('CREATE TABLE '+QuotedStrd('temp'+ctablename)+#13#10+ctbaccountscreateparams);
          ExecuteDirect('INSERT INTO '+QuotedStrd('temp'+ctablename)+' SELECT * FROM '+QuotedStrd(ctablename));
          ExecuteDirect('DROP TABLE '+QuotedStrd(ctablename));
          ExecuteDirect('ALTER TABLE '+QuotedStrd('temp'+ctablename)+' RENAME TO '+QuotedStrd(ctablename));
        end;
        ftrans.Commit;
        fquery.Open;
      except
        ftrans.Rollback;
      end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

function TAccountManager.GetValueString(const AName, AField: string): string;
begin
  Result := '';
  if fquery.Active = False then Exit;
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
  Result := False;
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then Result := fquery.FieldByName(AField).AsBoolean
    else Result := False;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

function TAccountManager.GetValueInteger(const AName, AField: string): Integer;
begin
  Result := 0;
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then Result := fquery.FieldByName(AField).AsInteger;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.SetValueStr(const RecIndex, FieldIndex: Integer;
  AValue: string);
begin
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  EnterCriticalsection(locklocate);
  try
    fquery.RecNo := RecIndex + 1;
    fquery.Edit;
    if (FieldIndex = 3) or (FieldIndex = 4) then
      fquery.Fields[FieldIndex].AsString := encode(AValue)
    else
      fquery.Fields[FieldIndex].AsString := decode(AValue);
    try
      fquery.Post;
    except
      fquery.Cancel;
    end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.SetValueBool(const RecIndex, FieldIndex: Integer;
  AValue: boolean);
begin
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  EnterCriticalsection(locklocate);
  try
    fquery.RecNo := RecIndex + 1;
    fquery.Edit;
    fquery.Fields[FieldIndex].AsBoolean := AValue;
    try
      fquery.Post;
    except
      fquery.Cancel;
    end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.SetValueInt(const RecIndex, FieldIndex: Integer;
  AValue: Integer);
begin
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  EnterCriticalsection(locklocate);
  try
    fquery.RecNo := RecIndex + 1;
    fquery.Edit;
    fquery.Fields[FieldIndex].AsInteger := AValue;
    try
      fquery.Post;
    except
      fquery.Cancel;
    end;
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
      try
        fquery.Post;
      except
        fquery.Cancel;
      end;
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
      try
        fquery.Post;
      except
        fquery.Cancel;
      end;
    end;
  finally
    LeaveCriticalsection(locklocate);
  end;
end;

procedure TAccountManager.SetValueInteger(const AName, AField: string;
  AValue: Integer);
begin
  if fquery.Active = False then Exit;
  EnterCriticalsection(locklocate);
  try
    if fquery.Locate('aname', AName, []) then begin
      fquery.Edit;
      fquery.FieldByName(AField).AsInteger := AValue;
      try
        fquery.Post;
      except
        fquery.Cancel;
      end;
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
      Logger.SendException('TAccountManager.Vacuum.Failed!', E);
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
      Logger.SendException('TAccountManager.Save.Failed, ' + ffilename, E);
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
      FieldByName('enabled').AsBoolean := True;
      FieldByName('aname').AsString := AName;
      FieldByName('username').AsString := AUsername;
      FieldByName('password').AsString := encode(APassword);
      FieldByName('status').AsInteger := Integer(asUnknown);
      Post;
    end;
    GetRecordCount;
    Result := True;
  except
    on E: Exception do
      Logger.SendException('TAccountManager.AddAccount.Failed, ' + AName, E);
  end;
end;

function TAccountManager.DeleteAccount(const AName: string): Boolean;
begin
  Result := False;
  if fquery.Active = False then Exit;
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
      Logger.SendException('TAccountManager.DeleteAccount.Failed, ' + AName, E);
  end;
end;

function TAccountManager.DeleteAccount(const RecIndex: Integer): Boolean;
begin
  Result := False;
  if fquery.Active = False then Exit;
  if (RecIndex > frecordcount) and (RecIndex < 0) then Exit;
  try
    with fquery do begin
      RecNo := RecIndex + 1;
      Delete;
      GetRecordCount;
      Result := True;
    end;
  except
    on E: Exception do
      Logger.SendException('TAccountManager.DeleteAccount.Failed, ' + IntToStr(RecIndex), E);
  end;
end;

finalization
  if Assigned(Account) then Account.Free;

end.
