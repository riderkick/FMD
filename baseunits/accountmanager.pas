unit accountmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, base64;

type

  { TAccountManager }

  TAccountManager = class
  private
    fstorage: TIniFile;
    function GetUsername(Name: string): string;
    function GetPassword(Name: string): string;
    function GetCookies(Name: string): string;
    function GetStatus(Name: string): string;
    procedure SetUsername(Name: string; AValue: string);
    procedure SetPassword(Name: string; AValue: string);
    procedure SetCookies(Name: string; AValue: string);
    procedure SetStatus(Name: string; AValue: string);
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    procedure Save;
    property Username[Name: string]: string read GetUsername write SetUsername;
    property Password[Name: string]: string read GetPassword write SetPassword;
    property Cookies[Name: string]: string read GetCookies write SetCookies;
    property Status[Name: string]: string read GetStatus write SetStatus;
  end;

  procedure InitAccountManager(const Filename: string);

var
  Account: TAccountManager;

implementation

procedure InitAccountManager(const Filename: string);
begin
  Account := TAccountManager.Create(Filename);
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

function TAccountManager.GetUsername(Name: string): string;
begin
  Result := decode(fstorage.ReadString(Name, 'username', ''));
end;

function TAccountManager.GetPassword(Name: string): string;
begin
  Result := decode(fstorage.ReadString(Name, 'password', ''));
end;

function TAccountManager.GetCookies(Name: string): string;
begin
  Result := decode(fstorage.ReadString(Name, 'cookies', ''));
end;

function TAccountManager.GetStatus(Name: string): string;
begin
  Result := decode(fstorage.ReadString(Name, 'status', ''));
end;

procedure TAccountManager.SetUsername(Name: string; AValue: string);
begin
  fstorage.WriteString(Name, 'username', encode(AValue));
end;

procedure TAccountManager.SetPassword(Name: string; AValue: string);
begin
  fstorage.WriteString(Name, 'password', encode(AValue));
end;

procedure TAccountManager.SetCookies(Name: string; AValue: string);
begin
  fstorage.WriteString(Name, 'cookies', encode(AValue));
end;

procedure TAccountManager.SetStatus(Name: string; AValue: string);
begin
  fstorage.WriteString(Name, 'status', encode(AValue));
end;

constructor TAccountManager.Create(const Filename: string);
begin
  fstorage := TIniFile.Create(Filename);
end;

destructor TAccountManager.Destroy;
begin
  fstorage.Free;
  inherited Destroy;
end;

procedure TAccountManager.Save;
begin
  fstorage.UpdateFile;
end;

finalization
  if Assigned(Account) then Account.Free;

end.

