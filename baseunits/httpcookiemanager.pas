unit httpcookiemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, StrUtils, DateUtils, syncobjs, synautil, httpsend;

type

  { THTTPCookie }

  THTTPCookie = class
  private
    FName,
    FValue,
    FDomain,
    FPath,
    FSameSite: String;
    FExpires: TDateTime;
    FHostOnly,
    FHttpOnly,
    FSecure,
    FPersistent: Boolean;
  published
    property Name: String read FName write FName;
    property Value: String read FValue write FValue;
    property Domain: String read FDomain write FDomain;
    property Path: String read FPath write FPath;
    property SameSite: String read FSameSite write FSameSite;
    property Expires: TDateTime read FExpires write FExpires;
    property HostOnly: Boolean read FHostOnly write FHostOnly;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    property Secure: Boolean read FSecure write FSecure;
    property Persistent: Boolean read FPersistent write FPersistent;
  end;

  THTTPCookies = specialize TFPGObjectList<THTTPCookie>;

  { THTTPCookieManager }

  THTTPCookieManager = class
  private
    FCookies: THTTPCookies;
    FGuardian: TCriticalSection;
  protected
    procedure AddServerCookie(const AURL, ACookie: String; const ADate: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddServerCookies(const AURL: String; const AHTTP: THTTPSend);
    procedure SetCookies(const AURL: String; const AHTTP: THTTPSend);
  published
    property Cookies: THTTPCookies read FCookies;
  end;

implementation

{ defined in RFC 6265 https://tools.ietf.org/html/rfc6265 }

{ THTTPCookieManager }

constructor THTTPCookieManager.Create;
begin
  FGuardian := TCriticalSection.Create;
  FCookies := THTTPCookies.Create(True);
end;

destructor THTTPCookieManager.Destroy;
begin
  FCookies.Free;
  FGuardian.Free;
  inherited Destroy;
end;

procedure THTTPCookieManager.AddServerCookie(const AURL, ACookie: String;
  const ADate: TDateTime);
var
  s, n, ni, v: String;
  c: THTTPCookie;
  Prot, User, Pass, Host, Port, Path, Para: String;
  i: Integer;
begin
  if Trim(ACookie) = '' then Exit;
  n := Trim(SeparateLeft(ACookie, '='));
  for i := 0 to FCookies.Count-1 do
  begin
    if n = FCookies[i].Name then
    begin
      FCookies.Delete(i);
      Break;
    end;
  end;
  c := THTTPCookie.Create;
  FCookies.Add(c);
  for s in ACookie.Split([';']) do
  begin
    n := Trim(SeparateLeft(s,'='));
    v := Trim(SeparateRight(s, '='));
    ni := LowerCase(n);
    if ni = 'domain' then
      {
        leading %x2E (".") is ignored per revised specification
        https://tools.ietf.org/html/rfc6265#section-4.1.2.3
      }
      c.Domain := LowerCase(TrimLeftSet(v, ['.']))
    else if ni = 'path' then
      c.Path := v
    else if ni = 'expires' then
    begin
      c.Expires := DecodeRfcDateTime(v);
      c.Persistent := True;
    end
    else if ni = 'max-age' then
    begin
      c.Expires := IncSecond(ADate, StrToIntDef(v, 0));
      c.Persistent := True;
    end
    else if ni = 'secure' then
      c.Secure := True
    else if ni = 'httponly' then
      c.HttpOnly := True
    else if ni = 'samesite' then
      c.SameSite := LowerCase(v)
    else
    begin
      c.Name := n;
      c.Value := v;
    end;
  end;

  ParseURL(AURL, Prot, User, Pass, Host, Port, Path, Para);
  if c.Domain = '' then
    c.Domain := LowerCase(Host);
  if c.Path = '' then
    c.Path := Path;
  if c.SameSite = '' then
    c.SameSite := 'none';
end;

procedure THTTPCookieManager.AddServerCookies(const AURL: String;
  const AHTTP: THTTPSend);
var
  i: Integer;
  s: String;
  d: TDateTime;
begin
  FGuardian.Enter;
  try
    s := Trim(AHTTP.Headers.Values['Date']);
    if s <> '' then
      d := DecodeRfcDateTime(s)
    else
      d := Now;
    for i := 0 to AHTTP.Headers.Count - 1 do
    begin
      if Pos('set-cookie', LowerCase(AHTTP.Headers[i])) = 1 then
      begin
        AddServerCookie(AURL, Trim(AHTTP.Headers.ValueFromIndex[i]), d);
      end;
    end;
  finally
    FGuardian.Leave;
  end;
end;

function CharEquals(const AString: string; const ACharPos: Integer; const AValue: Char): Boolean;
begin
  if ACharPos < 1 then Exit(False);
  Result := (ACharPos <= Length(AString)) and (AString[ACharPos] = AValue);
end;

function TextStartWith(const S, SubS: string): Boolean;
begin
  Result := (Length(SubS) <= Length(S)) and (Pos(SubS, S) = 1);
end;

function TextEndsWith(const S, SubS: string): Boolean;
var
  LS, LSubs: Integer;
begin
  LS := Length(s);
  LSubs := Length(SubS);
  Result := (LSubS <= LS) and (Pos(Subs, S) = (LS - LSubs + 1));
end;

procedure THTTPCookieManager.SetCookies(const AURL: String;
  const AHTTP: THTTPSend);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  i: Integer;
  c: THTTPCookie;

  function IsPathMatch: Boolean;
  begin
    Result := SameText(Path, c.Path) or
              ( (TextStartWith(Path, c.Path)) and
                ( TextEndsWith(c.Path, '/') or CharEquals(Path, Length(c.Path), '/') )
              );
  end;

  function IsDomainMatch: Boolean;
  begin
    Result := False;
    if (Host <> '') and (c.Domain <> '') then
    begin
      if SameText(Host, c.Domain) then
        Result := True
      else
      if TextEndsWith(Host, c.Domain) then
      begin
        if TextEndsWith(Copy(Host, 1, Length(Host)-Length(c.Domain)), '.') then
          Result := True;
      end;
    end;
  end;

  function MatchesHost: Boolean;
  begin
    if c.HostOnly then
      Result := SameText(Host, c.Domain)
    else
      Result := IsDomainMatch;
  end;

  function IsHTTP: Boolean;
  begin
    Result := (Prot = 'http') or (Prot = 'https');
  end;

begin
  if FCookies.Count = 0 then Exit;
  FGuardian.Enter;
  try
    ParseURL(AURL, Prot, User, Pass, Host, Port, Path, Para);
    Prot := LowerCase(Prot);
    Host := LowerCase(Host);
    i := 0;
    repeat
      c := FCookies[i];
      if (c.Persistent) and (c.Expires <= Now) then
        FCookies.Delete(i)
      else
      begin
        if MatchesHost and IsPathMatch and
            ((not c.Secure) or (c.Secure and c.Secure)) and
            ((not c.HttpOnly) or (c.HttpOnly and IsHTTP)) then
        begin
          AHTTP.Cookies.Values[c.Name] := c.Value;
        end;
        Inc(i);
      end;
    until i >= FCookies.Count-1;
  finally
    FGuardian.Leave;
  end;
end;

end.

