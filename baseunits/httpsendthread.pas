unit httpsendthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, synautil, synacode, ssl_openssl, blcksock,
  uFMDThread, GZIPUtils, Graphics, RegExpr;

type

  { THTTPSendThread }

  THTTPSendThread = class(THTTPSend)
  private
    FOwner: TFMDThread;
    FRetryCount: Integer;
    FGZip: Boolean;
    FFollowRedirection: Boolean;
    FAllowServerErrorResponse: Boolean;
    procedure SetTimeout(AValue: Integer);
    procedure OnOwnerTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TFMDThread = nil);
    destructor Destroy; override;
    function HTTPRequest(const Method, URL: String; const Response: TObject = nil): Boolean;
    function HEAD(const URL: String; const Response: TObject = nil): Boolean;
    function GET(const URL: String; const Response: TObject = nil): Boolean;
    function POST(const URL: String; const POSTData: String = ''; const Response: TObject = nil): Boolean;
    function GetCookies: String;
    function ThreadTerminated: Boolean;
    procedure RemoveCookie(const CookieName: String);
    procedure SetProxy(const ProxyType, Host, Port, User, Pass: String);
    procedure SetNoProxy;
    procedure Reset;
    property Timeout: Integer read FTimeout write SetTimeout;
    property RetryCount: Integer read FRetryCount write FRetryCount;
    property GZip: Boolean read FGZip write FGZip;
    property FollowRedirection: Boolean read FFollowRedirection write FFollowRedirection;
    property AllowServerErrorResponse: Boolean read FAllowServerErrorResponse write FAllowServerErrorResponse;
    property Thread: TFMDThread read FOwner;
  end;

  TKeyValuePair = array[0..1] of String;

function KeyVal(const AKey, AValue: String): TKeyValuePair;
function QueryString(KeyValuePairs: array of TKeyValuePair): String;
function SetDefaultProxy(const ProxyType, Host, Port, User, Pass: String): Boolean;
procedure SetDefaultProxyAndApply(const ProxyType, Host, Port, User, Pass: String);
procedure SetDefaultTimeoutAndApply(const ATimeout: Integer);
procedure SetDefaultRetryCountAndApply(const ARetryCount: Integer);

function MaybeEncodeURL(const AValue: String): String;

var
  DefaultUserAgent: String =
    'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36';
  DefaultRetryCount: Integer = 0;
  DefaultTimeout: Integer = 15000;
  DefaultProxyType: String = '';
  DefaultProxyHost: String = '';
  DefaultProxyPort: String = '';
  DefaultProxyUser: String = '';
  DefaultProxyPass: String = '';

implementation

var
  ALLHTTPSendThread: TFPList;
  CS_ALLHTTPSendThread: TRTLCriticalSection;

function KeyVal(const AKey, AValue: String): TKeyValuePair;
begin
  Result[0] := AKey;
  Result[1] := AValue;
end;

function QueryString(KeyValuePairs: array of TKeyValuePair): String;
var
  i: Integer;
begin
  Result := '';
  if Length(KeyValuePairs) > 0 then
    for i := Low(KeyValuePairs) to High(KeyValuePairs) do
    begin
      if Result <> '' then
        Result := Result + '&';
      Result := Result + EncodeURL(KeyValuePairs[i, 0]) + '=' + EncodeURL(KeyValuePairs[i, 1]);
    end;
end;

function SetDefaultProxy(const ProxyType, Host, Port, User, Pass: String): Boolean;
begin
  Result := (ProxyType <> DefaultProxyType) or
    (Host <> DefaultProxyHost) or
    (Port <> DefaultProxyPort) or
    (User <> DefaultProxyUser) or
    (Pass <> DefaultProxyPass);
  if not Result then Exit;
  DefaultProxyType := ProxyType;
  DefaultProxyHost := Host;
  DefaultProxyPort := Port;
  DefaultProxyUser := User;
  DefaultProxyPass := Pass;
end;

procedure SetDefaultProxyAndApply(const ProxyType, Host, Port, User, Pass: String);
var
  i: SizeInt;
begin
  if not SetDefaultProxy(ProxyType, Host, Port, User, Pass) then Exit;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    if ALLHTTPSendThread.Count > 0 then
      for i := 0 to ALLHTTPSendThread.Count - 1 do
        THTTPSendThread(ALLHTTPSendThread[i]).SetProxy(ProxyType, Host, Port, User, Pass);
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

procedure SetDefaultTimeoutAndApply(const ATimeout: Integer);
var
  i: SizeInt;
begin
  if ATimeout = DefaultTimeout then Exit;
  DefaultTimeout := ATimeout;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    if ALLHTTPSendThread.Count > 0 then
      for i := 0 to ALLHTTPSendThread.Count - 1 do
        THTTPSendThread(ALLHTTPSendThread[i]).Timeout := ATimeout;
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

procedure SetDefaultRetryCountAndApply(const ARetryCount: Integer);
var
  i: SizeInt;
begin
  if ARetryCount = DefaultRetryCount then Exit;
  DefaultRetryCount := ARetryCount;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    if ALLHTTPSendThread.Count > 0 then
      for i := 0 to ALLHTTPSendThread.Count - 1 do
        THTTPSendThread(ALLHTTPSendThread[i]).RetryCount := ARetryCount;
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

function MaybeEncodeURL(const AValue: String): String;
begin
  Result := Trim(AValue);
  if Result = '' then Exit;
  if Length(DecodeURL(Result)) >= Length(Result) then
    Result := EncodeURL(Result);
end;

{ THTTPSendThread }

procedure THTTPSendThread.SetTimeout(AValue: Integer);
begin
  if FTimeout = AValue then Exit;
  FTimeout := AValue;
  Sock.ConnectionTimeout := FTimeout;
  Sock.SetTimeout(FTimeout);
end;

procedure THTTPSendThread.OnOwnerTerminate(Sender: TObject);
begin
  Sock.Tag := 1;
  Sock.AbortSocket;
end;

constructor THTTPSendThread.Create(AOwner: TFMDThread);
begin
  inherited Create;
  KeepAlive := True;
  if Trim(DefaultUserAgent) <> '' then
    UserAgent := DefaultUserAgent;
  Protocol := '1.1';
  Headers.NameValueSeparator := ':';
  Cookies.NameValueSeparator := '=';
  FGZip := True;
  FFollowRedirection := True;
  FAllowServerErrorResponse := False;
  FRetryCount := DefaultRetryCount;
  SetTimeout(DefaultTimeout);
  SetProxy(DefaultProxyType, DefaultProxyHost, DefaultProxyPort, DefaultProxyUser, DefaultProxyPass);
  Reset;
  if Assigned(AOwner) then
  begin
    FOwner := AOwner;
    FOwner.OnCustomTerminate := @OnOwnerTerminate;
  end;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    ALLHTTPSendThread.Add(Self);
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

destructor THTTPSendThread.Destroy;
begin
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    ALLHTTPSendThread.Remove(Self);
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
  inherited Destroy;
end;

function THTTPSendThread.HTTPRequest(const Method, URL: String; const Response: TObject): Boolean;

  function CheckTerminate: Boolean;
  begin
    Result := Sock.Tag = 1;
    if Result then Sock.Tag := 0;
  end;

var
  counter: Integer = 0;
  rurl, s: String;
  HTTPHeader: TStringList;
  mstream: TMemoryStream;
begin
  Result := False;
  rurl := MaybeEncodeURL(URL);
  if Pos('HTTP/', Headers.Text) = 1 then Reset;
  HTTPHeader := TStringList.Create;
  HTTPHeader.Assign(Headers);
  try
    // first request
    while (not HTTPMethod(Method, rurl)) or
      ((not FAllowServerErrorResponse) and (ResultCode > 500)) do begin
      if CheckTerminate then Exit;
      if (FRetryCount > -1) and (FRetryCount <= counter) then Exit;
      Inc(Counter);
      Headers.Assign(HTTPHeader);
    end;

    // redirection      '
    if FFollowRedirection then
      while (ResultCode > 300) and (ResultCode < 400) do begin
        if CheckTerminate then Exit;
        HTTPHeader.Values['Referer'] := ' ' + rurl;
        s := Trim(Headers.Values['Location']);
        if s <> '' then begin
          with TRegExpr.Create do
            try
              Expression := '(?ig)^(\w+://)?([^/]*\.\w+)?(\:\d+)?(/?.*)$';
              if Replace(s, '$1', True) = '' then begin
                if s[1] <> '/' then s := '/' + s;
                rurl := Replace(rurl, '$1$2$3', True) + s;
              end else rurl := s;
            finally
              Free;
            end;
        end;

        Clear;
        Headers.Assign(HTTPHeader);
        counter := 0;
        while (not HTTPMethod('GET', rurl)) or
          ((not FAllowServerErrorResponse) and (ResultCode > 500)) do begin
          if checkTerminate then Exit;
          if (FRetryCount > -1) and (FRetryCount <= counter) then Exit;
          Inc(counter);
          Clear;
          Headers.Assign(HTTPHeader);
        end;
      end;

    // response
    // decompress data
    s := LowerCase(Headers.Values['Content-Encoding']);
    if (Pos('gzip', s) <> 0) or (Pos('deflate', s) <> 0) then
    begin
      mstream := TMemoryStream.Create;
      try
        ZUncompressStream(Document, mstream);
        Document.Clear;
        Document.LoadFromStream(mstream);
      except
      end;
      mstream.Free;
    end;
    if Assigned(Response) then
    begin
      if Response is TStringList then
        TStringList(Response).LoadFromStream(Document)
      else
      if Response is TPicture then
        TPicture(Response).LoadFromStream(Document)
      else
      if Response is TStream then
        Document.SaveToStream(TStream(Response));
    end;
    Result := Document.Size > 0;
  finally
    HTTPHeader.Free;
  end;
end;

function THTTPSendThread.HEAD(const URL: String; const Response: TObject): Boolean;
begin
  Result := HTTPRequest('HEAD', URL, Response);
end;

function THTTPSendThread.GET(const URL: String; const Response: TObject): Boolean;
begin
  Result := HTTPRequest('GET', URL, Response);
end;

function THTTPSendThread.POST(const URL: String; const POSTData: String; const Response: TObject): Boolean;
begin
  if POSTData <> '' then begin
    Document.Clear;
    WriteStrToStream(Document, POSTData);
  end;
  if (MimeType = 'text/html') or (MimeType = '') then
    MimeType := 'application/x-www-form-urlencoded';
  Result := HTTPRequest('POST', URL, Response);
end;

function THTTPSendThread.GetCookies: String;
var
  i: Integer;
begin
  Result := '';
  if Cookies.Count > 0 then
    for i := 0 to Cookies.Count - 1 do begin
      if Result = '' then Result := Cookies.Strings[i]
      else Result := Result + '; ' + Cookies.Strings[i];
    end;
end;

function THTTPSendThread.ThreadTerminated: Boolean;
begin
  Result := False;
  if Assigned(FOwner) then
    Result := FOwner.IsTerminated;
end;

procedure THTTPSendThread.RemoveCookie(const CookieName: String);
var
  i: Integer;
begin
  if CookieName = '' then Exit;
  if Cookies.Count > 0 then begin
    i := Cookies.IndexOfName(CookieName);
    if i > -1 then Cookies.Delete(i);
  end;
end;

procedure THTTPSendThread.SetProxy(const ProxyType, Host, Port, User, Pass: String);
var
  pt: String;
begin
  pt := AnsiUpperCase(ProxyType);
  with Sock do begin
    ProxyHost := '';
    ProxyPort := '';
    ProxyUser := '';
    ProxyPass := '';
    SocksIP := '';
    SocksPort := '1080';
    SocksType := ST_Socks5;
    SocksUsername := '';
    SocksPassword := '';
    if pt = 'HTTP' then
    begin
      ProxyHost := Host;
      ProxyPort := Port;
      ProxyUser := User;
      ProxyPass := Pass;
    end
    else
    if (pt = 'SOCKS4') or (pt = 'SOCKS5') then
    begin
      if pt = 'SOCKS4' then
        SocksType := ST_Socks4
      else
      if pt = 'SOCKS5' then
        SocksType := ST_Socks5;
      SocksIP := Host;
      SocksPort := Port;
      SocksUsername := User;
      SocksPassword := Pass;
    end;
  end;
end;

procedure THTTPSendThread.SetNoProxy;
begin
  SetProxy('', '', '', '', '');
end;

procedure THTTPSendThread.Reset;
begin
  Clear;
  Headers.Values['DNT'] := ' 1';
  Headers.Values['Accept'] := ' text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  Headers.Values['Accept-Charset'] := ' utf8';
  Headers.Values['Accept-Language'] := ' en-US,en;q=0.8';
  if FGZip then Headers.Values['Accept-Encoding'] := ' gzip, deflate';
end;

initialization
  InitCriticalSection(CS_ALLHTTPSendThread);
  ALLHTTPSendThread := TFPList.Create;

finalization
  ALLHTTPSendThread.Free;
  DoneCriticalsection(CS_ALLHTTPSendThread);

end.
