unit httpsendthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, synautil, synacode, ssl_openssl, blcksock,
  GZIPUtils, BaseThread;

type

  { THTTPSendThread }

  THTTPSendThread = class(THTTPSend)
  private
    FOwner: TBaseThread;
    FRetryCount: Integer;
    FGZip: Boolean;
    FFollowRedirection: Boolean;
    FAllowServerErrorResponse: Boolean;
    procedure SetTimeout(AValue: Integer);
    procedure OnOwnerTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TBaseThread = nil);
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
    property Thread: TBaseThread read FOwner;
  end;

  TKeyValuePair = array[0..1] of String;

function KeyVal(const AKey, AValue: String): TKeyValuePair;
function QueryString(KeyValuePairs: array of TKeyValuePair): String;
function SetDefaultProxy(const ProxyType, Host, Port, User, Pass: String): Boolean;
procedure SetDefaultProxyAndApply(const ProxyType, Host, Port, User, Pass: String);
procedure SetDefaultTimeoutAndApply(const ATimeout: Integer);
procedure SetDefaultRetryCountAndApply(const ARetryCount: Integer);

function MaybeEncodeURL(const AValue: String): String;
procedure SplitURL(const AURL: String; const AHost, APath: PString;
  const AIncludeProtocol: Boolean = True; const AIncludePort: Boolean = True);

const
  UserAgentSynapse   = 'Mozilla/4.0 (compatible; Synapse)';
  UserAgentCURL      = 'curl/7.52.1';
  UserAgentGooglebot = 'Mozilla/5.0 (compatible; Googlebot/2.1;  http://www.google.com/bot.html)';
  UserAgentMSIE      = 'Mozilla/5.0 (Windows NT 10.0; Win64; Trident/7.0; rv:11.0) like Gecko';
  UserAgentFirefox   = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:50.0) Gecko/20100101 Firefox/50.0';
  UserAgentChrome    = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36';
  UserAgentOpera     = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36 OPR/42.0.2393.94';
  UserAgentEdge      = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.79 Safari/537.36 Edge/14.14393';

var
  DefaultUserAgent: String = UserAgentChrome;
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

function poschar(const c:char;const s:string;const offset:cardinal=1):integer;
var
  i:integer;
begin
  for i:=offset to length(s) do
    if s[i]=c then Exit(i);
  Result:=0;
end;

procedure SplitURL(const AURL: String; const AHost, APath: PString;
  const AIncludeProtocol: Boolean; const AIncludePort: Boolean);

procedure cleanuri(var u:string);
begin
  while (Length(u)<>0) and (u[1] in ['.',':','/']) do
    Delete(u,1,1);
end;

var
  iurl,ihost,ipath,iproto,iport: String;
  p,q: Integer;
begin
  if (AHost=nil) and (APath=nil) then Exit;
  if Assigned(AHost) then AHost^:='';
  if Assigned(APath) then APath^:='';
  iurl:=Trim(AURL);
  if iurl='' then Exit;
  ihost:='';
  ipath:='';
  iproto:='';
  iport:='';
  if iurl[1]='/' then
    if Length(iurl)=1 then Exit
    else
    if iurl[2]<>'/' then
    begin
      if Assigned(APath) then APath^:=iurl;
      Exit;
    end;
  p:=poschar(':',iurl);
  if (p<>0) and (p<Length(iurl)) and (iurl[P+1]='/') then
  begin
    iproto:=Copy(iurl,1,p-1);
    Delete(iurl,1,p);
    p:=poschar(':',iurl);
  end;
  q:=0;
  if (p<>0) and (p<Length(iurl)) and (iurl[P+1] in ['0'..'9']) then
  begin
    for q:=p+1 to Length(iurl) do
      if not (iurl[q] in ['0'..'9']) then Break;
    if q=Length(iurl) then Inc(q);
    iport:=Copy(iurl,p+1,q-p-1);
    delete(iurl,p,q-p);
  end;
  cleanuri(iurl);
  p:=poschar('.',iurl);
  q:=poschar('/',iurl);
  if (p<>0) and (p<Length(iurl)) then
    if p<q then
    begin
      ihost:=Copy(iurl,1,q-1);
      Delete(iurl,1,q-1);
      cleanuri(iurl);
    end
    else
    if q=0 then
    begin
      q:=poschar('.',iurl,p+1);
      if (q<>0) and (q<Length(iurl)) then
      begin
        ihost:=iurl;
        iurl:='';
      end;
    end;
  if (ihost='') and (iurl<>'') and ((iproto<>'') or (iport<>'')) then
  begin
    ihost:=iurl;
    iurl:='';
  end;
  if ihost<>'' then
  begin
    if AIncludeProtocol then
    begin
      if iproto<>'' then ihost:=iproto+'://'+ihost
      else ihost:='http://'+ihost;
    end;
    if AIncludePort and (iport<>'') then
      ihost:=ihost+':'+iport;
  end;
  if iurl<>'' then
    ipath:='/'+iurl;
  if Assigned(AHost) then AHost^:=ihost;
  if Assigned(APath) then APath^:=ipath;
end;

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
  Sock.SocksTimeout := FTimeout;
  Sock.SetTimeout(FTimeout);
end;

procedure THTTPSendThread.OnOwnerTerminate(Sender: TObject);
begin
  Sock.Tag := 1;
  Sock.AbortSocket;
end;

constructor THTTPSendThread.Create(AOwner: TBaseThread);
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
  rurl, s, h, p: String;
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
        if s<>'' then
        begin
          SplitURL(s,@h,@p);
          s:=p;
          if h='' then
            SplitURL(rurl,@h,@p);
          rurl:=h+s;
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
  if Assigned(FOwner) then
    Result := FOwner.IsTerminated
  else
    Result := False;
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
