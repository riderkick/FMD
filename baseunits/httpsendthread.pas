unit httpsendthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, synautil, synacode, ssl_openssl, blcksock,
  uFMDThread, GZIPUtils, Graphics, SimpleLogger, RegExpr;

type

  { THTTPSendThread }

  THTTPSendThread = class(THTTPSend)
  private
    fowner: TFMDThread;
    fretrycount: Integer;
    fgzip: Boolean;
    ffollowredirection: Boolean;
    procedure SetTimeout(AValue: Integer);
    procedure OnOwnerTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TFMDThread = nil);
    function HTTPRequest(const Method, URL: String; const Response: TObject = nil): Boolean;
    function HEAD(const URL: String; const Response: TObject = nil): Boolean;
    function GET(const URL: String; const Response: TObject = nil): Boolean;
    function POST(const URL: String; const POSTData: String = ''; const Response: TObject = nil): Boolean;
    function GetCookies: String;
    procedure RemoveCookie(const CookieName: String);
    procedure SetProxy(const ProxyType, Host, Port, User, Pass: String);
    procedure SetNoProxy;
    procedure Reset;
    property Timeout: Integer read FTimeout write SetTimeout;
    property RetryCount: Integer read fretrycount write fretrycount;
    property GZip: Boolean read fgzip write fgzip;
    property FollowRedirection: Boolean read ffollowredirection write ffollowredirection;
  end;

var
  DefaultUserAgent: String =
  'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:44.0) Gecko/20100101 Firefox/44.0';

implementation

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
  fgzip := True;
  fretrycount := 0;
  ffollowredirection := True;
  SetTimeout(15000);
  Reset;
  if Assigned(AOwner) then
  begin
    fowner := AOwner;
    fowner.OnCustomTerminate := @OnOwnerTerminate;
  end;
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
  rurl := EncodeURL(DecodeURL(URL));
  if Pos('HTTP/', Headers.Text) = 1 then Reset;
  HTTPHeader := TStringList.Create;
  HTTPHeader.Assign(Headers);
  try
    // first request
    while (not HTTPMethod(Method, rurl)) or (ResultCode = 500) do begin
      if CheckTerminate then Exit;
      if (fretrycount > -1) and (fretrycount <= counter) then Exit;
      Inc(Counter);
      Headers.Assign(HTTPHeader);
    end;

    // redirection      '
    if ffollowredirection then
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
        while (not HTTPMethod('GET', rurl)) or (ResultCode > 500) do begin
          if checkTerminate then Exit;
          if (fretrycount > -1) and (fretrycount <= counter) then Exit;
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
      try
        if Response is TStringList then
          TStringList(Response).LoadFromStream(Document)
        else
        if Response is TPicture then
          TPicture(Response).LoadFromStream(Document)
        else
        if Response is TStream then
          Document.SaveToStream(TStream(Response));
      except
        on E: Exception do
          WriteLog_E('HTTPRequest.WriteOutput.Error!', E);
      end;
    Result := ResultCode < 500;
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
  pt := upcase(ProxyType);
  with Sock do begin
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
    end
    else
    begin
      SocksIP := '';
      SocksPort := '1080';
      SocksType := ST_Socks5;
      SocksUsername := '';
      SocksPassword := '';
      ProxyHost := '';
      ProxyPort := '';
      ProxyUser := '';
      ProxyPass := '';
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
  if fgzip then Headers.Values['Accept-Encoding'] := ' gzip, deflate';
end;

end.
