unit Cloudflare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseUnit, XQueryEngineHTML, httpsendthread, synautil,
  BESEN, BESENValue, RegExpr, dateutils;

type

  { TCFProps }

  TCFProps = class
  public
    cf_clearance: string;
    Expires: TDateTime;
    CS: TRTLCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure AddCookiesTo(const ACookies: TStringList);
  end;

function CFRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const CFProps: TCFProps): Boolean;

implementation

const
  MIN_WAIT_TIME = 4000;

function AntiBotActive(const AHTTP: THTTPSendThread): Boolean;
var
  s: String;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if AHTTP.ResultCode < 500 then Exit;
  if Pos('text/html', AHTTP.Headers.Values['Content-Type']) = 0 then Exit;
  s := StreamToString(AHTTP.Document);
  Result := Pos('name="jschl_vc"',s) <> 0;
  s := '';
end;

function JSGetAnsweredURL(const Source, URL: String; var OMethod, OURL: String;
  var OSleepTime: Integer): Boolean;
var
  s, meth, surl, jschl_vc, pass, jschl_answer: String;
  v: TBESENValue;
begin
  Result := False;
  if (Source = '') or (URL = '') then Exit;

  meth := '';
  surl := '';
  jschl_vc := '';
  pass := '';
  jschl_answer := '';

  with TXQueryEngineHTML.Create(Source) do
    try
      meth := UpperCase(XPathString('//form[@id="challenge-form"]/@method'));
      surl := XPathString('//form[@id="challenge-form"]/@action');
      jschl_vc := XPathString('//input[@name="jschl_vc"]/@value');
      pass := XPathString('//input[@name="pass"]/@value');
    finally
      Free;
    end;

  if (meth = '') or (surl = '') or (jschl_vc = '') or (pass = '') then Exit;

  s := Source;
  with TRegExpr.Create do
    try
      ModifierG := False;
      ModifierI := True;

      Expression := 'setTimeout\(function\(\)\{\s*var.*\w,\s+(\S.+a\.value =[^;]+;)';
      Exec(s);
      if SubExprMatchCount > 0 then
      begin
        s := Match[1];
        Expression := '\s{3,}[a-z](\s*=\s*document\.|\.).+;\r?\n';
        s := Replace(s, '', False);
        Expression := 't\s=\s*t\.firstChild.href;';
        s := Replace(s, ' t = "' + URL + '";', False);
        Expression := 'a\.value\s*=';
        s := Replace(s, 'a =', False);
        Expression := '^.*\.submit\(.*\},\s*(\d{4,})\).*$';
        OSleepTime := StrToIntDef(Replace(Source, '$1', True), MIN_WAIT_TIME);

        with TBESEN.Create do
          try
            v := Execute(s);
            if v.ValueType = bvtNUMBER then
              jschl_answer := FloatToStr(v.Num);
          finally
            Free;
          end;
      end;
    finally
      Free;
    end;

  if jschl_answer = '' then Exit;
  OMethod := meth;
  OURL := surl + '?jschl_vc=' + jschl_vc + '&pass=' + pass + '&jschl_answer=' + jschl_answer;
  Result := True;
end;

function CFJS(const AHTTP: THTTPSendThread; AURL: String; var Acf_clearance: String; var AExpires: TDateTime): Boolean;
var
  m, u, h: String;
  st, sc, counter, maxretry: Integer;
begin
  Result := False;
  if AHTTP = nil then Exit;
  counter := 0;
  maxretry := AHTTP.RetryCount;
  AHTTP.RetryCount := 0;
  while True do
  begin
    Inc(counter);
    m := 'GET';
    u := '';
    h := AppendURLDelim(GetHostURL(AURL));
    st := MIN_WAIT_TIME;
    if JSGetAnsweredURL(StreamToString(AHTTP.Document), h, m, u, st) then
      if (m <> '') and (u <> '') then
      begin
        AHTTP.Reset;
        AHTTP.Headers.Values['Referer'] := ' ' + AURL;
        if st < MIN_WAIT_TIME then st := MIN_WAIT_TIME;
        sc := 0;
        while sc < st do begin
          if AHTTP.ThreadTerminated then Break;
          Inc(sc, 250);
          Sleep(250);
        end;
        AHTTP.FollowRedirection := False;
        if AHTTP.HTTPRequest(m, FillHost(h, u)) then
        begin
          Acf_clearance := AHTTP.Cookies.Values['cf_clearance'];
          Result := Acf_clearance <> '';
          if Result then
            AExpires := AHTTP.CookiesExpires;
        end;
        AHTTP.FollowRedirection := True;
      end;
    if AHTTP.RetryCount <> 0 then
    begin
      maxretry := AHTTP.RetryCount;
      AHTTP.RetryCount := 0;
    end;
    if Result then Break;
    if AHTTP.ThreadTerminated then Break;
    if (maxretry > -1) and (maxretry <= counter) then Break;
    AHTTP.Reset;
    AHTTP.HTTPRequest('GET', AURL);
  end;
  AHTTP.RetryCount := maxretry;
end;

function CFRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const CFProps: TCFProps): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if (CFProps.Expires <> 0.0) and (Now > CFProps.Expires) then
    CFProps.Reset;
  CFProps.AddCookiesTo(AHTTP.Cookies);
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(Method, AURL);
  if AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(CFProps.CS) > 0 then
      try
        CFProps.Reset;
        //AHTTP.Cookies.Clear;
        Result := CFJS(AHTTP, AURL, CFProps.cf_clearance, CFProps.Expires);
        // reduce the expires by 5 minutes, usually it is 24 hours or 16 hours
        // in case of the different between local and server time
        if Result then
          CFProps.Expires := IncMinute(CFProps.Expires, -5);
      finally
        LeaveCriticalsection(CFProps.CS);
      end
    else
      try
        EnterCriticalsection(CFProps.CS);
        CFProps.AddCookiesTo(AHTTP.Cookies);
      finally
        LeaveCriticalsection(CFProps.CS);
      end;
    if not AHTTP.ThreadTerminated then
      Result := AHTTP.HTTPRequest(Method, AURL);
  end;
  if Assigned(Response) then
    if Response is TStringList then
      TStringList(Response).LoadFromStream(AHTTP.Document)
    else
    if Response is TStream then
      AHTTP.Document.SaveToStream(TStream(Response));
end;

{ TCFProps }

constructor TCFProps.Create;
begin
  InitCriticalSection(CS);
  Reset;
end;

destructor TCFProps.Destroy;
begin
  DoneCriticalsection(CS);
  inherited Destroy;
end;

procedure TCFProps.Reset;
begin
  if TryEnterCriticalsection(CS) <> 0 then
    try
      cf_clearance := '';
      Expires := 0.0;
    finally
      LeaveCriticalsection(CS);
    end;
end;

procedure TCFProps.AddCookiesTo(const ACookies: TStringList);
begin
  if cf_clearance <> '' then
    ACookies.Values['cf_clearance'] := cf_clearance;
end;

end.
