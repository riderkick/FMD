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
    Cookies: String;
    Expires: TDateTime;
    CS: TRTLCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
  end;

  { THTTPSendThreadHelper }

  THTTPSendThreadHelper = class helper for THTTPSendThread
    function GETCF(const AURL: String; const CFProps: TCFProps): Boolean;
  end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; const CFProps: TCFProps): Boolean;

implementation

const
  MIN_WAIT_TIME = 4000;

function AntiBotActive(const AHTTP: THTTPSendThread): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if AHTTP.ResultCode < 500 then Exit;
  with TXQueryEngineHTML.Create(AHTTP.Document) do
    try
      Result := XPathString('//input[@name="jschl_vc"]/@value') <> '';
    finally
      Free;
    end;
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

function CFJS(const AHTTP: THTTPSendThread; AURL: String; var Cookie: String; var Expires: TDateTime): Boolean;
var
  m, u, h: String;
  st, sc, counter, maxretry: Integer;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if Cookie <> '' then AHTTP.Cookies.Text := Cookie;
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
          Result := AHTTP.Cookies.Values['cf_clearance'] <> '';
          if Result then
          begin
            Cookie := AHTTP.GetCookies;
            Expires := AHTTP.CookiesExpires;
          end;
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
    AHTTP.GET(AURL);
  end;
  AHTTP.RetryCount := maxretry;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; const CFProps: TCFProps): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if (CFProps.Expires <> 0.0) and (Now > CFProps.Expires) then
    CFProps.Reset;
  AHTTP.Cookies.Text := CFProps.Cookies;
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.GET(AURL);
  if AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(CFProps.CS) > 0 then
      try
        CFProps.Cookies := '';
        AHTTP.Cookies.Clear;
        Result := CFJS(AHTTP, AURL, CFProps.Cookies, CFProps.Expires);
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
        AHTTP.Cookies.Text := CFProps.Cookies;
      finally
        LeaveCriticalsection(CFProps.CS);
      end;
    if not AHTTP.ThreadTerminated then
      Result := AHTTP.GET(AURL);
  end;
end;

{ TCFProps }

constructor TCFProps.Create;
begin
  Reset;
  InitCriticalSection(CS);
end;

destructor TCFProps.Destroy;
begin
  DoneCriticalsection(CS);
  inherited Destroy;
end;

procedure TCFProps.Reset;
begin
  Cookies := '';
  Expires := 0.0;
end;

{ THTTPSendThreadHelper }

function THTTPSendThreadHelper.GETCF(const AURL: String; const CFProps: TCFProps): Boolean;
begin
  Cloudflare.GETCF(Self, AURL, CFProps);
end;

end.
