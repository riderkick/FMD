unit Cloudflare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseUnit, XQueryEngineHTML, httpsendthread, BESEN, BESENValue,
  RegExpr;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String;
  var CS: TRTLCriticalSection): Boolean; overload;
function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String): Boolean; overload;
function GETCF(const AHTTP: THTTPSendThread; const AURL: String): Boolean; overload;

implementation

const
  MIN_WAIT_TIME = 4000;
  MAX_RETRY = 3;

function AntiBotActive(const AHTTP: THTTPSendThread): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  Result := Pos('URL=/cdn-cgi/', AHTTP.Headers.Values['Refresh']) > 0;
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

function CFJS(const AHTTP: THTTPSendThread; AURL: String; var Cookie: String): Boolean;
var
  m, u, h: String;
  st, sc, counter: Integer;
begin
  Result := False;
  if AHTTP = nil then Exit;
  if Cookie <> '' then AHTTP.Cookies.Text := Cookie;
  counter := 0;
  while counter < MAX_RETRY do begin
    Inc(counter);
    if AntiBotActive(AHTTP) then begin
      m := 'GET';
      u := '';
      h := AppendURLDelim(GetHostURL(AURL));
      st := MIN_WAIT_TIME;
      if JSGetAnsweredURL(StreamToString(AHTTP.Document), h, m, u, st) then
        if (m <> '') and (u <> '') then begin
          AHTTP.Reset;
          AHTTP.Headers.Values['Referer'] := ' ' + AURL;
          if st < MIN_WAIT_TIME then st := MIN_WAIT_TIME;
          sc := 0;
          while sc < st do begin
            if AHTTP.ThreadTerminated then
              Exit;
            Inc(sc, 500);
            Sleep(500);
          end;
          AHTTP.FollowRedirection := False;
          if AHTTP.HTTPRequest(m, FillHost(h, u)) then
            Result := AHTTP.Cookies.Values['cf_clearance'] <> '';
          AHTTP.FollowRedirection := True;
          if Result then Cookie := AHTTP.GetCookies;
        end;
    end;
    if Result then Exit
    else if counter < MAX_RETRY then begin
      AHTTP.Reset;
      Result := AHTTP.GET(AURL);
    end;
  end;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String;
  var CS: TRTLCriticalSection): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  AHTTP.Cookies.Text := Cookie;
  Result := AHTTP.GET(AURL);
  if (AHTTP.ResultCode > 500) and AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(CS) > 0 then
      try
        Result := CFJS(AHTTP, AURL, Cookie);
      finally
        LeaveCriticalsection(CS);
      end
    else
      try
        EnterCriticalsection(CS);
        AHTTP.Cookies.Text := Cookie;
      finally
        LeaveCriticalsection(CS);
      end;
    if not AHTTP.ThreadTerminated then
      Result := AHTTP.GET(AURL);
  end;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String): Boolean;
begin
  Result := False;
  if AHTTP = nil then Exit;
  AHTTP.Cookies.Text := Cookie;
  Result := AHTTP.GET(AURL);
  if (AHTTP.ResultCode > 500) and AntiBotActive(AHTTP) then begin
    Result := CFJS(AHTTP, AURL, Cookie);
    Result := AHTTP.GET(AURL);
  end;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
var
  Cookie: String;
begin
  Cookie := '';
  Result := GETCF(AHTTP, AURL, Cookie);
end;

end.
