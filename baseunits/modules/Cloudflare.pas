unit Cloudflare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseUnit, XQueryEngineHTML, httpsendthread, synautil,
  synacode, JSUtils, RegExpr, dateutils;

type

  { TCFProps }

  TCFProps = class
  public
    websitemodule: TObject;
    fcf_clearance: string;
    fexpires: TDateTime;
    CS: TRTLCriticalSection;
    constructor Create(awebsitemodule: TObject);
    destructor Destroy; override;
    procedure Reset;
    procedure AddCookiesTo(const ACookies: TStringList);
  published
    property CF_Clearance: String read fcf_clearance write fcf_clearance;
    property Expires: TDateTime read fexpires write fexpires;
  end;

function CFRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const CFProps: TCFProps): Boolean;

implementation

uses WebsiteModules;

const
  MIN_WAIT_TIME = 5000;

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

function JSGetAnsweredURL(const Source, URL: String; var OMethod, OURL, opostdata: String;
  var OSleepTime: Integer): Boolean;
var
  s, meth, surl, r, jschl_vc, pass, jschl_answer: String;
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
      r:=xpathstring('//input[@name="r"]/@value');
      jschl_vc := XPathString('//input[@name="jschl_vc"]/@value');
      pass := XPathString('//input[@name="pass"]/@value');
    finally
      Free;
    end;

  if (meth = '') or (surl = '') or (r='') or (jschl_vc = '') or (pass = '') then Exit;

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
        jschl_answer := ExecJS(s);
      end;
    finally
      Free;
    end;

  if jschl_answer = '' then Exit;
  OMethod := meth;
  OURL := surl;
  opostdata:='r='+encodeurlelement(r)+'&jschl_vc='+encodeurlelement(jschl_vc)+'&pass='+encodeurlelement(pass)+'&jschl_answer='+encodeurlelement(jschl_answer);
  Result := True;
end;

function CFJS(const AHTTP: THTTPSendThread; AURL: String; const cfprops: TCFProps): Boolean;
var
  m, u, h,postdata: String;
  st, sc, counter, maxretry: Integer;
begin
  Result := False;
  if AHTTP = nil then Exit;
  counter := 0;
  maxretry := AHTTP.RetryCount;
  AHTTP.RetryCount := 0;
  m:='POST';
  u:='';
  h:=StringReplace(AppendURLDelim(GetHostURL(AURL)),'http://','https://',[rfIgnoreCase]);
  postdata:='';
  st:=MIN_WAIT_TIME;
  // retry to solve until max retry count in connection setting
  while True do
  begin
    Inc(counter);
    if JSGetAnsweredURL(StreamToString(AHTTP.Document), h, m, u,postdata, st) then
      if (m <> '') and (u <> '') then
      begin
        AHTTP.Reset;
        if m='POST' then
        begin
          writestrtostream(ahttp.document,postdata);
          ahttp.mimetype:='application/x-www-form-urlencoded';
        end;
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
          cfprops.fcf_clearance := AHTTP.Cookies.Values['cf_clearance'];
          Result := cfprops.fcf_clearance <> '';
          if Result then
            cfprops.fexpires := AHTTP.CookiesExpires;
        end;
        AHTTP.FollowRedirection := True;
      end;
    // update retry count in case user change it in the middle of process
    if AHTTP.RetryCount <> 0 then
    begin
      maxretry := AHTTP.RetryCount;
      AHTTP.RetryCount := 0;
    end;
    if Result then begin
      // if success force replace protocol to https for rooturl in modulecontainer
      with TModuleContainer(cfprops.websitemodule) do
        if Pos('http://',RootURL)=1 then
          RootURL:=stringreplace(RootURL,'http://','https://',[rfIgnoreCase]);
      Break;
    end;
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
  if (CFProps.fexpires <> 0.0) and (Now > CFProps.fexpires) then
    CFProps.Reset;
  CFProps.AddCookiesTo(AHTTP.Cookies);
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(Method, AURL);
  if AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(CFProps.CS) > 0 then
      try
        CFProps.Reset;
        //AHTTP.Cookies.Clear;
        Result := CFJS(AHTTP, AURL, CFProps);
        // reduce the expires by 5 minutes, usually it is 24 hours or 16 hours
        // in case of the different between local and server time
        if Result then
          CFProps.fexpires := IncMinute(CFProps.fexpires, -5);
      finally
        LeaveCriticalsection(CFProps.CS);
      end
    else begin
      try
        EnterCriticalsection(CFProps.CS);
        CFProps.AddCookiesTo(AHTTP.Cookies);
      finally
        LeaveCriticalsection(CFProps.CS);
      end;
      if not AHTTP.ThreadTerminated then
        Result := AHTTP.HTTPRequest(Method, AURL);
    end;
  end;
  if Assigned(Response) then
    if Response is TStringList then
      TStringList(Response).LoadFromStream(AHTTP.Document)
    else
    if Response is TStream then
      AHTTP.Document.SaveToStream(TStream(Response));
end;

{ TCFProps }

constructor TCFProps.Create(awebsitemodule: TObject);
begin
  websitemodule:=awebsitemodule;
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
      fcf_clearance := '';
      fexpires := 0.0;
    finally
      LeaveCriticalsection(CS);
    end;
end;

procedure TCFProps.AddCookiesTo(const ACookies: TStringList);
begin
  if fcf_clearance <> '' then
    ACookies.Values['cf_clearance'] := fcf_clearance;
end;

end.
