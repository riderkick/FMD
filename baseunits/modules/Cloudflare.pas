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
    CS: TRTLCriticalSection;
    constructor Create(awebsitemodule: TObject);
    destructor Destroy; override;
  end;

function CFRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const CFProps: TCFProps): Boolean;

implementation

uses WebsiteModules, MultiLog;

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
  meth, surl, r, jschl_vc, pass, jschl_answer,
  body, javascript, challenge, innerHTML, i, k, domain: String;
  re: TRegExpr;
  ms: Integer;
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

  re:=TRegExpr.Create;
  try
    body:=source;
    // main script
    re.Expression := '\<script type\=\"text\/javascript\"\>\n(.*?)\<\/script\>';
    if re.Exec(body) then javascript:=re.Match[1];

    // challenge
    re.Expression := 'setTimeout\(function\(\)\{\s*(var '+
                     's,t,o,p,b,r,e,a,k,i,n,g,f.+?\r?\n.+?a\.value\s*=.+?)\r?\n'+
                     '([^\{<>]*\},\s*(\d{4,}))?';
    ms:=0;
    if re.Exec(javascript) and (re.SubExprMatchCount>0) then begin
      challenge:=re.Match[1];
      if re.SubExprMatchCount=3 then ms:=StrToIntDef(re.Match[3], MIN_WAIT_TIME);
    end;
    if ms=0 then ms:=MIN_WAIT_TIME;

    //
    innerHTML:='';
    for i in javascript.Split([';']) do
      if SeparateLeft(i,'=').trim = 'k' then begin
        k:=SeparateRight(i,'=').trim(' ''');
         re.Expression := '\<div.*?id\=\"'+k+'\".*?\>(.*?)\<\/div\>';
         if re.Exec(body) then
           innerHTML := re.Match[1];
      end;

    SplitURL(URL,@domain,nil,false,false);
    challenge := Format(
    '        var document = {'+LineEnding+
    '            createElement: function () {'+LineEnding+
    '              return { firstChild: { href: "http://%s/" } }'+LineEnding+
    '            },'+LineEnding+
    '            getElementById: function () {'+LineEnding+
    '              return {"innerHTML": "%s"};'+LineEnding+
    '            }'+LineEnding+
    '          };'+LineEnding+
    LineEnding+
    '        %s; a.value',
                [domain,
                innerHTML,
                challenge]);
    jschl_answer := ExecJS(challenge);
  finally
    re.free;
  end;

  if jschl_answer = '' then Exit;
  OSleepTime:=ms;
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
        AHTTP.HTTPRequest(m, FillHost(h, u));
        Result := AHTTP.Cookies.Values['cf_clearance']<>'';
        if AHTTP.ResultCode=403 then
           Logger.SendError('cloudflare bypass failed, probably asking for captcha! '+AURL);
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
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(Method, AURL);
  if AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(CFProps.CS) > 0 then
      try
        Result := CFJS(AHTTP, AURL, CFProps);
      finally
        LeaveCriticalsection(CFProps.CS);
      end
    else begin
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
end;

destructor TCFProps.Destroy;
begin
  DoneCriticalsection(CS);
  inherited Destroy;
end;

end.
