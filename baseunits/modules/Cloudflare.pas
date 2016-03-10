unit Cloudflare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseUnit, XQueryEngineHTML, BESEN, BESENValue, RegExpr;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String;
  var CS: TRTLCriticalSection): Boolean; overload;
function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String): Boolean; overload;
function GETCF(const AHTTP: THTTPSendThread; const AURL: String): Boolean; overload;

implementation

function AntiBotActive(const AHTTP:THTTPSendThread): Boolean;
begin
  Result:=False;
  if AHTTP=nil then Exit;
  Result:=Pos('URL=/cdn-cgi/',AHTTP.Headers.Values['Refresh'])>0;
end;

function JSGetAnsweredURL(const Source,URL: String; var OMethod, OURL: String;
  var OSleepTime: Integer): Boolean;
var
  s, meth, surl, jschl_vc, pass, jschl_answer: String;
  v: TBESENValue;
begin
  Result:=False;
  if (Source='') or (URL='') then Exit;

  meth:='';
  surl:='';
  jschl_vc:='';
  pass:='';
  jschl_answer:='';

  with TXQueryEngineHTML.Create(Source) do
  try
    meth:=UpperCase(XPathString('//form[@id="challenge-form"]/@method'));
    surl:=XPathString('//form[@id="challenge-form"]/@action');
    jschl_vc:=XPathString('//input[@name="jschl_vc"]/@value');
    pass:=XPathString('//input[@name="pass"]/@value');
  finally
    Free;
  end;

  if (meth='') or (surl='') or (jschl_vc='') or (pass='') then Exit;

  s:=Source;
  with TRegExpr.Create do
    try
      ModifierG:=False;
      ModifierI:=True;
      Expression:='^.*setTimeout\(function\(\)\{\s+var t,r,a,f,\s*(\S.+a\.value =.+)\r?\n.*$';
      Expression:='^.*setTimeout\(function\(\)\{\s+var t,r,a,f,\s*(\S.+a\.value =.+)\r?\n.*$';
      s:=Replace(s,'$1',True);
      Expression:='\s{3,}[a-z](\s*=\s*document\.|\.).+;\r?\n';
      s:=Replace(s,'',False);
      Expression:='t\s=\s*t\.firstChild.href;';
      s:=Replace(s,' t = "'+URL+'";',False);
      Expression:='a\.value\s*=';
      s:=Replace(s,'a =',False);
      Expression:='^.*\.submit\(.*\},\s*(\d{4,})\).*$';
      OSleepTime:=StrToIntDef(Replace(Source,'$1',True),5000);
    finally
      Free;
    end;

  with TBESEN.Create do begin
    try
      v:=Execute(s);
      if v.ValueType=bvtNUMBER then
        jschl_answer:=FloatToStr(v.Num);
    except
      jschl_answer:='';
    end;
    Free;
  end;

  if jschl_answer='' then Exit;
  OMethod:=meth;
  OURL:=surl+'?jschl_vc='+jschl_vc+'&pass='+pass+'&jschl_answer='+jschl_answer;
  Result:=True;
end;

function CFJS(const AHTTP: THTTPSendThread; AURL: String; var Cookie: String): Boolean;
var
  m, u, h: String;
  st, counter: Integer;
begin
  Result:=False;
  if AHTTP=nil then Exit;
  if Cookie<>'' then AHTTP.Cookies.Text:=Cookie;
  counter:=0;
  while counter<3 do begin
    Inc(counter);
    if AntiBotActive(AHTTP) then begin
      m:='GET';
      u:='';
      h:=AppendURLDelim(GetHostURL(AURL));
      st:=5000;
      if JSGetAnsweredURL(StreamToString(AHTTP.Document),h,m,u,st) then
        if (m<>'') and (u<>'') then begin
          AHTTP.Reset;
          AHTTP.Headers.Values['Referer']:=' '+AURL;
          //minimum wait time is 5000
          if st<5000 then st:=5000;
          Sleep(st);
          AHTTP.FollowRedirection:=False;
          if AHTTP.HTTPRequest(m,FillHost(h,u)) then
            Result:=AHTTP.Cookies.Values['cf_clearance']<>'';
          AHTTP.FollowRedirection:=True;
          if Result then Cookie:=AHTTP.GetCookies;
        end;
    end;
    if Result then Exit
    else if counter<3 then begin
      AHTTP.Reset;
      Result:=AHTTP.GET(AURL);
    end;
  end;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String;
  var CS: TRTLCriticalSection): Boolean;
begin
  Result:=False;
  if AHTTP=nil then Exit;
  AHTTP.Cookies.Text:=Cookie;
  Result:=AHTTP.GET(AURL);
  if (AHTTP.ResultCode>500) and AntiBotActive(AHTTP) then begin
    if TryEnterCriticalsection(CS)>0 then
      try
        Result:=CFJS(AHTTP,AURL,Cookie);
      finally
        LeaveCriticalsection(CS);
      end
    else
      try
        EnterCriticalsection(CS);
        AHTTP.Cookies.Text:=Cookie;
      finally
        LeaveCriticalsection(CS);
      end;
    Result:=AHTTP.GET(AURL);
  end;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String; var Cookie: String): Boolean;
begin
  Result:=False;
  if AHTTP=nil then Exit;
  AHTTP.Cookies.Text:=Cookie;
  Result:=AHTTP.GET(AURL);
  if (AHTTP.ResultCode>500) and AntiBotActive(AHTTP) then begin
    Result:=CFJS(AHTTP,AURL,Cookie);
    Result:=AHTTP.GET(AURL);
  end;
end;

function GETCF(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
var
  Cookie: String;
begin
  Cookie:='';
  Result:=GETCF(AHTTP,AURL,Cookie);
end;

end.
