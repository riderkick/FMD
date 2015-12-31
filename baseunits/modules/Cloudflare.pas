unit Cloudflare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseUnit, BESEN, BESENValue, RegExpr;

function AntiBotActive(const AHTTP:THTTPSendThread):Boolean;
function GETCF(const AHTTP: THTTPSendThread; AURL: String; var Cookie: String):Boolean;

implementation

function GetAnsweredURL(const Source,URL:String;var OMethod,OURL:String;var OSleepTime:Integer):Boolean;
var
  s, meth, surl, jschl_vc, pass, jschl_answer: String;
  query: TXQueryEngineHTML;
  js: TBESEN;
  v: TBESENValue;
begin
  Result:=False;
  if (Source='') or (URL='') then Exit;

  meth:='';
  surl:='';
  jschl_vc:='';
  pass:='';
  jschl_answer:='';

  query:=TXQueryEngineHTML.Create;
  try
    query.ParseHTML(Source);
    meth:=UpperCase(query.XPathString('//form[@id="challenge-form"]/@method'));
    surl:=query.XPathString('//form[@id="challenge-form"]/@action');
    jschl_vc:=query.XPathString('//input[@name="jschl_vc"]/@value');
    pass:=query.XPathString('//input[@name="pass"]/@value');
  finally
    query.Free;
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

  js:=TBESEN.Create;
  try
    v:=js.Execute(s);
    if v.ValueType=bvtNUMBER then
      jschl_answer:=FloatToStr(v.Num);
  except
    jschl_answer:='';
  end;
  js.Free;

  if jschl_answer='' then Exit;
  OMethod:=meth;
  OURL:=surl+'?jschl_vc='+jschl_vc+'&pass='+pass+'&jschl_answer='+jschl_answer;
  Result:=True;
end;

function AntiBotActive(const AHTTP:THTTPSendThread):Boolean;
begin
  Result:=False;
  if AHTTP=nil then Exit;
  Result:=Pos('URL=/cdn-cgi/',AHTTP.Headers.Values['Refresh'])>0;
end;

function GETCF(const AHTTP:THTTPSendThread;AURL:String;var Cookie:String):Boolean;
var
  m, u, h: String;
  st: Integer;
begin
  Result:=False;
  if AHTTP=nil then Exit;
  if Cookie<>'' then AHTTP.Cookies.Text:=Cookie;
  Result := AHTTP.GET(AURL);
  if AntiBotActive(AHTTP) then begin
    m:='GET';
    u:='';
    h:=AppendURLDelim(GetHostURL(AURL));
    st:=5000;
    if GetAnsweredURL(StreamToString(AHTTP.Document),h,m,u,st) then
      if (m<>'') and (u<>'') then begin
        AHTTP.Reset;
        AHTTP.Headers.Values['Referer']:=' '+AURL;
        Sleep(st);
        AHTTP.FollowRedirection:=False;
        if AHTTP.HTTPRequest(m,FillHost(h,u)) then
          Result:=AHTTP.Cookies.Values['cf_clearance']<>'';
        AHTTP.FollowRedirection:=True;
        if Result then Cookie:=AHTTP.GetCookies;
      end;
  end;
end;

end.
