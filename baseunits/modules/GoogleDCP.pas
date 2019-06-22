unit GoogleDCP;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, httpsendthread, synautil, synacode, dateutils;

procedure SetGoogleDCP(const AHTTP: THTTPSendThread);
procedure RemoveGoogleDCP(const AHTTP: THTTPSendThread);

implementation

const
  proxyhost='proxy.googlezip.net';
  authkey='ac4500dd3b7579186c1b0620614fdb1f7d61f944';

function randint: String;
begin
  Result:=IntToStr(Random(999999999));
end;

procedure BeforeHTTPMethod(const AHTTP: THTTPSendThread; var Method, URL: String);
var
  timestamp: String;
begin
  if (Method='GET') and (LowerCase(Copy(URL,1,7))='http://') then
  begin
    AHTTP.SetProxy('HTTP',proxyhost,'80','','');
    timestamp:=IntToStr(DateTimeToUnix(Now));
    AHTTP.Headers.Add('Chrome-Proxy: ps='+timestamp+'-'+randint+'-'+randint+'-'+randint+', sid='+StrToHex(MD5(timestamp+authkey+timestamp))+', c=win, b=3029, p=110');
  end;
end;

procedure AfterHTTPMethod(const AHTTP: THTTPSendThread; var Method, URL: String);
begin
  if AHTTP.ProxyHost = proxyhost then
    AHTTP.SetDefaultProxy;
end;

procedure SetGoogleDCP(const AHTTP: THTTPSendThread);
begin
  AHTTP.BeforeHTTPMethod := @BeforeHTTPMethod;
  AHTTP.AfterHTTPMethod := @AfterHTTPMethod;
end;

procedure RemoveGoogleDCP(const AHTTP: THTTPSendThread);
begin
  if AHTTP.BeforeHTTPMethod=@BeforeHTTPMethod then
    AHTTP.BeforeHTTPMethod:=nil;
  if AHTTP.AfterHTTPMethod=@AfterHTTPMethod then
    AHTTP.AfterHTTPMethod:=nil;
end;

end.

