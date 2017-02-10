unit ImageHoster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, XQueryEngineHTML;

function GetImageHoster(const AHTTP: THTTPSendThread; const AURL: String): String;

implementation

function GetImageHoster(const AHTTP: THTTPSendThread; const AURL: String): String;
var
  u: String;
begin
  Result := '';
  u := Trim(LowerCase(AURL));
  if u = '' then Exit;
  if Pos('imagetwist.com', u) <> 0 then
  begin
    AHTTP.GET(AURL);
    AHTTP.GET(AURL);
    with TXQueryEngineHTML.Create(AHTTP.Document) do
      try
        Result := XPathString('//img[@class="pic img img-responsive"]/@src');
      finally
        Free;
      end;
  end;
end;

end.

