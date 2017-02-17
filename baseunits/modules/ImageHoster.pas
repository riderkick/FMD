unit ImageHoster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, XQueryEngineHTML;

function GetImageHoster(const AHTTP: THTTPSendThread; const AURL: String): String;

implementation

const
  imghosts: array [0..2] of array [0..1] of string = (
    ('imagetwist.com', '//img[@class="pic img img-responsive"]/@src'),
    ('imgchili.net', '//img[@id="show_image"]/@src'),
    ('imgbox.com', '//img[@id="img"]/@src ')
    );

function GetImageHoster(const AHTTP: THTTPSendThread; const AURL: String): String;
var
  u: String;
  i: Integer;
begin
  Result := '';
  u := Trim(LowerCase(AURL));
  if u = '' then Exit;

  for i := Low(imghosts) to High(imghosts) do
  begin
    if Pos(imghosts[i, 0], u) <> 0 then
    begin
      AHTTP.GET(AURL);
      if i = 0 then
        AHTTP.GET(AURL);
      with TXQueryEngineHTML.Create(AHTTP.Document) do
        try
          Result := XPathString(imghosts[i, 1]);
        finally
          Free;
        end;
      Break;
    end;
  end;
end;

end.

