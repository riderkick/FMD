unit SourceForge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread;

function Download(const AHTTP: THTTPSendThread; const AURL: String): Boolean;

implementation

uses XQueryEngineHTML, RegExpr, synautil;

function Download(const AHTTP: THTTPSendThread; const AURL: String): Boolean;
var
  s, lurl: String;
  ml: TStringList;
  fr: Boolean;
  i: LongInt;
begin
  Result := False;
  AHTTP.UserAgent := UserAgentCURL;
  if AHTTP.GET(AURL) and (AHTTP.ResultCode < 300) then
  begin
    // the Sourceforge site is currently in Disaster Recovery mode, and currently requires the use of javascript to function.
    if Pos('/#!/', AHTTP.LastURL) <> 0 then
    begin
      lurl := AHTTP.LastURL;
      // load the mirror list
      if AHTTP.GET('https://sourceforge.net/js/mirrors.js') then
      begin
        s := GetBetween('mirror_list = shuffle(',');',ReadStrFromStream(AHTTP.Document, AHTTP.Document.Size));
        ml := TStringList.Create;
        try
          XPathStringAll('json(.)()("abbr")', s, ml);
          fr := AHTTP.FollowRedirection;
          AHTTP.FollowRedirection := False;
          // 302 means not found?
          // keep retry on another mirrors if not found
          //while (not Result) and (not AHTTP.ThreadTerminated) and (ml.Count <> 0) do
          //begin
            i := Random(ml.Count);
            Result := AHTTP.GET(
              ReplaceRegExpr('^.*/#!/projects/(\w+)/files(/.*?)(/download)?$', lurl,
                'https://' + ml[i] +'.dl.sourceforge.net/project/$1$2', True))
              and (AHTTP.ResultCode < 300);
          //  if not Result then
          //    ml.Delete(i);
          //end;
          AHTTP.FollowRedirection := fr;
        finally
          ml.Free;
        end;
      end;
    end
    else
      Result := True;
  end;
end;

end.

