unit JSUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Duktape.Api, MultiLog;

function ExecJS(const text: String): String;

implementation

function ExecJS(const text: String): String;
var
  ctx: PDukContext;
  r: TDukInt;
  s: String;
begin
  Result := '';
  ctx := duk_create_heap_default;
  if ctx = nil then begin
    Logger.SendError('Failed to create a Duktape heap.');
    Exit;
  end;
  duk_push_string(ctx, PAnsiChar(text));
  r := duk_peval(ctx);
  s := duk_safe_to_string(ctx, -1);
  if r <> 0 then Logger.SendError('Error: ' + s)
  else Result := s;
  duk_destroy_heap(ctx);
end;

end.

