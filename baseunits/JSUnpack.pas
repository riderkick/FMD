unit JSUnpack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, RegExpr, math;

type
  TStringMap = specialize TFPGMap<String, String>;
  TJSUnpack36 = class
    FDict: TStringMap;
    function Encode36(code: Integer): String;
    function GetIdentifier(c, a: Integer): String;
    function Replace(ARegExpr : TRegExpr): String;
  public
    constructor Create;
    destructor Destroy; override;
    function Unpack(text: String; a, c: Integer; words: TStringArray): String;
  end;

implementation

const
  lookup36 = '0123456789abcdefghijklmnopqrstuvwxyz';

function TJSUnpack36.Encode36(code: Integer): String;
var
  digit: Integer;
  i: Integer = 0;
begin
  Result := '';
  repeat
    digit := (code div Trunc(power(36, i))) mod 36;
    Result := Char(lookup36[digit + 1]) + Result;
    code -= digit * Trunc(power(36, i));
    Inc(i);
  until code <= 0;
end;

constructor TJSUnpack36.Create;
begin
  inherited;
  FDict := TStringMap.Create;
end;

destructor TJSUnpack36.Destroy;
begin
  FDict.Free;
  inherited;
end;

function TJSUnpack36.GetIdentifier(c, a: Integer): String;
begin
  if c < a then Result := ''
  else Result := GetIdentifier(c div a, a);
  c := c mod a;
  if c > 35 then Result += Chr(Byte(c + 29))
  else Result += Encode36(c);
end;

function TJSUnpack36.Replace(ARegExpr : TRegExpr): String;
var
  s: String;
begin
  Result := ARegExpr.Match[0];
  if FDict.TryGetData(ARegExpr.Match[0], s) and (s <> '') then
    Result := s;
end;

function TJSUnpack36.Unpack(text: String; a, c: Integer; words: TStringArray): String;
var
  rg: TRegExpr;
begin
  FDict.Clear;
  rg := TRegExpr.Create('\b\w+\b');
  try
    while c <> 0 do begin
      Dec(c);
      FDict.Add(GetIdentifier(c, a), words[c]);
    end;
    Result := rg.Replace(text, @Replace);
  finally
    rg.Free;
  end;
end;

end.

