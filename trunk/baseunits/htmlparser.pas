{
        File: htmlparser.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit htmlparser;

{$mode delphi}

// use for Batoto only

interface

uses
  SysUtils;

type
  TOnFoundTag  = procedure(Tag: String) of object;
  TOnFoundText = procedure(Text: String) of object;

  THTMLParser = class
    OnFoundTag : TOnFoundTag;
    OnFoundText: TOnFoundText;
    Raw        : PChar;

    constructor Create(const pRaw: PChar);
    destructor  Destroy; override;
    procedure   Exec;
  end;

implementation

function  CopyBuffer(StartIndex: PChar; len: Integer): String;
var
  s: String;
begin
  SetLength(s, len);
  StrLCopy(@s[1], StartIndex, len);
  Result:= s;
  S:= '';
end;

constructor THTMLParser.Create(const pRaw: PChar);
begin
  inherited Create;
  Raw:= pRaw;
end;

destructor  THTMLParser.Destroy;
begin
  Raw:= nil;
  inherited Destroy;
end;

procedure   THTMLParser.Exec;
var
  P,
  startText : PChar;
begin
  P:= Raw; startText:= nil;
  if P <> nil then
  while P^ <> #0 do
  begin
    if P^ in ['<'] then
    begin
      if (startText<>nil) AND (Assigned(OnFoundText)) AND (Integer(P-startText)>0) then
        OnFoundText(CopyBuffer(startText, Integer(P-startText)))
      else
        OnFoundText('');
      startText:= P;
    end
    else
    if P^ in ['>'] then
    begin
      Inc(P);
      if Assigned(OnFoundTag) then
        OnFoundTag(CopyBuffer(startText, Integer(P-startText)));
      startText:= P;
    end;

    Inc(P);
  end;
end;

end.

