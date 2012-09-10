{
        File: genericlib.pas
        License: GPLv3
        This unit is part of Free Manga Downloader
}

unit genericlib;

{$mode objfpc}

interface

type
  generic TGenericList<T> = class
  private
    FindPos: LongInt;
    FindVal: T;
    procedure   Right(const Pos: Cardinal);
    procedure   Left(const Pos: Cardinal);
  public
    Items: array of T;
    Count: Cardinal;
    constructor Create;
    destructor  Destroy;
    procedure   Clear;
    procedure   Add(AValue: T);
    procedure   Insert(AValue: T; Pos: Cardinal);
    procedure   Delete(Pos: Cardinal);
    function    Find(AValue: T): LongInt;
    function    FindNext: LongInt;
    procedure   Exchange(const p1, p2: Cardinal);
    procedure   SetSize(ASize: Cardinal);
  end;

implementation

constructor TGenericList.Create;
begin
  SetLength(Items, 0);
  Count:= 0;
  FindPos:= -1;
  inherited Create;
end;

destructor  TGenericList.Destroy;
begin
  SetLength(Items, 0);
  inherited Destroy;
end;

procedure   TGenericList.Clear;
begin
  SetLength(Items, 0);
  Count:= 0;
  FindPos:= -1;
end;

procedure   TGenericList.Add(AValue: T);
begin
  Inc(Count);
  SetLength(Items, Count);
  Items[Count-1]:= AValue;
end;

procedure   TGenericList.Insert(AValue: T; Pos: Cardinal);
begin
  if Pos<=Count-1 then
  begin
    Inc(Count);
    SetLength(Items, Count);
    Right(Pos);
    Items[Pos]:= AValue;
  end;
end;

procedure   TGenericList.Delete(Pos: Cardinal);
begin
  if Pos<=Count-1 then
  begin
    Left(Pos);
    Dec(Count);
    SetLength(Items, Count);
  end;
end;

function     TGenericList.Find(AValue: T): LongInt;
var I: Cardinal;
begin
  I:= 0;
  result:= -1;
  FindPos:= -1;
  FindVal:= AValue;
  repeat
    if Items[I]=AValue then
    begin
      result:= I;
      FindPos:= I;
      break;
    end;
    Inc(I);
  until (I=Count);
end;

function     TGenericList.FindNext: LongInt;
var I: Cardinal;
begin
  if (FindPos<>-1) AND (FindPos<>Count-1) then
  begin
    I:= FindPos+1;
    result:= -1;
    repeat
      if Items[I]=FindVal then
      begin
        result:= I;
        FindPos:= I;
        break;
      end;
      Inc(I);
    until (I=Count);
  end;
end;

procedure    TGenericList.Exchange(const p1, p2: Cardinal);
var tmp: T;
begin
  if (p1<count) AND (p2<count) then
  begin
    tmp:= Items[p1];
    Items[p1]:= Items[p2];
    Items[p2]:= tmp;
  end;
end;

procedure    TGenericList.SetSize(ASize: Cardinal);
begin
  SetLength(Items, ASize);
  Count:= ASize;
end;

procedure    TGenericList.Right(const Pos: Cardinal);
var I: Cardinal;
begin
  for I:= Count-1 downto Pos+1 do
    Items[I]:= Items[I-1];
end;

procedure    TGenericList.Left(const Pos: Cardinal);
var I: Cardinal;
begin
  for I:= Pos+1 to Count-1 do
    Items[I-1]:= Items[I];
end;

end.

