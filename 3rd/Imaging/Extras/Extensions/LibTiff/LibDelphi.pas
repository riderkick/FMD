unit LibDelphi;

interface

uses
  Windows, SysUtils;

function  fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
function  sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
function  fputs(s: Pointer; stream: Pointer): Integer; cdecl;
function  fputc(c: Integer; stream: Pointer): Integer; cdecl;
function  isprint(c: Integer): Integer; cdecl;
procedure memset(a: Pointer; b: Integer; c: Cardinal); cdecl;
function  memcpy(dest: Pointer; const src: Pointer; count: Cardinal): Pointer; cdecl;
function  _ftol: Integer; cdecl;
function  malloc(s: Longint): Pointer; cdecl;
procedure free(p: Pointer); cdecl;
function  _ltolower(ch: Integer): Integer; cdecl;
function  _ltoupper(ch: Integer): Integer; cdecl;
function  _ltowlower(ch: Integer): Integer; cdecl;
function  _ltowupper(ch: Integer): Integer; cdecl;
function  strcpy(dest: Pointer; src: Pointer): Pointer; cdecl;

function  sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer): Integer;

var
  __turboFloat: LongBool = False;
  _streams: Integer;

implementation

{PODD}

function fputc(c: Integer; stream: Pointer): Integer; cdecl;
var
  m: array[0..1] of AnsiChar;
  n: Cardinal;
  o: Cardinal;
begin
  if c=13 then
  begin
    m[0]:=#13;
    m[1]:=#10;
    n:=2;
  end
  else
  begin
    m[0]:=AnsiChar(c);
    n:=1;
  end;
  WriteFile(Cardinal(stream),m[0],n,o,nil);
  Result:=c;
end;

function isprint(c: Integer): Integer; cdecl;
begin
  if (c<32) or (127<=c) then
    Result:=0
  else
    Result:=1;
end;

function fputs(s: Pointer; stream: Pointer): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
  o: Cardinal;
begin
  m:=0;
  n:=s;
  while PByte(n)^<>0 do
  begin
    Inc(m);
    Inc(PByte(n));
  end;
  WriteFile(Cardinal(stream),s^,Cardinal(m),o,nil);
  Result:=1;
end;

function sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := sprintfsec(buffer,format,@arguments);
end;

function fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
  o: Cardinal;
begin
  m:=sprintfsec(nil,format,@arguments);
  GetMem(n,m);
  sprintfsec(n,format,@arguments);
  WriteFile(Cardinal(stream),n^,Cardinal(m),o,nil);
  FreeMem(n);
  Result := m;
end;

function strcpy(dest: Pointer; src: Pointer): Pointer; cdecl;
var
  ma,mb: PByte;
  n: Integer;
begin
  ma:=src;
  mb:=dest;
  while True do
  begin
    n:=ma^;
    mb^:=n;
    if n=0 then break;
    Inc(ma);
    Inc(mb);
  end;
  Result:=dest;
end;

function _ltolower(ch: Integer): Integer; cdecl;
begin
  raise Exception.Create('LibDelphi - call to _ltolower - should presumably not occur');
end;

function _ltoupper(ch: Integer): Integer; cdecl;
begin
  raise Exception.Create('LibDelphi - call to _ltoupper - should presumably not occur');
end;

function _ltowlower(ch: Integer): Integer; cdecl;
begin
  raise Exception.Create('LibDelphi - call to _ltowlower - should presumably not occur');
end;

function _ltowupper(ch: Integer): Integer; cdecl;
begin
  raise Exception.Create('LibDelphi - call to _ltowupper - should presumably not occur');
end;

function sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer): Integer;
var
  Modifier: Integer;
  Width: Integer;
  m,ma: PByte;
  mb: Boolean;
  n: PByte;
  o: PByte;
  r: PByte;

procedure Append(const p: AnsiString);
var
  q: Integer;
begin
  if Width>Length(p) then
  begin
    if buffer<>nil then
    begin
      for q:=0 to Width-Length(p)-1 do
      begin
        o^:=Ord('0');
        Inc(o);
      end;
    end
    else
      Inc(o,Width-Length(p));
  end;
  if buffer<>nil then CopyMemory(o,PAnsiChar(p),Length(p));
  Inc(o,Length(p));
end;
begin
  m:=format;
  n:=arguments;
  o:=buffer;
  while True do
  begin
    if m^=0 then break;
    if m^=Ord('%') then
    begin
      ma:=m;
      mb:=True;
      Inc(m);
      Width:=-1;
      Modifier:=0;
      {flags}
      case m^ of
        Ord('-'): mb:=False;
        Ord('+'): mb:=False;
        Ord(' '): mb:=False;
        Ord('#'): mb:=False;
      end;
      if mb then
      begin
        {width}
        case m^ of
          Ord('1')..Ord('9'):
          begin
            Width:=0;
            while True do
            begin
              if (m^<Ord('0')) or (Ord('9')<m^) then break;
              Width:=Width*10+m^-Ord('0');
              Inc(m);
            end;
          end;
          Ord('0'): mb:=False;
          Ord('*'): mb:=False;
        end;
      end;
      if mb then
      begin
        {prec}
        case m^ of
          Ord('.'): mb:=False;
        end;
      end;
      if mb then
      begin
        {modifier}
        case m^ of
          Ord('F'): mb:=False;
          Ord('N'): mb:=False;
          Ord('h'): mb:=False;
          Ord('l'):
          begin
            Modifier:=4;
            Inc(m);
          end;
          Ord('L'): mb:=False;
        end;
      end;
      if mb then
      begin
        {type}
        case m^ of
          Ord('d'):
          begin
            case Modifier of
              0:
              begin
                Append(IntToStr(PInteger(n)^));
                Inc(m);
                Inc(n,SizeOf(Integer));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('i'): mb:=False;
          Ord('o'): mb:=False;
          Ord('u'):
          begin
            case Modifier of
              0,4:
              begin
                Append(IntToStr(PCardinal(n)^));
                Inc(m);
                Inc(n,SizeOf(Cardinal));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('x'):
          begin
            case Modifier of
              0,4:
              begin
                Append(IntToHex(PCardinal(n)^,8));
                Inc(m);
                Inc(n,SizeOf(Cardinal));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('X'): mb:=False;
          Ord('f'): mb:=False;
          Ord('e'): mb:=False;
          Ord('g'):
          begin
            case Modifier of
              0:
              begin
                Append(FloatToStr(PSingle(n)^));
                Inc(m);
                Inc(n,SizeOf(Single));
              end;
            else
              mb:=False;
            end;
          end;
          Ord('E'): mb:=False;
          Ord('G'): mb:=False;
          Ord('c'): mb:=False;
          Ord('s'):
          begin
            r:=PPointer(n)^;
            while r^<>0 do
            begin
              if buffer<>nil then o^:=r^;
              Inc(o);
              Inc(r);
            end;
            Inc(n,SizeOf(Pointer));
            Inc(m);
          end;
          Ord('%'): mb:=False;
          Ord('n'): mb:=False;
          Ord('p'): mb:=False;
        else
          raise Exception.Create('LibDelphi');
        end;
      end;
      if mb=False then
      begin
        m:=ma;
        if buffer<>nil then o^:=m^;
        Inc(o);
        Inc(m);
      end;
    end
    else if m^=10 then
    begin
      if buffer<>nil then o^:=13;
      Inc(o);
      if buffer<>nil then o^:=10;
      Inc(o);
      Inc(m);
    end
    else
    begin
      if buffer<>nil then o^:=m^;
      Inc(o);
      Inc(m);
    end;
  end;
  if buffer<>nil then o^:=0;
  Inc(o);
  Result:=(Cardinal(o)-Cardinal(buffer));
end;

procedure free(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

function malloc(s: Longint): Pointer; cdecl;
begin
  Result := AllocMem(s);
end;

function _ftol: Integer; cdecl;
var
  f: double;
begin
  asm
    lea    eax, f             //  BC++ passes floats on the FPU stack
    fstp  qword ptr [eax]     //  Delphi passes floats on the CPU stack
  end;
  Result := Trunc(f);
end;

function memcpy(dest: Pointer; const src: Pointer; count: Cardinal): Pointer; cdecl;
begin
  CopyMemory(dest,src,count);
  Result:=dest;
end;

procedure memset(a: Pointer; b: Integer; c: Cardinal); cdecl;
begin
  FillMemory(a,c,b);
end;

end.
