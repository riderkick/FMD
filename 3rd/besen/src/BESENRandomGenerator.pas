(*******************************************************************************
                                 L I C E N S E
********************************************************************************

BESEN - A ECMAScript Fifth Edition Object Pascal Implementation
Copyright (C) 2009-2015, Benjamin 'BeRo' Rosseaux

The source code of the BESEN ecmascript engine library and helper tools are 
distributed under the Library GNU Lesser General Public License Version 2.1 
(see the file copying.txt) with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a module
which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but you 
are not obligated to do so. If you do not wish to do so, delete this exception
statement from your version.

If you didn't receive a copy of the license, see <http://www.gnu.org/licenses/>
or contact:
      Free Software Foundation
      675 Mass Ave
      Cambridge, MA  02139
      USA

*******************************************************************************)
unit BESENRandomGenerator;
{$i BESEN.inc}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}{$ifdef unix}dl,BaseUnix,Unix,
     UnixType,{$endif}SysUtils,Classes,Math,BESENConstants,BESENTypes,
     BESENObject,BESENValue,BESENCollectorObject;

type TBESENRandomGeneratorTable=array[0..BESEN_CMWCRND_SIZE-1] of longword;

     TBESENRandomGenerator=class(TBESENCollectorObject)
      public
       Table:TBESENRandomGeneratorTable;
       Position:longword;
       Carry:longword;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Reinitialize;
       function Get:longword;
       function GetNumber:TBESENNumber;
     end;

implementation

uses BESEN;

constructor TBESENRandomGenerator.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Reinitialize;
end;

destructor TBESENRandomGenerator.Destroy;
begin
 inherited Destroy;
end;

{$ifdef win32}
type HCRYPTPROV=DWORD;

const PROV_RSA_FULL=1;
      CRYPT_VERIFYCONTEXT=$F0000000;

function CryptAcquireContext(var phProv:HCRYPTPROV;pszContainer:PAnsiChar;pszProvider:PAnsiChar;dwProvType:DWORD; dwFlags:DWORD):BOOL; stdcall; external advapi32 name 'CryptAcquireContextA';
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL;  stdcall; external advapi32 name 'CryptReleaseContext';
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall; external advapi32 name 'CryptGenRandom';

function CoCreateGuid(var guid: TGUID): HResult; stdcall; external 'ole32.dll';
{$endif}

procedure TBESENRandomGenerator.Reinitialize;
const N=25;
      m=7;
      s=7;
      t=15;
      a=longword($8ebfd028);
      b=longword($2b5b2500);
      c=longword($db8b0000);
var LRG,LFSR,k,y,Seed1,Seed2:longword;
    i,j:integer;
    x:array[0..N-1] of longword;
    UnixTimeInMilliSeconds:int64;
{$ifdef unix}
    f:file of longword;
    ura,urb:longword;
{$else}
{$ifdef win32}
    lpc,lpf:int64;
    pp,p:pwidechar;
    st:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};
{$endif}
{$endif}
{$ifdef win32}
 function GenerateRandomBytes(var Buffer;Bytes:Cardinal):boolean;
 var CryptProv:HCRYPTPROV;
 begin
  try
   if not CryptAcquireContext(CryptProv,nil,nil,PROV_RSA_FULL,CRYPT_VERIFYCONTEXT) then begin
    result:=false;
    exit;
   end;
   FillChar(Buffer,Bytes,#0);
   result:=CryptGenRandom(CryptProv,Bytes,@Buffer);
   CryptReleaseContext(CryptProv,0);
  except
   result:=false;
  end;
 end;
 function GetRandomGUIDGarbage:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};
 var g:TGUID;
 begin
  CoCreateGUID(g);
  SetLength(result,sizeof(TGUID));
  move(g,result[1],sizeof(TGUID));
 end;
{$endif}
begin
 UnixTimeInMilliSeconds:=round((SysUtils.Now-25569.0)*86400000.0);
 Seed1:=longword(UnixTimeInMilliSeconds and $ffffffff)+longword(UnixTimeInMilliSeconds shr 32);
 Seed2:=longword(UnixTimeInMilliSeconds shr 32) xor not longword(UnixTimeInMilliSeconds and $ffffffff);
{$ifdef unix}
 ura:=0;
 urb:=0;
 AssignFile(f,'/dev/urandom');
 {$i-}System.reset(f,1);{$i+}
 if ioresult=0 then begin
  System.read(f,ura);
  System.read(f,urb);
  CloseFile(f);
 end else begin
  AssignFile(f,'/dev/random');
  {$i-}System.reset(f,1);{$i+}
  if ioresult=0 then begin
   System.read(f,ura);
   System.read(f,urb);
   CloseFile(f);
  end;
 end;
 Seed1:=Seed1 xor ura;
 Seed2:=Seed2 xor urb;
{$else}
{$ifdef win32}
 QueryPerformanceCounter(lpc);
 QueryPerformanceFrequency(lpf);
 inc(Seed1,timeGetTime+GetCurrentProcessId);
 dec(Seed2,GetTickCount-GetCurrentThreadId);
 inc(Seed1,paramcount);
 Seed1:=Seed1 xor (lpc shr 32);
 Seed2:=Seed2 xor lpc;
 Seed1:=(Seed1*lpc)+(Seed2*(lpf-Seed1));
 Seed2:=(Seed2*lpc)+(Seed1*(lpf-Seed2));
 pp:=GetEnvironmentStringsW;
 if assigned(pp) then begin
  p:=pp;
  while assigned(p) and (p^<>#0) do begin
   while assigned(p) and (p^<>#0) do begin
    inc(Seed1,(Seed2*word(p^)));
    Seed1:=(Seed1*1664525)+1013904223;
    Seed2:=Seed2 xor (Seed1*word(p^));
    Seed2:=Seed2 xor (Seed2 shl 13);
    Seed2:=Seed2 xor (Seed2 shr 17);
    Seed2:=Seed2 xor (Seed2 shl 5);
    inc(p);
   end;
   inc(p);
  end;
  FreeEnvironmentStringsW(pointer(pp));
 end;
 pp:=pointer(GetCommandLineW);
 if assigned(pp) then begin
  p:=pp;
  while assigned(p) and (p^<>#0) do begin
   inc(Seed1,(Seed2*word(p^)));
   Seed1:=(Seed1*1664525)+1013904223;
   Seed2:=Seed2 xor (Seed1*word(p^));
   Seed2:=Seed2 xor (Seed2 shl 13);
   Seed2:=Seed2 xor (Seed2 shr 17);
   Seed2:=Seed2 xor (Seed2 shl 5);
   inc(p);
  end;
 end;
 SetLength(st,4096);
 if GenerateRandomBytes(st[1],length(st)) then begin
  for i:=1 to length(st) do begin
   inc(Seed1,(Seed2*byte(st[i])));
   Seed1:=(Seed1*1664525)+1013904223;
   Seed2:=Seed2 xor (Seed1*byte(st[i]));
   Seed2:=Seed2 xor (Seed2 shl 13);
   Seed2:=Seed2 xor (Seed2 shr 17);
   Seed2:=Seed2 xor (Seed2 shl 5);
  end;
 end;
 st:=GetRandomGUIDGarbage;
 for i:=1 to length(st) do begin
  inc(Seed1,(Seed2*byte(st[i])));
  Seed1:=(Seed1*1664525)+1013904223;
  Seed2:=Seed2 xor (Seed1*byte(st[i]));
  Seed2:=Seed2 xor (Seed2 shl 13);
  Seed2:=Seed2 xor (Seed2 shr 17);
  Seed2:=Seed2 xor (Seed2 shl 5);
 end;
 SetLength(st,0);
{$endif}
{$endif}
 LRG:=not Seed1;
 LFSR:=Seed2;
 for i:=0 to N-1 do begin
  LRG:=(LRG*1664525)+1013904223;
  LFSR:=LFSR xor (LFSR shl 13);
  LFSR:=LFSR xor (LFSR shr 17);
  LFSR:=LFSR xor (LFSR shl 5);
  x[i]:=LRG xor not LFSR;
 end;
 k:=N-1;
 LRG:=Seed1;
 LFSR:=not Seed2;
 for i:=0 to BESEN_CMWCRND_MASK do begin
  LRG:=(LRG*1664525)+1013904223;
  LFSR:=LFSR xor (LFSR shl 13);
  LFSR:=LFSR xor (LFSR shr 17);
  LFSR:=LFSR xor (LFSR shl 5);
  inc(k);
  if k>=N then begin
   for j:=0 to (N-m)-1 do begin
    x[j]:=x[j+m] xor (x[j] shr 1) xor ((x[j] and 1)*a);
   end;
   for j:=(N-m) to N-1 do begin
    x[j]:=x[j+m-N] xor (x[j] shr 1) xor ((x[j] and 1)*a);
   end;
   k:=0;
  end;
  y:=x[k] xor ((x[k] shl s) and b);
  y:=y xor ((y shl t) and c);
  Table[i]:=(LRG+LFSR) xor y;
 end;
 Position:=(x[LFSR and $f] xor Table[LRG and BESEN_CMWCRND_MASK]) and BESEN_CMWCRND_MASK;
end;

function TBESENRandomGenerator.Get:longword;
var t:{$ifdef fpc}qword{$else}int64{$endif};
    x:longword;
begin
 Position:=(Position+1) and BESEN_CMWCRND_MASK;
 t:=(BESEN_CMWCRND_A*Table[Position])+Carry;
 Carry:=t shr 32;
 x:=t+Carry;
 if x<Carry then begin
  inc(x);
  inc(Carry);
 end;
 result:=BESEN_CMWCRND_M-x;
 Table[Position]:=result;
end;

function TBESENRandomGenerator.GetNumber:TBESENNumber;
const f:TBESENNumber=1.0/int64($100000000);
var i:int64;
begin
 i:=Get;
 result:=i*f;
end;

end.
