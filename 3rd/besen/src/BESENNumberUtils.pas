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
unit BESENNumberUtils;
{$i BESEN.inc}

interface

uses SysUtils,Math,BESENConstants,BESENTypes,BESENStringUtils;

type PBESENDoubleBytes=^TBESENDoubleBytes;
     TBESENDoubleBytes=array[0..sizeof(double)-1] of byte;

     TBESENNumberCodeFlagLookupTable=array[word,boolean] of byte;

     PBESENNumberCodeAbstractRelationalLookupTableItem=^TBESENNumberCodeAbstractRelationalLookupTableItem;
     TBESENNumberCodeAbstractRelationalLookupTableItem=packed record
      IsNotUndefined:bytebool;
      ResultBooleanValue:bytebool;
      DoCompare:bytebool;
      Spacer:bytebool;
     end;

     TBESENNumberCodeAbstractRelationalLookupTable=array[byte] of TBESENNumberCodeAbstractRelationalLookupTableItem;

const BESEN_ROUND_TO_NEAREST=0;
      BESEN_ROUND_TOWARD_ZERO=1;
      BESEN_ROUND_UPWARD=2;
      BESEN_ROUND_DOWNWARD=3;

      BESEN_CHECKNUMBERSTRING_FAIL=-1;
      BESEN_CHECKNUMBERSTRING_EMPTY=0;
      BESEN_CHECKNUMBERSTRING_VALID=1;
      BESEN_CHECKNUMBERSTRING_INFNEG=2;
      BESEN_CHECKNUMBERSTRING_INFPOS=3;
      BESEN_CHECKNUMBERSTRING_NAN=4;

      BESEN_DOUBLETOSTRINGMODE_STANDARD=0;
      BESEN_DOUBLETOSTRINGMODE_STANDARDEXPONENTIAL=1;
      BESEN_DOUBLETOSTRINGMODE_FIXED=2;
      BESEN_DOUBLETOSTRINGMODE_EXPONENTIAL=3;
      BESEN_DOUBLETOSTRINGMODE_PRECISION=4;
      BESEN_DOUBLETOSTRINGMODE_RADIX=5;

{$ifdef BIG_ENDIAN}
      BESENDoubleNaN:TBESENDoubleBytes=($7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff);
      BESENDoubleInfPos:TBESENDoubleBytes=($7f,$f0,$00,$00,$00,$00,$00,$00);
      BESENDoubleInfNeg:TBESENDoubleBytes=($ff,$f0,$00,$00,$00,$00,$00,$00);
      BESENDoubleMax:TBESENDoubleBytes=($7f,$ef,$ff,$ff,$ff,$ff,$ff,$ff);
      BESENDoubleMin:TBESENDoubleBytes=($00,$00,$00,$00,$00,$00,$00,$01);
{$else}
      BESENDoubleNaN:TBESENDoubleBytes=($ff,$ff,$ff,$ff,$ff,$ff,$ff,$7f);
      BESENDoubleInfPos:TBESENDoubleBytes=($00,$00,$00,$00,$00,$00,$f0,$7f);
      BESENDoubleInfNeg:TBESENDoubleBytes=($00,$00,$00,$00,$00,$00,$f0,$ff);
      BESENDoubleMax:TBESENDoubleBytes=($ff,$ff,$ff,$ff,$ff,$ff,$ef,$7f);
      BESENDoubleMin:TBESENDoubleBytes=($01,$00,$00,$00,$00,$00,$00,$00);
{$endif}
      BESENDoubleZero:TBESENNumber=0.0;
      BESENDoubleOne:TBESENNumber=1.0;

      BESENNumZero:TBESENNumber=0;
      BESENNumOne:TBESENNumber=1;

var BESENNumberCodeFlagLookupTable:TBESENNumberCodeFlagLookupTable;
    BESENNumberCodeAbstractRelationalLookupTable:TBESENNumberCodeAbstractRelationalLookupTable;

function BESENNumberCodeFlags(const AValue:TBESENNumber):byte; {$ifdef caninline}inline;{$endif}
    
function BESENIntLog2(x:longword):longword; {$ifdef cpu386}assembler; register;{$endif}

function BESENToInt(v:TBESENNumber):TBESENINT64;
function BESENToInt32(v:TBESENNumber):TBESENINT32;
function BESENToUInt32(v:TBESENNumber):TBESENUINT32;
function BESENToInt16(v:TBESENNumber):TBESENINT32;
function BESENToUInt16(v:TBESENNumber):TBESENUINT32;

function BESENToIntFast(v:PBESENNumber):TBESENINT64; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
function BESENToInt32Fast(v:PBESENNumber):TBESENINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
function BESENToUInt32Fast(v:PBESENNumber):TBESENUINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
function BESENToInt16Fast(v:PBESENNumber):TBESENINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
function BESENToUInt16Fast(v:PBESENNumber):TBESENUINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}

function BESENFloatToStr(const Value:TBESENNumber):TBESENString;
function BESENFloatToLocaleStr(const Value:TBESENNumber):TBESENString;

function BESENSameValue(const A,B:TBESENNumber):boolean;
function BESENIsNaN(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsFinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsPosInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsNegInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsPosZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsNegZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENIsNegative(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENNumberAbsolute(const AValue:TBESENNumber):TBESENNumber; {$ifdef caninline}inline;{$endif}
function BESENIsSameValue(const a,b:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
function BESENFloor(FloatValue:TBESENNumber):TBESENNumber; {$ifdef caninline}inline;{$endif}
function BESENCeil(FloatValue:TBESENNumber):TBESENNumber; {$ifdef caninline}inline;{$endif}

function BESENStringToDoubleExact(const StringValue:TBESENString;RoundingMode:longint=BESEN_ROUND_TO_NEAREST;Base:longint=-1;OK:pointer=nil):double;
function BESENStringToDoubleFast(const StringValue:TBESENString;RoundingMode:integer=BESEN_ROUND_TO_NEAREST):double;
function BESENStringToDouble(const StringValue:TBESENString):double;

function BESENStringToNumber(const StringValue:TBESENString;EmptyIsValid:TBESENBoolean=true;AcceptHex:TBESENBoolean=false;OnlyNumber:boolean=false;AcceptHexSign:boolean=true):TBESENNumber;

function BESENStringToNumberBase(const StringValue:TBESENString;Base:longint):TBESENNumber;

function BESENDoubleToString(const AValue:double;Mode,RequestedDigits:longint):{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};

function BESENNumberToString(const Value:TBESENNumber):TBESENString;

function BESENNumberToRadixString(const Value:TBESENNumber;Radix:longint):TBESENString;

function BESENCheckNumberString(const StringValue:TBESENString;var StartPosition,EndPosition:integer;IsParseFloat,AcceptHex,OnlyNumber,AcceptHexSign:boolean):integer;

function BESENModulo(x,y:double):double;{$ifdef cpu386}stdcall; assembler;{$endif}
function BESENModuloPos(x,y:double):double;

implementation

uses BESENLocale;

procedure InitBESENNumberTables;
var i:word;
    j:boolean;
    c,ca,cb:byte;
    Item:PBESENNumberCodeAbstractRelationalLookupTableItem;
begin
 for i:=low(TBESENNumberCodeFlagLookupTable) to high(TBESENNumberCodeFlagLookupTable) do begin
  for j:=low(boolean) to high(boolean) do begin
   c:=0;
   if j then begin
    if (i and $7ff0)=$7ff0 then begin
     c:=c or bncfNAN;
    end;
   end else begin
    if (i and $7ff0)=$7ff0 then begin
     if (i and $000f)<>0 then begin
      c:=c or bncfNAN;
     end else begin
      c:=c or bncfINFINITE;
     end;
    end else if (i and $7fff)=0 then begin
     c:=c or bncfZERO;
    end;
   end;
   if (i and $8000)<>0 then begin
    c:=c or bncfNEGATIVE;
   end;
   BESENNumberCodeFlagLookupTable[i,j]:=c;
  end;
 end;
 for c:=low(byte) to high(byte) do begin
  Item:=@BESENNumberCodeAbstractRelationalLookupTable[c];
  ca:=c shr 4;
  cb:=c and $f;
  Item^.Spacer:=false;
  if ((ca or cb) and bncfNAN)<>0 then begin
   Item^.IsNotUndefined:=false;
   Item^.ResultBooleanValue:=false;
   Item^.DoCompare:=false;
  end else if (((ca and cb) and bncfZERO)<>0) and (((ca xor cb) and bncfNEGATIVE)<>0) then begin
   Item^.IsNotUndefined:=true;
   Item^.ResultBooleanValue:=false;
   Item^.DoCompare:=false;
  end else if (ca and (bncfINFINITE or bncfNEGATIVE))=bncfINFINITE then begin
   Item^.IsNotUndefined:=true;
   Item^.ResultBooleanValue:=false;
   Item^.DoCompare:=false;
  end else if (cb and (bncfINFINITE or bncfNEGATIVE))=bncfINFINITE then begin
   Item^.IsNotUndefined:=true;
   Item^.ResultBooleanValue:=true;
   Item^.DoCompare:=false;
  end else if (cb and (bncfINFINITE or bncfNEGATIVE))=(bncfINFINITE or bncfNEGATIVE) then begin
   Item^.IsNotUndefined:=true;
   Item^.ResultBooleanValue:=false;
   Item^.DoCompare:=false;
  end else if (ca and (bncfINFINITE or bncfNEGATIVE))=(bncfINFINITE or bncfNEGATIVE) then begin
   Item^.IsNotUndefined:=true;
   Item^.ResultBooleanValue:=true;
   Item^.DoCompare:=false;
  end else begin
   Item^.IsNotUndefined:=true;
   Item^.ResultBooleanValue:=false;
   Item^.DoCompare:=true;
  end;
 end;
end;

function BESENNumberCodeFlags(const AValue:TBESENNumber):byte; {$ifdef caninline}inline;{$endif}
{$ifdef fpc}
var HighPart:longword;
begin
 HighPart:=qword(pointer(@AValue)^) shr 32;
 result:=BESENNumberCodeFlagLookupTable[HighPart shr 16,((HighPart and $ffff) or longword(qword(pointer(@AValue)^) and $ffffffff))<>0];
end;
{$else}
{$ifdef cpu386}
asm
 mov ecx,dword ptr [AValue+4]
 mov edx,ecx
 and edx,$0000ffff
 or edx,dword ptr [AValue]
 setnz dl
 and edx,$7f
 shr ecx,16
 mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
end;
{$else}
var HighPart:longword;
begin
 HighPart:=int64(pointer(@AValue)^) shr 32;
 result:=BESENNumberCodeFlagLookupTable[HighPart shr 16,((HighPart and $ffff) or longword(int64(pointer(@AValue)^) and $ffffffff))<>0];
end;
{$endif}
{$endif}

function BESENStringToDoubleExact(const StringValue:TBESENString;RoundingMode:longint=BESEN_ROUND_TO_NEAREST;Base:longint=-1;OK:pointer=nil):double;
type PDoubleCasted=^TDoubleCasted;
     TDoubleCasted=packed record
      case byte of
       0:(Value:double);
       1:({$ifdef BIG_ENDIAN}Hi,Lo{$else}Lo,Hi{$endif}:longword);
       2:(Value64:int64);
     end;
const MantissaWords=12; //6; // 12
      MantissaDigits=52; //28; // 52
      WordTopBit=$8000;
      WordBits=16;
      WordBitShift=4;
      WordBitMask=WordBits-1;
      WordMask=$ffff;
      IEEEFormatBytes=8;
      IEEEFormatBits=IEEEFormatBytes shl 3;
      IEEEFormatExplicit=0;
      IEEEFormatExponent=11;
      IEEEFormatOneMask=WordTopBit shr ((IEEEFormatExponent+IEEEFormatExplicit) and WordBitMask);
      IEEEFormatOnePos=(IEEEFormatExponent+IEEEFormatExplicit) shr WordBitShift;
      IEEEFormatExpMax=1 shl (IEEEFormatExponent-1);
      Bit53=int64(int64(1) shl 53);
      MaximumMultiplier=longword(longword($ffffffff) div 36);
{$ifndef BESENNoFPU}
      DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
      DtoAFPURoundingMode:TFPURoundingMode=rmNEAREST;
{$endif}
type PWords=^TWords;
     TWords=array[0..MantissaWords] of word;
     PTemp=^TTemp;
     TTemp=array[0..MantissaWords*2] of longword;
     PDigits=^TDigits;
     TDigits=array[0..MantissaDigits] of byte;
var MantissaPosition,Exponent,TenPower,TwoPower,ExtraTwos,Shift,i,p,q,r,Digit,Overflow,OverflowBits,DroppedBits,DroppedBitsMask,MiddleValue:longint;
    Bit,Carry:word;
    Negative,ExponentNegative,HasDigits,Started,ZeroTail,Done:boolean;
    ResultCasted:PDoubleCasted;
    Temp:PTemp;
    Digits:PDigits;
    MantissaMultiplicator,Mantissa:PWords;
    Value:int64;
    c:widechar;
    Part,Multiplier,NextMultiplier:longword;
{$ifndef BESENNoFPU}
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPUPrecisionMode:TFPUPrecisionMode;
    OldFPURoundingMode:TFPURoundingMode;
{$endif}
 function MantissaMultiply(vTo,vFrom:PWords):longint;
 var i,j,k:longint;
     v:longword;
     t:PTemp;
 begin
  t:=Temp;
  FillChar(t^,sizeof(TTemp),#0);
  for i:=0 to MantissaWords-1 do begin
   for j:=0 to MantissaWords-1 do begin
    v:=longword(vTo^[i]+0)*longword(vFrom^[j]+0);
    k:=i+j;
    inc(t^[k],v shr WordBits);
    inc(t^[k+1],v and WordMask);
   end;
  end;
  for i:=high(TTemp) downto 1 do begin
   inc(t^[i-1],t^[i] shr WordBits);
   t^[i]:=t^[i] and WordMask;
  end;
  if (t^[0] and WordTopBit)<>0 then begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=t^[i] and WordMask;
   end;
   result:=0;
  end else begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=(t^[i] shl 1)+word(ord((t^[i+1] and WordTopBit)<>0));
   end;
   result:=-1;
  end;
 end;
 procedure MantissaShiftRight(var Mantissa:TWords;Shift:longint);
 var Bits,Words,InvBits,Position:longint;
     Carry,Current:longword;
 begin
  Bits:=Shift and WordBitMask;
  Words:=Shift shr WordBitShift;
  InvBits:=WordBits-Bits;
  Position:=high(TWords);
  if Bits=0 then begin
   if Words<>0 then begin
    while Position>=Words do begin
     Mantissa[Position]:=Mantissa[Position-Words];
     dec(Position);
    end;
   end;
  end else begin
   if (high(TWords)-Words)>=0 then begin
    Carry:=Mantissa[high(TWords)-Words] shr Bits;
   end else begin
    Carry:=0;
   end;
   while Position>Words do begin
    Current:=Mantissa[Position-(Words+1)];
    Mantissa[Position]:=(Current shl InvBits) or Carry;
    Carry:=Current shr Bits;
    dec(Position);
   end;
   Mantissa[Position]:=Carry;
   dec(Position);
  end;
  while Position>=0 do begin
   Mantissa[Position]:=0;
   dec(Position);
  end;
 end;
 procedure MantissaSetBit(var Mantissa:TWords;i:longint);
 begin
  Mantissa[i shr WordBitShift]:=Mantissa[i shr WordBitShift] or (WordTopBit shr (i and WordBitMask));
 end;
 function MantissaTestBit(var Mantissa:TWords;i:longint):boolean;
 begin
  result:=(Mantissa[i shr WordBitShift] shr ((not i) and WordBitMask))<>0;
 end;
 function MantissaIsZero(var Mantissa:TWords):boolean;
 var i:longint;
 begin
  result:=true;
  for i:=low(TWords) to High(TWords) do begin
   if Mantissa[i]<>0 then begin
    result:=false;
    break;
   end;
  end;
 end;
 function MantissaRound(Negative:boolean;var Mantissa:TWords;BitPos:longint):boolean;
 var i,p:longint;
     Bit:longword;
  function RoundAbsDown:boolean;
  var j:longint;
  begin
   Mantissa[i]:=Mantissa[i] and not (Bit-1);
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   result:=false;
  end;
  function RoundAbsUp:boolean;
  var j:longint;
  begin
   Mantissa[i]:=(Mantissa[i] and not (Bit-1))+Bit;
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   while (i>0) and (Mantissa[i]=0) do begin
    dec(i);
    inc(Mantissa[i]);
   end;
   result:=Mantissa[0]=0;
  end;
  function RoundTowardsInfinity:boolean;
  var j:longint;
      m:longword;
  begin
   m:=Mantissa[i] and ((Bit shl 1)-1);
   for j:=i+1 to high(TWords) do begin
    m:=m or Mantissa[j];
   end;
   if m<>0 then begin
    result:=RoundAbsUp;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
  function RoundNear:boolean;
  var j:longint;
      m:longword;
  begin
   if (Mantissa[i] and Bit)<>0 then begin
    Mantissa[i]:=Mantissa[i] and not Bit;
    m:=Mantissa[i] and ((Bit shl 1)-1);
    for j:=i+1 to high(TWords) do begin
     m:=m or Mantissa[j];
    end;
    Mantissa[i]:=Mantissa[i] or Bit;
    if m<>0 then begin
     result:=RoundAbsUp;
    end else begin
     if MantissaTestBit(Mantissa,BitPos-1) then begin
      result:=RoundAbsUp;
     end else begin
      result:=RoundAbsDown;
     end;
    end;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
 begin
  i:=BitPos shr WordBitShift;
  p:=BitPos and WordBitMask;
  Bit:=WordTopBit shr p;
  case RoundingMode of
   BESEN_ROUND_TO_NEAREST:begin
    result:=RoundNear;
   end;
   BESEN_ROUND_TOWARD_ZERO:begin
    result:=RoundAbsDown;
   end;
   BESEN_ROUND_UPWARD:begin
    if Negative then begin
     result:=RoundAbsDown;
    end else begin
     result:=RoundTowardsInfinity;
    end;
   end;
   BESEN_ROUND_DOWNWARD:begin
    if Negative then begin
     result:=RoundTowardsInfinity;
    end else begin
     result:=RoundAbsDown;
    end;
   end;
   else begin
    result:=false;
   end;
  end;
 end;
 function CountLeadingZeros32(a:longword):longint;
 const CountLeadingZerosHigh:array[byte] of byte=(8,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,
                                                  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 begin
  result:=0;
  if a<$10000 then begin
   inc(result,16);
   a:=a shl 16;
  end;
  if a<$1000000 then begin
   inc(result,8);
   a:=a shl 8;
  end;
  inc(result,CountLeadingZerosHigh[a shr 24]);
 end;
 function CountLeadingZeros64(a:int64):longint;
 begin
 if a<int64($100000000) then begin
   result:=32;
  end else begin
   result:=0;
   a:=a shr 32;
  end;
  inc(result,CountLeadingZeros32(a));
 end;
begin
 if assigned(OK) then begin
  longbool(OK^):=false;
 end;
 ResultCasted:=pointer(@result);
 ResultCasted^.Hi:=$7ff80000;
 ResultCasted^.Lo:=$00000000;
 i:=1;
 p:=i;
 while (i<=length(StringValue)) and BESENUnicodeIsStringWhiteSpace(word(widechar(StringValue[i]))) do begin
  inc(i);
 end;
 if (i<=length(StringValue)) and ((StringValue[i]='-') or (StringValue[i]='+')) then begin
  Negative:=StringValue[i]='-';
  inc(i);
 end else begin
  Negative:=false;
 end;
 HasDigits:=false;
 if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  if Negative then begin
   ResultCasted^.Hi:=$fff00000;
   ResultCasted^.Lo:=$00000000;
  end else begin
   ResultCasted^.Hi:=$7ff00000;
   ResultCasted^.Lo:=$00000000;
  end;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  ResultCasted^.Hi:=$7ff80000;
  ResultCasted^.Lo:=$00000000;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else if (Base in [2,4,8,16,32]) or ((not (Base in [2..36])) and ((((i+1)<=length(StringValue)) and ((StringValue[i]='0') and ((StringValue[i+1]='b') or (StringValue[i+1]='o') or (StringValue[i+1]='x')))))) then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  case Base of
   2:begin
    Shift:=1;
   end;
   4:begin
    Shift:=2;
   end;
   8:begin
    Shift:=3;
   end;
   16:begin
    Shift:=4;
   end;
   32:begin
    Shift:=5;
   end;
   else begin
    inc(i);
    case StringValue[i] of
     'b':begin
      Shift:=1;
     end;
     'o':begin
      Shift:=3;
     end;
     else {'x':}begin
      Shift:=4;
     end;
    end;
    inc(i);
   end;
  end;
  TwoPower:=1 shl Shift;
  Value:=0;
  Exponent:=0;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   q:=0;
   Digit:=0;
   while i<=length(StringValue) do begin
    c:=StringValue[i];
    if ((c>='0') and (c<='9')) and (ord(c)<=(ord('0')+TwoPower)) then begin
     Digit:=ord(c)-ord('0');
    end else if (TwoPower>10) and (((c>='a') and (c<='z')) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('a'))+10;
    end else if (TwoPower>10) and (((c>='A') and (c<='Z')) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('A'))+10;
    end else begin
     break;
    end;
    inc(i);
    HasDigits:=true;
    Value:=(Value shl Shift) or Digit;
    Overflow:=longint(int64(Value shr 53));
    if Overflow<>0 then begin
     OverflowBits:=1;
     while Overflow>1 do begin
      inc(OverflowBits);
      Overflow:=Overflow shr 1;
     end;
     DroppedBitsMask:=(1 shl OverflowBits)-1;
     DroppedBits:=Value and DroppedBitsMask;
     Value:=Value shr OverflowBits;
     Exponent:=OverflowBits;
     ZeroTail:=true;
     while i<=length(StringValue) do begin
      c:=StringValue[i];
      if ((c>='0') and (c<='9')) and (ord(c)<=(ord('0')+TwoPower)) then begin
       Digit:=ord(c)-ord('0');
      end else if (TwoPower>10) and (((c>='a') and (c<='z')) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (TwoPower>10) and (((c>='A') and (c<='Z')) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       break;
      end;
      inc(i);
      if Digit<>0 then begin
       ZeroTail:=false;
      end;
      inc(Exponent,Shift);
     end;
     MiddleValue:=1 shl (OverflowBits-1);
     if DroppedBits>MiddleValue then begin
      inc(Value);
     end else if DroppedBits=MiddleValue then begin
      if ((Value and 1)<>0) or not ZeroTail then begin
       inc(Value);
      end;
     end;
     while (Value and Bit53)<>0 do begin
      Value:=Value shr 1;
      inc(Exponent);
     end;
     break;
    end;
   end;
   if (Exponent<IEEEFormatExponent) and (Value<Bit53) then begin
    Shift:=CountLeadingZeros64(Value)-11;
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((int64($432-(Shift-Exponent)) shl 52)+Value) and int64($7fffffffffffffff);
   end else begin
    New(Mantissa);
    try
     FillChar(Mantissa^,sizeof(TWords),#0);
     Shift:=CountLeadingZeros64(Value);
     Value:=Value shl Shift;
     inc(Exponent,64-Shift);
     Mantissa^[0]:=(Value shr 48) and $ffff;
     Mantissa^[1]:=(Value shr 32) and $ffff;
     Mantissa^[2]:=(Value shr 16) and $ffff;
     Mantissa^[3]:=(Value shr 0) and $ffff;
     if (Mantissa^[0] and WordTopBit)<>0 then begin
      dec(Exponent);
      if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
       inc(Exponent,IEEEFormatExpMax-1);
       MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
        MantissaShiftRight(Mantissa^,1);
        inc(Exponent);
       end;
       if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end;
      end else if Exponent>0 then begin
       ResultCasted^.Hi:=$7ff00000;
       ResultCasted^.Lo:=$00000000;
      end else begin
       Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
       MantissaShiftRight(Mantissa^,Shift);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
        Exponent:=1;
        if IEEEFormatExplicit=0 then begin
         Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
        end;
        Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end else begin
        if MantissaIsZero(Mantissa^) then begin
         ResultCasted^.Hi:=$00000000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end;
      end;
     end else begin
      ResultCasted^.Hi:=$00000000;
      ResultCasted^.Lo:=$00000000;
     end;
    finally
     Dispose(Mantissa);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    longbool(OK^):=true;
   end;
  end;
 end else if Base in [2..36] then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
{$ifndef BESENNoFPU}
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
{$endif}
   try
{$ifndef BESENNoFPU}
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(DtoAFPURoundingMode);
    end;
{$endif}
    Part:=0;
    Multiplier:=1;
    Digit:=0;
    Done:=false;
    while not Done do begin
     while true do begin
      c:=StringValue[i];
      if ((c>='0') and (c<='9')) and (ord(c)<=(ord('0')+Base)) then begin
       Digit:=ord(c)-ord('0');
      end else if (Base>10) and (((c>='a') and (c<='z')) and (ord(c)<=((ord('a')+Base)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (Base>10) and (((c>='A') and (c<='Z')) and (ord(c)<=((ord('A')+Base)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       Done:=true;
       break;
      end;
      HasDigits:=true;
      NextMultiplier:=Multiplier*longword(Base);
      if NextMultiplier>MaximumMultiplier then begin
       break;
      end;
      Part:=(Part*longword(Base))+longword(Digit);
      Multiplier:=NextMultiplier;
      Assert(Multiplier>Part);
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       break;
      end;
     end;
     ResultCasted^.Value:=(ResultCasted^.Value*Multiplier)+Part;
    end;
   finally
{$ifndef BESENNoFPU}
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
{$endif}
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    longbool(OK^):=true;
   end;
  end;
 end else begin
  HasDigits:=false;
  Value:=0;
  q:=i;
  while i<=length(StringValue) do begin
   c:=StringValue[i];
   if (c>='0') and (c<='9') then begin
    HasDigits:=true;
    Value:=(Value*10)+(ord(c)-ord('0'));
    if (Value shr 53)<>0 then begin
     HasDigits:=false;
     break;
    end;
   end else begin
    HasDigits:=false;
    break;
   end;
   inc(i);
  end;
  if HasDigits then begin
   if Value=0 then begin
    ResultCasted^.Hi:=$00000000;
    ResultCasted^.Lo:=$00000000;
   end else begin
    Shift:=CountLeadingZeros64(Value)-11;
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((int64($432-Shift) shl 52)+Value) and int64($7fffffffffffffff);
   end;
   if Negative then begin
    ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
   end;
   if assigned(OK) then begin
    longbool(OK^):=true;
   end;
  end else begin
   i:=q;
   New(MantissaMultiplicator);
   New(Mantissa);
   New(Temp);
   New(Digits);
   try
    FillChar(Digits^,sizeof(TDigits),#0);

    p:=0;
    TenPower:=0;
    HasDigits:=false;
    Started:=false;
    ExponentNegative:=false;
    Exponent:=0;
    while i<=length(StringValue) do begin
     case StringValue[i] of
      '0':begin
       HasDigits:=true;
       inc(i);
      end;
      else begin
       break;
      end;
     end;
    end;
    while i<=length(StringValue) do begin
     case StringValue[i] of
      '0'..'9':begin
       HasDigits:=true;
       Started:=true;
       if p<=high(TDigits) then begin
        Digits^[p]:=ord(StringValue[i])-ord('0');
        inc(p);
       end;
       inc(TenPower);
       inc(i);
      end;
      else begin
       break;
      end;
     end;
    end;
    if (i<=length(StringValue)) and (StringValue[i]='.') then begin
     inc(i);
     if not Started then begin
      while i<=length(StringValue) do begin
       case StringValue[i] of
        '0':begin
         HasDigits:=true;
         dec(TenPower);
         inc(i);
        end;
        else begin
         break;
        end;
       end;
      end;
     end;
     while i<=length(StringValue) do begin
      case StringValue[i] of
       '0'..'9':begin
        HasDigits:=true;
        if p<=high(TDigits) then begin
         Digits^[p]:=ord(StringValue[i])-ord('0');
         inc(p);
        end;
        inc(i);
       end;
       else begin
        break;
       end;
      end;
     end;
    end;
    if HasDigits then begin
     if (i<=length(StringValue)) and ((StringValue[i]='e') or (StringValue[i]='E')) then begin
      inc(i);
      if (i<=length(StringValue)) and ((StringValue[i]='+') or (StringValue[i]='-')) then begin
       ExponentNegative:=StringValue[i]='-';
       inc(i);
      end;
      HasDigits:=false;
      while i<=length(StringValue) do begin
       case StringValue[i] of
        '0'..'9':begin
         Exponent:=(Exponent*10)+longint(ord(StringValue[i])-ord('0'));
         HasDigits:=true;
         inc(i);
        end;
        else begin
         break;
        end;
       end;
      end;
     end;
     if HasDigits then begin
      if ExponentNegative then begin
       dec(TenPower,Exponent);
      end else begin
       inc(TenPower,Exponent);
      end;

      FillChar(Mantissa^,sizeof(TWords),#0);

      Bit:=WordTopBit;
      q:=0;
      Started:=false;
      TwoPower:=0;
      MantissaPosition:=0;
      while MantissaPosition<MantissaWords do begin
       Carry:=0;
       while (p>q) and (Digits^[p-1]=0) do begin
        dec(p);
       end;
       if p<=q then begin
        break;
       end;
       r:=p;
       while r>q do begin
        dec(r);
        i:=(2*Digits^[r])+Carry;
        if i>=10 then begin
         dec(i,10);
         Carry:=1;
        end else begin
         Carry:=0;
        end;
        Digits^[r]:=i;
       end;
       if Carry<>0 then begin
        Mantissa^[MantissaPosition]:=Mantissa^[MantissaPosition] or Bit;
        Started:=true;
       end;
       if Started then begin
        if Bit=1 then begin
         Bit:=WordTopBit;
         inc(MantissaPosition);
        end else begin
         Bit:=Bit shr 1;
        end;
       end else begin
        dec(TwoPower);
       end;
      end;
      inc(TwoPower,TenPower);

      if TenPower<0 then begin
       for i:=0 to high(TWords)-1 do begin
        MantissaMultiplicator^[i]:=$cccc;
       end;
       MantissaMultiplicator^[high(TWords)]:=$cccd;
       ExtraTwos:=-2;
       TenPower:=-TenPower;
      end else if TenPower>0 then begin
       MantissaMultiplicator^[0]:=$a000;
       for i:=1 to high(TWords) do begin
        MantissaMultiplicator^[i]:=$0000;
       end;
       ExtraTwos:=3;
      end else begin
       ExtraTwos:=0;
      end;
      while TenPower<>0 do begin
       if (TenPower and 1)<>0 then begin
        inc(TwoPower,ExtraTwos+MantissaMultiply(Mantissa,MantissaMultiplicator));
       end;
       inc(ExtraTwos,ExtraTwos+MantissaMultiply(MantissaMultiplicator,MantissaMultiplicator));
       TenPower:=TenPower shr 1;
      end;

      Exponent:=TwoPower;
      if (Mantissa^[0] and WordTopBit)<>0 then begin
       dec(Exponent);

       if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
        inc(Exponent,IEEEFormatExpMax-1);
        MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
         MantissaShiftRight(Mantissa^,1);
         inc(Exponent);
        end;
        if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
         ResultCasted^.Hi:=$7ff00000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end else if Exponent>0 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
        MantissaShiftRight(Mantissa^,Shift);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
         Exponent:=1;
         if IEEEFormatExplicit=0 then begin
          Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
         end;
         Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end else begin
         if MantissaIsZero(Mantissa^) then begin
          ResultCasted^.Hi:=$00000000;
          ResultCasted^.Lo:=$00000000;
         end else begin
          ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
          ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
         end;
        end;
       end;
      end else begin
       ResultCasted^.Hi:=$00000000;
       ResultCasted^.Lo:=$00000000;
      end;
      if Negative then begin
       ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
      end;
      if assigned(OK) then begin
       longbool(OK^):=true;
      end;
     end;
    end;
   finally
    Dispose(MantissaMultiplicator);
    Dispose(Mantissa);
    Dispose(Temp);
    Dispose(Digits);
   end;
  end;
 end;
end;

function BESENConvertMantissaExponentToDouble(Mantissa:int64;MantissaSize,FractionalDigits,FractionalExponent,Exponent,ExponentSign:integer):double; {$ifdef caninline}inline;{$endif}
const MaxExponentOfTen=308;
      MaxExponentOfTenMinusTwo=MaxExponentOfTen-2;
      PowersOfTen:array[0..8] of TBESENParsingNumberType=(1e1,1e2,1e4,1e8,1e16,1e32,1e64,1e128,1e256);
var FloatValue,ExponentFactor:TBESENParsingNumberType;
    MantissaExponent,PartExponent,Index:integer;
{$ifndef BESENNoFPU}
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
{$endif}
begin
{$ifndef BESENNoFPU}
 OldFPUExceptionMask:=GetExceptionMask;
 OldFPURoundingMode:=GetRoundMode;
 OldFPUPrecisionMode:=GetPrecisionMode;
{$endif}
 try
{$ifndef BESENNoFPU}
  if OldFPUExceptionMask<>BESENFPUExceptionMask then begin
   SetExceptionMask(BESENFPUExceptionMask);
  end;
  if OldFPURoundingMode<>rmNearest then begin
   SetRoundMode(rmNearest);
  end;
  if OldFPUPrecisionMode<>BESENFPUPrecisionMode then begin
   SetPrecisionMode(BESENFPUPrecisionMode);
  end;
{$endif}

  FloatValue:=Mantissa;

  MantissaExponent:=FractionalDigits+FractionalExponent;
  if MantissaSize>18 then begin
   dec(MantissaExponent,MantissaSize-18);
  end;

  if ExponentSign>0 then begin
   dec(Exponent,MantissaExponent);
  end else begin
   inc(Exponent,MantissaExponent);
  end;
  if Exponent<0 then begin
   ExponentSign:=-ExponentSign;
   Exponent:=-Exponent;
  end;

  while Exponent>0 do begin
   PartExponent:=Exponent;
   if PartExponent>MaxExponentOfTenMinusTwo then begin
    PartExponent:=MaxExponentOfTenMinusTwo;
   end;
   dec(Exponent,PartExponent);

   Index:=0;
   ExponentFactor:=1;
   while (PartExponent<>0) and (Index<length(PowersOfTen)) do begin
    if (PartExponent and 1)<>0 then begin
     ExponentFactor:=ExponentFactor*PowersOfTen[Index];
    end;
    inc(Index);
    PartExponent:=PartExponent shr 1;
   end;

   if ExponentSign>0 then begin
    FloatValue:=FloatValue*ExponentFactor;
   end else begin
    FloatValue:=FloatValue/ExponentFactor;
   end;
  end;
  result:=FloatValue;
 finally
{$ifndef BESENNoFPU}
  if OldFPUExceptionMask<>BESENFPUExceptionMask then begin
   SetExceptionMask(OldFPUExceptionMask);
  end;
  if OldFPURoundingMode<>rmNearest then begin
   SetRoundMode(OldFPURoundingMode);
  end;
  if OldFPUPrecisionMode<>BESENFPUPrecisionMode then begin
   SetPrecisionMode(OldFPUPrecisionMode);
  end;
{$endif}
 end;
end;

function BESENStringToDoubleFast(const StringValue:TBESENString;RoundingMode:integer=BESEN_ROUND_TO_NEAREST):double;
type PDoubleCasted=^TDoubleCasted;
     TDoubleCasted=packed record
      case byte of
       0:(Value:double);
       1:({$ifdef BIG_ENDIAN}Hi,Lo{$else}Lo,Hi{$endif}:longword);
     end;
const MaxExponentOfTen=308;
      MaxExponentOfTenMinusTwo=MaxExponentOfTen-2;
      PowersOfTen:array[0..8] of double=(1e1,1e2,1e4,1e8,1e16,1e32,1e64,1e128,1e256);
var i:integer;
    Negative,HasDigits:boolean;
    Mantissa:int64;
    MantissaSize,FractionalDigits,FractionalExponent,ExponentSign,Exponent:integer;
    ResultCasted:PDoubleCasted;
    FloatValue,ExponentFactor:double;
    MantissaExponent,PartExponent,Index:integer;
{$ifndef BESENNoFPU}
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode,NewFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
{$endif}
begin
 ResultCasted:=pointer(@result);
 ResultCasted^.Hi:=$7ff80000;
 ResultCasted^.Lo:=$00000000;
 i:=1;
 while (i<=length(StringValue)) and (BESENUnicodeIsStringWhiteSpace(word(widechar(StringValue[i])))) do begin
  inc(i);
 end;
 if (i<=length(StringValue)) and ((StringValue[i]='-') or (StringValue[i]='+')) then begin
  Negative:=StringValue[i]='-';
  inc(i);
 end else begin
  Negative:=false;
 end;
 if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  if Negative then begin
   ResultCasted^.Hi:=$fff00000;
   ResultCasted^.Lo:=$00000000;
  end else begin
   ResultCasted^.Hi:=$7ff00000;
   ResultCasted^.Lo:=$00000000;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  ResultCasted^.Hi:=$7ff80000;
  ResultCasted^.Lo:=$00000000;
 end else begin
  FractionalDigits:=0;
  FractionalExponent:=0;
  MantissaSize:=0;
  Mantissa:=0;
  HasDigits:=false;
  ExponentSign:=1;
  Exponent:=0;
  while i<=length(StringValue) do begin
   case word(widechar(StringValue[i])) of
    ord('0'):begin
     HasDigits:=true;
     inc(i);
    end;
    else begin
     break;
    end;
   end;
  end;
  while i<=length(StringValue) do begin
   case word(widechar(StringValue[i])) of
    ord('0')..ord('9'):begin
     if MantissaSize<18 then begin
      Mantissa:=(Mantissa*10)+(word(widechar(StringValue[i]))-ord('0'));
     end;
     inc(MantissaSize);
     HasDigits:=true;
     inc(i);
    end;
    else begin
     break;
    end;
   end;
  end;
  if (i<=length(StringValue)) and (StringValue[i]='.') then begin
   inc(i);
   if (MantissaSize=0) and (Mantissa=0) then begin
    while i<=length(StringValue) do begin
     case word(widechar(StringValue[i])) of
      ord('0'):begin
       inc(FractionalExponent);
       HasDigits:=true;
       inc(i);
      end;
      else begin
       break;
      end;
     end;
    end;
   end;
   while i<=length(StringValue) do begin
    case word(widechar(StringValue[i])) of
     ord('0')..ord('9'):begin
      if MantissaSize<18 then begin
       Mantissa:=(Mantissa*10)+(word(widechar(StringValue[i]))-ord('0'));
       inc(MantissaSize);
       inc(FractionalDigits);
      end;
      HasDigits:=true;
      inc(i);
     end;
     else begin
      break;
     end;
    end;
   end;
  end;
  if HasDigits then begin
   if (i<=length(StringValue)) and ((StringValue[i]='e') or (StringValue[i]='E')) then begin
    inc(i);
    if (i<=length(StringValue)) and ((StringValue[i]='+') or (StringValue[i]='-')) then begin
     if StringValue[i]='-' then begin
      ExponentSign:=-1;
     end;
     inc(i);
    end;
    HasDigits:=false;
    while i<=length(StringValue) do begin
     case word(widechar(StringValue[i])) of
      ord('0')..ord('9'):begin
       Exponent:=(Exponent*10)+integer(word(widechar(StringValue[i]))-ord('0'));
       HasDigits:=true;
       inc(i);
      end;
      else begin
       break;
      end;
     end;
    end;
   end;
   if HasDigits then begin

{$ifndef BESENNoFPU}
    case RoundingMode of
     BESEN_ROUND_TO_NEAREST:begin
      NewFPURoundingMode:=rmNearest;
     end;
     BESEN_ROUND_TOWARD_ZERO:begin
      NewFPURoundingMode:=rmTruncate;
     end;
     BESEN_ROUND_UPWARD:begin
      NewFPURoundingMode:=rmUp;
     end;
     BESEN_ROUND_DOWNWARD:begin
      NewFPURoundingMode:=rmDown;
     end;
     else begin
      NewFPURoundingMode:=rmNearest;
     end;
    end;

    OldFPUExceptionMask:=GetExceptionMask;
    OldFPURoundingMode:=GetRoundMode;
    OldFPUPrecisionMode:=GetPrecisionMode;
{$endif}
    try
{$ifndef BESENNoFPU}
     if OldFPUExceptionMask<>BESENFPUExceptionMask then begin
      SetExceptionMask(BESENFPUExceptionMask);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(NewFPURoundingMode);
     end;
     if OldFPUPrecisionMode<>BESENFPUPrecisionMode then begin
      SetPrecisionMode(BESENFPUPrecisionMode);
     end;
{$endif}

     FloatValue:=Mantissa;

     MantissaExponent:=FractionalDigits+FractionalExponent;
     if MantissaSize>18 then begin
      dec(MantissaExponent,MantissaSize-18);
     end;

     if ExponentSign>0 then begin
      dec(Exponent,MantissaExponent);
     end else begin
      inc(Exponent,MantissaExponent);
     end;
     if Exponent<0 then begin
      ExponentSign:=-ExponentSign;
      Exponent:=-Exponent;
     end;

     while Exponent>0 do begin
      PartExponent:=Exponent;
      if PartExponent>MaxExponentOfTenMinusTwo then begin
       PartExponent:=MaxExponentOfTenMinusTwo;
      end;
      dec(Exponent,PartExponent);

      Index:=0;
      ExponentFactor:=1;
      while (PartExponent<>0) and (Index<length(PowersOfTen)) do begin
       if (PartExponent and 1)<>0 then begin
        ExponentFactor:=ExponentFactor*PowersOfTen[Index];
       end;
       inc(Index);
       PartExponent:=PartExponent shr 1;
      end;

      if ExponentSign>0 then begin
       FloatValue:=FloatValue*ExponentFactor;
      end else begin
       FloatValue:=FloatValue/ExponentFactor;
      end;
     end;

     ResultCasted^.Value:=FloatValue;
     if Negative then begin
      ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
     end;
    finally
{$ifndef BESENNoFPU}
     if OldFPUExceptionMask<>BESENFPUExceptionMask then begin
      SetExceptionMask(OldFPUExceptionMask);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(OldFPURoundingMode);
     end;
     if OldFPUPrecisionMode<>BESENFPUPrecisionMode then begin
      SetPrecisionMode(OldFPUPrecisionMode);
     end;
{$endif}
    end;
   end;
  end;
 end;
end;

function BESENStringToDouble(const StringValue:TBESENString):double;
var OK:longbool;
begin
 OK:=false;
 result:=BESENStringToDoubleExact(StringValue,BESEN_ROUND_TO_NEAREST,-1,@OK);
 if not OK Then begin
  result:=TBESENNumber(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENCheckNumberString(const StringValue:TBESENString;var StartPosition,EndPosition:integer;IsParseFloat,AcceptHex,OnlyNumber,AcceptHexSign:boolean):integer;
var i,OldPosition:integer;
    Negative,HasDigits,HasSign:boolean;
begin
 result:=BESEN_CHECKNUMBERSTRING_FAIL;
 i:=1;
 while (i<=length(StringValue)) and (BESENUnicodeIsStringWhiteSpace(word(widechar(StringValue[i])))) do begin
  inc(i);
 end;
 StartPosition:=i;
 EndPosition:=i;
 if (i<=length(StringValue)) and ((StringValue[i]='-') or (StringValue[i]='+')) then begin
  Negative:=StringValue[i]='-';
  HasSign:=true;
  inc(i);
 end else begin
  Negative:=false;
  HasSign:=false;
 end;
 if i>length(StringValue) then begin
  result:=BESEN_CHECKNUMBERSTRING_EMPTY;
 end else if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  EndPosition:=i+7;
  if Negative then begin
   result:=BESEN_CHECKNUMBERSTRING_INFNEG;
  end else begin
   result:=BESEN_CHECKNUMBERSTRING_INFPOS;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  EndPosition:=i+2;
  result:=BESEN_CHECKNUMBERSTRING_NAN;
 end else begin
  HasDigits:=false;
  if (i<=length(StringValue)) and (StringValue[i]='0') then begin
   inc(i);
   HasDigits:=true;
   if (i<=length(StringValue)) and ((StringValue[i]='x') or (StringValue[i]='X')) then begin
    inc(i);
    HasDigits:=false;
    while i<=length(StringValue) do begin
     case word(widechar(StringValue[i])) of
      ord('0')..ord('9'),ord('a')..ord('f'),ord('A')..ord('F'):begin
       HasDigits:=true;
       inc(i);
      end;
      else begin
       break;
      end;
     end;
    end;
    if HasDigits then begin
     EndPosition:=i-1;
     if AcceptHex and (AcceptHexSign or not HasSign) then begin
      result:=BESEN_CHECKNUMBERSTRING_VALID;
     end else begin
      result:=BESEN_CHECKNUMBERSTRING_FAIL;
     end;
    end;
    exit;
   end;
  end;
  while i<=length(StringValue) do begin
   case word(widechar(StringValue[i])) of
    ord('0')..ord('9'):begin
     HasDigits:=true;
     inc(i);
    end;
    else begin
     break;
    end;
   end;
  end;
  if (i<=length(StringValue)) and (StringValue[i]='.') then begin
   inc(i);
   while i<=length(StringValue) do begin
    case word(widechar(StringValue[i])) of
     ord('0')..ord('9'):begin
      HasDigits:=true;
      inc(i);
     end;
     else begin
      break;
     end;
    end;
   end;
  end;
  if HasDigits then begin
   OldPosition:=i;
   if (i<=length(StringValue)) and ((StringValue[i]='e') or (StringValue[i]='E')) then begin
    inc(i);
    if (i<=length(StringValue)) and ((StringValue[i]='+') or (StringValue[i]='-')) then begin
     inc(i);
    end;
    HasDigits:=false;
    while i<=length(StringValue) do begin
     case word(widechar(StringValue[i])) of
      ord('0')..ord('9'):begin
       HasDigits:=true;
       inc(i);
      end;
      else begin
       break;
      end;
     end;
    end;
    if IsParseFloat and not HasDigits then begin
     i:=OldPosition;
     HasDigits:=true;
    end;
   end;
   if HasDigits then begin
    EndPosition:=i-1;
    if OnlyNumber then begin
     while (i<=length(StringValue)) and (BESENUnicodeIsStringWhiteSpace(word(widechar(StringValue[i])))) do begin
      inc(i);
     end;
     if i>length(StringValue) then begin
      result:=BESEN_CHECKNUMBERSTRING_VALID;
     end else begin               
      result:=BESEN_CHECKNUMBERSTRING_FAIL;
     end;
    end else begin
     result:=BESEN_CHECKNUMBERSTRING_VALID;
    end;
   end;
  end;
 end;
end;

function BESENStringToNumber(const StringValue:TBESENString;EmptyIsValid:TBESENBoolean=true;AcceptHex:TBESENBoolean=false;OnlyNumber:boolean=false;AcceptHexSign:boolean=true):TBESENNumber;
var StartPosition,EndPosition:integer;
begin
 case BESENCheckNumberString(StringValue,StartPosition,EndPosition,false,AcceptHex,OnlyNumber,AcceptHexSign) of
  BESEN_CHECKNUMBERSTRING_EMPTY:begin
   if EmptyIsValid then begin
    result:=0;
   end else begin
    result:=TBESENNumber(pointer(@BESENDoubleNaN)^);
   end;
  end;
  BESEN_CHECKNUMBERSTRING_VALID:begin
   result:=BESENStringToDouble(copy(StringValue,StartPosition,(EndPosition-StartPosition)+1));
  end;
  BESEN_CHECKNUMBERSTRING_INFNEG:begin
   result:=TBESENNumber(pointer(@BESENDoubleInfNeg)^);
  end;
  BESEN_CHECKNUMBERSTRING_INFPOS:begin
   result:=TBESENNumber(pointer(@BESENDoubleInfPos)^);
  end;
  else begin
   // CHECKNUMBERSTRING_FAIL,CHECKNUMBERSTRING_NAN
   result:=TBESENNumber(pointer(@BESENDoubleNaN)^);
  end;
 end;
end;

function BESENStringToNumberBase(const StringValue:TBESENString;Base:longint):TBESENNumber;
var OK:longbool;
begin
 case Base of
  10:begin
   result:=BESENStringToDouble(StringValue);
  end;
  else begin
   OK:=false;
   result:=BESENStringToDoubleExact(StringValue,BESEN_ROUND_TO_NEAREST,Base,@OK);
   if not OK Then begin
    result:=TBESENNumber(pointer(@BESENDoubleNaN)^);
   end;
  end;
 end;
end;

function BESENSameValue(const A,B:TBESENNumber):boolean;
 function min(a,b:TBESENNumber):TBESENNumber;
 begin
  if a<b then begin
   result:=a;
  end else begin
   result:=b;
  end;
 end;
 function max(a,b:TBESENNumber):TBESENNumber;
 begin
  if a>b then begin
   result:=a;
  end else begin
   result:=b;
  end;
 end;
const FuzzFactor=1000;
      DoubleResolution=1E-15*FuzzFactor;
var Epsilon:TBESENNumber;
begin
 if int64(pointer(@A)^)=int64(pointer(@B)^) then begin
  result:=true;
 end else begin
  Epsilon:=max(min(abs(A),abs(B))*DoubleResolution,DoubleResolution);
  if A>B then begin
   result:=(A-B)<=Epsilon;
  end else begin
   result:=(B-A)<=Epsilon;
  end;
 end;
end;

function BESENSameNumber(const A,B:TBESENNumber):boolean;
 function min(a,b:TBESENNumber):TBESENNumber;
 begin
  if a<b then begin
   result:=a;
  end else begin
   result:=b;
  end;
 end;
 function max(a,b:TBESENNumber):TBESENNumber;
 begin
  if a>b then begin
   result:=a;
  end else begin
   result:=b;
  end;
 end;
const FuzzFactor=1000;
      DoubleResolution=1E-15*FuzzFactor;
var Epsilon:TBESENNumber;
begin
 if int64(pointer(@A)^)=int64(pointer(@B)^) then begin
  result:=true;
 end else if BESENIsNaN(A) and BESENIsNaN(B) then begin
  result:=true;
 end else if (abs(A)=0) and (abs(B)=0) then begin
  result:=(int64(pointer(@A)^) shr 63)=(int64(pointer(@B)^) shr 63);
 end else begin
  Epsilon:=max(min(abs(A),abs(B))*DoubleResolution,DoubleResolution);
  if A>B then begin
   result:=(A-B)<=Epsilon;
  end else begin
   result:=(B-A)<=Epsilon;
  end;
 end;
end;

function BESENFloor(FloatValue:TBESENNumber):TBESENNumber; {$ifdef caninline}inline;{$endif}
begin
 result:=System.int(FloatValue);
 if System.frac(FloatValue)<0 then begin
  result:=result-1;
 end;
end;

function BESENCeil(FloatValue:TBESENNumber):TBESENNumber; {$ifdef caninline}inline;{$endif}
begin
 result:=System.int(FloatValue);
 if System.frac(FloatValue)>0 then begin
  result:=result+1;
 end;
end;

function BESENToInt(v:TBESENNumber):TBESENINT64;
begin
 result:=trunc(v);
end;

function BESENToInt32(v:TBESENNumber):TBESENINT32;
var Sign:longword;
begin
 if BESENIsNaN(v) or BESENIsInfinite(v) or BESENIsZero(v) then begin
  v:=0.0;
 end else begin
  Sign:=PBESENDoubleHiLo(@v)^.Hi and $80000000;
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi and $7fffffff;
  v:=BESENFloor(v);
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi or Sign;
  v:=BESENModulo(System.int(v),4294967296.0);
  if (PBESENDoubleHiLo(@v)^.Hi and $80000000)<>0 then begin
   v:=v+4294967296.0;
  end;
  if v>=2147483648.0 then begin
   v:=v-4294967296.0;
  end;
 end;
 result:=trunc(v);
end;

function BESENToUInt32(v:TBESENNumber):TBESENUINT32;
var Sign:longword;
begin
 if BESENIsNaN(v) or BESENIsInfinite(v) or BESENIsZero(v) then begin
  v:=0.0;
 end else begin
  Sign:=PBESENDoubleHiLo(@v)^.Hi and $80000000;
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi and $7fffffff;
  v:=BESENFloor(v);
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi or Sign;
  v:=BESENModulo(System.int(v),4294967296.0);
  if (PBESENDoubleHiLo(@v)^.Hi and $80000000)<>0 then begin
   v:=v+4294967296.0;
  end;
 end;
 result:=trunc(v);
end;

function BESENToInt16(v:TBESENNumber):TBESENINT32;
var Sign:longword;
begin
 if BESENIsNaN(v) or BESENIsInfinite(v) or BESENIsZero(v) then begin
  v:=0.0;
 end else begin
  Sign:=PBESENDoubleHiLo(@v)^.Hi and $80000000;
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi and $7fffffff;
  v:=BESENFloor(v);
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi or Sign;
  v:=BESENModulo(System.int(v),65536.0);
  if (PBESENDoubleHiLo(@v)^.Hi and $80000000)<>0 then begin
   v:=v+65536.0;
  end;
  if v>=32768.0 then begin
   v:=v-65536.0;
  end;
 end;
 result:=trunc(v);
end;

function BESENToUInt16(v:TBESENNumber):TBESENUINT32;
var Sign:longword;
begin
 if BESENIsNaN(v) or BESENIsInfinite(v) or BESENIsZero(v) then begin
  v:=0.0;
 end else begin
  Sign:=PBESENDoubleHiLo(@v)^.Hi and $80000000;
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi and $7fffffff;
  v:=BESENFloor(v);
  PBESENDoubleHiLo(@v)^.Hi:=PBESENDoubleHiLo(@v)^.Hi or Sign;
  v:=BESENModulo(System.int(v),65536.0);
  if (PBESENDoubleHiLo(@v)^.Hi and $80000000)<>0 then begin
   v:=v+65536.0;
  end;
 end;
 result:=trunc(v);
end;

function BESENToIntFast(v:PBESENNumber):TBESENINT64; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
begin
 result:=trunc(v^);
end;

function BESENToInt32Fast(v:PBESENNumber):TBESENINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
begin
 if not ((v^>=2147483648.0) and (v^<2147483648.0)) then begin
  result:=BESENToInt32(v^);
 end else begin
  result:=trunc(v^);
 end;
end;

function BESENToUInt32Fast(v:PBESENNumber):TBESENUINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
begin
 if not ((v^>=0.0) and (v^<4294967296.0)) then begin
  result:=BESENToUInt32(v^);
 end else begin
  result:=int64(trunc(v^));
 end;
end;

function BESENToInt16Fast(v:PBESENNumber):TBESENINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
begin
 if not ((v^>=-32768.0) and (v^<32768.0)) then begin
  result:=BESENToInt16(v^);
 end else begin
  result:=trunc(v^);
 end;
end;

function BESENToUInt16Fast(v:PBESENNumber):TBESENUINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
begin
 if not ((v^>=0.0) and (v^<65536.0)) then begin
  result:=BESENToUInt16(v^);
 end else begin
  result:=trunc(v^);
 end;
end;

function BESENIntLog2(x:longword):longword; {$ifdef cpu386}assembler; register;
asm
 test eax,eax
 jz @Done
 bsr eax,eax
 @Done:
end;
{$else}
begin
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
 x:=x shr 1;
 x:=x-((x shr 1) and $55555555);
 x:=((x shr 2) and $33333333)+(x and $33333333);
 x:=((x shr 4)+x) and $0f0f0f0f;
 x:=x+(x shr 8);
 x:=x+(x shr 16);
 result:=x and $3f;
end;
{$endif}

{$ifdef cpu64}
function BESENIsNaN(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBESENINT64(@AValue)^ and $7ff0000000000000)=$7ff0000000000000) and ((PBESENINT64(@AValue)^ and $000fffffffffffff)<>$0000000000000000);
end;

function BESENIsInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENINT64(@AValue)^ and $7fffffffffffffff)=$7ff0000000000000;
end;

function BESENIsFinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENINT64(@AValue)^ and $7ff0000000000000)<>$7ff0000000000000;
end;

function BESENIsPosInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PBESENINT64(@AValue)^=int64($7ff0000000000000);
end;

function BESENIsNegInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=qword(pointer(@AValue)^)=qword($fff0000000000000);
{$else}
 result:=PBESENINT64(@AValue)^=int64($fff0000000000000);
{$endif}
end;

function BESENIsPosZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PBESENINT64(@AValue)^=int64($0000000000000000);
end;

function BESENIsNegZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=qword(pointer(@AValue)^)=qword($8000000000000000);
{$else}
 result:=PBESENINT64(@AValue)^=int64($8000000000000000);
{$endif}
end;

function BESENIsZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(qword(pointer(@AValue)^) and qword($7fffffffffffffff))=qword($0000000000000000);
{$else}
 result:=(PBESENINT64(@AValue)^ and int64($7fffffffffffffff))=int64($0000000000000000);
{$endif}
end;

function BESENIsNegative(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(qword(pointer(@AValue)^) and qword($8000000000000000))<>0;
{$else}
 result:=(PBESENINT64(@AValue)^ shr 63)<>0;
{$endif}
end;
{$else}
{$ifdef TrickyNumberChecks}
function BESENIsNaN(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
var l:longword;
begin
 l:=PBESENDoubleHiLo(@AValue)^.Lo;
 result:=(longword($7ff00000-longword(longword(PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff) or ((l or (-l)) shr 31))) shr 31)<>0;
end;

function BESENIsInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=longword((longword(PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff) xor $7ff00000) or PBESENDoubleHiLo(@AValue)^.Lo)=0;
end;

function BESENIsFinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(longword((PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff)-$7ff00000) shr 31)<>0;
end;

function BESENIsPosInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBESENDoubleHiLo(@AValue)^.Hi;
 result:=longword(((longword(h and $7fffffff) xor $7ff00000) or PBESENDoubleHiLo(@AValue)^.Lo) or longword(h shr 31))=0;
end;

function BESENIsNegInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBESENDoubleHiLo(@AValue)^.Hi;
 result:=longword(((longword(h and $7fffffff) xor $7ff00000) or PBESENDoubleHiLo(@AValue)^.Lo) or longword(longword(not h) shr 31))=0;
end;

function BESENIsPosZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBESENDoubleHiLo(@AValue)^.Hi;
 result:=longword(longword(longword(h and $7fffffff) or PBESENDoubleHiLo(@AValue)^.Lo) or longword(h shr 31))=0;
end;

function BESENIsNegZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBESENDoubleHiLo(@AValue)^.Hi;
 result:=longword(longword(longword(h and $7fffffff) or PBESENDoubleHiLo(@AValue)^.Lo) or longword(longword(not h) shr 31))=0;
end;

function BESENIsZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longword(PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff) or PBESENDoubleHiLo(@AValue)^.Lo)=0;
end;

function BESENIsNegative(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(PBESENDoubleHiLo(@AValue)^.Hi and longword($80000000))<>0;
end;
{$else}
function BESENIsNaN(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBESENDoubleHiLo(@AValue)^.Hi and $7ff00000)=$7ff00000) and (((PBESENDoubleHiLo(@AValue)^.Hi and $000fffff) or PBESENDoubleHiLo(@AValue)^.Lo)<>0);
end;

function BESENIsInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff)=$7ff00000) and (PBESENDoubleHiLo(@AValue)^.Lo=0);
end;

function BESENIsFinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENDoubleHiLo(@AValue)^.Hi and $7ff00000)<>$7ff00000;
end;

function BESENIsPosInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENDoubleHiLo(@AValue)^.Hi=$7ff00000) and (PBESENDoubleHiLo(@AValue)^.Lo=0);
end;

function BESENIsNegInfinite(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENDoubleHiLo(@AValue)^.Hi=$fff00000) and (PBESENDoubleHiLo(@AValue)^.Lo=0);
end;

function BESENIsPosZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENDoubleHiLo(@AValue)^.Hi or PBESENDoubleHiLo(@AValue)^.Lo)=0;
end;

function BESENIsNegZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENDoubleHiLo(@AValue)^.Hi=$80000000) and (PBESENDoubleHiLo(@AValue)^.Lo=0);
end;

function BESENIsZero(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff) or PBESENDoubleHiLo(@AValue)^.Lo)=0;
end;

function BESENIsNegative(const AValue:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBESENDoubleHiLo(@AValue)^.Hi and $80000000)<>0;
end;
{$endif}
{$endif}

function BESENNumberAbsolute(const AValue:TBESENNumber):TBESENNumber; {$ifdef caninline}inline;{$endif}
begin
{$ifdef cpu64}
 PBESENINT64(@result)^:=PBESENINT64(@AValue)^ and $7fffffffffffffff;
{$else}
 PBESENDoubleHiLo(@result)^.Hi:=PBESENDoubleHiLo(@AValue)^.Hi and $7fffffff;
 PBESENDoubleHiLo(@result)^.Lo:=PBESENDoubleHiLo(@AValue)^.Lo;
{$endif}
end;

function BESENIsSameValue(const a,b:TBESENNumber):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=qword(pointer(@a)^)=qword(pointer(@b)^);
{$else}
 result:=PBESENINT64(@a)^=PBESENINT64(@b)^;
{$endif}
end;

function BESENModulo(x,y:double):double;{$ifdef cpu386}stdcall; assembler;
asm
 fld qword ptr y
 fld qword ptr x
 @Repeat:
  fprem
  fstsw ax
  sahf
  jp @Repeat
 fstp st(1)
end;
{$else}
begin
 result:=x-(BESENFloor(x/y)*y);
end;
{$endif}

function BESENModuloPos(x,y:double):double;
begin
 result:=BESENModulo(x,y);
 if BESENIsNegative(result) then begin
  result:=result+y;
 end;
end;

const BESENDoubleToStringPowerOfTenTable:array[0..86,0..2] of int64=((int64($fa8fd5a0081c0288),-1220,-348),
                                                                     (int64($baaee17fa23ebf76),-1193,-340),
                                                                     (int64($8b16fb203055ac76),-1166,-332),
                                                                     (int64($cf42894a5dce35ea),-1140,-324),
                                                                     (int64($9a6bb0aa55653b2d),-1113,-316),
                                                                     (int64($e61acf033d1a45df),-1087,-308),
                                                                     (int64($ab70fe17c79ac6ca),-1060,-300),
                                                                     (int64($ff77b1fcbebcdc4f),-1034,-292),
                                                                     (int64($be5691ef416bd60c),-1007,-284),
                                                                     (int64($8dd01fad907ffc3c),-980,-276),
                                                                     (int64($d3515c2831559a83),-954,-268),
                                                                     (int64($9d71ac8fada6c9b5),-927,-260),
                                                                     (int64($ea9c227723ee8bcb),-901,-252),
                                                                     (int64($aecc49914078536d),-874,-244),
                                                                     (int64($823c12795db6ce57),-847,-236),
                                                                     (int64($c21094364dfb5637),-821,-228),
                                                                     (int64($9096ea6f3848984f),-794,-220),
                                                                     (int64($d77485cb25823ac7),-768,-212),
                                                                     (int64($a086cfcd97bf97f4),-741,-204),
                                                                     (int64($ef340a98172aace5),-715,-196),
                                                                     (int64($b23867fb2a35b28e),-688,-188),
                                                                     (int64($84c8d4dfd2c63f3b),-661,-180),
                                                                     (int64($c5dd44271ad3cdba),-635,-172),
                                                                     (int64($936b9fcebb25c996),-608,-164),
                                                                     (int64($dbac6c247d62a584),-582,-156),
                                                                     (int64($a3ab66580d5fdaf6),-555,-148),
                                                                     (int64($f3e2f893dec3f126),-529,-140),
                                                                     (int64($b5b5ada8aaff80b8),-502,-132),
                                                                     (int64($87625f056c7c4a8b),-475,-124),
                                                                     (int64($c9bcff6034c13053),-449,-116),
                                                                     (int64($964e858c91ba2655),-422,-108),
                                                                     (int64($dff9772470297ebd),-396,-100),
                                                                     (int64($a6dfbd9fb8e5b88f),-369,-92),
                                                                     (int64($f8a95fcf88747d94),-343,-84),
                                                                     (int64($b94470938fa89bcf),-316,-76),
                                                                     (int64($8a08f0f8bf0f156b),-289,-68),
                                                                     (int64($cdb02555653131b6),-263,-60),
                                                                     (int64($993fe2c6d07b7fac),-236,-52),
                                                                     (int64($e45c10c42a2b3b06),-210,-44),
                                                                     (int64($aa242499697392d3),-183,-36),
                                                                     (int64($fd87b5f28300ca0e),-157,-28),
                                                                     (int64($bce5086492111aeb),-130,-20),
                                                                     (int64($8cbccc096f5088cc),-103,-12),
                                                                     (int64($d1b71758e219652c),-77,-4),
                                                                     (int64($9c40000000000000),-50,4),
                                                                     (int64($e8d4a51000000000),-24,12),
                                                                     (int64($ad78ebc5ac620000),3,20),
                                                                     (int64($813f3978f8940984),30,28),
                                                                     (int64($c097ce7bc90715b3),56,36),
                                                                     (int64($8f7e32ce7bea5c70),83,44),
                                                                     (int64($d5d238a4abe98068),109,52),
                                                                     (int64($9f4f2726179a2245),136,60),
                                                                     (int64($ed63a231d4c4fb27),162,68),
                                                                     (int64($b0de65388cc8ada8),189,76),
                                                                     (int64($83c7088e1aab65db),216,84),
                                                                     (int64($c45d1df942711d9a),242,92),
                                                                     (int64($924d692ca61be758),269,100),
                                                                     (int64($da01ee641a708dea),295,108),
                                                                     (int64($a26da3999aef774a),322,116),
                                                                     (int64($f209787bb47d6b85),348,124),
                                                                     (int64($b454e4a179dd1877),375,132),
                                                                     (int64($865b86925b9bc5c2),402,140),
                                                                     (int64($c83553c5c8965d3d),428,148),
                                                                     (int64($952ab45cfa97a0b3),455,156),
                                                                     (int64($de469fbd99a05fe3),481,164),
                                                                     (int64($a59bc234db398c25),508,172),
                                                                     (int64($f6c69a72a3989f5c),534,180),
                                                                     (int64($b7dcbf5354e9bece),561,188),
                                                                     (int64($88fcf317f22241e2),588,196),
                                                                     (int64($cc20ce9bd35c78a5),614,204),
                                                                     (int64($98165af37b2153df),641,212),
                                                                     (int64($e2a0b5dc971f303a),667,220),
                                                                     (int64($a8d9d1535ce3b396),694,228),
                                                                     (int64($fb9b7cd9a4a7443c),720,236),
                                                                     (int64($bb764c4ca7a44410),747,244),
                                                                     (int64($8bab8eefb6409c1a),774,252),
                                                                     (int64($d01fef10a657842c),800,260),
                                                                     (int64($9b10a4e5e9913129),827,268),
                                                                     (int64($e7109bfba19c0c9d),853,276),
                                                                     (int64($ac2820d9623bf429),880,284),
                                                                     (int64($80444b5e7aa7cf85),907,292),
                                                                     (int64($bf21e44003acdd2d),933,300),
                                                                     (int64($8e679c2f5e44ff8f),960,308),
                                                                     (int64($d433179d9c8cb841),986,316),
                                                                     (int64($9e19db92b4e31ba9),1013,324),
                                                                     (int64($eb96bf6ebadf77d9),1039,332),
                                                                     (int64($af87023b9bf0ee6b),1066,340));

      BESENDoubleToStringPowerOfTenBinaryExponentTable:array[-1220..(1066+27)-1] of byte=(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                                                          1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,
                                                                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                                                          2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,
                                                                                          3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                                                          3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                                                                          4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,
                                                                                          5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                                                                          5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,
                                                                                          6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                                                          6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                                                                          7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,
                                                                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                                                                          8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,
                                                                                          9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
                                                                                          9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                                                                                          10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,
                                                                                          11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
                                                                                          11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,
                                                                                          12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                                                                          13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
                                                                                          13,13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,
                                                                                          14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
                                                                                          14,14,14,14,14,14,15,15,15,15,15,15,15,15,15,15,
                                                                                          15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
                                                                                          16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                                                                          16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,
                                                                                          17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
                                                                                          17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,
                                                                                          18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
                                                                                          19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,
                                                                                          19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,
                                                                                          20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                                                                                          20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,
                                                                                          21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                                                                          22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,
                                                                                          22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,
                                                                                          23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
                                                                                          23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,
                                                                                          24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,
                                                                                          25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,
                                                                                          25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,
                                                                                          26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,
                                                                                          26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27,
                                                                                          27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,
                                                                                          28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,
                                                                                          28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,
                                                                                          29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
                                                                                          29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,
                                                                                          30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,31,
                                                                                          31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,
                                                                                          31,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                          32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
                                                                                          32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,33,
                                                                                          33,33,33,33,33,33,33,33,33,33,33,33,33,33,34,34,
                                                                                          34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
                                                                                          34,34,34,34,34,34,34,34,34,35,35,35,35,35,35,35,
                                                                                          35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,
                                                                                          35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,
                                                                                          36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,37,
                                                                                          37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
                                                                                          37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                          38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,
                                                                                          38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,
                                                                                          39,39,39,39,39,39,39,39,39,39,39,39,39,39,40,40,
                                                                                          40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
                                                                                          40,40,40,40,40,40,40,40,41,41,41,41,41,41,41,41,
                                                                                          41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,
                                                                                          41,41,41,42,42,42,42,42,42,42,42,42,42,42,42,42,
                                                                                          42,42,42,42,42,42,42,42,42,42,42,42,42,42,43,43,
                                                                                          43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,
                                                                                          43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,
                                                                                          44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,
                                                                                          44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,45,
                                                                                          45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,
                                                                                          46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
                                                                                          46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,
                                                                                          47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,
                                                                                          47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,
                                                                                          48,48,48,48,48,48,48,48,48,48,48,48,48,49,49,49,
                                                                                          49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,
                                                                                          49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,
                                                                                          50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,
                                                                                          50,50,51,51,51,51,51,51,51,51,51,51,51,51,51,51,
                                                                                          51,51,51,51,51,51,51,51,51,51,51,51,51,52,52,52,
                                                                                          52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,
                                                                                          52,52,52,52,52,52,52,53,53,53,53,53,53,53,53,53,
                                                                                          53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,
                                                                                          53,53,54,54,54,54,54,54,54,54,54,54,54,54,54,54,
                                                                                          54,54,54,54,54,54,54,54,54,54,54,54,54,55,55,55,
                                                                                          55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,
                                                                                          55,55,55,55,55,55,55,56,56,56,56,56,56,56,56,56,
                                                                                          56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
                                                                                          56,56,57,57,57,57,57,57,57,57,57,57,57,57,57,57,
                                                                                          57,57,57,57,57,57,57,57,57,57,57,57,58,58,58,58,
                                                                                          58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,
                                                                                          58,58,58,58,58,58,58,59,59,59,59,59,59,59,59,59,
                                                                                          59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,
                                                                                          59,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,
                                                                                          60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,
                                                                                          61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,
                                                                                          61,61,61,61,61,61,61,62,62,62,62,62,62,62,62,62,
                                                                                          62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,
                                                                                          62,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,
                                                                                          63,63,63,63,63,63,63,63,63,63,63,63,64,64,64,64,
                                                                                          64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,
                                                                                          64,64,64,64,64,64,65,65,65,65,65,65,65,65,65,65,
                                                                                          65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,
                                                                                          65,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,
                                                                                          66,66,66,66,66,66,66,66,66,66,66,67,67,67,67,67,
                                                                                          67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,
                                                                                          67,67,67,67,67,67,68,68,68,68,68,68,68,68,68,68,
                                                                                          68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,
                                                                                          68,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                                                                                          69,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,
                                                                                          70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,
                                                                                          70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,
                                                                                          71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,
                                                                                          72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,
                                                                                          72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,
                                                                                          73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,
                                                                                          73,73,73,73,73,74,74,74,74,74,74,74,74,74,74,74,
                                                                                          74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,
                                                                                          75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,
                                                                                          75,75,75,75,75,75,75,75,75,75,75,76,76,76,76,76,
                                                                                          76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,
                                                                                          76,76,76,76,76,77,77,77,77,77,77,77,77,77,77,77,
                                                                                          77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,
                                                                                          78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,
                                                                                          78,78,78,78,78,78,78,78,78,78,79,79,79,79,79,79,
                                                                                          79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,
                                                                                          79,79,79,79,79,80,80,80,80,80,80,80,80,80,80,80,
                                                                                          80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,
                                                                                          81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,
                                                                                          81,81,81,81,81,81,81,81,81,81,82,82,82,82,82,82,
                                                                                          82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,
                                                                                          82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,
                                                                                          83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,84,
                                                                                          84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,
                                                                                          84,84,84,84,84,84,84,84,84,84,85,85,85,85,85,85,
                                                                                          85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,
                                                                                          85,85,85,85,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                          86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                          86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                          86,86,86,86,86,86,86,86,86);

      BESENDoubleToStringPowerOfTenDecimalExponentTable:array[-348..(340+8)-1] of byte=(0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,
                                                                                        2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,
                                                                                        4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,
                                                                                        6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,
                                                                                        8,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,
                                                                                        10,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,
                                                                                        12,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,
                                                                                        14,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,
                                                                                        16,17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,
                                                                                        18,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,
                                                                                        20,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,
                                                                                        22,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,
                                                                                        24,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,
                                                                                        26,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,
                                                                                        28,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,
                                                                                        30,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                        32,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,
                                                                                        34,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,
                                                                                        36,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                        38,39,39,39,39,39,39,39,39,40,40,40,40,40,40,40,
                                                                                        40,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,
                                                                                        42,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,
                                                                                        44,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,
                                                                                        46,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,
                                                                                        48,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,
                                                                                        50,51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,
                                                                                        52,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,
                                                                                        54,55,55,55,55,55,55,55,55,56,56,56,56,56,56,56,
                                                                                        56,57,57,57,57,57,57,57,57,58,58,58,58,58,58,58,
                                                                                        58,59,59,59,59,59,59,59,59,60,60,60,60,60,60,60,
                                                                                        60,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,
                                                                                        62,63,63,63,63,63,63,63,63,64,64,64,64,64,64,64,
                                                                                        64,65,65,65,65,65,65,65,65,66,66,66,66,66,66,66,
                                                                                        66,67,67,67,67,67,67,67,67,68,68,68,68,68,68,68,
                                                                                        68,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,
                                                                                        70,71,71,71,71,71,71,71,71,72,72,72,72,72,72,72,
                                                                                        72,73,73,73,73,73,73,73,73,74,74,74,74,74,74,74,
                                                                                        74,75,75,75,75,75,75,75,75,76,76,76,76,76,76,76,
                                                                                        76,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,
                                                                                        78,79,79,79,79,79,79,79,79,80,80,80,80,80,80,80,
                                                                                        80,81,81,81,81,81,81,81,81,82,82,82,82,82,82,82,
                                                                                        82,83,83,83,83,83,83,83,83,84,84,84,84,84,84,84,
                                                                                        84,85,85,85,85,85,85,85,85,86,86,86,86,86,86,86,
                                                                                        86,86,86,86,86,86,86,86);

      BESENDoubleToStringEstimatePowerFactorTable:array[2..36] of int64=(4294967296, // round((ln(2)/ln(Radix))*4294967296.0);
                                                                         2709822658,
                                                                         2147483648,
                                                                         1849741732,
                                                                         1661520155,
                                                                         1529898219,
                                                                         1431655765,
                                                                         1354911329,
                                                                         1292913986,
                                                                         1241523975,
                                                                         1198050829,
                                                                         1160664035,
                                                                         1128071163,
                                                                         1099331346,
                                                                         1073741824,
                                                                         1050766077,
                                                                         1029986701,
                                                                         1011073584,
                                                                         993761859,
                                                                         977836272,
                                                                         963119891,
                                                                         949465783,
                                                                         936750801,
                                                                         924870866,
                                                                         913737342,
                                                                         903274219,
                                                                         893415894,
                                                                         884105413,
                                                                         875293062,
                                                                         866935226,
                                                                         858993459,
                                                                         851433729,
                                                                         844225782,
                                                                         837342623,
                                                                         830760078);

function BESENDoubleToString(const AValue:double;Mode,RequestedDigits:longint):{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};
{$ifndef fpc}
type qword=int64;
{$endif}
const SignificantMantissaSize=64;
      MinimalTargetExponent=-60;
      MaximalTargetExponent=-32;
      mSHORTEST=0;
      mFIXED=1;
      mPRECISION=2;
      BigNumMaxSignificantMantissaBits=3584;
      BigitChunkSize=32;
      BigitDoubleChunkSize=64;
      BigitSize=28;
      BigitMask=(1 shl BigitSize)-1;
      BigNumCapacity=(BigNumMaxSignificantMantissaBits+(BigitSize-1)) div BigitSize;
{$ifdef BESENSingleStringType}
      DigitChars:array[0..9] of char=('0','1','2','3','4','5','6','7','8','9');
{$endif}
type TDoubleValue=record
      SignificantMantissa:qword;
      Exponent:longint;
     end;
     TBigNumChunk=longword;
     TBigNumDoubleChunk=qword;
     TBigNum=record
      Bigits:array[0..BigNumCapacity] of TBigNumChunk;
      UsedDigits:longint;
      Exponent:longint;
     end;
 function QWordLessOrEqual(a,b:qword):boolean;
 begin
  result:=(a=b) or (((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff))));
 end;
 function QWordGreaterOrEqual(a,b:qword):boolean;
 begin
  result:=(a=b) or (((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff))));
 end;
 function QWordLess(a,b:qword):boolean;
 begin
  result:=((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff)));
 end;
 function QWordGreater(a,b:qword):boolean;
 begin
  result:=((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff)));
 end;
 function DoubleValue(SignificantMantissa:qword=0;Exponent:longint=0):TDoubleValue;
 begin
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure SplitDouble(Value:double;var SignificantMantissa:qword;var Exponent:longint);
 var Casted:qword absolute Value;
 begin
  SignificantMantissa:=Casted and qword($000fffffffffffff);
  if (Casted and qword($7ff0000000000000))<>0 then begin
   inc(SignificantMantissa,qword($0010000000000000));
   Exponent:=((Casted and qword($7ff0000000000000)) shr 52)-($3ff+52);
  end else begin
   Exponent:=(-($3ff+52))+1;
  end;
 end;
 function DoubleValueGet(Value:double):TDoubleValue;
 var SignificantMantissa:qword;
     Exponent:longint;
 begin
  Assert(Value>0);
  SplitDouble(Value,SignificantMantissa,Exponent);
  while (SignificantMantissa and qword($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  SignificantMantissa:=SignificantMantissa shl (SignificantMantissaSize-53);
  dec(Exponent,SignificantMantissaSize-53);
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure DoubleValueSubtract(var Left:TDoubleValue;const Right:TDoubleValue);
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  dec(Left.SignificantMantissa,Right.SignificantMantissa);
 end;
 function DoubleValueMinus(const Left,Right:TDoubleValue):TDoubleValue;
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  result.Exponent:=Left.Exponent;
  result.SignificantMantissa:=Left.SignificantMantissa-Right.SignificantMantissa;
 end;
 procedure DoubleValueMuliply(var Left:TDoubleValue;const Right:TDoubleValue);
 var a,b,c,d,ac,bc,ad,bd:qword;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  inc(Left.Exponent,Right.Exponent+64);
  Left.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(qword(((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(qword(1) shl 31)) shr 32);
 end;
 function DoubleValueMul(const Left,Right:TDoubleValue):TDoubleValue;
 var a,b,c,d,ac,bc,ad,bd:qword;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  result.Exponent:=Left.Exponent+(Right.Exponent+64);
  a:=((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(qword(1) shl 31);
  result.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(a shr 32);
 end;
 procedure DoubleValueNormalize(var Value:TDoubleValue);
 var SignificantMantissa:qword;
     Exponent:longint;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and qword($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and qword($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  Value.SignificantMantissa:=SignificantMantissa;
  Value.Exponent:=Exponent;
 end;
 function DoubleValueNorm(const Value:TDoubleValue):TDoubleValue;
 var SignificantMantissa:qword;
     Exponent:longint;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and qword($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and qword($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 function BigNumNew:TBigNum;
 begin
  FillChar(result,sizeof(TBigNum),#0);
 end;
 procedure BigNumZero(var BigNum:TBigNum);
 begin
  BigNum.UsedDigits:=0;
  BigNum.Exponent:=0;
 end;
 procedure BigNumEnsureCapacity(var BigNum:TBigNum;Size:longint);
 begin
 end;
 procedure BigNumClamp(var BigNum:TBigNum);
 begin
  while (BigNum.UsedDigits>0) and (BigNum.Bigits[BigNum.UsedDigits-1]=0) do begin
   dec(BigNum.UsedDigits);
  end;
  if BigNum.UsedDigits=0 then begin
   BigNum.Exponent:=0;
  end;
 end;
 function BigNumIsClamped(const BigNum:TBigNum):boolean;
 begin
  result:=(BigNum.UsedDigits=0) or (BigNum.Bigits[BigNum.UsedDigits-1]<>0);
 end;
 procedure BigNumAlign(var BigNum:TBigNum;const Other:TBigNum);
 var ZeroDigits,i:longint;
 begin
  if BigNum.Exponent>Other.Exponent then begin
   ZeroDigits:=BigNum.Exponent-Other.Exponent;
   BigNumEnsureCapacity(BigNum,Bignum.UsedDigits+ZeroDigits);
   for i:=BigNum.UsedDigits-1 downto 0 do begin
    BigNum.Bigits[i+ZeroDigits]:=BigNum.Bigits[i];
   end;
   for i:=0 to ZeroDigits-1 do begin
    BigNum.Bigits[i]:=0;
   end;
   inc(BigNum.UsedDigits,ZeroDigits);
   dec(BigNum.Exponent,ZeroDigits);
   Assert(BigNum.UsedDigits>=0);
   Assert(BigNum.Exponent>=0);
  end;
 end;
 procedure BigNumAssignUInt16(var BigNum:TBigNum;Value:word);
 begin
  Assert(BigitSize>=(sizeof(word)*8));
  BigNumZero(BigNum);
  if Value<>0 then begin
   BigNumEnsureCapacity(BigNum,1);
   BigNum.Bigits[0]:=Value;
   BigNum.UsedDigits:=1;
  end;
 end;
 procedure BigNumAssignUInt64(var BigNum:TBigNum;Value:qword);
 var i,j:longint;
 begin
  BigNumZero(BigNum);
  if Value<>0 then begin
   j:=(64 div BigitSize)+1;
   BigNumEnsureCapacity(BigNum,j);
   for i:=0 to j-1 do begin
    BigNum.Bigits[i]:=Value and BigitMask;
    Value:=Value shr BigitSize;
   end;
   BigNum.UsedDigits:=j;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumAssignBigNum(var BigNum:TBigNum;const Other:TBigNum);
 begin
  BigNum.Exponent:=Other.Exponent;
  BigNum.Bigits:=Other.Bigits;
  BigNum.UsedDigits:=Other.UsedDigits;
 end;
 procedure BigNumAddBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Carry,Sum:TBigNumChunk;
     BigitPos,i:longint;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  BigNumAlign(BigNum,Other);
  BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+Other.UsedDigits);
  BigitPos:=Other.Exponent-BigNum.Exponent;
  Assert(BigitPos>=0);
  Carry:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Sum:=BigNum.Bigits[BigitPos]+Other.Bigits[i]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  while Carry<>0 do begin
   Sum:=BigNum.Bigits[BigitPos]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  if BigNum.UsedDigits<BigitPos then begin
   BigNum.UsedDigits:=BigitPos;
  end;
  Assert(BigNumIsClamped(BigNum));
 end;
 procedure BigNumAddUInt64(var BigNum:TBigNum;const Value:qword);
 var Other:TBigNum;
 begin
  Other:=BigNumNew;
  BigNumAssignUInt64(Other,Value);
  BigNumAddBigNum(BigNum,Other);
 end;
 function BigNumBigitAt(const BigNum:TBigNum;Index:longint):TBigNumChunk;
 begin
  if (Index<BigNum.Exponent) or (Index>=(BigNum.UsedDigits+BigNum.Exponent)) then begin
   result:=0;
  end else begin
   result:=BigNum.Bigits[Index-BigNum.Exponent];
  end;
 end;
 function BigNumCompare(const a,b:TBigNum):longint;
 var la,lb,i,j:longint;
     ba,bb:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  if la<lb then begin
   result:=-1;
  end else if la>lb then begin
   result:=1;
  end else begin
   if a.Exponent<b.Exponent then begin
    j:=a.Exponent;
   end else begin
    j:=b.Exponent;
   end;
   result:=0;
   for i:=la-1 downto j do begin
    ba:=BigNumBigItAt(a,i);
    bb:=BigNumBigItAt(b,i);
    if ba<bb then begin
     result:=-1;
     break;
    end else if ba>bb then begin
     result:=1;
     break;
    end;
   end;
  end;
 end;
 function BigNumPlusCompare(const a,b,c:TBigNum):longint;
 var la,lb,lc,i,j:longint;
     ba,bb,bc,br,Sum:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  Assert(BigNumIsClamped(c));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  lc:=c.UsedDigits+c.Exponent;
  if la<lb then begin
   result:=BigNumPlusCompare(b,a,c);
  end else begin
   if (la+1)<lc then begin
    result:=-1;
   end else if la>lc then begin
    result:=1;
   end else if (a.Exponent>=lb) and (la<lc) then begin
    result:=-1;
   end else begin
    if a.Exponent<b.Exponent then begin
     if a.Exponent<c.Exponent then begin
      j:=a.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end else begin
     if b.Exponent<c.Exponent then begin
      j:=b.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end;
    br:=0;
    for i:=lc-1 downto j do begin
     ba:=BigNumBigItAt(a,i);
     bb:=BigNumBigItAt(b,i);
     bc:=BigNumBigItAt(c,i);
     Sum:=ba+bb;
     if Sum>(bc+br) then begin
      result:=1;
      exit;
     end else begin
      br:=(bc+br)-Sum;
      if br>1 then begin
       result:=-1;
       exit;
      end;
      br:=br shl BigitSize;
     end;
    end;
    if br=0 then begin
     result:=0;
    end else begin
     result:=-1;
    end;
   end;
  end;
 end;
 procedure BigNumSubtractBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Borrow,Difference:TBigNumChunk;
     i,Offset:longint;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(BigNumCompare(Other,BigNum)<=0);
  BigNumAlign(BigNum,Other);
  Offset:=Other.Exponent-BigNum.Exponent;
  Borrow:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Assert((Borrow=0) or (Borrow=1));
   Difference:=(BigNum.Bigits[i+Offset]-Other.Bigits[i])-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
  end;
  i:=Other.UsedDigits;
  while Borrow<>0 do begin
   Difference:=BigNum.Bigits[i+Offset]-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
   inc(i);
  end;
  BigNumClamp(BigNum);
 end;
 procedure BigNumBigitsShiftLeft(var BigNum:TBigNum;Shift:longint);
 var Carry,NextCarry:TBigNumChunk;
     i:longint;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  Carry:=0;
  for i:=0 to BigNum.UsedDigits-1 do begin
   NextCarry:=BigNum.Bigits[i] shr (BigitSize-Shift);
   BigNum.Bigits[i]:=((BigNum.Bigits[i] shl Shift)+Carry) and BigitMask;
   Carry:=NextCarry;
  end;
  if Carry<>0 then begin
   BigNum.Bigits[BigNum.UsedDigits]:=Carry;
   inc(BigNum.UsedDigits);
  end;
 end;
 procedure BigNumBigitsShiftRight(var BigNum:TBigNum;Shift:longint);
 var Carry,NextCarry:TBigNumChunk;
     i:longint;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  if BigNum.UsedDigits>0 then begin
   Carry:=0;
   for i:=BigNum.UsedDigits-1 downto 1 do begin
    NextCarry:=BigNum.Bigits[i] shl (BigitSize-Shift);
    BigNum.Bigits[i]:=((BigNum.Bigits[i] shr Shift)+Carry) and BigitMask;
    Carry:=NextCarry;
   end;
   BigNum.Bigits[0]:=(BigNum.Bigits[0] shr Shift)+Carry;
  end;
  BigNumClamp(BigNum);
 end;
 procedure BignumSubtractTimes(var BigNum:TBigNum;const Other:TBigNum;Factor:longint);
 var i,ExponentDiff:longint;
     Borrow,Difference:TBigNumChunk;
     Product,Remove:TBigNumDoubleChunk;
 begin
  Assert(BigNum.Exponent<=Other.Exponent);
  if Factor<3 then begin
   for i:=1 to Factor do begin
    BigNumSubtractBignum(BigNum,Other);
   end;
  end else begin
   Borrow:=0;
   ExponentDiff:=Other.Exponent-BigNum.Exponent;
   for i:=0 to Other.UsedDigits-1 do begin
    Product:=TBigNumDoubleChunk(Factor)*Other.Bigits[i];
    Remove:=Borrow+Product;
    Difference:=BigNum.Bigits[i+ExponentDiff]-TBigNumChunk(Remove and BigitMask);
    BigNum.Bigits[i+ExponentDiff]:=Difference and BigitMask;
    Borrow:=TBigNumChunk((Difference shr (BigitChunkSize-1))+(Remove shr BigitSize));
   end;
   for i:=Other.UsedDigits+ExponentDiff to BigNum.UsedDigits-1 do begin
    if Borrow=0 then begin
     exit;
    end;
    Difference:=BigNum.Bigits[i]-Borrow;
    BigNum.Bigits[i]:=Difference and BigitMask;
    Borrow:=TBigNumChunk(Difference shr (BigitChunkSize-1));
   end;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumShiftLeft(var BigNum:TBigNum;Shift:longint);
 begin
  if BigNum.UsedDigits<>0 then begin
   inc(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
   BigNumBigitsShiftLeft(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumShiftRight(var BigNum:TBigNum;Shift:longint);
 begin
  if BigNum.UsedDigits<>0 then begin
   dec(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits);
   BigNumBigitsShiftRight(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumMultiplyByUInt32(var BigNum:TBigNum;Factor:word);
 var Carry,Product:qword;
     i:longint;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   for i:=0 to BigNum.UsedDigits-1 do begin
    Product:=(Factor*BigNum.Bigits[i])+Carry;
    BigNum.Bigits[i]:=Product and BigitMask;
    Carry:=Product shr BigitSize;
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumMultiplyByUInt64(var BigNum:TBigNum;Factor:qword);
 var Carry,Low,High,ProductLow,ProductHigh,Tmp:qword;
     i:longint;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   Low:=Factor and $ffffffff;
   High:=Factor shr 32;
   for i:=0 to BigNum.UsedDigits-1 do begin
    ProductLow:=Low*BigNum.Bigits[i];
    ProductHigh:=High*BigNum.Bigits[i];
    Tmp:=(Carry and BigitMask)+ProductLow;
    BigNum.Bigits[i]:=Tmp and BigitMask;
    Carry:=(Carry shr BigitSize)+(Tmp shr BigitSize)+(ProductHigh shl (32-BigitSize));
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumSquare(var BigNum:TBigNum);
 var ProductLength,CopyOffset,i,BigitIndex1,BigitIndex2:longint;
     Accumulator:TBigNumDoubleChunk;
     Chunk1,Chunk2:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(BigNum));
  ProductLength:=2*BigNum.UsedDigits;
  BigNumEnsureCapacity(BigNum,ProductLength);
  Assert(not ((1 shl (2*(BigItChunkSize-BigitSize)))<=BigNum.UsedDigits));
  Accumulator:=0;
  CopyOffset:=BigNum.UsedDigits;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigNum.Bigits[i+CopyOffset]:=BigNum.Bigits[i];
  end;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigitIndex1:=i;
   BigitIndex2:=0;
   while BigitIndex1>=0 do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  for i:=BigNum.UsedDigits-1 to ProductLength-1 do begin
   BigitIndex1:=BigNum.UsedDigits-1;
   BigitIndex2:=i-BigitIndex1;
   while BigitIndex2<BigNum.UsedDigits do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  Assert(Accumulator=0);
  BigNum.UsedDigits:=ProductLength;
  inc(BigNum.Exponent,BigNum.Exponent);
  BigNumClamp(BigNum);
 end;
 procedure BigNumAssignPowerUInt16(var BigNum:TBigNum;Base:word;PowerExponent:longint);
 var Shifts,BitSize,TmpBase,FinalSize,Mask:longint;
     ThisValue:qword;
     DelayedMultipliciation:boolean;
 begin
  Assert(Base<>0);
  Assert(PowerExponent>=0);
  if PowerExponent=0 then begin
   BigNumAssignUInt16(BigNum,1);
  end else begin
   BigNumZero(BigNum);
   Shifts:=0;
   while (Base and 1)=0 do begin
    Base:=Base shr 1;
    inc(Shifts);
   end;
   BitSize:=0;
   TmpBase:=Base;
   while TmpBase<>0 do begin
    TmpBase:=TmpBase shr 1;
    inc(BitSize);
   end;
   FinalSize:=BitSize*PowerExponent;
   BigNumEnsureCapacity(BigNum,FinalSize);
   Mask:=1;
   while Mask<=PowerExponent do begin
    inc(Mask,Mask);
   end;
   Mask:=Mask shr 2;
   ThisValue:=Base;
   DelayedMultipliciation:=false;
   while (Mask<>0) and (ThisValue<=$ffffffff) do begin
    ThisValue:=ThisValue*ThisValue;
    if (PowerExponent and Mask)<>0 then begin
     if (ThisValue and not ((qword(1) shl (64-BitSize))-1))=0 then begin
      ThisValue:=ThisValue*Base;
     end else begin
      DelayedMultipliciation:=true;
     end;
    end;
    Mask:=Mask shr 1;
   end;
   BigNumAssignUInt64(BigNum,ThisValue);
   if DelayedMultipliciation then begin
    BigNumMultiplyByUInt32(BigNum,Base);
   end;
   while Mask<>0 do begin
    BigNumSquare(BigNum);
    if (PowerExponent and Mask)<>0 then begin
     BigNumMultiplyByUInt32(BigNum,Base);
    end;
    Mask:=Mask shr 1;
   end;
   BigNumShiftLeft(BigNum,Shifts*PowerExponent);
  end;
 end;
 function BigNumDivideModuloIntBigNum(var BigNum:TBigNum;const Other:TBigNum):word;
 var ThisBigit,OtherBigit:TBigNumChunk;
     Quotient,DivisionEstimate:longword;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(Other.UsedDigits>0);
  result:=0;
  if (BigNum.UsedDigits+BigNum.Exponent)>=(Other.UsedDigits+Other.Exponent) then begin
   BigNumAlign(BigNum,Other);
   while (BigNum.UsedDigits+BigNum.Exponent)>(Other.UsedDigits+Other.Exponent) do begin
    Assert(Other.Bigits[Other.UsedDigits-1]>=((1 shl BigitSize) div 16));
    inc(result,BigNum.Bigits[BigNum.UsedDigits-1]);
    BigNumSubtractTimes(BigNum,Other,BigNum.Bigits[BigNum.UsedDigits-1]);
   end;
   Assert((BigNum.UsedDigits+BigNum.Exponent)=(Other.UsedDigits+Other.Exponent));
   ThisBigit:=BigNum.Bigits[BigNum.UsedDigits-1];
   OtherBigit:=Other.Bigits[Other.UsedDigits-1];
   if Other.UsedDigits=1 then begin
    Quotient:=ThisBigit div OtherBigit;
    BigNum.Bigits[BigNum.UsedDigits-1]:=ThisBigit-(OtherBigit*Quotient);
    inc(result,Quotient);
    BigNumClamp(BigNum);
   end else begin
    DivisionEstimate:=ThisBigit div (OtherBigit+1);
    inc(result,DivisionEstimate);
    BigNumSubtractTimes(BigNum,Other,DivisionEstimate);
    if (OtherBigit*(DivisionEstimate+1))<=ThisBigit then begin
     while BigNumCompare(Other,BigNum)<=0 do begin
      BigNumSubtractBigNum(BigNum,Other);
      inc(result);
     end;
    end;
   end;
  end;
 end;
 function BigNumDivideModuloInt(var BigNum:TBigNum;Divisor:word):word;
 var q0,r0,q1,r1:qword;
     i:integer;
 begin
  Assert(BigNumIsClamped(BigNum));
  q0:=0;
  for i:=BigNum.UsedDigits-1 downto 1 do begin
   q1:=(BigNum.Bigits[i] div Divisor)+q0;
   r1:=((BigNum.Bigits[i] mod Divisor) shl 16)+(BigNum.Bigits[i-1] shr 16);
   q0:=((r1 div Divisor) shl 16);
   r0:=r1 mod Divisor;
   BigNum.Bigits[i]:=q1;
   BigNum.Bigits[i-1]:=(r0 shl 16)+(BigNum.Bigits[i-1] and $ffff);
  end;
  q1:=(BigNum.Bigits[0] div Divisor)+q0;
  r1:=BigNum.Bigits[0] mod Divisor;
  BigNum.Bigits[0]:=q1;
  result:=r1;
  BigNumClamp(BigNum);
 end;
 function NormalizedExponent(SignificantMantissa:qword;Exponent:longint):longint;
 begin
  Assert(SignificantMantissa<>0);
  while (SignificantMantissa and qword($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result:=Exponent;
 end;
 function GetEstimatePower(Exponent:longint):longint;
 begin
  result:=longint(int64(((Exponent+52)*int64(1292913986))-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*0.30102999566398114)-(1e-10)));
 end;
 function GetEstimatePowerOf(Exponent,Radix:longint):longint;
 begin
  result:=longint(int64(((Exponent+52)*BESENDoubleToStringEstimatePowerFactorTable[Radix])-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*(ln(2)/ln(Radix)))-(1e-10)));
 end;
 procedure GenerateShortestDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:boolean;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
 var Digit,Compare:longint;
     InDeltaRoomMinus,InDeltaRoomPlus:boolean;
 begin
  Len:=0;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,10);
    BigNumMultiplyByUInt32(DeltaMinus,10);
    BigNumMultiplyByUInt32(DeltaPlus,10);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin
    end else if Compare>0 then begin
     Assert(Buffer[Len]<>'9');
     inc(Buffer[Len]);
    end else begin
     if ((ord(Buffer[Len])-ord('0')) and 1)<>0 then begin
      Assert(Buffer[Len]<>'9');
      inc(Buffer[Len]);
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(Buffer[Len]<>'9');
    inc(Buffer[Len]);
    exit;
   end;
  end;
 end;
 procedure GenerateCountedDigits(Count:longint;var DecimalPoint:longint;var Numerator,Denominator:TBigNum;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
 var i,Digit:longint;
 begin
  Assert(Count>=0);
  for i:=1 to Count-1 do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
   BigNumMultiplyByUInt32(Numerator,10);
  end;
  Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
  if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
   inc(Digit);
  end;
  inc(Len);
  if Len>=length(Buffer) then begin
   SetLength(Buffer,Len*2);
  end;
  Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
  for i:=Len downto 2 do begin
   if ord(Buffer[i])<>(ord('0')+10) then begin
    break;
   end;
   Buffer[i]:='0';
   inc(Buffer[i-1]);
  end;
  if ord(Buffer[1])=(ord('0')+10) then begin
   Buffer[1]:='1';
   inc(DecimalPoint);
  end;
 end;
 procedure GenerateFixedDigits(RequestedDigits:longint;var DecimalPoint:longint;var Numerator,Denominator:TBigNum;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
 begin
  if (-DecimalPoint)>RequestedDigits then begin
   DecimalPoint:=-RequestedDigits;
   Len:=0;
  end else if (-DecimalPoint)=RequestedDigits then begin
   Assert(DecimalPoint=(-RequestedDigits));
   BigNumMultiplyByUInt32(Denominator,10);
   if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
    Buffer:='1';
    Len:=1;
  end else begin
    Len:=0;
   end;
  end else begin
   GenerateCountedDigits(DecimalPoint+RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
  end;
 end;
 procedure FixupMultiplyBase(EstimatedPower:longint;IsEven:boolean;var DecimalPoint:longint;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 var InRange:boolean;
 begin
  if IsEven then begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
  end else begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
  end;
  if InRange then begin
   DecimalPoint:=EstimatedPower+1;
  end else begin
   DecimalPoint:=EstimatedPower;
   BigNumMultiplyByUInt32(Numerator,Base);
   if BigNumCompare(DeltaMinus,DeltaPlus)=0 then begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumAssignBigNum(DeltaPlus,DeltaMinus);
   end else begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumMultiplyByUInt32(DeltaPlus,Base);
   end;
  end;
 end;
 procedure InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  Assert(EstimatedPower>=0);

  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumShiftLeft(Numerator,Exponent);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumShiftLeft(DeltaPlus,Exponent);

   BigNumAssignUInt16(DeltaMinus,1);
   BigNumShiftLeft(DeltaMinus,Exponent);

   if (Casted and qword($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumAssignUInt16(DeltaMinus,1);

   if (Casted and qword($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  BigNumAssignPowerUInt16(Numerator,Base,-EstimatedPower);
  if NeedBoundaryDeltas then begin
   BigNumAssignBigNum(DeltaPlus,Numerator);
   BigNumAssignBigNum(DeltaMinus,Numerator);
  end;
  BigNumMultiplyByUInt64(Numerator,SignificantMantissa);

  BigNumAssignUInt16(Denominator,1);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);
   if ((Casted and qword($000fffffffffffff))=0) and ((Casted and qword($7ff0000000000000))<>qword($0010000000000000)) then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValues(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  if Exponent>=0 then begin
   InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else if EstimatedPower>=0 then begin
   InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else begin
   InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end;
 end;
 procedure DoubleToDecimal(Value:double;Mode,RequestedDigits:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint);
 var Casted:qword absolute Value;
     SignificantMantissa:qword;
     Exponent,EstimatedPower:longint;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:boolean;
 begin
  Assert(Value>0);
  Assert(BESENIsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePower(NormalizedExponent(SignificantMantissa,Exponent));
  if (Mode=mFIXED) and (((-EstimatedPower)-1)>RequestedDigits) then begin
   Buffer:='';
   Len:=0;
   DecimalPoint:=-RequestedDigits;
  end else begin
   Assert(BigNumMaxSignificantMantissaBits>=(324*4));
   NeedBoundaryDeltas:=Mode=mSHORTEST;
   InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   case Mode of
    mSHORTEST:begin
     GenerateShortestDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len);
    end;
    mFIXED:begin
     GenerateFixedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
    else {mPRECISION:}begin
     GenerateCountedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
   end;
  end;
 end;
 procedure GenerateRadixDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:boolean;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint;Radix:longint);
 const Base36:array[0..36] of {$ifdef BESENSingleStringType}char{$else}ansichar{$endif}='0123456789abcdefghijklmnopqrstuvwxyz{';
 var Digit,Compare,MaxDigit:longint;
     InDeltaRoomMinus,InDeltaRoomPlus:boolean;
  function ValueOf(c:{$ifdef BESENSingleStringType}char{$else}ansichar{$endif}):longint;
  begin
   case c of
    '0'..'9':begin
     result:=ord(c)-ord('0');
    end;
    else begin
     result:=(ord(c)-ord('a'))+$a;
    end;
   end;
  end;
 begin
  Len:=0;
  MaxDigit:=Radix-1;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=MaxDigit));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=Base36[Digit];
   BigNumClamp(Numerator);
   BigNumClamp(DeltaMinus);
   BigNumClamp(DeltaPlus);
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,Radix);
    BigNumMultiplyByUInt32(DeltaMinus,Radix);
    BigNumMultiplyByUInt32(DeltaPlus,Radix);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin       
    end else if Compare>0 then begin
     Assert(ValueOf(Buffer[Len])<>MaxDigit);
     Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    end else begin
     if (ValueOf(Buffer[Len]) and 1)<>0 then begin
      Assert(ValueOf(Buffer[Len])<>MaxDigit);
      Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(ValueOf(Buffer[Len])<>MaxDigit);
    Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    exit;
   end;
  end;
 end;
 procedure DoubleToRadix(Value:double;Radix:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint);
 var Casted:qword absolute Value;
     SignificantMantissa:qword;
     Exponent,EstimatedPower:longint;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:boolean;
 begin
  Assert(Value>0);
  Assert(BESENIsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePowerOf(NormalizedExponent(SignificantMantissa,Exponent),Radix);
  Assert(BigNumMaxSignificantMantissaBits>=(324*4));
  NeedBoundaryDeltas:=true;
  InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  GenerateRadixDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len,Radix);
 end;
 procedure FastDoubleToRadix(v:double;Radix:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint);
 const Base36:array[0..35] of {$ifdef BESENSingleStringType}char{$else}ansichar{$endif}='0123456789abcdefghijklmnopqrstuvwxyz';
{$ifndef BESENNoFPU}
       DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
       DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
       DtoAFPURoundingMode:TFPURoundingMode=rmNEAREST;
{$endif}
 var IntPart,FracPart,Old,Epsilon:double;
     Digit,i,j:longint;
     TempBuffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};
{$ifndef BESENNoFPU}
     OldFPUExceptionMask:TFPUExceptionMask;
     OldFPUPrecisionMode:TFPUPrecisionMode;
     OldFPURoundingMode:TFPURoundingMode;
{$endif}
     IntPart64:int64;
 begin
  if (Radix<2) or (Radix>36) then begin
   result:='';
  end else begin
{$ifndef BESENNoFPU}
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
{$endif}
   try
{$ifndef BESENNoFPU}
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(DtoAFPURoundingMode);
    end;
{$endif}
    try
     TempBuffer:='';
     IntPart:=System.Int(v);
     FracPart:=System.Frac(v);
     if IntPart=0 then begin
      result:='0';
     end else begin
      if IntPart<4294967295.0 then begin
       IntPart64:=trunc(IntPart);
       while IntPart64>0 do begin
        Digit:=IntPart64 mod Radix;
        Assert((Digit>=0) and (Digit<Radix));
        IntPart64:=IntPart64 div Radix;
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end else begin
       while IntPart>0 do begin
        Old:=IntPart;
        IntPart:=System.Int(IntPart/Radix);
        Digit:=trunc(Old-(IntPart*Radix));
        Assert((Digit>=0) and (Digit<Radix));
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end;
      SetLength(Buffer,Len);
      j:=1;
      for i:=Len downto 1 do begin
       Buffer[j]:=TempBuffer[i];
       inc(j);
      end;
     end;
     if FracPart<>0 then begin
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:='.';
      Epsilon:=0.001/Radix;
      while (FracPart>=Epsilon) and (Len<32) do begin
       FracPart:=FracPart*Radix;
       Digit:=trunc(FracPart);
       FracPart:=System.Frac(FracPart);
       Assert((Digit>=0) and (Digit<Radix));
       inc(Len);
       if Len>=length(Buffer) then begin
        SetLength(Buffer,Len*2);
       end;
       Buffer[Len]:=Base36[Digit];
      end;
     end;
    finally
     TempBuffer:='';
    end;
   finally
{$ifndef BESENNoFPU}
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
{$endif}
   end;
  end;
 end;
 function GetCachedPowerForBinaryExponentRange(MinExponent,MaxExponent:longint;var Power:TDoubleValue;var DecimalExponent:longint):boolean;
 var Index:longint;
 begin
  result:=false;
  if (low(BESENDoubleToStringPowerOfTenBinaryExponentTable)<=MinExponent) and (MinExponent<=high(BESENDoubleToStringPowerOfTenBinaryExponentTable)) then begin
   Index:=BESENDoubleToStringPowerOfTenBinaryExponentTable[MinExponent];
   if ((Index>=0) and (Index<length(BESENDoubleToStringPowerOfTenTable))) and ((MinExponent<=BESENDoubleToStringPowerOfTenTable[Index,1]) and (BESENDoubleToStringPowerOfTenTable[Index,1]<=MaxExponent)) then begin
    Power.SignificantMantissa:=BESENDoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=BESENDoubleToStringPowerOfTenTable[Index,1];
    DecimalExponent:=BESENDoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function GetCachedPowerForDecimalExponent(RequestedExponent:longint;var Power:TDoubleValue;var FoundExponent:longint):boolean;
 var Index:longint;
 begin
  result:=false;
  if (low(BESENDoubleToStringPowerOfTenDecimalExponentTable)<=RequestedExponent) and (RequestedExponent<=high(BESENDoubleToStringPowerOfTenDecimalExponentTable)) then begin
   Index:=BESENDoubleToStringPowerOfTenDecimalExponentTable[RequestedExponent];
   if (Index>=0) and (Index<length(BESENDoubleToStringPowerOfTenTable)) then begin
    Power.SignificantMantissa:=BESENDoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=BESENDoubleToStringPowerOfTenTable[Index,1];
    FoundExponent:=BESENDoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function RoundWeed(var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};Len:longint;DistanceTooHighW,UnsafeInterval,Rest,TenCapacity,UnitValue:qword):boolean;
 var SmallDistance,BigDistance:qword;
 begin
  SmallDistance:=DistanceTooHighW-UnitValue;
  BigDistance:=DistanceTooHighW+UnitValue;
  Assert(QWordLessOrEqual(Rest,UnsafeInterval));
  while (QWordLess(Rest,SmallDistance) and (QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity))) and (QWordLess(Rest+TenCapacity,SmallDistance) or QWordGreaterOrEqual(SmallDistance-Rest,((Rest+TenCapacity)-SmallDistance))) do begin
   dec(Buffer[Len]);
   inc(Rest,TenCapacity);
  end;
  if ((QWordLess(Rest,BigDistance) and QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity)) and (QWordLess(Rest+TenCapacity,BigDistance) or QWordGreater(BigDistance-Rest,((Rest+TenCapacity)-BigDistance)))) then begin
   result:=false;
  end else begin
   result:=(QWordLessOrEqual(2*UnitValue,Rest) and QWordLessOrEqual(Rest,UnsafeInterval-(4*UnitValue)));
  end;
 end;
 function RoundWeedCounted(var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};Len:longint;Rest,TenCapacity,UnitValue:qword;var Capacity:longint):boolean;
 var i:longint;
 begin
  Assert(QWordLess(Rest,TenCapacity));
  result:=false;
  if QWordGreater(TenCapacity-UnitValue,UnitValue) then begin
   result:=QWordGreater(TenCapacity-Rest,Rest) and QWordGreaterOrEqual(TenCapacity-(2*Rest),2*UnitValue);
   if not result then begin
    result:=QWordGreater(Rest,UnitValue) and QWordLessOrEqual(TenCapacity-(Rest-UnitValue),Rest-UnitValue);
    if result then begin
     inc(Buffer[Len]);
     for i:=Len downto 2 do begin
      if ord(Buffer[i])<>(ord('0')+10) then begin
       break;
      end;
      Buffer[i]:='0';
      inc(Buffer[i-1]);
     end;
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(Capacity);
    end;
   end;
  end;
 end;
 function BiggestPowerTen(Number:longword;NumberBits:longint;var Power:longword;var Exponent:longint):boolean;
 label c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11;
 begin
  result:=true;
  case NumberBits of
   30,31,32:begin
    c1:
    if 1000000000<=Number then begin
     Power:=1000000000;
     Exponent:=9;
    end else begin
     goto c2;
    end;
   end;
   27,28,29:begin
    c2:
    if 100000000<=Number then begin
     Power:=100000000;
     Exponent:=8;
    end else begin
     goto c3;
    end;
   end;
   24,25,26:begin
    c3:
    if 10000000<=Number then begin
     Power:=10000000;
     Exponent:=7;
    end else begin
     goto c4;
    end;
   end;
   20,21,22,23:begin
    c4:
    if 1000000<=Number then begin
     Power:=1000000;
     Exponent:=6;
    end else begin
     goto c5;
    end;
   end;
   17,18,19:begin
    c5:
    if 100000<=Number then begin
     Power:=100000;
     Exponent:=5;
    end else begin
     goto c6;
    end;
   end;
   14,15,16:begin
    c6:
    if 10000<=Number then begin
     Power:=10000;
     Exponent:=4;
    end else begin
     goto c7;
    end;
   end;
   10,11,12,13:begin
    c7:
    if 1000<=Number then begin
     Power:=1000;
     Exponent:=3;
    end else begin
     goto c8;
    end;
   end;
   7,8,9:begin
    c8:
    if 100<=Number then begin
     Power:=100;
     Exponent:=2;
    end else begin
     goto c9;
    end;
   end;
   4,5,6:begin
    c9:
    if 10<=Number then begin
     Power:=10;
     Exponent:=1;
    end else begin
     goto c10;
    end;
   end;
   1,2,3:begin
    c10:
    if 1<=Number then begin
     Power:=1;
     Exponent:=0;
    end else begin
     goto c11;
    end;
   end;
   0:begin
    c11:
    Power:=0;
    Exponent:=-1;
   end;
   else begin
    Power:=0;
    Exponent:=0;
    result:=false;
   end;
  end;
 end;
 function DigitGen(Low,w,High:TDoubleValue;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,Capacity:longint):boolean;
 var UnitValue,Fractionals,Rest:qword;
     TooLow,TooHigh,UnsafeInterval,One:TDoubleValue;
     Integrals,Divisor,Digit:longword;
     DivisorExponent:longint;
 begin
  result:=false;
  if ((Low.Exponent=w.Exponent) and (w.Exponent=High.Exponent)) and (QWordLessOrEqual(Low.SignificantMantissa+1,High.SignificantMantissa-1) and
     ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent))) then begin
   UnitValue:=1;
   TooLow.SignificantMantissa:=Low.SignificantMantissa-UnitValue;
   TooLow.Exponent:=Low.Exponent;
   TooHigh.SignificantMantissa:=High.SignificantMantissa+UnitValue;
   TooHigh.Exponent:=High.Exponent;
   UnsafeInterval:=DoubleValueMinus(TooHigh,TooLow);
   One.SignificantMantissa:=qword(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=TooHigh.SignificantMantissa shr (-One.Exponent);
   Fractionals:=TooHigh.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
     dec(Capacity);
     Rest:=qword(qword(Integrals) shl (-One.Exponent))+Fractionals;
     if QWordLess(Rest,UnsafeInterval.SignificantMantissa) then begin
      result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa,UnsafeInterval.SignificantMantissa,Rest,qword(Divisor) shl (-One.Exponent),UnitValue);
      exit;
     end;
     Divisor:=Divisor div 10;
    end;
    if (One.Exponent>=-60) and (QWordLess(Fractionals,One.SignificantMantissa) and QWordGreaterOrEqual(qword($1999999999999999),One.SignificantMantissa)) then begin
     while true do begin
      Fractionals:=Fractionals*10;
      UnitValue:=UnitValue*10;
      UnsafeInterval.SignificantMantissa:=UnsafeInterval.SignificantMantissa*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
      if QWordLess(Fractionals,UnsafeInterval.SignificantMantissa) then begin
       result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa*UnitValue,UnsafeInterval.SignificantMantissa,Fractionals,One.SignificantMantissa,UnitValue);
       exit;
      end;
     end;
    end;
   end;
  end;
 end;
 function DigitGenCounted(w:TDoubleValue;RequestedDigits:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,Capacity:longint):boolean;
 var wError,Fractionals,Rest:qword;
     One:TDoubleValue;
     Integrals,Divisor,Digit:longword;
     DivisorExponent:longint;
 begin
  result:=false;
  if ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent)) and ((MinimalTargetExponent>=-60) and (MaximalTargetExponent<=-32)) then begin
   wError:=1;
   One.SignificantMantissa:=qword(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=w.SignificantMantissa shr (-One.Exponent);
   Fractionals:=w.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
     dec(RequestedDigits);
     dec(Capacity);
     if RequestedDigits=0 then begin
      break;
     end;
     Divisor:=Divisor div 10;
    end;
    if RequestedDigits=0 then begin
     Rest:=qword(qword(Integrals) shl (-One.Exponent))+Fractionals;
     result:=RoundWeedCounted(Buffer,Len,Rest,qword(Divisor) shl (-One.Exponent),wError,Capacity);
     exit;
    end;
    if ((One.Exponent>=-60) and QWordLess(Fractionals,One.SignificantMantissa)) and QWordGreaterOrEqual(qword($1999999999999999),One.SignificantMantissa) then begin
     while (RequestedDigits>0) and (Fractionals>wError) do begin
      Fractionals:=Fractionals*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
      dec(RequestedDigits);
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
     end;
     if RequestedDigits=0 then begin
      result:=RoundWeedCounted(Buffer,Len,Fractionals,One.SignificantMantissa,wError,Capacity);
     end else begin
      result:=false;
     end;
    end;
   end;
  end;
 end;
 procedure NormalizedBoundaries(Value:double;var BoundaryMinus,BoundaryPlus:TDoubleValue);
 var v:TDoubleValue;
     SignificantMantissaIsZero:boolean;
 begin
  Assert(not BESENIsNegative(Value));
  Assert(BESENIsFinite(Value));
  SplitDouble(Value,v.SignificantMantissa,v.Exponent);
  SignificantMantissaIsZero:=v.SignificantMantissa=qword($0010000000000000);
  BoundaryPlus.SignificantMantissa:=(v.SignificantMantissa shl 1)+1;
  BoundaryPlus.Exponent:=v.Exponent-1;
  DoubleValueNormalize(BoundaryPlus);
  if SignificantMantissaIsZero and (v.Exponent<>((-($3ff+52))+1)) then begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 2)-1;
   BoundaryMinus.Exponent:=v.Exponent-2;
  end else begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 1)-1;
   BoundaryMinus.Exponent:=v.Exponent-1;
  end;
  BoundaryMinus.SignificantMantissa:=BoundaryMinus.SignificantMantissa shl (BoundaryMinus.Exponent-BoundaryPlus.Exponent);
  BoundaryMinus.Exponent:=BoundaryPlus.Exponent;
 end;
 function DoFastShortest(Value:double;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalExponent:longint):boolean;
 var w,BoundaryMinus,BoundaryPlus,TenMK,ScaledW,ScaledBoundaryMinus,ScaledBoundaryPlus:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:longint;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  NormalizedBoundaries(Value,BoundaryMinus,BoundaryPlus);
  Assert(BoundaryPlus.Exponent=w.Exponent);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    if ScaledW.Exponent=(BoundaryPlus.Exponent+TenMK.Exponent+SignificantMantissaSize) then begin
     ScaledBoundaryMinus:=DoubleValueMul(BoundaryMinus,TenMK);
     ScaledBoundaryPlus:=DoubleValueMul(BoundaryPlus,TenMK);
     Capacity:=0;
     result:=DigitGen(ScaledBoundaryMinus,ScaledW,ScaledBoundaryPlus,Buffer,Len,Capacity);
     DecimalExponent:=Capacity-mK;
    end;
   end;
  end;
 end;
 function DoFastPrecision(Value:double;RequestedDigits:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalExponent:longint):boolean;
 var w,TenMK,ScaledW:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:longint;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    Capacity:=0;
    result:=DigitGenCounted(ScaledW,RequestedDigits,Buffer,Len,Capacity);
    DecimalExponent:=Capacity-mK;
   end;
  end;
 end;
 function DoFastFixed(Value:double;FracitionalCount:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint):boolean;
 const Five17=$b1a2bc2ec5; // 5^17
 type TInt128=record
       High,Low:qword;
      end;
  procedure Int128Mul(var a:TInt128;const Multiplicand:longword);
  var Accumulator:qword;
      Part:longword;
  begin
   Accumulator:=(a.Low and $ffffffff)*Multiplicand;
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.Low shr 32)*Multiplicand);
   a.Low:=(Accumulator shl 32)+Part;
   Accumulator:=(Accumulator shr 32)+((a.High and $ffffffff)*Multiplicand);
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.High shr 32)*Multiplicand);
   a.High:=(Accumulator shl 32)+Part;
   Assert((Accumulator shr 32)=0);
  end;
  procedure Int128Shift(var a:TInt128;const Shift:longint);
  begin
   Assert(((-64)<=Shift) and (Shift<=64));
   if Shift<>0 then begin
    if Shift=-64 then begin
     a.High:=a.Low;
     a.Low:=0;
    end else if Shift=64 then begin
     a.Low:=a.High;
     a.High:=0;
    end else if Shift<=0 then begin
     a.High:=(a.High shl (-Shift))+(a.Low shr (64+Shift));
     a.Low:=a.Low shl (-Shift);
    end else begin
     a.Low:=(a.Low shr Shift)+(a.High shl (64-Shift));
     a.High:=a.High shr Shift;
    end;
   end;
  end;
  function Int128DivModPowerOfTwo(var a:TInt128;const Power:longint):longint;
  begin
   if Power>=64 then begin
    result:=a.High shr (Power-64);
    dec(a.High,result shl (Power-64));
   end else begin
    result:=(a.Low shr Power)+(a.High shl (64-Power));
    a.High:=0;
    dec(a.Low,(a.Low shr Power) shl Power);
   end;
  end;
  function Int128IsZero(const a:TInt128):boolean;
  begin
   result:=(a.High=0) and (a.Low=0);
  end;
  function Int128BitAt(const a:TInt128;const Position:longint):boolean;
  begin
   if Position>=64 then begin
    result:=((a.High shr (Position-64)) and 1)<>0;
   end else begin
    result:=((a.LOw shr Position) and 1)<>0;
   end;
  end;
  procedure FillDigits32FixedLength(Number:longword;RequestedLength:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
  var i,l:longint;
  begin
   l:=Len;
   inc(Len,RequestedLength);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   for i:=RequestedLength downto 1 do begin
    Buffer[l+i]:={$ifdef BESENSingleStringType}DigitChars[Number mod 10]{$else}AnsiChar(byte(byte(AnsiChar('0'))+(Number mod 10))){$endif};
    Number:=Number div 10;
   end;
  end;
  procedure FillDigits32(Number:longword;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
  var NumberLength,i,l:longint;
      OldNumber:longword;
  begin
   OldNumber:=Number;
   NumberLength:=0;
   while Number<>0 do begin
    Number:=Number div 10;
    inc(NumberLength);
   end;
   if NumberLength<>0 then begin
    l:=Len;
    inc(Len,NumberLength);
    if Len>=length(Buffer) then begin
     SetLength(Buffer,Len*2);
    end;
    Number:=OldNumber;
    for i:=NumberLength downto 1 do begin
     Buffer[l+i]:={$ifdef BESENSingleStringType}DigitChars[Number mod 10]{$else}AnsiChar(byte(byte(AnsiChar('0'))+(Number mod 10))){$endif};
     Number:=Number div 10;
    end;
   end;
  end;
  procedure FillDigits64FixedLength(Number:qword;RequestedLength:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
  var p0,p1,p2:longword;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   FillDigits32FixedLength(p0,3,Buffer,Len);
   FillDigits32FixedLength(p1,7,Buffer,Len);
   FillDigits32FixedLength(p2,7,Buffer,Len);
  end;
  procedure FillDigits64(Number:qword;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len:longint);
  var p0,p1,p2:longword;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   if p0<>0 then begin
    FillDigits32(p0,Buffer,Len);
    FillDigits32FixedLength(p1,7,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else if p1<>0 then begin
    FillDigits32(p1,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else begin
    FillDigits32(p2,Buffer,Len);
   end;
  end;
  procedure RoundUp(var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint);
  var i:longint;
  begin
   if Len=0 then begin
    Buffer:='1';
    Len:=1;
    DecimalPoint:=1;
   end else begin
    inc(Buffer[Len]);
    for i:=Len downto 2 do begin
     if ord(Buffer[i])<>(ord('0')+10) then begin
      exit;
     end;
     Buffer[i]:='0';
     inc(Buffer[i-1]);
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(DecimalPoint);
    end;
   end;
  end;
  procedure FillFractionals(Fractionals:qword;Exponent:longint;FractionalCount:longint;var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint);
  var Point,i,Digit:longint;
      Fractionals128:TInt128;
  begin
   Assert(((-128)<=Exponent) and (Exponent<=0));
   if (-Exponent)<=64 then begin
    Assert((Fractionals shr 56)=0);
    Point:=-Exponent;
    for i:=1 to FracitionalCount do begin
     Fractionals:=Fractionals*5;
     dec(Point);
     Digit:=Fractionals shr Point;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
     dec(Fractionals,qword(Digit) shl Point);
    end;
    if ((Fractionals shr (Point-1)) and 1)<>0 then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end else begin
    Assert((64<(-Exponent)) and ((-Exponent)<=128));
    Fractionals128.High:=Fractionals;
    Fractionals128.Low:=0;
    Int128Shift(Fractionals128,(-Exponent)-64);
    Point:=128;
    for i:=1 to FracitionalCount do begin
     if Int128IsZero(Fractionals128) then begin
      break;
     end;
     Int128Mul(Fractionals128,5);
     dec(Point);
     Digit:=Int128DivModPowerOfTwo(Fractionals128,Point);
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:={$ifdef BESENSingleStringType}DigitChars[Digit]{$else}AnsiChar(byte(byte(AnsiChar('0'))+Digit)){$endif};
    end;
    if Int128BitAt(Fractionals128,Point-1) then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end;
  end;
  procedure TrimZeros(var Buffer:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};var Len,DecimalPoint:longint);
  var i:longint;
  begin
   while (Len>0) and (Buffer[Len]='0') do begin
    dec(Len);
   end;
   i:=0;
   while (i<Len) and (Buffer[i+1]='0') do begin
    inc(i);
   end;
   if i<>0 then begin
    Delete(Buffer,1,i);
    dec(Len,i);
    dec(DecimalPoint,i);
   end;
  end;
 var SignificantMantissa,Divisor,Dividend,Remainder,Integrals,Fractionals:qword;
     Exponent,DivisorPower:longint;
     Quotient:longword;
 begin
  result:=false;
  SplitDouble(Value,SignificantMantissa,Exponent);
  if (Exponent<=20) and (FracitionalCount<=20) then begin
   Len:=0;
   if (Exponent+53)>74 then begin
    Divisor:=Five17;
    DivisorPower:=17;
    Dividend:=SignificantMantissa;
    if Exponent>DivisorPower then begin
     Dividend:=Dividend shl (Exponent-DivisorPower);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl DivisorPower;
    end else begin
     Dividend:=Dividend shl (DivisorPower-Exponent);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl Exponent;
    end;
    FillDigits32(Quotient,Buffer,Len);
    FillDigits64FixedLength(Remainder,DivisorPower,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>=0 then begin
    SignificantMantissa:=SignificantMantissa shl Exponent;
    FillDigits64(SignificantMantissa,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>-53 then begin
    Integrals:=SignificantMantissa shr (-Exponent);
    Fractionals:=SignificantMantissa-(Integrals shl (-Exponent));
    if Integrals>$ffffffff then begin
     FillDigits64(Integrals,Buffer,Len);
    end else begin
     FillDigits32(Integrals,Buffer,Len);
    end;
    DecimalPoint:=Len;
    FillFractionals(Fractionals,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end else if Exponent<-128 then begin
    Assert(FracitionalCount>=20);
    Buffer:='';
    Len:=0;
    DecimalPoint:=-FracitionalCount;
   end else begin
    DecimalPoint:=0;
    FillFractionals(SignificantMantissa,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end;
   TrimZeros(Buffer,Len,DecimalPoint);
   SetLength(Buffer,Len);
   if Len=0 then begin
    DecimalPoint:=-FracitionalCount;
   end;
   result:=true;
  end;
 end;
var OK,Fast:boolean;
    Len,DecimalPoint,ZeroPrefixLength,ZeroPostfixLength,i:longint;
begin
 if BESENIsNaN(AValue) then begin
  result:='NaN';
 end else if BESENIsZero(AValue) then begin
  result:='0';
 end else if BESENIsNegInfinite(AValue) then begin
  result:='-Infinity';
 end else if BESENIsNegative(AValue) then begin
  result:='-'+BESENDoubleToString(BESENNumberAbsolute(AValue),Mode,RequestedDigits);
 end else if BESENIsInfinite(AValue) then begin
  result:='Infinity';
 end else begin
  result:='0';
  if AValue<>0 then begin
   Len:=0;
   DecimalPoint:=0;
   OK:=false;
   Fast:=false;
   if ((Mode=BESEN_DOUBLETOSTRINGMODE_FIXED) and (AValue>=1e21)) or ((Mode=BESEN_DOUBLETOSTRINGMODE_RADIX) and (RequestedDigits=10)) then begin
    Mode:=BESEN_DOUBLETOSTRINGMODE_STANDARD;
   end;
   case Mode of
    BESEN_DOUBLETOSTRINGMODE_STANDARD,BESEN_DOUBLETOSTRINGMODE_STANDARDEXPONENTIAL:begin
     OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
     inc(DecimalPoint,Len);
    end;
    BESEN_DOUBLETOSTRINGMODE_FIXED:begin
     OK:=DoFastFixed(AValue,RequestedDigits,result,Len,DecimalPoint);
    end;
    BESEN_DOUBLETOSTRINGMODE_EXPONENTIAL,BESEN_DOUBLETOSTRINGMODE_PRECISION:begin
     if RequestedDigits<=0 then begin
      OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
      RequestedDigits:=Len-1;
     end else begin
      OK:=DoFastPrecision(AValue,RequestedDigits,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
     end;
     Assert((Len>0) and (Len<=(RequestedDigits+1)));
    end;
    BESEN_DOUBLETOSTRINGMODE_RADIX:begin
     if ((RequestedDigits>=2) and (RequestedDigits<=36)) and (BESENIsFinite(AValue) and (AValue<4294967295.0) and (System.Int(AValue)=AValue)) then begin
      FastDoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
      Fast:=true;
      OK:=true;
     end;
    end;
   end;
   if not OK then begin
    case Mode of
     BESEN_DOUBLETOSTRINGMODE_STANDARD,BESEN_DOUBLETOSTRINGMODE_STANDARDEXPONENTIAL:begin
      DoubleToDecimal(AValue,mSHORTEST,RequestedDigits,result,Len,DecimalPoint);
      OK:=true;
     end;
     BESEN_DOUBLETOSTRINGMODE_FIXED:begin
      DoubleToDecimal(AValue,mFIXED,RequestedDigits,result,Len,DecimalPoint);
      OK:=true;
     end;
     BESEN_DOUBLETOSTRINGMODE_EXPONENTIAL,BESEN_DOUBLETOSTRINGMODE_PRECISION:begin
      if RequestedDigits<=0 then begin
       DoubleToDecimal(AValue,mSHORTEST,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
       RequestedDigits:=Len-1;
      end else begin
       DoubleToDecimal(AValue,mPRECISION,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
      Assert((Len>0) and (Len<=(RequestedDigits+1)));
     end;
     BESEN_DOUBLETOSTRINGMODE_RADIX:begin
      if (RequestedDigits>=2) and (RequestedDigits<=36) then begin
       DoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
     end;
    end;
   end;
   if OK then begin
    SetLength(result,Len);
    case Mode of
     BESEN_DOUBLETOSTRINGMODE_STANDARD:begin
      if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
       SetLength(result,DecimalPoint);
       FillChar(result[Len+1],DecimalPoint-Len,'0');
      end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
       Insert('.',result,DecimalPoint+1);
      end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
       for i:=1 to -DecimalPoint do begin
        result:='0'+result;
       end;
       result:='0.'+result;
      end else begin
       if Len<>1 then begin
        Insert('.',result,2);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
       end;
      end;
     end;
     BESEN_DOUBLETOSTRINGMODE_STANDARDEXPONENTIAL:begin
      if Len<>1 then begin
       Insert('.',result,2);
      end;
      if DecimalPoint>=0 then begin
       result:=result+'e+'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
      end else begin
       result:=result+'e-'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
      end;
     end;
     BESEN_DOUBLETOSTRINGMODE_FIXED:begin
      ZeroPrefixLength:=0;
      ZeroPostfixLength:=0;
      if DecimalPoint<=0 then begin
       ZeroPrefixLength:=(-DecimalPoint)+1;
       DecimalPoint:=1;
      end;
      if (ZeroPrefixLength+Len)<(DecimalPoint+RequestedDigits) then begin
       ZeroPostfixLength:=((DecimalPoint+RequestedDigits)-Len)-ZeroPrefixLength;
      end;
      for i:=1 to ZeroPrefixLength do begin
       result:='0'+result;
      end;
      for i:=1 to ZeroPostfixLength do begin
       result:=result+'0';
      end;
      if (RequestedDigits>0) and (DecimalPoint>0) and (DecimalPoint<=length(result)) then begin
       Insert('.',result,DecimalPoint+1);
      end;
     end;
     BESEN_DOUBLETOSTRINGMODE_EXPONENTIAL:begin
      if RequestedDigits<1 then begin
       RequestedDigits:=1;
      end;
      if RequestedDigits<>1 then begin
       Insert('.',result,2);
       for i:=Len+1 to RequestedDigits do begin
        result:=result+'0';
       end;
      end else begin
       SetLength(result,1);
      end;
      if DecimalPoint>=0 then begin
       result:=result+'e+'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
      end else begin
       result:=result+'e-'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
      end;
     end;
     BESEN_DOUBLETOSTRINGMODE_PRECISION:begin
      if RequestedDigits<1 then begin
       RequestedDigits:=1;
      end;
      if (DecimalPoint<-6) or (DecimalPoint>=RequestedDigits) then begin
       if RequestedDigits<>1 then begin
        Insert('.',result,2);
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,1);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
       end;
      end else begin
       if DecimalPoint<=0 then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,RequestedDigits);
        for i:=Len+1 to RequestedDigits do begin
         result[i]:='0';
        end;
        if DecimalPoint<RequestedDigits then begin
         if Len<>1 then begin
          Insert('.',result,DecimalPoint+1);
         end;
        end;
       end;
      end;
     end;
     BESEN_DOUBLETOSTRINGMODE_RADIX:begin
      if not Fast then begin
       if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
        SetLength(result,DecimalPoint);
        FillChar(result[Len+1],DecimalPoint-Len,'0');
       end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
        Insert('.',result,DecimalPoint+1);
       end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
       end else begin
        if Len<>1 then begin
         Insert('.',result,2);
        end;
        if DecimalPoint>=0 then begin
         result:=result+'p+'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
        end else begin
         result:=result+'p-'+{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif}(IntToStr(abs(DecimalPoint-1)));
        end;
       end;
       while (length(result)>1) and ((result[1]='0') and (result[2] in ['0'..'9','a'..'f'])) do begin
        Delete(result,1,1);
       end;
      end;
     end;
    end;
   end else begin
    result:='';
   end;
  end;
 end;
end;

function BESENNumberToString(const Value:TBESENNumber):TBESENString;
begin
 if BESENIsNaN(Value) then begin
  result:='NaN';
 end else if BESENIsZero(Value) then begin
  result:='0';
 end else if BESENIsNegInfinite(Value) then begin
  result:='-Infinity';
 end else if BESENIsNegative(Value) then begin
  result:='-'+BESENFloatToStr(-Value);
 end else if BESENIsInfinite(Value) then begin
  result:='Infinity';
 end else begin
  result:=TBESENString(BESENDoubleToString(Value,BESEN_DOUBLETOSTRINGMODE_STANDARD,0));
 end;
end;

function BESENNumberToRadixString(const Value:TBESENNumber;Radix:longint):TBESENString;
begin
 if BESENIsNaN(Value) then begin
  result:='NaN';
 end else if BESENIsZero(Value) then begin
  result:='0';
 end else if BESENIsNegInfinite(Value) then begin
  result:='-Infinity';
 end else if BESENIsNegative(Value) then begin
  result:='-'+BESENFloatToStr(-Value);
 end else if BESENIsInfinite(Value) then begin
  result:='Infinity';
 end else begin
  result:=TBESENString(BESENDoubleToString(Value,BESEN_DOUBLETOSTRINGMODE_RADIX,Radix));
 end;
end;

function BESENFloatToStr(const Value:TBESENNumber):TBESENString;
begin
 if BESENIsNaN(Value) then begin
  result:='NaN';
 end else if BESENIsZero(Value) then begin
  result:='0';
 end else if BESENIsNegInfinite(Value) then begin
  result:='-Infinity';
 end else if BESENIsNegative(Value) then begin
  result:='-'+BESENFloatToStr(-Value);
 end else if BESENIsInfinite(Value) then begin
  result:='Infinity';
 end else begin
  result:=TBESENString(BESENDoubleToString(Value,BESEN_DOUBLETOSTRINGMODE_STANDARD,0));
 end;
end;

function BESENFloatToLocaleStr(const Value:TBESENNumber):TBESENString;
var i:integer;
begin
 if BESENIsNaN(Value) then begin
  result:='NaN';
 end else if BESENIsZero(Value) then begin
  result:='0';
 end else if BESENIsNegInfinite(Value) then begin
  result:='-Infinity';
 end else if BESENIsNegative(Value) then begin
  result:='-'+BESENFloatToLocaleStr(-Value);
 end else if BESENIsInfinite(Value) then begin
  result:='Infinity';
 end else begin
  result:=TBESENString(BESENDoubleToString(Value,BESEN_DOUBLETOSTRINGMODE_STANDARD,0));
  for i:=1 to length(result) do begin
   case word(result[i]) of
    ord('.'):begin
     result[i]:=widechar(word(ord(BESENLocaleFormatSettings.DecimalSeparator)));
    end;
   end;
  end;
 end;
end;

procedure InitBESEN;
begin
 InitBESENNumberTables;
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;
end.
