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
unit BESENStringUtils;
{$i BESEN.inc}

interface

uses SysUtils,Classes,BESENConstants,BESENTypes,BESENCharset;

type TBESENHexValues=array[word] of byte;

const BESENBase64Chars='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

const BESENHexChars:array[boolean,0..15] of widechar=(('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'),
                                                      ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'));

var BESENHexValues:TBESENHexValues;

function BESENPosChar(ToFindChar:TBESENWIDECHAR;const InString:TBESENSTRING):longint;
function BESENPos(const ToFindString,InString:TBESENSTRING):longint;

{$ifndef BESENSingleStringType}
function BESENANSIPosChar(ToFindChar:TBESENCHAR;const InString:TBESENANSISTRING):longint;
function BESENANSIPos(const ToFindString,InString:TBESENANSISTRING):longint;

function BESENANSITrim(const InputString:TBESENANSISTRING):TBESENANSISTRING;

function BESENANSIUpperCase(const InputString:TBESENANSISTRING):TBESENANSISTRING;
{$endif}

function BESENStringCompare(const s1,s2:TBESENString):longint; {$ifdef caninline}inline;{$else}{$ifdef UseRegister}register;{$endif}{$endif}

{$ifdef BESENSingleStringType}
function BESENGetFileContent(fn:TBESENSTRING):TBESENSTRING;
{$else}
function BESENGetFileContent(fn:TBESENANSISTRING):TBESENANSISTRING;
function BESENConvertToUTF8(s:TBESENANSISTRING):TBESENANSISTRING;

function BESENDecodeBase64(s:TBESENANSISTRING):TBESENANSISTRING;
function BESENEncodeBase64(s:TBESENANSISTRING):TBESENANSISTRING;
function BESENDequote(s:TBESENANSISTRING):TBESENANSISTRING;

function BESENIsUTF8(const s:TBESENANSISTRING):boolean;
function BESENEncodeString(Value:TBESENANSISTRING;CharFrom:TBESENCharset;CharTo:TBESENCharset):TBESENANSISTRING;
function BESENGetCodePage(Value:TBESENANSISTRING):TBESENCharset;
function BESENGetCodePageID(Value:TBESENCharset):TBESENANSISTRING;
function BESENDoNeedEncoding(Value:TBESENANSISTRING):boolean;
function BESENFindIdealCoding(Value:TBESENANSISTRING;CharFrom:TBESENCharset;CharTo:TBESENCharsetSet):TBESENCharset;
function BESENISOToUTF8(s:TBESENANSISTRING):TBESENANSISTRING;

function BESENUTF32Pos(const ToFindString,InString:TBESENUTF32STRING):longint;
procedure BESENUTF32Delete(var s:TBESENUTF32STRING;Index,Len:longint);
function BESENUTF32Compare(const a,b:TBESENUTF32STRING):boolean; overload;
function BESENUTF32Compare(const a:TBESENUTF32STRING;const b:TBESENANSISTRING):boolean; overload;
procedure BESENUTF32Clear(var a:TBESENUTF32STRING);
procedure BESENBESENUTF32AddString(var a:TBESENUTF32STRING;const b:TBESENANSISTRING);
procedure BESENBESENUTF32AddChar(var a:TBESENUTF32STRING;const b:TBESENUTF32CHAR); overload;
procedure BESENBESENUTF32AddChar(var a:TBESENUTF32STRING;const b:TBESENCHAR); overload;
procedure BESENUTF32Add(var a:TBESENUTF32STRING;const b:TBESENUTF32STRING);
function BESENUTF32ToUTF8(const s:TBESENUTF32STRING):TBESENUTF8STRING;
function BESENUTF8ToUTF32(const s:TBESENUTF8STRING):TBESENUTF32STRING;
function BESENUTF8ToUTF16(const s:TBESENUTF8STRING):TBESENUTF16STRING;
function BESENUTF16ToUTF8(const s:TBESENUTF16STRING):TBESENUTF8STRING;
{$endif}
function BESENUTF32ToUTF16(const s:TBESENUTF32STRING):TBESENUTF16STRING;
function BESENUTF32CHARToUTF16(w:TBESENUTF32CHAR):TBESENUTF16STRING;
function BESENUTF16ToUTF32(const s:TBESENUTF16STRING):TBESENUTF32STRING;
{$ifndef BESENSingleStringType}
function BESENUTF32ToWIDESTRING(const s:TBESENUTF32STRING):widestring;
function BESENWIDESTRINGToUTF32(const s:widestring):TBESENUTF32STRING;
function BESENUTF32ToSTRING(const s:TBESENUTF32STRING):TBESENANSISTRING;
function BESENSTRINGToUTF32(const s:TBESENANSISTRING):TBESENUTF32STRING;
function BESENUTF32CharToUTF8(u4c:TBESENUTF32CHAR):TBESENUTF8STRING;
procedure BESENUTF8Inc(var s:TBESENUTF8STRING;var i:longint);
procedure BESENUTF8Dec(var s:TBESENUTF8STRING;var i:longint);
procedure BESENUTF8Delete(var s:TBESENUTF8STRING;i:longint);
function BESENUTF8Length(const s:TBESENUTF8STRING):longint;
function BESENUTF8GetRawPos(const s:TBESENUTF8STRING;Index:longint):longint;
function BESENUTF8GetChar(var s:TBESENUTF8STRING;Index:longint):TBESENUTF32CHAR;
function BESENUTF8GetRawChar(var s:TBESENUTF8STRING;i:longint):TBESENUTF32CHAR;
function BESENUTF8GetRawCharAndInc(var s:TBESENUTF8STRING;var i:longint):TBESENUTF32CHAR;
function BESENUTF8Pos(ToFindString,InString:TBESENUTF8STRING):longint;
{$endif}

function BESENUnicodeGetLUT(c:TBESENUTF32CHAR):longword; {$ifdef caninline}inline;{$endif}
function BESENUnicodeGetType(c:TBESENUTF32CHAR):longword; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsAlpha(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsAlphaNumber(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsLetter(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsIDPartEx(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsIDStart(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsIDPart(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsDigit(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsStringWhiteSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsParserWhiteSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsWhiteSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsLineTerminator(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsPrint(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsUpper(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeIsLower(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
function BESENUnicodeToUpper(c:TBESENUTF32CHAR):TBESENUTF32CHAR; {$ifdef caninline}inline;{$endif}
function BESENUnicodeToLower(c:TBESENUTF32CHAR):TBESENUTF32CHAR; {$ifdef caninline}inline;{$endif}

function BESENLowercase(const s:TBESENString):TBESENString;
function BESENUppercase(const s:TBESENString):TBESENString;

function BESENJSONStringQuote(const s:TBESENString):TBESENString;

function BESENIsHex(const v:word):boolean;

implementation

uses {$ifdef BESENEmbarcaderoNextGen}System.Character,{$endif}BESENUnicodeTables;

const JSCT_UNASSIGNED=0;
      JSCT_UPPERCASE_LETTER=1;
      JSCT_LOWERCASE_LETTER=2;
      JSCT_TITLECASE_LETTER=3;
      JSCT_MODIFIER_LETTER=4;
      JSCT_OTHER_LETTER=5;
      JSCT_NON_SPACING_MARK=6;
      JSCT_ENCLOSING_MARK=7;
      JSCT_COMBINING_SPACING_MARK=8;
      JSCT_DECIMAL_DIGIT_NUMBER=9;
      JSCT_LETTER_NUMBER=10;
      JSCT_OTHER_NUMBER=11;
      JSCT_SPACE_SEPARATOR=12;
      JSCT_LINE_SEPARATOR=13;
      JSCT_PARAGRAPH_SEPARATOR=14;
      JSCT_CONTROL=15;
      JSCT_FORMAT=16;
      JSCT_PRIVATE_USE=18;
      JSCT_SURROGATE=19;
      JSCT_DASH_PUNCTUATION=20;
      JSCT_START_PUNCTUATION=21;
      JSCT_END_PUNCTUATION=22;
      JSCT_CONNECTOR_PUNCTUATION=23;
      JSCT_OTHER_PUNCTUATION=24;
      JSCT_MATH_SYMBOL=25;
      JSCT_CURRENCY_SYMBOL=26;
      JSCT_MODIFIER_SYMBOL=27;
      JSCT_OTHER_SYMBOL=28;

function BESENUnicodeGetLUT(c:TBESENUTF32CHAR):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=UnicodeALut[UnicodeYLut[(UnicodeXLut[(c and $ffff) shr 6] shl 6) or (c and $3f)]];
end;

function BESENUnicodeGetType(c:TBESENUTF32CHAR):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=BESENUnicodeGetLUT(c) and $1f;
end;

function BESENUnicodeIsAlpha(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((((1 shl JSCT_UPPERCASE_LETTER) or
            (1 shl JSCT_LOWERCASE_LETTER) or
            (1 shl JSCT_TITLECASE_LETTER) or
            (1 shl JSCT_MODIFIER_LETTER) or
            (1 shl JSCT_OTHER_LETTER)) shr BESENUnicodeGetType(c)) and 1)<>0;
end;

function BESENUnicodeIsAlphaNumber(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((((1 shl JSCT_UPPERCASE_LETTER) or
            (1 shl JSCT_LOWERCASE_LETTER) or
            (1 shl JSCT_TITLECASE_LETTER) or
            (1 shl JSCT_MODIFIER_LETTER) or
            (1 shl JSCT_OTHER_LETTER) or
            (1 shl JSCT_DECIMAL_DIGIT_NUMBER)) shr BESENUnicodeGetType(c)) and 1)<>0;
end;

function BESENUnicodeIsLetter(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((((1 shl JSCT_UPPERCASE_LETTER) or
            (1 shl JSCT_LOWERCASE_LETTER) or
            (1 shl JSCT_TITLECASE_LETTER) or
            (1 shl JSCT_MODIFIER_LETTER) or
            (1 shl JSCT_OTHER_LETTER) or
            (1 shl JSCT_LETTER_NUMBER)) shr BESENUnicodeGetType(c)) and 1)<>0;
end;

function BESENUnicodeIsIDPartEx(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((((1 shl JSCT_UPPERCASE_LETTER) or
            (1 shl JSCT_LOWERCASE_LETTER) or
            (1 shl JSCT_TITLECASE_LETTER) or
            (1 shl JSCT_MODIFIER_LETTER) or
            (1 shl JSCT_OTHER_LETTER) or
            (1 shl JSCT_LETTER_NUMBER) or
            (1 shl JSCT_NON_SPACING_MARK) or
            (1 shl JSCT_COMBINING_SPACING_MARK) or
            (1 shl JSCT_DECIMAL_DIGIT_NUMBER) or
            (1 shl JSCT_CONNECTOR_PUNCTUATION)) shr BESENUnicodeGetType(c)) and 1)<>0;
end;

function BESENUnicodeIsIDStart(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=BESENUnicodeIsLetter(c) or ((c=ord('_')) or (c=ord('$')));
end;

function BESENUnicodeIsIDPart(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=BESENUnicodeIsIDPartEx(c) or ((c=ord('_')) or (c=ord('$')) or ((c=$200c) or (c=$200d)));
end;

function BESENUnicodeIsDigit(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=BESENUnicodeGetType(c)=JSCT_DECIMAL_DIGIT_NUMBER;
end;

function BESENUnicodeIsSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(BESENUnicodeGetLUT(c) and $00070000)=$0040000;
end;

function BESENUnicodeIsStringWhiteSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function BESENUnicodeIsParserWhiteSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function BESENUnicodeIsWhiteSpace(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(c=$0009) or (c=$000b) or (c=$000c) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function BESENUnicodeIsLineTerminator(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(c=$000a) or (c=$000d) or (c=$2028) or (c=$2029);
end;

function BESENUnicodeIsPrint(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=c<128;
end;

function BESENUnicodeIsUpper(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=BESENUnicodeGetType(c)=JSCT_UPPERCASE_LETTER;
end;

function BESENUnicodeIsLower(c:TBESENUTF32CHAR):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=BESENUnicodeGetType(c)=JSCT_LOWERCASE_LETTER;
end;

function BESENUnicodeToUpper(c:TBESENUTF32CHAR):TBESENUTF32CHAR; {$ifdef caninline}inline;{$endif}
begin
 if (BESENUnicodeGetLUT(c) and $00100000)<>0 then begin
  result:=c-TBESENUTF32CHAR(BESENUnicodeGetLUT(c) shr 22);
 end else begin
  result:=c;
 end;
end;

function BESENUnicodeToLower(c:TBESENUTF32CHAR):TBESENUTF32CHAR; {$ifdef caninline}inline;{$endif}
begin
 if (BESENUnicodeGetLUT(c) and $00200000)<>0 then begin
  result:=c+TBESENUTF32CHAR(BESENUnicodeGetLUT(c) shr 22);
 end else begin
  result:=c;
 end;
end;

function BESENPosChar(ToFindChar:TBESENWIDECHAR;const InString:TBESENSTRING):longint;
var i:longint;
begin
 result:=0;
 for i:=1 to length(InString) do begin
  if InString[i]=ToFindChar then begin
   result:=i;
   break;
  end;
 end;
end;

function BESENPos(const ToFindString,InString:TBESENSTRING):longint;
var i,j,l:longint;
    OK:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InString) do begin
  l:=i+length(ToFindString)-1;
  if l>length(InString) then begin
   exit;
  end;
  OK:=true;
  for j:=1 to length(ToFindString) do begin
   if InString[i+j-1]<>ToFindString[j] then begin
    OK:=false;
    break;
   end;
  end;
  if OK then begin
   result:=i;
   exit;
  end;
  inc(i);
 end;
end;

{$ifndef BESENSingleStringType}
function BESENANSIPosChar(ToFindChar:TBESENCHAR;const InString:TBESENANSISTRING):longint;
var i:longint;
begin
 result:=0;
 for i:=1 to length(InString) do begin
  if InString[i]=ToFindChar then begin
   result:=i;
   break;
  end;
 end;
end;

function BESENANSIPos(const ToFindString,InString:TBESENANSISTRING):longint;
var i,j,l:longint;
    OK:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InString) do begin
  l:=i+length(ToFindString)-1;
  if l>length(InString) then begin
   exit;
  end;
  OK:=true;
  for j:=1 to length(ToFindString) do begin
   if InString[i+j-1]<>ToFindString[j] then begin
    OK:=false;
    break;
   end;
  end;
  if OK then begin
   result:=i;
   exit;
  end;
  inc(i);
 end;
end;

function BESENANSITrim(const InputString:TBESENANSISTRING):TBESENANSISTRING;
var Counter,FromHere,ToHere:longint;
begin
 FromHere:=1;
 ToHere:=length(InputString);
 for Counter:=1 to length(InputString) do begin
  if not (InputString[Counter] in [#0..#32]) then begin
   FromHere:=Counter;
   break;
  end;
 end;
 for Counter:=length(InputString) downto FromHere do begin
  if not (InputString[Counter] in [#0..#32]) then begin
   ToHere:=Counter;
   break;
  end;
 end;
 result:=copy(InputString,FromHere,(ToHere-FromHere)+1);
end;

function BESENANSIUpperCase(const InputString:TBESENANSISTRING):TBESENANSISTRING;
var i:longint;
begin
 result:=InputString;
 for i:=1 to length(result) do begin
  if result[i] in ['a'..'z'] then begin
   inc(byte(AnsiChar(result[i])),byte(AnsiChar('A'))-byte(AnsiChar('a')));
  end;
 end;
end;
{$endif}

function BESENStringCompare(const s1,s2:TBESENString):longint; {$ifdef caninline}inline;{$else}{$ifdef UseRegister}register;{$endif}{$endif}
const BoolToSign:array[boolean] of longint=(1,-1);
var i,j:longint;
begin
 if pointer(s1)=pointer(s2) then begin
  result:=0;
  exit;
 end;
 j:=length(s1);
 if length(s2)<j then begin
  j:=length(s2);
 end;
 for i:=1 to j do begin
  result:=longint(word(s1[i]))-longint(word(s2[i]));
  if result<>0 then begin
   result:=BoolToSign[result<0];
   exit;
  end;
 end;
 result:=length(s1)-length(s2);
 if result<>0 then begin
  result:=BoolToSign[result<0];
 end;
end;

{$ifdef cpu64}
function BESENStringEquals(const s1,s2:TBESENString):boolean; {$ifdef caninline}inline;{$else}{$ifdef UseRegister}register;{$endif}{$endif}
begin
 result:=s1=s2;
end;
{$else}
{$ifdef cpu386}
procedure BESENStringEqualsAlignFiller; assembler; register;
asm
 nop;
end;

function BESENStringEquals(const s1,s2:TBESENString):boolean; assembler; register;
asm
 push ebx
 cmp eax,edx
 je @Match
 test eax,eax
 jz @CheckNullEAX
 test edx,edx
 jz @CheckNullEDX
 mov ecx,dword ptr [eax-4]
 cmp ecx,dword ptr [edx-4]
 jne @Mismatch
 sub ecx,8
 jl @Small
 mov ebx,dword ptr [eax]
 cmp ebx,dword ptr [edx]
 jne @Mismatch
 lea ebx,dword ptr [eax+ecx]
 add edx,ecx
 mov eax,dword ptr [ebx]
 cmp eax,dword ptr [edx]
 jne @Mismatch
 mov eax,dword ptr [ebx+4]
 cmp eax,dword ptr [edx+4]
 jne @Mismatch
 sub ecx,4
 jle @Match
 neg ecx
 add ecx,ebx
 and ecx,-4
 sub ecx,ebx
@LargeLoop:
 mov eax,dword ptr [ebx+ecx]
 cmp eax,dword ptr [edx+ecx]
 jne @Mismatch
 mov eax,dword ptr [ebx+ecx+4]
 cmp eax,dword ptr [edx+ecx+4]
 jne @Mismatch
 add ecx,8
 jl @LargeLoop
@Match:
 mov eax,1
 jmp @Done
@Small:
 add ecx,8
 jle @Match
@SmallLoop:
 mov bx,word ptr [eax]
 cmp bx,word ptr [edx]
 jne @Mismatch
 add eax,2
 add edx,2
 sub ecx,2
 jnz @SmallLoop
 jmp @Match
@CheckNullEAX:
 cmp dword ptr [edx-4],eax
 je @Match
 jmp @Mismatch
@CheckNullEDX:
 cmp dword ptr [eax-4],edx
 je @Match
@Mismatch:
 xor eax,eax
@Done:
 pop ebx
end;
(* is a plain asm implementation of:
function BESENStringEquals(const s1,s2:TBESENString):boolean; {$ifdef UseRegister}register;{$endif}
var l:TBESENINT32;
    x,y:PWideChar;
begin
 result:=false;
 x:=PWideChar(s1);
 y:=PWideChar(s2);
 if x=y then begin
  result:=true;
  exit;
 end;
 if (ptruint(x) and ptruint(y))=0 then begin
  result:=(assigned(x) and (PBESENUINT32(pointer(ptruint(ptruint(x)-sizeof(longword))))^=0)) or
          (assigned(y) and (PBESENUINT32(pointer(ptruint(ptruint(y)-sizeof(longword))))^=0));
  exit;
 end;
 l:=PBESENINT32(pointer(ptruint(ptruint(x)-sizeof(longword))))^;
 if l<>PBESENINT32(pointer(ptruint(ptruint(y)-sizeof(longword))))^ then begin
  exit;
 end;
 if l<8 then begin
  while l>0 do begin
   if x^<>y^ then begin
    exit;
   end;
   inc(x);
   inc(y);
   dec(l,2);
  end;
 end else begin
  if (PBESENUINT32(x)^<>PBESENUINT32(y)^) then begin
   exit;
  end;
  dec(l,8);
  inc(ptrint(x),l);
  inc(ptrint(y),l);
  if PBESENUINT32Array(x)^[0]<>PBESENUINT32Array(y)^[0] then begin
   exit;
  end;
  if PBESENUINT32Array(x)^[1]<>PBESENUINT32Array(y)^[1] then begin
   exit;
  end;
  dec(l,4);
  if l>0 then begin
   l:=(((-l)+ptrint(x)) and (-4))-ptrint(x);
   inc(ptrint(x),l);
   inc(ptrint(y),l);
   repeat
    if PBESENUINT32Array(x)^[0]<>PBESENUINT32Array(y)^[0] then begin
     exit;
    end;
    if PBESENUINT32Array(x)^[1]<>PBESENUINT32Array(y)^[1] then begin
     exit;
    end;
    inc(PBESENUINT32(x),2);
    inc(PBESENUINT32(y));
    inc(l,8);
   until l>=0;
  end;
 end;
 result:=true;
end;
*)
{$else}
function BESENStringEquals(const s1,s2:TBESENString):boolean; {$ifdef caninline}inline;{$else}{$ifdef UseRegister}register;{$endif}{$endif}
begin
 result:=s1=s2;
end;
{$endif}
{$endif}

{$ifdef BESENSingleStringType}
function BESENGetFileContent(fn:TBESENSTRING):TBESENSTRING;
var sl:TStringList;
begin
 result:='';
 sl:=TStringList.Create;
 try
  sl.LoadFromFile(fn);
  result:=sl.Text;
 finally
  sl.Free;
 end;
end;
{$else}
function BESENGetFileContent(fn:TBESENANSISTRING):TBESENANSISTRING;
var fm:byte;
    f:file;
begin
 result:='';
 fm:=filemode;
 filemode:=0;
 assignfile(f,String(fn));
 {$i-}reset(f,1);{$i+};
 if ioresult=0 then begin
  SetLength(result,filesize(f));
  if length(result)>0 then begin
   {$i-}blockread(f,result[1],length(result));{$i+}
   if ioresult<>0 then begin
    {$i-}closefile(f);{$i+}
    filemode:=fm;
    exit;
   end;
  end;
  {$i-}closefile(f);{$i+}
 end;
 filemode:=fm;
end;

function BESENConvertToUTF8(s:TBESENANSISTRING):TBESENANSISTRING;
var s2:TBESENANSISTRING;
    i,j,k:longint;
begin
 if (length(s)>=3) and (s[1]=#$ef) and (s[2]=#$bb) and (s[3]=#$bf) then begin
  // UTF8
  result:=copy(s,4,length(s)-3);
 end else if (length(s)>=4) and (s[1]=#$00) and (s[2]=#$00) and (s[3]=#$fe) and (s[4]=#$ff) then begin
  // UTF32 big endian
  s:=copy(s,5,length(s)-4);
  s2:=s;
  SetLength(s2,(length(s)+3) and not 3);
  for i:=1 to length(s) do begin
   j:=i shr 2;
   k:=i and 3;
   s2[(j shl 2)+(4-k)]:=s[i];
  end;
  result:=BESENEncodeString(s,UTF_32,UTF_8);
 end else if (length(s)>=4) and (s[1]=#$ff) and (s[2]=#$fe) and (s[3]=#$00) and (s[4]=#$00) then begin
  // UTF32 little endian
  result:=BESENEncodeString(copy(s,5,length(s)-4),UTF_32,UTF_8);
 end else if (length(s)>=2) and (s[1]=#$fe) and (s[2]=#$ff) then begin
  // UTF16 big endian
  s:=copy(s,3,length(s)-2);
  s2:=s;
  SetLength(s2,(length(s)+1) and not 1);
  for i:=1 to length(s) do begin
   j:=i shr 1;
   k:=i and 1;
   s2[(j shl 1)+(2-k)]:=s[i];
  end;
  result:=BESENEncodeString(s,UTF_16,UTF_8);
 end else if (length(s)>=2) and (s[1]=#$ff) and (s[2]=#$fe) then begin
  // UTF16 little endian
  result:=BESENEncodeString(copy(s,3,length(s)-2),UTF_16,UTF_8);
 end else if BESENIsUTF8(s) then begin
  // UTF8 without byte order mark or plain US-ASCII
  result:=s;
 end else begin
  // Without any unicode byte order mark
  result:=BESENEncodeString(s,BESENLocaleCharset,UTF_8);
 end;
end;

const Base64Table:array[0..63] of TBESENCHAR='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

var Base64DecoderTable:array[TBESENCHAR] of byte;

procedure InitBase64;
var i:longint;
begin
 for i:=0 to 63 do begin
  Base64DecoderTable[Base64Table[i]]:=i;
 end;
end;

function BESENDecodeBase64(s:TBESENANSISTRING):TBESENANSISTRING;
var i,j,l:longint;
    c:longword;
begin
 SetLength(result,((length(s)*3) shr 2)+3);
{while (length(s) and 3)<>0 do begin
  s:=s+'=';
 end;}
 l:=0;
 i:=1;
 while i<=length(s) do begin
  c:=0;
  for j:=1 to 4 do begin
   if i<=length(s) then begin
    case s[i] of
     'A'..'Z','a'..'z','0'..'9','+','/':begin
      c:=c or (Base64DecoderTable[s[i]] shl (24-((j shl 2)+(j shl 1))));
     end;
     '=':begin
      c:=(c and $00ffffff) or (((c shr 24)+1) shl 24);
     end;
     else begin
      c:=c or $f0000000;
      break;
     end;
    end;
   end else begin
    c:=(c and $00ffffff) or (((c shr 24)+1) shl 24);
   end;
   inc(i);
  end;
  if (c shr 24)<3 then begin
   inc(l);
   result[l]:=ansichar(byte((c shr 16) and $ff));
   if (c shr 24)<2 then begin
    inc(l);
    result[l]:=ansichar(byte((c shr 8) and $ff));
    if (c shr 24)<1 then begin
     inc(l);
     result[l]:=ansichar(byte(c and $ff));
    end;
   end;
  end else begin
   break;
  end;
 end;
 SetLength(result,l);
end;

function BESENEncodeBase64(s:TBESENANSISTRING):TBESENANSISTRING;
var i,l:longint;
    c:longword;
begin
 if length(s)=0 then begin
  result:='';
 end else begin
  SetLength(result,((length(s)*4) div 3)+4);
  l:=1;
  i:=1;
  while (i+2)<=length(s) do begin
   c:=(byte(s[i]) shl 16) or (byte(s[i+1]) shl 8) or byte(s[i+2]);
   result[l]:=Base64Table[(c shr 18) and $3f];
   result[l+1]:=Base64Table[(c shr 12) and $3f];
   result[l+2]:=Base64Table[(c shr 6) and $3f];
   result[l+3]:=Base64Table[c and $3f];
   inc(i,3);
   inc(l,4);
  end;
  if (i+1)<=length(s) then begin
   c:=(byte(s[i]) shl 16) or (byte(s[i+1]) shl 8);
   result[l]:=Base64Table[(c shr 18) and $3f];
   result[l+1]:=Base64Table[(c shr 12) and $3f];
   result[l+2]:=Base64Table[(c shr 6) and $3f];
   result[l+3]:='=';
   inc(l,4);
  end else if i<=length(s) then begin
   c:=byte(s[i]) shl 16;
   result[l]:=Base64Table[(c shr 18) and $3f];
   result[l+1]:=Base64Table[(c shr 12) and $3f];
   result[l+2]:='=';
   result[l+3]:='=';
   inc(l,4);
  end;
  if l>1 then begin
   SetLength(result,l-1);
  end else begin
   result:=BESENAnsiTrim(result);
  end;
 end;
end;

function BESENDequote(s:TBESENANSISTRING):TBESENANSISTRING;
const hexa:array[1..$F] of TBESENCHAR='123456789ABCDEF';
var p:longint;
    encode:TBESENANSISTRING;
begin
 if s='' then begin
  result:=#13#10;
 end else begin
  result:='';
  if s[length(s)]='=' then begin
   SetLength(s,Length(s)-1)
  end else begin
   if (length(s)>=3) and (s[length(s)-2]<>'=') then begin
    s:=s+#13#10;
   end;
  end;
  p:=BESENANSIPosChar('=',s);
  while p>0 do begin
   encode:=ansichar(byte((pos(s[p+1],hexa) shl 4) or pos(s[p+2],hexa)));
   if encode=#0 then begin
    encode:=#13#10;
   end;
   result:=result+copy(s,1,p-1)+encode;
   delete(s,1,p+2);
   p:=BESENANSIPosChar('=',s);
  end;
  result:=result+s;
  p:=BESENANSIPosChar('_',result);
  while p>0 do begin
   result[p]:=' ';
   p:=BESENANSIPosChar('_',result);
  end;
 end;
end;

const UnknownChar=#254;

function GetCharsetTable(CharSet:TBESENCharset):TBESENCharsetTable;
begin
 case CharSet of
  ISO_8859_1:result:=BESENCharISO_8859_1;
  ISO_8859_2:result:=BESENCharISO_8859_2;
  ISO_8859_3:result:=BESENCharISO_8859_3;
  ISO_8859_4:result:=BESENCharISO_8859_4;
  ISO_8859_5:result:=BESENCharISO_8859_5;
  ISO_8859_6:result:=BESENCharISO_8859_6;
  ISO_8859_7:result:=BESENCharISO_8859_7;
  ISO_8859_8:result:=BESENCharISO_8859_8;
  ISO_8859_9:result:=BESENCharISO_8859_9;
  ISO_8859_10:result:=BESENCharISO_8859_10;
  CP1250:result:=BESENCharCP_1250;
  CP1251:result:=BESENCharCP_1251;
  CP1252:result:=BESENCharCP_1252;
  CP1253:result:=BESENCharCP_1253;
  CP1254:result:=BESENCharCP_1254;
  CP1255:result:=BESENCharCP_1255;
  CP1256:result:=BESENCharCP_1256;
  CP1257:result:=BESENCharCP_1257;
  CP1258:result:=BESENCharCP_1258;
  KOI8_R:result:=BESENCharKOI8_R;
 end;
end;

function BESENIsUTF8(const s:TBESENANSISTRING):boolean;
var i,j:longint;
    b:byte;
begin
 j:=0;
 i:=1;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   inc(i);
   inc(j);
  end else if ((i+1)<length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   inc(i,2);
   inc(j);
  end else if ((i+2)<length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   inc(i,3);
   inc(j);
  end else if ((i+3)<length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   inc(i,4);
   inc(j);
{$ifndef strictutf8}
  end else if ((i+4)<length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   inc(i,5);
   inc(j);
  end else if ((i+5)<length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   inc(i,6);
   inc(j);
{$endif}
  end else begin
   i:=0;
   j:=0;
   break;
  end;
 end;
 result:=(i>0) and (j>0);
end;

function BESENEncodeString(Value:TBESENANSISTRING;CharFrom:TBESENCharset;CharTo:TBESENCharset):TBESENANSISTRING;
 function UTF16ToUTF32(const Value:TBESENANSISTRING):TBESENANSISTRING;
 var i,j:longint;
     s:widestring;
     Buffer:array of longword;
     v:longword;
     w:word;
 begin
  SetLength(s,(length(Value)+1) and not 1);
  i:=1;
  j:=0;
  while (i+1)<=length(Value) do begin
   inc(j);
   s[j]:=widechar(word((word(byte(Value[i])) shl 8) or byte(Value[i+1])));
   inc(i,2);
  end;
  SetLength(s,j);
  SetLength(Buffer,length(s));
  j:=0;
  i:=1;
  while i<=length(s) do begin
   w:=word(s[i]);
   if (w<=$d7ff) or (w>=$e000) then begin
    Buffer[j]:=w;
    inc(j);
    inc(i);
   end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
    Buffer[j]:=(TBESENUTF32CHAR(TBESENUTF32CHAR(w and $3ff) shl 10) or TBESENUTF32CHAR(word(s[i+1]) and $3ff))+$10000;
    inc(j);
    inc(i,2);
   end else begin
    Buffer[j]:=$fffd;
    inc(j);
    inc(i);
   end;
  end;
  SetLength(Buffer,j);
  SetLength(result,length(Buffer)*4);
  j:=1;
  for i:=0 to length(Buffer)-1 do begin
   v:=Buffer[i];
   result[j]:=ansichar(byte((v shr 24) and $ff));
   inc(j);
   result[j]:=ansichar(byte((v shr 16) and $ff));
   inc(j);
   result[j]:=ansichar(byte((v shr 8) and $ff));
   inc(j);
   result[j]:=ansichar(byte(v and $ff));
   inc(j);
  end;
  SetLength(Buffer,0);
 end;
 function UTF32ToUTF16(const Value:TBESENANSISTRING):TBESENANSISTRING;
 var i,j:longint;
     w,u4c:TBESENUTF32CHAR;
     Buffer:widestring;
     s:array of longword;
 begin
  SetLength(s,(length(Value)+3) shr 2);
  for i:=0 to length(s)-1 do begin
   j:=(i shl 2)+1;
   u4c:=0;
   if j<=length(Value) then begin
    u4c:=u4c or (byte(Value[j]) shl 24);
    inc(j);
    if j<=length(Value) then begin
     u4c:=u4c or (byte(Value[j]) shl 16);
     inc(j);
     if j<=length(Value) then begin
      u4c:=u4c or (byte(Value[j]) shl 8);
      inc(j);
      if j<=length(Value) then begin
       u4c:=u4c or byte(Value[j]);
      end;
     end;
    end;
   end;
   s[i]:=u4c;
  end;
  SetLength(Buffer,length(s)*2);
  j:=0;
  for i:=0 to length(s)-1 do begin
   w:=s[i];
   if w<=$d7ff then begin
    inc(j);
    Buffer[j]:=widechar(word(w));
   end else if w<=$dfff then begin
    inc(j);
    Buffer[j]:=#$fffd;
   end else if w<=$fffd then begin
    inc(j);
    Buffer[j]:=widechar(word(w));
   end else if w<=$ffff then begin
    inc(j);
    Buffer[j]:=#$fffd;
   end else if w<=$10ffff then begin
    dec(w,$10000);
    inc(j);
    Buffer[j]:=widechar(word((w shr 10) or $d800));
    inc(j);
    Buffer[j]:=widechar(word((w and $3ff) or $dc00));
   end else begin
    inc(j);
    Buffer[j]:=#$fffd;
   end;
  end;
  SetLength(Buffer,j);
  SetLength(result,length(Buffer)*2);
  j:=1;
  for i:=1 to length(Buffer) do begin
   w:=word(Buffer[i]);
   result[j]:=ansichar(byte((w shr 8) and $ff));
   inc(j);
   result[j]:=ansichar(byte(w and $ff));
   inc(j);
  end;
  SetLength(Buffer,0);
 end;
 function UTF8ToUTF32(Value:TBESENANSISTRING):TBESENANSISTRING;
 var i,j:longint;
     b:byte;
     Buffer:array of longword;
     v:longword;
 begin
  j:=0;
  i:=1;
  while i<=length(Value) do begin
   b:=byte(Value[i]);
   if (b and $80)=0 then begin
    inc(i);
    inc(j);
   end else if ((i+1)<=length(Value)) and ((b and $e0)=$c0) and ((byte(Value[i+1]) and $c0)=$80) then begin
    inc(i,2);
    inc(j);
   end else if ((i+2)<=length(Value)) and ((b and $f0)=$e0) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) then begin
    inc(i,3);
    inc(j);
   end else if ((i+3)<=length(Value)) and ((b and $f8)=$f0) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) and ((byte(Value[i+3]) and $c0)=$80) then begin
    inc(i,4);
    inc(j);
{$ifndef strictutf8}
   end else if ((i+4)<=length(Value)) and ((b and $fc)=$f8) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) and ((byte(Value[i+3]) and $c0)=$80) and ((byte(Value[i+4]) and $c0)=$80) then begin
    inc(i,5);
    inc(j);
   end else if ((i+5)<=length(Value)) and ((b and $fe)=$fc) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) and ((byte(Value[i+3]) and $c0)=$80) and ((byte(Value[i+4]) and $c0)=$80) and ((byte(Value[i+5]) and $c0)=$80) then begin
    inc(i,6);
    inc(j);
{$endif}
   end else begin
    inc(i);
    inc(j);
   end;
  end;
  SetLength(Buffer,j);
  if j=0 then begin
   exit;
  end;
  j:=0;
  i:=1;
  while i<=length(Value) do begin
   b:=byte(Value[i]);
   if (b and $80)=0 then begin
    Buffer[j]:=b;
    inc(i);
    inc(j);
   end else if ((i+1)<=length(Value)) and ((b and $e0)=$c0) and ((byte(Value[i+1]) and $c0)=$80) then begin
    Buffer[j]:=((byte(Value[i]) and $1f) shl 6) or (byte(Value[i+1]) and $3f);
    inc(i,2);
    inc(j);
   end else if ((i+2)<=length(Value)) and ((b and $f0)=$e0) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) then begin
    Buffer[j]:=((byte(Value[i]) and $0f) shl 12) or ((byte(Value[i+1]) and $3f) shl 6) or (byte(Value[i+2]) and $3f);
    inc(i,3);
    inc(j);
   end else if ((i+3)<=length(Value)) and ((b and $f8)=$f0) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) and ((byte(Value[i+3]) and $c0)=$80) then begin
    Buffer[j]:=((byte(Value[i]) and $07) shl 18) or ((byte(Value[i+1]) and $3f) shl 12) or ((byte(Value[i+2]) and $3f) shl 6) or (byte(Value[i+3]) and $3f);
    inc(i,4);
    inc(j);
{$ifndef strictutf8}
   end else if ((i+4)<=length(Value)) and ((b and $fc)=$f8) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) and ((byte(Value[i+3]) and $c0)=$80) and ((byte(Value[i+4]) and $c0)=$80) then begin
    Buffer[j]:=((byte(Value[i]) and $03) shl 24) or ((byte(Value[i+1]) and $3f) shl 18) or ((byte(Value[i+2]) and $3f) shl 12) or ((byte(Value[i+3]) and $3f) shl 6) or (byte(Value[i+4]) and $3f);
    inc(i,5);
    inc(j);
   end else if ((i+5)<=length(Value)) and ((b and $fe)=$fc) and ((byte(Value[i+1]) and $c0)=$80) and ((byte(Value[i+2]) and $c0)=$80) and ((byte(Value[i+3]) and $c0)=$80) and ((byte(Value[i+4]) and $c0)=$80) and ((byte(Value[i+5]) and $c0)=$80) then begin
    Buffer[j]:=((byte(Value[i]) and $01) shl 30) or ((byte(Value[i+1]) and $3f) shl 24) or ((byte(Value[i+2]) and $3f) shl 18) or ((byte(Value[i+3]) and $3f) shl 12) or ((byte(Value[i+4]) and $3f) shl 6) or (byte(Value[i+5]) and $3f);
    inc(i,6);
    inc(j);
{$endif}
   end else begin
    Buffer[j]:=$fffd;
    inc(i);
    inc(j);
   end;
  end;
  SetLength(result,length(Buffer)*4);
  j:=1;
  for i:=0 to length(Buffer)-1 do begin
   v:=Buffer[i];
   result[j]:=ansichar(byte((v shr 24) and $ff));
   inc(j);
   result[j]:=ansichar(byte((v shr 16) and $ff));
   inc(j);
   result[j]:=ansichar(byte((v shr 8) and $ff));
   inc(j);
   result[j]:=ansichar(byte(v and $ff));
   inc(j);
  end;
  SetLength(Buffer,0);
 end;
 function UTF32toUTF8(Value:TBESENANSISTRING):TBESENANSISTRING;
 var i,j:longint;
     u4c:longword;
     s:array of longword;
 begin
  SetLength(s,(length(Value)+3) shr 2);
  for i:=0 to length(s)-1 do begin
   j:=(i shl 2)+1;
   u4c:=0;
   if j<=length(Value) then begin
    u4c:=u4c or (byte(Value[j]) shl 24);
    inc(j);
    if j<=length(Value) then begin
     u4c:=u4c or (byte(Value[j]) shl 16);
     inc(j);
     if j<=length(Value) then begin
      u4c:=u4c or (byte(Value[j]) shl 8);
      inc(j);
      if j<=length(Value) then begin
       u4c:=u4c or byte(Value[j]);
      end;
     end;
    end;
   end;
   s[i]:=u4c;
  end;
  result:='';
  j:=0;
  for i:=0 to length(s)-1 do begin
   u4c:=s[i];
   if u4c<=$7f then begin
    inc(j);
   end else if u4c<=$7ff then begin
    inc(j,2);
   end else if u4c<=$ffff then begin
    inc(j,3);
   end else if u4c<=$1fffff then begin
    inc(j,4);
{$ifndef strictutf8}
   end else if u4c<=$3ffffff then begin
    inc(j,5);
   end else if u4c<=$7fffffff then begin
    inc(j,6);
{$endif}    
   end else begin
    inc(j,3);
   end;
  end;
  SetLength(result,j);
  j:=1;
  for i:=0 to length(s)-1 do begin
   u4c:=s[i];
   if u4c<=$7f then begin
    result[j]:=ansichar(byte(u4c));
    inc(j);
   end else if u4c<=$7ff then begin
    result[j]:=ansichar(byte($c0 or ((u4c shr 6) and $1f)));
    result[j+1]:=ansichar(byte($80 or (u4c and $3f)));
    inc(j,2);
   end else if u4c<=$ffff then begin
    result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
    result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
    result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
    inc(j,3);
   end else if u4c<=$1fffff then begin
    result[j]:=ansichar(byte($f0 or ((u4c shr 18) and $07)));
    result[j+1]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
    result[j+2]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
    result[j+3]:=ansichar(byte($80 or (u4c and $3f)));
    inc(j,4);
{$ifndef strictutf8}
   end else if u4c<=$3ffffff then begin
    result[j]:=ansichar(byte($f8 or ((u4c shr 24) and $03)));
    result[j+1]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
    result[j+2]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
    result[j+3]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
    result[j+4]:=ansichar(byte($80 or (u4c and $3f)));
    inc(j,5);
   end else if u4c<=$7fffffff then begin
    result[j]:=ansichar(byte($fc or ((u4c shr 30) and $01)));
    result[j+1]:=ansichar(byte($80 or ((u4c shr 24) and $3f)));
    result[j+2]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
    result[j+3]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
    result[j+4]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
    result[j+5]:=ansichar(byte($80 or (u4c and $3f)));
    inc(j,6);
{$endif}
   end else begin
    u4c:=$fffd;
    result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
    result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
    result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
    inc(j,3);
   end;
  end;
  SetLength(s,0);
 end;
 function UTF7toUCS2(Value:TBESENANSISTRING):TBESENANSISTRING;
 var i:longint;
     c:TBESENCHAR;
     s:TBESENANSISTRING;
 begin
  result:='';
  i:=1;
  while i<=length(Value) do begin
   c:=Value[i];
   inc(i);
   if c<>'+' then begin
    result:=result+#0+c;
   end else begin
    s:='';
    while i<=length(Value) do begin
     c:=Value[i];
     inc(i);
     if c='-' then begin
      break;
     end else if (c='=') or (BESENANSIPosChar(c,BESENBase64Chars)<1) then begin
      dec(i);
      break;
     end;
     s:=s+c;
    end;
    if s='' then begin
     s:='+';
    end else begin
     s:=BESENDecodeBase64(s);
    end;
    result:=result+s;
   end;
  end;
 end;
 function UCS2toUTF7(Value:TBESENANSISTRING):TBESENANSISTRING;
 var s:TBESENANSISTRING;
     c1,c2:TBESENCHAR;
     i,j:longint;
 begin
  result:='';
  i:=1;
  while i<=length(Value) do begin
   c2:=Value[i];
   if (i+1)<=length(Value) then begin
    c1:=Value[i+1];
   end else begin
    c1:=#0;
   end;
   inc(i,2);
   if (c2=#0) and (c1<#128) then begin
    if c1='+' then begin
     result:=result+'+-';
    end else begin
     result:=result+TBESENCHAR(c1);
    end;
   end else begin
    s:=c2+c1;
    while i<=length(Value) do begin
     c2:=Value[i];
     if (i+1)<=length(Value) then begin
      c1:=Value[i+1];
     end else begin
      c1:=#0;
     end;
     if c2=#0 then begin
      break;
     end else begin
      inc(i,2);
      s:=s+c2+c1;
     end;
    end;
    s:=BESENEncodeBase64(s);
    j:=BESENANSIPosChar('=',s);
    if j>0 then begin
     s:=copy(s,1,j-1);
    end;
    result:=result+'+'+s+'-';
   end;
  end;
 end;
var Unicode:word;
    i,j:longint;
    b:byte;
    c1,c2,c3,c4:TBESENCHAR;
    SourceTable,TargetTable:TBESENCharsetTable;
    FromByteCount,ToByteCount:byte;
begin
 if CharFrom=CharTo then begin
  result:=Value;
 end else begin
  SourceTable:=GetCharsetTable(CharFrom);
  TargetTable:=GetCharsetTable(CharTo);
  if CharFrom in [UCS_2,UTF_7] then begin
   FromByteCount:=2;
  end else if CharFrom in [UCS_4,UTF_32,UTF_16,UTF_8] then begin
   FromByteCount:=4;
  end else begin
   FromByteCount:=1;
  end;
  if CharTo in [UCS_2,UTF_7] then begin
   ToByteCount:=2;
  end else if CharTo in [UCS_4,UTF_32,UTF_16,UTF_8] then begin
   ToByteCount:=4;
  end else begin
   ToByteCount:=1;
  end;
  case CharFrom of
   UTF_7:begin
    Value:=UTF7toUCS2(Value);
   end;
   UTF_8:begin
    Value:=UTF8ToUTF32(Value);
   end;
   UTF_16:begin
    Value:=UTF16ToUTF32(Value);
   end;
  end;
  c1:=#0;
  c2:=#0;
  c3:=#0;
  c4:=#0;
  result:='';
  i:=1;
  while i<=length(Value) do begin
   case FromByteCount of
    1:begin
     c1:=Value[i];
     if c1>#127 then begin
      Unicode:=SourceTable[byte(c1)];
      c1:=ansichar(byte(Unicode and $ff));
      c2:=ansichar(byte(Unicode shr 8));
     end;
     inc(i);
    end;
    2:begin
     c2:=Value[i];
     if (i+1)<=length(Value) then begin
      c1:=Value[i+1];
     end else begin
      c1:=#0;
     end;
     inc(i,2);
    end;
    3:begin
     c3:=Value[i];
     if (i+1)<=length(Value) then begin
      c2:=Value[i+1];
     end else begin
      c2:=#0;
     end;
     if (i+2)<=length(Value) then begin
      c1:=Value[i+2];
     end else begin
      c1:=#0;
     end;
     inc(i,3);
    end;
    4:begin
     c4:=Value[i];
     if (i+1)<=length(Value) then begin
      c3:=Value[i+1];
     end else begin
      c3:=#0;
     end;
     if (i+2)<=length(Value) then begin
      c2:=Value[i+2];
     end else begin
      c2:=#0;
     end;
     if (i+3)<=length(Value) then begin
      c1:=Value[i+3];
     end else begin
      c1:=#0;
     end;
     inc(i,4);
    end;
   end;
   Unicode:=(byte(c2) shl 8) or byte(c1);
   if ToByteCount=1 then begin
    if (c3<>#0) or (c4<>#0) then begin
     c1:=UnknownChar;
     c2:=#0;
     c3:=#0;
     c4:=#0;
    end else begin
     if Unicode>127 then begin
      b:=ord(UnknownChar);
      for j:=128 to 255 do begin
       if TargetTable[j]=Unicode then begin
        b:=j;
        break;
       end;
      end;
      c1:=ansichar(byte(b));
      c2:=#0;
     end else begin
      c1:=ansichar(byte(Unicode and $ff));
     end;
    end;
   end;
   case ToByteCount of
    1:begin
     result:=result+c1;
    end;
    2:begin
     result:=result+c2+c1;
    end;
    3:begin
     result:=result+c3+c2+c1;
    end;
    4:begin
     result:=result+c4+c3+c2+c1;
    end;
   end;
  end;
  case CharTo of
   UTF_7:begin
    result:=UCS2toUTF7(result);
   end;
   UTF_8:begin
    result:=UTF32toUTF8(result);
   end;
   UTF_16:begin
    result:=UTF32ToUTF16(result);
   end;
  end;
 end;
end;

function BESENGetCodePage(Value:TBESENANSISTRING):TBESENCharset;
begin
 Value:=BESENANSIUpperCase(Value);
 if BESENANSIPos('ISO-8859-10',Value)>0 then begin
  result:=ISO_8859_10;
 end else if BESENANSIPos('ISO-8859-1',Value)>0 then begin
  result:=ISO_8859_1;
 end else if BESENANSIPos('ISO-8859-2',Value)>0 then begin
  result:=ISO_8859_2;
 end else if BESENANSIPos('ISO-8859-3',Value)>0 then begin
  result:=ISO_8859_3;
 end else if BESENANSIPos('ISO-8859-4',Value)>0 then begin
  result:=ISO_8859_4;
 end else if BESENANSIPos('ISO-8859-5',Value)>0 then begin
  result:=ISO_8859_5;
 end else if BESENANSIPos('ISO-8859-6',Value)>0 then begin
  result:=ISO_8859_6;
 end else if BESENANSIPos('ISO-8859-7',Value)>0 then begin
  result:=ISO_8859_7;
 end else if BESENANSIPos('ISO-8859-8',Value)>0 then begin
  result:=ISO_8859_8;
 end else if BESENANSIPos('ISO-8859-9',Value)>0 then begin
  result:=ISO_8859_9;
 end else if (BESENANSIPos('WINDOWS-1250',Value)>0) or (BESENANSIPos('X-CP1250',Value)>0) then begin
  result:=CP1250;
 end else if (BESENANSIPos('WINDOWS-1251',Value)>0) or (BESENANSIPos('X-CP1251',Value)>0) then begin
  result:=CP1251;
 end else if (BESENANSIPos('WINDOWS-1252',Value)>0) or (BESENANSIPos('X-CP1252',Value)>0) then begin
  result:=CP1252;
 end else if (BESENANSIPos('WINDOWS-1253',Value)>0) or (BESENANSIPos('X-CP1253',Value)>0) then begin
  result:=CP1253;
 end else if (BESENANSIPos('WINDOWS-1254',Value)>0) or (BESENANSIPos('X-CP1254',Value)>0) then begin
  result:=CP1254;
 end else if (BESENANSIPos('WINDOWS-1255',Value)>0) or (BESENANSIPos('X-CP1255',Value)>0) then begin
  result:=CP1255;
 end else if (BESENANSIPos('WINDOWS-1256',Value)>0) or (BESENANSIPos('X-CP1256',Value)>0) then begin
  result:=CP1256;
 end else if (BESENANSIPos('WINDOWS-1257',Value)>0) or (BESENANSIPos('X-CP1257',Value)>0) then begin
  result:=CP1257;
 end else if (BESENANSIPos('WINDOWS-1258',Value)>0) or (BESENANSIPos('X-CP1258',Value)>0) then begin
  result:=CP1258;
 end else if BESENANSIPos('KOI8-R',Value)>0 then begin
  result:=KOI8_R;
 end else if BESENANSIPos('UTF-7',Value)>0 then begin
  result:=UTF_7;
 end else if BESENANSIPos('UTF-8',Value)>0 then begin
  result:=UTF_8;
 end else if BESENANSIPos('UTF-16',Value)>0 then begin
  result:=UTF_16;
 end else if BESENANSIPos('UTF-32',Value)>0 then begin
  result:=UTF_32;
 end else if BESENANSIPos('UCS-4',Value)>0 then begin
  result:=UCS_4;
 end else if BESENANSIPos('UCS-2',Value)>0 then begin
  result:=UCS_2;
 end else if BESENANSIPos('UNICODE',Value)>0 then begin
  result:=UCS_2;
 end else begin
  result:=ISO_8859_1;
 end;
end;

function BESENGetCodePageID(Value:TBESENCharset):TBESENANSISTRING;
begin
 case Value of
  ISO_8859_2:result:='ISO-8859-2';
  ISO_8859_3:result:='ISO-8859-3';
  ISO_8859_4:result:='ISO-8859-4';
  ISO_8859_5:result:='ISO-8859-5';
  ISO_8859_6:result:='ISO-8859-6';
  ISO_8859_7:result:='ISO-8859-7';
  ISO_8859_8:result:='ISO-8859-8';
  ISO_8859_9:result:='ISO-8859-9';
  ISO_8859_10:result:='ISO-8859-10';
  CP1250:result:='WINDOWS-1250';
  CP1251:result:='WINDOWS-1251';
  CP1252:result:='WINDOWS-1252';
  CP1253:result:='WINDOWS-1253';
  CP1254:result:='WINDOWS-1254';
  CP1255:result:='WINDOWS-1255';
  CP1256:result:='WINDOWS-1256';
  CP1257:result:='WINDOWS-1257';
  CP1258:result:='WINDOWS-1258';
  KOI8_R:result:='KOI8-R';
  UCS_2:result:='Unicode-1-1-UCS-2';
  UCS_4:result:='Unicode-1-1-UCS-4';
  UTF_32:result:='UTF-32';
  UTF_16:result:='UTF-16';
  UTF_8:result:='UTF-8';
  UTF_7:result:='UTF-7';
  else result:='ISO-8859-1';
 end;
end;

function BESENDoNeedEncoding(Value:TBESENANSISTRING):boolean;
var i:longint;
begin
 result:=false;
 for i:=1 to length(Value) do begin
  if ord(Value[i])>127 then begin
   result:=true;
   break;
  end;
 end;
end;

function BESENFindIdealCoding(Value:TBESENANSISTRING;CharFrom:TBESENCharset;CharTo:TBESENCharsetSet):TBESENCharset;
var cs:TBESENCharset;
    i,j,k:longint;
    s,t:TBESENANSISTRING;
begin
 result:=ISO_8859_1;
 s:='';
 for i:=1 to length(Value) do begin
  if ord(Value[i])>127 then begin
   s:=s+Value[i];
  end;
 end;
 j:=128;
 for cs:=low(TBESENCharset) to high(TBESENCharset) do begin
  if cs in CharTo then begin
   t:=BESENEncodeString(s,CharFrom,cs);
   k:=0;
   for i:=1 to length(t) do begin
    if t[i]=UnknownChar then begin
     inc(k);
    end;
   end;
   if k<j then begin
    j:=k;
    result:=cs;
    if k=0 then begin
     break;
    end;
   end;
  end;
 end;
end;

function BESENISOToUTF8(s:TBESENANSISTRING):TBESENANSISTRING;
var q,us,e:TBESENANSISTRING;
    encode:TBESENCHAR;
    p1,p2,p3:longint;
    cs:TBESENCharset;
begin
 result:='';
 us:=BESENANSIUpperCase(s);
 p1:=BESENANSIPos('=?ISO',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=BESENANSIPosChar('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  e:=copy(q,1,p2-1);
  cs:=BESENGetCodePage(e);
  encode:=TBESENCHAR(upcase(TBESENCHAR(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=BESENANSIPos('?=',q);
  if p3=0 then begin
   break;
  end;
  SetLength(q,p3-1);
  if encode='B' then begin
   q:=BESENDecodeBase64(q);
  end else if encode='Q' then begin
   q:=BESENDequote(q+'=');
  end else begin
   break;
  end;
  q:=BESENEncodeString(q,cs,UTF_8);
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=BESENANSIPos('=?ISO',us);
 end;
 p1:=BESENANSIPos('=?UTF-7',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=BESENANSIPosChar('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  encode:=TBESENCHAR(upcase(TBESENCHAR(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=BESENANSIPos('?=',q);
  if p3=0 then begin
   break;
  end;
  SetLength(q,p3-1);
  if encode='B' then begin
   q:=BESENDecodeBase64(q);
  end else if encode='Q' then begin
   q:=BESENDequote(q+'=');
  end else begin
   break;
  end;
  q:=BESENEncodeString(s,UTF_7,UTF_8);
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=BESENANSIPos('=?UTF-7',us);
 end;
 p1:=BESENANSIPos('=?UTF-8',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=BESENANSIPosChar('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  encode:=TBESENCHAR(upcase(TBESENCHAR(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=BESENANSIPos('?=',q);
  if p3=0 then begin
   break;
  end;
  SetLength(q,p3-1);
  if encode='B' then begin
   q:=BESENDecodeBase64(q);
  end else if encode='Q' then begin
   q:=BESENDequote(q+'=');
  end else begin
   break;
  end;
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=BESENANSIPos('=?UTF-8',us);
 end;
 p1:=BESENANSIPos('=?',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=BESENANSIPosChar('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  e:=copy(q,1,p2-1);
  cs:=BESENGetCodePage(e);
  if cs=ISO_8859_1 then begin
   result:=result+'=?';
   delete(s,1,p1);
   delete(us,1,p1);
   p1:=BESENANSIPos('=?',us);
   continue;
  end;
  encode:=TBESENCHAR(upcase(TBESENCHAR(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=BESENANSIPos('?=',q);
  if p3=0 then begin
   break;
  end;
  SetLength(q,p3-1);
  if encode='B' then begin
   q:=BESENDecodeBase64(q);
  end else if encode='Q' then begin
   q:=BESENDequote(q+'=');
  end else begin
   break;
  end;
  q:=BESENEncodeString(q,cs,UTF_8);
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=BESENANSIPos('=?',us);
 end;
 result:=result+BESENEncodeString(s,ISO_8859_1,UTF_8);
end;

function BESENUTF32Pos(const ToFindString,InString:TBESENUTF32STRING):longint;
var i,j:longint;
begin
 result:=-1;
 for i:=0 to length(InString)-length(ToFindString) do begin
  for j:=0 to length(ToFindString)-1 do begin
   if ToFindString[j]<>InString[i+j] then begin
    break;
   end;
   result:=i;
   exit;
  end;
 end;
end;

procedure BESENUTF32Delete(var s:TBESENUTF32STRING;Index,Len:longint);
var i,a,b:longint;
begin
 if Index>=length(s) then begin
  exit;
 end;
 if Len>length(s) then begin
  Len:=length(s);
 end;
 if (Index+Len)>=length(s) then begin
  Len:=length(s)-Index;
 end;
 a:=Index+Len;
 b:=Index;
 i:=length(s)-a;
 move(s[a],s[b],i*sizeof(TBESENUTF32CHAR));
 SetLength(s,length(s)-Len);
end;

function BESENUTF32Compare(const a,b:TBESENUTF32STRING):boolean; overload;
var i:longint;
begin
 if a=b then begin
  result:=true;
  exit;
 end;
 if length(a)<>length(b) then begin
  result:=false;
  exit;
 end;
 for i:=0 to length(a)-1 do begin
  if a[i]<>b[i] then begin
   result:=false;
   exit;
  end;
 end;
 result:=true;
end;

function BESENUTF32Compare(const a:TBESENUTF32STRING;const b:TBESENANSISTRING):boolean; overload;
var i:longint;
begin
 if length(a)<>length(b) then begin
  result:=false;
  exit;
 end;
 for i:=0 to length(a)-1 do begin
  if a[i]<>byte(b[i+1]) then begin
   result:=false;
   exit;
  end;
 end;
 result:=true;
end;

procedure BESENUTF32Clear(var a:TBESENUTF32STRING);
begin
 SetLength(a,0);
end;

procedure BESENBESENUTF32AddString(var a:TBESENUTF32STRING;const b:TBESENANSISTRING);
var i,j:longint;
begin
 j:=length(a);
 SetLength(a,j+length(b));
 for i:=1 to length(b) do begin
  a[j+i-1]:=byte(b[i]);
 end;
end;

procedure BESENBESENUTF32AddChar(var a:TBESENUTF32STRING;const b:TBESENUTF32CHAR); overload;
var j:longint;
begin
 j:=length(a);
 SetLength(a,j+1);
 a[j]:=b;
end;

procedure BESENBESENUTF32AddChar(var a:TBESENUTF32STRING;const b:TBESENCHAR); overload;
var j:longint;
begin
 j:=length(a);
 SetLength(a,j+1);
 a[j]:=byte(b);
end;

procedure BESENUTF32Add(var a:TBESENUTF32STRING;const b:TBESENUTF32STRING);
var i,j:longint;
begin
 j:=length(a);
 SetLength(a,j+length(b));
 for i:=0 to length(b)-1 do begin
  a[j+i]:=byte(b[i]);
 end;
end;

function BESENUTF32ToUTF8(const s:TBESENUTF32STRING):TBESENUTF8STRING;
var i,j:longint;
    u4c:TBESENUTF32CHAR;
begin
 result:='';
 j:=0;
 for i:=0 to length(s)-1 do begin
  u4c:=s[i];
  if u4c<=$7f then begin
   inc(j);
  end else if u4c<=$7ff then begin
   inc(j,2);
  end else if u4c<=$ffff then begin
   inc(j,3);
  end else if u4c<=$1fffff then begin
   inc(j,4);
{$ifndef strictutf8}
  end else if u4c<=$3ffffff then begin
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   inc(j,6);
{$endif}
  end else begin
   inc(j,3);
  end;
 end;
 SetLength(result,j);
 j:=1;
 for i:=0 to length(s)-1 do begin
  u4c:=s[i];
  if u4c<=$7f then begin
   result[j]:=ansichar(byte(u4c));
   inc(j);
  end else if u4c<=$7ff then begin
   result[j]:=ansichar(byte($c0 or ((u4c shr 6) and $1f)));
   result[j+1]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,2);
  end else if u4c<=$ffff then begin
   result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end else if u4c<=$1fffff then begin
   result[j]:=ansichar(byte($f0 or ((u4c shr 18) and $07)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+3]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,4);
{$ifndef strictutf8}
  end else if u4c<=$3ffffff then begin
   result[j]:=ansichar(byte($f8 or ((u4c shr 24) and $03)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+3]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+4]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   result[j]:=ansichar(byte($fc or ((u4c shr 30) and $01)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 24) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
   result[j+3]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+4]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+5]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,6);
{$endif}
  end else begin
   u4c:=$fffd;
   result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end;
 end;
end;

function BESENUTF8ToUTF32(const s:TBESENUTF8STRING):TBESENUTF32STRING;
var i,j:longint;
    b:byte;
begin
 j:=0;
 i:=1;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   inc(i);
   inc(j);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   inc(i,2);
   inc(j);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   inc(i,3);
   inc(j);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   inc(i,4);
   inc(j);
{$ifndef strictutf8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   inc(i,5);
   inc(j);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   inc(i,6);
   inc(j);
{$endif}   
  end else begin
   inc(i);
   inc(j);
  end;
 end;
 SetLength(result,j);
 if j=0 then begin
  exit;
 end;
 j:=0;
 i:=1;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   result[j]:=b;
   inc(i);
   inc(j);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   result[j]:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
   inc(i,2);
   inc(j);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   result[j]:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
   inc(i,3);
   inc(j);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   result[j]:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
   inc(i,4);
   inc(j);
{$ifndef strictutf8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   result[j]:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
   inc(i,5);
   inc(j);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   result[j]:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
   inc(i,6);
   inc(j);
{$endif}
  end else begin
   result[j]:=$fffd;
   inc(i);
   inc(j);
  end;
 end;
end;

function BESENUTF8ToUTF16(const s:TBESENUTF8STRING):TBESENUTF16STRING;
var i,j:longint;
    w:TBESENUTF32CHAR;
    b:byte;
begin
 result:='';
 i:=1;
 j:=0;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   w:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
   inc(i,4);
{$ifndef strictutf8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
   inc(i,6);
{$endif}
  end else begin
   w:=$fffd;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
  end else if w<=$dfff then begin
   inc(j);
  end else if w<=$fffd then begin
   inc(j);
  end else if w<=$ffff then begin
   inc(j);
  end else if w<=$10ffff then begin
   inc(j,2);
  end else begin
   inc(j);
  end;
 end;
 SetLength(result,j);
 i:=1;
 j:=0;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   w:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
   inc(i,4);
{$ifndef strictutf8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
   inc(i,6);
{$endif}
  end else begin
   w:=$fffd;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
   result[j]:=widechar(word(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=widechar(word(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=widechar(word((w shr 10) or $d800));
   inc(j);
   result[j]:=widechar(word((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
end;

function BESENUTF16ToUTF8(const s:TBESENUTF16STRING):TBESENUTF8STRING;
var i,j:longint;
    w:word;
    u4c:TBESENUTF32CHAR;
begin
 result:='';
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   u4c:=w;
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   u4c:=(TBESENUTF32CHAR(TBESENUTF32CHAR(w and $3ff) shl 10) or TBESENUTF32CHAR(word(s[i+1]) and $3ff))+$10000;
   inc(i,2);
  end else begin
   u4c:=$fffd;
   inc(i);
  end;
  if u4c<=$7f then begin
   inc(j);
  end else if u4c<=$7ff then begin
   inc(j,2);
  end else if u4c<=$ffff then begin
   inc(j,3);
  end else if u4c<=$1fffff then begin
   inc(j,4);
{$ifndef strictutf8}
  end else if u4c<=$3ffffff then begin
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   inc(j,6);
{$endif}
  end else begin
   inc(j,3);
  end;
 end;
 SetLength(result,j);
 j:=1;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   u4c:=w;
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   u4c:=(TBESENUTF32CHAR(TBESENUTF32CHAR(w and $3ff) shl 10) or TBESENUTF32CHAR(word(s[i+1]) and $3ff))+$10000;
   inc(i,2);
  end else begin
   u4c:=$fffd;
   inc(i);
  end;
  if u4c<=$7f then begin
   result[j]:=ansichar(byte(u4c));
   inc(j);
  end else if u4c<=$7ff then begin
   result[j]:=ansichar(byte($c0 or ((u4c shr 6) and $1f)));
   result[j+1]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,2);
  end else if u4c<=$ffff then begin
   result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end else if u4c<=$1fffff then begin
   result[j]:=ansichar(byte($f0 or ((u4c shr 18) and $07)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+3]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,4);
{$ifndef strictutf8}
  end else if u4c<=$3ffffff then begin
   result[j]:=ansichar(byte($f8 or ((u4c shr 24) and $03)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+3]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+4]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   result[j]:=ansichar(byte($fc or ((u4c shr 30) and $01)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 24) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
   result[j+3]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+4]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+5]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,6);
{$endif}
  end else begin
   u4c:=$fffd;
   result[j]:=ansichar(byte($e0 or (u4c shr 12)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end;
 end;
end;
{$endif}

function BESENUTF32ToUTF16(const s:TBESENUTF32STRING):TBESENUTF16STRING;
var i,j:longint;
    w:TBESENUTF32CHAR;
begin
 SetLength(result,length(s)*2);
 j:=0;
 for i:=0 to length(s)-1 do begin
  w:=s[i];
  if w<=$d7ff then begin
   inc(j);
   result[j]:=widechar(word(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=widechar(word(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=widechar(word((w shr 10) or $d800));
   inc(j);
   result[j]:=widechar(word((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
 SetLength(result,j);
end;

function BESENUTF32CHARToUTF16(w:TBESENUTF32CHAR):TBESENUTF16STRING;
begin
 if w<=$d7ff then begin
  result:=widechar(word(w));
 end else if w<=$dfff then begin
  result:=#$fffd;
 end else if w<=$fffd then begin
  result:=widechar(word(w));
 end else if w<=$ffff then begin
  result:=#$fffd;
 end else if w<=$10ffff then begin
  dec(w,$10000);
  result:=widechar(word((w shr 10) or $d800));
  result:=result+widechar(word((w and $3ff) or $dc00));
 end else begin
  result:=#$fffd;
 end;
end;

function BESENUTF16ToUTF32(const s:TBESENUTF16STRING):TBESENUTF32STRING;
var i,j:longint;
    w:word;
begin
 SetLength(result,length(s));
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   result[j]:=w;
   inc(j);
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   result[j]:=(TBESENUTF32CHAR(TBESENUTF32CHAR(w and $3ff) shl 10) or TBESENUTF32CHAR(word(s[i+1]) and $3ff))+$10000;
   inc(j);
   inc(i,2);
  end else begin
   result[j]:=$fffd;
   inc(j);
   inc(i);
  end;
 end;
 SetLength(result,j);
end;

{$ifndef BESENSingleStringType}
function BESENUTF32ToWIDESTRING(const s:TBESENUTF32STRING):widestring;
var i:longint;
begin
 SetLength(result,length(s));
 for i:=0 to length(s)-1 do begin
  result[i+1]:=widechar(word(s[i]));
 end;
end;

function BESENWIDESTRINGToUTF32(const s:widestring):TBESENUTF32STRING;
var i:longint;
begin
 SetLength(result,length(s));
 for i:=1 to length(s) do begin
  result[i-1]:=word(widechar(s[i]));
 end;
end;

function BESENUTF32ToSTRING(const s:TBESENUTF32STRING):TBESENANSISTRING;
var i:longint;
begin
 SetLength(result,length(s));
 for i:=0 to length(s)-1 do begin
  result[i+1]:=TBESENCHAR(byte(s[i]));
 end;
end;

function BESENSTRINGToUTF32(const s:TBESENANSISTRING):TBESENUTF32STRING;
var i:longint;
begin
 SetLength(result,length(s));
 for i:=1 to length(s) do begin
  result[i-1]:=byte(TBESENCHAR(s[i]));
 end;
end;

function BESENUTF32CharToUTF8(u4c:TBESENUTF32CHAR):TBESENUTF8STRING;
begin
 if u4c<=$7f then begin
  result:=ansichar(byte(u4c));
 end else if u4c<=$7ff then begin
  result:=ansichar(byte($c0 or ((u4c shr 6) and $1f)))+ansichar(byte($80 or (u4c and $3f)));
 end else if u4c<=$ffff then begin
  result:=ansichar(byte($e0 or ((u4c shr 12) and $0f)))+ansichar(byte($80 or ((u4c shr 6) and $3f)))+ansichar(byte($80 or (u4c and $3f)));
 end else if u4c<=$1fffff then begin
  result:=ansichar(byte($f0 or ((u4c shr 18) and $07)))+ansichar(byte($80 or ((u4c shr 12) and $3f)))+ansichar(byte($80 or ((u4c shr 6) and $3f)))+ansichar(byte($80 or (u4c and $3f)));
{$ifndef strictutf8}
 end else if u4c<=$3ffffff then begin
  result:=ansichar(byte($f8 or ((u4c shr 24) and $03)))+ansichar(byte($80 or ((u4c shr 18) and $3f)))+ansichar(byte($80 or ((u4c shr 12) and $3f)))+ansichar(byte($80 or ((u4c shr 6) and $3f)))+ansichar(byte($80 or (u4c and $3f)));
 end else if u4c<=$7fffffff then begin
  result:=ansichar(byte($fc or ((u4c shr 30) and $01)))+ansichar(byte($80 or ((u4c shr 24) and $3f)))+ansichar(byte($80 or ((u4c shr 18) and $3f)))+ansichar(byte($80 or ((u4c shr 12) and $3f)))+ansichar(byte($80 or ((u4c shr 6) and $3f)))+ansichar(byte($80 or (u4c and $3f)));
{$endif}
 end else begin
  u4c:=$fffd;
  result:=ansichar(byte($e0 or ((u4c shr 12) and $0f)))+ansichar(byte($80 or ((u4c shr 6) and $3f)))+ansichar(byte($80 or (u4c and $3f)));
 end;
end;                                                                                               

procedure BESENUTF8Inc(var s:TBESENUTF8STRING;var i:longint);
var b:byte;
begin
 if (i>=1) and (i<=length(s)) then begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   inc(i,4);
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   inc(i,6);
  end else begin
   inc(i);
  end;
 end;
end;

procedure BESENUTF8Dec(var s:TBESENUTF8STRING;var i:longint);
begin
 if (i>=1) and (i<=(length(s)+1)) then begin
  dec(i);
  while i>0 do begin
   if byte(s[i]) in [$80..$bf] then begin
    dec(i);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure BESENUTF8Delete(var s:TBESENUTF8STRING;i:longint);
begin
 if (i>=1) and (i<=length(s)) then begin
  delete(s,i,1);
  while (i>=1) and (i<=length(s)+1) do begin
   if byte(s[i]) in [$80..$bf] then begin
    delete(s,i,1);
   end else begin
    break;
   end;
  end;
 end;
end;

function BESENUTF8Length(const s:TBESENUTF8STRING):longint;
var b:byte;
    i,j:longint;
begin
 result:=0;
 i:=1;
 j:=length(s);
 while i<=j do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   inc(i,4);
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   inc(i,6);
  end else begin
   inc(i);
  end;
  inc(result);
 end;
end;

function BESENUTF8GetRawPos(const s:TBESENUTF8STRING;Index:longint):longint;
var b:byte;
    i,j,k:longint;
begin
 result:=0;
 k:=1;
 i:=1;
 j:=length(s);
 while i<=j do begin
  if k=Index then begin
   result:=i;
   exit;
  end;
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   inc(i,4);
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   inc(i,6);
  end else begin
   inc(i);
  end;
  inc(k);
 end;
end;

function BESENUTF8GetChar(var s:TBESENUTF8STRING;Index:longint):TBESENUTF32CHAR;
var b,i,j:longint;
begin
 i:=1;
 j:=0;
 while i<=Index do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   result:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   result:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   result:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   result:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
   inc(i,4);
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   result:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   result:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
   inc(i,6);
  end else begin
   result:=$fffd;
   inc(i);
  end;
  inc(j);
  if j=Index then begin
   exit;
  end;
 end;
 result:=0;
end;

function BESENUTF8GetRawChar(var s:TBESENUTF8STRING;i:longint):TBESENUTF32CHAR;
var b:byte;
begin
 if (i<1) or (i>length(s)) then begin
  result:=0;
  inc(i);
  exit;
 end;
 b:=byte(s[i]);
 if (b and $80)=0 then begin
  result:=b;
 end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
 end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
 end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
 end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
 end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
 end else begin
  result:=$fffd;
 end;
end;

function BESENUTF8GetRawCharAndInc(var s:TBESENUTF8STRING;var i:longint):TBESENUTF32CHAR;
var b:byte;
begin
 if (i<1) or (i>length(s)) then begin
  result:=0;
  inc(i);
  exit;
 end;
 b:=byte(s[i]);
 if (b and $80)=0 then begin
  result:=b;
  inc(i);
 end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
  inc(i,2);
 end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
  inc(i,3);
 end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
  inc(i,4);
{$ifndef strictutf8}
 end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
  inc(i,5);
 end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
  result:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
  inc(i,6);
{$endif}
 end else begin
  result:=$fffd;
  inc(i);
 end;
end;
{$endif}

function BESENUTF8Pos(ToFindString,InString:TBESENUTF8STRING):longint;
var i,j,l:longint;
    OK:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InString) do begin
  l:=i+length(ToFindString)-1;
  if l>length(InString) then begin
   exit;
  end;
  OK:=true;
  for j:=1 to length(ToFindString) do begin
   if InString[i+j-1]<>ToFindString[j] then begin
    OK:=false;
    break;
   end;
  end;
  if OK then begin
   result:=i;
   exit;
  end;
  BESENUTF8Inc(InString,i);
 end;
end;

function BESENLowercase(const s:TBESENString):TBESENString;
var t:TBESENUTF32STRING;
    i:longint;
begin
 t:=BESENUTF16ToUTF32(s);
 for i:=0 to length(t)-1 do begin
  t[i]:=BESENUnicodeToLower(t[i]);
 end;
 result:=BESENUTF32ToUTF16(t);
 SetLength(t,0);
end;

function BESENUppercase(const s:TBESENString):TBESENString;
var t:TBESENUTF32STRING;
    i:longint;
begin
 t:=BESENUTF16ToUTF32(s);
 for i:=0 to length(t)-1 do begin
  t[i]:=BESENUnicodeToUpper(t[i]);
 end;
 result:=BESENUTF32ToUTF16(t);
 SetLength(t,0);
end;

function BESENJSONStringQuote(const s:TBESENString):TBESENString;
var i:longint;
    c:word;
begin
 result:='"';
 i:=1;
 while i<=length(s) do begin
  case s[i] of
   '"','\':begin
    result:=result+'\'+s[i];
    inc(i);
   end;
   #$0008:begin
    result:=result+'\b';
    inc(i);
   end;
   #$0009:begin
    result:=result+'\t';
    inc(i);
   end;
   #$000a:begin
    result:=result+'\n';
    inc(i);
   end;
   #$000b:begin
    result:=result+'\v';
    inc(i);
   end;
   #$000c:begin
    result:=result+'\f';
    inc(i);
   end;
   #$000d:begin
    result:=result+'\r';
    inc(i);
   end;
   #$0000..#$0007,#$000e..#$001f,#$007d..#$009f,#$00ad,#$0600..#$0604,#$070f,#$17b4,#$17b5,#$200c..#$200f,#$2028..#$202f,#$2060..#$206f,#$feff,#$fff0..#$ffff:begin
    c:=word(widechar(s[i]));
    result:=result+'\u'+BESENHexChars[false,(c shr 12) and $f]+BESENHexChars[false,(c shr 8) and $f]+BESENHexChars[false,(c shr 4) and $f]+BESENHexChars[false,c and $f];
    inc(i);
   end;
   else begin
    result:=result+s[i];
    inc(i);
   end;
  end;
 end;
 result:=result+'"';
end;

function BESENIsHex(const v:word):boolean;
const IsCharHex:TBESENCharBitmap=($00,$00,$00,$00,$00,$00,$ff,$03,$7e,$00,$00,$00,$7e,$00,$00,$00); // [0-9a-fA-F]
begin
 result:=(v<$80) and ((IsCharHex[(v shr 3) and $7f] and (1 shl (v and 7)))<>0);
end;

procedure InitHexValues;
var i:longint;
begin
 fillchar(BESENHexValues,sizeof(TBESENHexValues),#0);
 for i:=0 to 15 do begin
  BESENHexValues[word(widechar(BESENHexChars[false,i]))]:=i;
  BESENHexValues[word(widechar(BESENHexChars[true,i]))]:=i;
 end;
end;

procedure InitBESEN;
begin
{$ifndef BESENSingleStringType}
 InitBase64;
{$endif}
 InitHexValues;
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;
end.
