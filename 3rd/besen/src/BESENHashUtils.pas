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
unit BESENHashUtils;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes;

function BESENHashKey(const Key:TBESENString):TBESENHash;
function BESENDoubleHash(Hash:TBESENHash):TBESENHash;

implementation

function BESENHashKey(const Key:TBESENString):TBESENHash;
{$ifdef PurePascal}
var i,h:longword;
begin
 h:=2166136261;
 for i:=1 to length(Key) do begin
{$ifdef UseOptimizedHashing}
  h:=h xor ord(Key[i]);
  inc(h,(h shl 1)+(h shl 4)+(h shl 7)+(h shl 8)+(h shl 24));
{$else}
  h:=(h xor ord(Key[i]))*16777619;
{$endif}
 end;
 if h=0 then begin
  // result must be never zero ! ! !
  result:=$ffffffff;
 end else begin
  result:=h;
 end;
end;
{$else}
{$ifdef cpu64}
var i:longword;
    h:{$ifdef fpc}qword{$else}int64{$endif};
begin
 h:={$ifdef fpc}qword(14695981039346656037){$else}int64($14650FB0739D0383){$endif};
 for i:=1 to length(Key) do begin
{$ifdef UseOptimizedHashing}
  h:=h xor word(widechar(Key[i]));
  inc(h,(h shl 1)+(h shl 4)+(h shl 5)+(h shl 7)+(h shl 8)+(h shl 40));
{$else}
  h:=(h xor word(widechar(Key[i])))*1099511628211;
{$endif}
 end;
 result:=h;
 dec(result,ord(result=0));
end;
{$else}
{$ifdef cpu386} assembler; register;
asm
 push ebx
 push esi
 push edi
  test eax,eax
  jz @Zero
  mov ecx,dword ptr [eax-4]
  jecxz @Zero
   shr ecx,1
   mov esi,eax
   mov eax,2166136261
   xor ebx,ebx
{$ifndef UseOptimizedHashing}
   mov edi,16777619
{$endif}
   @Loop:
    mov bx,word ptr [esi]
    xor eax,ebx
{$ifdef UseOptimizedHashing}
    mov ebx,eax
    mov edi,ebx
    add edi,edi
    add eax,edi
    mov edi,ebx
    shl edi,4
    add eax,edi
    mov edi,ebx
    shl edi,7
    add eax,edi
    mov edi,ebx
    shl edi,8
    add eax,edi
    shl ebx,24
    add eax,ebx
{$else}
    mul edi
{$endif}
    add esi,2
    dec ecx
   jnz @Loop
   or eax,eax
   jnz @Done
   @Zero:
    xor eax,eax
    not eax
  @Done:
 pop edi
 pop esi
 pop ebx
end;
{$else}
var i,h:longword;
begin
 h:=2166136261;
 for i:=1 to length(Key) do begin
{$ifdef UseOptimizedHashing}
  h:=h xor word(widechar(Key[i]));
  inc(h,(h shl 1)+(h shl 4)+(h shl 7)+(h shl 8)+(h shl 24));
{$else}
  h:=(h xor word(widechar(Key[i])))*16777619;
{$endif}
 end;
 result:=h-ord(h=0);
end;
{$endif}
{$endif}
{$endif}

function BESENDoubleHash(Hash:TBESENHash):TBESENHash;
begin
 result:=(not Hash)+(Hash shr 23);
 result:=result xor (result shl 22);
 result:=result xor (result shr 7);
 result:=result xor (result shl 2);
 result:=result xor (result shr 20);
end;

end.
