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
unit BESENArrayUtils;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENValue;

function BESENArrayToIndex(AProp:TBESENString;var v:int64):TBESENBoolean;
function BESENArrayToIndexEx(AProp:TBESENString):int64;
function BESENArrayIndexToStr(v:TBESENUINT32):TBESENString;
procedure BESENArrayCheckTooLong(const a,b:TBESENUINT32);

implementation

uses BESEN,BESENUtils,BESENErrors;

function BESENArrayToIndex(AProp:TBESENString;var v:int64):TBESENBoolean;
var i:integer;
begin
 if length(AProp)=0 then begin
  result:=false;
 end else if (length(AProp)>1) and (AProp[1]='0') then begin
  result:=false;
 end else begin
  result:=true;
  v:=0;
  for i:=1 to length(AProp) do begin
   if (word(widechar(AProp[i]))>=ord('0')) and (word(widechar(AProp[i]))<=ord('9')) then begin
    v:=(v*10)+(word(widechar(AProp[i]))-ord('0'));
   end else begin
    result:=false;
    break;
   end;
  end;
  if result and ((IntToStr(v and $ffffffff)<>AProp) or (v>=$ffffffff)) then begin
   result:=false;
  end;
 end;
end;

function BESENArrayToIndexEx(AProp:TBESENString):int64;
var i:integer;
begin
 if length(AProp)=0 then begin
  result:=0;
 end else if (length(AProp)>1) and (AProp[1]='0') then begin
  result:=0;
 end else begin
  result:=0;
  for i:=1 to length(AProp) do begin
   if (word(widechar(AProp[i]))>=ord('0')) and (word(widechar(AProp[i]))<=ord('9')) then begin
    result:=(result*10)+(word(widechar(AProp[i]))-ord('0'));
   end else begin
    result:=0;
    break;
   end;
  end;
 end;
end;

function BESENArrayIndexToStr(v:TBESENUINT32):TBESENString;
var i:integer;
    o:TBESENUINT32;
begin
 if v=0 then begin
  result:='0';
 end else begin
  result:='';
  i:=0;
  o:=v;
  while o<>0 do begin
   inc(i);
   o:=o div 10;
  end;
  SetLength(result,i);
  while (v<>0) and (i>0) do begin
   result[i]:=widechar(word(v mod 10)+ord('0'));
   dec(i);
   v:=v div 10;
  end;
 end;
end;

procedure BESENArrayCheckTooLong(const a,b:TBESENUINT32);
var c:TBESENUINT32;
begin
 c:=TBESENUINT32(a+b) and $ffffffff;
 if (c<a) or (c<b) then begin
  BESENThrowRangeError('Array too long');
 end;
end;

end.
