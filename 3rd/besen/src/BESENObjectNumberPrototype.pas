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
unit BESENObjectNumberPrototype;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectNumber,BESENValue,BESENObjectPropertyDescriptor,
     BESENObjectBoolean;

type TBESENObjectNumberPrototype=class(TBESENObjectNumber)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToFixed(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToExponential(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToPrecision(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENErrors,BESENNumberUtils;

constructor TBESENObjectNumberPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Number';
 ObjectName:='Number';

 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;

 RegisterNativeFunction('toString',NativeToString,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toLocaleString',NativeToLocaleString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('valueOf',NativeValueOf,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toFixed',NativeToFixed,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toExponential',NativeToExponential,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toPrecision',NativeToPrecision,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectNumberPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectNumberPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Radix:integer;
    nv:TBESENValue;
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  nv:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  nv:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
 if CountArguments=0 then begin
  Radix:=10;
 end else begin
  Radix:=TBESEN(Instance).ToInt32(Arguments^[0]^);
 end;
 if Radix=10 then begin
  TBESEN(Instance).ToStringValue(nv,ResultValue);
 end else if Radix in [2..36] then begin
  ResultValue.ValueType:=bvtSTRING;
  if BESENIsNaN(nv.Num) then begin
   ResultValue.Str:='NaN';
  end else if BESENIsZero(nv.Num) then begin
   ResultValue.Str:='0';
  end else if BESENIsInfinite(nv.Num) then begin
   if BESENIsNegative(nv.Num) then begin
    ResultValue.Str:='-Infinity';
   end else begin
    ResultValue.Str:='Infinity';
   end;
  end else begin
   ResultValue.Str:=BESENNumberToRadixString(nv.Num,Radix);
  end;
 end else begin
  raise EBESENRangeError.Create('Bad radix');
 end;
end;

procedure TBESENObjectNumberPrototype.NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var nv:TBESENValue;
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  nv:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  nv:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
 ResultValue:=BESENStringValue(BESENFloatToLocaleStr(nv.Num));
end;

procedure TBESENObjectNumberPrototype.NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  ResultValue:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  ResultValue:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
end;

procedure TBESENObjectNumberPrototype.NativeToFixed(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
{$ifdef UseDTOA}
var f:int64;
    v,nv:TBESENValue;
    x:TBESENNumber;
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  nv:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  nv:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
 f:=0;
 if (CountArguments>0) and (Arguments^[0]^.ValueType<>bvtUNDEFINED) then begin
  TBESEN(Instance).ToIntegerValue(Arguments^[0]^,v);
  if (TBESEN(Instance).Compatibility and COMPAT_BESEN)<>0 then begin
   if BESENIsNaN(v.Num) or ((v.Num<0) or (v.Num>1076)) then begin
    raise EBESENRangeError.CreateUTF16('Fixed width '+BESENFloatToStr(v.Num)+' out of range');
   end;
  end else begin
   if BESENIsNaN(v.Num) or ((v.Num<0) or (v.Num>20)) then begin
    raise EBESENRangeError.CreateUTF16('Fixed width '+BESENFloatToStr(v.Num)+' out of range');
   end;
  end;
  f:=trunc(v.Num);
 end;
 x:=nv.Num;
 ResultValue.ValueType:=bvtSTRING;
 if (not BESENIsFinite(x)) or ((x<-1e+21) or (x>1e+21)) then begin
  ResultValue.Str:=BESENFloatToStr(x);
 end else begin
  ResultValue.Str:=dtostr(x,DTOSTR_FIXED,f);
 end;
end;
{$else}
var f:int64;
    v,nv:TBESENValue;
    x:TBESENNumber;
    s:TBESENString;
    ss:shortstring;
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  nv:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  nv:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
 f:=0;
 if (CountArguments>0) and (Arguments^[0]^.ValueType<>bvtUNDEFINED) then begin
  TBESEN(Instance).ToIntegerValue(Arguments^[0]^,v);
  if BESENIsNaN(v.Num) or ((v.Num<0) or (v.Num>20)) then begin
   raise EBESENRangeError.CreateUTF16('Fixed width '+BESENFloatToStr(v.Num)+' out of range');
  end;
  f:=trunc(v.Num);
 end;
 x:=nv.Num;
 ResultValue.ValueType:=bvtSTRING;
 if (not BESENIsFinite(x)) or ((x<-1e+21) or (x>1e+21)) then begin
  ResultValue.Str:=BESENFloatToStr(x);
 end else begin
  str(x:1:f,ss);
  s:=TBESENString(ss);
  ResultValue.Str:=s;
 end;
end;
{$endif}

procedure TBESENObjectNumberPrototype.NativeToExponential(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v,nv:TBESENValue;
    x:TBESENNumber;
    f:integer;
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  nv:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  nv:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
 f:=0;
 if (CountArguments>0) and (Arguments^[0]^.ValueType<>bvtUNDEFINED) then begin
  TBESEN(Instance).ToIntegerValue(Arguments^[0]^,v);
  if (TBESEN(Instance).Compatibility and COMPAT_BESEN)<>0 then begin
   if BESENIsNaN(v.Num) or ((v.Num<0) or (v.Num>1076)) then begin
    raise EBESENRangeError.CreateUTF16('Exponent width '+BESENFloatToStr(v.Num)+' out of range');
   end;
  end else begin
   if BESENIsNaN(v.Num) or ((v.Num<0) or (v.Num>20)) then begin
    raise EBESENRangeError.CreateUTF16('Exponent width '+BESENFloatToStr(v.Num)+' out of range');
   end;
  end;
  f:=trunc(v.Num);
 end;
 x:=nv.Num;
 if not BESENIsFinite(x) then begin
  ResultValue:=BESENStringValue(BESENFloatToStr(x));
 end else begin
  if (CountArguments>0) and (Arguments^[0]^.ValueType<>bvtUNDEFINED) then begin
   ResultValue:=BESENStringValue(TBESENString(BESENDoubleToString(x,BESEN_DOUBLETOSTRINGMODE_EXPONENTIAL,f+1)));
  end else begin
   ResultValue:=BESENStringValue(TBESENString(BESENDoubleToString(x,BESEN_DOUBLETOSTRINGMODE_STANDARDEXPONENTIAL,0)));
  end;
 end;
end;

procedure TBESENObjectNumberPrototype.NativeToPrecision(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var f:int64;
    v,nv:TBESENValue;
    x:TBESENNumber;
begin
 if ThisArgument.ValueType=bvtNUMBER then begin
  nv:=ThisArgument;
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectNumber) then begin
  nv:=BESENNumberValue(TBESENObjectNumber(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a Number object');
 end;
 f:=0;
 if (CountArguments>0) and (Arguments^[0]^.ValueType<>bvtUNDEFINED) then begin
  TBESEN(Instance).ToIntegerValue(Arguments^[0]^,v);
  if (TBESEN(Instance).Compatibility and COMPAT_BESEN)<>0 then begin
   if BESENIsNaN(v.Num) or ((v.Num<1) or (v.Num>1076)) then begin
    raise EBESENRangeError.CreateUTF16('Precision '+BESENFloatToStr(v.Num)+' out of range');
   end;
  end else begin
   if BESENIsNaN(v.Num) or ((v.Num<1) or (v.Num>21)) then begin
    raise EBESENRangeError.CreateUTF16('Precision '+BESENFloatToStr(v.Num)+' out of range');
   end;
  end;
  f:=trunc(v.Num);
 end;
 x:=nv.Num;
 if (not BESENIsFinite(x)) or ((x<-1e+21) or (x>1e+21)) then begin
  ResultValue:=BESENStringValue(BESENFloatToStr(x));
 end else begin
  ResultValue:=BESENStringValue(TBESENString(BESENDoubleToString(x,BESEN_DOUBLETOSTRINGMODE_PRECISION,f)));
 end;
end;

end.
