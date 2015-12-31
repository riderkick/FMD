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
unit BESENObjectArrayPrototype;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectArray,BESENValue,BESENObjectPropertyDescriptor,
     BESENObjectBoolean;

type TBESENObjectArrayPrototype=class(TBESENObjectArray)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeConcat(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeJoin(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativePop(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativePush(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeReverse(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeShift(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSlice(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSort(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSplice(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeUnshift(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeIndexOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeLastIndexOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeEvery(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSome(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeForEach(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeMap(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeFilter(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeReduce(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeReduceRight(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENErrors,BESENGlobals,BESENArrayUtils,BESENHashUtils,BESENNumberUtils,BESENStringUtils,BESENLocale;

constructor TBESENObjectArrayPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Array';
 ObjectName:='Array';

 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;

 RegisterNativeFunction('toString',NativeToString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toLocaleString',NativeToLocaleString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('concat',NativeConcat,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('join',NativeJoin,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('pop',NativePop,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('push',NativePush,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('reverse',NativeReverse,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('shift',NativeShift,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('slice',NativeSlice,2,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('sort',NativeSort,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('splice',NativeSplice,2,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('unshift',NativeUnshift,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('indexOf',NativeIndexOf,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('lastIndexOf',NativeLastIndexOf,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('every',NativeEvery,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('some',NativeSome,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('forEach',NativeForEach,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('map',NativeMap,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('filter',NativeFilter,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('reduce',NativeReduce,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('reduceRight',NativeReduceRight,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectArrayPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectArrayPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v,ov:TBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('join',v);
  if BESENIsCallable(v) then begin
   TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),ThisArgument,nil,0,ResultValue);
  end else begin
   TBESEN(Instance).ObjectPrototype.NativeToString(ThisArgument,Arguments,CountArguments,ResultValue);
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,v,vo,vs:TBESENValue;
    i,l:int64;
    Separator,s:TBESENString;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(v);
  Separator:=BESENLocaleFormatSettings.ListSeparator;
  s:='';
  if l>0 then begin
   i:=0;
   while i<l do begin
    if i>0 then begin
     s:=s+Separator;
    end;
    TBESENObject(ov.Obj).Get(BESENArrayIndexToStr(i),v);
    TBESEN(Instance).ToObjectValue(v,vo);
    if (vo.ValueType<>bvtOBJECT) or not assigned(TBESENObject(vo.Obj)) then begin
     raise EBESENTypeError.Create('No valid object');
    end;
    TBESENObject(vo.Obj).Get('toLocaleString',v);
    if (v.ValueType<>bvtOBJECT) or not (assigned(TBESENObject(v.Obj)) and TBESENObject(v.Obj).HasCall) then begin
     raise EBESENTypeError.Create('No callable');
    end;
    TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),vo,nil,0,vs);
    if v.ValueType<>bvtSTRING then begin
     raise EBESENTypeError.Create('No string');
    end;
    s:=s+TBESEN(Instance).ToStr(vs);
    inc(i);
   end;
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENStringValue(s);
end;

procedure TBESENObjectArrayPrototype.NativeConcat(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var a,eA:TBESENObjectArray;
    ov,e,v:TBESENValue;
    l:int64;
    pk:TBESENString;
    k,n:int64;
    i:integer;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 a:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(a);
 a.GarbageCollectorLock;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  e:=BESENEmptyValue;
  e:=BESENObjectValue(TBESENObject(ov.Obj));
  n:=0;
  i:=0;
  while true do begin
   if (e.ValueType=bvtOBJECT) and assigned(e.Obj) and (e.Obj is TBESENObjectArray) then begin
    eA:=TBESENObjectArray(e.Obj);
    eA.Get('length',v,eA,BESENLengthHash);
    l:=TBESEN(Instance).ToUINT32(v);
    k:=0;
    while k<l do begin
     BESENArrayCheckTooLong(n,1);
     pk:=BESENArrayIndexToStr(k);
     if eA.Get(pk,v) then begin
      a.DefineOwnProperty(BESENArrayIndexToStr(n),BESENDataPropertyDescriptor(v,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
     end;
     inc(n);
     inc(k);
    end;
   end else begin
    BESENArrayCheckTooLong(n,1);
    A.DefineOwnProperty(BESENArrayIndexToStr(n),BESENDataPropertyDescriptor(e,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
    inc(n);
   end;
   if i<CountArguments then begin
    e:=Arguments^[i]^;
    inc(i);
   end else begin
    break;
   end;
  end;
  a.Len:=n;
 finally
  a.GarbageCollectorUnlock;
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(a);
end;

procedure TBESENObjectArrayPrototype.NativeJoin(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,v:TBESENValue;
    i,l:int64;
    UseComma:boolean;
    Separator,s:TBESENString;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  v:=BESENEmptyValue;
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(v);
  UseComma:=(CountArguments=0) or (Arguments^[0]^.ValueType=bvtUNDEFINED);
  if UseComma then begin
   Separator:=',';
  end else begin
   Separator:=TBESEN(Instance).ToStr(Arguments^[0]^);
  end;
  s:='';
  if l>0 then begin
   i:=0;
   while i<l do begin
    if i>0 then begin
     s:=s+Separator;
    end;
    TBESENObject(ov.Obj).Get(BESENArrayIndexToStr(i),v);
    if not (v.ValueType in [bvtUNDEFINED,bvtNULL]) then begin
     s:=s+TBESEN(Instance).ToStr(v);
    end;
    inc(i);
   end;
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENStringValue(s);
end;

procedure TBESENObjectArrayPrototype.NativePop(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,v:TBESENValue;
    l:int64;
    si:TBESENString;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 ResultValue:=BESENUndefinedValue;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(v);
  if l=0 then begin
   TBESENObject(ov.Obj).Put('length',BESENNumberValue(0),true,BESENLengthHash);
   ResultValue:=BESENUndefinedValue;
  end else begin
   si:=BESENArrayIndexToStr(l-1);
   TBESENObject(ov.Obj).Get(si,ResultValue);
   TBESENObject(ov.Obj).Delete(si,true);
   TBESENObject(ov.Obj).Put('length',BESENNumberValue(l-1),true,BESENLengthHash);
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativePush(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,v:TBESENValue;
    l:int64;
    p:integer;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(v);
  for p:=0 to CountArguments-1 do begin
   BESENArrayCheckTooLong(l,1);
   TBESENObject(ov.Obj).Put(BESENArrayIndexToStr(l),Arguments^[p]^,true);
   inc(l);
  end;
  TBESENObject(ov.Obj).Put('length',BESENNumberValue(l),true,BESENLengthHash);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENNumberValue(l);
end;

procedure TBESENObjectArrayPrototype.NativeReverse(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,uv,lv:TBESENValue;
    l:int64;
    up,lp:TBESENString;
    middle,lower,upper:longword;
    ue,le:TBESENBoolean;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  uv:=BESENEmptyValue;
  lv:=BESENEmptyValue;
  TBESENObject(ov.Obj).Get('length',lv,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(lv);
  middle:=l shr 1;
  lower:=0;
  while lower<>middle do begin
   upper:=l-(lower+1);
   up:=BESENArrayIndexToStr(upper);
   lp:=BESENArrayIndexToStr(lower);
   le:=TBESENObject(ov.Obj).Get(lp,lv);
   ue:=TBESENObject(ov.Obj).Get(up,uv);
   if le and ue then begin
    TBESENObject(ov.Obj).Put(lp,uv,true);
    TBESENObject(ov.Obj).Put(up,lv,true);
   end else if ue and not le then begin
    TBESENObject(ov.Obj).Put(lp,uv,true);
    TBESENObject(ov.Obj).Delete(up,true);
   end else if le and not ue then begin
    TBESENObject(ov.Obj).Put(up,lv,true);
    TBESENObject(ov.Obj).Delete(lp,true);
   end;
   inc(lower);
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(TBESENObject(ov.Obj));
end;

procedure TBESENObjectArrayPrototype.NativeShift(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,v:TBESENValue;
    l,k:int64;
    fp,tp:TBESENString;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(v);
  if l=0 then begin
   TBESENObject(ov.Obj).Put('length',BESENNumberValue(0),true,BESENLengthHash);
   ResultValue:=BESENUndefinedValue;
  end else begin
   TBESENObject(ov.Obj).Get('0',ResultValue);
   k:=1;
   while k<l do begin
    fp:=BESENArrayIndexToStr(k);
    tp:=BESENArrayIndexToStr(k-1);
    if TBESENObject(ov.Obj).Get(fp,v) then begin
     TBESENObject(ov.Obj).Put(tp,v,true);
    end else begin
     TBESENObject(ov.Obj).Delete(tp,true);
    end;
    inc(k);
   end;
   TBESENObject(ov.Obj).Delete(BESENArrayIndexToStr(l-1),true);
   TBESENObject(ov.Obj).Put('length',BESENNumberValue(l-1),true,BESENLengthHash);
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeSlice(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var a:TBESENObjectArray;
    ov,v:TBESENValue;
    l:int64;
    Pk:TBESENString;
    rs,re,k,f,n:int64;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 a:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(a);
 a.GarbageCollectorLock;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   rs:=TBESEN(Instance).ToInt32(Arguments^[0]^);
  end else begin
   rs:=0;
  end;
  if rs<0 then begin
   k:=l+rs;
   if k<0 then begin
    k:=0;
   end;
  end else begin
   k:=rs;
   if l<k then begin
    k:=l;
   end;
  end;
  if (CountArguments>1) and (Arguments^[1]^.ValueType<>bvtUNDEFINED) then begin
   re:=TBESEN(Instance).ToInt32(Arguments^[1]^);
  end else begin
   re:=l;
  end;
  if re<0 then begin
   f:=l+re;
   if f<0 then begin
    f:=0;
   end;
  end else begin
   f:=re;
   if l<f then begin
    f:=l;
   end;
  end;
  n:=0;
  while k<f do begin
   Pk:=BESENArrayIndexToStr(k);
   if TBESENObject(ov.Obj).Get(Pk,v) then begin
    a.DefineOwnProperty(BESENArrayIndexToStr(n),BESENDataPropertyDescriptor(v,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
   end;
   inc(k);
   inc(n);
  end;
  a.Put('length',BESENNumberValue(n),true,BESENLengthHash);
 finally
  a.GarbageCollectorUnlock;
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(a);
end;

procedure TBESENObjectArrayPrototype.NativeSort(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var CompareFunction:TBESENObject;
    ov,sva,svb:TBESENValue;
 procedure IntroSort(Left,Right,Depth:int64);
  function SortCompare(const x,y:TBESENValue):integer;
  var vn:TBESENValue;
      sx,sy:TBESENString;
      ValuePointers:array[0..1] of PBESENValue;
  begin
   if (x.ValueType=bvtNONE) and (y.ValueType=bvtNONE) then begin
    result:=0;
   end else if x.ValueType=bvtNONE then begin
    result:=1;
   end else if y.ValueType=bvtNONE then begin
    result:=-1;
   end else if (x.ValueType=bvtUNDEFINED) and (y.ValueType=bvtUNDEFINED) then begin
    result:=0;
   end else if x.ValueType=bvtUNDEFINED then begin
    result:=1;
   end else if y.ValueType=bvtUNDEFINED then begin
    result:=-1;
   end else begin
    if assigned(CompareFunction) then begin
     ValuePointers[0]:=@x;
     ValuePointers[1]:=@y;
     TBESEN(Instance).ObjectCall(CompareFunction,ThisArgument,@ValuePointers,2,vn);
     if (vn.ValueType<>bvtNUMBER) or BESENIsNaN(vn.Num) then begin
      raise EBESENTypeError.Create('Array sort error');
     end;
     if BESENIsNegative(vn.Num) then begin
      result:=-1;
     end else if not BESENIsZero(0) then begin
      result:=1;
     end else begin
      result:=0;
     end;
    end else begin
     sx:=TBESEN(Instance).ToStr(x);
     sy:=TBESEN(Instance).ToStr(y);
     if sx<sy then begin
      result:=-1;
     end else if sx>sy then begin
      result:=1;
     end else begin
      result:=0;
     end;
     sx:='';
     sy:='';
    end;
   end;
  end;
  procedure GetIndex(i:int64;var r:TBESENValue);
  var s:TBESENString;
  begin
   s:=BESENArrayIndexToStr(i);
   if not TBESENObject(ov.Obj).Get(s,r) then begin
    r.ValueType:=bvtNONE;
   end;
  end;
  procedure PutIndex(i:int64;const a:TBESENValue);
  begin
   if a.ValueType=bvtNONE then begin
    TBESENObject(ov.Obj).Delete(BESENArrayIndexToStr(i),true);
   end else begin
    TBESENObject(ov.Obj).Put(BESENArrayIndexToStr(i),a,true);
   end;
  end;
  function CompareIndex(const a,b:int64):integer;
  begin
   GetIndex(a,sva);
   GetIndex(b,svb);
   result:=SortCompare(sva,svb);
  end;
  procedure Swap(a,b:int64);
  begin
   GetIndex(a,sva);
   GetIndex(b,svb);
   PutIndex(b,sva);
   PutIndex(a,svb);
  end;
  procedure SiftDown(Current,MaxIndex:int64);
  var SiftLeft,SiftRight,Largest:int64;
      sl,sr,l:TBESENValue;
  begin
   SiftLeft:=Left+(2*(Current-Left))+1;
   SiftRight:=Left+(2*(Current-Left))+2;
   Largest:=Current;
   sl:=BESENEmptyValue;
   sr:=BESENEmptyValue;
   l:=BESENEmptyValue;
   GetIndex(SiftLeft,sl);
   GetIndex(Largest,l);
   if (SiftLeft<=MaxIndex) and (SortCompare(sl,l)>0) then begin
    Largest:=SiftLeft;
    GetIndex(Largest,l);
   end;
   GetIndex(SiftRight,sr);
   if (SiftRight<=MaxIndex) and (SortCompare(sr,l)>0) then begin
    Largest:=SiftRight;
   end;
   if Largest<>Current then begin
    Swap(Current,Largest);
    SiftDown(Largest,MaxIndex);
   end;
  end;
  procedure InsertionSort;
  var i,j:int64;
      t,x,v:TBESENValue;
  begin
   t:=BESENEmptyValue;
   x:=BESENEmptyValue;
   v:=BESENEmptyValue;
   i:=Left+1;
   while i<=Right do begin
    GetIndex(i,t);
    j:=i-1;
    while j>=Left do begin
     GetIndex(j,v);
     if SortCompare(t,v)<0 then begin
      GetIndex(j,x);
      PutIndex(j+1,x);
      dec(j);
     end else begin
      break;
     end;
    end;
    PutIndex(j+1,t);
    inc(i);
   end;
  end;
  procedure HeapSort;
  var i:int64;
  begin
   i:=((Left+Right+1) div 2)-1;
   while i>=Left do begin
    SiftDown(i,Right);
    dec(i);
   end;
   i:=Right;
   while i>=Left+1 do begin
    Swap(i,Left);
    SiftDown(Left,i-1);
    dec(i);
   end;
  end;
  procedure QuickSortWidthMedianOfThreeOptimization;
  var Middle,i,j:integer;
      Pivot,v:TBESENValue;
  begin
   Middle:=(Left+Right) div 2;
   if (Right-Left)>3 then begin
    if CompareIndex(Left,Middle)>0 then begin
     Swap(Left,Middle);
    end;
    if CompareIndex(Left,Right)>0 then begin
     Swap(Left,Right);
    end;
    if CompareIndex(Middle,Right)>0 then begin
     Swap(Middle,Right);
    end;
   end;
   Pivot:=BESENEmptyValue;
   v:=BESENEmptyValue;
   GetIndex(Middle,Pivot);
   i:=Left;
   j:=Right;
   while true do begin
    while i<j do begin
     GetIndex(i,v);
     if SortCompare(v,Pivot)<0 then begin
      inc(i);
     end else begin
      break;
     end;
    end;
    while j>i do begin
     GetIndex(j,v);
     if SortCompare(v,Pivot)>0 then begin
      dec(j);
     end else begin
      break;
     end;
    end;
    if i>j then begin
     break;
    end;
    Swap(i,j);
    inc(i);
    dec(j);
   end;
   IntroSort(Left,j,Depth-1);
   IntroSort(i,Right,Depth-1);
  end;
 begin
  if Left<Right then begin
   if (Right-Left)<16 then begin
    InsertionSort;
   end else if Depth=0 then begin
    HeapSort;
   end else begin
    QuickSortWidthMedianOfThreeOptimization;
   end;
  end;
 end;
var l:int64;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',sva,TBESENObject(ov.Obj),BESENLengthHash);
  l:=TBESEN(Instance).ToUINT32(sva);
  if CountArguments>0 then begin
   if (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj) and TBESENObject(Arguments^[0]^.Obj).HasCall then begin
    CompareFunction:=TBESENObject(Arguments^[0]^.Obj);
   end else begin
    raise EBESENTypeError.Create('Bad argument');
   end;
  end else begin
   CompareFunction:=nil;
  end;
  IntroSort(0,l-1,BESENIntLog2(l)*2);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(TBESENObject(ov.Obj));
end;

procedure TBESENObjectArrayPrototype.NativeSplice(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,v:TBESENValue;
    A:TBESENObject;
    r3,r5,r6,r17,k:TBESENUINT32;
    s9,s11,s22,s33,s39:TBESENString;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 a:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(a);
 a.GarbageCollectorLock;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  r3:=TBESEN(Instance).ToUINT32(v);

  if CountArguments<1 then begin
   v:=BESENNumberValue(0);
  end else begin
   TBESEN(Instance).ToIntegerValue(Arguments^[0]^,v);
  end;
  if (-v.Num)>r3 then begin
   r5:=0;
  end else if BESENIsNegative(v.Num) then begin
   r5:=trunc(r3+v.Num);
  end else if v.Num<r3 then begin
   r5:=trunc(v.Num);
  end else begin
   r5:=r3;
  end;

  if CountArguments<2 then begin
   v:=BESENNumberValue(0);
  end else begin
   TBESEN(Instance).ToIntegerValue(Arguments^[1]^,v);
  end;
  if BESENIsNegative(v.Num) then begin
   r6:=0;
  end else begin
   r6:=trunc(v.Num);
  end;
  if (r3-r5)<r6 then begin
   r6:=r3-r5;
  end;
  k:=0;
  while k<r6 do begin
   s9:=BESENArrayIndexToStr(r5+k);
   if TBESENObject(ov.Obj).Get(s9,v) then begin
    s11:=BESENArrayIndexToStr(k);
    A.DefineOwnProperty(s11,BESENDataPropertyDescriptor(v,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
   end;
   inc(k);
  end;

  A.Put('length',BESENNumberValue(r6),true,BESENLengthHash);
  if CountArguments<2 then begin
   r17:=0;
  end else begin
   r17:=CountArguments-2;
  end;
  if r17<>r6 then begin
   if r17<=r6 then begin
    k:=r5;
    while k<(r3-r6) do begin
     s22:=BESENArrayIndexToStr(k+r6);
     if TBESENObject(ov.Obj).Get(s22,v) then begin
      TBESENObject(ov.Obj).Put(BESENArrayIndexToStr(k+r17),v,true);
     end else begin
      TBESENObject(ov.Obj).Delete(BESENArrayIndexToStr(k+r17),true);
     end;
     inc(k);
    end;
    k:=r3;
    while k>((r3+r17)-r6) do begin
     s33:=BESENArrayIndexToStr(k-1);
     TBESENObject(ov.Obj).Delete(s33,true);
     dec(k);
    end;
   end else begin
    k:=r3-r6;
    while k>r5 do begin
     s39:=BESENArrayIndexToStr(k+r6-1);
     if TBESENObject(ov.Obj).Get(s39,v) then begin
      TBESENObject(ov.Obj).Put(BESENArrayIndexToStr(k+r17-1),v,true);
     end else begin
      TBESENObject(ov.Obj).Delete(BESENArrayIndexToStr(k+r17-1),true);
     end;
     dec(k);
    end;
   end;
  end;

  k:=2;
  while k<longword(CountArguments) do begin
   TBESENObject(ov.Obj).Put(BESENArrayIndexToStr((k+r5)-2),Arguments^[k]^,true);
   inc(k);
  end;

  TBESENObject(ov.Obj).Put('length',BESENNumberValue((r3+r17)-r6),true,BESENLengthHash);
 finally
  a.GarbageCollectorUnlock;
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(a);
end;

procedure TBESENObjectArrayPrototype.NativeUnshift(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var r2,r3,k:TBESENUINT32;
    ov,v:TBESENValue;
    p:TBESENString;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  r2:=TBESEN(Instance).ToUINT32(v);
  r3:=CountArguments;
  BESENArrayCheckTooLong(r2,r3);
  k:=r2;
  while k>0 do begin
   p:=BESENArrayIndexToStr(k-1);
   if TBESENObject(ov.Obj).Get(p,v) then begin
    TBESENObject(ov.Obj).Put(BESENArrayIndexToStr((k+r3)-1),v,true);
   end else begin
    TBESENObject(ov.Obj).Delete(BESENArrayIndexToStr((k+r3)-1),true);
   end;
   dec(k);
  end;
  k:=0;
  while k<r3 do begin
   TBESENObject(ov.Obj).Put(BESENArrayIndexToStr(k),Arguments^[k]^,true);
   inc(k);
  end;
  TBESENObject(ov.Obj).Put('length',BESENNumberValue(r2+r3),true,BESENLengthHash);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
 ResultValue:=BESENUndefinedValue;
end;

procedure TBESENObjectArrayPrototype.NativeIndexOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,sv,v:TBESENValue;
    Len,n,k:int64;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  ResultValue.ValueType:=bvtNUMBER;
  ResultValue.Num:=-1;
  if Len>0 then begin
   if CountArguments>0 then begin
    sv:=Arguments^[0]^;
   end else begin
    sv:=BESENUndefinedValue;
   end;
   if CountArguments>1 then begin
    n:=TBESEN(Instance).ToInt(Arguments^[1]^);
   end else begin
    n:=0;
   end;
   if n<Len then begin
    if n<0 then begin
     k:=Len-abs(n);
     if k<0 then begin
      k:=0;
     end;
    end else begin
     k:=n;
    end;
    while k<Len do begin
     if TBESENObject(ov.Obj).Get(BESENArrayIndexToStr(k),v) then begin
      if BESENEqualityExpressionStrictEquals(sv,v) then begin
       ResultValue.ValueType:=bvtNUMBER;
       ResultValue.Num:=k;
       break;
      end;
     end;
     inc(k);
    end;
   end;
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeLastIndexOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,sv,v:TBESENValue;
    Len,n,k:int64;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 ResultValue.ValueType:=bvtNUMBER;
 ResultValue.Num:=-1;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if Len>0 then begin
   if CountArguments>0 then begin
    sv:=Arguments^[0]^;
   end else begin
    sv:=BESENUndefinedValue;
   end;
   if CountArguments>1 then begin
    n:=TBESEN(Instance).ToInt(Arguments^[1]^);
   end else begin
    n:=Len-1;
   end;
   if n<0 then begin
    k:=Len-abs(n);
   end else begin
    k:=min(n,Len-1);
   end;
   while k>=0 do begin
    if TBESENObject(ov.Obj).Get(BESENArrayIndexToStr(k),v) then begin
     if BESENEqualityExpressionStrictEquals(sv,v) then begin
      ResultValue.ValueType:=bvtNUMBER;
      ResultValue.Num:=k;
      break;
     end;
    end;
    dec(k);
   end;
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeEvery(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,tv,v,vr,vi:TBESENValue;
    Len,k:int64;
    Pk:TBESENString;
    ValuePointers:array[0..2] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  if CountArguments>1 then begin
   tv:=Arguments^[1]^;
  end else begin
   tv:=BESENUndefinedValue;
  end;
  ValuePointers[0]:=@v;
  ValuePointers[1]:=@vi;
  ValuePointers[2]:=@ov;
  ResultValue.ValueType:=bvtBOOLEAN;
  ResultValue.Bool:=true;
  k:=0;
  while k<Len do begin
   Pk:=BESENArrayIndexToStr(k);
   if TBESENObject(ov.Obj).Get(Pk,v) then begin
    vi.ValueType:=bvtNUMBER;
    vi.Num:=k;
    TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),tv,@ValuePointers,3,vr);
    if not TBESEN(Instance).ToBool(vr) then begin
     ResultValue.Bool:=false;
     break;
    end;
   end;
   inc(k);
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeSome(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,tv,v,vr,vi:TBESENValue;
    Len,k:int64;
    Pk:TBESENString;
    ValuePointers:array[0..2] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  if CountArguments>1 then begin
   tv:=Arguments^[1]^;
  end else begin
   tv:=BESENUndefinedValue;
  end;
  ResultValue.ValueType:=bvtBOOLEAN;
  ResultValue.Bool:=false;
  ValuePointers[0]:=@v;
  ValuePointers[1]:=@vi;
  ValuePointers[2]:=@ov;
  k:=0;
  while k<Len do begin
   Pk:=BESENArrayIndexToStr(k);
   if TBESENObject(ov.Obj).Get(Pk,v) then begin
    vi.ValueType:=bvtNUMBER;
    vi.Num:=k;
    TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),tv,@ValuePointers,3,vr);
    if TBESEN(Instance).ToBool(vr) then begin
     ResultValue.Bool:=true;
     break;
    end;
   end;
   inc(k);
  end;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeForEach(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,tv,v,vr,vi:TBESENValue;
    Len,k:int64;
    Pk:TBESENString;
    ValuePointers:array[0..2] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  if CountArguments>1 then begin
   tv:=Arguments^[1]^;
  end else begin
   tv:=BESENUndefinedValue;
  end;
  ValuePointers[0]:=@v;
  ValuePointers[1]:=@vi;
  ValuePointers[2]:=@ov;
  k:=0;
  while k<Len do begin
   Pk:=BESENArrayIndexToStr(k);
   if TBESENObject(ov.Obj).Get(Pk,v) then begin
    vi.ValueType:=bvtNUMBER;
    vi.Num:=k;
    TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),tv,@ValuePointers,3,vr);
   end;
   inc(k);
  end;
  ResultValue.ValueType:=bvtUNDEFINED;
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeMap(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,tv,v,vr,vi:TBESENValue;
    Len,k:int64;
    Pk:TBESENString;
    a:TBESENObjectArray;
    ValuePointers:array[0..2] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  if CountArguments>1 then begin
   tv:=Arguments^[1]^;
  end else begin
   tv:=BESENUndefinedValue;
  end;
  a:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
  TBESEN(Instance).GarbageCollector.Add(a);
  a.GarbageCollectorLock;
  try
   a.Len:=Len;
   ValuePointers[0]:=@v;
   ValuePointers[1]:=@vi;
   ValuePointers[2]:=@ov;
   k:=0;
   while k<Len do begin
    Pk:=BESENArrayIndexToStr(k);
    if TBESENObject(ov.Obj).Get(Pk,v) then begin
     vi.ValueType:=bvtNUMBER;
     vi.Num:=k;
     TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),tv,@ValuePointers,3,vr);
     a.DefineOwnProperty(Pk,BESENDataPropertyDescriptor(vr,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
    end;
    inc(k);
   end;
  finally
   a.GarbageCollectorUnlock;
  end;
  ResultValue:=BESENObjectValue(a);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeFilter(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,tv,kv,vr,vi:TBESENValue;
    Len,k,n:int64;
    Pk:TBESENString;
    a:TBESENObjectArray;
    ValuePointers:array[0..2] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',kv,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(kv);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  if CountArguments>1 then begin
   tv:=Arguments^[1]^;
  end else begin
   tv:=BESENUndefinedValue;
  end;
  a:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
  TBESEN(Instance).GarbageCollector.Add(a);
  a.GarbageCollectorLock;
  try
   ValuePointers[0]:=@kv;
   ValuePointers[1]:=@vi;
   ValuePointers[2]:=@ov;
   k:=0;
   n:=0;
   while k<Len do begin
    Pk:=BESENArrayIndexToStr(k);
    if TBESENObject(ov.Obj).Get(Pk,kv) then begin
     vi.ValueType:=bvtNUMBER;
     vi.Num:=k;
     TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),tv,@ValuePointers,3,vr);
     if TBESEN(Instance).ToBool(vr) then begin
      a.DefineOwnProperty(BESENArrayIndexToStr(n),BESENDataPropertyDescriptor(kv,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
      inc(n);
     end;
    end;
    inc(k);
   end;
  finally
   a.GarbageCollectorUnlock;
  end;
  ResultValue:=BESENObjectValue(a);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeReduce(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,av,v,vi,nav:TBESENValue;
    Len,k:int64;
    Pk:TBESENString;
    kPresent:boolean;
    ValuePointers:array[0..3] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  k:=0;
  if CountArguments>1 then begin
   av:=Arguments^[1]^;
  end else if Len=0 then begin
   raise EBESENTypeError.Create('Reduce failed');
  end else begin
   kPresent:=false;
   while (k<Len) and not kPresent do begin
    Pk:=BESENArrayIndexToStr(k);
    kPresent:=TBESENObject(ov.Obj).HasProperty(Pk);
    if kPresent then begin
     TBESENObject(ov.Obj).Get(Pk,av);
    end;
    inc(k);
   end;
   if not kPresent then begin
    raise EBESENTypeError.Create('Reduce failed');
   end;
  end;
  nav:=BesenUndefinedValue;
  ValuePointers[0]:=@av;
  ValuePointers[1]:=@v;
  ValuePointers[2]:=@vi;
  ValuePointers[3]:=@ov;
  while k<Len do begin
   Pk:=BESENArrayIndexToStr(k);
   if TBESENObject(ov.Obj).Get(Pk,v) then begin
    vi.ValueType:=bvtNUMBER;
    vi.Num:=k;
    TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),BESENUndefinedValue,@ValuePointers,4,nav);
    BesenCopyValue(av,nav);
   end;
   inc(k);
  end;
  BESENCopyValue(ResultValue,av);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectArrayPrototype.NativeReduceRight(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,cv,av,v,vi,nav:TBESENValue;
    Len,k:int64;
    Pk:TBESENString;
    kPresent:boolean;
    ValuePointers:array[0..3] of PBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 if not ((ov.ValueType=bvtOBJECT) and assigned(TBESENObject(ov.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObject(ov.Obj).GarbageCollectorLock;
 try
  TBESENObject(ov.Obj).Get('length',v,TBESENObject(ov.Obj),BESENLengthHash);
  Len:=TBESEN(Instance).ToUINT32(v);
  if CountArguments>0 then begin
   cv:=Arguments^[0]^;
  end else begin
   cv:=BESENUndefinedValue;
  end;
  if not BESENIsCallable(cv) then begin
   raise EBESENTypeError.Create('Callback not callable');
  end;
  k:=Len-1;
  if CountArguments>1 then begin
   av:=Arguments^[1]^;
  end else if Len=0 then begin
   raise EBESENTypeError.Create('Reduce failed');
  end else begin
   kPresent:=false;
   while (k>=0) and not kPresent do begin
    Pk:=BESENArrayIndexToStr(k);
    kPresent:=TBESENObject(ov.Obj).HasProperty(Pk);
    if kPresent then begin
     TBESENObject(ov.Obj).Get(Pk,av);
    end;
    dec(k);
   end;
   if not kPresent then begin
    raise EBESENTypeError.Create('Reduce failed');
   end;
  end;
  nav:=BesenUndefinedValue;
  ValuePointers[0]:=@av;
  ValuePointers[1]:=@v;
  ValuePointers[2]:=@vi;
  ValuePointers[3]:=@ov;
  while k>=0 do begin
   Pk:=BESENArrayIndexToStr(k);
   if TBESENObject(ov.Obj).Get(Pk,v) then begin
    vi.ValueType:=bvtNUMBER;
    vi.Num:=k;
    TBESEN(Instance).ObjectCall(TBESENObject(cv.Obj),BESENUndefinedValue,@ValuePointers,4,nav); // ES5 errata fix
    BesenCopyValue(av,nav);
   end;
   dec(k);
  end;
  BESENCopyValue(ResultValue,av);
 finally
  TBESENObject(ov.Obj).GarbageCollectorUnlock;
 end;
end;

end.
