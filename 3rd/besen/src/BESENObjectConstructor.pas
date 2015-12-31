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
unit BESENObjectConstructor;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       procedure NativeGetPrototypeOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetOwnPropertyDescriptor(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetOwnPropertyNames(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeCreate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeDefineProperty(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeDefineProperties(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSeal(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeFreeze(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativePreventExtensions(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeIsSealed(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeIsFrozen(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeIsExtensible(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeKeys(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENUtils,BESENArrayUtils,BESENErrors,BESENObjectString,BESENObjectArray;

constructor TBESENObjectConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';

 RegisterNativeFunction('getPrototypeOf',NativeGetPrototypeOf,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getOwnPropertyDescriptor',NativeGetOwnPropertyDescriptor,2,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getOwnPropertyNames',NativeGetOwnPropertyNames,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('create',NativeCreate,2,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('defineProperty',NativeDefineProperty,3,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('defineProperties',NativeDefineProperties,2,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('seal',NativeSeal,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('freeze',NativeFreeze,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('preventExtensions',NativePreventExtensions,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('isSealed',NativeIsSealed,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('isFrozen',NativeIsFrozen,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('isExtensible',NativeIsExtensible,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('keys',NativeKeys,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 if CountArguments>0 then begin
  case Arguments^[0]^.ValueType of
   bvtNULL,bvtUNDEFINED:begin
    AResult.ValueType:=bvtOBJECT;
    AResult.Obj:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype);
   end;
   else begin
    TBESEN(Instance).ToObjectValue(Arguments^[0]^,AResult);
   end;
  end;
 end else begin
  AResult.ValueType:=bvtOBJECT;
  AResult.Obj:=TBESENObject.Create(Instance,TBESEN(Instance).ObjectPrototype);
 end;
 if (AResult.ValueType=bvtOBJECT) and assigned(AResult.Obj) then begin
  TBESEN(Instance).GarbageCollector.Add(TBESENObject(AResult.Obj));
 end;
end;

procedure TBESENObjectConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 Construct(ThisArgument,Arguments,CountArguments,AResult);
end;

function TBESENObjectConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectConstructor.NativeGetPrototypeOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) then begin
  raise EBESENTypeError.Create('No object');
 end;
 ResultValue:=BESENObjectValueEx(TBESENObject(Arguments^[0]^.Obj).Prototype);
end;

procedure TBESENObjectConstructor.NativeGetOwnPropertyDescriptor(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var n:TBESENString;
    Descriptor:TBESENObjectPropertyDescriptor;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) then begin
  raise EBESENTypeError.Create('No object');
 end;
 if CountArguments>1 then begin
  n:=TBESEN(Instance).ToStr(Arguments^[1]^);
 end else begin
  n:='';
 end;
 TBESENObject(Arguments^[0]^.Obj).GetOwnProperty(n,Descriptor);
 TBESEN(Instance).FromPropertyDescriptor(Descriptor,ResultValue);
 if (ResultValue.ValueType=bvtOBJECT) and assigned(ResultValue.Obj) then begin
  TBESEN(Instance).GarbageCollector.Add(TBESENObject(ResultValue.Obj));
 end;
end;

procedure TBESENObjectConstructor.NativeGetOwnPropertyNames(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ArrayObject:TBESENObjectArray;
    o:TBESENObject;
    PropItem:TBESENObjectProperty;
    n:longword;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) then begin
  raise EBESENTypeError.Create('No object');
 end;
 ArrayObject:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(ArrayObject);
 ArrayObject.GarbageCollectorLock;
 try
  o:=TBESENObject(Arguments^[0]^.Obj);
  if o is TBESENObjectString then begin
   for n:=1 to length(TBESENObjectString(o).Value) do begin
    ArrayObject.DefineOwnProperty(BESENArrayIndexToStr(n-1),BESENDataPropertyDescriptor(BESENStringValue(BESENArrayIndexToStr(n-1)),[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
   end;
   n:=length(TBESENObjectString(o).Value);
  end else begin
   n:=0;
  end;
  PropItem:=o.Properties.First;
  while assigned(PropItem) do begin
   ArrayObject.DefineOwnProperty(BESENArrayIndexToStr(n),BESENDataPropertyDescriptor(BESENStringValue(PropItem.Key),[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
   inc(n);
   PropItem:=PropItem.Next;
  end;
  ArrayObject.Len:=n;
 finally
  ArrayObject.GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(ArrayObject);
end;

procedure TBESENObjectConstructor.NativeCreate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var o:TBESENObject;
    vo:TBESENValue;
    ValuePointers:array[0..1] of PBESENValue;
begin
 if not ((CountArguments>0) and ((Arguments^[0]^.ValueType=bvtNULL) or ((Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)))) then begin
  raise EBESENTypeError.Create('No object and not null');
 end;
 if Arguments^[0]^.ValueType=bvtNULL then begin
  o:=TBESENObject.Create(Instance,nil);
 end else begin
  o:=TBESENObject.Create(Instance,TBESENObject(Arguments^[0]^.Obj));
 end;
 if CountArguments>1 then begin
  vo.ValueType:=bvtOBJECT;
  TBESENObject(vo.Obj):=o;
  ValuePointers[0]:=@vo;
  ValuePointers[1]:=Arguments^[1];
  NativeDefineProperties(ThisArgument,@ValuePointers,CountArguments,ResultValue);
 end;
 ResultValue.ValueType:=bvtOBJECT;
 ResultValue.Obj:=o;
end;

procedure TBESENObjectConstructor.NativeDefineProperty(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var n:TBESENString;
    Descriptor:TBESENObjectPropertyDescriptor;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) then begin
  raise EBESENTypeError.Create('No object');
 end;
 if CountArguments>1 then begin
  n:=TBESEN(Instance).ToStr(Arguments^[1]^);
 end else begin
  n:='';
 end;
 if CountArguments>2 then begin
  TBESEN(Instance).ToPropertyDescriptor(Arguments^[2]^,Descriptor);
 end else begin
  Descriptor:=BESENUndefinedPropertyDescriptor;
 end;
 TBESENObject(Arguments^[0]^.Obj).DefineOwnProperty(n,Descriptor,true);
 BESENCopyValue(ResultValue,Arguments^[0]^);
end;

procedure TBESENObjectConstructor.NativeDefineProperties(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Props:TBESENObject;
    Names:TBESENStrings;
    Descriptors:TBESENObjectPropertyDescriptors;
    Enumerator:TBESENObjectPropertyEnumerator;
    i,Count:integer;
    v:TBESENValue;
    Key:TBESENString;
begin
 Names:=nil;
 Descriptors:=nil;
 Enumerator:=nil;
 Key:='';
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) then begin
  raise EBESENTypeError.Create('No object');
 end;
 try
  if CountArguments>1 then begin
   Props:=TBESEN(Instance).ToObj(Arguments^[1]^);
  end else begin
   Props:=TBESEN(Instance).ToObj(BESENUndefinedValue);
  end;
  TBESEN(Instance).GarbageCollector.Add(Props);
  Props.GarbageCollectorLock;
  try
   Count:=0;
   Enumerator:=Props.Enumerator(true,false);
   Enumerator.Reset;
   while Enumerator.GetNext(Key) do begin
    if Count>=length(Names) then begin
     SetLength(Names,Count+256);
    end;
    if Count>=length(Descriptors) then begin
     SetLength(Descriptors,Count+256);
    end;
    Names[Count]:=Key;
    Props.Get(Key,v);
    TBESEN(Instance).ToPropertyDescriptor(v,Descriptors[Count]);
    inc(Count);
   end;
   for i:=0 to Count-1 do begin
    TBESENObject(Arguments^[0]^.Obj).DefineOwnProperty(Names[i],Descriptors[i],true);
   end;
  finally
   Props.GarbageCollectorUnlock;
  end;
 finally
  BESENFreeAndNil(Enumerator);
  SetLength(Names,0);
  SetLength(Descriptors,0);
 end;
 ResultValue:=Arguments^[0]^;
end;


procedure TBESENObjectConstructor.NativeSeal(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Descriptor:TBESENObjectPropertyDescriptor;
    Enumerator:TBESENObjectPropertyEnumerator;
    Key:TBESENString;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) then begin
  raise EBESENTypeError.Create('No object');
 end;
 Enumerator:=nil;
 Key:='';
 try
  Enumerator:=TBESENObject(Arguments^[0]^.Obj).Enumerator(true,true);
  Enumerator.Reset;
  while Enumerator.GetNext(Key) do begin
   TBESENObject(Arguments^[0]^.Obj).GetOwnProperty(Key,Descriptor);
   Descriptor.Attributes:=Descriptor.Attributes-[bopaCONFIGURABLE];
   TBESENObject(Arguments^[0]^.Obj).DefineOwnProperty(Key,Descriptor,true);
  end;
 finally
  BESENFreeAndNil(Enumerator);
 end;
 TBESENObject(Arguments^[0]^.Obj).Extensible:=false;
 BESENCopyValue(ResultValue,Arguments^[0]^);
end;

procedure TBESENObjectConstructor.NativeFreeze(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Descriptor:TBESENObjectPropertyDescriptor;
    Enumerator:TBESENObjectPropertyEnumerator;
    Key:TBESENString;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(TBESENObject(Arguments^[0]^.Obj))) then begin
  raise EBESENTypeError.Create('No object');
 end;
 Enumerator:=nil;
 Key:='';
 try
  Enumerator:=TBESENObject(Arguments^[0]^.Obj).Enumerator(true,true);
  Enumerator.Reset;
  while Enumerator.GetNext(Key) do begin
   TBESENObject(Arguments^[0]^.Obj).GetOwnProperty(Key,Descriptor);
   if ([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[] then begin
    Descriptor.Attributes:=Descriptor.Attributes-[bopaWRITABLE];
   end;
   Descriptor.Attributes:=Descriptor.Attributes-[bopaCONFIGURABLE];
   TBESENObject(Arguments^[0]^.Obj).DefineOwnProperty(Key,Descriptor,true);
  end;
 finally
  BESENFreeAndNil(Enumerator);
 end;
 TBESENObject(Arguments^[0]^.Obj).Extensible:=false;
 BESENCopyValue(ResultValue,Arguments^[0]^);
end;

procedure TBESENObjectConstructor.NativePreventExtensions(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(TBESENObject(Arguments^[0]^.Obj))) then begin
  raise EBESENTypeError.Create('No object');
 end;
 TBESENObject(Arguments^[0]^.Obj).Extensible:=false;
 BESENCopyValue(ResultValue,Arguments^[0]^);
end;

procedure TBESENObjectConstructor.NativeIsSealed(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Descriptor:TBESENObjectPropertyDescriptor;
    Enumerator:TBESENObjectPropertyEnumerator;
    Key:TBESENString;
    IsSealed:boolean;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(TBESENObject(Arguments^[0]^.Obj))) then begin
  raise EBESENTypeError.Create('No object');
 end;
 IsSealed:=true;
 Enumerator:=nil;
 Key:='';
 try
  Enumerator:=TBESENObject(Arguments^[0]^.Obj).Enumerator(true,true);
  Enumerator.Reset;
  while Enumerator.GetNext(Key) do begin
   TBESENObject(Arguments^[0]^.Obj).GetOwnProperty(Key,Descriptor);
   if (boppCONFIGURABLE In Descriptor.Presents) and (bopaCONFIGURABLE In Descriptor.Attributes) then begin
    IsSealed:=false;
    break;
   end;
  end;
 finally
  BESENFreeAndNil(Enumerator);
 end;
 if TBESENObject(Arguments^[0]^.Obj).Extensible then begin
  IsSealed:=false;
 end;
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=IsSealed;
end;

procedure TBESENObjectConstructor.NativeIsFrozen(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Descriptor:TBESENObjectPropertyDescriptor;
    Enumerator:TBESENObjectPropertyEnumerator;
    Key:TBESENString;
    IsFrozen:boolean;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(TBESENObject(Arguments^[0]^.Obj))) then begin
  raise EBESENTypeError.Create('No object');
 end;
 IsFrozen:=true;
 Enumerator:=nil;
 Key:='';
 try
  Enumerator:=TBESENObject(Arguments^[0]^.Obj).Enumerator(true,true);
  Enumerator.Reset;
  while Enumerator.GetNext(Key) do begin
   TBESENObject(Arguments^[0]^.Obj).GetOwnProperty(Key,Descriptor);
   if ((([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[]) and ((boppWRITABLE In Descriptor.Presents) and (bopaWRITABLE In Descriptor.Attributes))) or
      ((boppCONFIGURABLE In Descriptor.Presents) and (bopaCONFIGURABLE In Descriptor.Attributes)) then begin
    IsFrozen:=false;
    break;
   end;
  end;
 finally
  BESENFreeAndNil(Enumerator);
 end;
 if TBESENObject(Arguments^[0]^.Obj).Extensible then begin
  IsFrozen:=false;
 end;
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=IsFrozen;
end;

procedure TBESENObjectConstructor.NativeIsExtensible(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(TBESENObject(Arguments^[0]^.Obj))) then begin
  raise EBESENTypeError.Create('No object');
 end;
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=TBESENObject(Arguments^[0]^.Obj).Extensible;
end;

procedure TBESENObjectConstructor.NativeKeys(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ArrayObject:TBESENObjectArray;
    Enumerator:TBESENObjectPropertyEnumerator;
    Key:TBESENString;
    Index:longword;
begin
 if not ((CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(TBESENObject(Arguments^[0]^.Obj))) then begin
  raise EBESENTypeError.Create('No object');
 end;
 ArrayObject:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(ArrayObject);
 ArrayObject.GarbageCollectorLock;
 try
  Index:=0;
  Enumerator:=nil;
  Key:='';
  try
   Enumerator:=TBESENObject(Arguments^[0]^.Obj).Enumerator(true,false);
   Enumerator.Reset;
   while Enumerator.GetNext(Key) do begin
    ArrayObject.DefineOwnProperty(BESENArrayIndexToStr(Index),BESENDataPropertyDescriptor(BESENStringValue(Key),[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false);
    inc(Index);
   end;
  finally
   BESENFreeAndNil(Enumerator);
  end;
  ArrayObject.Len:=Index;
 finally
  ArrayObject.GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(ArrayObject);
end;

end.
