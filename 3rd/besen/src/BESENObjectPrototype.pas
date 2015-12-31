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
unit BESENObjectPrototype;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectPrototype=class(TBESENObject)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToSource(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeHasOwnProperty(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeIsPrototypeOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativePropertyIsEnumerable(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENErrors;

constructor TBESENObjectPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectName:='prototype';

 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;

 RegisterNativeFunction('toString',NativeToString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toLocaleString',NativeToLocaleString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toSource',NativeToSource,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('valueOf',NativeValueOf,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('hasOwnProperty',NativeHasOwnProperty,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('isPrototypeOf',NativeIsPrototypeOf,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('propertyIsEnumerable',NativePropertyIsEnumerable,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var o:TBesenObject;
begin
 // ES5 errata fix
 case ThisArgument.ValueType of
  bvtUNDEFINED:begin
   ResultValue:=BESENStringValue('[object Undefined]');
  end;
  bvtNULL:begin
   ResultValue:=BESENStringValue('[object Null]');
  end;
  else begin
   o:=TBESEN(Instance).ToObj(ThisArgument);
   o.GarbageCollectorLock;
   try
    if assigned(o) then begin
     if length(o.ObjectClassName)>0 then begin
      ResultValue:=BESENStringValue('[object '+o.ObjectClassName+']');
     end else begin
      ResultValue:=BESENStringValue('[object Object]');
     end;
    end else begin
     BESENThrowTypeError('Null this object');
    end;
   finally
    o.GarbageCollectorUnlock;
   end;
  end;
 end;
end;

procedure TBESENObjectPrototype.NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v:TBESENValue;
    o:TBesenObject;
begin
 o:=TBESEN(Instance).ToObj(ThisArgument);
 if assigned(o) then begin
  o.GarbageCollectorLock;
  try
   o.Get('toString',v);
   if (v.ValueType=bvtOBJECT) and assigned(TBESENObject(v.Obj)) and TBESENObject(v.Obj).HasCall then begin
    TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),ThisArgument,Arguments,CountArguments,ResultValue);
   end else begin
    BESENThrowTypeError('Null this object');
   end;
  finally
   o.GarbageCollectorUnlock;
  end;
 end else begin
  BESENThrowTypeError('Null this object');
 end;
end;

procedure TBESENObjectPrototype.NativeToSource(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue:=BESENStringValue('/* Unimplemented */');
end;

procedure TBESENObjectPrototype.NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ResultValue);
end;

procedure TBESENObjectPrototype.NativeHasOwnProperty(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Descriptor:TBESENObjectPropertyDescriptor;
    o:TBesenObject;
begin
 o:=TBESEN(Instance).ToObj(ThisArgument);
 if not assigned(o) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 o.GarbageCollectorLock;
 try
  ResultValue.ValueType:=bvtBOOLEAN;
  if CountArguments>0 then begin
   ResultValue.Bool:=o.GetOwnProperty(TBESEN(Instance).ToStr(Arguments^[0]^),Descriptor);
  end else begin
   ResultValue.Bool:=false;
  end;
 finally
  o.GarbageCollectorUnlock;
 end;
end;

procedure TBESENObjectPrototype.NativeIsPrototypeOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v,o:TBESENObject;
begin
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=false;
 if (CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) then begin
  o:=TBESEN(Instance).ToObj(ThisArgument);
  if not assigned(o) then begin
   raise EBESENTypeError.Create('Null this object');
  end;
  o.GarbageCollectorLock;
  try
   v:=TBESENObject(Arguments^[0]^.Obj);
   while assigned(v) do begin
    v:=v.Prototype;
    if assigned(v) then begin
     if o=v then begin
      ResultValue.Bool:=true;
      break;
     end;
    end else begin
     ResultValue.Bool:=false;
     break;
    end;
   end;
  finally
   o.GarbageCollectorUnlock;
  end;
 end else begin
  ResultValue.Bool:=false;
 end;
end;

procedure TBESENObjectPrototype.NativePropertyIsEnumerable(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var Descriptor:TBESENObjectPropertyDescriptor;
    o:TBESENObject;
    s:TBESENString;
begin
 ResultValue.ValueType:=bvtBOOLEAN;
 if CountArguments>0 then begin
  s:=TBESEN(Instance).ToStr(Arguments^[0]^);
  o:=TBESEN(Instance).ToObj(ThisArgument);
  if not assigned(o) then begin
   raise EBESENTypeError.Create('Null this object');
  end;
  o.GarbageCollectorLock;
  try
   if o.GetOwnProperty(s,Descriptor) then begin
    ResultValue.Bool:=(boppENUMERABLE in Descriptor.Presents) and (bopaENUMERABLE in Descriptor.Attributes);
   end else begin
    ResultValue.Bool:=false;
   end;
  finally
   o.GarbageCollectorUnlock;
  end;
 end else begin
  ResultValue.Bool:=false;
 end;
end;

end.
 