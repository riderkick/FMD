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
unit BESENObjectFunctionPrototype;
{$i BESEN.inc}

interface

uses SysUtils,Math,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor,
     BESENObjectNativeFunction;

type TBESENObjectFunctionPrototype=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasCall:TBESENBoolean; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeApply(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeCall(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeBind(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENGlobals,BESENStringUtils,BESENErrors,BESENObjectDeclaredFunction,
     BESENObjectThrowTypeErrorFunction,BESENObjectArgGetterFunction,
     BESENObjectArgSetterFunction,BESENObjectBindingFunction;

constructor TBESENObjectFunctionPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin                                  
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='prototype';

 OverwriteData('length',BESENNumberValue(0),[]);
 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;
end;

destructor TBESENObjectFunctionPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectFunctionPrototype.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 AResult:=BESENUndefinedValue;
end;

function TBESENObjectFunctionPrototype.HasCall:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectFunctionPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ThisArgumentObj:TObject;
begin
 ThisArgumentObj:=ThisArgument.Obj;
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(ThisArgumentObj)) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectDeclaredFunction) then begin
  if assigned(TBESENObjectDeclaredFunction(ThisArgumentObj).Node) then begin
   ResultValue:=BESENStringValue(BESENUTF8ToUTF16(TBESEN(Instance).Decompile(TBESENObjectDeclaredFunction(ThisArgumentObj).Node)));
  end else begin
   ResultValue:=BESENStringValue('function () {'#10'}'#10);
  end;
 end else if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectNativeFunction) then begin
  ResultValue:=BESENStringValue('function () {'#10#9'[native code]'#10'}'#10);
 end else if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectThrowTypeErrorFunction) then begin
  ResultValue:=BESENStringValue('function () {'#10#9'[native code, ThrowTypeError]'#10'}'#10);
 end else if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectArgGetterFunction) then begin
  ResultValue:=BESENStringValue('function () {'#10#9'[native code, ArgGetter]'#10'}'#10);
 end else if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectArgSetterFunction) then begin
  ResultValue:=BESENStringValue('function () {'#10#9'[native code, ArgSetter]'#10'}'#10);
 end else if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectBindingFunction) then begin
  ResultValue:=BESENStringValue('function () {'#10#9'[native code, Binding]'#10'}'#10);
 end else if assigned(ThisArgumentObj) and (ThisArgumentObj is TBESENObjectFunction) then begin
  ResultValue:=BESENStringValue('function () {'#10'}'#10);
 end else begin
  raise EBESENTypeError.Create('Not a function object');
 end;
end;

procedure TBESENObjectFunctionPrototype.NativeApply(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var CallThisArg:TBESENValue;
    v2,v3:TBESENValue;
    vArgs:TBESENValues;
    pArgs:TBESENValuePointers;
    i,j:integer;
begin
 // ES5 errata fix
 vArgs:=nil;
 pArgs:=nil;
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(ThisArgument.Obj)) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if not TBESENObject(ThisArgument.Obj).HasCall then begin
  raise EBESENTypeError.Create('No callable');
 end;
 v2:=BESENEmptyValue;
 v3:=BESENEmptyValue;
 try
  if CountArguments<1 then begin
   CallThisArg:=BESENUndefinedValue;
  end else begin
   BESENCopyValue(CallThisArg,Arguments^[0]^);
   if (CountArguments>1) and not (Arguments^[1]^.ValueType in [bvtUNDEFINED,bvtNULL]) then begin
    TBESEN(Instance).ToObjectValue(Arguments^[1]^,v2);
    if (v2.ValueType=bvtOBJECT) and assigned(v2.Obj) then begin
     TBESENObject(v2.Obj).GarbageCollectorLock;
    end;
    TBESENObject(v2.Obj).Get('length',v3,TBESENObject(v2.Obj),BESENLengthHash);
    j:=TBESEN(Instance).ToUInt32(v3);
    SetLength(vArgs,j);
    SetLength(pArgs,j);
    for i:=0 to j-1 do begin
     TBESENObject(v2.Obj).Get(inttostr(i),vArgs[i]);
     pArgs[i]:=@vArgs[i];
    end;
   end;
  end;
  TBESEN(Instance).ObjectCall(TBESENObject(ThisArgument.Obj),CallThisArg,@pArgs[0],length(pArgs),ResultValue);
 finally
  if (v2.ValueType=bvtOBJECT) and assigned(v2.Obj) then begin
   TBESENObject(v2.Obj).GarbageCollectorUnlock;
  end;
  SetLength(vArgs,0);
  SetLength(pArgs,0);
 end;
end;

procedure TBESENObjectFunctionPrototype.NativeCall(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var CallThisArg:TBESENValue;
    pArgs:TBESENValuePointers;
    i:integer;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(ThisArgument.Obj)) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if not TBESENObject(ThisArgument.Obj).HasCall then begin
  raise EBESENTypeError.Create('No callable');
 end;
 pArgs:=nil;
 try
  if CountArguments<1 then begin
   CallThisArg:=BESENUndefinedValue;
  end else begin
   BESENCopyValue(CallThisArg,Arguments^[0]^);
   if CountArguments>1 then begin
    SetLength(pArgs,CountArguments-1);
    for i:=0 to length(pArgs)-1 do begin
     pArgs[i]:=Arguments[i+1];
    end;
   end;
  end;
  TBESEN(Instance).ObjectCall(TBESENObject(ThisArgument.Obj),CallThisArg,@pArgs[0],length(pArgs),ResultValue);
 finally
  SetLength(pArgs,0);
 end;
end;

procedure TBESENObjectFunctionPrototype.NativeBind(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var o:TBESENObjectBindingFunction;
    i:integer;
    v:TBESENValue;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(ThisArgument.Obj)) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if not TBESENObject(ThisArgument.Obj).HasCall then begin
  raise EBESENTypeError.Create('Bad arg');
 end;                                                            
 o:=TBESENObjectBindingFunction.Create(Instance,TBESEN(Instance).ObjectFunctionPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(o);
 o.GarbageCollectorLock;
 try
  o.TargetFunction:=TBESENObject(ThisArgument.Obj);
  if CountArguments>0 then begin
   BESENCopyValue(o.BoundThis,Arguments^[0]^);
  end else begin
   o.BoundThis.ValueType:=bvtUNDEFINED;
  end;
  if CountArguments>1 then begin
   SetLength(o.BoundArguments,CountArguments-1);
   for i:=1 to CountArguments-1 do begin
    BESENCopyValue(o.BoundArguments[i-1],Arguments^[i]^);
   end;
  end else begin
   SetLength(o.BoundArguments,0);
  end;
  if o.TargetFunction is TBESENObjectFunction then begin
   TBESENObjectFunction(o.TargetFunction).Get('length',v,o.TargetFunction,BESENLengthHash);
   o.OverwriteData('length',BESENNumberValue(max(0,TBESEN(Instance).ToInt(v)-length(o.BoundArguments))),[]);
  end else begin
   o.OverwriteData('length',BESENNumberValue(0),[]);
  end;
  o.Extensible:=true;
  o.OverwriteAccessor('caller',TBESEN(Instance).ObjectThrowTypeErrorFunction,TBESEN(Instance).ObjectThrowTypeErrorFunction,[],false);
  o.OverwriteAccessor('arguments',TBESEN(Instance).ObjectThrowTypeErrorFunction,TBESEN(Instance).ObjectThrowTypeErrorFunction,[],false);
 finally
  o.GarbageCollectorUnlock;
 end;
 ResultValue:=BESENObjectValue(o);
end;

end.
 