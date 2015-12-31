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
unit BESENObjectArrayConstructor;
{$i BESEN.inc}

interface

uses SysUtils,Math,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectArrayConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       procedure NativeIsArray(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENGlobals,BESENObjectArray,BESENNumberUtils,BESENErrors;

constructor TBESENObjectArrayConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);

 ObjectClassName:='Function';
 ObjectName:='Array';

 RegisterNativeFunction('isArray',NativeIsArray,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectArrayConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectArrayConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObjectArray;
    r3:TBESENValue;
    l:int64;
    i:integer;
begin
 r3:=BESENEmptyValue;
 r1:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(r1);
 if CountArguments>0 then begin
  r1.GarbageCollectorLock;
  try
   if (CountArguments=1) and (Arguments^[0]^.ValueType=bvtNUMBER) then begin
    r3:=Arguments^[0]^;
    l:=TBESEN(Instance).ToUInt32(r3);
    if (l<>r3.Num) or not BESENIsFinite(r3.Num) then begin
     raise EBESENRangeError.Create('Bad array length');
    end;
    r1.Put('length',r3,true,BESENLengthHash);
   end else begin
    for i:=0 to CountArguments-1 do begin
     r1.DefineOwnProperty(inttostr(i),BESENDataPropertyDescriptor(Arguments^[i]^,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),true);
    end;
    r3:=BESENNumberValue(CountArguments);
    r1.Put('length',r3,true,BESENLengthHash);
   end;
  finally
   r1.GarbageCollectorUnlock;
  end;
 end;
 AResult.ValueType:=bvtOBJECT;
 AResult.Obj:=r1;
end;

procedure TBESENObjectArrayConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 Construct(ThisArgument,Arguments,CountArguments,AResult);
end;

function TBESENObjectArrayConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectArrayConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectArrayConstructor.NativeIsArray(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=(CountArguments>0) and (((Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj)) and (TBESENObject(Arguments^[0]^.Obj).ObjectClassName='Array'));
end;

end.
