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
unit BESENObjectNumberConstructor;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectNumberConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
     end;

implementation

uses BESEN,BESENObjectNumber,BESENNumberUtils,BESENGlobals,BESENObjectArray;

constructor TBESENObjectNumberConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
var v:TBESENValue;
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='Number';

 v:=BESENEmptyValue;

 v:=BESENNumberValue(0);
 move(BESENDoubleMax,v.Num,sizeof(TBESENNumber));
 OverwriteData('MAX_VALUE',v,[]);

 v:=BESENNumberValue(0);
 move(BESENDoubleMin,v.Num,sizeof(TBESENNumber));
 OverwriteData('MIN_VALUE',v,[]);

 v:=BESENNumberValue(0);
 move(BESENDoubleNaN,v.Num,sizeof(TBESENNumber));
 OverwriteData('NaN',v,[]);

 v:=BESENNumberValue(0);
 move(BESENDoubleInfNeg,v.Num,sizeof(TBESENNumber));
 OverwriteData('NEGATIVE_INFINITY',v,[]);

 v:=BESENNumberValue(0);
 move(BESENDoubleInfPos,v.Num,sizeof(TBESENNumber));
 OverwriteData('POSITIVE_INFINITY',v,[]);
end;

destructor TBESENObjectNumberConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectNumberConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObjectNumber;
begin
 r1:=TBESENObjectNumber.Create(Instance,TBESEN(Instance).ObjectNumberPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(r1);
 r1.GarbageCollectorLock;
 try
  if CountArguments>0 then begin
   r1.Value:=TBESEN(Instance).ToNum(Arguments^[0]^);
  end else begin
   r1.Value:=0.0;
  end;
 finally
  r1.GarbageCollectorUnlock;
 end;
 AResult.ValueType:=bvtOBJECT;
 AResult.Obj:=r1;
end;

procedure TBESENObjectNumberConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var v:TBESENValue;
begin
 if CountArguments<1 then begin
  AResult.ValueType:=bvtNUMBER;
  AResult.Num:=0;
 end else if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj) and (Arguments^[0]^.Obj is TBESENObjectArray) then begin
  TBESENObjectArray(Arguments^[0]^.Obj).Get('length',v,TBESENObject(Arguments^[0]^.Obj),BESENLengthHash);
  TBESEN(Instance).ToNumberValue(v,AResult);
 end else begin
  TBESEN(Instance).ToNumberValue(Arguments^[0]^,AResult);
 end;
end;

function TBESENObjectNumberConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectNumberConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

end.
