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
unit BESENObjectBindingFunction;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor,BESENEnvironmentRecord;

type TBESENObjectBindingFunction=class(TBESENObjectFunction)
      public
       TargetFunction:TBESENObject;
       BoundThis:TBESENValue;
       BoundArguments:TBESENValues;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       function GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean; override;
       function GetIndex(const Index,ID:integer;var AResult:TBESENValue;Base:TBESENObject=nil):boolean; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasInstance(const AInstance:TBESENValue):TBESENBoolean; override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       function HasHasInstance:TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENErrors,BESENHashUtils;

constructor TBESENObjectBindingFunction.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='Binding';
 TargetFunction:=nil;
 BoundThis:=BESENUndefinedValue;
 BoundArguments:=nil;
end;

destructor TBESENObjectBindingFunction.Destroy;
begin
 TargetFunction:=nil;
 BoundThis:=BESENUndefinedValue;
 SetLength(BoundArguments,0);
 inherited Destroy;
end;

function TBESENObjectBindingFunction.GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 result:=inherited GetEx(p,AResult,Descriptor,Base,Hash);
end;

function TBESENObjectBindingFunction.GetIndex(const Index,ID:integer;var AResult:TBESENValue;Base:TBESENObject=nil):boolean;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 result:=inherited GetIndex(Index,ID,AResult,Base);
end;

procedure TBESENObjectBindingFunction.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var CallArgs:TBESENValuePointers;
    i,j:integer;
begin
 CallArgs:=nil;
 if assigned(TargetFunction) and TargetFunction.HasConstruct then begin
  SetLength(CallArgs,length(BoundArguments)+CountArguments);
  try
   j:=0;
   for i:=0 to length(BoundArguments)-1 do begin
    CallArgs[j]:=@BoundArguments[i];
    inc(j);
   end;
   for i:=0 to CountArguments-1 do begin
    CallArgs[j]:=Arguments^[i];
    inc(j);
   end;
   TargetFunction.Construct(BoundThis,@CallArgs[0],length(CallArgs),AResult);
  finally
   SetLength(CallArgs,0);
  end;
 end else begin
  raise EBESENTypeError.Create('No hasConstruct');
 end;
end;

procedure TBESENObjectBindingFunction.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var CallArgs:TBESENValuePointers;
    i,j:integer;
begin
 CallArgs:=nil;
 if assigned(TargetFunction) and TargetFunction.HasCall then begin
  SetLength(CallArgs,length(BoundArguments)+CountArguments);
  try
   j:=0;
   for i:=0 to length(BoundArguments)-1 do begin
    CallArgs[j]:=@BoundArguments[i];
    inc(j);
   end;
   for i:=0 to CountArguments-1 do begin
    CallArgs[j]:=Arguments^[i];
    inc(j);
   end;
   TBESEN(Instance).ObjectCall(TargetFunction,BoundThis,@CallArgs[0],length(CallArgs),AResult);
  finally
   SetLength(CallArgs,0);
  end;
 end else begin
  AResult.ValueType:=bvtUNDEFINED;
 end;
end;

function TBESENObjectBindingFunction.HasInstance(const AInstance:TBESENValue):TBESENBoolean;
begin
 if not TargetFunction.HasHasInstance then begin
  raise EBESENTypeError.Create('No hasInstance');
 end;
 result:=assigned(TargetFunction) and TargetFunction.HasInstance(AInstance);
end;

function TBESENObjectBindingFunction.HasConstruct:TBESENBoolean;
begin
 result:=assigned(TargetFunction) and TargetFunction.HasConstruct;
end;

function TBESENObjectBindingFunction.HasCall:TBESENBoolean;
begin
 result:=assigned(TargetFunction) and TargetFunction.HasCall;
end;

function TBESENObjectBindingFunction.HasHasInstance:TBESENBoolean;
begin
 result:=assigned(TargetFunction) and TargetFunction.HasHasInstance;
end;

procedure TBESENObjectBindingFunction.Finalize;
begin
 TargetFunction:=nil;
 BoundThis:=BESENUndefinedValue;
 SetLength(BoundArguments,0);
 inherited Finalize;
end;

procedure TBESENObjectBindingFunction.Mark;
var i:integer;
begin
 if assigned(TargetFunction) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(TargetFunction);
 end;
 TBESEN(Instance).GarbageCollector.GrayValue(BoundThis);
 for i:=0 to length(BoundArguments)-1 do begin
  TBESEN(Instance).GarbageCollector.GrayValue(BoundArguments[i]);
 end;
 inherited Mark;
end;

end.
