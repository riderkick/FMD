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
unit BESENCodeGeneratorContext;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENHashMap;

type TBESENCodeGeneratorContextVariableScope=record
      Ident:TBESENString;
      ID:integer;
      InScope:longbool;
      ValueType:TBESENValueType;
      Initialized:longbool;
      MutableOrDeletable:longbool;
      RegNr:integer;
     end;

     TBESENCodeGeneratorContextVariableScopes=array of TBESENCodeGeneratorContextVariableScope;

     TBESENCodeGeneratorContextPatchables=class(TBESENBaseObject)
      public
       Continues:TBESENIntegers;
       Breaks:TBESENIntegers;
       CountContinues:integer;
       CountBreaks:integer;
       Previous:TBESENCodeGeneratorContextPatchables;
       Continuable:longbool;
       BlockDepth:integer;
       Target:integer;
       ContinueValueTypesItems:TBESENValueTypesItems;
       BreakValueTypesItems:TBESENValueTypesItems;
       CountContinueValueTypesItems:integer;
       CountBreakValueTypesItems:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure AddContinue(Address:integer;const ValueTypes:TBESENValueTypes);
       procedure AddBreak(Address:integer;const ValueTypes:TBESENValueTypes);
     end;

     TBESENCodeGeneratorContextRegister=record
      IsWhat:longword;
      IsLocal:longbool;
      LocalIndex:longint;
      ReferenceValueType:TBESENValueType;
      InUse:longbool;
      Variable:longint;
     end;

     TBESENCodeGeneratorContextRegisters=array of TBESENCodeGeneratorContextRegister;

     TBESENCodeGeneratorContextRegisterStates=record
      Registers:TBESENCodeGeneratorContextRegisters;
      MaxRegisters:integer;
     end;

     TBESENCodeGeneratorContext=class(TBESENBaseObject)
      public
       Next:TBESENCodeGeneratorContext;
       Code:TObject;
       BlockDepth:integer;
       MaxBlockDepth:integer;
       LoopDepth:integer;
       MaxLoopDepth:integer;
       MaxParamArgs:integer;
       Patchables:TBESENCodeGeneratorContextPatchables;
       VariableScopes:TBESENCodeGeneratorContextVariableScopes;
       CountVariableScopes:integer;
       InVariableScope:longbool;
       VariableScopeHashMap:TBESENHashMap;
       VariableNameHashMap:TBESENHashMap;
       Registers:TBESENCodeGeneratorContextRegisters;
       MaxRegisters:integer;
       LookupHashMap:TBESENHashMap;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure PushPatchables(Target:integer;Continuable:boolean);
       procedure PopPatchables(ContinueAddr,BreakAddr:integer);
       function FindPatchables(Target:integer;Continuable:boolean):TBESENCodeGeneratorContextPatchables;
       procedure BlockEnter;
       procedure BlockLeave;
       function BlockCurrent:integer;
       function AllocateRegister:integer;
       procedure DeallocateRegister(var RegNr:integer);
       procedure GetRegisterStates(var RegisterStates:TBESENCodeGeneratorContextRegisterStates);
       procedure SetRegisterStates(var RegisterStates:TBESENCodeGeneratorContextRegisterStates);
       function VariableIndex(const Ident:TBESENString):integer;
       function VariableGetInitialized(const Ident:TBESENString):TBESENBoolean;
       function VariableGetMutableOrDeletable(const Ident:TBESENString):TBESENBoolean;
       procedure VariableSetFlags(const Ident:TBESENString;const Initialized,MutableOrDeletable:boolean);
       function VariableGetRegister(const Ident:TBESENString):integer;
       procedure VariableSetRegister(const Ident:TBESENString;const RegNr:integer);
       function VariableGetType(const Ident:TBESENString):TBESENValueType;
       procedure VariableSetType(const Ident:TBESENString;const ValueType:TBESENValueType);
       procedure VariableAllSetType(const ValueType:TBESENValueType);
       function VariableGetTypes:TBESENValueTypes;
       procedure VariableSetTypes(const ValueTypes:TBESENValueTypes);
       function VariableGetIdent(const Index:integer):TBESENString;
       function VariableID(const Ident:TBESENString):integer;
       function IsVariableInScope(const Ident:TBESENString):boolean;
       procedure VariableSetScope(const Ident:TBESENString;IsInScope,IsParameter:boolean;ParameterIndex:integer);
       function VariableSetAllScope(IsInScope:boolean):boolean;
     end;

implementation

uses BESEN,BESENUtils,BESENCode;

constructor TBESENCodeGeneratorContextPatchables.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Continues:=nil;
 Breaks:=nil;
 CountContinues:=0;
 CountBreaks:=0;
 Previous:=nil;
 Continuable:=false;
 BlockDepth:=0;
 Target:=0;
 ContinueValueTypesItems:=nil;
 BreakValueTypesItems:=nil;
 CountContinueValueTypesItems:=0;
 CountBreakValueTypesItems:=0;
end;

destructor TBESENCodeGeneratorContextPatchables.Destroy;
begin
 SetLength(Continues,0);
 SetLength(Breaks,0);
 SetLength(ContinueValueTypesItems,0);
 SetLength(BreakValueTypesItems,0);
 inherited Destroy;
end;

procedure TBESENCodeGeneratorContextPatchables.AddContinue(Address:integer;const ValueTypes:TBESENValueTypes);
begin
 if CountContinues>=length(Continues) then begin
  SetLength(Continues,CountContinues+256);
 end;
 Continues[CountContinues]:=Address;
 inc(CountContinues);
 if CountContinueValueTypesItems>=length(ContinueValueTypesItems) then begin
  SetLength(ContinueValueTypesItems,CountContinueValueTypesItems+256);
 end;
 ContinueValueTypesItems[CountContinueValueTypesItems]:=copy(ValueTypes,0,length(ValueTypes));
 inc(CountContinueValueTypesItems);
end;

procedure TBESENCodeGeneratorContextPatchables.AddBreak(Address:integer;const ValueTypes:TBESENValueTypes);
begin
 if CountBreaks>=length(Breaks) then begin
  SetLength(Breaks,CountBreaks+256);
 end;
 Breaks[CountBreaks]:=Address;
 inc(CountBreaks);
 if CountBreakValueTypesItems>=length(BreakValueTypesItems) then begin
  SetLength(BreakValueTypesItems,CountBreakValueTypesItems+256);
 end;
 BreakValueTypesItems[CountBreakValueTypesItems]:=copy(ValueTypes,0,length(ValueTypes));
 inc(CountBreakValueTypesItems);
end;

constructor TBESENCodeGeneratorContext.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Next:=nil;
 Code:=nil;
 BlockDepth:=0;
 MaxBlockDepth:=0;
 LoopDepth:=0;
 MaxLoopDepth:=0;
 MaxParamArgs:=0;
 Patchables:=nil;
 VariableScopes:=nil;
 CountVariableScopes:=0;
 InVariableScope:=true;
 VariableScopeHashMap:=TBESENHashMap.Create;
 VariableNameHashMap:=TBESENHashMap.Create;
 Registers:=nil;
 MaxRegisters:=0;
 LookupHashMap:=TBESENHashMap.Create;
end;

destructor TBESENCodeGeneratorContext.Destroy;
var NextPatchables:TBESENCodeGeneratorContextPatchables;
begin
 BESENFreeAndNil(LookupHashMap);
 while assigned(Patchables) do begin
  NextPatchables:=Patchables.Previous;
  Patchables.Free;
  Patchables:=NextPatchables;
 end;
 SetLength(Registers,0);
 SetLength(VariableScopes,0);
 VariableScopeHashMap.Free;
 VariableNameHashMap.Free;
 inherited Destroy;
end;

procedure TBESENCodeGeneratorContext.PushPatchables(Target:integer;Continuable:boolean);
var p:TBESENCodeGeneratorContextPatchables;
begin
 p:=TBESENCodeGeneratorContextPatchables.Create(Instance);
 p.Target:=Target;
 p.Continuable:=Continuable;
 p.Previous:=Patchables;
 p.BlockDepth:=BlockDepth;
 Patchables:=p;
end;

procedure TBESENCodeGeneratorContext.PopPatchables(ContinueAddr,BreakAddr:integer);
var i:integer;
    p:TBESENCodeGeneratorContextPatchables;
begin
 p:=Patchables;
 if ContinueAddr>=0 then begin
  for i:=0 to p.CountContinues-1 do begin
   TBESENCode(Code).Patch(p.Continues[i],ContinueAddr);
  end;
 end;
 if BreakAddr>=0 then begin
  for i:=0 to p.CountBreaks-1 do begin
   TBESENCode(Code).Patch(p.Breaks[i],BreakAddr);
  end;
 end;
 Patchables:=p.Previous;
 BESENFreeAndNil(p);
end;

function TBESENCodeGeneratorContext.FindPatchables(Target:integer;Continuable:boolean):TBESENCodeGeneratorContextPatchables;
var p:TBESENCodeGeneratorContextPatchables;
begin
 result:=nil;
 if (Target=bcttNOTARGET) and Continuable then begin
  p:=Patchables;
  while assigned(p) do begin
   if p.Continuable then begin
    result:=p;
    break;
   end;
   p:=p.Previous;
  end;
 end else if Target=bcttNOTARGET then begin
  result:=Patchables;
 end else begin
  p:=Patchables;
  while assigned(p) do begin
   if p.Target=Target then begin
    result:=p;
    break;
   end;
   p:=p.Previous;
  end;
 end;
{$ifdef UseAssert}
 Assert(assigned(result),'Lost patchable');
{$endif}
end;

procedure TBESENCodeGeneratorContext.BlockEnter;
begin
 inc(BlockDepth);
 if MaxBlockDepth<BlockDepth then begin
  MaxBlockDepth:=BlockDepth;
 end;
end;

procedure TBESENCodeGeneratorContext.BlockLeave;
begin
 dec(BlockDepth);
end;

function TBESENCodeGeneratorContext.BlockCurrent:integer;
begin
 result:=BlockDepth;
end;

function TBESENCodeGeneratorContext.AllocateRegister:integer;
var i:integer;
begin
 result:=-1;
 for i:=0 to MaxRegisters-1 do begin
  if not Registers[i].InUse then begin
   result:=i;
   break;
  end;
 end;
 if result<0 then begin
  result:=MaxRegisters;
  inc(MaxRegisters);
  if result>=length(Registers) then begin
   SetLength(Registers,result+256);
  end;
 end;
 Registers[result].InUse:=true;
 Registers[result].IsWhat:=0;
 Registers[result].IsLocal:=false;
 Registers[result].LocalIndex:=-1;
 Registers[result].Variable:=-1;
end;

procedure TBESENCodeGeneratorContext.DeallocateRegister(var RegNr:integer);
begin
 if ((RegNr>=0) and (RegNr<MaxRegisters)) and not (Registers[RegNr].Variable>=0) then begin
  Registers[RegNr].InUse:=false;
  Registers[RegNr].IsWhat:=0;
  Registers[RegNr].IsLocal:=false;
  Registers[RegNr].LocalIndex:=-1;
 end;
 RegNr:=-1;
end;

procedure TBESENCodeGeneratorContext.GetRegisterStates(var RegisterStates:TBESENCodeGeneratorContextRegisterStates);
begin
 RegisterStates.Registers:=copy(Registers,0,length(Registers));
 RegisterStates.MaxRegisters:=MaxRegisters;
end;

procedure TBESENCodeGeneratorContext.SetRegisterStates(var RegisterStates:TBESENCodeGeneratorContextRegisterStates);
begin
 Registers:=copy(RegisterStates.Registers,0,length(RegisterStates.Registers));
 MaxRegisters:=RegisterStates.MaxRegisters;
end;

function TBESENCodeGeneratorContext.VariableIndex(const Ident:TBESENString):integer;
var Item:PBESENHashMapItem;
begin
 Item:=VariableScopeHashMap.GetKey(Ident);
 if assigned(Item) then begin
  result:=Item^.Value;
 end else begin
  result:=-1;
 end;
end;

function TBESENCodeGeneratorContext.VariableGetInitialized(const Ident:TBESENString):TBESENBoolean;
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  result:=VariableScopes[i].Initialized;
 end else begin
  result:=false;
 end;
end;

function TBESENCodeGeneratorContext.VariableGetMutableOrDeletable(const Ident:TBESENString):TBESENBoolean;
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  result:=VariableScopes[i].MutableOrDeletable;
 end else begin
  result:=false;
 end;
end;

procedure TBESENCodeGeneratorContext.VariableSetFlags(const Ident:TBESENString;const Initialized,MutableOrDeletable:boolean);
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  VariableScopes[i].Initialized:=Initialized;
  VariableScopes[i].MutableOrDeletable:=MutableOrDeletable;
 end;
end;

function TBESENCodeGeneratorContext.VariableGetRegister(const Ident:TBESENString):integer;
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  result:=VariableScopes[i].RegNr;
 end else begin
  result:=-1;
 end;
end;

procedure TBESENCodeGeneratorContext.VariableSetRegister(const Ident:TBESENString;const RegNr:integer);
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  VariableScopes[i].RegNr:=RegNr;
 end;
end;

function TBESENCodeGeneratorContext.VariableGetType(const Ident:TBESENString):TBESENValueType;
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  result:=VariableScopes[i].ValueType;
 end else begin
  result:=bvtUNDEFINED;
 end;
end;

procedure TBESENCodeGeneratorContext.VariableSetType(const Ident:TBESENString;const ValueType:TBESENValueType);
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  VariableScopes[i].ValueType:=ValueType;
 end;
end;

procedure TBESENCodeGeneratorContext.VariableAllSetType(const ValueType:TBESENValueType);
var i:integer;
begin
 for i:=0 to CountVariableScopes-1 do begin
  VariableScopes[i].ValueType:=ValueType;
 end;
end;

function TBESENCodeGeneratorContext.VariableGetTypes:TBESENValueTypes;
var i:integer;
begin
 SetLength(result,CountVariableScopes);
 for i:=0 to CountVariableScopes-1 do begin
  result[i]:=VariableScopes[i].ValueType;
 end;
end;

procedure TBESENCodeGeneratorContext.VariableSetTypes(const ValueTypes:TBESENValueTypes);
var i,j:integer;
begin
 j:=CountVariableScopes;
 if j>length(ValueTypes) then begin
  j:=length(ValueTypes);
 end;
 for i:=0 to j-1 do begin
  VariableScopes[i].ValueType:=ValueTypes[i];
 end;
end;

function TBESENCodeGeneratorContext.VariableGetIdent(const Index:integer):TBESENString;
begin
 if (Index>=0) and (Index<CountVariableScopes) then begin
  result:=VariableScopes[Index].Ident;
 end else begin
  result:='';
 end;
end;

function TBESENCodeGeneratorContext.VariableID(const Ident:TBESENString):integer;
var i:integer;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  result:=VariableScopes[i].ID;
 end else begin
  result:=-1;
 end;
end;

function TBESENCodeGeneratorContext.IsVariableInScope(const Ident:TBESENString):boolean;
var i:integer;
begin
 result:=false;
 if InVariableScope then begin
  i:=VariableIndex(Ident);
  if i>=0 then begin
   result:=VariableScopes[i].InScope;
  end;
 end;
end;

procedure TBESENCodeGeneratorContext.VariableSetScope(const Ident:TBESENString;IsInScope,IsParameter:boolean;ParameterIndex:integer);
var i:integer;
    Item:PBESENHashMapItem;
begin
 i:=VariableIndex(Ident);
 if i>=0 then begin
  VariableScopes[i].InScope:=IsInScope;
 end else begin
  if IsInScope then begin
   if CountVariableScopes>=length(VariableScopes) then begin
    SetLength(VariableScopes,CountVariableScopes+256);
   end;
   VariableScopes[CountVariableScopes].Ident:=Ident;
   VariableScopes[CountVariableScopes].ID:=TBESENCode(Code).GenVariable(Ident,IsParameter,ParameterIndex,self);
   VariableScopes[CountVariableScopes].InScope:=IsInScope;
   VariableScopes[CountVariableScopes].Initialized:=false;
   VariableScopes[CountVariableScopes].MutableOrDeletable:=false;
   VariableScopes[CountVariableScopes].ValueType:=bvtUNDEFINED;
   VariableScopes[CountVariableScopes].RegNr:=-1;
   Item:=VariableScopeHashMap.NewKey(Ident,true);
   Item^.Value:=CountVariableScopes;
   inc(CountVariableScopes);
  end;
 end;
end;

function TBESENCodeGeneratorContext.VariableSetAllScope(IsInScope:boolean):boolean;
begin
 result:=InVariableScope;
 InVariableScope:=IsInScope;
end;

end.
