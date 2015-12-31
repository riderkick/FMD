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
unit BESENCode;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENASTNodes,BESENStringList,
     BESENHashMap;

type TBESENCodeVariable=record
      Name:TBESENString;
      IsParameter:longbool;
      ParameterIndex:integer;
     end;

     TBESENCodeVariables=array of TBESENCodeVariable;

     PBESENCodePolymorphicInlineCacheItem=^TBESENCodePolymorphicInlineCacheItem;
     TBESENCodePolymorphicInlineCacheItem=record
      StructureID:TBESENINT32;
      Index:TBESENINT32;
      ID:TBESENINT32;
      NestedLevel:TBESENINT32;
     end;

     TBESENCodePolymorphicInlineCacheItems=array[0..BESENPolymorphicInlineCacheSize-1] of TBESENCodePolymorphicInlineCacheItem;

     PBESENCodePolymorphicInlineCacheInstruction=^TBESENCodePolymorphicInlineCacheInstruction;
     TBESENCodePolymorphicInlineCacheInstruction=record
      CacheItems:TBESENCodePolymorphicInlineCacheItems;
      CacheItemPositions:TBESENUINT32;
     end;

     TBESENCodePolymorphicInlineCacheInstructions=array of PBESENCodePolymorphicInlineCacheInstruction;

     TBESENCode=class(TBESENBaseObject)
      public
       CodePrevious,CodeNext:TBESENCode;
       ByteCode:TBESENUINT32s;
       ByteCodeLen:integer;
       NativeCode:pointer;
       NativeCodeSize:ptruint;
       NativeCodePCOffsets:TBESENNativeCodePCOffsets;
       Body:TBESENASTNodeFunctionBody;
       FunctionLiteralContainers:TBESENFunctionLiteralContainers;
       Literals:TBESENValues;
       Locations:TBESENLocations;
       Variables:TBESENCodeVariables;
       PolymorphicInlineCacheInstructions:TBESENCodePolymorphicInlineCacheInstructions;
       CountFunctionLiteralContainers:integer;
       CountLiterals:integer;
       CountLocations:integer;
       CountVariables:integer;
       CountPolymorphicInlineCacheInstructions:integer;
       MaxRegisters:integer;
       MaxBlock:integer;
       MaxParamArgs:integer;
       MaxLoop:integer;
       HasLocalDelete:TBESENBoolean;
       IsComplexFunction:TBESENBoolean;
       HasMaybeDirectEval:TBESENBoolean;
       HoldLocalVariablesInRegisters:TBESENBoolean;
       LastCodePos:integer;
       LastOpcode:integer;
       LastLine:integer;
       LookupNames:TBESENStringList;
       FreeCodeContexts:TObject;
       CountOfFreeCodeContexts:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Mark;
       procedure Finish;
       procedure ResetPolymorphicInlineCacheInstructions;
       function Put(v:TBESENUINT32):integer;
       function Patch(Offset:integer;v:TBESENUINT32):boolean;
       function GenOp(Opcode:TBESENUINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7,Operand8:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7,Operand8,Operand9:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7,Operand8,Operand9,Operand10:TBESENINT32):integer; overload;
       function GenOp(Opcode:TBESENUINT32;Operands:array of TBESENINT32):integer; overload;
       function GenLabel(Offset:TBESENUINT32):boolean;
       function Here:integer;
       procedure Restore(Pos:integer);
       function GenLiteral(const v:TBESENValue;DestRegNr:integer):integer;
       function GenFunction(const f:TBESENFunctionLiteralContainer;DestRegNr:integer):integer;
       function GenLocation(const l:TBESENLocation):integer;
       function GenVariable(const s:TBESENString;IsParameter:boolean;ParameterIndex:integer;const CodeGeneratorContext:TObject):integer;
       function GenPolymorphicInlineCacheInstruction:integer;
       function Disassemble:TBESENString;
       procedure Execute(const Context:TObject;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENContext,BESENCodeContext,BESENUtils,BESENOpcodes,BESENCodeGeneratorContext;

constructor TBESENCode.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 CodeNext:=nil;
 if assigned(TBESEN(Instance).CodeLast) then begin
  TBESEN(Instance).CodeLast.CodeNext:=self;
  CodePrevious:=TBESEN(Instance).CodeLast;
  TBESEN(Instance).CodeLast:=self;
 end else begin
  CodePrevious:=nil;
  TBESEN(Instance).CodeFirst:=self;
  TBESEN(Instance).CodeLast:=self;
 end;
 ByteCode:=nil;
 ByteCodeLen:=0;
 NativeCode:=nil;
 NativeCodeSize:=0;
 NativeCodePCOffsets:=nil;
 Body:=nil;
 FunctionLiteralContainers:=nil;
 Literals:=nil;
 Locations:=nil;
 Variables:=nil;
 PolymorphicInlineCacheInstructions:=nil;
 CountFunctionLiteralContainers:=0;
 CountLiterals:=0;
 CountLocations:=0;
 CountVariables:=0;
 CountPolymorphicInlineCacheInstructions:=0;
 MaxRegisters:=0;
 MaxBlock:=0;
 MaxLoop:=0;
 MaxParamArgs:=0;
 HasLocalDelete:=false;
 IsComplexFunction:=false;
 HasMaybeDirectEval:=false;
 HoldLocalVariablesInRegisters:=false;
 LastCodePos:=-1;
 LastOpcode:=-1;
 LastLine:=-1;
 LookupNames:=TBESENStringList.Create;
 FreeCodeContexts:=nil;
 CountOfFreeCodeContexts:=0;
end;

destructor TBESENCode.Destroy;
var i:integer;
    NextCodeContext:TBESENCodeContext;
begin
 while assigned(TBESENCodeContext(FreeCodeContexts)) do begin
  NextCodeContext:=TBESENCodeContext(FreeCodeContexts).NextCodeContext;
  TBESENCodeContext(FreeCodeContexts).NextCodeContext:=nil;
  BESENFreeAndNil(FreeCodeContexts);
  TBESENCodeContext(FreeCodeContexts):=NextCodeContext;
 end;
 BESENFreeAndNil(LookupNames);
 for i:=0 to length(Literals)-1 do begin
  Literals[i].Str:='';
  Literals[i].ReferenceBase.Str:='';
  Literals[i]:=BESENEmptyValue;
 end;
 for i:=0 to length(Variables)-1 do begin
  Variables[i].Name:='';
 end;
 SetLength(FunctionLiteralContainers,0);
 SetLength(Literals,0);
 SetLength(Locations,0);
 SetLength(Variables,0);
 for i:=0 to length(PolymorphicInlineCacheInstructions)-1 do begin
  if assigned(PolymorphicInlineCacheInstructions[i]) then begin
   Dispose(PolymorphicInlineCacheInstructions[i]);
   PolymorphicInlineCacheInstructions[i]:=nil;
  end;
 end;
 SetLength(PolymorphicInlineCacheInstructions,0);
 SetLength(ByteCode,0);
 SetLength(NativeCodePCOffsets,0);
 if assigned(NativeCode) then begin
  TBESEN(Instance).NativeCodeMemoryManager.FreeMemory(NativeCode);
  NativeCode:=nil;
 end;
 if assigned(CodePrevious) then begin
  CodePrevious.CodeNext:=CodeNext;
 end else if TBESEN(Instance).CodeFirst=self then begin
  TBESEN(Instance).CodeFirst:=CodeNext;
 end;
 if assigned(CodeNext) then begin
  CodeNext.CodePrevious:=CodePrevious;
 end else if TBESEN(Instance).CodeLast=self then begin
  TBESEN(Instance).CodeLast:=CodePrevious;
 end;
 CodePrevious:=nil;
 CodeNext:=nil;
 inherited Destroy;
end;

procedure TBESENCode.Mark;
var i:integer;
begin
 for i:=0 to CountFunctionLiteralContainers-1 do begin
  if assigned(FunctionLiteralContainers[i]) then begin
   TBESEN(Instance).GarbageCollector.GrayIt(FunctionLiteralContainers[i]);
  end;
 end;
 for i:=0 to CountLiterals-1 do begin
  TBESEN(Instance).GarbageCollector.GrayValue(Literals[i]);
 end;
end;

procedure TBESENCode.Finish;
var i,j:integer;
begin
 SetLength(ByteCode,ByteCodeLen);
 SetLength(FunctionLiteralContainers,CountFunctionLiteralContainers);
 SetLength(Literals,CountLiterals);
 SetLength(Locations,CountLocations);
 SetLength(Variables,CountVariables);
 j:=length(PolymorphicInlineCacheInstructions);
 SetLength(PolymorphicInlineCacheInstructions,CountPolymorphicInlineCacheInstructions);
 for i:=j to length(PolymorphicInlineCacheInstructions)-1 do begin
   PolymorphicInlineCacheInstructions[i]:=nil;
 end;
 for i:=0 to length(PolymorphicInlineCacheInstructions)-1 do begin
  if not assigned(PolymorphicInlineCacheInstructions[i]) then begin
   New(PolymorphicInlineCacheInstructions[i]);
  end;
 end;
 ResetPolymorphicInlineCacheInstructions;
end;

function TBESENCode.Put(v:TBESENUINT32):integer;
begin
 result:=ByteCodeLen;
 if ByteCodeLen>=length(ByteCode) then begin
  SetLength(ByteCode,(ByteCodeLen+4096) and not 4095);
 end;
 ByteCode[result]:=v;
 inc(ByteCodeLen);
end;

function TBESENCode.Patch(Offset:integer;v:TBESENUINT32):boolean;
begin
 result:=Offset<=ByteCodeLen;
 if result then begin
  ByteCode[Offset]:=v;
 end;
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32):integer;
begin
 if (Opcode in [bopGC,bopSTRICTCHECKREF,bopDEBUGGER,bopCHECKOBJECTCOERCIBLE,bopTRACE]) and (TBESENUINT32(LastOpcode)=Opcode) then begin
  result:=LastCodePos;
 end else begin
  LastCodePos:=Here;
  LastOpcode:=Opcode;
  result:=Put(Opcode and $ff);
 end;
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (1 shl 8));
 Put(Operand);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (2 shl 8));
 Put(Operand1);
 Put(Operand2);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (3 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (4 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (5 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
 Put(Operand5);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (6 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
 Put(Operand5);
 Put(Operand6);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (7 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
 Put(Operand5);
 Put(Operand6);
 Put(Operand7);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7,Operand8:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (8 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
 Put(Operand5);
 Put(Operand6);
 Put(Operand7);
 Put(Operand8);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7,Operand8,Operand9:TBESENINT32):integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (9 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
 Put(Operand5);
 Put(Operand6);
 Put(Operand7);
 Put(Operand8);
 Put(Operand9);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operand1,Operand2,Operand3,Operand4,Operand5,Operand6,Operand7,Operand8,Operand9,Operand10:TBESENINT32):integer; 
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (10 shl 8));
 Put(Operand1);
 Put(Operand2);
 Put(Operand3);
 Put(Operand4);
 Put(Operand5);
 Put(Operand6);
 Put(Operand7);
 Put(Operand8);
 Put(Operand9);
 Put(Operand10);
end;

function TBESENCode.GenOp(Opcode:TBESENUINT32;Operands:array of TBESENINT32):integer;
var i:integer;
begin
 LastCodePos:=Here;
 LastOpcode:=Opcode;
 result:=Put((Opcode and $ff) or (longword(length(Operands)) shl 8));
 for i:=0 to length(Operands)-1 do begin
  Put(Operands[i]);
 end;
end;

function TBESENCode.GenLabel(Offset:TBESENUINT32):boolean;
begin
 result:=Patch(Offset,Here);
end;

function TBESENCode.Here:integer;
begin
 result:=ByteCodeLen;
end;

procedure TBESENCode.Restore(Pos:integer);
begin
 ByteCodeLen:=Pos;
 LastCodePos:=-1;
 LastOpcode:=-1;
end;

function TBESENCode.GenLiteral(const v:TBESENValue;DestRegNr:integer):integer;
const BoolToInt:array[boolean] of longword=(0,$ffffffff);
var i:integer;
    Okay:boolean;
begin
 result:=-1;
 for i:=0 to CountLiterals-1 do begin
  if Literals[i].ValueType=v.ValueType then begin
   case v.ValueType of
    bvtUNDEFINED:begin
     Okay:=true;
    end;
    bvtNULL:begin
     Okay:=true;
    end;
    bvtBOOLEAN:begin
     Okay:=Literals[i].Bool=v.Bool;
    end;
    bvtNUMBER:begin
     Okay:=int64(pointer(@Literals[i].Num)^)=int64(pointer(@v.Num)^);
    end;
    bvtSTRING:begin
     Okay:=Literals[i].Str=v.Str;
    end;
    bvtOBJECT:begin
     Okay:=Literals[i].Obj=v.Obj;
    end;
    else begin
     Okay:=false;
    end;
   end;
   if Okay then begin
    result:=i;
    break;
   end;
  end;
 end;
 if result<0 then begin
  result:=CountLiterals;
  if CountLiterals>=length(Literals) then begin
   SetLength(Literals,CountLiterals+256);
  end;
  Literals[result]:=v;
  inc(CountLiterals);
 end;
 case v.ValueType of
  bvtUNDEFINED:begin
   GenOp(bopLITERALUNDEF,DestRegNr);
  end;
  bvtNULL:begin
   GenOp(bopLITERALNULL,DestRegNr);
  end;
  bvtBOOLEAN:begin
   if v.Bool then begin
    GenOp(bopLITERALBOOL,DestRegNr,BoolToInt[true]);
   end else begin
    GenOp(bopLITERALBOOL,DestRegNr,BoolToInt[false]);
   end;
  end;
  bvtNUMBER:begin
   GenOp(bopLITERALNUM,DestRegNr,result);
  end;
  bvtSTRING:begin
   GenOp(bopLITERALSTR,DestRegNr,result);
  end;
  bvtOBJECT:begin
   GenOp(bopLITERALOBJ,DestRegNr,result);
  end;
  else begin
   GenOp(bopLITERAL,DestRegNr,result);
  end;
 end;
end;

function TBESENCode.GenFunction(const f:TBESENFunctionLiteralContainer;DestRegNr:integer):integer;
var i:integer;
begin
 result:=-1;
 for i:=0 to CountFunctionLiteralContainers-1 do begin
  if FunctionLiteralContainers[i]=f then begin
   result:=i;
   break;
  end;
 end;
 if result<0 then begin
  result:=CountFunctionLiteralContainers;
  if CountFunctionLiteralContainers>=length(FunctionLiteralContainers) then begin
   SetLength(FunctionLiteralContainers,CountFunctionLiteralContainers+256);
  end;
  FunctionLiteralContainers[result]:=f;
  inc(CountFunctionLiteralContainers);
 end;
 GenOp(bopFUNC,DestRegNr,result);
end;

function TBESENCode.GenLocation(const l:TBESENLocation):integer;
var i:integer;
    LastOpcodeWasLine:boolean;
begin
 result:=-1;
 if TBESEN(Instance).CodeLineInfo or TBESEN(Instance).CodeTracable then begin
  for i:=0 to CountLocations-1 do begin
   if Locations[i].LineNumber=l.LineNumber then begin
    result:=i;
    break;
   end;
  end;
  if result<0 then begin
   result:=CountLocations;
   if CountLocations>=length(Locations) then begin
    SetLength(Locations,CountLocations+256);
   end;
   Locations[result]:=l;
   inc(CountLocations);
  end;
  LastOpcodeWasLine:=LastOpcode=bopLINE;
  if LastLine<>result then begin
   LastLine:=result;
   if LastOpcodeWasLine then begin
    ByteCode[LastCodePos+1]:=result;
   end else begin
    GenOp(bopLINE,result);
   end;
  end;
  if TBESEN(Instance).CodeTracable and not LastOpcodeWasLine then begin
   GenOp(bopTRACE);
  end;
 end;
end;

function TBESENCode.GenVariable(const s:TBESENString;IsParameter:boolean;ParameterIndex:integer;const CodeGeneratorContext:TObject):integer;
var Item:PBESENHashMapItem;
begin
 Item:=TBESENCodeGeneratorContext(CodeGeneratorContext).VariableNameHashMap.GetKey(s);
 if assigned(Item) then begin
  result:=Item^.Value;
 end else begin
  result:=-1;
 end;
 if result<0 then begin
  result:=CountVariables;
  if CountVariables>=length(Variables) then begin
   SetLength(Variables,CountVariables+256);
  end;
  Variables[result].Name:=s;
  Variables[result].IsParameter:=IsParameter;
  Variables[result].ParameterIndex:=ParameterIndex;
  Item:=TBESENCodeGeneratorContext(CodeGeneratorContext).VariableNameHashMap.NewKey(s,true);
  Item^.Value:=result;
  inc(CountVariables);
 end;
end;

function TBESENCode.GenPolymorphicInlineCacheInstruction:integer;
begin
 result:=CountPolymorphicInlineCacheInstructions;
 inc(CountPolymorphicInlineCacheInstructions);
end;

procedure TBESENCode.ResetPolymorphicInlineCacheInstructions;
var PC,OPC,Instruction:TBESENUINT32;
    i,j:integer;
    PolymorphicInlineCacheInstruction:PBESENCodePolymorphicInlineCacheInstruction;
    CacheItem:PBESENCodePolymorphicInlineCacheItem;
begin
 PC:=0;
 while PC<TBESENUINT32(ByteCodeLen) do begin
  Instruction:=ByteCode[PC];
  OPC:=PC+1;
  inc(PC,1+(Instruction shr 8));
  case Instruction and $ff of
   bopREF:begin
    ByteCode[OPC+6]:=TBESENUINT32(TBESENINT32(-1));
    ByteCode[OPC+7]:=TBESENUINT32(TBESENINT32(-1));
    ByteCode[OPC+8]:=TBESENUINT32(TBESENINT32(-1));
    ByteCode[OPC+9]:=TBESENUINT32(TBESENINT32(-1));
   end;
   bopVREF:begin
    ByteCode[OPC+4]:=TBESENUINT32(TBESENINT32(-1));
    ByteCode[OPC+5]:=TBESENUINT32(TBESENINT32(-1));
    ByteCode[OPC+6]:=TBESENUINT32(TBESENINT32(-1));
    ByteCode[OPC+7]:=TBESENUINT32(TBESENINT32(-1));
   end;
  end;
 end;
 for i:=0 to CountPolymorphicInlineCacheInstructions-1 do begin
  PolymorphicInlineCacheInstruction:=PolymorphicInlineCacheInstructions[i];
  PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENPolymorphicInlineCacheStartItemPositions;
  for j:=0 to BESENPolymorphicInlineCacheSize-1 do begin
   CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[j];
   CacheItem^.StructureID:=-1;
   CacheItem^.Index:=-1;
   CacheItem^.ID:=-1;
   CacheItem^.NestedLevel:=-1;
  end;
 end;
end;

function TBESENCode.Disassemble:TBESENString;
var PC,OPC,Instruction,i:TBESENUINT32;
    Output:TBESENString;
 procedure Add(const s:TBESENString);
 begin
  Output:=Output+s;
 end;
begin
 Output:='';
 PC:=0;
 while PC<TBESENUINT32(ByteCodeLen) do begin
  Output:=Output+IntToHex(PC,8)+': ';
  Instruction:=ByteCode[PC];
  OPC:=PC+1;
  inc(PC,1+(Instruction shr 8));
  Add('0x'+IntToHex(Instruction and $ff,2)+' ');
  if (Instruction and $ff)<longword(length(OpcodeNames)) then begin
   Add(OpcodeNames[Instruction and $ff]);
  end else begin
   Add('UNKNOWN');
  end;
  for i:=1 to Instruction shr 8 do begin
   Add(' 0x'+IntToHex(ByteCode[OPC+(i-1)],8));
  end;
  Output:=Output+#13#10;
 end;
 result:=Output;
end;

procedure TBESENCode.Execute(const Context:TObject;var ResultValue:TBESENValue);
var CodeContext:TBESENCodeContext;
begin
 if assigned(FreeCodeContexts) then begin
  CodeContext:=TBESENCodeContext(FreeCodeContexts);
  TBESENCodeContext(FreeCodeContexts):=TBESENCodeContext(FreeCodeContexts).NextCodeContext;
  dec(CountOfFreeCodeContexts);
 end else begin
  CodeContext:=TBESENCodeContext.Create(Instance,self);
 end;
 TBESENCodeContext(TBESENContext(Context).CodeContext):=CodeContext;
 try
  TBESENCodeContext(TBESENContext(Context).CodeContext).Execute(TBESENContext(Context),ResultValue);
 finally
  TBESENContext(Context).CodeContext:=nil;
  TBESENCodeContext(CodeContext).Context:=nil;
  if CountOfFreeCodeContexts<TBESEN(Instance).MaxCountOfFreeCodeContexts then begin
   CodeContext.NextCodeContext:=TBESENCodeContext(FreeCodeContexts);
   TBESENCodeContext(FreeCodeContexts):=CodeContext;
   inc(CountOfFreeCodeContexts);
  end else begin
   BESENFreeAndNil(CodeContext);
  end;                        
 end;
end;

end.
