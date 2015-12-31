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
unit BESENCompiler;
{$i BESEN.inc}

interface

uses SysUtils,Math,BESENConstants,BESENTypes,BESENValue,BESENBaseObject,BESENASTNodes,
     BESENEvalCacheItem;

type TBESENCompiler=class(TBESENBaseObject)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function Compile(InputSource:TBESENUTF8STRING;const Parameters:TBESENUTF8STRING='';IsFunction:TBESENBoolean=false;IsJSON:TBESENBoolean=false):TBESENASTNode;
     end;

implementation

uses BESEN,BESENUtils,BESENPointerList,BESENHashMap,BESENErrors,BESENNumberUtils,
     BESENCode,BESENCodeGeneratorContext,BESENOpcodes,BESENHashUtils,BESENGlobals,
     BESENParser;

const bcgtUNDEFINED=$01;
      bcgtNULL=$02;
      bcgtBOOLEAN=$04;
      bcgtNUMBER=$08;
      bcgtSTRING=$10;
      bcgtOBJECT=$20;
      bcgtREFERENCE=$40;
      bcgtPRIMITIVE=bcgtUNDEFINED or bcgtNULL or bcgtBOOLEAN or bcgtNUMBER or bcgtSTRING;
      bcgtVALUE=bcgtPRIMITIVE or bcgtOBJECT;
            
function sar(Value,Shift:integer):integer;
{$ifdef PurePascal}{$ifdef caninline}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,Shift);
{$else}
 Shift:=Shift and 31;
 result:=(longword(Value) shr Shift) or (longword(longint(longword(0-longword(longword(Value) shr 31)) and longword(0-longword(ord(Shift<>0))))) shl (32-Shift));
{$endif}
end;
{$else}
{$ifdef HasSAR} inline;
begin
result:=SARLongint(Value,Shift);
end;
{$else}
{$ifdef cpu386}
{$ifdef fpc} assembler; register; //inline;
asm
 mov ecx,edx
 sar eax,cl
end;// ['eax','edx','ecx'];
{$else} assembler; register;
asm
 mov ecx,edx
 sar eax,cl
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; //inline;
asm
 mov r0,r0,asr R1
end;// ['r0','R1'];
{$else}{$ifdef caninline}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,Shift);
{$else}
 Shift:=Shift and 31;
 result:=(longword(Value) shr Shift) or (longword(longint(longword(0-longword(longword(Value) shr 31)) and longword(0-longword(ord(Shift<>0))))) shl (32-Shift));
{$endif}
end;
{$endif}
{$endif}
{$endif}
{$endif}

constructor TBESENCompiler.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
end;

destructor TBESENCompiler.Destroy;
begin
 inherited Destroy;
end;

function TBESENCompiler.Compile(InputSource:TBESENUTF8STRING;const Parameters:TBESENUTF8STRING='';IsFunction:TBESENBoolean=false;IsJSON:TBESENBoolean=false):TBESENASTNode;
 function ConvertToValue(n:TBESENASTNode;var v:TBESENValue):boolean;
 begin
  case n.NodeType of
   bntBOOLEANLITERAL:begin
    v.ValueType:=bvtBOOLEAN;
    v.Bool:=TBESENASTNodeBooleanLiteral(n).Value;
    result:=true;
   end;
   bntNUMBERLITERAL:begin
    v.ValueType:=bvtNUMBER;
    v.Num:=TBESENASTNodeNumberLiteral(n).Value;
    result:=true;
   end;
   bntSTRINGLITERAL:begin
    v.ValueType:=bvtSTRING;
    v.Str:=TBESENASTNodeStringLiteral(n).Value;
    result:=true;
   end;
   else begin
    v.ValueType:=bvtNONE;
    result:=false;
   end;
  end;
 end;
 procedure TrimStuffAfterReturnBreakContinue(Node:TBESENASTNode;var Statements:TBESENASTNodeStatements);
 var Counter,NewCount,TrimmedCounter:integer;
     Statement:TBESENASTNodeStatement;
 begin
  NewCount:=length(Statements);
  for Counter:=0 to length(Statements)-1 do begin
   Statement:=Statements[Counter];
   if assigned(Statement) then begin
    case Statement.NodeType of
     bntBREAKSTATEMENT,bntCONTINUESTATEMENT,bntRETURNSTATEMENT:begin
      NewCount:=Counter+1;
      break;
     end;
    end;
   end;
  end;
  if NewCount<>length(Statements) then begin
   TrimmedCounter:=Node.CountTrashNodes;
   inc(Node.CountTrashNodes,length(Statements)-NewCount);
   if Node.CountTrashNodes>=length(Node.TrashNodes) then begin
    SetLength(Node.TrashNodes,Node.CountTrashNodes+256);
   end;
   for Counter:=NewCount to length(Statements)-1 do begin
    Node.TrashNodes[TrimmedCounter]:=Statements[Counter];
    inc(TrimmedCounter);
   end;
   SetLength(Statements,NewCount);
  end;
 end;
 procedure AddTrashNode(Node,TrashNode:TBESENASTNode);
 var Index:integer;
 begin
  if assigned(Node) and assigned(TrashNode) then begin
   Index:=Node.CountTrashNodes;
   inc(Node.CountTrashNodes);
   if Node.CountTrashNodes>=length(Node.TrashNodes) then begin
    SetLength(Node.TrashNodes,Node.CountTrashNodes+256);
   end;
   Node.TrashNodes[Index]:=TrashNode;
   TrashNode:=nil;
  end;
 end;
 function IsLastNodeBreakContinueReturnNode(const Statements:TBESENASTNodeStatements):boolean;
 var Counter:integer;
     Statement:TBESENASTNodeStatement;
 begin
  result:=false;
  for Counter:=length(Statements)-1 downto 0 do begin
   Statement:=Statements[Counter];
   if assigned(Statement) then begin
    result:=Statement.NodeType in [bntBREAKSTATEMENT,bntCONTINUESTATEMENT,bntRETURNSTATEMENT];
    break;
   end;
  end;
 end;
 function IsBreakContinueReturnNode(Node:TBESENASTNode):boolean;
 begin
  result:=false;
  if assigned(Node) then begin
   case Node.NodeType of
    bntBREAKSTATEMENT,bntCONTINUESTATEMENT,bntRETURNSTATEMENT:begin
     result:=true;
    end;
    bntBLOCKSTATEMENT:begin
     result:=IsLastNodeBreakContinueReturnNode(TBESENASTNodeBlockStatement(Node).Statements);
    end;
   end;
  end;
 end;
 function HasLabelBreakContinue(RootNode:TBESENASTNode):boolean;
 var HasResult:boolean;
  procedure Visit(ToVisit:TBESENASTNode);
  var Counter:integer;
  begin
   if assigned(ToVisit) and not HasResult then begin
    if ToVisit is TBESENASTNodeStatement then begin
     if TBESENASTNodeStatement(ToVisit).Location.LineNumber>0 then begin
      TBESEN(Instance).LineNumber:=TBESENASTNodeStatement(ToVisit).Location.LineNumber;
     end;
    end;
    case ToVisit.NodeType of
     bntNONE:begin
     end;
     bntEXPRESSION:begin
     end;
     bntLITERAL:begin
     end;
     bntIDENTIFIER:begin
     end;
     bntVARIABLEDECLARATION:begin
      Visit(TBESENASTNodeVariableDeclaration(ToVisit).Identifier);
      Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression);
     end;
     bntVARIABLEEXPRESSION:begin
      for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
       Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]);
      end;
     end;
     bntFUNCTIONBODY:begin
      for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]);
      end;
     end;
     bntFUNCTIONLITERAL:begin
      Visit(TBESENASTNodeFunctionLiteral(ToVisit).Body);
     end;
     bntSTATEMENT:begin
     end;
     bntVARIABLESTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
       Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]);
      end;
     end;
     bntFUNCTIONDECLARATION:begin
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       Visit(TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal);
      end;
     end;
     bntEXPRESSIONSTATEMENT:begin
      Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression);
     end;
     bntEMPTYSTATEMENT:begin
     end;
     bntBLOCKSTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeBlockStatement(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeBlockStatement(ToVisit).Statements[Counter]);
      end;
     end;
     bntDEBUGGERSTATEMENT:begin
     end;
     bntBREAKSTATEMENT:begin
      Visit(TBESENASTNodeBreakStatement(ToVisit).Identifier);
      if assigned(TBESENASTNodeBreakStatement(ToVisit).Identifier) then begin
       HasResult:=true;
      end;
     end;
     bntCONTINUESTATEMENT:begin
      Visit(TBESENASTNodeContinueStatement(ToVisit).Identifier);
      if assigned(TBESENASTNodeContinueStatement(ToVisit).Identifier) then begin
       HasResult:=true;
      end;
     end;
     bntDOSTATEMENT:begin
      Visit(TBESENASTNodeDoStatement(ToVisit).Statement);
      Visit(TBESENASTNodeDoStatement(ToVisit).Expression);
     end;
     bntWHILESTATEMENT:begin
      Visit(TBESENASTNodeWhileStatement(ToVisit).Expression);
      Visit(TBESENASTNodeWhileStatement(ToVisit).Statement);
     end;
     bntWITHSTATEMENT:begin
      Visit(TBESENASTNodeWithStatement(ToVisit).Expression);
      Visit(TBESENASTNodeWithStatement(ToVisit).Statement);
     end;
     bntFORSTATEMENT:begin
      Visit(TBESENASTNodeForStatement(ToVisit).Initial);
      Visit(TBESENASTNodeForStatement(ToVisit).Condition);
      Visit(TBESENASTNodeForStatement(ToVisit).Increment);
      Visit(TBESENASTNodeForStatement(ToVisit).Statement);
     end;
     bntFORINSTATEMENT:begin
      Visit(TBESENASTNodeForInStatement(ToVisit).Variable);
      Visit(TBESENASTNodeForInStatement(ToVisit).Expression);
      Visit(TBESENASTNodeForInStatement(ToVisit).Statement);
     end;
     bntIFSTATEMENT:begin
      Visit(TBESENASTNodeIfStatement(ToVisit).Expression);
      Visit(TBESENASTNodeIfStatement(ToVisit).TrueStatement);
      Visit(TBESENASTNodeIfStatement(ToVisit).FalseStatement);
     end;
     bntLABELLEDSTATEMENT:begin
      HasResult:=true;
      for Counter:=0 to length(TBESENASTNodeLabelledStatement(ToVisit).Identifiers)-1 do begin
       Visit(TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]);
      end;
      Visit(TBESENASTNodeLabelledStatement(ToVisit).Statement);
     end;
     bntCASESTATEMENT:begin
      Visit(TBESENASTNodeCaseStatement(ToVisit).Expression);
      for Counter:=0 to length(TBESENASTNodeCaseStatement(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]);
      end;
     end;
     bntSWITCHSTATEMENT:begin
      Visit(TBESENASTNodeSwitchStatement(ToVisit).Expression);
      for Counter:=0 to length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements)-1 do begin
       Visit(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter]);
      end;
     end;
     bntTHROWSTATEMENT:begin
      Visit(TBESENASTNodeThrowStatement(ToVisit).Expression);
     end;
     bntTRYSTATEMENT:begin
      Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock);
      Visit(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier);
      Visit(TBESENASTNodeTryStatement(ToVisit).CatchBlock);
      Visit(TBESENASTNodeTryStatement(ToVisit).FinallyBlock);
     end;
     bntARRAYLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeArrayLiteral(ToVisit).Elements)-1 do begin
       if assigned(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]) then begin
        Visit(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]);
       end;
      end;
     end;
     bntBINARYEXPRESSION:begin
      Visit(TBESENASTNodeBinaryExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTOPERATOREXPRESSION:begin
      Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTMULTIPLYEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTDIVIDEEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTMODULOEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTPLUSEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTMINUSEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTSHIFTLEFTEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTSHIFTRIGHTEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTBITWISEANDEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTBITWISEXOREXPRESSION:begin
      Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTBITWISEOREXPRESSION:begin
      Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).RightExpression);
     end;
     bntBINARYOPERATOREXPRESSION:begin
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression);
     end;
     bntBINARYCOMMAEXPRESSION:begin
      Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).RightExpression);
     end;
     bntBINARYDIVIDEEXPRESSION:begin
      Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression);
     end;
     bntBINARYMODULOEXPRESSION:begin
      Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression);
     end;
     bntBINARYMULTIPLYEXPRESSION:begin
      Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression);
     end;
     bntBINARYPLUSEXPRESSION:begin
      Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression);
     end;
     bntBINARYMINUSEXPRESSION:begin
      Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression);
     end;
     bntBINARYSHIFTLEFTEXPRESSION:begin
      Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression);
     end;
     bntBINARYSHIFTRIGHTEXPRESSION:begin
      Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression);
     end;
     bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression);
     end;
     bntBINARYGREATERTHANEXPRESSION:begin
      Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression);
     end;
     bntBINARYGREATERTHANOREQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYLESSTHANEXPRESSION:begin
      Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression);
     end;
     bntBINARYLESSTHANOREQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYINSTANCEOFEXPRESSION:begin
      Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).RightExpression);
     end;
     bntBINARYINEXPRESSION:begin
      Visit(TBESENASTNodeBinaryInExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryInExpression(ToVisit).RightExpression);
     end;
     bntBINARYEQUALEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYEQUALEQUALEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYNOTEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYNOTEQUALEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYBITWISEANDEXPRESSION:begin
      Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression);
     end;
     bntBINARYBITWISEXOREXPRESSION:begin
      Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression);
     end;
     bntBINARYBITWISEOREXPRESSION:begin
      Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression);
     end;
     bntBOOLEANLITERAL:begin
     end;
     bntCALLEXPRESSION:begin
      Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction);
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]);
      end;
     end;
     bntNEWEXPRESSION:begin
      Visit(TBESENASTNodeNewExpression(ToVisit).TheFunction);
      for Counter:=0 to length(TBESENASTNodeNewExpression(ToVisit).Arguments)-1 do begin
       Visit(TBESENASTNodeNewExpression(ToVisit).Arguments[Counter]);
      end;
     end;
     bntCONDITIONALEXPRESSION:begin
      Visit(TBESENASTNodeConditionalExpression(ToVisit).Expression);
      Visit(TBESENASTNodeConditionalExpression(ToVisit).TrueExpression);
      Visit(TBESENASTNodeConditionalExpression(ToVisit).FalseExpression);
     end;
     bntUNARYEXPRESSION:begin
      Visit(TBESENASTNodeUnaryExpression(ToVisit).SubExpression);
     end;
     bntUNARYOPERATOREXPRESSION:begin
      Visit(TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression);
     end;
     bntUNARYPLUSEXPRESSION:begin
      Visit(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression);
     end;
     bntUNARYMINUSEXPRESSION:begin
      Visit(TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression);
     end;
     bntUNARYBITWISENOTEXPRESSION:begin
      Visit(TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression);
     end;
     bntUNARYLOGICALNOTEXPRESSION:begin
      Visit(TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression);
     end;
     bntUNARYVOIDEXPRESSION:begin
      Visit(TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression);
     end;
     bntUNARYTYPEOFEXPRESSION:begin
      Visit(TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression);
     end;
     bntPROPERTYEXPRESSION:begin
      Visit(TBESENASTNodePropertyExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodePropertyExpression(ToVisit).RightExpression);
     end;
     bntLOGICALANDEXPRESSION:begin
      Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression);
     end;
     bntLOGICALOREXPRESSION:begin
      Visit(TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression);
     end;
     bntDELETEEXPRESSION:begin
      Visit(TBESENASTNodeDeleteExpression(ToVisit).SubExpression);
     end;
     bntPOSTFIXINCEXPRESSION:begin
      Visit(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression);
     end;
     bntPOSTFIXDECEXPRESSION:begin
      Visit(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression);
     end;
     bntPREFIXINCEXPRESSION:begin
      Visit(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression);
     end;
     bntPREFIXDECEXPRESSION:begin
      Visit(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression);
     end;
     bntNULLLITERAL:begin
     end;
     bntNUMBERLITERAL:begin
     end;
     bntREGEXPLITERAL:begin
     end;
     bntSTRINGLITERAL:begin
     end;
     bntTHISLITERAL:begin
     end;
     bntOBJECTLITERALPROPERTY:begin
      Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Value);
      if assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container) then begin
       Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal);
      end;
     end;
     bntOBJECTLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeObjectLiteral(ToVisit).Properties)-1 do begin
       Visit(TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter]);
      end;
     end;
     bntRETURNSTATEMENT:begin
      Visit(TBESENASTNodeReturnStatement(ToVisit).Expression);
     end;
     bntPROGRAM:begin
      Visit(TBESENASTNodeProgram(ToVisit).Body);
     end;
     bntFUNCTIONEXPRESSION:begin
      if assigned(TBESENASTNodeFunctionExpression(ToVisit).Container) then begin
       Visit(TBESENASTNodeFunctionExpression(ToVisit).Container.Literal);
      end;
     end;
    end;
   end;
  end;
 begin
  HasResult:=false;
  Visit(RootNode);
  result:=HasResult;
 end;
 procedure CheckNodeForFeatures(RootNode:TBESENASTNode;var HasLocalDelete,IsComplexFunction,HasMaybeDirectEval,HasFunctions,HoldLocalVariablesInRegisters:boolean);
  procedure Visit(ToVisit:TBESENASTNode);
  var Counter:integer;
  begin
   if assigned(ToVisit) then begin
    if ToVisit is TBESENASTNodeStatement then begin
     if TBESENASTNodeStatement(ToVisit).Location.LineNumber>0 then begin
      TBESEN(Instance).LineNumber:=TBESENASTNodeStatement(ToVisit).Location.LineNumber;
     end;
    end;
    case ToVisit.NodeType of
     bntNONE:begin
     end;
     bntEXPRESSION:begin
     end;
     bntLITERAL:begin
     end;
     bntIDENTIFIER:begin
      if (TBESENASTNodeIdentifier(ToVisit).Name='eval') or (TBESENASTNodeIdentifier(ToVisit).Name='arguments') then begin
       if TBESENASTNodeIdentifier(ToVisit).Name='eval' then begin
        HasMaybeDirectEval:=true;
       end;
       IsComplexFunction:=true;
       HoldLocalVariablesInRegisters:=false;
      end;
     end;
     bntVARIABLEDECLARATION:begin
      Visit(TBESENASTNodeVariableDeclaration(ToVisit).Identifier);
      Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression);
     end;
     bntVARIABLEEXPRESSION:begin
      for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
       Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]);
      end;
     end;
     bntFUNCTIONBODY:begin
      IsComplexFunction:=true;
      for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]);
      end;
     end;
     bntFUNCTIONLITERAL:begin
      IsComplexFunction:=true;
      HasFunctions:=true;
      Visit(TBESENASTNodeFunctionLiteral(ToVisit).Body);
     end;
     bntSTATEMENT:begin
     end;
     bntVARIABLESTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
       Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]);
      end;
     end;
     bntFUNCTIONDECLARATION:begin
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       Visit(TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal);
      end;
     end;
     bntEXPRESSIONSTATEMENT:begin
      Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression);
     end;
     bntEMPTYSTATEMENT:begin
     end;
     bntBLOCKSTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeBlockStatement(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeBlockStatement(ToVisit).Statements[Counter]);
      end;
     end;
     bntDEBUGGERSTATEMENT:begin
     end;
     bntBREAKSTATEMENT:begin
      Visit(TBESENASTNodeBreakStatement(ToVisit).Identifier);
     end;
     bntCONTINUESTATEMENT:begin
      Visit(TBESENASTNodeContinueStatement(ToVisit).Identifier);
     end;
     bntDOSTATEMENT:begin
      Visit(TBESENASTNodeDoStatement(ToVisit).Statement);
      Visit(TBESENASTNodeDoStatement(ToVisit).Expression);
     end;
     bntWHILESTATEMENT:begin
      Visit(TBESENASTNodeWhileStatement(ToVisit).Expression);
      Visit(TBESENASTNodeWhileStatement(ToVisit).Statement);
     end;
     bntWITHSTATEMENT:begin
      HoldLocalVariablesInRegisters:=false;
      Visit(TBESENASTNodeWithStatement(ToVisit).Expression);
      Visit(TBESENASTNodeWithStatement(ToVisit).Statement);
     end;
     bntFORSTATEMENT:begin
      Visit(TBESENASTNodeForStatement(ToVisit).Initial);
      Visit(TBESENASTNodeForStatement(ToVisit).Condition);
      Visit(TBESENASTNodeForStatement(ToVisit).Increment);
      Visit(TBESENASTNodeForStatement(ToVisit).Statement);
     end;
     bntFORINSTATEMENT:begin
      Visit(TBESENASTNodeForInStatement(ToVisit).Variable);
      Visit(TBESENASTNodeForInStatement(ToVisit).Expression);
      Visit(TBESENASTNodeForInStatement(ToVisit).Statement);
     end;
     bntIFSTATEMENT:begin
      Visit(TBESENASTNodeIfStatement(ToVisit).Expression);
      Visit(TBESENASTNodeIfStatement(ToVisit).TrueStatement);
      Visit(TBESENASTNodeIfStatement(ToVisit).FalseStatement);
     end;
     bntLABELLEDSTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeLabelledStatement(ToVisit).Identifiers)-1 do begin
       Visit(TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]);
      end;
      Visit(TBESENASTNodeLabelledStatement(ToVisit).Statement);
     end;
     bntCASESTATEMENT:begin
      Visit(TBESENASTNodeCaseStatement(ToVisit).Expression);
      for Counter:=0 to length(TBESENASTNodeCaseStatement(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]);
      end;
     end;
     bntSWITCHSTATEMENT:begin
      Visit(TBESENASTNodeSwitchStatement(ToVisit).Expression);
      for Counter:=0 to length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements)-1 do begin
       Visit(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter]);
      end;
     end;
     bntTHROWSTATEMENT:begin
      Visit(TBESENASTNodeThrowStatement(ToVisit).Expression);
     end;
     bntTRYSTATEMENT:begin
      Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock);
      Visit(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier);
      Visit(TBESENASTNodeTryStatement(ToVisit).CatchBlock);
      Visit(TBESENASTNodeTryStatement(ToVisit).FinallyBlock);
      HoldLocalVariablesInRegisters:=false;
     end;
     bntARRAYLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeArrayLiteral(ToVisit).Elements)-1 do begin
       if assigned(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]) then begin
        Visit(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]);
       end;
      end;
     end;
     bntBINARYEXPRESSION:begin
      Visit(TBESENASTNodeBinaryExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTOPERATOREXPRESSION:begin
      Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTMULTIPLYEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTDIVIDEEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTMODULOEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTPLUSEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTMINUSEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTSHIFTLEFTEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTSHIFTRIGHTEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTBITWISEANDEXPRESSION:begin
      Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTBITWISEXOREXPRESSION:begin
      Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).RightExpression);
     end;
     bntASSIGNMENTBITWISEOREXPRESSION:begin
      Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).RightExpression);
     end;
     bntBINARYOPERATOREXPRESSION:begin
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression);
     end;
     bntBINARYCOMMAEXPRESSION:begin
      Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).RightExpression);
     end;
     bntBINARYDIVIDEEXPRESSION:begin
      Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression);
     end;
     bntBINARYMODULOEXPRESSION:begin
      Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression);
     end;
     bntBINARYMULTIPLYEXPRESSION:begin
      Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression);
     end;
     bntBINARYPLUSEXPRESSION:begin
      Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression);
     end;
     bntBINARYMINUSEXPRESSION:begin
      Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression);
     end;
     bntBINARYSHIFTLEFTEXPRESSION:begin
      Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression);
     end;
     bntBINARYSHIFTRIGHTEXPRESSION:begin
      Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression);
     end;
     bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression);
     end;
     bntBINARYGREATERTHANEXPRESSION:begin
      Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression);
     end;
     bntBINARYGREATERTHANOREQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYLESSTHANEXPRESSION:begin
      Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression);
     end;
     bntBINARYLESSTHANOREQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYINSTANCEOFEXPRESSION:begin
      Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).RightExpression);
     end;
     bntBINARYINEXPRESSION:begin
      Visit(TBESENASTNodeBinaryInExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryInExpression(ToVisit).RightExpression);
     end;
     bntBINARYEQUALEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYEQUALEQUALEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYNOTEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYNOTEQUALEQUALEXPRESSION:begin
      Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression);
     end;
     bntBINARYBITWISEANDEXPRESSION:begin
      Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression);
     end;
     bntBINARYBITWISEXOREXPRESSION:begin
      Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression);
     end;
     bntBINARYBITWISEOREXPRESSION:begin
      Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression);
     end;
     bntBOOLEANLITERAL:begin
     end;
     bntCALLEXPRESSION:begin
      Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction);
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]);
      end;
     end;
     bntNEWEXPRESSION:begin
      Visit(TBESENASTNodeNewExpression(ToVisit).TheFunction);
      for Counter:=0 to length(TBESENASTNodeNewExpression(ToVisit).Arguments)-1 do begin
       Visit(TBESENASTNodeNewExpression(ToVisit).Arguments[Counter]);
      end;
     end;
     bntCONDITIONALEXPRESSION:begin
      Visit(TBESENASTNodeConditionalExpression(ToVisit).Expression);
      Visit(TBESENASTNodeConditionalExpression(ToVisit).TrueExpression);
      Visit(TBESENASTNodeConditionalExpression(ToVisit).FalseExpression);
     end;
     bntUNARYEXPRESSION:begin
      Visit(TBESENASTNodeUnaryExpression(ToVisit).SubExpression);
     end;
     bntUNARYOPERATOREXPRESSION:begin
      Visit(TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression);
     end;
     bntUNARYPLUSEXPRESSION:begin
      Visit(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression);
     end;
     bntUNARYMINUSEXPRESSION:begin
      Visit(TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression);
     end;
     bntUNARYBITWISENOTEXPRESSION:begin
      Visit(TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression);
     end;
     bntUNARYLOGICALNOTEXPRESSION:begin
      Visit(TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression);
     end;
     bntUNARYVOIDEXPRESSION:begin
      Visit(TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression);
     end;
     bntUNARYTYPEOFEXPRESSION:begin
      Visit(TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression);
     end;
     bntPROPERTYEXPRESSION:begin
      Visit(TBESENASTNodePropertyExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodePropertyExpression(ToVisit).RightExpression);
     end;
     bntLOGICALANDEXPRESSION:begin
      Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression);
     end;
     bntLOGICALOREXPRESSION:begin
      Visit(TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression);
      Visit(TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression);
     end;
     bntDELETEEXPRESSION:begin
      IsComplexFunction:=true;
      HoldLocalVariablesInRegisters:=false;
      Visit(TBESENASTNodeDeleteExpression(ToVisit).SubExpression);
      if assigned(TBESENASTNodeDeleteExpression(ToVisit).SubExpression) and (TBESENASTNodeDeleteExpression(ToVisit).SubExpression.NodeType=bntIDENTIFIER) then begin
       HasLocalDelete:=true;
      end;
     end;
     bntPOSTFIXINCEXPRESSION:begin
      Visit(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression);
     end;
     bntPOSTFIXDECEXPRESSION:begin
      Visit(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression);
     end;
     bntPREFIXINCEXPRESSION:begin
      Visit(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression);
     end;
     bntPREFIXDECEXPRESSION:begin
      Visit(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression);
     end;
     bntNULLLITERAL:begin
     end;
     bntNUMBERLITERAL:begin
     end;
     bntREGEXPLITERAL:begin
     end;
     bntSTRINGLITERAL:begin
     end;
     bntTHISLITERAL:begin
     end;
     bntOBJECTLITERALPROPERTY:begin
      Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Value);
      if assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container) then begin
       Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal);
      end;
     end;
     bntOBJECTLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeObjectLiteral(ToVisit).Properties)-1 do begin
       Visit(TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter]);
      end;
     end;
     bntRETURNSTATEMENT:begin
      Visit(TBESENASTNodeReturnStatement(ToVisit).Expression);
     end;
     bntPROGRAM:begin
      IsComplexFunction:=true;
      HoldLocalVariablesInRegisters:=false;
      Visit(TBESENASTNodeProgram(ToVisit).Body);
     end;
     bntFUNCTIONEXPRESSION:begin
      IsComplexFunction:=true;
      if assigned(TBESENASTNodeFunctionExpression(ToVisit).Container) then begin
       Visit(TBESENASTNodeFunctionExpression(ToVisit).Container.Literal);
      end;
     end;
    end;
   end;
  end;
 begin
  Visit(RootNode);
 end;
 procedure ScanCollectAndPreprocess(RootNode:TBESENASTNode);
 var Functions:TBESENPointerList;
     Variables:TBESENPointerList;
     VariableHashMap:TBESENHashMap;
     AreLocalsOptimizable,IsFunction,IsMaybeArgumentsObjectUsed:boolean;
     FunctionLiteral:TBESENASTNodeFunctionLiteral;
  procedure AddVariable(Identifier:TBESENASTNodeIdentifier;IsParameter:boolean;ParameterIndex:integer);
  var Item:PBESENHashMapItem;
  begin
   if assigned(Variables) then begin
    Item:=VariableHashMap.GetKey(Identifier.Name);
    if not assigned(Item) then begin
     Item:=VariableHashMap.NewKey(Identifier.Name,true);
     Item^.Value:=Variables.Add(Identifier);
     Identifier.Index:=Item^.Value;
     Identifier.IsParameter:=IsParameter;
     Identifier.ParameterIndex:=ParameterIndex;
    end;
   end;
  end;
  function Visit(ToVisit:TBESENASTNode):TBESENASTNode;
  var Counter,NonEmptyStatementCount:integer;
      OldFunctions:TBESENPointerList;
      OldVariables:TBESENPointerList;
      OldVariableHashMap:TBESENHashMap;
      OldAreLocalsOptimizable,OldIsFunction,OldIsMaybeArgumentsObjectUsed,OldIsStrict:boolean;
      OldFunctionLiteral:TBESENASTNodeFunctionLiteral;
 begin
   result:=ToVisit;
   if assigned(ToVisit) then begin
    if ToVisit is TBESENASTNodeStatement then begin
     if TBESENASTNodeStatement(ToVisit).Location.LineNumber>0 then begin
      TBESEN(Instance).LineNumber:=TBESENASTNodeStatement(ToVisit).Location.LineNumber;
     end;
    end;
    case ToVisit.NodeType of
     bntNONE:begin
     end;
     bntEXPRESSION:begin
     end;
     bntLITERAL:begin
     end;
     bntIDENTIFIER:begin
      TBESENASTNodeIdentifier(ToVisit).ID:=TBESEN(Instance).KeyIDManager.Get(TBESENASTNodeIdentifier(ToVisit).Name);
      if (TBESENASTNodeIdentifier(ToVisit).Name='eval') or (TBESENASTNodeIdentifier(ToVisit).Name='arguments') then begin
       IsMaybeArgumentsObjectUsed:=true;
      end;
     end;
     bntVARIABLEDECLARATION:begin
      TBESENASTNodeVariableDeclaration(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeVariableDeclaration(ToVisit).Identifier));
      TBESENASTNodeVariableDeclaration(ToVisit).Expression:=pointer(Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression));
      AddVariable(TBESENASTNodeVariableDeclaration(ToVisit).Identifier,false,-1);
     end;
     bntVARIABLEEXPRESSION:begin
      for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
       TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]:=pointer(Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]));
      end;
     end;
     bntFUNCTIONBODY:begin
      OldFunctions:=Functions;
      OldVariables:=Variables;
      OldVariableHashMap:=VariableHashMap;
      OldAreLocalsOptimizable:=AreLocalsOptimizable;
      OldIsStrict:=TBESEN(Instance).IsStrict;
      Functions:=TBESENPointerList.Create;
      Variables:=TBESENPointerList.Create;
      VariableHashMap:=TBESENHashMap.Create;
      try
       TBESEN(Instance).IsStrict:=TBESENASTNodeFunctionBody(ToVisit).IsStrict;
       AreLocalsOptimizable:=true;
       if IsFunction and assigned(FunctionLiteral) then begin
        for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Parameters)-1 do begin
         TBESENASTNodeFunctionBody(ToVisit).Parameters[Counter]:=pointer(Visit(TBESENASTNodeFunctionBody(ToVisit).Parameters[Counter]));
         AddVariable(TBESENASTNodeFunctionBody(ToVisit).Parameters[Counter],true,Counter);
        end;
       end;
       NonEmptyStatementCount:=0;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
        TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]:=pointer(Visit(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]));
        if assigned(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]) and (TBESENASTNodeFunctionBody(ToVisit).Statements[Counter].NodeType<>bntEMPTYSTATEMENT) then begin
         inc(NonEmptyStatementCount);
        end;
       end;
       SetLength(TBESENASTNodeFunctionBody(ToVisit).Functions,Functions.Count);
       for Counter:=0 to Functions.Count-1 do begin
        TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]:=Functions[Counter];
       end;
       SetLength(TBESENASTNodeFunctionBody(ToVisit).Variables,Variables.Count);
       for Counter:=0 to Variables.Count-1 do begin
        TBESENASTNodeFunctionBody(ToVisit).Variables[Counter]:=Variables[Counter];
       end;
       TBESENASTNodeFunctionBody(ToVisit).EnableLocalsOptimization:=IsFunction and AreLocalsOptimizable;
       TBESENASTNodeFunctionBody(ToVisit).DisableArgumentsObject:=not (IsFunction and IsMaybeArgumentsObjectUsed);
       TBESENASTNodeFunctionBody(ToVisit).IsEmpty:=NonEmptyStatementCount=0;
      finally
       BESENFreeAndNil(Functions);
       BESENFreeAndNil(Variables);
       BESENFreeAndNil(VariableHashMap);
       Functions:=OldFunctions;
       Variables:=OldVariables;
       VariableHashMap:=OldVariableHashMap;
       TBESEN(Instance).IsStrict:=OldIsStrict;
       AreLocalsOptimizable:=OldAreLocalsOptimizable;
      end;
     end;
     bntFUNCTIONLITERAL:begin
      OldIsFunction:=IsFunction;
      OldFunctionLiteral:=FunctionLiteral;
      OldIsMaybeArgumentsObjectUsed:=IsMaybeArgumentsObjectUsed;
      IsMaybeArgumentsObjectUsed:=false;
      IsFunction:=true;
      FunctionLiteral:=TBESENASTNodeFunctionLiteral(ToVisit);
      TBESENASTNodeFunctionLiteral(ToVisit).Body:=pointer(Visit(TBESENASTNodeFunctionLiteral(ToVisit).Body));
      FunctionLiteral:=OldFunctionLiteral;
      IsFunction:=OldIsFunction;
      IsMaybeArgumentsObjectUsed:=OldIsMaybeArgumentsObjectUsed;
     end;
     bntSTATEMENT:begin
     end;
     bntVARIABLESTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
       TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]:=pointer(Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]));
      end;
     end;
     bntFUNCTIONDECLARATION:begin
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal:=pointer(Visit(TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal));
      end;
      if assigned(Functions) then begin
       Functions.Add(ToVisit);
       result:=TBESENASTNodeEmptyStatement.Create(Instance);
       TBESENASTNodeEmptyStatement(result).Location.LineNumber:=TBESENASTNodeFunctionDeclaration(ToVisit).Location.LineNumber;
      end;
     end;
     bntEXPRESSIONSTATEMENT:begin
      TBESENASTNodeExpressionStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression));
     end;
     bntEMPTYSTATEMENT:begin
     end;
     bntBLOCKSTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeBlockStatement(ToVisit).Statements)-1 do begin
       TBESENASTNodeBlockStatement(ToVisit).Statements[Counter]:=pointer(Visit(TBESENASTNodeBlockStatement(ToVisit).Statements[Counter]));
      end;
     end;
     bntDEBUGGERSTATEMENT:begin
     end;
     bntBREAKSTATEMENT:begin
      TBESENASTNodeBreakStatement(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeBreakStatement(ToVisit).Identifier));
     end;
     bntCONTINUESTATEMENT:begin
      TBESENASTNodeContinueStatement(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeContinueStatement(ToVisit).Identifier));
     end;
     bntDOSTATEMENT:begin
      TBESENASTNodeDoStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeDoStatement(ToVisit).Statement));
      TBESENASTNodeDoStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeDoStatement(ToVisit).Expression));
     end;
     bntWHILESTATEMENT:begin
      TBESENASTNodeWhileStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeWhileStatement(ToVisit).Expression));
      TBESENASTNodeWhileStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeWhileStatement(ToVisit).Statement));
     end;
     bntWITHSTATEMENT:begin
      if TBESEN(Instance).IsStrict then begin
       raise EBESENSyntaxError.Create('"with" not allowed here');
      end;
      TBESENASTNodeWithStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeWithStatement(ToVisit).Expression));
      TBESENASTNodeWithStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeWithStatement(ToVisit).Statement));
     end;
     bntFORSTATEMENT:begin
      TBESENASTNodeForStatement(ToVisit).Initial:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Initial));
      TBESENASTNodeForStatement(ToVisit).Condition:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Condition));
      TBESENASTNodeForStatement(ToVisit).Increment:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Increment));
      TBESENASTNodeForStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Statement));
     end;
     bntFORINSTATEMENT:begin
      TBESENASTNodeForInStatement(ToVisit).Variable:=pointer(Visit(TBESENASTNodeForInStatement(ToVisit).Variable));
      TBESENASTNodeForInStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeForInStatement(ToVisit).Expression));
      TBESENASTNodeForInStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeForInStatement(ToVisit).Statement));
     end;
     bntIFSTATEMENT:begin
      TBESENASTNodeIfStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeIfStatement(ToVisit).Expression));
      TBESENASTNodeIfStatement(ToVisit).TrueStatement:=pointer(Visit(TBESENASTNodeIfStatement(ToVisit).TrueStatement));
      TBESENASTNodeIfStatement(ToVisit).FalseStatement:=pointer(Visit(TBESENASTNodeIfStatement(ToVisit).FalseStatement));
     end;
     bntLABELLEDSTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeLabelledStatement(ToVisit).Identifiers)-1 do begin
       TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]:=pointer(Visit(TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]));
      end;
      TBESENASTNodeLabelledStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeLabelledStatement(ToVisit).Statement));
     end;
     bntCASESTATEMENT:begin
      TBESENASTNodeBreakStatement(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeBreakStatement(ToVisit).Identifier));
      for Counter:=0 to length(TBESENASTNodeCaseStatement(ToVisit).Statements)-1 do begin
       TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]:=pointer(Visit(TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]));
      end;
     end;
     bntSWITCHSTATEMENT:begin
      TBESENASTNodeSwitchStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeSwitchStatement(ToVisit).Expression));
      for Counter:=0 to length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements)-1 do begin
       TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter]:=pointer(Visit(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter]));
      end;
     end;
     bntTHROWSTATEMENT:begin
      TBESENASTNodeThrowStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeThrowStatement(ToVisit).Expression));
     end;
     bntTRYSTATEMENT:begin
      TBESENASTNodeTryStatement(ToVisit).TryBlock:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock));
      TBESENASTNodeTryStatement(ToVisit).CatchIdentifier:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier));
      TBESENASTNodeTryStatement(ToVisit).CatchBlock:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).CatchBlock));
      TBESENASTNodeTryStatement(ToVisit).FinallyBlock:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).FinallyBlock));
     end;
     bntARRAYLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeArrayLiteral(ToVisit).Elements)-1 do begin
       if assigned(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]) then begin
        TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]:=pointer(Visit(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]));
       end;
      end;
     end;
     bntBINARYEXPRESSION:begin
      TBESENASTNodeBinaryExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTEXPRESSION:begin
      TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTOPERATOREXPRESSION:begin
      TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTMULTIPLYEXPRESSION:begin
      TBESENASTNodeAssignmentMultiplyExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentMultiplyExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTDIVIDEEXPRESSION:begin
      TBESENASTNodeAssignmentDivideExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentDivideExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTMODULOEXPRESSION:begin
      TBESENASTNodeAssignmentModuloExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentModuloExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTPLUSEXPRESSION:begin
      TBESENASTNodeAssignmentPlusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentPlusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTMINUSEXPRESSION:begin
      TBESENASTNodeAssignmentMinusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentMinusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTSHIFTLEFTEXPRESSION:begin
      TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTSHIFTRIGHTEXPRESSION:begin
      TBESENASTNodeAssignmentShiftRightExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentShiftRightExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTBITWISEANDEXPRESSION:begin
      TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTBITWISEXOREXPRESSION:begin
      TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTBITWISEOREXPRESSION:begin
      TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).RightExpression));
     end;
     bntBINARYOPERATOREXPRESSION:begin
      TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression));
     end;
     bntBINARYCOMMAEXPRESSION:begin
      TBESENASTNodeBinaryCommaExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryCommaExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).RightExpression));
     end;
     bntBINARYDIVIDEEXPRESSION:begin
      TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression));
     end;
     bntBINARYMODULOEXPRESSION:begin
      TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression));
     end;
     bntBINARYMULTIPLYEXPRESSION:begin
      TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression));
     end;
     bntBINARYPLUSEXPRESSION:begin
      TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression));
     end;
     bntBINARYMINUSEXPRESSION:begin
      TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression));
     end;
     bntBINARYSHIFTLEFTEXPRESSION:begin
      TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression));
     end;
     bntBINARYSHIFTRIGHTEXPRESSION:begin
      TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression));
     end;
     bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression));
     end;
     bntBINARYGREATERTHANEXPRESSION:begin
      TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression));
     end;
     bntBINARYGREATERTHANOREQUALEXPRESSION:begin
      TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression));
     end;
     bntBINARYLESSTHANEXPRESSION:begin
      TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression));
     end;
     bntBINARYLESSTHANOREQUALEXPRESSION:begin
      TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression));
     end;
     bntBINARYINSTANCEOFEXPRESSION:begin
      TBESENASTNodeBinaryInstanceOfExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryInstanceOfExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).RightExpression));
     end;
     bntBINARYINEXPRESSION:begin
      TBESENASTNodeBinaryInExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryInExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryInExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryInExpression(ToVisit).RightExpression));
     end;
     bntBINARYEQUALEQUALEXPRESSION:begin
      TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression));
     end;
     bntBINARYEQUALEQUALEQUALEXPRESSION:begin
      TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression));
     end;
     bntBINARYNOTEQUALEXPRESSION:begin
      TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression));
     end;
     bntBINARYNOTEQUALEQUALEXPRESSION:begin
      TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression));
     end;
     bntBINARYBITWISEANDEXPRESSION:begin
      TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression));
     end;
     bntBINARYBITWISEXOREXPRESSION:begin
      TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression));
     end;
     bntBINARYBITWISEOREXPRESSION:begin
      TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression));
     end;
     bntBOOLEANLITERAL:begin
     end;
     bntCALLEXPRESSION:begin
      TBESENASTNodeCallExpression(ToVisit).TheFunction:=pointer(Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction));
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]:=pointer(Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]));
      end;
     end;
     bntNEWEXPRESSION:begin
      TBESENASTNodeNewExpression(ToVisit).TheFunction:=pointer(Visit(TBESENASTNodeNewExpression(ToVisit).TheFunction));
      for Counter:=0 to length(TBESENASTNodeNewExpression(ToVisit).Arguments)-1 do begin
       TBESENASTNodeNewExpression(ToVisit).Arguments[Counter]:=pointer(Visit(TBESENASTNodeNewExpression(ToVisit).Arguments[Counter]));
      end;
     end;
     bntCONDITIONALEXPRESSION:begin
      TBESENASTNodeConditionalExpression(ToVisit).Expression:=pointer(Visit(TBESENASTNodeConditionalExpression(ToVisit).Expression));
      TBESENASTNodeConditionalExpression(ToVisit).TrueExpression:=pointer(Visit(TBESENASTNodeConditionalExpression(ToVisit).TrueExpression));
      TBESENASTNodeConditionalExpression(ToVisit).FalseExpression:=pointer(Visit(TBESENASTNodeConditionalExpression(ToVisit).FalseExpression));
     end;
     bntUNARYEXPRESSION:begin
      TBESENASTNodeUnaryExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryExpression(ToVisit).SubExpression));
     end;
     bntUNARYOPERATOREXPRESSION:begin
      TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression));
     end;
     bntUNARYPLUSEXPRESSION:begin
      TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression));
     end;
     bntUNARYMINUSEXPRESSION:begin
      TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression));
     end;
     bntUNARYBITWISENOTEXPRESSION:begin
      TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression));
     end;
     bntUNARYLOGICALNOTEXPRESSION:begin
      TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression));
     end;
     bntUNARYVOIDEXPRESSION:begin
      TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression));
     end;
     bntUNARYTYPEOFEXPRESSION:begin
      TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression));
     end;
     bntPROPERTYEXPRESSION:begin
      TBESENASTNodePropertyExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodePropertyExpression(ToVisit).LeftExpression));
      TBESENASTNodePropertyExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodePropertyExpression(ToVisit).RightExpression));
     end;
     bntLOGICALANDEXPRESSION:begin
      TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression));
      TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression));
     end;
     bntLOGICALOREXPRESSION:begin
      TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression));
      TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression));
     end;
     bntDELETEEXPRESSION:begin
      TBESENASTNodeDeleteExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeDeleteExpression(ToVisit).SubExpression));
     end;
     bntPOSTFIXINCEXPRESSION:begin
      TBESENASTNodePostfixIncExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression));
     end;
     bntPOSTFIXDECEXPRESSION:begin
      TBESENASTNodePostfixDecExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression));
     end;
     bntPREFIXINCEXPRESSION:begin
      TBESENASTNodePrefixIncExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression));
     end;
     bntPREFIXDECEXPRESSION:begin
      TBESENASTNodePrefixDecExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression));
     end;
     bntNULLLITERAL:begin
     end;
     bntNUMBERLITERAL:begin
     end;
     bntREGEXPLITERAL:begin
     end;
     bntSTRINGLITERAL:begin
     end;
     bntTHISLITERAL:begin
     end;
     bntOBJECTLITERALPROPERTY:begin
      TBESENASTNodeObjectLiteralProperty(ToVisit).Value:=pointer(Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Value));
      if assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container) then begin
       TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal:=pointer(Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal));
      end;
     end;
     bntOBJECTLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeObjectLiteral(ToVisit).Properties)-1 do begin
       TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter]:=pointer(Visit(TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter]));
      end;
     end;
     bntRETURNSTATEMENT:begin
      if not IsFunction then begin
       raise EBESENSyntaxError.Create('Return outside a function');
      end;
      TBESENASTNodeReturnStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeReturnStatement(ToVisit).Expression));
     end;
     bntPROGRAM:begin
      OldIsFunction:=IsFunction;
      OldFunctionLiteral:=FunctionLiteral;
      OldIsMaybeArgumentsObjectUsed:=IsMaybeArgumentsObjectUsed;
      IsFunction:=false;
      IsMaybeArgumentsObjectUsed:=false;
      FunctionLiteral:=nil;
      TBESENASTNodeProgram(ToVisit).Body:=pointer(Visit(TBESENASTNodeProgram(ToVisit).Body));
      FunctionLiteral:=OldFunctionLiteral;
      IsFunction:=OldIsFunction;
      IsMaybeArgumentsObjectUsed:=IsMaybeArgumentsObjectUsed;
     end;
     bntFUNCTIONEXPRESSION:begin
      if assigned(TBESENASTNodeFunctionExpression(ToVisit).Container) then begin
       TBESENASTNodeFunctionExpression(ToVisit).Container.Literal:=pointer(Visit(TBESENASTNodeFunctionExpression(ToVisit).Container.Literal));
      end;
     end;
    end;
   end;
  end;
 begin
  Functions:=nil;
  Variables:=nil;
  VariableHashMap:=nil;
  AreLocalsOptimizable:=true;
  IsFunction:=false;
  IsMaybeArgumentsObjectUsed:=false;
  FunctionLiteral:=nil;
  Visit(RootNode);
 end;
 function Optimize(RootNode:TBESENASTNode):TBESENASTNode;
 var vl,vr:TBESENValue;
  function Visit(ToVisit:TBESENASTNode):TBESENASTNode;
  var Counter,LastLineNumber:integer;
      nr:TBESENNumber;
      sr:TBESENString;
      br,OldIsStrict:TBESENBoolean;
  begin
   result:=ToVisit;
   if assigned(ToVisit) then begin
    if ToVisit is TBESENASTNodeStatement then begin
     if TBESENASTNodeStatement(ToVisit).Location.LineNumber>0 then begin
      TBESEN(Instance).LineNumber:=TBESENASTNodeStatement(ToVisit).Location.LineNumber;
     end;
    end;
    case ToVisit.NodeType of
     bntNONE:begin
     end;
     bntEXPRESSION:begin
     end;
     bntLITERAL:begin
     end;
     bntIDENTIFIER:begin
     end;
     bntVARIABLEDECLARATION:begin
      TBESENASTNodeVariableDeclaration(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeVariableDeclaration(ToVisit).Identifier));
      TBESENASTNodeVariableDeclaration(ToVisit).Expression:=pointer(Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression));
     end;
     bntVARIABLEEXPRESSION:begin
      for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
       TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]:=pointer(Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]));
      end;
     end;
     bntFUNCTIONBODY:begin
      OldIsStrict:=TBESEN(Instance).IsStrict;
      try
       TBESEN(Instance).IsStrict:=TBESENASTNodeFunctionBody(ToVisit).IsStrict;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Parameters)-1 do begin
        TBESENASTNodeFunctionBody(ToVisit).Parameters[Counter]:=pointer(Visit(TBESENASTNodeFunctionBody(ToVisit).Parameters[Counter]));
       end;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Functions)-1 do begin
        TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]:=pointer(Visit(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]));
       end;
       TrimStuffAfterReturnBreakContinue(ToVisit,TBESENASTNodeFunctionBody(ToVisit).Statements);
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
        TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]:=pointer(Visit(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]));
       end;
      finally
       TBESEN(Instance).IsStrict:=OldIsStrict;
      end;
     end;
     bntFUNCTIONLITERAL:begin
      TBESENASTNodeFunctionLiteral(ToVisit).Body:=pointer(Visit(TBESENASTNodeFunctionLiteral(ToVisit).Body));
     end;
     bntSTATEMENT:begin
     end;
     bntVARIABLESTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
       TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]:=pointer(Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]));
      end;
     end;
     bntFUNCTIONDECLARATION:begin
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal:=pointer(Visit(TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal));
      end;
     end;
     bntEXPRESSIONSTATEMENT:begin
      TBESENASTNodeExpressionStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression));
     end;
     bntEMPTYSTATEMENT:begin
     end;
     bntBLOCKSTATEMENT:begin
      TrimStuffAfterReturnBreakContinue(ToVisit,TBESENASTNodeBlockStatement(ToVisit).Statements);
      for Counter:=0 to length(TBESENASTNodeBlockStatement(ToVisit).Statements)-1 do begin
       TBESENASTNodeBlockStatement(ToVisit).Statements[Counter]:=pointer(Visit(TBESENASTNodeBlockStatement(ToVisit).Statements[Counter]));
      end;
     end;
     bntDEBUGGERSTATEMENT:begin
     end;
     bntBREAKSTATEMENT:begin
      TBESENASTNodeBreakStatement(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeBreakStatement(ToVisit).Identifier));
     end;
     bntCONTINUESTATEMENT:begin
      TBESENASTNodeContinueStatement(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeContinueStatement(ToVisit).Identifier));
     end;
     bntDOSTATEMENT:begin
      TBESENASTNodeDoStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeDoStatement(ToVisit).Statement));
      TBESENASTNodeDoStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeDoStatement(ToVisit).Expression));
      if ConvertToValue(TBESENASTNodeDoStatement(ToVisit).Expression,vl) then begin
       if not TBESEN(Instance).ToBool(vl) then begin
        if assigned(TBESENASTNodeDoStatement(ToVisit).Statement) then begin
         result:=TBESENASTNodeEmptyStatement(ToVisit).Create(Instance);
         AddTrashNode(result,TBESENASTNodeDoStatement(ToVisit).Expression);
         AddTrashNode(result,TBESENASTNodeDoStatement(ToVisit).Statement);
         TBESENASTNodeDoStatement(ToVisit).Statement:=nil;
         TBESENASTNodeDoStatement(ToVisit).Expression:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeDoStatement(ToVisit).Statement;
         AddTrashNode(result,TBESENASTNodeDoStatement(ToVisit).Expression);
         TBESENASTNodeDoStatement(ToVisit).Statement:=nil;
         TBESENASTNodeDoStatement(ToVisit).Expression:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end;
      end;
      if not assigned(result) then begin
       result:=TBESENASTNodeEmptyStatement(ToVisit).Create(Instance);
      end;
     end;
     bntWHILESTATEMENT:begin
      TBESENASTNodeWhileStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeWhileStatement(ToVisit).Expression));
      TBESENASTNodeWhileStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeWhileStatement(ToVisit).Statement));
      if ConvertToValue(TBESENASTNodeWhileStatement(ToVisit).Expression,vl) then begin
       if not TBESEN(Instance).ToBool(vl) then begin
        result:=TBESENASTNodeEmptyStatement.Create(Instance);
        TBESENASTNodeEmptyStatement(result).Location.LineNumber:=TBESENASTNodeFunctionDeclaration(ToVisit).Location.LineNumber;
        AddTrashNode(result,TBESENASTNodeWhileStatement(ToVisit).Expression);
        AddTrashNode(result,TBESENASTNodeWhileStatement(ToVisit).Statement);
        TBESENASTNodeWhileStatement(ToVisit).Statement:=nil;
        TBESENASTNodeWhileStatement(ToVisit).Expression:=nil;
        BESENFreeAndNil(ToVisit);
       end;
      end;
      if not assigned(result) then begin
       result:=TBESENASTNodeEmptyStatement(ToVisit).Create(Instance);
      end;
     end;
     bntWITHSTATEMENT:begin
      TBESENASTNodeWithStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeWithStatement(ToVisit).Expression));
      TBESENASTNodeWithStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeWithStatement(ToVisit).Statement));
     end;
     bntFORSTATEMENT:begin
      TBESENASTNodeForStatement(ToVisit).Initial:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Initial));
      TBESENASTNodeForStatement(ToVisit).Condition:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Condition));
      TBESENASTNodeForStatement(ToVisit).Increment:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Increment));
      TBESENASTNodeForStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeForStatement(ToVisit).Statement));
     end;
     bntFORINSTATEMENT:begin
      TBESENASTNodeForInStatement(ToVisit).Variable:=pointer(Visit(TBESENASTNodeForInStatement(ToVisit).Variable));
      TBESENASTNodeForInStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeForInStatement(ToVisit).Expression));
      TBESENASTNodeForInStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeForInStatement(ToVisit).Statement));
     end;
     bntIFSTATEMENT:begin
      LastLineNumber:=ToVisit.Location.LineNumber;
      TBESENASTNodeIfStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeIfStatement(ToVisit).Expression));
      TBESENASTNodeIfStatement(ToVisit).TrueStatement:=pointer(Visit(TBESENASTNodeIfStatement(ToVisit).TrueStatement));
      TBESENASTNodeIfStatement(ToVisit).FalseStatement:=pointer(Visit(TBESENASTNodeIfStatement(ToVisit).FalseStatement));
      if ConvertToValue(TBESENASTNodeIfStatement(ToVisit).Expression,vl) then begin
       if TBESEN(Instance).ToBool(vl) then begin
        if assigned(TBESENASTNodeIfStatement(ToVisit).TrueStatement) then begin
         result:=TBESENASTNodeIfStatement(ToVisit).TrueStatement;
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).Expression);
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).FalseStatement);
         TBESENASTNodeIfStatement(ToVisit).Expression:=nil;
         TBESENASTNodeIfStatement(ToVisit).TrueStatement:=nil;
         TBESENASTNodeIfStatement(ToVisit).FalseStatement:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeEmptyStatement.Create(Instance);
         result.Location.LineNumber:=LastLineNumber;
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).Expression);
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).TrueStatement);
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).FalseStatement);
         TBESENASTNodeIfStatement(ToVisit).Expression:=nil;
         TBESENASTNodeIfStatement(ToVisit).TrueStatement:=nil;
         TBESENASTNodeIfStatement(ToVisit).FalseStatement:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end else begin
        if assigned(TBESENASTNodeIfStatement(ToVisit).FalseStatement) then begin
         result:=TBESENASTNodeIfStatement(ToVisit).FalseStatement;
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).Expression);
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).TrueStatement);
         TBESENASTNodeIfStatement(ToVisit).Expression:=nil;
         TBESENASTNodeIfStatement(ToVisit).TrueStatement:=nil;
         TBESENASTNodeIfStatement(ToVisit).FalseStatement:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeEmptyStatement.Create(Instance);
         result.Location.LineNumber:=LastLineNumber;
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).Expression);
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).TrueStatement);
         AddTrashNode(result,TBESENASTNodeIfStatement(ToVisit).FalseStatement);
         TBESENASTNodeIfStatement(ToVisit).Expression:=nil;
         TBESENASTNodeIfStatement(ToVisit).TrueStatement:=nil;
         TBESENASTNodeIfStatement(ToVisit).FalseStatement:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end;
      end;
      if not assigned(result) then begin
       result:=TBESENASTNodeEmptyStatement.Create(Instance);
       result.Location.LineNumber:=LastLineNumber;
      end;
     end;
     bntLABELLEDSTATEMENT:begin
      for Counter:=0 to length(TBESENASTNodeLabelledStatement(ToVisit).Identifiers)-1 do begin
       TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]:=pointer(Visit(TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]));
      end;
      TBESENASTNodeLabelledStatement(ToVisit).Statement:=pointer(Visit(TBESENASTNodeLabelledStatement(ToVisit).Statement));
     end;
     bntCASESTATEMENT:begin
      TBESENASTNodeBreakStatement(ToVisit).Identifier:=pointer(Visit(TBESENASTNodeBreakStatement(ToVisit).Identifier));
      TrimStuffAfterReturnBreakContinue(ToVisit,TBESENASTNodeCaseStatement(ToVisit).Statements);
      for Counter:=0 to length(TBESENASTNodeCaseStatement(ToVisit).Statements)-1 do begin
       TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]:=pointer(Visit(TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]));
      end;
     end;
     bntSWITCHSTATEMENT:begin
      TBESENASTNodeSwitchStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeSwitchStatement(ToVisit).Expression));
      for Counter:=0 to length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements)-1 do begin
       TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter]:=pointer(Visit(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter]));
      end;
     end;
     bntTHROWSTATEMENT:begin
      TBESENASTNodeThrowStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeThrowStatement(ToVisit).Expression));
     end;
     bntTRYSTATEMENT:begin
      TBESENASTNodeTryStatement(ToVisit).TryBlock:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock));
      TBESENASTNodeTryStatement(ToVisit).CatchIdentifier:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier));
      TBESENASTNodeTryStatement(ToVisit).CatchBlock:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).CatchBlock));
      TBESENASTNodeTryStatement(ToVisit).FinallyBlock:=pointer(Visit(TBESENASTNodeTryStatement(ToVisit).FinallyBlock));
     end;
     bntARRAYLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeArrayLiteral(ToVisit).Elements)-1 do begin
       if assigned(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]) then begin
        TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]:=pointer(Visit(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]));
       end;
      end;
     end;
     bntBINARYEXPRESSION:begin
     end;
     bntASSIGNMENTEXPRESSION:begin
      TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTOPERATOREXPRESSION:begin
      TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTMULTIPLYEXPRESSION:begin
      TBESENASTNodeAssignmentMultiplyExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentMultiplyExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentMultiplyExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTDIVIDEEXPRESSION:begin
      TBESENASTNodeAssignmentDivideExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentDivideExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentDivideExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTMODULOEXPRESSION:begin
      TBESENASTNodeAssignmentModuloExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentModuloExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentModuloExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTPLUSEXPRESSION:begin
      TBESENASTNodeAssignmentPlusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentPlusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentPlusExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTMINUSEXPRESSION:begin
      TBESENASTNodeAssignmentMinusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentMinusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentMinusExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTSHIFTLEFTEXPRESSION:begin
      TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftLeftExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTSHIFTRIGHTEXPRESSION:begin
      TBESENASTNodeAssignmentShiftRightExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentShiftRightExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentShiftRightUnsignedExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTBITWISEANDEXPRESSION:begin
      TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseAndExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTBITWISEXOREXPRESSION:begin
      TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseXorExpression(ToVisit).RightExpression));
     end;
     bntASSIGNMENTBITWISEOREXPRESSION:begin
      TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).LeftExpression));
      TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeAssignmentBitwiseOrExpression(ToVisit).RightExpression));
     end;
     bntBINARYOPERATOREXPRESSION:begin
      TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression));
     end;
     bntBINARYCOMMAEXPRESSION:begin
      TBESENASTNodeBinaryCommaExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryCommaExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryCommaExpression(ToVisit).RightExpression));
     end;
     bntBINARYDIVIDEEXPRESSION:begin
      TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryDivideExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryDivideExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESEN(Instance).ToNum(vl)/TBESEN(Instance).ToNum(vr);
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYMODULOEXPRESSION:begin
      TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryModuloExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryModuloExpression(ToVisit).RightExpression,vr) then begin
       nr:=BESENModulo(TBESEN(Instance).ToNum(vl),TBESEN(Instance).ToNum(vr));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYMULTIPLYEXPRESSION:begin
      TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryMultiplyExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryMultiplyExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESEN(Instance).ToNum(vl)*TBESEN(Instance).ToNum(vr);
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYPLUSEXPRESSION:begin
      TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression,vr) then begin
       if (TBESENASTNodeBinaryPlusExpression(ToVisit).LeftExpression is TBESENASTNodeStringLiteral) or (TBESENASTNodeBinaryPlusExpression(ToVisit).RightExpression is TBESENASTNodeStringLiteral) then begin
        sr:=TBESEN(Instance).ToStr(vl)+TBESEN(Instance).ToStr(vr);
        BESENFreeAndNil(ToVisit);
        result:=TBESENASTNodeStringLiteral.Create(Instance);
        TBESENASTNodeStringLiteral(result).Value:=sr;
       end else begin
        nr:=TBESEN(Instance).ToNum(vl)+TBESEN(Instance).ToNum(vr);
        BESENFreeAndNil(ToVisit);
        result:=TBESENASTNodeNumberLiteral.Create(Instance);
        TBESENASTNodeNumberLiteral(result).Value:=nr;
       end;
      end;
     end;
     bntBINARYMINUSEXPRESSION:begin
      TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryMinusExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryMinusExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESEN(Instance).ToNum(vl)-TBESEN(Instance).ToNum(vr);
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYSHIFTLEFTEXPRESSION:begin
      TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryShiftLeftExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESENINT32(TBESEN(Instance).ToInt32(vl) shl (TBESEN(Instance).ToUInt32(vr) and 31));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYSHIFTRIGHTEXPRESSION:begin
      TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryShiftRightExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryShiftRightExpression(ToVisit).RightExpression,vr) then begin
       nr:=sar(TBESEN(Instance).ToInt32(vl),(TBESEN(Instance).ToUInt32(vr) and 31));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryShiftRightUnsignedExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESENUINT32(TBESEN(Instance).ToUInt32(vl) shr (TBESEN(Instance).ToUInt32(vr) and 31));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYGREATERTHANEXPRESSION:begin
      TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryGreaterThanExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=TBESEN(Instance).EqualityExpressionCompare(vl,vr)>0;
      end;
     end;
     bntBINARYGREATERTHANOREQUALEXPRESSION:begin
      TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryGreaterThanOrEqualExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=TBESEN(Instance).EqualityExpressionCompare(vl,vr)>=0;
      end;
     end;
     bntBINARYLESSTHANEXPRESSION:begin
      TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryLessThanExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryLessThanExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=TBESEN(Instance).EqualityExpressionCompare(vl,vr)<0;
      end;
     end;
     bntBINARYLESSTHANOREQUALEXPRESSION:begin
      TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryLessThanOrEqualExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=TBESEN(Instance).EqualityExpressionCompare(vl,vr)<=0;
      end;
     end;
     bntBINARYINSTANCEOFEXPRESSION:begin
      TBESENASTNodeBinaryInstanceOfExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryInstanceOfExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryInstanceOfExpression(ToVisit).RightExpression));
     end;
     bntBINARYINEXPRESSION:begin
      TBESENASTNodeBinaryInExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryInExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryInExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryInExpression(ToVisit).RightExpression));
     end;
     bntBINARYEQUALEQUALEXPRESSION:begin
      TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryEqualEqualExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=TBESEN(Instance).EqualityExpressionEquals(vl,vr);
      end;
     end;
     bntBINARYEQUALEQUALEQUALEXPRESSION:begin
      TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryEqualEqualEqualExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=BESENEqualityExpressionStrictEquals(vl,vr);
      end;
     end;
     bntBINARYNOTEQUALEXPRESSION:begin
      TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryNotEqualExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryNotEqualExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=not TBESEN(Instance).EqualityExpressionEquals(vl,vr);
      end;
     end;
     bntBINARYNOTEQUALEQUALEXPRESSION:begin
      TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryNotEqualEqualExpression(ToVisit).RightExpression,vr) then begin
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=not BESENEqualityExpressionStrictEquals(vl,vr);
      end;
     end;
     bntBINARYBITWISEANDEXPRESSION:begin
      TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryBitwiseAndExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESENINT32(TBESEN(Instance).ToInt32(vl) and TBESEN(Instance).ToInt32(vr));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYBITWISEXOREXPRESSION:begin
      TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryBitwiseXorExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESENINT32(TBESEN(Instance).ToInt32(vl) xor TBESEN(Instance).ToInt32(vr));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBINARYBITWISEOREXPRESSION:begin
      TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression));
      TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).LeftExpression,vl) and ConvertToValue(TBESENASTNodeBinaryBitwiseOrExpression(ToVisit).RightExpression,vr) then begin
       nr:=TBESENINT32(TBESEN(Instance).ToInt32(vl) or TBESEN(Instance).ToInt32(vr));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntBOOLEANLITERAL:begin
     end;
     bntCALLEXPRESSION:begin
      TBESENASTNodeCallExpression(ToVisit).TheFunction:=pointer(Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction));
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]:=pointer(Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]));
      end;
     end;
     bntNEWEXPRESSION:begin
      TBESENASTNodeNewExpression(ToVisit).TheFunction:=pointer(Visit(TBESENASTNodeNewExpression(ToVisit).TheFunction));
      for Counter:=0 to length(TBESENASTNodeNewExpression(ToVisit).Arguments)-1 do begin
       TBESENASTNodeNewExpression(ToVisit).Arguments[Counter]:=pointer(Visit(TBESENASTNodeNewExpression(ToVisit).Arguments[Counter]));
      end;
     end;
     bntCONDITIONALEXPRESSION:begin
      TBESENASTNodeConditionalExpression(ToVisit).TrueExpression:=pointer(Visit(TBESENASTNodeConditionalExpression(ToVisit).TrueExpression));
      TBESENASTNodeConditionalExpression(ToVisit).FalseExpression:=pointer(Visit(TBESENASTNodeConditionalExpression(ToVisit).FalseExpression));
      if ConvertToValue(TBESENASTNodeConditionalExpression(ToVisit).Expression,vl) then begin
       if TBESEN(Instance).ToBool(vl) then begin
        result:=TBESENASTNodeConditionalExpression(ToVisit).TrueExpression;
        TBESENASTNodeConditionalExpression(ToVisit).TrueExpression:=nil;
        BESENFreeAndNil(ToVisit);
       end else begin
        result:=TBESENASTNodeConditionalExpression(ToVisit).FalseExpression;
        TBESENASTNodeConditionalExpression(ToVisit).FalseExpression:=nil;
        BESENFreeAndNil(ToVisit);
       end;
      end;
     end;
     bntUNARYEXPRESSION:begin
      TBESENASTNodeUnaryExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryExpression(ToVisit).SubExpression));
     end;
     bntUNARYOPERATOREXPRESSION:begin
      TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression));
     end;
     bntUNARYPLUSEXPRESSION:begin
      TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression));
      if ConvertToValue(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression,vl) then begin
       nr:=TBESEN(Instance).ToNum(vl);
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntUNARYMINUSEXPRESSION:begin
      TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression));
      if ConvertToValue(TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression,vl) then begin
       nr:=TBESEN(Instance).ToNum(vl);
       PBESENDoubleHiLo(@nr)^.Hi:=PBESENDoubleHiLo(@nr)^.Hi xor $80000000;
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntUNARYBITWISENOTEXPRESSION:begin
      TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression));
      if ConvertToValue(TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression,vl) then begin
       nr:=TBESENINT32(not TBESEN(Instance).ToInt32(vl));
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeNumberLiteral.Create(Instance);
       TBESENASTNodeNumberLiteral(result).Value:=nr;
      end;
     end;
     bntUNARYLOGICALNOTEXPRESSION:begin
      TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression));
      if ConvertToValue(TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression,vl) then begin
       br:=not TBESEN(Instance).ToBool(vl);
       BESENFreeAndNil(ToVisit);
       result:=TBESENASTNodeBooleanLiteral.Create(Instance);
       TBESENASTNodeBooleanLiteral(result).Value:=br;
      end;
     end;
     bntUNARYVOIDEXPRESSION:begin
      TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression));
     end;
     bntUNARYTYPEOFEXPRESSION:begin
      TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression));
     end;
     bntPROPERTYEXPRESSION:begin
      TBESENASTNodePropertyExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodePropertyExpression(ToVisit).LeftExpression));
      TBESENASTNodePropertyExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodePropertyExpression(ToVisit).RightExpression));
     end;
     bntLOGICALANDEXPRESSION:begin
      TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression));
      TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression,vl) then begin
       ConvertToValue(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression,vr);
       if vl.ValueType=vr.ValueType then begin
        if TBESEN(Instance).ToBool(vr) then begin
         result:=TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression;
         AddTrashNode(result,TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression);
         TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression;
         AddTrashNode(result,TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression);
         TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end else begin
        if TBESEN(Instance).ToBool(vl) then begin
         result:=TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression;
         AddTrashNode(result,TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression);
         TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression;
         AddTrashNode(result,TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression);
         TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end;
      end;
     end;
     bntLOGICALOREXPRESSION:begin
      TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression:=pointer(Visit(TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression));
      TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression:=pointer(Visit(TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression));
      if ConvertToValue(TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression,vl) then begin
       ConvertToValue(TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression,vr);
       if vl.ValueType=vr.ValueType then begin
        if TBESEN(Instance).ToBool(vr) then begin
         result:=TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression;
         AddTrashNode(result,TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression);
         TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression;
         AddTrashNode(result,TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression);
         TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end else begin
        if TBESEN(Instance).ToBool(vl) then begin
         result:=TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression;
         AddTrashNode(result,TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression);
         TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end else begin
         result:=TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression;
         AddTrashNode(result,TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression);
         TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression:=nil;
         TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression:=nil;
         BESENFreeAndNil(ToVisit);
        end;
       end;
      end;
     end;
     bntDELETEEXPRESSION:begin
      TBESENASTNodeDeleteExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodeDeleteExpression(ToVisit).SubExpression));
     end;
     bntPOSTFIXINCEXPRESSION:begin
      TBESENASTNodePostfixIncExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression));
     end;
     bntPOSTFIXDECEXPRESSION:begin
      TBESENASTNodePostfixDecExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression));
     end;
     bntPREFIXINCEXPRESSION:begin
      TBESENASTNodePrefixIncExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression));
     end;
     bntPREFIXDECEXPRESSION:begin
      TBESENASTNodePrefixDecExpression(ToVisit).SubExpression:=pointer(Visit(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression));
     end;
     bntNULLLITERAL:begin
     end;
     bntNUMBERLITERAL:begin
     end;
     bntREGEXPLITERAL:begin
     end;
     bntSTRINGLITERAL:begin
     end;
     bntTHISLITERAL:begin
     end;
     bntOBJECTLITERALPROPERTY:begin
      TBESENASTNodeObjectLiteralProperty(ToVisit).Value:=pointer(Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Value));
      if assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container) then begin
       TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal:=pointer(Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal));
      end;
     end;
     bntOBJECTLITERAL:begin
      for Counter:=0 to length(TBESENASTNodeObjectLiteral(ToVisit).Properties)-1 do begin
       TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter]:=pointer(Visit(TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter]));
      end;
     end;
     bntRETURNSTATEMENT:begin
      TBESENASTNodeReturnStatement(ToVisit).Expression:=pointer(Visit(TBESENASTNodeReturnStatement(ToVisit).Expression));
     end;
     bntPROGRAM:begin
      TBESENASTNodeProgram(ToVisit).Body:=pointer(Visit(TBESENASTNodeProgram(ToVisit).Body));
     end;
     bntFUNCTIONEXPRESSION:begin
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       TBESENASTNodeFunctionExpression(ToVisit).Container.Literal:=pointer(Visit(TBESENASTNodeFunctionExpression(ToVisit).Container.Literal));
      end;
     end;
    end;
   end;
  end;
 begin
  result:=Visit(RootNode);
 end;
 procedure GenerateByteCode(RootNode:TBESENASTNode);
 var Code:TBESENCode;
     CodeGeneratorContext:TBESENCodeGeneratorContext;
     OptimizeLocals,HasFunctions,DoHasLocalDelete,DoIsComplexFunction,DoHasMaybeDirectEval,DoHoldLocalVariablesInRegisters:boolean;
  procedure Visit(ToVisit:TBESENASTNode;var DestRegNr:integer;CalleeNeedReturnedValue:boolean);
  var Counter,L1,L2,L3,L3a,L3b,P1,P2,PR,PR2,r1,r2,r3,r4,r5,r6,v1:integer;
      OldCode:TBESENCode;
      OldCodeGeneratorContext:TBESENCodeGeneratorContext;
      v:TBESENValue;
      OldVarScope,OldDoHasLocalDelete,OldOptimizeLocals,OldHasFunctions,OldDoIsComplexFunction,OldDoHasMaybeDirectEval,OldDoHoldLocalVariablesInRegisters,DefaultCase,InScope:boolean;
      Patchables:TBESENCodeGeneratorContextPatchables;
      SwitchPatches:array of integer;
      vta,vtb,vtc,vtd,vte,vtTemp,vtBegin,vtInner,vtEnd:TBESENValueTypes;
      RegStates:array[0..3] of TBESENCodeGeneratorContextRegisterStates;
      Operands:array of TBESENINT32;
      hi:PBESENHashMapItem;
      Hash:TBESENHash;
      IsWhat1,IsWhat2:longword;
   function IsValue(const r:integer):boolean;
   begin
    result:=(CodeGeneratorContext.Registers[r].IsWhat and bcgtREFERENCE)=0;
   end;
   function IsPrimitive(const r:integer):boolean;
   begin
    result:=(CodeGeneratorContext.Registers[r].IsWhat and (bcgtREFERENCE or bcgtOBJECT))=0;
   end;
   function IsLocalVariableReference(const r:integer):boolean;
   begin
    result:=DoHoldLocalVariablesInRegisters and ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].Variable>=0));
   end;
   function IsValueBoolean(const r:integer):boolean;
   begin
    result:=(CodeGeneratorContext.Registers[r].IsWhat=bcgtBOOLEAN) or ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].ReferenceValueType=bvtBOOLEAN));
    if (not result) and ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].Variable>=0)) then begin
     result:=CodeGeneratorContext.VariableGetType(Code.Variables[CodeGeneratorContext.Registers[r].Variable].Name)=bvtBOOLEAN;
    end;
   end;
   function IsValueNumber(const r:integer):boolean;
   begin
    result:=(CodeGeneratorContext.Registers[r].IsWhat=bcgtNUMBER) or ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].ReferenceValueType=bvtNUMBER));
    if (not result) and ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].Variable>=0)) then begin
     result:=CodeGeneratorContext.VariableGetType(Code.Variables[CodeGeneratorContext.Registers[r].Variable].Name)=bvtNUMBER;
    end;
   end;
   function IsValueString(const r:integer):boolean;
   begin
    result:=(CodeGeneratorContext.Registers[r].IsWhat=bcgtSTRING) or ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].ReferenceValueType=bvtSTRING));
    if (not result) and ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].Variable>=0)) then begin
     result:=CodeGeneratorContext.VariableGetType(Code.Variables[CodeGeneratorContext.Registers[r].Variable].Name)=bvtSTRING;
    end;
   end;
   function IsValueObject(const r:integer):boolean;
   begin
    result:=(CodeGeneratorContext.Registers[r].IsWhat=bcgtOBJECT) or ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].ReferenceValueType=bvtOBJECT));
    if (not result) and ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].Variable>=0)) then begin
     result:=CodeGeneratorContext.VariableGetType(Code.Variables[CodeGeneratorContext.Registers[r].Variable].Name)=bvtOBJECT;
    end;
   end;
   function IsValuePrimitive(const r:integer):boolean;
   begin
    result:=((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].ReferenceValueType in [bvtBOOLEAN,bvtNUMBER,bvtSTRING])) or ((CodeGeneratorContext.Registers[r].IsWhat and (bcgtREFERENCE or bcgtOBJECT))=0);
    if (not result) and ((CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].Variable>=0)) then begin
     result:=CodeGeneratorContext.VariableGetType(Code.Variables[CodeGeneratorContext.Registers[r].Variable].Name) in [bvtBOOLEAN,bvtNUMBER,bvtSTRING];
    end;
   end;
   function IsNodeLocalVarReg(Node:TBESENASTNode;var r:integer):boolean;
   begin
    if DoHoldLocalVariablesInRegisters and (assigned(Node) and ((Node is TBESENASTNodeIdentifier) and CodeGeneratorContext.IsVariableInScope(TBESENASTNodeIdentifier(Node).Name))) then begin
     r:=CodeGeneratorContext.VariableGetRegister(TBESENASTNodeIdentifier(Node).Name);
    end else begin
     r:=-1;
    end;
    result:=r>=0;
   end;
   function IsNodeLocalVarRegister(Node:TBESENASTNode):boolean;
   begin
    result:=(DoHoldLocalVariablesInRegisters and (assigned(Node) and ((Node is TBESENASTNodeIdentifier) and CodeGeneratorContext.IsVariableInScope(TBESENASTNodeIdentifier(Node).Name)))) and (CodeGeneratorContext.VariableGetRegister(TBESENASTNodeIdentifier(Node).Name)>0);
   end;
   procedure GenSetC(const r:integer);
   begin
    if IsValueBoolean(r) then begin
     Code.GenOp(bopSETCBOOL,r);
    end else if IsValueNumber(r) then begin
     Code.GenOp(bopSETCNUM,r);
    end else if IsValueString(r) then begin
     Code.GenOp(bopSETCSTR,r);
    end else if IsValueObject(r) then begin
     Code.GenOp(bopSETCOBJ,r);
    end else begin
     if IsLocalVariableReference(r) then begin
      Code.GenOp(bopSETC,r);
     end else if (CodeGeneratorContext.Registers[r].IsWhat=bcgtREFERENCE) and (CodeGeneratorContext.Registers[r].ReferenceValueType in [bvtUNDEFINED,bvtNULL]) then begin
      case CodeGeneratorContext.Registers[r].ReferenceValueType of
       bvtNULL:begin
        Code.GenOp(bopSETCNULL);
       end;
       else begin
        Code.GenOp(bopSETC,r);
       end;
      end;
     end else begin
      Code.GenOp(bopSETC,r);
     end;
    end;
   end;
   procedure GenGetValueEx(const r,DestRegNr:integer);
   begin
    if CodeGeneratorContext.Registers[r].Variable>=0 then begin
     case CodeGeneratorContext.Registers[r].ReferenceValueType of
      bvtBOOLEAN:begin
       Code.GenOp(bopCOPYBOOL,DestRegNr,r);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtBOOLEAN;
      end;
      bvtNUMBER:begin
       Code.GenOp(bopCOPYNUM,DestRegNr,r);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtNUMBER;
      end;
      bvtSTRING:begin
       Code.GenOp(bopCOPYSTR,DestRegNr,r);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtSTRING;
      end;
      bvtOBJECT:begin
       Code.GenOp(bopCOPYOBJ,DestRegNr,r);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtOBJECT;
      end;
      else begin
       Code.GenOp(bopCOPY,DestRegNr,r);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
      end;
     end;
    end else begin
     case CodeGeneratorContext.Registers[r].ReferenceValueType of
      bvtBOOLEAN:begin
       Code.GenOp(bopGETVALUELOCALINDEXBOOL,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtBOOLEAN;
      end;
      bvtNUMBER:begin
       Code.GenOp(bopGETVALUELOCALINDEXNUM,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtNUMBER;
      end;
      bvtSTRING:begin
       Code.GenOp(bopGETVALUELOCALINDEXSTR,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtSTRING;
      end;
      bvtOBJECT:begin
       Code.GenOp(bopGETVALUELOCALINDEXOBJ,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtOBJECT;
      end;
      else begin
       Code.GenOp(bopGETVALUELOCALINDEX,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
      end;
     end;
    end;
   end;
   procedure GenPutValueEx(const r,SrcRegNr:integer;ValueType:TBESENValueType=bvtUNDEFINED);
   begin
    if CodeGeneratorContext.Registers[r].Variable>=0 then begin
     case ValueType of
      bvtBOOLEAN:begin
       Code.GenOp(bopCOPYBOOL,r,SrcRegNr);
      end;
      bvtNUMBER:begin
       Code.GenOp(bopCOPYNUM,r,SrcRegNr);
      end;
      bvtSTRING:begin
       Code.GenOp(bopCOPYSTR,r,SrcRegNr);
      end;
      bvtOBJECT:begin
       Code.GenOp(bopCOPYOBJ,r,SrcRegNr);
      end;
      else begin
       Code.GenOp(bopCOPY,r,SrcRegNr);
      end;
     end;
    end else begin
     case ValueType of
      bvtBOOLEAN:begin
       Code.GenOp(bopPUTVALUELOCALINDEXBOOL,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
      end;
      bvtNUMBER:begin
       Code.GenOp(bopPUTVALUELOCALINDEXNUM,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
      end;
      bvtSTRING:begin
       Code.GenOp(bopPUTVALUELOCALINDEXSTR,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
      end;
      bvtOBJECT:begin
       Code.GenOp(bopPUTVALUELOCALINDEXOBJ,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
      end;
      else begin
       Code.GenOp(bopPUTVALUELOCALINDEX,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
      end;
     end;
    end;
   end;
   procedure GenGetValue(const r,DestRegNr:integer);
   begin
    if (CodeGeneratorContext.Registers[r].IsWhat and bcgtREFERENCE)<>0 then begin
     if CodeGeneratorContext.Registers[r].IsLocal then begin
      if CodeGeneratorContext.Registers[r].Variable>=0 then begin
       if (Code.LastOpcode=bopLREF) and ((Code.ByteCode[Code.LastCodePos+1]=TBESENUINT32(r)) and (Code.ByteCode[Code.LastCodePos+2]=TBESENUINT32(CodeGeneratorContext.Registers[r].LocalIndex))) then begin
        Code.Restore(Code.LastCodePos);
       end;
       case CodeGeneratorContext.Registers[r].ReferenceValueType of
        bvtBOOLEAN:begin
         Code.GenOp(bopCOPYBOOL,DestRegNr,r);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtBOOLEAN;
        end;
        bvtNUMBER:begin
         Code.GenOp(bopCOPYNUM,DestRegNr,r);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtNUMBER;
        end;
        bvtSTRING:begin
         Code.GenOp(bopCOPYSTR,DestRegNr,r);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtSTRING;
        end;
        bvtOBJECT:begin
         Code.GenOp(bopCOPYOBJ,DestRegNr,r);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtOBJECT;
        end;
        else begin
         Code.GenOp(bopCOPY,DestRegNr,r);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
        end;
       end;
      end else if (not DoHasLocalDelete) and CodeGeneratorContext.VariableGetInitialized(CodeGeneratorContext.VariableGetIdent(CodeGeneratorContext.Registers[r].LocalIndex)) then begin
       if (Code.LastOpcode=bopLREF) and ((Code.ByteCode[Code.LastCodePos+1]=TBESENUINT32(r)) and (Code.ByteCode[Code.LastCodePos+2]=TBESENUINT32(CodeGeneratorContext.Registers[r].LocalIndex))) then begin
        Code.Restore(Code.LastCodePos);
       end;
       case CodeGeneratorContext.Registers[r].ReferenceValueType of
        bvtBOOLEAN:begin
         Code.GenOp(bopGETVALUELOCALINDEXBOOL,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtBOOLEAN;
        end;
        bvtNUMBER:begin
         Code.GenOp(bopGETVALUELOCALINDEXNUM,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtNUMBER;
        end;
        bvtSTRING:begin
         Code.GenOp(bopGETVALUELOCALINDEXSTR,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtSTRING;
        end;
        bvtOBJECT:begin
         Code.GenOp(bopGETVALUELOCALINDEXOBJ,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtOBJECT;
        end;
        else begin
         Code.GenOp(bopGETVALUELOCALINDEX,DestRegNr,CodeGeneratorContext.Registers[r].LocalIndex);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
        end;
       end;
      end else begin
       Code.GenOp(bopGETVALUELOCAL,DestRegNr,r);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
      end;
     end else begin
      Code.GenOp(bopGETVALUEREF,DestRegNr,r);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
      CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
     end;
    end else begin
     Code.GenOp(bopGETVALUE,DestRegNr,r);
     CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
     CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
    end;
   end;
   procedure GenPutValue(const r,SrcRegNr:integer;ValueType:TBESENValueType=bvtUNDEFINED);
   begin
    if (CodeGeneratorContext.Registers[r].IsWhat and bcgtREFERENCE)<>0 then begin
     if CodeGeneratorContext.Registers[r].IsLocal then begin
      if CodeGeneratorContext.Registers[r].Variable>=0 then begin
       if (Code.LastOpcode=bopLREF) and ((Code.ByteCode[Code.LastCodePos+1]=TBESENUINT32(r)) and (Code.ByteCode[Code.LastCodePos+2]=TBESENUINT32(CodeGeneratorContext.Registers[r].LocalIndex))) then begin
        Code.Restore(Code.LastCodePos);
       end;
       case ValueType of
        bvtBOOLEAN:begin
         Code.GenOp(bopCOPYBOOL,r,SrcRegNr);
        end;
        bvtNUMBER:begin
         Code.GenOp(bopCOPYNUM,r,SrcRegNr);
        end;
        bvtSTRING:begin
         Code.GenOp(bopCOPYSTR,r,SrcRegNr);
        end;
        bvtOBJECT:begin
         Code.GenOp(bopCOPYOBJ,r,SrcRegNr);
        end;
        else begin
         Code.GenOp(bopCOPY,r,SrcRegNr);
        end;
       end;
      end else if (not DoHasLocalDelete) and (CodeGeneratorContext.VariableGetInitialized(CodeGeneratorContext.VariableGetIdent(CodeGeneratorContext.Registers[r].LocalIndex)) and
                  CodeGeneratorContext.VariableGetMutableOrDeletable(CodeGeneratorContext.VariableGetIdent(CodeGeneratorContext.Registers[r].LocalIndex))) then begin
       if (Code.LastOpcode=bopLREF) and ((Code.ByteCode[Code.LastCodePos+1]=TBESENUINT32(r)) and (Code.ByteCode[Code.LastCodePos+2]=TBESENUINT32(CodeGeneratorContext.Registers[r].LocalIndex))) then begin
        Code.Restore(Code.LastCodePos);
       end;
       case ValueType of
        bvtBOOLEAN:begin
         Code.GenOp(bopPUTVALUELOCALINDEXBOOL,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
        end;
        bvtNUMBER:begin
         Code.GenOp(bopPUTVALUELOCALINDEXNUM,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
        end;
        bvtSTRING:begin
         Code.GenOp(bopPUTVALUELOCALINDEXSTR,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
        end;
        bvtOBJECT:begin
         Code.GenOp(bopPUTVALUELOCALINDEXOBJ,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
        end;
        else begin
         Code.GenOp(bopPUTVALUELOCALINDEX,CodeGeneratorContext.Registers[r].LocalIndex,SrcRegNr);
        end;
       end;
      end else begin
       Code.GenOp(bopPUTVALUELOCAL,r,SrcRegNr);
      end;
     end else begin
      Code.GenOp(bopPUTVALUEREF,r,SrcRegNr);
     end;
    end else begin
     Code.GenOp(bopPUTVALUE,r,SrcRegNr);
    end;
   end;
   procedure AssignSetType(const n:TBESENASTNode;const t:TBESENValueType);
   begin
    if (OptimizeLocals and not HasFunctions) and assigned(CodeGeneratorContext) then begin
     case n.NodeType of
      bntIDENTIFIER:begin
       if CodeGeneratorContext.IsVariableInScope(TBESENASTNodeIdentifier(n).Name) then begin
        CodeGeneratorContext.VariableSetType(TBESENASTNodeIdentifier(n).Name,t);
       end;
      end;
     end;
    end;
   end;
   procedure AssignSetWhatType(const n:TBESENASTNode;const wt:longword);
   begin
    if (OptimizeLocals and not HasFunctions) and assigned(CodeGeneratorContext) then begin
     case wt of
      bcgtBOOLEAN:begin
       AssignSetType(n,bvtBOOLEAN);
      end;
      bcgtNUMBER:begin
       AssignSetType(n,bvtNUMBER);
      end;
      bcgtSTRING:begin
       AssignSetType(n,bvtSTRING);
      end;
      bcgtOBJECT:begin
       AssignSetType(n,bvtOBJECT);
      end;
      else begin
       AssignSetType(n,bvtUNDEFINED);
      end;
     end;
    end;
   end;
   function AssignGetType(const n:TBESENASTNode):TBESENValueType;
   begin
    result:=bvtUNDEFINED;
    if (OptimizeLocals and not HasFunctions) and assigned(CodeGeneratorContext) then begin
     case n.NodeType of
      bntIDENTIFIER:begin
       if CodeGeneratorContext.IsVariableInScope(TBESENASTNodeIdentifier(n).Name) then begin
        result:=CodeGeneratorContext.VariableGetType(TBESENASTNodeIdentifier(n).Name);
       end;
      end;
     end;
    end;
   end;
   procedure AssignOpPre;
   begin
    if DestRegNr<0 then begin
     if IsNodeLocalVarReg(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1) then begin
      DestRegNr:=r1;
     end else begin
      DestRegNr:=CodeGeneratorContext.AllocateRegister;
     end;
    end;
    r1:=-1;
    r2:=-1;
    r3:=-1;
    Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1,true);
    if r1<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsLocalVariableReference(r1) then begin
     if IsValueNumber(r1) then begin
      r3:=r1;
     end else begin
      r3:=CodeGeneratorContext.AllocateRegister;
      Code.GenOp(bopTONUMBER,r3,r1);
     end;
    end else begin
     if IsPrimitive(r1) then begin
      raise EBESENReferenceError.Create('Left hand side must be a reference');
     end;
     r3:=CodeGeneratorContext.AllocateRegister;
     if IsValueNumber(r1) then begin
      GenGetValue(r1,r3);
     end else begin
      r4:=CodeGeneratorContext.AllocateRegister;
      GenGetValue(r1,r4);
      Code.GenOp(bopTONUMBER,r3,r4);
      CodeGeneratorContext.DeallocateRegister(r4);
     end;
    end;
    Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression,r2,true);
    if r2<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r2) or IsLocalVariableReference(r2) then begin
     r5:=r2;
    end else begin
     r5:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r2,r5);
    end;
    if IsValueNumber(r2) then begin
     r4:=r5;
    end else begin
     r4:=CodeGeneratorContext.AllocateRegister;
     Code.GenOp(bopTONUMBER,r4,r5);
     if r2<>r5 then begin
      CodeGeneratorContext.DeallocateRegister(r5);
     end;
    end;
    if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
     if not CodeGeneratorContext.Registers[r1].IsLocal then begin
      Code.GenOp(bopSTRICTCHECKREF,r1);
     end;
    end else begin
     Code.GenOp(bopSTRICTCHECKREF,r1);
    end;
   end;
   procedure AssignOpAddPre;
   begin
    if DestRegNr<0 then begin
     if IsNodeLocalVarReg(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1) then begin
      DestRegNr:=r1;
     end else begin
      DestRegNr:=CodeGeneratorContext.AllocateRegister;
     end;
    end;
    r1:=-1;
    r2:=-1;
    r3:=-1;
    Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1,true);
    if r1<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsLocalVariableReference(r1) then begin
     if IsValuePrimitive(r1) then begin
      r3:=r1;
     end else begin
      r3:=CodeGeneratorContext.AllocateRegister;
      Code.GenOp(bopTOPRIMITIVE,r3,r1);
     end;
    end else begin
     if IsPrimitive(r1) then begin
      raise EBESENReferenceError.Create('Left hand side must be a reference');
     end;
     r3:=CodeGeneratorContext.AllocateRegister;
     if IsValuePrimitive(r1) then begin
      GenGetValue(r1,r3);
     end else begin
      r4:=CodeGeneratorContext.AllocateRegister;
      GenGetValue(r1,r4);
      Code.GenOp(bopTOPRIMITIVE,r3,r4);
      CodeGeneratorContext.DeallocateRegister(r4);
     end;
    end;
    Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression,r2,true);
    if r2<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r2) or IsLocalVariableReference(r2) then begin
     r5:=r2;
    end else begin
     r5:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r2,r5);
    end;
    if IsValuePrimitive(r2) then begin
     r4:=r5;
    end else begin
     r4:=CodeGeneratorContext.AllocateRegister;
     Code.GenOp(bopTOPRIMITIVE,r4,r5);
     if r2<>r5 then begin
      CodeGeneratorContext.DeallocateRegister(r5);
     end;
    end;
    if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
     if not CodeGeneratorContext.Registers[r1].IsLocal then begin
      Code.GenOp(bopSTRICTCHECKREF,r1);
     end;
    end else begin
     Code.GenOp(bopSTRICTCHECKREF,r1);
    end;
   end;
   procedure AssignOpShiftPre;
   begin
    if DestRegNr<0 then begin
     if IsNodeLocalVarReg(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1) then begin
      DestRegNr:=r1;
     end else begin
      DestRegNr:=CodeGeneratorContext.AllocateRegister;
     end;
    end;
    r1:=-1;
    r2:=-1;
    Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1,true);
    if r1<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsLocalVariableReference(r1) then begin
     r3:=r1;
    end else begin
     if IsPrimitive(r1) then begin
      raise EBESENReferenceError.Create('Left hand side must be a reference');
     end;
     r3:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r1,r3);
    end;
    Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression,r2,true);
    if r2<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r2) or IsLocalVariableReference(r2) then begin
     r4:=r2;
    end else begin
     r4:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r2,r4);
    end;
    if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
     if not CodeGeneratorContext.Registers[r1].IsLocal then begin
      Code.GenOp(bopSTRICTCHECKREF,r1);
     end;
    end else begin
     Code.GenOp(bopSTRICTCHECKREF,r1);
    end;
   end;
   procedure AssignOpPost(Number,Str,FromRight:boolean);
   begin
    if IsLocalVariableReference(r1) then begin
     if DestRegNr=r1 then begin
      if Number then begin
       AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtNUMBER);
      end else if Str then begin
       AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtSTRING);
      end else if FromRight then begin
       if IsValueBoolean(r2) then begin
        AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtBOOLEAN);
       end else if IsValueNumber(r2) then begin
        AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtNUMBER);
       end else if IsValueString(r2) then begin
        AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtSTRING);
       end else if IsValueObject(r2) then begin
        AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtOBJECT);
       end else begin
        AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtUNDEFINED);
       end;
      end else begin
       AssignSetType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,bvtUNDEFINED);
      end;
     end else begin
      if Number then begin
       Code.GenOp(bopCOPYNUM,r1,DestRegNr);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
      end else if Str then begin
       Code.GenOp(bopCOPYSTR,r1,DestRegNr);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
      end else if FromRight then begin
       if IsValueBoolean(r2) then begin
        Code.GenOp(bopCOPYBOOL,r1,DestRegNr);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
       end else if IsValueNumber(r2) then begin
        Code.GenOp(bopCOPYNUM,r1,DestRegNr);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       end else if IsValueString(r2) then begin
        Code.GenOp(bopCOPYSTR,r1,DestRegNr);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
       end else if IsValueObject(r2) then begin
        Code.GenOp(bopCOPYOBJ,r1,DestRegNr);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
       end else begin
        Code.GenOp(bopCOPY,r1,DestRegNr);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
       end;
      end else begin
       Code.GenOp(bopCOPY,r1,DestRegNr);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
      end;
      AssignSetWhatType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
     end;
    end else begin
     if Number then begin
      GenPutValue(r1,DestRegNr,bvtNUMBER);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end else if Str then begin
      GenPutValue(r1,DestRegNr,bvtSTRING);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
     end else if FromRight then begin
      if IsValueBoolean(r2) then begin
       GenPutValue(r1,DestRegNr,bvtBOOLEAN);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
      end else if IsValueNumber(r2) then begin
       GenPutValue(r1,DestRegNr,bvtNUMBER);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
      end else if IsValueString(r2) then begin
       GenPutValue(r1,DestRegNr,bvtSTRING);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
      end else if IsValueObject(r2) then begin
       GenPutValue(r1,DestRegNr,bvtOBJECT);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
      end else begin
       GenPutValue(r1,DestRegNr);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
      end;
     end else begin
      GenPutValue(r1,DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
     end;
     AssignSetWhatType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
    end;
    CodeGeneratorContext.DeallocateRegister(r1);
    CodeGeneratorContext.DeallocateRegister(r2);
    CodeGeneratorContext.DeallocateRegister(r3);
    CodeGeneratorContext.DeallocateRegister(r4);
    CodeGeneratorContext.DeallocateRegister(r5);
   end;
   procedure BinaryPre;
   begin
    if DestRegNr<0 then begin
     DestRegNr:=CodeGeneratorContext.AllocateRegister;
    end;
    r1:=-1;
    r2:=-1;
    Visit(TBESENASTNodeBinaryExpression(ToVisit).LeftExpression,r1,true);
    if r1<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r1) or IsLocalVariableReference(r1) then begin
     r3:=r1;
    end else begin
     r3:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r1,r3);
    end;
    Visit(TBESENASTNodeBinaryExpression(ToVisit).RightExpression,r2,true);
    if r2<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r2) or IsLocalVariableReference(r2) then begin
     r4:=r2;
    end else begin
     r4:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r2,r4);
    end;
   end;
   procedure BinaryNumberPre;
   begin
    if DestRegNr<0 then begin
     DestRegNr:=CodeGeneratorContext.AllocateRegister;
    end;
    r1:=-1;
    r2:=-1;
    Visit(TBESENASTNodeBinaryExpression(ToVisit).LeftExpression,r1,true);
    if r1<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r1) or IsLocalVariableReference(r1) then begin
     r3:=r1;
    end else begin
     r3:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r1,r3);
    end;
    Visit(TBESENASTNodeBinaryExpression(ToVisit).RightExpression,r2,true);
    if r2<0 then begin
     BESENThrowCodeGeneratorInvalidRegister;
    end;
    if IsValue(r2) or IsLocalVariableReference(r2) then begin
     r4:=r2;
    end else begin
     r4:=CodeGeneratorContext.AllocateRegister;
     GenGetValue(r2,r4);
    end;
    if IsValueNumber(r1) then begin
     r5:=r3;
    end else begin
     r5:=CodeGeneratorContext.AllocateRegister;
     Code.GenOp(bopTONUMBER,r5,r3);
     if r1<>r3 then begin
      CodeGeneratorContext.DeallocateRegister(r3);
     end;
     r3:=r5;
     r5:=-1;
    end;
    if not IsValueNumber(r2) then begin
     r5:=CodeGeneratorContext.AllocateRegister;
     Code.GenOp(bopTONUMBER,r5,r4);
     if r2<>r4 then begin
      CodeGeneratorContext.DeallocateRegister(r4);
     end;
     r4:=r5;
     r5:=-1;
    end;
    CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
   end;
   procedure BinaryPost;
   begin
    CodeGeneratorContext.DeallocateRegister(r1);
    CodeGeneratorContext.DeallocateRegister(r2);
    CodeGeneratorContext.DeallocateRegister(r3);
    CodeGeneratorContext.DeallocateRegister(r4);
    CodeGeneratorContext.DeallocateRegister(r5);
   end;
   function AreStrictValueTypesEqual(const vtBefore,vtAfter:TBESENValueTypes):boolean;
   var i,j:integer;
   begin
    if length(vtBefore)<>length(vtAfter) then begin
     result:=false;
    end else begin
     result:=true;
     j:=min(length(vtBefore),length(vtAfter));
     for i:=0 to j-1 do begin
      if vtBefore[i]<>vtAfter[i] then begin
       result:=false;
       break;
      end;
     end;
    end;
   end;
   procedure ResetDifferentValueTypes(var vtBefore:TBESENValueTypes;vtAfter:TBESENValueTypes);
   var i,MinLength,MaxLength,BeforeLength,AfterLength:integer;
   begin
    BeforeLength:=length(vtBefore);
    AfterLength:=length(vtAfter);
    MinLength:=min(BeforeLength,AfterLength);
    MaxLength:=max(BeforeLength,AfterLength);
    SetLength(vtBefore,MaxLength);
    for i:=0 to MinLength-1 do begin
     if vtBefore[i]<>vtAfter[i] then begin
      vtBefore[i]:=bvtUNDEFINED;
     end;
    end;
    for i:=MinLength to MaxLength-1 do begin
     vtBefore[i]:=bvtUNDEFINED;
    end;
   end;
   procedure ResetDifferentMultiValueTypes(var vtBefore:TBESENValueTypes;const vtAfter:TBESENValueTypesItems);
   var i:integer;
   begin
    for i:=0 to length(vtAfter)-1 do begin
     ResetDifferentValueTypes(vtBefore,vtAfter[i]);
    end;
   end;
  begin
   SwitchPatches:=nil;
   vta:=nil;
   vtb:=nil;
   vtc:=nil;
   vtd:=nil;
   vte:=nil;
   vtTemp:=nil;
   vtBegin:=nil;
   vtInner:=nil;
   vtEnd:=nil;
   for Counter:=low(RegStates) to high(RegStates) do begin
    RegStates[Counter].Registers:=nil;
    RegStates[Counter].MaxRegisters:=0;
   end;
   Operands:=nil;
   v:=BESENEmptyValue;
   if assigned(ToVisit) then begin
    r1:=-1;
    r2:=-1;
    r3:=-1;
    r4:=-1;
    r5:=-1;
    r6:=-1;
    if ToVisit is TBESENASTNodeStatement then begin
     if TBESENASTNodeStatement(ToVisit).Location.LineNumber>0 then begin
      TBESEN(Instance).LineNumber:=TBESENASTNodeStatement(ToVisit).Location.LineNumber;
     end;
    end;
    case ToVisit.NodeType of
     bntNONE:begin
     end;
     bntEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLiteral(BESENNullValue,DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNULL;
     end;
     bntLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLiteral(BESENNullValue,DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNULL;
     end;
     bntIDENTIFIER:begin
      if DoHoldLocalVariablesInRegisters and OptimizeLocals and (DestRegNr<0) and CodeGeneratorContext.IsVariableInScope(TBESENASTNodeIdentifier(ToVisit).Name) then begin
       p1:=CodeGeneratorContext.VariableGetRegister(TBESENASTNodeIdentifier(ToVisit).Name);
      end else begin
       p1:=-1;
      end;
      if p1>=0 then begin
       DestRegNr:=p1;
       CodeGeneratorContext.Registers[DestRegNr].LocalIndex:=CodeGeneratorContext.VariableID(TBESENASTNodeIdentifier(ToVisit).Name);
       CodeGeneratorContext.Registers[DestRegNr].IsLocal:=true;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=AssignGetType(ToVisit);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtREFERENCE;
      end else begin
       if DestRegNr<0 then begin
        DestRegNr:=CodeGeneratorContext.AllocateRegister;
       end;
       CodeGeneratorContext.Registers[DestRegNr].LocalIndex:=-1;
       CodeGeneratorContext.Registers[DestRegNr].IsLocal:=false;
       CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=bvtUNDEFINED;
       Hash:=BESENHashKey(TBESENASTNodeIdentifier(ToVisit).Name);
       if CodeGeneratorContext.IsVariableInScope(TBESENASTNodeIdentifier(ToVisit).Name) then begin
        if OptimizeLocals then begin
         CodeGeneratorContext.Registers[DestRegNr].LocalIndex:=CodeGeneratorContext.VariableID(TBESENASTNodeIdentifier(ToVisit).Name);
         CodeGeneratorContext.Registers[DestRegNr].IsLocal:=true;
         Code.GenOp(bopLREF,DestRegNr,CodeGeneratorContext.Registers[DestRegNr].LocalIndex);
         CodeGeneratorContext.Registers[DestRegNr].ReferenceValueType:=AssignGetType(ToVisit);
        end else begin
         Code.GenOp(bopVREF,DestRegNr,CodeGeneratorContext.VariableID(TBESENASTNodeIdentifier(ToVisit).Name),Hash,Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
        end;
       end else begin
        hi:=CodeGeneratorContext.LookupHashMap.GetKey(TBESENASTNodeIdentifier(ToVisit).Name);
        if not assigned(hi) then begin
         hi:=CodeGeneratorContext.LookupHashMap.NewKey(TBESENASTNodeIdentifier(ToVisit).Name,true);
         hi^.Value:=Code.LookupNames.Add(TBESENASTNodeIdentifier(ToVisit).Name);
        end;
        Code.GenOp(bopLOOKUP,DestRegNr,hi^.Value,Hash,Code.GenPolymorphicInlineCacheInstruction);
       end;
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtREFERENCE;
      end;
     end;
     bntVARIABLEDECLARATION:begin
      if assigned(TBESENASTNodeVariableDeclaration(ToVisit).Identifier) then begin
       TBESENASTNodeVariableDeclaration(ToVisit).Identifier.IsReached:=true;
      end;
      if assigned(TBESENASTNodeVariableDeclaration(ToVisit).Identifier) and assigned(TBESENASTNodeVariableDeclaration(ToVisit).Expression) then begin
       if DoHoldLocalVariablesInRegisters and CodeGeneratorContext.IsVariableInScope(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name) then begin
        r1:=CodeGeneratorContext.VariableGetRegister(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name);
       end else begin
        r1:=-1;
       end;
       if r1>=0 then begin
        r2:=-1;
        PR:=Code.Here;
        CodeGeneratorContext.GetRegisterStates(RegStates[0]);
        vta:=CodeGeneratorContext.VariableGetTypes;
        Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression,r2,true);
        if r2<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsLocalVariableReference(r2) then begin
         GenGetValue(r2,r1);
         AssignSetWhatType(TBESENASTNodeVariableDeclaration(ToVisit).Identifier,CodeGeneratorContext.Registers[r2].IsWhat);
        end else if IsValue(r2) then begin
         CodeGeneratorContext.DeallocateRegister(r2);
         CodeGeneratorContext.SetRegisterStates(RegStates[0]);
         Code.Restore(PR);
         CodeGeneratorContext.VariableSetTypes(vta);
         Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression,r1,true);
         AssignSetWhatType(TBESENASTNodeVariableDeclaration(ToVisit).Identifier,CodeGeneratorContext.Registers[r1].IsWhat);
        end else begin
         GenGetValue(r2,r1);
         AssignSetWhatType(TBESENASTNodeVariableDeclaration(ToVisit).Identifier,CodeGeneratorContext.Registers[r2].IsWhat);
        end;
        CodeGeneratorContext.Registers[r1].IsWhat:=bcgtREFERENCE;
        if CodeGeneratorContext.VariableGetMutableOrDeletable(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name) then begin
         CodeGeneratorContext.VariableSetFlags(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name,true,true);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);
       end else begin
        r1:=CodeGeneratorContext.AllocateRegister;
        r2:=-1;
        Hash:=BESENHashKey(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name);
        if CodeGeneratorContext.IsVariableInScope(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name) then begin
         if OptimizeLocals then begin
          Code.GenOp(bopLREF,r1,CodeGeneratorContext.VariableID(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name));
         end else begin
          Code.GenOp(bopVREF,r1,CodeGeneratorContext.VariableID(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name),Hash,Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
         end;
        end else begin
         hi:=CodeGeneratorContext.LookupHashMap.GetKey(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name);
         if not assigned(hi) then begin
          hi:=CodeGeneratorContext.LookupHashMap.NewKey(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name,true);
          hi^.Value:=Code.LookupNames.Add(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name);
         end;
         Code.GenOp(bopLOOKUP,r1,hi^.Value,Hash,Code.GenPolymorphicInlineCacheInstruction);
        end;
        Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression,r2,true);
        if r2<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        AssignSetWhatType(TBESENASTNodeVariableDeclaration(ToVisit).Identifier,CodeGeneratorContext.Registers[r2].IsWhat);
        if IsValue(r2) then begin
         r3:=r2;
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r2,r3);
        end;
        if CodeGeneratorContext.IsVariableInScope(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name) then begin
         if OptimizeLocals then begin
          Code.GenOp(bopPUTVALUELOCAL,r1,r3);
         end else begin
          Code.GenOp(bopPUTVALUEREF,r1,r3);
         end;
        end else begin
         Code.GenOp(bopPUTVALUE,r1,r3);
        end;
        if CodeGeneratorContext.VariableGetMutableOrDeletable(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name) then begin
         CodeGeneratorContext.VariableSetFlags(TBESENASTNodeVariableDeclaration(ToVisit).Identifier.Name,true,true);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);
       end;
{     end else if CalleeNeedReturnedValue then begin
       Visit(TBESENASTNodeVariableDeclaration(ToVisit).Identifier,DestRegNr,true);}
      end;
     end;
     bntVARIABLEEXPRESSION:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if DestRegNr<0 then begin
       for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
        r1:=-1;
        Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter],r1,false);
        CodeGeneratorContext.DeallocateRegister(r1);
       end;
      end else begin
       for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
        Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter],DestRegNr,true);
       end;
      end;
     end;
     bntFUNCTIONBODY:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      OldCode:=Code;
      OldCodeGeneratorContext:=CodeGeneratorContext;
      Code:=TBESENCode(TBESENASTNodeFunctionBody(ToVisit).Code);
      CodeGeneratorContext:=TBESENCodeGeneratorContext.Create(Instance);
      CodeGeneratorContext.Code:=Code;
      OldDoHasLocalDelete:=DoHasLocalDelete;
      OldDoIsComplexFunction:=DoIsComplexFunction;
      OldDoHasMaybeDirectEval:=DoHasMaybeDirectEval;
      OldOptimizeLocals:=OptimizeLocals;
      OldHasFunctions:=HasFunctions;
      OldDoHoldLocalVariablesInRegisters:=DoHoldLocalVariablesInRegisters;
      HasFunctions:=false;
      OptimizeLocals:=TBESENASTNodeFunctionBody(ToVisit).EnableLocalsOptimization;
      try
       DoHasLocalDelete:=false;
       DoIsComplexFunction:=not TBESENASTNodeFunctionBody(ToVisit).IsFunction;
       DoHasMaybeDirectEval:=false;
       HasFunctions:=length(TBESENASTNodeFunctionBody(ToVisit).Functions)>0;
       DoHoldLocalVariablesInRegisters:=TBESENASTNodeFunctionBody(ToVisit).IsFunction;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
        CheckNodeForFeatures(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter],DoHasLocalDelete,DoIsComplexFunction,DoHasMaybeDirectEval,HasFunctions,DoHoldLocalVariablesInRegisters);
       end;
       DoHoldLocalVariablesInRegisters:=DoHoldLocalVariablesInRegisters and (OptimizeLocals and not (DoHasLocalDelete or DoIsComplexFunction or DoHasMaybeDirectEval or HasFunctions));
       if TBESENASTNodeFunctionBody(ToVisit).IsStrict then begin
        Code.GenOp(bopSTRICT,1);
       end else begin
        Code.GenOp(bopSTRICT,0);
       end;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Functions)-1 do begin
        r1:=-1;
        Visit(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter],r1,false);
        CodeGeneratorContext.DeallocateRegister(r1);
        if assigned(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]) and (TBESENASTNodeFunctionBody(ToVisit).Functions[Counter] is TBESENASTNodeFunctionDeclaration) then begin
         if assigned(TBESENASTNodeFunctionDeclaration(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]).Container.Literal) and assigned(TBESENASTNodeFunctionDeclaration(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]).Container.Literal.Name) then begin
          CodeGeneratorContext.VariableSetScope(TBESENASTNodeFunctionDeclaration(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]).Container.Literal.Name.Name,true,false,-1);
          CodeGeneratorContext.VariableSetType(TBESENASTNodeFunctionDeclaration(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]).Container.Literal.Name.Name,bvtUNDEFINED);
          CodeGeneratorContext.VariableSetFlags(TBESENASTNodeFunctionDeclaration(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]).Container.Literal.Name.Name,false,false);
         end;
        end;
       end;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Variables)-1 do begin
        CodeGeneratorContext.VariableSetScope(TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].Name,true,TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].IsParameter,TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].ParameterIndex);
        CodeGeneratorContext.VariableSetType(TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].Name,bvtUNDEFINED);
        CodeGeneratorContext.VariableSetFlags(TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].Name,false,not DoHasLocalDelete);
        if DoHoldLocalVariablesInRegisters and not TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].IsParameter then begin
         r1:=CodeGeneratorContext.AllocateRegister;
         CodeGeneratorContext.Registers[r1].Variable:=Counter;
         CodeGeneratorContext.VariableSetRegister(TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].Name,r1);
        end else begin
         CodeGeneratorContext.VariableSetRegister(TBESENASTNodeFunctionBody(ToVisit).Variables[Counter].Name,-1);
        end;
       end;
       for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
        r1:=-1;
        Visit(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter],r1,false);
        CodeGeneratorContext.DeallocateRegister(r1);
       end;
       if DoHoldLocalVariablesInRegisters then begin
        for Counter:=0 to length(CodeGeneratorContext.Registers)-1 do begin
         if CodeGeneratorContext.Registers[Counter].Variable>=0 then begin
          CodeGeneratorContext.Registers[Counter].Variable:=-1;
          r1:=Counter;
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end;
       end;
       if TBESENASTNodeFunctionBody(ToVisit).IsFunction then begin
        Code.GenOp(bopSETCUNDEF);
       end;
       Code.GenOp(bopEND,0);
       Code.HasLocalDelete:=DoHasLocalDelete;
       Code.IsComplexFunction:=DoIsComplexFunction;
       Code.HasMaybeDirectEval:=DoHasMaybeDirectEval;
       Code.HoldLocalVariablesInRegisters:=DoHoldLocalVariablesInRegisters;
       Code.MaxBlock:=CodeGeneratorContext.MaxBlockDepth;
       Code.MaxLoop:=CodeGeneratorContext.MaxLoopDepth;
       Code.MaxParamArgs:=CodeGeneratorContext.MaxParamArgs;
       Code.MaxRegisters:=CodeGeneratorContext.MaxRegisters;
      finally
       BESENFreeAndNil(CodeGeneratorContext);
       Code:=OldCode;
       CodeGeneratorContext:=OldCodeGeneratorContext;
       DoHasLocalDelete:=OldDoHasLocalDelete;
       DoIsComplexFunction:=OldDoIsComplexFunction;
       DoHasMaybeDirectEval:=OldDoHasMaybeDirectEval;
       OptimizeLocals:=OldOptimizeLocals;
       HasFunctions:=OldHasFunctions;
       DoHoldLocalVariablesInRegisters:=OldDoHoldLocalVariablesInRegisters;
      end;
     end;
     bntFUNCTIONLITERAL:begin
      r1:=-1;
      Visit(TBESENASTNodeFunctionLiteral(ToVisit).Body,r1,false);
      CodeGeneratorContext.DeallocateRegister(r1);
      if TBESENCode(TBESENASTNodeFunctionLiteral(ToVisit).Body.Code).ByteCodeLen=0 then begin
       TBESENCode(TBESENASTNodeFunctionLiteral(ToVisit).Body.Code).GenOp(bopEND,0);
      end;
      TBESENCode(TBESENASTNodeFunctionLiteral(ToVisit).Body.Code).Finish;
     end;
     bntSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
     end;
     bntVARIABLESTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if DestRegNr<0 then begin
       for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
        r1:=-1;
        Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter],r1,false);
        CodeGeneratorContext.DeallocateRegister(r1);
       end;
      end else begin
       for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
        Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter],DestRegNr,true);
       end;
      end;
     end;
     bntFUNCTIONDECLARATION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       Visit(TBESENASTNodeFunctionDeclaration(ToVisit).Container.Literal,DestRegNr,true);
      end;
     end;
     bntEXPRESSIONSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      if Code.Body.IsFunction then begin
       PR:=Code.Here;
       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       vta:=CodeGeneratorContext.VariableGetTypes;
       r1:=-1;
       Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r1) then begin
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        CodeGeneratorContext.VariableSetTypes(vta);
        Code.Restore(PR);
        r1:=-1;
        Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression,r1,false);
        CodeGeneratorContext.DeallocateRegister(r1);
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end else begin
       r1:=-1;
       Visit(TBESENASTNodeExpressionStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       if not Code.Body.IsFunction then begin
        GenSetC(r2);
       end;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
     end;
     bntEMPTYSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
     end;
     bntBLOCKSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if DestRegNr<0 then begin
       for Counter:=0 to length(TBESENASTNodeBlockStatement(ToVisit).Statements)-1 do begin
        r1:=-1;
        Visit(TBESENASTNodeBlockStatement(ToVisit).Statements[Counter],r1,false);
        CodeGeneratorContext.DeallocateRegister(r1);
       end;
      end else begin
       for Counter:=0 to length(TBESENASTNodeBlockStatement(ToVisit).Statements)-1 do begin
        Visit(TBESENASTNodeBlockStatement(ToVisit).Statements[Counter],DestRegNr,true);
       end;
      end;
     end;
     bntDEBUGGERSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      Code.GenOp(bopDEBUGGER);
     end;
     bntBREAKSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      Patchables:=CodeGeneratorContext.FindPatchables(TBESENASTNodeBreakStatement(ToVisit).Target,false);
      if assigned(Patchables) then begin
       if Patchables.BlockDepth<CodeGeneratorContext.BlockDepth then begin
        Code.GenOp(bopEND,Patchables.BlockDepth+1);
       end;
       Patchables.AddBreak(Code.GenOp(bopJMP,0)+1,CodeGeneratorContext.VariableGetTypes);
      end;
     end;
     bntCONTINUESTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      Patchables:=CodeGeneratorContext.FindPatchables(TBESENASTNodeContinueStatement(ToVisit).Target,true);
      if assigned(Patchables) then begin
       if Patchables.BlockDepth<CodeGeneratorContext.BlockDepth then begin
        Code.GenOp(bopEND,Patchables.BlockDepth+1);
       end;
       Patchables.AddContinue(Code.GenOp(bopJMP,0)+1,CodeGeneratorContext.VariableGetTypes);
      end;
     end;
     bntDOSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      inc(CodeGeneratorContext.LoopDepth);
      if CodeGeneratorContext.MaxLoopDepth<CodeGeneratorContext.LoopDepth then begin
       CodeGeneratorContext.MaxLoopDepth:=CodeGeneratorContext.LoopDepth;
      end;
      if OptimizeLocals and not HasLabelBreakContinue(TBESENASTNodeDoStatement(ToVisit).Statement) then begin

       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       PR:=Code.Here;

       // Pass one - Collecting value type differences and generating final code if value types are static
       //  *** vta = Before do statement
       //    * vtb = After do statement and before do while expression
       //    * vtc = After do while expression
       begin
        CodeGeneratorContext.PushPatchables(TBESENASTNodeDoStatement(ToVisit).Target,true);

        vta:=CodeGeneratorContext.VariableGetTypes;
        L1:=Code.Here;
        Visit(TBESENASTNodeDoStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));

        vtb:=CodeGeneratorContext.VariableGetTypes;
        L2:=Code.Here;
        Code.GenLocation(ToVisit.Location);
        r1:=-1;
        Visit(TBESENASTNodeDoStatement(ToVisit).Expression,r1,true);
        if r1<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsLocalVariableReference(r1) then begin
         r2:=r1;
        end else if IsValue(r1) then begin
         r2:=r1;
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r2);
        end;
        if IsValueNumber(r1) then begin
         r3:=r2;
         Code.GenOp(bopJNZERO,L1,r3);
        end else begin
         if IsValueBoolean(r1) then begin
          r3:=r2;
         end else begin
          r3:=CodeGeneratorContext.AllocateRegister;
          Code.GenOp(bopTOBOOLEAN,r3,r2);
          if r2<>r3 then begin
           CodeGeneratorContext.DeallocateRegister(r2);
          end;
         end;
         Code.GenOp(bopJZ,L1,r3);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);

        L3:=Code.Here;

        vtc:=CodeGeneratorContext.VariableGetTypes;

        vtInner:=copy(vta,0,length(vta));
        ResetDifferentValueTypes(vtInner,vtb);
        ResetDifferentValueTypes(vtInner,vtc);

        if CodeGeneratorContext.Patchables.CountContinueValueTypesItems>0 then begin
         SetLength(CodeGeneratorContext.Patchables.ContinueValueTypesItems,CodeGeneratorContext.Patchables.CountContinueValueTypesItems);
         ResetDifferentMultiValueTypes(vtInner,CodeGeneratorContext.Patchables.ContinueValueTypesItems);
        end;

        vtEnd:=copy(vtInner,0,length(vtInner));
        if CodeGeneratorContext.Patchables.CountBreakValueTypesItems>0 then begin
         SetLength(CodeGeneratorContext.Patchables.BreakValueTypesItems,CodeGeneratorContext.Patchables.CountBreakValueTypesItems);
         ResetDifferentMultiValueTypes(vtEnd,CodeGeneratorContext.Patchables.BreakValueTypesItems);
        end;

        CodeGeneratorContext.PopPatchables(L2,L3);

        CodeGeneratorContext.VariableSetTypes(vtEnd);
       end;

       // Pass two - Generating final loop code if value types are different
       if not (AreStrictValueTypesEqual(vta,vtb) and AreStrictValueTypesEqual(vta,vtc) and AreStrictValueTypesEqual(vta,vtInner) and AreStrictValueTypesEqual(vtc,vtEnd)) then begin
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(PR);

        CodeGeneratorContext.PushPatchables(TBESENASTNodeDoStatement(ToVisit).Target,true);

        CodeGeneratorContext.VariableSetTypes(vtInner);
        L1:=Code.Here;
        Visit(TBESENASTNodeDoStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));

        CodeGeneratorContext.VariableSetTypes(vtInner);
        L2:=Code.Here;
        Code.GenLocation(ToVisit.Location);
        r1:=-1;
        Visit(TBESENASTNodeDoStatement(ToVisit).Expression,r1,true);
        if r1<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsLocalVariableReference(r1) then begin
         r2:=r1;
        end else if IsValue(r1) then begin
         r2:=r1;
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r2);
        end;
        if IsValueNumber(r1) then begin
         r3:=r2;
         Code.GenOp(bopJNZERO,L1,r3);
        end else begin
         if IsValueBoolean(r1) then begin
          r3:=r2;
         end else begin
          r3:=CodeGeneratorContext.AllocateRegister;
          Code.GenOp(bopTOBOOLEAN,r3,r2);
          if r2<>r3 then begin
           CodeGeneratorContext.DeallocateRegister(r2);
          end;
         end;
         Code.GenOp(bopJZ,L1,r3);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);

        L3:=Code.Here;
        CodeGeneratorContext.PopPatchables(L2,L3);

        CodeGeneratorContext.VariableSetTypes(vtEnd);
       end;
      end else begin
       // It's extremly type instable global code or the loop body contains labelled break/continue, eo regenerate code for an type instable loop
       CodeGeneratorContext.PushPatchables(TBESENASTNodeDoStatement(ToVisit).Target,true);
       L1:=Code.Here;
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
       Visit(TBESENASTNodeDoStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       L2:=Code.Here;
       Code.GenLocation(ToVisit.Location);
       r1:=-1;
       Visit(TBESENASTNodeDoStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else if IsValue(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       if IsValueNumber(r1) then begin
        r3:=r2;
        Code.GenOp(bopJNZERO,L1,r3);
       end else begin
        if IsValueBoolean(r1) then begin
         r3:=r2;
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         Code.GenOp(bopTOBOOLEAN,r3,r2);
         if r2<>r3 then begin
          CodeGeneratorContext.DeallocateRegister(r2);
         end;
        end;
        Code.GenOp(bopJZ,L1,r3);
       end;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);

       L3:=Code.Here;
       CodeGeneratorContext.PopPatchables(L2,L3);
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      end;
      dec(CodeGeneratorContext.LoopDepth);
     end;
     bntWHILESTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      inc(CodeGeneratorContext.LoopDepth);
      if CodeGeneratorContext.MaxLoopDepth<CodeGeneratorContext.LoopDepth then begin
       CodeGeneratorContext.MaxLoopDepth:=CodeGeneratorContext.LoopDepth;
      end;
      if OptimizeLocals and not HasLabelBreakContinue(TBESENASTNodeWhileStatement(ToVisit).Statement) then begin

       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       PR:=Code.Here;

       // Pass one - Collecting value type differences and generating final code if value types are static
       //  *** vta = Before first while expression execution and before while statement
       //    * vtb = Before while expression
       //    * vtc = After while expression
       begin
        Code.GenLocation(ToVisit.Location);

        CodeGeneratorContext.PushPatchables(TBESENASTNodeWhileStatement(ToVisit).Target,true);

        vta:=CodeGeneratorContext.VariableGetTypes;
        P1:=Code.GenOp(bopJMP,0)+1;

        L1:=Code.Here;
        Visit(TBESENASTNodeWhileStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));

        L2:=Code.Here;
        Code.GenLocation(ToVisit.Location);

        vtb:=CodeGeneratorContext.VariableGetTypes;
        Code.GenLabel(P1);
        r1:=-1;
        Visit(TBESENASTNodeWhileStatement(ToVisit).Expression,r1,true);
        if r1<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsLocalVariableReference(r1) then begin
         r2:=r1;
        end else if IsValue(r1) then begin
         r2:=r1;
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r2);
        end;
        if IsValueNumber(r1) then begin
         r3:=r2;
         Code.GenOp(bopJNZERO,L1,r3);
        end else begin
         if IsValueBoolean(r1) then begin
          r3:=r2;
         end else begin
          r3:=CodeGeneratorContext.AllocateRegister;
          Code.GenOp(bopTOBOOLEAN,r3,r2);
          if r2<>r3 then begin
           CodeGeneratorContext.DeallocateRegister(r2);
          end;
         end;
         Code.GenOp(bopJZ,L1,r3);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);

        L3:=Code.Here;

        vtc:=CodeGeneratorContext.VariableGetTypes;

        vtInner:=copy(vta,0,length(vta));
        ResetDifferentValueTypes(vtInner,vtb);
        ResetDifferentValueTypes(vtInner,vtc);

        if CodeGeneratorContext.Patchables.CountContinueValueTypesItems>0 then begin
         SetLength(CodeGeneratorContext.Patchables.ContinueValueTypesItems,CodeGeneratorContext.Patchables.CountContinueValueTypesItems);
         ResetDifferentMultiValueTypes(vtInner,CodeGeneratorContext.Patchables.ContinueValueTypesItems);
        end;

        vtEnd:=copy(vtInner,0,length(vtInner));
        if CodeGeneratorContext.Patchables.CountBreakValueTypesItems>0 then begin
         SetLength(CodeGeneratorContext.Patchables.BreakValueTypesItems,CodeGeneratorContext.Patchables.CountBreakValueTypesItems);
         ResetDifferentMultiValueTypes(vtEnd,CodeGeneratorContext.Patchables.BreakValueTypesItems);
        end;

        CodeGeneratorContext.PopPatchables(L2,L3);

        CodeGeneratorContext.VariableSetTypes(vtEnd);
       end;

       // Pass two - Generating final loop code if value types are different
       if not (AreStrictValueTypesEqual(vta,vtb) and AreStrictValueTypesEqual(vta,vtc) and AreStrictValueTypesEqual(vta,vtInner) and AreStrictValueTypesEqual(vtc,vtEnd)) then begin
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(PR);

        CodeGeneratorContext.VariableSetTypes(vtInner);

        Code.GenLocation(ToVisit.Location);

        CodeGeneratorContext.PushPatchables(TBESENASTNodeWhileStatement(ToVisit).Target,true);

        P1:=Code.GenOp(bopJMP,0)+1;

        L1:=Code.Here;
        CodeGeneratorContext.VariableSetTypes(vtInner);
        Visit(TBESENASTNodeWhileStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));

        L2:=Code.Here;
        Code.GenLocation(ToVisit.Location);

        Code.GenLabel(P1);
        CodeGeneratorContext.VariableSetTypes(vtInner);
        r1:=-1;
        Visit(TBESENASTNodeWhileStatement(ToVisit).Expression,r1,true);
        if r1<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsLocalVariableReference(r1) then begin
         r2:=r1;
        end else if IsValue(r1) then begin
         r2:=r1;
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r2);
        end;
        if IsValueNumber(r1) then begin
         r3:=r2;
         Code.GenOp(bopJNZERO,L1,r3);
        end else begin
         if IsValueBoolean(r1) then begin
          r3:=r2;
         end else begin
          r3:=CodeGeneratorContext.AllocateRegister;
          Code.GenOp(bopTOBOOLEAN,r3,r2);
          if r2<>r3 then begin
           CodeGeneratorContext.DeallocateRegister(r2);
          end;
         end;
         Code.GenOp(bopJZ,L1,r3);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);

        L3:=Code.Here;

        CodeGeneratorContext.VariableSetTypes(vtEnd);

        CodeGeneratorContext.PopPatchables(L2,L3);
       end;
      end else begin
       // It's extremly type instable global code or the loop body contains labelled break/continue, eo regenerate code for an type instable loop
       Code.GenLocation(ToVisit.Location);
       CodeGeneratorContext.PushPatchables(TBESENASTNodeWhileStatement(ToVisit).Target,true);
       P1:=Code.GenOp(bopJMP,0)+1;

       L1:=Code.Here;
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
       Visit(TBESENASTNodeWhileStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));

       Code.GenLabel(P1);
       L2:=Code.Here;
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
       Code.GenLocation(ToVisit.Location);

       r1:=CodeGeneratorContext.AllocateRegister;
       Visit(TBESENASTNodeWhileStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else if IsValue(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       if IsValueNumber(r1) then begin
        r3:=r2;
        Code.GenOp(bopJNZERO,L1,r3);
       end else begin
        if IsValueBoolean(r1) then begin
         r3:=r2;
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         Code.GenOp(bopTOBOOLEAN,r3,r2);
         if r2<>r3 then begin
          CodeGeneratorContext.DeallocateRegister(r2);
         end;
        end;
        Code.GenOp(bopJZ,L1,r3);
       end;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);

       L3:=Code.Here;
       CodeGeneratorContext.PopPatchables(L2,L3);
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      end;
      dec(CodeGeneratorContext.LoopDepth);
     end;
     bntWITHSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeWithStatement(ToVisit).Expression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else if IsValue(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      if IsValueObject(r1) then begin
       r3:=r2;
      end else begin
       r3:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOOBJECT,r3,r2);
       if r2<>r3 then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end;
      if r1<>r3 then begin
       CodeGeneratorContext.DeallocateRegister(r1);
      end;
      if r2<>r3 then begin
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      Code.GenOp(bopSWITH,r3);
      CodeGeneratorContext.DeallocateRegister(r3);
      CodeGeneratorContext.BlockEnter;
      CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      OldVarScope:=CodeGeneratorContext.VariableSetAllScope(false);
      OldOptimizeLocals:=OptimizeLocals;
      OptimizeLocals:=false;
      Visit(TBESENASTNodeWithStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
      OptimizeLocals:=OldOptimizeLocals;
      Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent);
      CodeGeneratorContext.BlockLeave;
      CodeGeneratorContext.VariableSetAllScope(OldVarScope);
      CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
     end;
     bntFORSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      inc(CodeGeneratorContext.LoopDepth);
      if CodeGeneratorContext.MaxLoopDepth<CodeGeneratorContext.LoopDepth then begin
       CodeGeneratorContext.MaxLoopDepth:=CodeGeneratorContext.LoopDepth;
      end;
      if OptimizeLocals and not HasLabelBreakContinue(TBESENASTNodeForStatement(ToVisit).Statement) then begin
       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       PR:=Code.Here;

       // Pass one - Collecting value type differences and generating final code if value types are static
       //  *** vta = Before initial expression
       //    * vtb = After conditional expression and before statement
       //    * vtc = After before statement and before increment expression
       //    * vtd = After increment expression and before conditional expression
       //    * vte = After conditional expression
       begin
        CodeGeneratorContext.PushPatchables(TBESENASTNodeForStatement(ToVisit).Target,true);

        vta:=CodeGeneratorContext.VariableGetTypes;
        if assigned(TBESENASTNodeForStatement(ToVisit).Initial) then begin
         Code.GenLocation(ToVisit.Location);
         PR2:=Code.Here;
         CodeGeneratorContext.GetRegisterStates(RegStates[1]);
         vtTemp:=CodeGeneratorContext.VariableGetTypes;
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,true);
         if (r1>=0) and (TBESENASTNodeForStatement(ToVisit).Initial.NodeType<>bntVARIABLEEXPRESSION) then begin
          if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
           r2:=CodeGeneratorContext.AllocateRegister;
           GenGetValue(r1,r2);
           CodeGeneratorContext.DeallocateRegister(r2);
           CodeGeneratorContext.DeallocateRegister(r1);
          end else begin
           CodeGeneratorContext.DeallocateRegister(r1);
           Code.Restore(PR2);
           CodeGeneratorContext.SetRegisterStates(RegStates[1]);
           CodeGeneratorContext.VariableSetTypes(vtTemp);
           r1:=-1;
           Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,false);
           CodeGeneratorContext.DeallocateRegister(r1);
          end;
         end else begin
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.SetRegisterStates(RegStates[1]);
          Code.Restore(PR2);
          CodeGeneratorContext.VariableSetTypes(vtTemp);
          r1:=-1;
          Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,false);
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end;

        P1:=Code.GenOp(bopJMP,0)+1;

        vtb:=CodeGeneratorContext.VariableGetTypes;
        L1:=Code.Here;
        if assigned(TBESENASTNodeForStatement(ToVisit).Statement) then begin
         Code.GenLocation(ToVisit.Location);
         Visit(TBESENASTNodeForStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        end;

        vtc:=CodeGeneratorContext.VariableGetTypes;
        L2:=Code.Here;
        if assigned(TBESENASTNodeForStatement(ToVisit).Increment) then begin
         Code.GenLocation(ToVisit.Location);
         PR2:=Code.Here;
         vtTemp:=CodeGeneratorContext.VariableGetTypes;
         CodeGeneratorContext.GetRegisterStates(RegStates[1]);
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,true);
         if r1>=0 then begin
          if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
           r2:=CodeGeneratorContext.AllocateRegister;
           GenGetValue(r1,r2);
           CodeGeneratorContext.DeallocateRegister(r2);
           CodeGeneratorContext.DeallocateRegister(r1);
          end else begin
           CodeGeneratorContext.DeallocateRegister(r1);
           CodeGeneratorContext.SetRegisterStates(RegStates[1]);
           Code.Restore(PR2);
           CodeGeneratorContext.VariableSetTypes(vtTemp);
           r1:=-1;
           Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,false);
           CodeGeneratorContext.DeallocateRegister(r1);
          end;
         end else begin
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.SetRegisterStates(RegStates[1]);
          Code.Restore(PR2);
          CodeGeneratorContext.VariableSetTypes(vtTemp);
          r1:=-1;
          Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,false);
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end;

        vtd:=CodeGeneratorContext.VariableGetTypes;
        Code.GenLabel(P1);
        Code.GenLocation(ToVisit.Location);
        if assigned(TBESENASTNodeForStatement(ToVisit).Condition) then begin
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Condition,r1,true);
         if r1>=0 then begin
          if (IsValue(r1) or IsLocalVariableReference(r1)) then begin
           r2:=r1;
          end else begin
           r2:=CodeGeneratorContext.AllocateRegister;
           GenGetValue(r1,r2);
          end;
          if IsValueNumber(r1) then begin
           r3:=r2;
           Code.GenOp(bopJNZERO,L1,r3);
          end else begin
           if IsValueBoolean(r1) then begin
            r3:=r2;
           end else begin
            r3:=CodeGeneratorContext.AllocateRegister;
            Code.GenOp(bopTOBOOLEAN,r3,r2);
            if r2<>r3 then begin
             CodeGeneratorContext.DeallocateRegister(r2);
            end;
           end;
           Code.GenOp(bopJZ,L1,r3);
          end;
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.DeallocateRegister(r2);
          CodeGeneratorContext.DeallocateRegister(r3);
         end else begin
          Code.GenOp(bopJMP,L1);
         end;
        end else begin
         Code.GenOp(bopJMP,L1);
        end;
        L3:=Code.Here;

        vte:=CodeGeneratorContext.VariableGetTypes;

        vtBegin:=copy(vta,0,length(vta));
        
        vtInner:=copy(vtb,0,length(vtb));
        ResetDifferentValueTypes(vtInner,vtc);
        ResetDifferentValueTypes(vtInner,vtd);
        ResetDifferentValueTypes(vtInner,vte);

        if CodeGeneratorContext.Patchables.CountContinueValueTypesItems>0 then begin
         SetLength(CodeGeneratorContext.Patchables.ContinueValueTypesItems,CodeGeneratorContext.Patchables.CountContinueValueTypesItems);
         ResetDifferentMultiValueTypes(vtInner,CodeGeneratorContext.Patchables.ContinueValueTypesItems);
        end;

        vtEnd:=copy(vtInner,0,length(vtInner));
        if CodeGeneratorContext.Patchables.CountBreakValueTypesItems>0 then begin
         SetLength(CodeGeneratorContext.Patchables.BreakValueTypesItems,CodeGeneratorContext.Patchables.CountBreakValueTypesItems);
         ResetDifferentMultiValueTypes(vtEnd,CodeGeneratorContext.Patchables.BreakValueTypesItems);
        end;

        CodeGeneratorContext.PopPatchables(L2,L3);

        CodeGeneratorContext.VariableSetTypes(vtEnd);
       end;

       // Pass two - Generating final loop code if value types are different
       if not (AreStrictValueTypesEqual(vta,vtb) and AreStrictValueTypesEqual(vta,vtc) and AreStrictValueTypesEqual(vta,vtd) and AreStrictValueTypesEqual(vta,vte) and AreStrictValueTypesEqual(vta,vtBegin) and AreStrictValueTypesEqual(vta,vtInner) and AreStrictValueTypesEqual(vtc,vtEnd)) then begin
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(PR);

        CodeGeneratorContext.PushPatchables(TBESENASTNodeForStatement(ToVisit).Target,true);

        CodeGeneratorContext.VariableSetTypes(vtBegin);
        if assigned(TBESENASTNodeForStatement(ToVisit).Initial) then begin
         Code.GenLocation(ToVisit.Location);
         PR2:=Code.Here;
         CodeGeneratorContext.GetRegisterStates(RegStates[1]);
         vtTemp:=CodeGeneratorContext.VariableGetTypes;
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,true);
         if (r1>=0) and (TBESENASTNodeForStatement(ToVisit).Initial.NodeType<>bntVARIABLEEXPRESSION) then begin
          if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
           r2:=CodeGeneratorContext.AllocateRegister;
           GenGetValue(r1,r2);
           CodeGeneratorContext.DeallocateRegister(r2);
           CodeGeneratorContext.DeallocateRegister(r1);
          end else begin
           CodeGeneratorContext.DeallocateRegister(r1);
           CodeGeneratorContext.SetRegisterStates(RegStates[1]);
           Code.Restore(PR2);
           CodeGeneratorContext.VariableSetTypes(vtTemp);
           r1:=-1;
           Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,false);
           CodeGeneratorContext.DeallocateRegister(r1);
          end;
         end else begin
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.SetRegisterStates(RegStates[1]);
          Code.Restore(PR2);
          CodeGeneratorContext.VariableSetTypes(vtTemp);
          r1:=-1;
          Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,false);
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end;

        P1:=Code.GenOp(bopJMP,0)+1;

        CodeGeneratorContext.VariableSetTypes(vtInner);
        L1:=Code.Here;
        if assigned(TBESENASTNodeForStatement(ToVisit).Statement) then begin
         Code.GenLocation(ToVisit.Location);
         Visit(TBESENASTNodeForStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        end;

        CodeGeneratorContext.VariableSetTypes(vtInner);
        L2:=Code.Here;
        if assigned(TBESENASTNodeForStatement(ToVisit).Increment) then begin
         Code.GenLocation(ToVisit.Location);
         PR2:=Code.Here;
         CodeGeneratorContext.GetRegisterStates(RegStates[1]);
         vtTemp:=CodeGeneratorContext.VariableGetTypes;
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,true);
         if r1>=0 then begin
          if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
           r2:=CodeGeneratorContext.AllocateRegister;
           GenGetValue(r1,r2);
           CodeGeneratorContext.DeallocateRegister(r2);
           CodeGeneratorContext.DeallocateRegister(r1);
          end else begin
           CodeGeneratorContext.DeallocateRegister(r1);
           CodeGeneratorContext.SetRegisterStates(RegStates[1]);
           Code.Restore(PR2);
           CodeGeneratorContext.VariableSetTypes(vtTemp);
           r1:=-1;
           Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,false);
           CodeGeneratorContext.DeallocateRegister(r1);
          end;
         end else begin
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.SetRegisterStates(RegStates[1]);
          Code.Restore(PR2);
          CodeGeneratorContext.VariableSetTypes(vtTemp);
          r1:=-1;
          Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,false);
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end;

        CodeGeneratorContext.VariableSetTypes(vtInner);
        Code.GenLabel(P1);
        Code.GenLocation(ToVisit.Location);
        if assigned(TBESENASTNodeForStatement(ToVisit).Condition) then begin
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Condition,r1,true);
         if r1>=0 then begin
          if (IsValue(r1) or IsLocalVariableReference(r1)) then begin
           r2:=r1;
          end else begin
           r2:=CodeGeneratorContext.AllocateRegister;
           GenGetValue(r1,r2);
          end;
          if IsValueNumber(r1) then begin
           r3:=r2;
           Code.GenOp(bopJNZERO,L1,r3);
          end else begin
           if IsValueBoolean(r1) then begin
            r3:=r2;
           end else begin
            r3:=CodeGeneratorContext.AllocateRegister;
            Code.GenOp(bopTOBOOLEAN,r3,r2);
            if r2<>r3 then begin
             CodeGeneratorContext.DeallocateRegister(r2);
            end;
           end;
           Code.GenOp(bopJZ,L1,r3);
          end;
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.DeallocateRegister(r2);
          CodeGeneratorContext.DeallocateRegister(r3);
         end else begin
          Code.GenOp(bopJMP,L1);
         end;
        end else begin
         Code.GenOp(bopJMP,L1);
        end;
        L3:=Code.Here;

        CodeGeneratorContext.VariableSetTypes(vtEnd);

        CodeGeneratorContext.PopPatchables(L2,L3);
       end;
      end else begin
       // It's extremly type instable global code or the loop body contains labelled break/continue, eo regenerate code for an type instable loop
       CodeGeneratorContext.PushPatchables(TBESENASTNodeForStatement(ToVisit).Target,true);

       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);

       if assigned(TBESENASTNodeForStatement(ToVisit).Initial) then begin
        Code.GenLocation(ToVisit.Location);
        PR2:=Code.Here;
        CodeGeneratorContext.GetRegisterStates(RegStates[1]);
        r1:=-1;
        Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,true);
        if (r1>=0) and (TBESENASTNodeForStatement(ToVisit).Initial.NodeType<>bntVARIABLEEXPRESSION) then begin
         if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
          r2:=CodeGeneratorContext.AllocateRegister;
          GenGetValue(r1,r2);
          CodeGeneratorContext.DeallocateRegister(r2);
          CodeGeneratorContext.DeallocateRegister(r1);
         end else begin
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.SetRegisterStates(RegStates[1]);
          Code.Restore(PR2);
          CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
          r1:=-1;
          Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,false);
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end else begin
         CodeGeneratorContext.DeallocateRegister(r1);
         CodeGeneratorContext.SetRegisterStates(RegStates[1]);
         Code.Restore(PR2);
         CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Initial,r1,false);
         CodeGeneratorContext.DeallocateRegister(r1);
        end;
       end;
       P1:=Code.GenOp(bopJMP,0)+1;

       L1:=Code.Here;
       if assigned(TBESENASTNodeForStatement(ToVisit).Statement) then begin
        Code.GenLocation(ToVisit.Location);
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        Visit(TBESENASTNodeForStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       end;

       L2:=Code.Here;
       if assigned(TBESENASTNodeForStatement(ToVisit).Increment) then begin
        Code.GenLocation(ToVisit.Location);
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        PR2:=Code.Here;
        CodeGeneratorContext.GetRegisterStates(RegStates[1]);
        r1:=-1;
        Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,true);
        if r1>=0 then begin
         if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
          r2:=CodeGeneratorContext.AllocateRegister;
          GenGetValue(r1,r2);
          CodeGeneratorContext.DeallocateRegister(r2);
          CodeGeneratorContext.DeallocateRegister(r1);
         end else begin
          CodeGeneratorContext.DeallocateRegister(r1);
          CodeGeneratorContext.SetRegisterStates(RegStates[1]);
          Code.Restore(PR2);
          CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
          r1:=-1;
          Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,false);
          CodeGeneratorContext.DeallocateRegister(r1);
         end;
        end else begin
         CodeGeneratorContext.DeallocateRegister(r1);
         CodeGeneratorContext.SetRegisterStates(RegStates[1]);
         Code.Restore(PR2);
         CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
         r1:=-1;
         Visit(TBESENASTNodeForStatement(ToVisit).Increment,r1,false);
         CodeGeneratorContext.DeallocateRegister(r1);
        end;
       end;

       Code.GenLabel(P1);
       Code.GenLocation(ToVisit.Location);
       if assigned(TBESENASTNodeForStatement(ToVisit).Condition) then begin
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        r1:=-1;
        Visit(TBESENASTNodeForStatement(ToVisit).Condition,r1,true);
        if r1>=0 then begin
         if IsValue(r1) or IsLocalVariableReference(r1) then begin
          r2:=r1;
         end else begin
          r2:=CodeGeneratorContext.AllocateRegister;
          GenGetValue(r1,r2);
         end;
         if IsValueNumber(r1) then begin
          r3:=r2;
          Code.GenOp(bopJNZERO,L1,r3);
         end else begin
          if IsValueBoolean(r1) then begin
           r3:=r2;
          end else begin
           r3:=CodeGeneratorContext.AllocateRegister;
           Code.GenOp(bopTOBOOLEAN,r3,r2);
           if r2<>r3 then begin
            CodeGeneratorContext.DeallocateRegister(r2);
           end;
          end;
          Code.GenOp(bopJZ,L1,r3);
         end;
         CodeGeneratorContext.DeallocateRegister(r1);
         CodeGeneratorContext.DeallocateRegister(r2);
         CodeGeneratorContext.DeallocateRegister(r3);
        end else begin
         Code.GenOp(bopJMP,L1);
        end;
       end else begin
        Code.GenOp(bopJMP,L1);
       end;
       L3:=Code.Here;

       CodeGeneratorContext.PopPatchables(L2,L3);
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      end;
      dec(CodeGeneratorContext.LoopDepth);
     end;
     bntFORINSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      inc(CodeGeneratorContext.LoopDepth);
      if CodeGeneratorContext.MaxLoopDepth<CodeGeneratorContext.LoopDepth then begin
       CodeGeneratorContext.MaxLoopDepth:=CodeGeneratorContext.LoopDepth;
      end;
      Code.GenLocation(ToVisit.Location);
      if TBESENASTNodeForInStatement(ToVisit).Variable.NodeType=bntVARIABLEDECLARATION then begin
       r1:=-1;
       r2:=-1;
       if assigned(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier) then begin
        Visit(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier,r1,true);
       end else begin
        Visit(TBESENASTNodeForInStatement(ToVisit).Variable,r1,true);
       end;
       Visit(TBESENASTNodeForInStatement(ToVisit).Expression,r2,true);
       if r2<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r2) or IsLocalVariableReference(r2) then begin
        r3:=r2;
       end else begin
        r3:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r2,r3);
       end;
       P2:=Code.GenOp(bopJNULL,0,r3)+1;
       if IsValueObject(r2) then begin
        r4:=r3;
       end else begin
        r4:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopTOOBJECT,r4,r3);
        if r3<>r4 then begin
         CodeGeneratorContext.DeallocateRegister(r3);
        end;
       end;
       Code.GenOp(bopSENUM,r4);
       r5:=CodeGeneratorContext.AllocateRegister;
       CodeGeneratorContext.BlockEnter;
       CodeGeneratorContext.PushPatchables(TBESENASTNodeForInStatement(ToVisit).Target,true);
       P1:=Code.GenOp(bopJMP,0)+1;
       L1:=Code.Here;
       r3:=CodeGeneratorContext.AllocateRegister;
       Hash:=BESENHashKey(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name);
       if CodeGeneratorContext.IsVariableInScope(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name) then begin
        if IsLocalVariableReference(r1) then begin
         Code.GenOp(bopCOPY,r1,r5);
        end else if OptimizeLocals then begin
         Code.GenOp(bopLREF,r3,CodeGeneratorContext.VariableID(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name));
         Code.GenOp(bopPUTVALUELOCAL,r3,r5);
        end else begin
         Code.GenOp(bopVREF,r3,CodeGeneratorContext.VariableID(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name),Hash,Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
         Code.GenOp(bopPUTVALUEREF,r3,r5);
        end;
       end else begin
        hi:=CodeGeneratorContext.LookupHashMap.GetKey(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name);
        if not assigned(hi) then begin
         hi:=CodeGeneratorContext.LookupHashMap.NewKey(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name,true);
         hi^.Value:=Code.LookupNames.Add(TBESENASTNodeVariableDeclaration(TBESENASTNodeForInStatement(ToVisit).Variable).Identifier.Name);
        end;
        Code.GenOp(bopLOOKUP,r3,hi^.Value,Hash,Code.GenPolymorphicInlineCacheInstruction);
        Code.GenOp(bopPUTVALUEREF,r3,r5);
       end;
       CodeGeneratorContext.DeallocateRegister(r3);
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
       Visit(TBESENASTNodeForInStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       L2:=Code.Here;
       Code.GenLabel(P1);
       Code.GenOp(bopLOOPENUM,L1,r5);
       L3:=Code.Here;
       Code.GenLabel(P2);
       CodeGeneratorContext.PopPatchables(L2,L3);
       Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent);
       CodeGeneratorContext.BlockLeave;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
       CodeGeneratorContext.DeallocateRegister(r4);
       CodeGeneratorContext.DeallocateRegister(r5);
       CodeGeneratorContext.DeallocateRegister(r6);
      end else begin
       r2:=-1;
       Visit(TBESENASTNodeForInStatement(ToVisit).Expression,r2,true);
       if r2<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r2) or IsLocalVariableReference(r2) then begin
        r3:=r2;
       end else begin
        r3:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r2,r3);
       end;
       P2:=Code.GenOp(bopJNULL,0,r3)+1;
       if IsValueObject(r2) then begin
        r4:=r3;
       end else begin
        r4:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopTOOBJECT,r4,r3);
        if r3<>r4 then begin
         CodeGeneratorContext.DeallocateRegister(r3);
        end;
       end;
       Code.GenOp(bopSENUM,r4);
       r5:=CodeGeneratorContext.AllocateRegister;
       CodeGeneratorContext.BlockEnter;
       CodeGeneratorContext.PushPatchables(TBESENASTNodeForInStatement(ToVisit).Target,true);
       P1:=Code.GenOp(bopJMP,0)+1;
       L1:=Code.Here;
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
       r1:=-1;
       Visit(TBESENASTNodeForInStatement(ToVisit).Variable,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsLocalVariableReference(r1) then begin
        Code.GenOp(bopCOPY,r1,r5);
       end else begin
        Code.GenOp(bopPUTVALUE,r1,r5);
       end;
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
       Visit(TBESENASTNodeForInStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       L2:=Code.Here;
       Code.GenLabel(P1);
       Code.GenOp(bopLOOPENUM,L1,r5);
       L3:=Code.Here;
       Code.GenLabel(P2);
       CodeGeneratorContext.PopPatchables(L2,L3);
       Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent);
       CodeGeneratorContext.BlockLeave;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
       CodeGeneratorContext.DeallocateRegister(r4);
       CodeGeneratorContext.DeallocateRegister(r5);
       CodeGeneratorContext.DeallocateRegister(r6);
      end;
      CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      dec(CodeGeneratorContext.LoopDepth);
     end;
     bntIFSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if OptimizeLocals and not (HasLabelBreakContinue(TBESENASTNodeIfStatement(ToVisit).TrueStatement) or HasLabelBreakContinue(TBESENASTNodeIfStatement(ToVisit).FalseStatement)) then begin
       PR:=Code.Here;

       vta:=CodeGeneratorContext.VariableGetTypes;

       Code.GenLocation(ToVisit.Location);
       r1:=-1;
       Visit(TBESENASTNodeIfStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       if IsValueNumber(r1) then begin
        r3:=r2;
        L1:=Code.GenOp(bopJNZERO,0,r3)+1;
       end else begin
        if IsValueBoolean(r1) then begin
         r3:=r2;
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         Code.GenOp(bopTOBOOLEAN,r3,r2);
         if r2<>r3 then begin
          CodeGeneratorContext.DeallocateRegister(r2);
         end;
        end;
        L1:=Code.GenOp(bopJZ,0,r3)+1;
       end;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);

       vtb:=CodeGeneratorContext.VariableGetTypes;

       Visit(TBESENASTNodeIfStatement(ToVisit).FalseStatement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       if IsBreakContinueReturnNode(TBESENASTNodeIfStatement(ToVisit).FalseStatement) then begin
        vtc:=copy(vtb,0,length(vtb));
       end else begin
        vtc:=CodeGeneratorContext.VariableGetTypes;
       end;
       L2:=Code.GenOp(bopJMP,0)+1;

       Code.GenLabel(L1);
       CodeGeneratorContext.VariableSetTypes(vtb);
       Visit(TBESENASTNodeIfStatement(ToVisit).TrueStatement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       if IsBreakContinueReturnNode(TBESENASTNodeIfStatement(ToVisit).TrueStatement) then begin
        vtd:=copy(vtb,0,length(vtb));
       end else begin
        vtd:=CodeGeneratorContext.VariableGetTypes;
       end;

       Code.GenLabel(L2);

       vte:=copy(vtc,0,length(vtc));
       if not AreStrictValueTypesEqual(vte,vtd) then begin
        ResetDifferentValueTypes(vte,vtd);
       end;

       CodeGeneratorContext.VariableSetTypes(vte);
      end else begin
       // The if-statmente body contains break/continue or it's extremly type instable global code
       Code.GenLocation(ToVisit.Location);
       r1:=-1;
       Visit(TBESENASTNodeIfStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       if IsValueNumber(r1) then begin
        r3:=r2;
        L1:=Code.GenOp(bopJNZERO,0,r3)+1;
       end else begin
        if IsValueBoolean(r1) then begin
         r3:=r2;
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         Code.GenOp(bopTOBOOLEAN,r3,r2);
         if r2<>r3 then begin
          CodeGeneratorContext.DeallocateRegister(r2);
         end;
        end;
        L1:=Code.GenOp(bopJZ,0,r3)+1;
       end;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
       vta:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodeIfStatement(ToVisit).FalseStatement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       L2:=Code.GenOp(bopJMP,0)+1;
       Code.GenLabel(L1);
       CodeGeneratorContext.VariableSetTypes(vta);
       Visit(TBESENASTNodeIfStatement(ToVisit).TrueStatement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
       Code.GenLabel(L2);
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      end;
     end;
     bntLABELLEDSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLocation(ToVisit.Location);
      CodeGeneratorContext.PushPatchables(TBESENASTNodeLabelledStatement(ToVisit).Target,false);
      Visit(TBESENASTNodeLabelledStatement(ToVisit).Statement,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
      L1:=Code.Here;
      CodeGeneratorContext.PopPatchables(-1,L1);
     end;
     bntCASESTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      Code.GenLocation(ToVisit.Location);
      for Counter:=0 to length(TBESENASTNodeCaseStatement(ToVisit).Statements)-1 do begin
       Visit(TBESENASTNodeCaseStatement(ToVisit).Statements[Counter],DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
      end;
     end;
     bntSWITCHSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLocation(ToVisit.Location);
      SetLength(SwitchPatches,length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements));
      r1:=-1;
      Visit(TBESENASTNodeSwitchStatement(ToVisit).Expression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      for Counter:=0 to length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements)-1 do begin
       if assigned(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter].Expression) then begin
        r3:=-1;
        Visit(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter].Expression,r3,true);
        if r3<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsValue(r3) then begin
         r4:=r3;
        end else begin
         r4:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r3,r4);
        end;
        r5:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopSEQ,r5,r2,r4);
        CodeGeneratorContext.DeallocateRegister(r3);
        CodeGeneratorContext.DeallocateRegister(r4);
        SwitchPatches[Counter]:=Code.GenOp(bopJZ,0,r5)+1;
        CodeGeneratorContext.DeallocateRegister(r5);
       end;
      end;
      L1:=Code.GenOp(bopJMP,0)+1;
      CodeGeneratorContext.PushPatchables(TBESENASTNodeSwitchStatement(ToVisit).Target,false);
      DefaultCase:=false;
      for Counter:=0 to length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements)-1 do begin
       if assigned(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter].Expression) then begin
        Code.GenLabel(SwitchPatches[Counter]);
       end else begin
        Code.GenLabel(L1);
        DefaultCase:=true;
       end;
       Visit(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements[Counter],DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
      end;
      if not DefaultCase then begin
       Code.GenLabel(L1);
      end;
      SetLength(SwitchPatches,0);
      CodeGeneratorContext.PopPatchables(-1,Code.Here);
      CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
      CodeGeneratorContext.DeallocateRegister(r4);
      CodeGeneratorContext.DeallocateRegister(r5);
      CodeGeneratorContext.DeallocateRegister(r6);
     end;
     bntTHROWSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      if assigned(TBESENASTNodeThrowStatement(ToVisit).Expression) then begin
       r1:=-1;
       Visit(TBESENASTNodeThrowStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       Code.GenOp(bopTHROW,r2);
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
     end;
     bntTRYSTATEMENT:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLocation(ToVisit.Location);
      if assigned(TBESENASTNodeTryStatement(ToVisit).TryBlock) then begin
       if assigned(TBESENASTNodeTryStatement(ToVisit).CatchBlock) and assigned(TBESENASTNodeTryStatement(ToVisit).FinallyBlock) then begin
        L1:=Code.GenOp(bopSTRYF,0)+1;
        CodeGeneratorContext.BlockEnter;
        r1:=CodeGeneratorContext.AllocateRegister;
        Code.GenLiteral(BESENStringValue(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name),r1);
        L2:=Code.GenOp(bopSTRYC,0,r1)+1;
        CodeGeneratorContext.BlockEnter;
        Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        L3a:=Code.GenOp(bopJMP,0)+1;
        Code.GenLabel(L2);
        InScope:=CodeGeneratorContext.IsVariableInScope(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name);
        if InScope then begin
         CodeGeneratorContext.VariableSetScope(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name,false,false,-1);
        end;
        Code.GenOp(bopSCATCH);
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        Visit(TBESENASTNodeTryStatement(ToVisit).CatchBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        if InScope then begin
         CodeGeneratorContext.VariableSetScope(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name,true,false,-1);
         CodeGeneratorContext.VariableSetType(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name,bvtUNDEFINED);
        end;
        L3b:=Code.GenOp(bopJMP,0)+1;
        CodeGeneratorContext.BlockLeave;
        Code.GenLabel(L1);
        if not Code.Body.IsFunction then begin
         Code.GenOp(bopGETC,r1);
        end;
        if CodeGeneratorContext.MaxBlockDepth>CodeGeneratorContext.BlockCurrent then begin
         Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent+1);
        end;
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        Visit(TBESENASTNodeTryStatement(ToVisit).FinallyBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        if not Code.Body.IsFunction then begin
         GenSetC(r1);
        end;
        Code.GenOp(bopENDF);
        Code.GenLabel(L3a);
        Code.GenLabel(L3b);
        Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent);
        CodeGeneratorContext.BlockLeave;
        CodeGeneratorContext.DeallocateRegister(r1);
       end else if assigned(TBESENASTNodeTryStatement(ToVisit).CatchBlock) then begin
        r1:=CodeGeneratorContext.AllocateRegister;
        Code.GenLiteral(BESENStringValue(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name),r1);
        L1:=Code.GenOp(bopSTRYC,0,r1)+1;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.BlockEnter;
        Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        L2:=Code.GenOp(bopJMP,0)+1;
        Code.GenLabel(L1);
        InScope:=CodeGeneratorContext.IsVariableInScope(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name);
        if InScope then begin
         CodeGeneratorContext.VariableSetScope(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name,false,false,-1);
        end;
        Code.GenOp(bopSCATCH);
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        Visit(TBESENASTNodeTryStatement(ToVisit).CatchBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        if InScope then begin
         CodeGeneratorContext.VariableSetScope(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name,true,false,-1);
         CodeGeneratorContext.VariableSetType(TBESENASTNodeTryStatement(ToVisit).CatchIdentifier.Name,bvtUNDEFINED);
        end;
        Code.GenLabel(L2);
        Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent);
        CodeGeneratorContext.BlockLeave;
       end else if assigned(TBESENASTNodeTryStatement(ToVisit).FinallyBlock) then begin
        L1:=Code.GenOp(bopSTRYF,0)+1;
        CodeGeneratorContext.BlockEnter;
        Visit(TBESENASTNodeTryStatement(ToVisit).TryBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        L2:=Code.GenOp(bopJMP,0)+1;
        Code.GenLabel(L1);
        r1:=CodeGeneratorContext.AllocateRegister;
        if not Code.Body.IsFunction then begin
         Code.GenOp(bopGETC,r1);
        end;
        if CodeGeneratorContext.MaxBlockDepth>CodeGeneratorContext.BlockCurrent then begin
         Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent+1);
        end;
        CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
        Visit(TBESENASTNodeTryStatement(ToVisit).FinallyBlock,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
        if not Code.Body.IsFunction then begin
         GenSetC(r1);
        end;
        Code.GenOp(bopENDF);
        Code.GenLabel(L2);
        Code.GenOp(bopEND,CodeGeneratorContext.BlockCurrent);
        CodeGeneratorContext.BlockLeave;
        CodeGeneratorContext.DeallocateRegister(r1);
       end;
      end;
     end;
     bntARRAYLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=CodeGeneratorContext.AllocateRegister;
      r2:=CodeGeneratorContext.AllocateRegister;
      Code.GenOp(bopARRAY,r1);
      Code.GenOp(bopNEW,DestRegNr,r1,0);
      for Counter:=0 to length(TBESENASTNodeArrayLiteral(ToVisit).Elements)-1 do begin
       if assigned(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]) then begin
        r3:=CodeGeneratorContext.AllocateRegister;
        Code.GenLiteral(BESENStringValue(inttostr(Counter)),r3);
        r4:=-1;
        Visit(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter],r4,true);
        if r4<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsValue(r4) or IsLocalVariableReference(r4) then begin
         r5:=r4;
        end else begin
         r5:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r4,r5);
        end;
        Code.GenOp(bopPUTOBJVALUE,DestRegNr,r3,r5);
        CodeGeneratorContext.DeallocateRegister(r3);
        CodeGeneratorContext.DeallocateRegister(r4);
        CodeGeneratorContext.DeallocateRegister(r5);
       end;
      end;
      r3:=CodeGeneratorContext.AllocateRegister;
      v:=BESENStringValue('length');
      Code.GenLiteral(v,r3);
      r4:=CodeGeneratorContext.AllocateRegister;
      Code.GenOp(bopREF,r4,DestRegNr,r3,TBESEN(Instance).KeyIDManager.LengthID,BESENLengthHash,Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
      r5:=CodeGeneratorContext.AllocateRegister;
      Code.GenLiteral(BESENNumberValue(length(TBESENASTNodeArrayLiteral(ToVisit).Elements)),r5);
      Code.GenOp(bopPUTVALUEREF,r4,r5);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
      CodeGeneratorContext.DeallocateRegister(r4);
      CodeGeneratorContext.DeallocateRegister(r5);
      CodeGeneratorContext.DeallocateRegister(r6);
     end;
     bntBINARYEXPRESSION:begin
     end;
     bntASSIGNMENTEXPRESSION:begin
      if IsNodeLocalVarReg(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,r1) then begin
       r1:=-1;
       Visit(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,r1,true);
       r2:=-1;
       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       P1:=Code.Here;
       vtb:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression,r2,true);
       if r2<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsLocalVariableReference(r2) then begin
        GenGetValue(r2,r1);
        AssignSetWhatType(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[r1].IsWhat);
       end else if IsValue(r2) then begin
        CodeGeneratorContext.DeallocateRegister(r2);
        r2:=r1;
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(P1);
        CodeGeneratorContext.VariableSetTypes(vtb);
        Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression,r2,true);
        AssignSetWhatType(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[r2].IsWhat);
       end else begin
        GenGetValue(r2,r1);
        AssignSetWhatType(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[r1].IsWhat);
       end;
       CodeGeneratorContext.Registers[r1].IsWhat:=bcgtREFERENCE;
       if CalleeNeedReturnedValue or (DestRegNr>=0) then begin
        if DestRegNr<0 then begin
         DestRegNr:=r1;
        end else begin
         DestRegNr:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,DestRegNr);
        end;
       end;
       CodeGeneratorContext.DeallocateRegister(r2);
      end else begin
       r1:=-1;
      end;
      if r1<0 then begin
       if DestRegNr<0 then begin
        DestRegNr:=CodeGeneratorContext.AllocateRegister;
       end;
       vta:=CodeGeneratorContext.VariableGetTypes;
       PR:=Code.Here;
       r1:=-1;
       Visit(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsPrimitive(r1) then begin
        raise EBESENReferenceError.Create('Left hand side must be a reference');
       end;
       r2:=-1;
       P1:=Code.Here;
       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       vtb:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression,r2,true);
       if r2<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsLocalVariableReference(r2) then begin
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
        GenGetValue(r2,DestRegNr);
        if not IsValue(DestRegNr) then begin
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
        end;
       end else if IsValue(r2) then begin
        CodeGeneratorContext.DeallocateRegister(r2);
        Code.Restore(P1);
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        CodeGeneratorContext.VariableSetTypes(vtb);
        Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression,DestRegNr,true);
        r2:=DestRegNr;
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[r2].IsWhat;
       end else begin
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
        GenGetValue(r2,DestRegNr);
        if not IsValue(DestRegNr) then begin
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
        end;
       end;
       AssignSetWhatType(TBESENASTNodeAssignmentExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
       if (not DoHasLocalDelete) and (((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0) and
           CodeGeneratorContext.Registers[r1].IsLocal) and
           ((TBESENASTNodeAssignmentExpression(ToVisit).RightExpression.NodeType=bntIDENTIFIER) and
             (((CodeGeneratorContext.Registers[r2].IsWhat and bcgtREFERENCE)<>0) and
             CodeGeneratorContext.Registers[r2].IsLocal)) then begin
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        r2:=-1;
        Visit(TBESENASTNodeAssignmentExpression(ToVisit).RightExpression,r2,true);
        if r2<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        Code.Restore(PR);
        CodeGeneratorContext.VariableSetTypes(vta);
        GenGetValueEx(r2,DestRegNr);
        if IsValueBoolean(r2) then begin
         GenPutValueEx(r1,DestRegNr,bvtBOOLEAN);
        end else if IsValueNumber(r2) then begin
         GenPutValueEx(r1,DestRegNr,bvtNUMBER);
        end else if IsValueString(r2) then begin
         GenPutValueEx(r1,DestRegNr,bvtSTRING);
        end else if IsValueObject(r2) then begin
         GenPutValueEx(r1,DestRegNr,bvtOBJECT);
        end else begin
         GenPutValueEx(r1,DestRegNr);
        end;
       end else begin
        if ((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)=0) or not CodeGeneratorContext.Registers[r1].IsLocal then begin
         Code.GenOp(bopSTRICTCHECKREF,r1);
        end;
        if IsValueBoolean(r2) then begin
         GenPutValue(r1,DestRegNr,bvtBOOLEAN);
        end else if IsValueNumber(r2) then begin
         GenPutValue(r1,DestRegNr,bvtNUMBER);
        end else if IsValueString(r2) then begin
         GenPutValue(r1,DestRegNr,bvtSTRING);
        end else if IsValueObject(r2) then begin
         GenPutValue(r1,DestRegNr,bvtOBJECT);
        end else begin
         GenPutValue(r1,DestRegNr);
        end;
       end;
       if r1<>DestRegNr then begin
        CodeGeneratorContext.DeallocateRegister(r1);
       end;
       if r2<>DestRegNr then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end;
     end;
     bntASSIGNMENTOPERATOREXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsPrimitive(r1) then begin
       raise EBESENReferenceError.Create('Left hand side must be a reference');
      end;
      r2:=-1;
      Visit(TBESENASTNodeAssignmentOperatorExpression(ToVisit).RightExpression,r2,true);
      if r2<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r2) then begin
       if IsValueBoolean(r2) then begin
        Code.GenOp(bopCOPYBOOL,DestRegNr,r2);
       end else if IsValueNumber(r2) then begin
        Code.GenOp(bopCOPYNUM,DestRegNr,r2);
       end else if IsValueString(r2) then begin
        Code.GenOp(bopCOPYSTR,DestRegNr,r2);
       end else if IsValueObject(r2) then begin
        Code.GenOp(bopCOPYOBJ,DestRegNr,r2);
       end else begin
        Code.GenOp(bopCOPY,DestRegNr,r2);
       end;
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[r2].IsWhat;
      end else begin
       GenGetValue(r2,DestRegNr);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
      end;
      AssignSetWhatType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
      if ((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)=0) or not CodeGeneratorContext.Registers[r1].IsLocal then begin
       Code.GenOp(bopSTRICTCHECKREF);
      end;
      AssignOpPost(false,false,true);
     end;
     bntASSIGNMENTMULTIPLYEXPRESSION:begin
      AssignOpPre;
      Code.GenOp(bopMUL,DestRegNr,r3,r4);
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTDIVIDEEXPRESSION:begin
      AssignOpPre;
      Code.GenOp(bopDIV,DestRegNr,r3,r4);
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTMODULOEXPRESSION:begin
      AssignOpPre;
      Code.GenOp(bopMOD,DestRegNr,r3,r4);
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTPLUSEXPRESSION:begin
      AssignOpAddPre;
      if IsValueNumber(r1) and IsValueNumber(r2) then begin
       Code.GenOp(bopADDNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopADD,DestRegNr,r3,r4);
      end;
      if IsValueString(r1) or IsValueString(r2) then begin
       AssignOpPost(false,true,false);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
      end else if IsValueNumber(r1) and IsValueNumber(r2) then begin
       AssignOpPost(true,false,false);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
      end else begin
       AssignOpPost(false,false,false);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING or bcgtNUMBER;
      end;
      AssignSetWhatType(TBESENASTNodeAssignmentOperatorExpression(ToVisit).LeftExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
     end;
     bntASSIGNMENTMINUSEXPRESSION:begin
      AssignOpPre;
      Code.GenOp(bopSUB,DestRegNr,r3,r4);
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTSHIFTLEFTEXPRESSION:begin
      AssignOpShiftPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopSHLBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopSHLNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopSHL,DestRegNr,r3,r4);
      end;
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTSHIFTRIGHTEXPRESSION:begin
      AssignOpShiftPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopSHRBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopSHRNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopSHR,DestRegNr,r3,r4);
      end;
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      AssignOpShiftPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopSHRBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopUSHRNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopUSHR,DestRegNr,r3,r4);
      end;
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTBITWISEANDEXPRESSION:begin
      AssignOpPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopBANDBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopBANDNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopBAND,DestRegNr,r3,r4);
      end;
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTBITWISEXOREXPRESSION:begin
      AssignOpPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopBXORBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopBXORNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopBXOR,DestRegNr,r3,r4);
      end;
      AssignOpPost(true,false,false);
     end;
     bntASSIGNMENTBITWISEOREXPRESSION:begin
      AssignOpPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopBORBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopBORNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopBOR,DestRegNr,r3,r4);
      end;
      AssignOpPost(true,false,false);
     end;
     bntBINARYOPERATOREXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if not IsValue(r1) then begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      r1:=-1;
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) then begin
       if IsValueBoolean(r1) then begin
        Code.GenOp(bopCOPYBOOL,DestRegNr,r1);
       end else if IsValueNumber(r1) then begin
        Code.GenOp(bopCOPYNUM,DestRegNr,r1);
       end else if IsValueString(r1) then begin
        Code.GenOp(bopCOPYSTR,DestRegNr,r1);
       end else if IsValueObject(r1) then begin
        Code.GenOp(bopCOPYOBJ,DestRegNr,r1);
       end else begin
        Code.GenOp(bopCOPY,DestRegNr,r1);
       end;
      end else begin
       GenGetValue(r1,DestRegNr);
      end;
      if IsValue(r1) then begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[r1].IsWhat;
      end else begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
     end;
     bntBINARYCOMMAEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if not IsValue(r1) then begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      PR:=Code.Here;
      vta:=CodeGeneratorContext.VariableGetTypes;
      CodeGeneratorContext.GetRegisterStates(RegStates[0]);
      r1:=-1;
      Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) then begin
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.SetRegisterStates(RegStates[0]);
       Code.Restore(PR);
       CodeGeneratorContext.VariableSetTypes(vta);
       Visit(TBESENASTNodeBinaryOperatorExpression(ToVisit).RightExpression,DestRegNr,true);
       r1:=DestRegNr;
      end else begin
       GenGetValue(r1,DestRegNr);
       CodeGeneratorContext.DeallocateRegister(r1);
      end;
      if IsValue(r1) then begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[r1].IsWhat;
      end else begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
      end;
     end;
     bntBINARYDIVIDEEXPRESSION:begin
      BinaryNumberPre;
      Code.GenOp(bopDIV,DestRegNr,r3,r4);
      BinaryPost;
     end;
     bntBINARYMODULOEXPRESSION:begin
      BinaryNumberPre;
      Code.GenOp(bopMOD,DestRegNr,r3,r4);
      BinaryPost;
     end;
     bntBINARYMULTIPLYEXPRESSION:begin
      BinaryNumberPre;
      Code.GenOp(bopMUL,DestRegNr,r3,r4);
      BinaryPost;
     end;
     bntBINARYPLUSEXPRESSION:begin
      BinaryPre;
      if IsValuePrimitive(r1) then begin
       r5:=r3;
      end else begin
       r5:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOPRIMITIVE,r5,r3);
      end;
      if IsValuePrimitive(r2) then begin
       r6:=r4;
      end else begin
       r6:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOPRIMITIVE,r6,r4);
      end;
      if IsValueNumber(r1) and IsValueNumber(r2) then begin
       Code.GenOp(bopADDNUM,DestRegNr,r5,r6);
      end else begin
       Code.GenOp(bopADD,DestRegNr,r5,r6);
      end;
      if IsValueString(r1) or IsValueString(r2) then begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
      end else if IsValueNumber(r1) and IsValueNumber(r2) then begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
      end else begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING or bcgtNUMBER;
      end;
      BinaryPost;
     end;
     bntBINARYMINUSEXPRESSION:begin
      BinaryNumberPre;
      Code.GenOp(bopSUB,DestRegNr,r3,r4);
      BinaryPost;
     end;
     bntBINARYSHIFTLEFTEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopSHLBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopSHLNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopSHL,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntBINARYSHIFTRIGHTEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopSHRBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopSHRNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopSHR,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopSHRBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopUSHRNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopUSHR,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntBINARYGREATERTHANEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopGTBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopGTNUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopGTNUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopGTSTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopGT,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYGREATERTHANOREQUALEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopGEBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopGENUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopGENUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopGESTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopGE,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYLESSTHANEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopLTBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopLTNUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopLTNUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopLTSTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopLT,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYLESSTHANOREQUALEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopLEBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopLENUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopLENUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopLESTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopLE,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYINSTANCEOFEXPRESSION:begin
      BinaryPre;
      Code.GenOp(bopINSTANCEOF,DestRegNr,r3,r4);
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYINEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      r2:=-1;
      Visit(TBESENASTNodeBinaryExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) then begin
       r3:=r1;
      end else begin
       r3:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r3);
      end;
      Visit(TBESENASTNodeBinaryExpression(ToVisit).RightExpression,r2,true);
      if r2<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r2) then begin
       r4:=r2;
      end else begin
       r4:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r2,r4);
      end;
      if IsValueString(r1) then begin
       r5:=r3;
      end else begin
       r5:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOSTRING,r5,r3);
      end;
      Code.GenOp(bopIN,DestRegNr,r5,r4);
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYEQUALEQUALEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopEQBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopEQNUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopEQNUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopEQSTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopEQ,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYEQUALEQUALEQUALEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopEQBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopEQNUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopEQNUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopEQSTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopSEQ,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYNOTEQUALEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopNEQBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopNEQNUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopNEQNUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopNEQSTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopNEQ,DestRegNr,r3,r4);
      end;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYNOTEQUALEQUALEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) and IsValueBoolean(r4) then begin
       Code.GenOp(bopNEQBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) and IsValueNumber(r4) then begin
       if Code.LastOpcode=bopLITERALNUM then begin
        v1:=Code.ByteCode[Code.LastCodePos+2];
        Code.Restore(Code.LastCodePos);
        Code.GenOp(bopNEQNUMCONST,DestRegNr,r3,v1);
       end else begin
        Code.GenOp(bopNEQNUM,DestRegNr,r3,r4);
       end;
      end else if IsValueString(r3) and IsValueString(r4) then begin
       Code.GenOp(bopNEQSTR,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopNSEQ,DestRegNr,r3,r4);
      end;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntBINARYBITWISEANDEXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopBANDBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopBANDNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopBAND,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntBINARYBITWISEXOREXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopBXORBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopBXORNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopBXOR,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntBINARYBITWISEOREXPRESSION:begin
      BinaryPre;
      if IsValueBoolean(r3) or IsValueBoolean(r4) then begin
       Code.GenOp(bopBORBOOL,DestRegNr,r3,r4);
      end else if IsValueNumber(r3) or IsValueNumber(r4) then begin
       Code.GenOp(bopBORNUM,DestRegNr,r3,r4);
      end else begin
       Code.GenOp(bopBOR,DestRegNr,r3,r4);
      end;
      BinaryPost;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntBOOLEANLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      v.ValueType:=bvtBOOLEAN;
      v.Bool:=TBESENASTNodeBooleanLiteral(ToVisit).Value;
      Code.GenLiteral(v,DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntCALLEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if CodeGeneratorContext.MaxParamArgs<length(TBESENASTNodeCallExpression(ToVisit).Arguments) then begin
       CodeGeneratorContext.MaxParamArgs:=length(TBESENASTNodeCallExpression(ToVisit).Arguments);
      end;
      SetLength(Operands,4+length(TBESENASTNodeCallExpression(ToVisit).Arguments));
      Operands[0]:=DestRegNr;
      Operands[1]:=r1;
      Operands[2]:=r1;
      Operands[3]:=length(TBESENASTNodeCallExpression(ToVisit).Arguments);
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       Operands[4+Counter]:=CodeGeneratorContext.AllocateRegister;
      end;
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       vta:=CodeGeneratorContext.VariableGetTypes;
       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       pr:=Code.Here;
       r3:=-1;
       Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter],r3,true);
       if r3<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r3) or IsLocalVariableReference(r3) then begin
        if IsLocalVariableReference(r3) then begin
         Operands[4+Counter]:=-1;
        end;
        CodeGeneratorContext.DeallocateRegister(r3);
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(pr);
        CodeGeneratorContext.VariableSetTypes(vta);
        Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter],Operands[4+Counter],true);
       end else begin
        GenGetValue(r3,Operands[4+Counter]);
        CodeGeneratorContext.DeallocateRegister(r3);
       end;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      Operands[2]:=r2;
      if TBESEN(Instance).CodeTracable then begin
       Code.GenOp(bopTRACECALL,Operands);
      end else begin
       Code.GenOp(bopCALL,Operands);
      end;
      if r1<>r2 then begin
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      for Counter:=4 to length(Operands)-1 do begin
       CodeGeneratorContext.DeallocateRegister(Operands[Counter]);
      end;
      SetLength(Operands,0);
      if (OptimizeLocals and HasFunctions) or not OptimizeLocals then begin
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      end;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
     end;
     bntNEWEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if CodeGeneratorContext.MaxParamArgs<length(TBESENASTNodeCallExpression(ToVisit).Arguments) then begin
       CodeGeneratorContext.MaxParamArgs:=length(TBESENASTNodeCallExpression(ToVisit).Arguments);
      end;
      SetLength(Operands,3+length(TBESENASTNodeCallExpression(ToVisit).Arguments));
      Operands[0]:=DestRegNr;
      Operands[1]:=r1;
      Operands[2]:=length(TBESENASTNodeCallExpression(ToVisit).Arguments);
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       Operands[3+Counter]:=CodeGeneratorContext.AllocateRegister;
      end;
      for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
       vta:=CodeGeneratorContext.VariableGetTypes;
       CodeGeneratorContext.GetRegisterStates(RegStates[0]);
       pr:=Code.Here;
       r3:=-1;
       Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter],r3,true);
       if r3<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r3) or IsLocalVariableReference(r3) then begin
        if IsLocalVariableReference(r3) then begin
         Operands[3+Counter]:=-1;
        end;
        CodeGeneratorContext.DeallocateRegister(r3);
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(pr);
        CodeGeneratorContext.VariableSetTypes(vta);
        Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter],Operands[3+Counter],true);
       end else begin
        GenGetValue(r3,Operands[3+Counter]);
        CodeGeneratorContext.DeallocateRegister(r3);
       end;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      Operands[1]:=r2;
      if TBESEN(Instance).CodeTracable then begin
       Code.GenOp(bopTRACENEW,Operands);
      end else begin
       Code.GenOp(bopNEW,Operands);
      end;
      if r1<>r2 then begin
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      for Counter:=3 to length(Operands)-1 do begin
       CodeGeneratorContext.DeallocateRegister(Operands[Counter]);
      end;
      SetLength(Operands,0);
      if (OptimizeLocals and HasFunctions) or not OptimizeLocals then begin
       CodeGeneratorContext.VariableAllSetType(bvtUNDEFINED);
      end;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
     end;
     bntCONDITIONALEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeConditionalExpression(ToVisit).Expression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      if IsValueBoolean(r1) then begin
       r3:=r2;
      end else begin
       r3:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOBOOLEAN,r3,r2);
      end;
      L1:=Code.GenOp(bopJZ,0,r3)+1;
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
      vta:=CodeGeneratorContext.VariableGetTypes;

      PR:=Code.Here;
      r1:=-1;
      Visit(TBESENASTNodeConditionalExpression(ToVisit).FalseExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) then begin
       Code.Restore(PR);
       CodeGeneratorContext.VariableSetTypes(vta);
       Visit(TBESENASTNodeConditionalExpression(ToVisit).FalseExpression,DestRegNr,true);
      end else begin
       GenGetValue(r1,DestRegNr);
      end;
      vtb:=CodeGeneratorContext.VariableGetTypes;

      L2:=Code.GenOp(bopJMP,0)+1;
      Code.GenLabel(L1);

      CodeGeneratorContext.VariableSetTypes(vta);

      PR:=Code.Here;
      r2:=-1;
      Visit(TBESENASTNodeConditionalExpression(ToVisit).TrueExpression,r2,true);
      if r2<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r2) then begin
       Code.Restore(PR);
       CodeGeneratorContext.VariableSetTypes(vta);
       Visit(TBESENASTNodeConditionalExpression(ToVisit).TrueExpression,DestRegNr,true);
      end else begin
       GenGetValue(r2,DestRegNr);
      end;
      vtc:=CodeGeneratorContext.VariableGetTypes;

      vte:=copy(vtb,0,length(vtb));
      if not AreStrictValueTypesEqual(vte,vtc) then begin
       ResetDifferentValueTypes(vte,vtc);
      end;
      CodeGeneratorContext.VariableSetTypes(vte);

      Code.GenLabel(L2);
      if (not IsValue(r1)) or not IsValue(r2) then begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
      end else begin
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[r1].IsWhat or CodeGeneratorContext.Registers[r2].IsWhat;
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
     end;
     bntUNARYEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Visit(TBESENASTNodeUnaryExpression(ToVisit).SubExpression,DestRegNr,true);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[DestRegNr].IsWhat;
     end;
     bntUNARYOPERATOREXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Visit(TBESENASTNodeUnaryOperatorExpression(ToVisit).SubExpression,DestRegNr,true);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=CodeGeneratorContext.Registers[DestRegNr].IsWhat;
     end;
     bntUNARYPLUSEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      CodeGeneratorContext.GetRegisterStates(RegStates[0]);
      PR:=Code.Here;
      vta:=CodeGeneratorContext.VariableGetTypes;
      r1:=-1;
      Visit(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValueNumber(r1) then begin
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.SetRegisterStates(RegStates[0]);
        Code.Restore(PR);
        CodeGeneratorContext.VariableSetTypes(vta);
        Visit(TBESENASTNodeUnaryPlusExpression(ToVisit).SubExpression,DestRegNr,true);
       end else begin
        GenGetValue(r1,DestRegNr);
       end;
      end else begin
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       Code.GenOp(bopTONUMBER,DestRegNr,r2);
       if r1<>r2 then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntUNARYMINUSEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeUnaryMinusExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      if IsValueNumber(r1) then begin
       r3:=r2;
      end else begin
       r3:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTONUMBER,r3,r2);
      end;
      Code.GenOp(bopNEG,DestRegNr,r3);
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntUNARYBITWISENOTEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeUnaryBitwiseNotExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      if IsValueNumber(r1) then begin
       r3:=r2;
      end else begin
       r3:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTONUMBER,r3,r2);
      end;
      Code.GenOp(bopINV,DestRegNr,r3);
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntUNARYLOGICALNOTEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeUnaryLogicalNotExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) or IsLocalVariableReference(r1) then begin
       r2:=r1;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
      end;
      if IsValueBoolean(r1) then begin
       r3:=r2;
      end else begin
       r3:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOBOOLEAN,r3,r2);
      end;
      Code.GenOp(bopNOT,DestRegNr,r3);
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
     end;
     bntUNARYVOIDEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeUnaryVoidExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if not (IsValue(r1) or IsLocalVariableReference(r1)) then begin
       r2:=CodeGeneratorContext.AllocateRegister;
       GenGetValue(r1,r2);
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      CodeGeneratorContext.DeallocateRegister(r1);
      Code.GenLiteral(BESENUndefinedValue,DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtUNDEFINED;
     end;
     bntUNARYTYPEOFEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeUnaryTypeOfExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      Code.GenOp(bopTYPEOF,DestRegNr,r1);
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
     end;
     bntPROPERTYEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodePropertyExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if TBESENASTNodePropertyExpression(ToVisit).Dot then begin
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       r3:=CodeGeneratorContext.AllocateRegister;
       if TBESENASTNodePropertyExpression(ToVisit).RightExpression is TBESENASTNodeStringLiteral then begin
        v:=BESENStringValue(TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Value);
        Code.GenLiteral(v,r3);
       end else if TBESENASTNodePropertyExpression(ToVisit).RightExpression is TBESENASTNodeNumberLiteral then begin
        v:=BESENNumberValue(TBESENASTNodeNumberLiteral(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Value);
        v:=BESENStringValue(TBESEN(Instance).ToStr(v));
        Code.GenLiteral(v,r3);
       end else if TBESENASTNodePropertyExpression(ToVisit).RightExpression is TBESENASTNodeBooleanLiteral then begin
        v.ValueType:=bvtBOOLEAN;
        v.Bool:=TBESENASTNodeBooleanLiteral(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Value;
        v:=BESENStringValue(TBESEN(Instance).ToStr(v));
        Code.GenLiteral(v,r3);
       end else if TBESENASTNodePropertyExpression(ToVisit).RightExpression is TBESENASTNodeNullLiteral then begin
        v:=BESENNullValue;
        v:=BESENStringValue(TBESEN(Instance).ToStr(v));
        Code.GenLiteral(v,r3);
       end else if TBESENASTNodePropertyExpression(ToVisit).RightExpression is TBESENASTNodeIdentifier then begin
        v:=BESENStringValue(TBESENASTNodeIdentifier(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Name);
        Code.GenLiteral(v,r3);
       end;
       if not IsValueObject(r1) then begin
        Code.GenOp(bopCHECKOBJECTCOERCIBLE,r2);
       end;
       Hash:=BESENHashKey(v.Str);
       Code.GenOp(bopREF,DestRegNr,r2,r3,TBESEN(Instance).KeyIDManager.Get(v.Str,Hash),Hash,Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
      end else begin
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
       end;
       r3:=-1;
       Visit(TBESENASTNodePropertyExpression(ToVisit).RightExpression,r3,true);
       if r3<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r3) then begin
        r4:=r3;
       end else begin
        r4:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r3,r4);
       end;
       if not IsValueObject(r1) then begin
        Code.GenOp(bopCHECKOBJECTCOERCIBLE,r2);
       end;
       if IsValueNumber(r3) then begin
        r5:=r4;
        if TBESENASTNodePropertyExpression(ToVisit).RightExpression.NodeType=bntNUMBERLITERAL then begin
         Code.GenOp(bopAREF,DestRegNr,r2,r4,BESENHashKey(TBESEN(Instance).ToStr(BESENNumberValue(TBESENASTNodeNumberLiteral(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Value))));
        end else begin
         Code.GenOp(bopAREF,DestRegNr,r2,r4,0);
        end;
       end else begin
        if IsValueString(r3) then begin
         r5:=r4;
        end else begin
         r5:=CodeGeneratorContext.AllocateRegister;
         Code.GenOp(bopTOSTRING,r5,r4);
        end;
        if TBESENASTNodePropertyExpression(ToVisit).RightExpression.NodeType=bntSTRINGLITERAL then begin
         Code.GenOp(bopREF,DestRegNr,r2,r5,TBESEN(Instance).KeyIDManager.Get(TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Value),BESENHashKey(TBESENASTNodeStringLiteral(TBESENASTNodePropertyExpression(ToVisit).RightExpression).Value),Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
        end else begin
         Code.GenOp(bopREF,DestRegNr,r2,r5,TBESENINT32($fefefefe),0,Code.GenPolymorphicInlineCacheInstruction,-1,-1,-1,-1);
        end;
       end;
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
       CodeGeneratorContext.DeallocateRegister(r4);
       CodeGeneratorContext.DeallocateRegister(r5);
      end;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtREFERENCE;
     end;
     bntLOGICALANDEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;

      vta:=CodeGeneratorContext.VariableGetTypes;
      CodeGeneratorContext.GetRegisterStates(RegStates[0]);
      pr:=Code.Here;
      r1:=-1;
      Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) and not IsLocalVariableReference(r1) then begin
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.VariableSetTypes(vta);
       CodeGeneratorContext.SetRegisterStates(RegStates[0]);
       Code.Restore(pr);
       Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression,DestRegNr,true);
      end else begin
       GenGetValue(r1,DestRegNr);
      end;
      IsWhat1:=CodeGeneratorContext.Registers[DestRegNr].IsWhat;
      if IsValueBoolean(DestRegNr) then begin
       r2:=DestRegNr;
      end else begin
       r2:=CodeGeneratorContext.AllocateRegister;
       Code.GenOp(bopTOBOOLEAN,r2,DestRegNr);
      end;
      L1:=Code.GenOp(bopJZ,0,r2)+1;
      L2:=Code.GenOp(bopJMP,0)+1;
      Code.GenLabel(L1);
      CodeGeneratorContext.DeallocateRegister(r1);
      if r2<>DestRegNr then begin
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      r2:=-1;

      vta:=CodeGeneratorContext.VariableGetTypes;
      CodeGeneratorContext.GetRegisterStates(RegStates[0]);
      pr:=Code.Here;
      r1:=-1;
      Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) and not IsLocalVariableReference(r1) then begin
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.VariableSetTypes(vta);
       CodeGeneratorContext.SetRegisterStates(RegStates[0]);
       Code.Restore(pr);
       Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression,DestRegNr,true);
      end else begin
       GenGetValue(r1,DestRegNr);
      end;
      IsWhat2:=CodeGeneratorContext.Registers[DestRegNr].IsWhat;
      Code.GenLabel(L2);
      CodeGeneratorContext.DeallocateRegister(r1);

      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=IsWhat1 or IsWhat2;

     end;
     bntLOGICALOREXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;

      vta:=CodeGeneratorContext.VariableGetTypes;
      CodeGeneratorContext.GetRegisterStates(RegStates[0]);
      pr:=Code.Here;
      r1:=-1;
      Visit(TBESENASTNodeLogicalOrExpression(ToVisit).LeftExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) and not IsLocalVariableReference(r1) then begin
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.VariableSetTypes(vta);
       CodeGeneratorContext.SetRegisterStates(RegStates[0]);
       Code.Restore(pr);
       Visit(TBESENASTNodeLogicalAndExpression(ToVisit).LeftExpression,DestRegNr,true);
      end else begin
       GenGetValue(r1,DestRegNr);
      end;
      IsWhat1:=CodeGeneratorContext.Registers[DestRegNr].IsWhat;
      if IsValueBoolean(DestRegNr) then begin
       r2:=DestRegNr;
      end else begin
       Code.GenOp(bopTOBOOLEAN,r2,DestRegNr);
      end;
      L1:=Code.GenOp(bopJZ,0,r2)+1;
      CodeGeneratorContext.DeallocateRegister(r1);
      if r2<>DestRegNr then begin
       CodeGeneratorContext.DeallocateRegister(r2);
      end;
      r2:=-1;

      vta:=CodeGeneratorContext.VariableGetTypes;
      CodeGeneratorContext.GetRegisterStates(RegStates[0]);
      pr:=Code.Here;
      r1:=-1;
      Visit(TBESENASTNodeLogicalOrExpression(ToVisit).RightExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if IsValue(r1) and not IsLocalVariableReference(r1) then begin
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.VariableSetTypes(vta);
       CodeGeneratorContext.SetRegisterStates(RegStates[0]);
       Code.Restore(pr);
       Visit(TBESENASTNodeLogicalAndExpression(ToVisit).RightExpression,DestRegNr,true);
      end else begin
       GenGetValue(r1,DestRegNr);
      end;
      IsWhat2:=CodeGeneratorContext.Registers[DestRegNr].IsWhat;
      CodeGeneratorContext.DeallocateRegister(r1);
      Code.GenLabel(L1);

      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=IsWhat1 or IsWhat2;
     end;
     bntDELETEEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=-1;
      Visit(TBESENASTNodeDeleteExpression(ToVisit).SubExpression,r1,true);
      if r1<0 then begin
       BESENThrowCodeGeneratorInvalidRegister;
      end;
      if TBESENASTNodeDeleteExpression(ToVisit).SubExpression.NodeType=bntIDENTIFIER then begin
       CodeGeneratorContext.VariableSetFlags(TBESENASTNodeIdentifier(TBESENASTNodeDeleteExpression(ToVisit).SubExpression).Name,false,false);
      end;
      Code.GenOp(bopDELETE,DestRegNr,r1);
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtBOOLEAN;
      AssignSetType(TBESENASTNodeDeleteExpression(ToVisit).SubExpression,bvtUNDEFINED);
     end;
     bntPOSTFIXINCEXPRESSION:begin
      if IsNodeLocalVarReg(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression,r1) then begin
       r1:=-1;
       Visit(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression,r1,true);
       if IsValueNumber(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopTONUMBER,r2,r1);
       end;
       if (CalleeNeedReturnedValue and (DestRegNr<0)) or (DestRegNr>=0) then begin
        if DestRegNr<0 then begin
         DestRegNr:=CodeGeneratorContext.AllocateRegister;
        end;
        Code.GenOp(bopCOPYNUM,DestRegNr,r2);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       end;
       Code.GenOp(bopINC,r1,r2);
       AssignSetType(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression,bvtNUMBER);
       if r1<>r2 then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end else begin
       if DestRegNr<0 then begin
        DestRegNr:=CodeGeneratorContext.AllocateRegister;
       end;
       PR:=Code.Here;
       r1:=-1;
       vta:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if (not DoHasLocalDelete) and ((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0) and
           CodeGeneratorContext.Registers[r1].IsLocal then begin
        Code.Restore(PR);
        CodeGeneratorContext.VariableSetTypes(vta);
        if IsValueNumber(r1) then begin
         GenGetValueEx(r1,DestRegNr);
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValueEx(r1,r2);
         Code.GenOp(bopTONUMBER,DestRegNr,r2);
        end;
        r3:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopINC,r3,DestRegNr);
        GenPutValueEx(r1,r3,bvtNUMBER);
       end else begin
        if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
         if not CodeGeneratorContext.Registers[r1].IsLocal then begin
          Code.GenOp(bopSTRICTCHECKREF,r1);
         end;
        end else begin
         Code.GenOp(bopSTRICTCHECKREF,r1);
        end;
        if IsValueNumber(r1) then begin
         GenGetValue(r1,DestRegNr);
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r2);
         Code.GenOp(bopTONUMBER,DestRegNr,r2);
        end;
        r3:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopINC,r3,DestRegNr);
        GenPutValue(r1,r3,bvtNUMBER);
       end;
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       AssignSetWhatType(TBESENASTNodePostfixIncExpression(ToVisit).SubExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
      end;
     end;
     bntPOSTFIXDECEXPRESSION:begin
      if IsNodeLocalVarReg(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression,r1) then begin
       r1:=-1;
       Visit(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression,r1,true);
       if IsValueNumber(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopTONUMBER,r2,r1);
       end;
       if (CalleeNeedReturnedValue and (DestRegNr<0)) or (DestRegNr>=0) then begin
        if DestRegNr<0 then begin
         DestRegNr:=CodeGeneratorContext.AllocateRegister;
        end;
        Code.GenOp(bopCOPYNUM,DestRegNr,r2);
        CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       end;
       Code.GenOp(bopDEC,r1,r2);
       AssignSetType(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression,bvtNUMBER);
       if r1<>r2 then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end else begin
       if DestRegNr<0 then begin
        DestRegNr:=CodeGeneratorContext.AllocateRegister;
       end;
       PR:=Code.Here;
       r1:=-1;
       vta:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if (not DoHasLocalDelete) and ((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0) and
           CodeGeneratorContext.Registers[r1].IsLocal then begin
        Code.Restore(PR);
        CodeGeneratorContext.VariableSetTypes(vta);
        if IsValueNumber(r1) then begin
         GenGetValueEx(r1,DestRegNr);
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValueEx(r1,r2);
         Code.GenOp(bopTONUMBER,DestRegNr,r2);
        end;
        r3:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopDEC,r3,DestRegNr);
        GenPutValueEx(r1,r3,bvtNUMBER);
       end else begin
        if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
         if not CodeGeneratorContext.Registers[r1].IsLocal then begin
          Code.GenOp(bopSTRICTCHECKREF,r1);
         end;
        end else begin
         Code.GenOp(bopSTRICTCHECKREF,r1);
        end;
        if IsValueNumber(r1) then begin
         GenGetValue(r1,DestRegNr);
        end else begin
         r2:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r2);
         Code.GenOp(bopTONUMBER,DestRegNr,r2);
        end;
        r3:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopDEC,r3,DestRegNr);
        GenPutValue(r1,r3,bvtNUMBER);
       end;
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       AssignSetWhatType(TBESENASTNodePostfixDecExpression(ToVisit).SubExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
      end;
     end;
     bntPREFIXINCEXPRESSION:begin
      if IsNodeLocalVarReg(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression,r1) then begin
       r1:=-1;
       Visit(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression,r1,true);
       if IsValueNumber(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopTONUMBER,r2,r1);
       end;
       Code.GenOp(bopINC,r1,r2);
       if (CalleeNeedReturnedValue and (DestRegNr<0)) or (DestRegNr>=0) then begin
        if DestRegNr<0 then begin
         DestRegNr:=r1;
        end else begin
         Code.GenOp(bopCOPYNUM,DestRegNr,r1);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
        end;
       end;
       AssignSetType(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression,bvtNUMBER);
       if r1<>r2 then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end else begin
       if DestRegNr<0 then begin
        DestRegNr:=CodeGeneratorContext.AllocateRegister;
       end;
       PR:=Code.Here;
       r1:=-1;
       vta:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if (not DoHasLocalDelete) and ((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0) and
           CodeGeneratorContext.Registers[r1].IsLocal then begin
        Code.Restore(PR);
        CodeGeneratorContext.VariableSetTypes(vta);
        r2:=CodeGeneratorContext.AllocateRegister;
        if IsValueNumber(r1) then begin
         GenGetValueEx(r1,r2);
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         GenGetValueEx(r1,r3);
         Code.GenOp(bopTONUMBER,r2,r3);
         CodeGeneratorContext.DeallocateRegister(r3);
        end;
        Code.GenOp(bopINC,DestRegNr,r2);
        GenPutValueEx(r1,DestRegNr,bvtNUMBER);
       end else begin
        if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
         if not CodeGeneratorContext.Registers[r1].IsLocal then begin
          Code.GenOp(bopSTRICTCHECKREF,r1);
         end;
        end else begin
         Code.GenOp(bopSTRICTCHECKREF,r1);
        end;
        r2:=CodeGeneratorContext.AllocateRegister;
        if IsValueNumber(r1) then begin
         GenGetValue(r1,r2);
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r3);
         Code.GenOp(bopTONUMBER,r2,r3);
         CodeGeneratorContext.DeallocateRegister(r3);
        end;
        Code.GenOp(bopINC,DestRegNr,r2);
        GenPutValue(r1,DestRegNr,bvtNUMBER);
       end;
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       AssignSetWhatType(TBESENASTNodePrefixIncExpression(ToVisit).SubExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
      end;
     end;
     bntPREFIXDECEXPRESSION:begin
      if IsNodeLocalVarReg(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression,r1) then begin
       r1:=-1;
       Visit(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression,r1,true);
       if IsValueNumber(r1) then begin
        r2:=r1;
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        Code.GenOp(bopTONUMBER,r2,r1);
       end;
       Code.GenOp(bopDEC,r1,r2);
       if (CalleeNeedReturnedValue and (DestRegNr<0)) or (DestRegNr>=0) then begin
        if DestRegNr<0 then begin
         DestRegNr:=r1;
        end else begin
         Code.GenOp(bopCOPYNUM,DestRegNr,r1);
         CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
        end;
       end;
       AssignSetType(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression,bvtNUMBER);
       if r1<>r2 then begin
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
      end else begin
       if DestRegNr<0 then begin
        DestRegNr:=CodeGeneratorContext.AllocateRegister;
       end;
       PR:=Code.Here;
       r1:=-1;
       vta:=CodeGeneratorContext.VariableGetTypes;
       Visit(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if (not DoHasLocalDelete) and ((CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0) and
           CodeGeneratorContext.Registers[r1].IsLocal then begin
        Code.Restore(PR);
        CodeGeneratorContext.VariableSetTypes(vta);
        r2:=CodeGeneratorContext.AllocateRegister;
        if IsValueNumber(r1) then begin
         GenGetValueEx(r1,r2);
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         GenGetValueEx(r1,r3);
         Code.GenOp(bopTONUMBER,r2,r3);
         CodeGeneratorContext.DeallocateRegister(r3);
        end;
        Code.GenOp(bopDEC,DestRegNr,r2);
        GenPutValueEx(r1,DestRegNr,bvtNUMBER);
       end else begin
        if (CodeGeneratorContext.Registers[r1].IsWhat and bcgtREFERENCE)<>0 then begin
         if not CodeGeneratorContext.Registers[r1].IsLocal then begin
          Code.GenOp(bopSTRICTCHECKREF,r1);
         end;
        end else begin
         Code.GenOp(bopSTRICTCHECKREF,r1);
        end;
        r2:=CodeGeneratorContext.AllocateRegister;
        if IsValueNumber(r1) then begin
         GenGetValue(r1,r2);
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r1,r3);
         Code.GenOp(bopTONUMBER,r2,r3);
         CodeGeneratorContext.DeallocateRegister(r3);
        end;
        Code.GenOp(bopDEC,DestRegNr,r2);
        GenPutValue(r1,DestRegNr,bvtNUMBER);
       end;
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
       AssignSetWhatType(TBESENASTNodePrefixDecExpression(ToVisit).SubExpression,CodeGeneratorContext.Registers[DestRegNr].IsWhat);
       CodeGeneratorContext.DeallocateRegister(r1);
       CodeGeneratorContext.DeallocateRegister(r2);
       CodeGeneratorContext.DeallocateRegister(r3);
      end;
     end;
     bntNULLLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLiteral(BESENNullValue,DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNULL;
     end;
     bntNUMBERLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLiteral(BESENNumberValue(TBESENASTNodeNumberLiteral(ToVisit).Value),DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtNUMBER;
     end;
     bntREGEXPLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if CodeGeneratorContext.MaxParamArgs<2 then begin
       CodeGeneratorContext.MaxParamArgs:=2;
      end;
      r1:=CodeGeneratorContext.AllocateRegister;
      r2:=CodeGeneratorContext.AllocateRegister;
      r3:=CodeGeneratorContext.AllocateRegister;
      Code.GenOp(bopREGEXP,r1);
      Code.GenLiteral(BESENStringValue(TBESENASTNodeRegExpLiteral(ToVisit).Source),r2);
      Code.GenLiteral(BESENStringValue(TBESENASTNodeRegExpLiteral(ToVisit).Flags),r3);
      Code.GenOp(bopNEW,DestRegNr,r1,2,r2,r3);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
      CodeGeneratorContext.DeallocateRegister(r1);
      CodeGeneratorContext.DeallocateRegister(r2);
      CodeGeneratorContext.DeallocateRegister(r3);
     end;
     bntSTRINGLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLiteral(BESENStringValue(TBESENASTNodeStringLiteral(ToVisit).Value),DestRegNr);
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtSTRING;
     end;
     bntTHISLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenOp(bopTHIS,DestRegNr);                             
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtVALUE;
     end;
     bntOBJECTLITERALPROPERTY:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      case TBESENASTNodeObjectLiteralProperty(ToVisit).PropertyType of
       banolptACCESSOR:begin
        if assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container) then begin
         r1:=CodeGeneratorContext.AllocateRegister;
         Code.GenLiteral(BESENStringValue(TBESENASTNodeObjectLiteralProperty(ToVisit).Name),r1);
         r2:=-1;
         Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal,r2,true);
         if r2<0 then begin
          BESENThrowCodeGeneratorInvalidRegister;
         end;
         CodeGeneratorContext.DeallocateRegister(r2);
         r2:=CodeGeneratorContext.AllocateRegister;
         Code.GenFunction(TBESENASTNodeObjectLiteralProperty(ToVisit).Container,r2);
         case TBESENASTNodeObjectLiteralProperty(ToVisit).PropertyAccessorType of
          banolpatGET:begin
           Code.GenOp(bopPUTOBJGET,DestRegNr,r1,r2);
          end;
          banolpatSET:begin
           Code.GenOp(bopPUTOBJSET,DestRegNr,r1,r2);
          end;
          else begin
           Code.GenOp(bopPUTOBJVALUE,DestRegNr,r1,r2);
          end;
         end;
         CodeGeneratorContext.DeallocateRegister(r1);
         CodeGeneratorContext.DeallocateRegister(r2);
        end;
       end;
       banolptDATA:begin
        r1:=CodeGeneratorContext.AllocateRegister;
        Code.GenLiteral(BESENStringValue(TBESENASTNodeObjectLiteralProperty(ToVisit).Name),r1);
        r2:=-1;
        Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Value,r2,true);
        if r2<0 then begin
         BESENThrowCodeGeneratorInvalidRegister;
        end;
        if IsValue(r2) then begin
         Code.GenOp(bopPUTOBJVALUE,DestRegNr,r1,r2);
        end else begin
         r3:=CodeGeneratorContext.AllocateRegister;
         GenGetValue(r2,r3);
         Code.GenOp(bopPUTOBJVALUE,DestRegNr,r1,r3);
        end;
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
        CodeGeneratorContext.DeallocateRegister(r3);
       end;
      end;
     end;
     bntOBJECTLITERAL:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      r1:=CodeGeneratorContext.AllocateRegister;
      Code.GenOp(bopOBJECT,r1);
      Code.GenOp(bopNEW,DestRegNr,r1,0);
      CodeGeneratorContext.DeallocateRegister(r1);
      for Counter:=0 to length(TBESENASTNodeObjectLiteral(ToVisit).Properties)-1 do begin
       Visit(TBESENASTNodeObjectLiteral(ToVisit).Properties[Counter],DestRegNr,true);
      end;
      CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
     end;
     bntRETURNSTATEMENT:begin
      Code.GenLocation(ToVisit.Location);
      if assigned(TBESENASTNodeReturnStatement(ToVisit).Expression) then begin
       r1:=-1;
       Visit(TBESENASTNodeReturnStatement(ToVisit).Expression,r1,true);
       if r1<0 then begin
        BESENThrowCodeGeneratorInvalidRegister;
       end;
       if IsValue(r1) or IsLocalVariableReference(r1) then begin
        GenSetC(r1);
        CodeGeneratorContext.DeallocateRegister(r1);
       end else begin
        r2:=CodeGeneratorContext.AllocateRegister;
        GenGetValue(r1,r2);
        GenSetC(r2);
        CodeGeneratorContext.DeallocateRegister(r1);
        CodeGeneratorContext.DeallocateRegister(r2);
       end;
       Code.GenOp(bopEND,0);
      end else begin
       Code.GenOp(bopSETCUNDEF);
       Code.GenOp(bopEND,0);
      end;
     end;
     bntPROGRAM:begin
      if CalleeNeedReturnedValue and (DestRegNr<0) then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      Code.GenLocation(ToVisit.Location);
      OldOptimizeLocals:=OptimizeLocals;
      OldDoHoldLocalVariablesInRegisters:=DoHoldLocalVariablesInRegisters;
      OptimizeLocals:=false;
      DoHoldLocalVariablesInRegisters:=false;
      Visit(TBESENASTNodeProgram(ToVisit).Body,DestRegNr,CalleeNeedReturnedValue or (DestRegNr>=0));
      OptimizeLocals:=OldOptimizeLocals;
      DoHoldLocalVariablesInRegisters:=OldDoHoldLocalVariablesInRegisters;
      if TBESENCode(TBESENASTNodeProgram(ToVisit).Body.Code).ByteCodeLen=0 then begin
       TBESENCode(TBESENASTNodeProgram(ToVisit).Body.Code).GenOp(bopEND,0);
      end;
      TBESENCode(TBESENASTNodeProgram(ToVisit).Body.Code).Finish;
     end;
     bntFUNCTIONEXPRESSION:begin
      if DestRegNr<0 then begin
       DestRegNr:=CodeGeneratorContext.AllocateRegister;
      end;
      if assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container) then begin
       r1:=-1;
       Visit(TBESENASTNodeFunctionExpression(ToVisit).Container.Literal,r1,false);
       CodeGeneratorContext.DeallocateRegister(r1);
       Code.GenFunction(TBESENASTNodeFunctionExpression(ToVisit).Container,DestRegNr);
       CodeGeneratorContext.Registers[DestRegNr].IsWhat:=bcgtOBJECT;
      end;
     end;
    end;
   end;
   if CalleeNeedReturnedValue and (DestRegNr<0) then begin
    DestRegNr:=CodeGeneratorContext.AllocateRegister;
    Code.GenOp(bopLITERALUNDEF,DestRegNr);
   end;
   for Counter:=low(RegStates) to high(RegStates) do begin
    SetLength(RegStates[Counter].Registers,0);
   end;
   SetLength(vta,0);
   SetLength(vtb,0);
   SetLength(vtc,0);
   SetLength(vtd,0);
   SetLength(vte,0);
   SetLength(vtTemp,0);
   SetLength(vtBegin,0);
   SetLength(vtInner,0);
   SetLength(vtEnd,0);
   SetLength(Operands,0);
  end;
 var tr:integer;
 begin
  Code:=TBESENCode.Create(Instance);
  try
   CodeGeneratorContext:=TBESENCodeGeneratorContext.Create(Instance);
   try
    CodeGeneratorContext.Code:=Code;
    DoHasLocalDelete:=false;
    DoIsComplexFunction:=false;
    DoHasMaybeDirectEval:=false;
    OptimizeLocals:=false;
    HasFunctions:=false;
    DoHoldLocalVariablesInRegisters:=false;
    tr:=-1;
    Visit(RootNode,tr,false);
   finally
    CodeGeneratorContext.Free;
   end;
  finally
   Code.Free;
  end;
 end;
var Parser:TBESENParser;
    RootNode:TBESENASTNode;
{$ifndef BESENNoFPU}
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
{$endif}
begin
 result:=nil;
 Parser:=nil;
 RootNode:=nil;
{$ifndef BESENNoFPU}
 OldFPUExceptionMask:=GetExceptionMask;
 OldFPURoundingMode:=GetRoundMode;
 OldFPUPrecisionMode:=GetPrecisionMode;
{$endif}
 try
{$ifndef BESENNoFPU}
  if OldFPUExceptionMask<>BESENFPUExceptionMask then begin
   SetExceptionMask(BESENFPUExceptionMask);
  end;
  if OldFPURoundingMode<>rmNearest then begin
   SetRoundMode(rmNearest);
  end;
  if OldFPUPrecisionMode<>BESENFPUPrecisionMode then begin
   SetPrecisionMode(BESENFPUPrecisionMode);
  end;
{$endif}
  try
   // Parse the code
   Parser:=TBESENParser.Create(Instance);
   if IsFunction then begin
    Parser.Lexer.Source:='function('+Parameters+'){'+InputSource+'}';
   end else begin
    Parser.Lexer.Source:=InputSource;
   end;
   Parser.Init;
   RootNode:=Parser.Parse(IsFunction,IsJSON);
   BESENFreeAndNil(Parser);

   // Scan, collect variables/functions and preprocess the abstract syntax tree
   ScanCollectAndPreprocess(RootNode);

   // Optimize the abstract syntax tree
   RootNode:=Optimize(RootNode);

   // Generate the byte code
   GenerateByteCode(RootNode);

   if assigned(RootNode) then begin
    result:=RootNode;
   end;
  except
   BESENFreeAndNil(RootNode);
   BESENFreeAndNil(Parser);
   raise;
  end;
 finally
{$ifndef BESENNoFPU}
  if OldFPUExceptionMask<>BESENFPUExceptionMask then begin
   SetExceptionMask(OldFPUExceptionMask);
  end;
  if OldFPURoundingMode<>rmNearest then begin
   SetRoundMode(OldFPURoundingMode);
  end;
  if OldFPUPrecisionMode<>BESENFPUPrecisionMode then begin
   SetPrecisionMode(OldFPUPrecisionMode);
  end;
{$endif}
 end;
end;


end.
