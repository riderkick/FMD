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
unit BESENCodeSnapshot;
{$i BESEN.inc}

interface

uses SysUtils,Classes,BESENConstants,BESENTypes,BESENBaseObject,BESENASTNodes;

type TBESENCodeSnapshot=class(TBESENBaseObject)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function LoadFromStream(const Stream:TStream):TBESENASTNode;
       procedure SaveToStream(const Stream:TStream;const RootNode:TBESENASTNode);
     end;

implementation

uses BESEN,BESENValue,BESENInt64SelfBalancedTree,BESENPointerSelfBalancedTree,
     BESENCode,BESENOpcodes,BESENUtils,BESENKeyIDManager,
     BESENVersionConstants;

constructor TBESENCodeSnapshot.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
end;

destructor TBESENCodeSnapshot.Destroy;
begin
 inherited Destroy;
end;

function TBESENCodeSnapshot.LoadFromStream(const Stream:TStream):TBESENASTNode;
type TLoadedKeyManagerID=packed record
      ID:integer;
      Key:TBESENString;
     end;
     TLoadedKeyManagerIDs=array of TLoadedKeyManagerID;
var VisitedNodes:TBESENInt64SelfBalancedTree;
    LoadedKeyManagerIDs:TLoadedKeyManagerIDs;
 procedure BESENThrowStream(const s:string);
 begin
  raise EStreamError.Create(s);
 end;
 function ReadByte:byte;
 begin
  if Stream.Read(result,sizeof(byte))<>sizeof(byte) then begin
   BESENThrowStream('Couldn''t read byte');
  end;
 end;
 function ReadBool:boolean;
 begin
  result:=ReadByte<>0;
 end;
 function ReadInt32:integer;
 begin
  if Stream.Read(result,sizeof(integer))<>sizeof(integer) then begin
   BESENThrowStream('Couldn''t read integer');
  end;
 end;
 function ReadInt64:int64;
 begin
  if Stream.Read(result,sizeof(int64))<>sizeof(int64) then begin
   BESENThrowStream('Couldn''t read int64');
  end;
 end;
 function ReadDouble:double;
 begin
  if Stream.Read(result,sizeof(double))<>sizeof(double) then begin
   BESENThrowStream('Couldn''t double double');
  end;
 end;
 function ReadWideString:widestring;
 var l:integer;
 begin
  l:=ReadInt32;
  SetLength(result,l);
  if length(result)>0 then begin
   if Stream.Read(result[1],sizeof(widechar)*length(result))<>(sizeof(widechar)*length(result)) then begin
    BESENThrowStream('Couldn''t read widestring');
   end;
  end;
 end;
 function ReadNode:TBESENASTNode;
 var Counter,Target,LineNumber,i,j,k:integer;
     VisitNodeValue:TBESENInt64SelfBalancedTreeValue;
     v:PBESENValue;
     Ptr:int64;
     NodeType:TBESENASTNodeType;
     Code:TBESENCode;
     FunctionLiteral:TBESENASTNodeFunctionLiteral;
  function CreateNode(c:TBESENASTNodeClass):TBESENASTNode;
  var Counter:integer;
  begin
   result:=c.Create(Instance);
   VisitNodeValue.p:=result;
   VisitedNodes.Insert(Ptr,VisitNodeValue);
   TBESENASTNode(result).NodeType:=NodeType;
   TBESENASTNode(result).Target:=Target;
   TBESENASTNode(result).Location.LineNumber:=LineNumber;
   TBESENASTNode(result).CountTrashNodes:=ReadInt32;
   SetLength(TBESENASTNode(result).TrashNodes,TBESENASTNode(result).CountTrashNodes);
   for Counter:=0 to TBESENASTNode(result).CountTrashNodes-1 do begin
    TBESENASTNode(result).TrashNodes[Counter]:=TBESENASTNodeStatement(ReadNode);
   end;
  end;
 begin
  result:=nil;
  try
   Ptr:=ReadInt64;
   if Ptr<>0 then begin
    if VisitedNodes.Find(Ptr,VisitNodeValue) then begin
     result:=VisitNodeValue.p;
    end else begin
     NodeType:=ReadByte;
     Target:=ReadInt32;
     LineNumber:=ReadInt32;
     case NodeType of
      bntNONE:begin
       result:=CreateNode(TBESENASTNode);
      end;
      bntEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeExpression);
      end;
      bntLITERAL:begin
       result:=CreateNode(TBESENASTNodeLiteral);
      end;
      bntIDENTIFIER:begin
       result:=CreateNode(TBESENASTNodeIdentifier);
       TBESENASTNodeIdentifier(result).Name:=ReadWideString;
       TBESENASTNodeIdentifier(result).Index:=ReadInt32;
       TBESENASTNodeIdentifier(result).ParameterIndex:=ReadInt32;
       i:=ReadInt32;
       if (i>=0) and (i<length(LoadedKeyManagerIDs)) then begin
        TBESENASTNodeIdentifier(result).ID:=LoadedKeyManagerIDs[i].ID;
       end else begin
        TBESENASTNodeIdentifier(result).ID:=i;
       end;
       TBESENASTNodeIdentifier(result).IsParameter:=ReadBool;
       TBESENASTNodeIdentifier(result).IsLocal:=ReadBool;
      end;
      bntVARIABLEDECLARATION:begin
       result:=CreateNode(TBESENASTNodeVariableDeclaration);
       TBESENASTNodeVariableDeclaration(result).Identifier:=TBESENASTNodeIdentifier(ReadNode);
       TBESENASTNodeVariableDeclaration(result).Expression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntVARIABLEEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeVariableExpression);
       SetLength(TBESENASTNodeVariableExpression(result).Declarations,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeVariableExpression(result).Declarations)-1 do begin
        TBESENASTNodeVariableExpression(result).Declarations[Counter]:=TBESENASTNodeVariableDeclaration(ReadNode);
       end;
      end;
      bntFUNCTIONBODY:begin
       result:=CreateNode(TBESENASTNodeFunctionBody);
       TBESENASTNodeFunctionBody(result).IsFunction:=ReadBool;
       TBESENASTNodeFunctionBody(result).IsEmpty:=ReadBool;
       TBESENASTNodeFunctionBody(result).EnableLocalsOptimization:=ReadBool;
       TBESENASTNodeFunctionBody(result).DisableArgumentsObject:=ReadBool;
       SetLength(TBESENASTNodeFunctionBody(result).Variables,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeFunctionBody(result).Variables)-1 do begin
        TBESENASTNodeFunctionBody(result).Variables[Counter]:=TBESENASTNodeIdentifier(ReadNode);
       end;
       SetLength(TBESENASTNodeFunctionBody(result).Parameters,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeFunctionBody(result).Parameters)-1 do begin
        TBESENASTNodeFunctionBody(result).Parameters[Counter]:=TBESENASTNodeIdentifier(ReadNode);
       end;
       SetLength(TBESENASTNodeFunctionBody(result).Functions,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeFunctionBody(result).Functions)-1 do begin
        TBESENASTNodeFunctionBody(result).Functions[Counter]:=TBESENASTNodeStatement(ReadNode);
       end;
       SetLength(TBESENASTNodeFunctionBody(result).Statements,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeFunctionBody(result).Statements)-1 do begin
        TBESENASTNodeFunctionBody(result).Statements[Counter]:=TBESENASTNodeStatement(ReadNode);
       end;
       if ReadBool then begin
        Code:=TBESENCode(TBESENASTNodeFunctionBody(result).Code);
        Code.Body:=TBESENASTNodeFunctionBody(ReadNode);
        Code.MaxRegisters:=ReadInt32;
        Code.MaxBlock:=ReadInt32;
        Code.MaxParamArgs:=ReadInt32;
        Code.MaxLoop:=ReadInt32;
        Code.HasLocalDelete:=ReadBool;
        Code.IsComplexFunction:=ReadBool;
        Code.HasMaybeDirectEval:=ReadBool;
        Code.HoldLocalVariablesInRegisters:=ReadBool;
        Code.CountFunctionLiteralContainers:=ReadInt32;
        SetLength(Code.FunctionLiteralContainers,Code.CountFunctionLiteralContainers);
        for Counter:=0 to Code.CountFunctionLiteralContainers-1 do begin
         FunctionLiteral:=TBESENASTNodeFunctionLiteral(ReadNode);
         if assigned(FunctionLiteral) then begin
          if not assigned(FunctionLiteral.Container) then begin
           FunctionLiteral.Container:=TBESENFunctionLiteralContainer.Create(Instance);
           FunctionLiteral.Container.Literal:=FunctionLiteral;
          end;
          Code.FunctionLiteralContainers[Counter]:=FunctionLiteral.Container;
         end else begin
          Code.FunctionLiteralContainers[Counter]:=nil;
         end;
        end;
        Code.CountLiterals:=ReadInt32;
        SetLength(Code.Literals,Code.CountLiterals);
        for Counter:=0 to Code.CountLiterals-1 do begin
         v:=@Code.Literals[Counter];
         v^.ValueType:=ReadByte;
         case v^.ValueType of
          bvtNONE:begin
          end;
          bvtUNDEFINED:begin
          end;
          bvtNULL:begin
          end;
          bvtBOOLEAN:begin
           v^.Bool:=ReadBool;
          end;
          bvtNUMBER:begin
           v^.Num:=ReadDouble;
          end;
          bvtSTRING:begin
           v^.Str:=ReadWideString;
          end;
          else begin
           BESENThrowStream('Couldn''t read value');
          end;
         end;
        end;
        Code.CountLocations:=ReadInt32;
        SetLength(Code.Locations,Code.CountLocations);
        for Counter:=0 to Code.CountLocations-1 do begin
         Code.Locations[Counter].LineNumber:=ReadInt32;
        end;
        Code.CountVariables:=ReadInt32;
        SetLength(Code.Variables,Code.CountVariables);
        for Counter:=0 to Code.CountVariables-1 do begin
         Code.Variables[Counter].Name:=ReadWideString;
         Code.Variables[Counter].IsParameter:=ReadBool;
         Code.Variables[Counter].ParameterIndex:=ReadInt32;
        end;
        Code.CountPolymorphicInlineCacheInstructions:=ReadInt32;
        k:=ReadInt32;
        Code.LookupNames.Clear;
        for Counter:=0 to k-1 do begin
         Code.LookupNames.Add(ReadWideString);
        end;
        Code.ByteCodeLen:=ReadInt32;
        SetLength(Code.ByteCode,Code.ByteCodeLen);
        if Code.ByteCodeLen>0 then begin
         if Stream.Read(Code.ByteCode[0],Code.ByteCodeLen*sizeof(TBESENUINT32))<>(Code.ByteCodeLen*sizeof(TBESENUINT32)) then begin
          BESENThrowStream('Couldn''t read byte code');
         end;
         i:=0;
         while i<Code.ByteCodeLen do begin
          j:=i;
          inc(i,1+((Code.ByteCode[i] shr 8) and $ffffff));
          case Code.ByteCode[j] and $ff of
           bopREF:begin
            k:=Code.ByteCode[j+4];
            if (k>=0) and (k<length(LoadedKeyManagerIDs)) then begin
             k:=LoadedKeyManagerIDs[k].ID;
            end;
            Code.ByteCode[j+4]:=k;
           end;
          end;
         end;
        end;
        Code.Finish;
       end;
      end;
      bntFUNCTIONLITERAL:begin
       result:=CreateNode(TBESENASTNodeFunctionLiteral);
       TBESENASTNodeFunctionLiteral(result).Name:=TBESENASTNodeIdentifier(ReadNode);
       FunctionLiteral:=TBESENASTNodeFunctionLiteral(ReadNode);
       if assigned(FunctionLiteral) then begin
        if not assigned(FunctionLiteral.Container) then begin
         FunctionLiteral.Container:=TBESENFunctionLiteralContainer.Create(Instance);
         FunctionLiteral.Container.Literal:=FunctionLiteral;
        end;
        TBESENASTNodeFunctionLiteral(result).Container:=FunctionLiteral.Container;
       end else begin
        TBESENASTNodeFunctionLiteral(result).Container:=nil;
       end;
       TBESENASTNodeFunctionLiteral(result).Index:=ReadInt32;
       TBESENASTNodeFunctionLiteral(result).Body:=TBESENASTNodeFunctionBody(ReadNode);
      end;
      bntSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeStatement);
      end;
      bntVARIABLESTATEMENT:begin
       result:=CreateNode(TBESENASTNodeVariableStatement);
       SetLength(TBESENASTNodeVariableStatement(result).Declarations,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeVariableStatement(result).Declarations)-1 do begin
        TBESENASTNodeVariableStatement(result).Declarations[Counter]:=TBESENASTNodeVariableDeclaration(ReadNode);
       end;
      end;
      bntFUNCTIONDECLARATION:begin
       result:=CreateNode(TBESENASTNodeFunctionDeclaration);
       if ReadBool then begin
        FunctionLiteral:=TBESENASTNodeFunctionLiteral(ReadNode);
        if assigned(FunctionLiteral) then begin
         if not assigned(FunctionLiteral.Container) then begin
          FunctionLiteral.Container:=TBESENFunctionLiteralContainer.Create(Instance);
          FunctionLiteral.Container.Literal:=FunctionLiteral;
         end;
         TBESENASTNodeFunctionDeclaration(result).Container:=FunctionLiteral.Container;
        end else begin
         TBESENASTNodeFunctionDeclaration(result).Container:=nil;
        end;
       end else begin
        TBESENASTNodeFunctionDeclaration(result).Container:=nil;
       end;
      end;
      bntEXPRESSIONSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeExpressionStatement);
       TBESENASTNodeExpressionStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntEMPTYSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeEmptyStatement);
      end;
      bntBLOCKSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeBlockStatement);
       SetLength(TBESENASTNodeBlockStatement(result).Statements,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeBlockStatement(result).Statements)-1 do begin
        TBESENASTNodeBlockStatement(result).Statements[Counter]:=TBESENASTNodeStatement(ReadNode);
       end;
      end;
      bntDEBUGGERSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeDebuggerStatement);
      end;
      bntBREAKSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeBreakStatement);
       TBESENASTNodeBreakStatement(result).Identifier:=TBESENASTNodeIdentifier(ReadNode);
      end;
      bntCONTINUESTATEMENT:begin
       result:=CreateNode(TBESENASTNodeContinueStatement);
       TBESENASTNodeContinueStatement(result).Identifier:=TBESENASTNodeIdentifier(ReadNode);
      end;
      bntDOSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeDoStatement);
       TBESENASTNodeDoStatement(result).Statement:=TBESENASTNodeStatement(ReadNode);
       TBESENASTNodeDoStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntWHILESTATEMENT:begin
       result:=CreateNode(TBESENASTNodeWhileStatement);
       TBESENASTNodeWhileStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeWhileStatement(result).Statement:=TBESENASTNodeStatement(ReadNode);
      end;
      bntWITHSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeWithStatement);
       TBESENASTNodeWithStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeWithStatement(result).Statement:=TBESENASTNodeStatement(ReadNode);
      end;
      bntFORSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeForStatement);
       TBESENASTNodeForStatement(result).Initial:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeForStatement(result).Condition:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeForStatement(result).Increment:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeForStatement(result).Statement:=TBESENASTNodeStatement(ReadNode);
      end;
      bntFORINSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeForInStatement);
       TBESENASTNodeForInStatement(result).Variable:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeForInStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeForInStatement(result).Statement:=TBESENASTNodeStatement(ReadNode);
      end;
      bntIFSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeIfStatement);
       TBESENASTNodeIfStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeIfStatement(result).TrueStatement:=TBESENASTNodeStatement(ReadNode);
       TBESENASTNodeIfStatement(result).FalseStatement:=TBESENASTNodeStatement(ReadNode);
      end;
      bntLABELLEDSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeLabelledStatement);
       SetLength(TBESENASTNodeLabelledStatement(result).Identifiers,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeLabelledStatement(result).Identifiers)-1 do begin
        TBESENASTNodeLabelledStatement(result).Identifiers[Counter]:=TBESENASTNodeIdentifier(ReadNode);
       end;
       TBESENASTNodeLabelledStatement(result).Statement:=TBESENASTNodeStatement(ReadNode);
      end;
      bntCASESTATEMENT:begin
       result:=CreateNode(TBESENASTNodeCaseStatement);
       TBESENASTNodeCaseStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
       SetLength(TBESENASTNodeCaseStatement(result).Statements,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeCaseStatement(result).Statements)-1 do begin
        TBESENASTNodeCaseStatement(result).Statements[Counter]:=TBESENASTNodeStatement(ReadNode);
       end;
      end;
      bntSWITCHSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeSwitchStatement);
       TBESENASTNodeSwitchStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
       SetLength(TBESENASTNodeSwitchStatement(result).CaseStatements,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeSwitchStatement(result).CaseStatements)-1 do begin
        TBESENASTNodeSwitchStatement(result).CaseStatements[Counter]:=TBESENASTNodeCaseStatement(ReadNode);
       end;
      end;
      bntTHROWSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeThrowStatement);
       TBESENASTNodeThrowStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntTRYSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeTryStatement);
       TBESENASTNodeTryStatement(result).TryBlock:=TBESENASTNodeBlockStatement(ReadNode);
       TBESENASTNodeTryStatement(result).CatchIdentifier:=TBESENASTNodeIdentifier(ReadNode);
       TBESENASTNodeTryStatement(result).CatchBlock:=TBESENASTNodeBlockStatement(ReadNode);
       TBESENASTNodeTryStatement(result).FinallyBlock:=TBESENASTNodeBlockStatement(ReadNode);
      end;
      bntARRAYLITERAL:begin
       result:=CreateNode(TBESENASTNodeArrayLiteral);
       SetLength(TBESENASTNodeArrayLiteral(result).Elements,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeArrayLiteral(result).Elements)-1 do begin
        TBESENASTNodeArrayLiteral(result).Elements[Counter]:=TBESENASTNodeExpression(ReadNode);
       end;
      end;
      bntBINARYEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryExpression);
       TBESENASTNodeBinaryExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentExpression);
       TBESENASTNodeAssignmentExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTOPERATOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentOperatorExpression);
       TBESENASTNodeAssignmentOperatorExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentOperatorExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTMULTIPLYEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentMultiplyExpression);
       TBESENASTNodeAssignmentMultiplyExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentMultiplyExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTDIVIDEEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentDivideExpression);
       TBESENASTNodeAssignmentDivideExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentDivideExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTMODULOEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentModuloExpression);
       TBESENASTNodeAssignmentModuloExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentModuloExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTPLUSEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentPlusExpression);
       TBESENASTNodeAssignmentPlusExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentPlusExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTMINUSEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentMinusExpression);
       TBESENASTNodeAssignmentMinusExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentMinusExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTSHIFTLEFTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentShiftLeftExpression);
       TBESENASTNodeAssignmentShiftLeftExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentShiftLeftExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTSHIFTRIGHTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentShiftRightExpression);
       TBESENASTNodeAssignmentShiftRightExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentShiftRightExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentShiftRightUnsignedExpression);
       TBESENASTNodeAssignmentShiftRightUnsignedExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentShiftRightUnsignedExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTBITWISEANDEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentBitwiseAndExpression);
       TBESENASTNodeAssignmentBitwiseAndExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentBitwiseAndExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTBITWISEXOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentBitwiseXorExpression);
       TBESENASTNodeAssignmentBitwiseXorExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentBitwiseXorExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntASSIGNMENTBITWISEOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeAssignmentBitwiseOrExpression);
       TBESENASTNodeAssignmentBitwiseOrExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeAssignmentBitwiseOrExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYOPERATOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryOperatorExpression);
       TBESENASTNodeBinaryOperatorExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryOperatorExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYCOMMAEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryCommaExpression);
       TBESENASTNodeBinaryCommaExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryCommaExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYDIVIDEEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryDivideExpression);
       TBESENASTNodeBinaryDivideExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryDivideExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYMODULOEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryModuloExpression);
       TBESENASTNodeBinaryModuloExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryModuloExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYMULTIPLYEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryMultiplyExpression);
       TBESENASTNodeBinaryMultiplyExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryMultiplyExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYPLUSEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryPlusExpression);
       TBESENASTNodeBinaryPlusExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryPlusExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYMINUSEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryMinusExpression);
       TBESENASTNodeBinaryMinusExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryMinusExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYSHIFTLEFTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryShiftLeftExpression);
       TBESENASTNodeBinaryShiftLeftExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryShiftLeftExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYSHIFTRIGHTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryShiftRightExpression);
       TBESENASTNodeBinaryShiftRightExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryShiftRightExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryShiftRightUnsignedExpression);
       TBESENASTNodeBinaryShiftRightUnsignedExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryShiftRightUnsignedExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYGREATERTHANEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryGreaterThanExpression);
       TBESENASTNodeBinaryGreaterThanExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryGreaterThanExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYGREATERTHANOREQUALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryGreaterThanOrEqualExpression);
       TBESENASTNodeBinaryGreaterThanOrEqualExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryGreaterThanOrEqualExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYLESSTHANEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryLessThanExpression);
       TBESENASTNodeBinaryLessThanExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryLessThanExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYLESSTHANOREQUALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryLessThanOrEqualExpression);
       TBESENASTNodeBinaryLessThanOrEqualExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryLessThanOrEqualExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYINSTANCEOFEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryInstanceOfExpression);
       TBESENASTNodeBinaryInstanceOfExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryInstanceOfExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYINEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryInExpression);
       TBESENASTNodeBinaryInExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryInExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYEQUALEQUALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryEqualEqualExpression);
       TBESENASTNodeBinaryEqualEqualExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryEqualEqualExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYEQUALEQUALEQUALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryEqualEqualEqualExpression);
       TBESENASTNodeBinaryEqualEqualEqualExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryEqualEqualEqualExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYNOTEQUALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryNotEqualExpression);
       TBESENASTNodeBinaryNotEqualExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryNotEqualExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYNOTEQUALEQUALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryNotEqualEqualExpression);
       TBESENASTNodeBinaryNotEqualEqualExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryNotEqualEqualExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYBITWISEANDEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryBitwiseAndExpression);
       TBESENASTNodeBinaryBitwiseAndExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryBitwiseAndExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYBITWISEXOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryBitwiseXorExpression);
       TBESENASTNodeBinaryBitwiseXorExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryBitwiseXorExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBINARYBITWISEOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeBinaryBitwiseOrExpression);
       TBESENASTNodeBinaryBitwiseOrExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeBinaryBitwiseOrExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntBOOLEANLITERAL:begin
       result:=CreateNode(TBESENASTNodeBooleanLiteral);
       TBESENASTNodeBooleanLiteral(result).Value:=ReadBool;
      end;
      bntCALLEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeCallExpression);
       TBESENASTNodeCallExpression(result).TheFunction:=TBESENASTNodeExpression(ReadNode);
       SetLength(TBESENASTNodeCallExpression(result).Arguments,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeCallExpression(result).Arguments)-1 do begin
        TBESENASTNodeCallExpression(result).Arguments[Counter]:=TBESENASTNodeExpression(ReadNode);
       end;
      end;
      bntNEWEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeNewExpression);
       TBESENASTNodeNewExpression(result).TheFunction:=TBESENASTNodeExpression(ReadNode);
       SetLength(TBESENASTNodeNewExpression(result).Arguments,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeNewExpression(result).Arguments)-1 do begin
        TBESENASTNodeNewExpression(result).Arguments[Counter]:=TBESENASTNodeExpression(ReadNode);
       end;
      end;
      bntCONDITIONALEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeConditionalExpression);
       TBESENASTNodeConditionalExpression(result).Expression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeConditionalExpression(result).TrueExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeConditionalExpression(result).FalseExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryExpression);
       TBESENASTNodeUnaryExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYOPERATOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryOperatorExpression);
       TBESENASTNodeUnaryOperatorExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYPLUSEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryPlusExpression);
       TBESENASTNodeUnaryPlusExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYMINUSEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryMinusExpression);
       TBESENASTNodeUnaryMinusExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYBITWISENOTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryBitwiseNotExpression);
       TBESENASTNodeUnaryBitwiseNotExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYLOGICALNOTEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryLogicalNotExpression);
       TBESENASTNodeUnaryLogicalNotExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYVOIDEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryVoidExpression);
       TBESENASTNodeUnaryVoidExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntUNARYTYPEOFEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeUnaryTypeOfExpression);
       TBESENASTNodeUnaryTypeOfExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntPROPERTYEXPRESSION:begin
       result:=CreateNode(TBESENASTNodePropertyExpression);
       TBESENASTNodePropertyExpression(result).Dot:=ReadBool;
       TBESENASTNodePropertyExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodePropertyExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntLOGICALANDEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeLogicalAndExpression);
       TBESENASTNodeLogicalAndExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeLogicalAndExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntLOGICALOREXPRESSION:begin
       result:=CreateNode(TBESENASTNodeLogicalOrExpression);
       TBESENASTNodeLogicalOrExpression(result).LeftExpression:=TBESENASTNodeExpression(ReadNode);
       TBESENASTNodeLogicalOrExpression(result).RightExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntDELETEEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeDeleteExpression);
       TBESENASTNodeDeleteExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntPOSTFIXINCEXPRESSION:begin
       result:=CreateNode(TBESENASTNodePostfixIncExpression);
       TBESENASTNodePostfixIncExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntPOSTFIXDECEXPRESSION:begin
       result:=CreateNode(TBESENASTNodePostfixDecExpression);
       TBESENASTNodePostfixDecExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntPREFIXINCEXPRESSION:begin
       result:=CreateNode(TBESENASTNodePrefixIncExpression);
       TBESENASTNodePrefixIncExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntPREFIXDECEXPRESSION:begin
       result:=CreateNode(TBESENASTNodePrefixDecExpression);
       TBESENASTNodePrefixDecExpression(result).SubExpression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntNULLLITERAL:begin
      end;
      bntNUMBERLITERAL:begin
       result:=CreateNode(TBESENASTNodeNumberLiteral);
       TBESENASTNodeNumberLiteral(result).Value:=ReadDouble;
      end;
      bntREGEXPLITERAL:begin
       result:=CreateNode(TBESENASTNodeRegExpLiteral);
       TBESENASTNodeRegExpLiteral(result).Source:=ReadWideString;
       TBESENASTNodeRegExpLiteral(result).Flags:=ReadWideString;
      end;
      bntSTRINGLITERAL:begin
       result:=CreateNode(TBESENASTNodeStringLiteral);
       TBESENASTNodeStringLiteral(result).Value:=ReadWideString;
      end;
      bntTHISLITERAL:begin
       result:=CreateNode(TBESENASTNodeThisLiteral);
      end;
      bntOBJECTLITERALPROPERTY:begin
       result:=CreateNode(TBESENASTNodeObjectLiteralProperty);
       TBESENASTNodeObjectLiteralProperty(result).Name:=ReadWideString;
       TBESENASTNodeObjectLiteralProperty(result).PropertyType:=TBESENASTNodeObjectLiteralPropertyType(ReadInt32);
       TBESENASTNodeObjectLiteralProperty(result).PropertyAccessorType:=TBESENASTNodeObjectLiteralPropertyAccessorType(ReadInt32);
       TBESENASTNodeObjectLiteralProperty(result).Value:=TBESENASTNodeExpression(ReadNode);
       if ReadBool then begin
        FunctionLiteral:=TBESENASTNodeFunctionLiteral(ReadNode);
        if assigned(FunctionLiteral) then begin
         if not assigned(FunctionLiteral.Container) then begin
          FunctionLiteral.Container:=TBESENFunctionLiteralContainer.Create(Instance);
          FunctionLiteral.Container.Literal:=FunctionLiteral;
         end;
         TBESENASTNodeObjectLiteralProperty(result).Container:=FunctionLiteral.Container;
        end else begin
         TBESENASTNodeObjectLiteralProperty(result).Container:=nil;
        end;
       end else begin
        TBESENASTNodeObjectLiteralProperty(result).Container:=nil;
       end;
      end;
      bntOBJECTLITERAL:begin
       result:=CreateNode(TBESENASTNodeObjectLiteral);
       SetLength(TBESENASTNodeObjectLiteral(result).Properties,ReadInt32);
       for Counter:=0 to length(TBESENASTNodeObjectLiteral(result).Properties)-1 do begin
        TBESENASTNodeObjectLiteral(result).Properties[Counter]:=TBESENASTNodeObjectLiteralProperty(ReadNode);
       end;
      end;
      bntRETURNSTATEMENT:begin
       result:=CreateNode(TBESENASTNodeReturnStatement);
       TBESENASTNodeReturnStatement(result).Expression:=TBESENASTNodeExpression(ReadNode);
      end;
      bntPROGRAM:begin
       result:=CreateNode(TBESENASTNodeProgram);
       TBESENASTNodeProgram(result).Body:=TBESENASTNodeFunctionBody(ReadNode);
      end;
      bntFUNCTIONEXPRESSION:begin
       result:=CreateNode(TBESENASTNodeFunctionExpression);
       if ReadBool then begin
        FunctionLiteral:=TBESENASTNodeFunctionLiteral(ReadNode);
        if assigned(FunctionLiteral) then begin
         if not assigned(FunctionLiteral.Container) then begin
          FunctionLiteral.Container:=TBESENFunctionLiteralContainer.Create(Instance);
          FunctionLiteral.Container.Literal:=FunctionLiteral;
         end;
         TBESENASTNodeFunctionExpression(result).Container:=FunctionLiteral.Container;
        end else begin
         TBESENASTNodeFunctionExpression(result).Container:=nil;
        end;
       end else begin
        TBESENASTNodeFunctionExpression(result).Container:=nil;
       end;
      end;
     end;
    end;
   end;
  except
   BESENFreeAndNil(result);
  end;
 end;
var Count,Counter:integer;
    Key:TBESENString;
begin
 VisitedNodes:=TBESENInt64SelfBalancedTree.Create;
 LoadedKeyManagerIDs:=nil;
 try
  result:=nil;
  try
   if ReadInt64<>BESENCodeFormatRevisionNumber then begin
    BESENThrowStream('Bad format');
   end;
   Count:=ReadInt32;
   SetLength(LoadedKeyManagerIDs,Count);
   for Counter:=0 to Count-1 do begin
    Key:=ReadWideString;
    LoadedKeyManagerIDs[Counter].Key:=Key;
    LoadedKeyManagerIDs[Counter].ID:=TBESEN(Instance).KeyIDManager.Get(Key);
   end;
   result:=ReadNode;
  except
   BESENFreeAndNil(result);
  end;
 finally
  BESENFreeAndNil(VisitedNodes);
  SetLength(LoadedKeyManagerIDs,0);
  Key:='';
 end;
end;

procedure TBESENCodeSnapshot.SaveToStream(const Stream:TStream;const RootNode:TBESENASTNode);
var VisitedNodes:TBESENPointerSelfBalancedTree;
 procedure BESENThrowStream(const s:string);
 begin
  raise EStreamError.Create(s);
 end;
 procedure WriteByte(const b:byte);
 begin
  if Stream.Write(b,sizeof(byte))<>sizeof(byte) then begin
   BESENThrowStream('Couldn''t write byte');
  end;
 end;
 procedure WriteBool(const b:boolean);
 begin
  if b then begin
   WriteByte($ff);
  end else begin
   WriteByte(0);
  end;
 end;
 procedure WriteInt32(const i:integer);
 begin
  if Stream.Write(i,sizeof(integer))<>sizeof(integer) then begin
   BESENThrowStream('Couldn''t write integer');
  end;
 end;
 procedure WriteInt64(const i:int64);
 begin
  if Stream.Write(i,sizeof(int64))<>sizeof(int64) then begin
   BESENThrowStream('Couldn''t write int64');
  end;
 end;
 procedure WriteDouble(const d:double);
 begin
  if Stream.Write(d,sizeof(double))<>sizeof(double) then begin
   BESENThrowStream('Couldn''t write double');
  end;
 end;
 procedure WriteWideString(const ws:widestring);
 begin
  WriteInt32(length(ws));
  if length(ws)>0 then begin
   if Stream.Write(ws[1],sizeof(widechar)*length(ws))<>(sizeof(widechar)*length(ws)) then begin
    BESENThrowStream('Couldn''t write widestring');
   end;
  end;
 end;
 procedure Visit(ToVisit:TBESENASTNode);
 var Counter:integer;
     VisitNodeValue:TBESENPointerSelfBalancedTreeValue;
     v:PBESENValue;
     Code:TBESENCode;
 begin
  WriteInt64(ptrint(ToVisit));
   if assigned(ToVisit) and not VisitedNodes.Find(ToVisit,VisitNodeValue) then begin
   VisitNodeValue.p:=ToVisit;
   VisitedNodes.Insert(ToVisit,VisitNodeValue);
   WriteByte(ToVisit.NodeType);
   WriteInt32(TBESENASTNode(ToVisit).Target);
   WriteInt32(TBESENASTNode(ToVisit).Location.LineNumber);
   WriteInt32(TBESENASTNode(ToVisit).CountTrashNodes);
   for Counter:=0 to TBESENASTNode(ToVisit).CountTrashNodes-1 do begin
    Visit(TBESENASTNode(ToVisit).TrashNodes[Counter]);
   end;
   case ToVisit.NodeType of
    bntNONE:begin
    end;
    bntEXPRESSION:begin
    end;
    bntLITERAL:begin
    end;
    bntIDENTIFIER:begin
     WriteWideString(TBESENASTNodeIdentifier(ToVisit).Name);
     WriteInt32(TBESENASTNodeIdentifier(ToVisit).Index);
     WriteInt32(TBESENASTNodeIdentifier(ToVisit).ParameterIndex);
     WriteInt32(TBESENASTNodeIdentifier(ToVisit).ID);
     WriteBool(TBESENASTNodeIdentifier(ToVisit).IsParameter);
     WriteBool(TBESENASTNodeIdentifier(ToVisit).IsLocal);
    end;
    bntVARIABLEDECLARATION:begin
     Visit(TBESENASTNodeVariableDeclaration(ToVisit).Identifier);
     Visit(TBESENASTNodeVariableDeclaration(ToVisit).Expression);
    end;
    bntVARIABLEEXPRESSION:begin
     WriteInt32(length(TBESENASTNodeVariableExpression(ToVisit).Declarations));
     for Counter:=0 to length(TBESENASTNodeVariableExpression(ToVisit).Declarations)-1 do begin
      Visit(TBESENASTNodeVariableExpression(ToVisit).Declarations[Counter]);
     end;
    end;
    bntFUNCTIONBODY:begin
     WriteBool(TBESENASTNodeFunctionBody(ToVisit).IsFunction);
     WriteBool(TBESENASTNodeFunctionBody(ToVisit).IsEmpty);
     WriteBool(TBESENASTNodeFunctionBody(ToVisit).EnableLocalsOptimization);
     WriteBool(TBESENASTNodeFunctionBody(ToVisit).DisableArgumentsObject);
     WriteInt32(length(TBESENASTNodeFunctionBody(ToVisit).Variables));
     for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Variables)-1 do begin
      Visit(TBESENASTNodeFunctionBody(ToVisit).Variables[Counter]);
     end;
     WriteInt32(length(TBESENASTNodeFunctionBody(ToVisit).Parameters));
     for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Parameters)-1 do begin
      Visit(TBESENASTNodeFunctionBody(ToVisit).Parameters[Counter]);
     end;
     WriteInt32(length(TBESENASTNodeFunctionBody(ToVisit).Functions));
     for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Functions)-1 do begin
      Visit(TBESENASTNodeFunctionBody(ToVisit).Functions[Counter]);
     end;
     WriteInt32(length(TBESENASTNodeFunctionBody(ToVisit).Statements));
     for Counter:=0 to length(TBESENASTNodeFunctionBody(ToVisit).Statements)-1 do begin
      Visit(TBESENASTNodeFunctionBody(ToVisit).Statements[Counter]);
     end;
     WriteBool(assigned(TBESENASTNodeFunctionBody(ToVisit).Code));
     if assigned(TBESENASTNodeFunctionBody(ToVisit).Code) then begin
      Code:=TBESENCode(TBESENASTNodeFunctionBody(ToVisit).Code);
      Visit(Code.Body);
      WriteInt32(Code.MaxRegisters);
      WriteInt32(Code.MaxBlock);
      WriteInt32(Code.MaxParamArgs);
      WriteInt32(Code.MaxLoop);
      WriteBool(Code.HasLocalDelete);
      WriteBool(Code.IsComplexFunction);
      WriteBool(Code.HasMaybeDirectEval);
      WriteBool(Code.HoldLocalVariablesInRegisters);
      WriteInt32(Code.CountFunctionLiteralContainers);
      for Counter:=0 to Code.CountFunctionLiteralContainers-1 do begin
       Visit(Code.FunctionLiteralContainers[Counter].Literal);
      end;
      WriteInt32(Code.CountLiterals);
      for Counter:=0 to Code.CountLiterals-1 do begin
       v:=@Code.Literals[Counter];
       WriteByte(v^.ValueType);
       case v^.ValueType of
        bvtNONE:begin
        end;
        bvtUNDEFINED:begin
        end;
        bvtNULL:begin
        end;
        bvtBOOLEAN:begin
         WriteBool(v^.Bool);
        end;
        bvtNUMBER:begin
         WriteDouble(v^.Num);
        end;
        bvtSTRING:begin
         WriteWideString(v^.Str);
        end;
        else begin
         BESENThrowStream('Couldn''t write value');
        end;
       end;
      end;
      WriteInt32(Code.CountLocations);
      for Counter:=0 to Code.CountLocations-1 do begin
       WriteInt32(Code.Locations[Counter].LineNumber);
      end;
      WriteInt32(Code.CountVariables);
      for Counter:=0 to Code.CountVariables-1 do begin
       WriteWideString(Code.Variables[Counter].Name);
       WriteBool(Code.Variables[Counter].IsParameter);
       WriteInt32(Code.Variables[Counter].ParameterIndex);
      end;
      WriteInt32(Code.CountPolymorphicInlineCacheInstructions);
      WriteInt32(Code.LookupNames.Count);
      for Counter:=0 to Code.LookupNames.Count-1 do begin
       WriteWideString(Code.LookupNames[Counter]);
      end;
      WriteInt32(Code.ByteCodeLen);
      if Code.ByteCodeLen>0 then begin
       if Stream.Write(Code.ByteCode[0],Code.ByteCodeLen*sizeof(TBESENUINT32))<>(Code.ByteCodeLen*sizeof(TBESENUINT32)) then begin
        BESENThrowStream('Couldn''t write byte code');
       end;
      end;
     end;
    end;
    bntFUNCTIONLITERAL:begin
     Visit(TBESENASTNodeFunctionLiteral(ToVisit).Name);
     Visit(TBESENASTNodeFunctionLiteral(ToVisit).Container.Literal);
     WriteInt32(TBESENASTNodeFunctionLiteral(ToVisit).Index);
     Visit(TBESENASTNodeFunctionLiteral(ToVisit).Body);
    end;
    bntSTATEMENT:begin
    end;
    bntVARIABLESTATEMENT:begin
     WriteInt32(length(TBESENASTNodeVariableStatement(ToVisit).Declarations));
     for Counter:=0 to length(TBESENASTNodeVariableStatement(ToVisit).Declarations)-1 do begin
      Visit(TBESENASTNodeVariableStatement(ToVisit).Declarations[Counter]);
     end;
    end;
    bntFUNCTIONDECLARATION:begin
     WriteBool(assigned(TBESENASTNodeFunctionDeclaration(ToVisit).Container));
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
     WriteInt32(length(TBESENASTNodeBlockStatement(ToVisit).Statements));
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
     WriteInt32(length(TBESENASTNodeLabelledStatement(ToVisit).Identifiers));
     for Counter:=0 to length(TBESENASTNodeLabelledStatement(ToVisit).Identifiers)-1 do begin
      Visit(TBESENASTNodeLabelledStatement(ToVisit).Identifiers[Counter]);
     end;
     Visit(TBESENASTNodeLabelledStatement(ToVisit).Statement);
    end;
    bntCASESTATEMENT:begin
     Visit(TBESENASTNodeCaseStatement(ToVisit).Expression);
     WriteInt32(length(TBESENASTNodeCaseStatement(ToVisit).Statements));
     for Counter:=0 to length(TBESENASTNodeCaseStatement(ToVisit).Statements)-1 do begin
      Visit(TBESENASTNodeCaseStatement(ToVisit).Statements[Counter]);
     end;
    end;
    bntSWITCHSTATEMENT:begin
     Visit(TBESENASTNodeSwitchStatement(ToVisit).Expression);
     WriteInt32(length(TBESENASTNodeSwitchStatement(ToVisit).CaseStatements));
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
     WriteInt32(length(TBESENASTNodeArrayLiteral(ToVisit).Elements));
     for Counter:=0 to length(TBESENASTNodeArrayLiteral(ToVisit).Elements)-1 do begin
      Visit(TBESENASTNodeArrayLiteral(ToVisit).Elements[Counter]);
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
     WriteBool(TBESENASTNodeBooleanLiteral(ToVisit).Value);
    end;
    bntCALLEXPRESSION:begin
     Visit(TBESENASTNodeCallExpression(ToVisit).TheFunction);
     WriteInt32(length(TBESENASTNodeCallExpression(ToVisit).Arguments));
     for Counter:=0 to length(TBESENASTNodeCallExpression(ToVisit).Arguments)-1 do begin
      Visit(TBESENASTNodeCallExpression(ToVisit).Arguments[Counter]);
     end;
    end;
    bntNEWEXPRESSION:begin
     Visit(TBESENASTNodeNewExpression(ToVisit).TheFunction);
     WriteInt32(length(TBESENASTNodeNewExpression(ToVisit).Arguments));
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
     WriteBool(TBESENASTNodePropertyExpression(ToVisit).Dot);
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
     WriteDouble(TBESENASTNodeNumberLiteral(ToVisit).Value);
    end;
    bntREGEXPLITERAL:begin
     WriteWideString(TBESENASTNodeRegExpLiteral(ToVisit).Source);
     WriteWideString(TBESENASTNodeRegExpLiteral(ToVisit).Flags);
    end;
    bntSTRINGLITERAL:begin
     WriteWideString(TBESENASTNodeStringLiteral(ToVisit).Value);
    end;
    bntTHISLITERAL:begin
    end;
    bntOBJECTLITERALPROPERTY:begin
     WriteWideString(TBESENASTNodeObjectLiteralProperty(ToVisit).Name);
     WriteInt32(integer(TBESENASTNodeObjectLiteralProperty(ToVisit).PropertyType));
     WriteInt32(integer(TBESENASTNodeObjectLiteralProperty(ToVisit).PropertyAccessorType));
     Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Value);
     WriteBool(assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container));
     if assigned(TBESENASTNodeObjectLiteralProperty(ToVisit).Container) then begin
      Visit(TBESENASTNodeObjectLiteralProperty(ToVisit).Container.Literal);
     end;
    end;
    bntOBJECTLITERAL:begin
     WriteInt32(length(TBESENASTNodeObjectLiteral(ToVisit).Properties));
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
     WriteBool(assigned(TBESENASTNodeFunctionExpression(ToVisit).Container));
     if assigned(TBESENASTNodeFunctionExpression(ToVisit).Container) then begin
      Visit(TBESENASTNodeFunctionExpression(ToVisit).Container.Literal);
     end;
    end;
   end;
  end;
 end;
var Counter:integer;
begin
 VisitedNodes:=TBESENPointerSelfBalancedTree.Create;
 try
  WriteInt64(BESENCodeFormatRevisionNumber);
  WriteInt32(TBESEN(Instance).KeyIDManager.List.Count);
  for Counter:=0 to TBESEN(Instance).KeyIDManager.List.Count-1 do begin
   WriteWideString(TBESEN(Instance).KeyIDManager.List[Counter]);
  end;
  Visit(RootNode);
 finally
  BESENFreeAndNil(VisitedNodes);
 end;
end;

end.
