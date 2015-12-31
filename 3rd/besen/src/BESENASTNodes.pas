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
unit BESENASTNodes;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENGarbageCollector;

const bntNONE=0;
      bntEXPRESSION=1;
      bntLITERAL=2;
      bntIDENTIFIER=3;
      bntVARIABLEDECLARATION=4;
      bntVARIABLEEXPRESSION=5;
      bntFUNCTIONBODY=6;
      bntFUNCTIONLITERAL=7;
      bntSTATEMENT=8;
      bntVARIABLESTATEMENT=9;
      bntFUNCTIONDECLARATION=10;
      bntEXPRESSIONSTATEMENT=11;
      bntEMPTYSTATEMENT=12;
      bntBLOCKSTATEMENT=13;
      bntDEBUGGERSTATEMENT=14;
      bntBREAKSTATEMENT=15;
      bntCONTINUESTATEMENT=16;
      bntDOSTATEMENT=17;
      bntWHILESTATEMENT=18;
      bntWITHSTATEMENT=19;
      bntFORSTATEMENT=20;
      bntFORINSTATEMENT=21;
      bntIFSTATEMENT=22;
      bntLABELLEDSTATEMENT=23;
      bntCASESTATEMENT=24;
      bntSWITCHSTATEMENT=25;
      bntTHROWSTATEMENT=26;
      bntTRYSTATEMENT=27;
      bntARRAYLITERAL=28;
      bntBINARYEXPRESSION=29;
      bntASSIGNMENTEXPRESSION=30;
      bntASSIGNMENTOPERATOREXPRESSION=31;
      bntASSIGNMENTMULTIPLYEXPRESSION=32;
      bntASSIGNMENTDIVIDEEXPRESSION=33;
      bntASSIGNMENTMODULOEXPRESSION=34;
      bntASSIGNMENTPLUSEXPRESSION=35;
      bntASSIGNMENTMINUSEXPRESSION=36;
      bntASSIGNMENTSHIFTLEFTEXPRESSION=37;
      bntASSIGNMENTSHIFTRIGHTEXPRESSION=38;
      bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION=39;
      bntASSIGNMENTBITWISEANDEXPRESSION=40;
      bntASSIGNMENTBITWISEXOREXPRESSION=41;
      bntASSIGNMENTBITWISEOREXPRESSION=42;
      bntBINARYOPERATOREXPRESSION=43;
      bntBINARYCOMMAEXPRESSION=44;
      bntBINARYDIVIDEEXPRESSION=45;
      bntBINARYMODULOEXPRESSION=46;
      bntBINARYMULTIPLYEXPRESSION=47;
      bntBINARYPLUSEXPRESSION=48;
      bntBINARYMINUSEXPRESSION=49;
      bntBINARYSHIFTLEFTEXPRESSION=50;
      bntBINARYSHIFTRIGHTEXPRESSION=51;
      bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION=52;
      bntBINARYGREATERTHANEXPRESSION=53;
      bntBINARYGREATERTHANOREQUALEXPRESSION=54;
      bntBINARYLESSTHANEXPRESSION=55;
      bntBINARYLESSTHANOREQUALEXPRESSION=56;
      bntBINARYINSTANCEOFEXPRESSION=57;
      bntBINARYINEXPRESSION=58;
      bntBINARYEQUALEQUALEXPRESSION=59;
      bntBINARYEQUALEQUALEQUALEXPRESSION=60;
      bntBINARYNOTEQUALEXPRESSION=61;
      bntBINARYNOTEQUALEQUALEXPRESSION=62;
      bntBINARYBITWISEANDEXPRESSION=63;
      bntBINARYBITWISEXOREXPRESSION=64;
      bntBINARYBITWISEOREXPRESSION=65;
      bntBOOLEANLITERAL=66;
      bntCALLEXPRESSION=67;
      bntNEWEXPRESSION=68;
      bntCONDITIONALEXPRESSION=69;
      bntUNARYEXPRESSION=70;
      bntUNARYOPERATOREXPRESSION=71;
      bntUNARYPLUSEXPRESSION=72;
      bntUNARYMINUSEXPRESSION=73;
      bntUNARYBITWISENOTEXPRESSION=74;
      bntUNARYLOGICALNOTEXPRESSION=75;
      bntUNARYVOIDEXPRESSION=76;
      bntUNARYTYPEOFEXPRESSION=77;
      bntPROPERTYEXPRESSION=78;
      bntLOGICALANDEXPRESSION=79;
      bntLOGICALOREXPRESSION=80;
      bntDELETEEXPRESSION=81;
      bntPOSTFIXINCEXPRESSION=82;
      bntPOSTFIXDECEXPRESSION=83;
      bntPREFIXINCEXPRESSION=84;
      bntPREFIXDECEXPRESSION=85;
      bntNULLLITERAL=86;
      bntNUMBERLITERAL=87;
      bntREGEXPLITERAL=88;
      bntSTRINGLITERAL=89;
      bntTHISLITERAL=90;
      bntOBJECTLITERALPROPERTY=91;
      bntOBJECTLITERAL=92;
      bntRETURNSTATEMENT=93;
      bntPROGRAM=94;
      bntFUNCTIONEXPRESSION=95;

type TBESENFunctionLiteralContainer=class;

     TBESENFunctionLiteralContainers=array of TBESENFunctionLiteralContainer;

     TBESENASTNodeFunctionLiteral=class;

     TBESENFunctionLiteralContainer=class(TBESENGarbageCollectorObject)
      public
       Literal:TBESENASTNodeFunctionLiteral;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

     TBESENASTNodeType=byte;

     TBESENASTNodeCodeGenData=record
      RegNr:integer;
     end;

     TBESENASTNode=class;

     TBESENASTNodes=array of TBESENASTNode;

     TBESENASTNode=class(TBESENCollectorObject)
      public
       NodeType:TBESENASTNodeType;
       Target:TBESENTarget;
       Location:TBESENLocation;
       CodeGenData:TBESENASTNodeCodeGenData;
       TrashNodes:TBESENASTNodes;
       CountTrashNodes:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure ExecuteCode(const Context:TObject;var AResult:TBESENValue); virtual;
     end;

     TBESENASTNodeClass=class of TBESENASTNode;

     TBESENASTNodeExpression=class(TBESENASTNode)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeExpressions=array of TBESENASTNodeExpression;

     TBESENASTNodeLiteral=class(TBESENASTNodeExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeIdentifier=class(TBESENASTNodeLiteral)
      public
       Name:TBESENString;
       Index:integer;
       ParameterIndex:integer;
       ID:TBESENINT32;
       IsParameter:longbool;
       IsLocal:longbool;
       IsReached:longbool;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeIdentifiers=array of TBESENASTNodeIdentifier;

     TBESENASTNodeVariableDeclaration=class(TBESENASTNodeExpression)
      public
       Identifier:TBESENASTNodeIdentifier;
       Expression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeVariableDeclarations=array of TBESENASTNodeVariableDeclaration;

     TBESENASTNodeVariableExpression=class(TBESENASTNodeExpression)
      public
       Declarations:TBESENASTNodeVariableDeclarations;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeStatement=class;

     TBESENASTNodeStatements=array of TBESENASTNodeStatement;

     TBESENASTNodeBlockStatement=class;

     TBESENASTNodeFunctionBody=class(TBESENASTNode)
      public
       Variables:TBESENASTNodeIdentifiers;
       Parameters:TBESENASTNodeIdentifiers;
       Functions:TBESENASTNodeStatements;
       Statements:TBESENASTNodeStatements;
       IsFunction:longbool;
       IsEmpty:longbool;
       EnableLocalsOptimization:longbool;
       DisableArgumentsObject:longbool;
       IsStrict:longbool;
       Code:TObject;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure ExecuteCode(const Context:TObject;var AResult:TBESENValue); override;
     end;

     TBESENASTNodeFunctionLiteral=class(TBESENASTNodeLiteral)
      public
       Name:TBESENASTNodeIdentifier;
       Container:TBESENFunctionLiteralContainer;
       Body:TBESENASTNodeFunctionBody;
       Index:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure ExecuteCode(const Context:TObject;var AResult:TBESENValue); override;
     end;

     TBESENASTNodeStatement=class(TBESENASTNode)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeVariableStatement=class(TBESENASTNodeStatement)
      public
       Declarations:TBESENASTNodeVariableDeclarations;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeFunctionDeclaration=class(TBESENASTNodeStatement)
      public
       Container:TBESENFunctionLiteralContainer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeFunctionExpression=class(TBESENASTNodeExpression)
      public
       Container:TBESENFunctionLiteralContainer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeExpressionStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeEmptyStatement=class(TBESENASTNodeStatement)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBlockStatement=class(TBESENASTNodeStatement)
      public
       Statements:TBESENASTNodeStatements;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeDebuggerStatement=class(TBESENASTNodeStatement)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBreakStatement=class(TBESENASTNodeStatement)
      public
       Identifier:TBESENASTNodeIdentifier;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeContinueStatement=class(TBESENASTNodeStatement)
      public
       Identifier:TBESENASTNodeIdentifier;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeDoStatement=class(TBESENASTNodeStatement)
      public
       Statement:TBESENASTNodeStatement;
       Expression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeWhileStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       Statement:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeWithStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       Statement:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeForStatement=class(TBESENASTNodeStatement)
      public
       Initial:TBESENASTNodeExpression;
       Condition:TBESENASTNodeExpression;
       Increment:TBESENASTNodeExpression;
       Statement:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeForInStatement=class(TBESENASTNodeStatement)
      public
       Variable:TBESENASTNodeExpression;
       Expression:TBESENASTNodeExpression;
       Statement:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeIfStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       TrueStatement:TBESENASTNodeStatement;
       FalseStatement:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeLabelledStatement=class(TBESENASTNodeStatement)
      public
       Identifiers:TBESENASTNodeIdentifiers;
       Statement:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeCaseStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       Statements:TBESENASTNodeStatements;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeCaseStatements=array of TBESENASTNodeCaseStatement;

     TBESENASTNodeSwitchStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       CaseStatements:TBESENASTNodeCaseStatements;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeThrowStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeTryStatement=class(TBESENASTNodeStatement)
      public
       TryBlock:TBESENASTNodeStatement;
       CatchIdentifier:TBESENASTNodeIdentifier;
       CatchBlock:TBESENASTNodeStatement;
       FinallyBlock:TBESENASTNodeStatement;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeArrayLiteral=class(TBESENASTNodeLiteral)
      public
       Elements:TBESENASTNodeExpressions;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryExpression=class(TBESENASTNodeExpression)
      public
       LeftExpression:TBESENASTNodeExpression;
       RightExpression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentExpression=class(TBESENASTNodeBinaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentOperatorExpression=class(TBESENASTNodeBinaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentMultiplyExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentDivideExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentModuloExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentPlusExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentMinusExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentShiftLeftExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentShiftRightExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentShiftRightUnsignedExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentBitwiseAndExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentBitwiseXorExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeAssignmentBitwiseOrExpression=class(TBESENASTNodeAssignmentOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryOperatorExpression=class(TBESENASTNodeBinaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryCommaExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryDivideExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryModuloExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryMultiplyExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryPlusExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryMinusExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryShiftLeftExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryShiftRightExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryShiftRightUnsignedExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryGreaterThanExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryGreaterThanOrEqualExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryLessThanExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryLessThanOrEqualExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryInstanceOfExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryInExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryEqualEqualExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryEqualEqualEqualExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryNotEqualExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryNotEqualEqualExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryBitwiseAndExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryBitwiseXorExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBinaryBitwiseOrExpression=class(TBESENASTNodeBinaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeBooleanLiteral=class(TBESENASTNodeLiteral)
      public
       Value:longbool;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeCallExpression=class(TBESENASTNodeExpression)
      public
       TheFunction:TBESENASTNodeExpression;
       Arguments:TBESENASTNodeExpressions;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeNewExpression=class(TBESENASTNodeExpression)
      public
       TheFunction:TBESENASTNodeExpression;
       Arguments:TBESENASTNodeExpressions;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeConditionalExpression=class(TBESENASTNodeExpression)
      public
       Expression:TBESENASTNodeExpression;
       TrueExpression:TBESENASTNodeExpression;
       FalseExpression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryExpression=class(TBESENASTNodeExpression)
      public
       SubExpression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryOperatorExpression=class(TBESENASTNodeUnaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryPlusExpression=class(TBESENASTNodeUnaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryMinusExpression=class(TBESENASTNodeUnaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryBitwiseNotExpression=class(TBESENASTNodeUnaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryLogicalNotExpression=class(TBESENASTNodeUnaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryVoidExpression=class(TBESENASTNodeUnaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeUnaryTypeOfExpression=class(TBESENASTNodeUnaryOperatorExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodePropertyExpression=class(TBESENASTNodeBinaryExpression)
      public
       Dot:longbool;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeLogicalAndExpression=class(TBESENASTNodeBinaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeLogicalOrExpression=class(TBESENASTNodeBinaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeDeleteExpression=class(TBESENASTNodeUnaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodePostfixIncExpression=class(TBESENASTNodeUnaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodePostfixDecExpression=class(TBESENASTNodeUnaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodePrefixIncExpression=class(TBESENASTNodeUnaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodePrefixDecExpression=class(TBESENASTNodeUnaryExpression)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeNullLiteral=class(TBESENASTNodeLiteral)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeNumberLiteral=class(TBESENASTNodeLiteral)
      public
       Value:double;
       Index:integer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeRegExpLiteral=class(TBESENASTNodeLiteral)
      public
       Source,Flags:TBESENString;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeStringLiteral=class(TBESENASTNodeLiteral)
      public
       Value:TBESENString;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeThisLiteral=class(TBESENASTNodeLiteral)
      public
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeObjectLiteralPropertyType=(banolptNONE,banolptACCESSOR,banolptDATA);

     TBESENASTNodeObjectLiteralPropertyAccessorType=(banolpatNONE,banolpatGET,banolpatSET);

     TBESENASTNodeObjectLiteralProperty=class(TBESENASTNodeLiteral)
      public
       PropertyType:TBESENASTNodeObjectLiteralPropertyType;
       PropertyAccessorType:TBESENASTNodeObjectLiteralPropertyAccessorType;
       Name:TBESENString;
       Value:TBESENASTNodeExpression;
       Container:TBESENFunctionLiteralContainer;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeObjectLiteralProperties=array of TBESENASTNodeObjectLiteralProperty;

     TBESENASTNodeObjectLiteral=class(TBESENASTNodeLiteral)
      public
       Properties:TBESENASTNodeObjectLiteralProperties;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeReturnStatement=class(TBESENASTNodeStatement)
      public
       Expression:TBESENASTNodeExpression;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
     end;

     TBESENASTNodeProgram=class(TBESENASTNode)
      public
       Body:TBESENASTNodeFunctionBody;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure ExecuteCode(const Context:TObject;var AResult:TBESENValue); override;
     end;

implementation

uses BESEN,BESENUtils,BESENCode;

constructor TBESENFunctionLiteralContainer.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 Literal:=nil;
end;

destructor TBESENFunctionLiteralContainer.Destroy;
begin
 BESENFreeAndNil(Literal);
 inherited Destroy;
end;

procedure TBESENFunctionLiteralContainer.Finalize;
begin
 if assigned(Literal) then begin
  Literal.Container:=nil;
  BESENFreeAndNil(Literal);
 end;
 inherited Finalize;
end;

procedure TBESENFunctionLiteralContainer.Mark;
begin
 inherited Mark;
 if (assigned(Literal) and assigned(Literal.Body)) and assigned(Literal.Body.Code) then begin
  TBESENCode(Literal.Body.Code).Mark;
 end;
end;

constructor TBESENASTNode.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntNONE;
 Target:=-1;
 Location.LineNumber:=-1;
 fillchar(CodeGenData,sizeof(TBESENASTNodeCodeGenData),#0);
 TrashNodes:=nil;
 CountTrashNodes:=0;
end;

destructor TBESENASTNode.Destroy;
var i:integer;
begin
 for i:=0 to CountTrashNodes-1 do begin
  BESENFreeAndNil(TrashNodes[i]);
 end;
 SetLength(TrashNodes,0);
 inherited Destroy;
end;

procedure TBESENASTNode.ExecuteCode(const Context:TObject;var AResult:TBESENValue);
begin
 AResult:=BESENUndefinedValue;
end;

constructor TBESENASTNodeExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntEXPRESSION;
end;

destructor TBESENASTNodeExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntLITERAL;
end;

destructor TBESENASTNodeLiteral.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeIdentifier.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntIDENTIFIER;
 Name:='';
 Index:=-1;
 ParameterIndex:=-1;
 ID:=-1;
 IsParameter:=false;
 IsLocal:=false;
 IsReached:=false;
end;

destructor TBESENASTNodeIdentifier.Destroy;
begin
 Name:='';
 inherited Destroy;
end;

constructor TBESENASTNodeVariableDeclaration.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntVARIABLEDECLARATION;
 Identifier:=nil;
 Expression:=nil;
end;

destructor TBESENASTNodeVariableDeclaration.Destroy;
begin
 Identifier.Free;
 Expression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeVariableExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntVARIABLEEXPRESSION;
 Declarations:=nil;
end;

destructor TBESENASTNodeVariableExpression.Destroy;
var i:integer;
begin
 for i:=0 to length(Declarations)-1 do begin
  BESENFreeAndNil(Declarations[i]);
 end;
 SetLength(Declarations,0);
 inherited Destroy;
end;

constructor TBESENASTNodeVariableStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntVARIABLESTATEMENT;
 Declarations:=nil;
end;

destructor TBESENASTNodeVariableStatement.Destroy;
var i:integer;
begin
 for i:=0 to length(Declarations)-1 do begin
  BESENFreeAndNil(Declarations[i]);
 end;
 SetLength(Declarations,0);
 inherited Destroy;
end;

constructor TBESENASTNodeFunctionBody.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntFUNCTIONBODY;
 Variables:=nil;
 Parameters:=nil;
 Functions:=nil;
 Statements:=nil;
 IsFunction:=false;
 IsEmpty:=false;
 EnableLocalsOptimization:=false;
 DisableArgumentsObject:=false;
 IsStrict:=false;
 Code:=TBESENCode.Create(Instance);
 TBESENCode(Code).Body:=self;
end;

destructor TBESENASTNodeFunctionBody.Destroy;
var i:integer;
begin
{$ifdef BESENEmbarcaderoNextGen}
 Code:=nil;
{$else}
 Code.Free;
{$endif}
 for i:=0 to length(Parameters)-1 do begin
  BESENFreeAndNil(Parameters[i]);
 end;
 for i:=0 to length(Functions)-1 do begin
  BESENFreeAndNil(Functions[i]);
 end;
 for i:=0 to length(Statements)-1 do begin
  BESENFreeAndNil(Statements[i]);
 end;
 SetLength(Parameters,0);
 SetLength(Variables,0);
 SetLength(Functions,0);
 SetLength(Statements,0);
 inherited Destroy;
end;

procedure TBESENASTNodeFunctionBody.ExecuteCode(const Context:TObject;var AResult:TBESENValue);
begin
 TBESENCode(Code).Execute(Context,AResult);
end;

constructor TBESENASTNodeFunctionLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntFUNCTIONLITERAL;
 Name:=nil;
 Container:=TBESENFunctionLiteralContainer.Create(Instance);
 Container.Literal:=self;
 Body:=TBESENASTNodeFunctionBody.Create(Instance);
 Body.IsFunction:=true;
 Index:=-1;
end;

destructor TBESENASTNodeFunctionLiteral.Destroy;
begin
 BESENFreeAndNil(Name);
 BESENFreeAndNil(Body);
 if assigned(Container) then begin
  Container.Literal:=nil;
  BESENFreeAndNil(Container);
 end;
 inherited Destroy;
end;

procedure TBESENASTNodeFunctionLiteral.ExecuteCode(const Context:TObject;var AResult:TBESENValue);
begin
 if assigned(Container) then begin
  Container.GarbageCollectorLock;
  try
   Body.ExecuteCode(Context,AResult);
  finally
   Container.GarbageCollectorUnlock;
  end;
 end else begin
  Body.ExecuteCode(Context,AResult);
 end;
end;

constructor TBESENASTNodeStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntSTATEMENT;
end;

destructor TBESENASTNodeStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeFunctionDeclaration.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntFUNCTIONDECLARATION;
 Container:=nil;
end;

destructor TBESENASTNodeFunctionDeclaration.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeFunctionExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntFUNCTIONEXPRESSION;
 Container:=nil;
end;

destructor TBESENASTNodeFunctionExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeExpressionStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntEXPRESSIONSTATEMENT;
 Expression:=nil;
end;

destructor TBESENASTNodeExpressionStatement.Destroy;
begin
 Expression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeEmptyStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntEMPTYSTATEMENT;
end;

destructor TBESENASTNodeEmptyStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBlockStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBLOCKSTATEMENT;
 Statements:=nil;
end;

destructor TBESENASTNodeBlockStatement.Destroy;
var i:integer;
begin
 for i:=0 to length(Statements)-1 do begin
  BESENFreeAndNil(Statements[i]);
 end;
 SetLength(Statements,0);
 inherited Destroy;
end;

constructor TBESENASTNodeDebuggerStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntDEBUGGERSTATEMENT;
end;

destructor TBESENASTNodeDebuggerStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBreakStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBREAKSTATEMENT;
 Identifier:=nil;
end;

destructor TBESENASTNodeBreakStatement.Destroy;
begin
 Identifier.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeContinueStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntCONTINUESTATEMENT;
 Identifier:=nil;
end;

destructor TBESENASTNodeContinueStatement.Destroy;
begin
 Identifier.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeDoStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntDOSTATEMENT;
 Statement:=nil;
 Expression:=nil;
end;

destructor TBESENASTNodeDoStatement.Destroy;
begin
 Statement.Free;
 Expression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeWhileStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntWHILESTATEMENT;
 Expression:=nil;
 Statement:=nil;
end;

destructor TBESENASTNodeWhileStatement.Destroy;
begin
 Expression.Free;
 Statement.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeWithStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntWITHSTATEMENT;
 Expression:=nil;
 Statement:=nil;
end;

destructor TBESENASTNodeWithStatement.Destroy;
begin
 Expression.Free;
 Statement.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeForStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntFORSTATEMENT;
 Initial:=nil;
 Condition:=nil;
 Increment:=nil;
 Statement:=nil;
end;

destructor TBESENASTNodeForStatement.Destroy;
begin
 Initial.Free;
 Condition.Free;
 Increment.Free;
 Statement.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeForInStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntFORINSTATEMENT;
 Variable:=nil;
 Expression:=nil;
 Statement:=nil;
end;

destructor TBESENASTNodeForInStatement.Destroy;
begin
 Variable.Free;
 Expression.Free;
 Statement.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeIfStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntIFSTATEMENT;
 Expression:=nil;
 TrueStatement:=nil;
 FalseStatement:=nil;
end;

destructor TBESENASTNodeIfStatement.Destroy;
begin
 Expression.Free;
 TrueStatement.Free;
 FalseStatement.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeLabelledStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntLABELLEDSTATEMENT;
 Identifiers:=nil;
 Statement:=nil;
end;

destructor TBESENASTNodeLabelledStatement.Destroy;
var i:integer;
begin
 for i:=0 to length(Identifiers)-1 do begin
  BESENFreeAndNil(Identifiers[i]);
 end;
 SetLength(Identifiers,0);
 BESENFreeAndNil(Statement);
 inherited Destroy;
end;

constructor TBESENASTNodeCaseStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntCASESTATEMENT;
 Expression:=nil;
 Statements:=nil;
end;

destructor TBESENASTNodeCaseStatement.Destroy;
var i:integer;
begin
 BESENFreeAndNil(Expression);
 for i:=0 to length(Statements)-1 do begin
  BESENFreeAndNil(Statements[i]);
 end;
 SetLength(Statements,0);
 inherited Destroy;
end;

constructor TBESENASTNodeSwitchStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntSWITCHSTATEMENT;
 Expression:=nil;
 CaseStatements:=nil;
end;

destructor TBESENASTNodeSwitchStatement.Destroy;
var i:integer;
begin
 Expression.Free;
 for i:=0 to length(CaseStatements)-1 do begin
  BESENFreeAndNil(CaseStatements[i]);
 end;
 SetLength(CaseStatements,0);
 inherited Destroy;
end;

constructor TBESENASTNodeThrowStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntTHROWSTATEMENT;
 Expression:=nil;
end;

destructor TBESENASTNodeThrowStatement.Destroy;
begin
 Expression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeTryStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntTRYSTATEMENT;
 TryBlock:=nil;
 CatchIdentifier:=nil;
 CatchBlock:=nil;
 FinallyBlock:=nil;
end;

destructor TBESENASTNodeTryStatement.Destroy;
begin
 TryBlock.Free;
 CatchIdentifier.Free;
 CatchBlock.Free;
 FinallyBlock.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeArrayLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntARRAYLITERAL;
 Elements:=nil;
end;

destructor TBESENASTNodeArrayLiteral.Destroy;
var i:integer;
begin
 for i:=0 to length(Elements)-1 do begin
  BESENFreeAndNil(Elements[i]);
 end;
 SetLength(Elements,0);
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYEXPRESSION;
 LeftExpression:=nil;
 RightExpression:=nil;
end;

destructor TBESENASTNodeBinaryExpression.Destroy;
begin
 LeftExpression.Free;
 RightExpression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTEXPRESSION;
end;

destructor TBESENASTNodeAssignmentExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentOperatorExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTOPERATOREXPRESSION;
end;

destructor TBESENASTNodeAssignmentOperatorExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentMultiplyExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTMULTIPLYEXPRESSION;
end;

destructor TBESENASTNodeAssignmentMultiplyExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentDivideExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTDIVIDEEXPRESSION;
end;

destructor TBESENASTNodeAssignmentDivideExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentModuloExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTMODULOEXPRESSION;
end;

destructor TBESENASTNodeAssignmentModuloExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentPlusExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTPLUSEXPRESSION;
end;

destructor TBESENASTNodeAssignmentPlusExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentMinusExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTMINUSEXPRESSION;
end;

destructor TBESENASTNodeAssignmentMinusExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentShiftLeftExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTSHIFTLEFTEXPRESSION;
end;

destructor TBESENASTNodeAssignmentShiftLeftExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentShiftRightExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTSHIFTRIGHTEXPRESSION;
end;

destructor TBESENASTNodeAssignmentShiftRightExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentShiftRightUnsignedExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTSHIFTRIGHTUNSIGNEDEXPRESSION;
end;

destructor TBESENASTNodeAssignmentShiftRightUnsignedExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentBitwiseAndExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTBITWISEANDEXPRESSION;
end;

destructor TBESENASTNodeAssignmentBitwiseAndExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentBitwiseXorExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTBITWISEXOREXPRESSION;
end;

destructor TBESENASTNodeAssignmentBitwiseXorExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeAssignmentBitwiseOrExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntASSIGNMENTBITWISEOREXPRESSION;
end;

destructor TBESENASTNodeAssignmentBitwiseOrExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryOperatorExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYOPERATOREXPRESSION;
end;

destructor TBESENASTNodeBinaryOperatorExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryCommaExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYCOMMAEXPRESSION;
end;

destructor TBESENASTNodeBinaryCommaExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryDivideExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYDIVIDEEXPRESSION;
end;

destructor TBESENASTNodeBinaryDivideExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryModuloExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYMODULOEXPRESSION;
end;

destructor TBESENASTNodeBinaryModuloExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryMultiplyExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYMULTIPLYEXPRESSION;
end;

destructor TBESENASTNodeBinaryMultiplyExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryPlusExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYPLUSEXPRESSION;
end;

destructor TBESENASTNodeBinaryPlusExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryMinusExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYMINUSEXPRESSION;
end;

destructor TBESENASTNodeBinaryMinusExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryShiftLeftExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYSHIFTLEFTEXPRESSION;
end;

destructor TBESENASTNodeBinaryShiftLeftExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryShiftRightExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYSHIFTRIGHTEXPRESSION;
end;

destructor TBESENASTNodeBinaryShiftRightExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryShiftRightUnsignedExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYSHIFTRIGHTUNSIGNEDEXPRESSION;
end;

destructor TBESENASTNodeBinaryShiftRightUnsignedExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryGreaterThanExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYGREATERTHANEXPRESSION;
end;

destructor TBESENASTNodeBinaryGreaterThanExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryGreaterThanOrEqualExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYGREATERTHANOREQUALEXPRESSION;
end;

destructor TBESENASTNodeBinaryGreaterThanOrEqualExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryLessThanExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYLESSTHANEXPRESSION;
end;

destructor TBESENASTNodeBinaryLessThanExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryLessThanOrEqualExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYLESSTHANOREQUALEXPRESSION;
end;

destructor TBESENASTNodeBinaryLessThanOrEqualExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryInstanceOfExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYINSTANCEOFEXPRESSION;
end;

destructor TBESENASTNodeBinaryInstanceOfExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryInExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYINEXPRESSION;
end;

destructor TBESENASTNodeBinaryInExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryEqualEqualExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYEQUALEQUALEXPRESSION;
end;

destructor TBESENASTNodeBinaryEqualEqualExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryEqualEqualEqualExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYEQUALEQUALEQUALEXPRESSION;
end;

destructor TBESENASTNodeBinaryEqualEqualEqualExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryNotEqualExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYNOTEQUALEXPRESSION;
end;

destructor TBESENASTNodeBinaryNotEqualExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryNotEqualEqualExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYNOTEQUALEQUALEXPRESSION;
end;

destructor TBESENASTNodeBinaryNotEqualEqualExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryBitwiseAndExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYBITWISEANDEXPRESSION;
end;

destructor TBESENASTNodeBinaryBitwiseAndExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryBitwiseXorExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYBITWISEXOREXPRESSION;
end;

destructor TBESENASTNodeBinaryBitwiseXorExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBinaryBitwiseOrExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBINARYBITWISEOREXPRESSION;
end;

destructor TBESENASTNodeBinaryBitwiseOrExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeBooleanLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntBOOLEANLITERAL;
 Value:=false;
end;

destructor TBESENASTNodeBooleanLiteral.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeCallExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntCALLEXPRESSION;
 TheFunction:=nil;
 Arguments:=nil;
end;

destructor TBESENASTNodeCallExpression.Destroy;
var i:integer;
begin
 TheFunction.Free;
 for i:=0 to length(Arguments)-1 do begin
  BESENFreeAndNil(Arguments[i]);
 end;
 SetLength(Arguments,0);
 inherited Destroy;
end;

constructor TBESENASTNodeNewExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntNEWEXPRESSION;
 TheFunction:=nil;
 Arguments:=nil;
end;

destructor TBESENASTNodeNewExpression.Destroy;
var i:integer;
begin
 TheFunction.Free;
 for i:=0 to length(Arguments)-1 do begin
  BESENFreeAndNil(Arguments[i]);
 end;
 SetLength(Arguments,0);
 inherited Destroy;
end;

constructor TBESENASTNodeConditionalExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntCONDITIONALEXPRESSION;
 Expression:=nil;
 TrueExpression:=nil;
 FalseExpression:=nil;
end;

destructor TBESENASTNodeConditionalExpression.Destroy;
begin
 BESENFreeAndNil(Expression);
 BESENFreeAndNil(TrueExpression);
 BESENFreeAndNil(FalseExpression);
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYEXPRESSION;
 SubExpression:=nil;
end;

destructor TBESENASTNodeUnaryExpression.Destroy;
begin
 SubExpression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryOperatorExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYOPERATOREXPRESSION;
end;

destructor TBESENASTNodeUnaryOperatorExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryPlusExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYPLUSEXPRESSION;
end;

destructor TBESENASTNodeUnaryPlusExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryMinusExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYMINUSEXPRESSION;
end;

destructor TBESENASTNodeUnaryMinusExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryBitwiseNotExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYBITWISENOTEXPRESSION;
end;

destructor TBESENASTNodeUnaryBitwiseNotExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryLogicalNotExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYLOGICALNOTEXPRESSION;
end;

destructor TBESENASTNodeUnaryLogicalNotExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryVoidExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYVOIDEXPRESSION;
end;

destructor TBESENASTNodeUnaryVoidExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeUnaryTypeOfExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntUNARYTYPEOFEXPRESSION;
end;

destructor TBESENASTNodeUnaryTypeOfExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodePropertyExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntPROPERTYEXPRESSION;
 Dot:=false;
end;

destructor TBESENASTNodePropertyExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeLogicalAndExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntLOGICALANDEXPRESSION;
end;

destructor TBESENASTNodeLogicalAndExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeLogicalOrExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntLOGICALOREXPRESSION;
end;

destructor TBESENASTNodeLogicalOrExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeDeleteExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntDELETEEXPRESSION;
end;

destructor TBESENASTNodeDeleteExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodePostfixIncExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntPOSTFIXINCEXPRESSION;
end;

destructor TBESENASTNodePostfixIncExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodePostfixDecExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntPOSTFIXDECEXPRESSION;
end;

destructor TBESENASTNodePostfixDecExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodePrefixIncExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntPREFIXINCEXPRESSION;
end;

destructor TBESENASTNodePrefixIncExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodePrefixDecExpression.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntPREFIXDECEXPRESSION;
end;

destructor TBESENASTNodePrefixDecExpression.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeNullLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntNULLLITERAL;
end;

destructor TBESENASTNodeNullLiteral.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeNumberLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntNUMBERLITERAL;
 Value:=0;
 Index:=0;
end;

destructor TBESENASTNodeNumberLiteral.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeRegExpLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntREGEXPLITERAL;
 Source:='';
 Flags:='';
end;

destructor TBESENASTNodeRegExpLiteral.Destroy;
begin
 Source:='';
 Flags:='';
 inherited Destroy;
end;

constructor TBESENASTNodeStringLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntSTRINGLITERAL;
 Value:='';
end;

destructor TBESENASTNodeStringLiteral.Destroy;
begin
 Value:='';
 inherited Destroy;
end;

constructor TBESENASTNodeThisLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntTHISLITERAL;
end;

destructor TBESENASTNodeThisLiteral.Destroy;
begin
 inherited Destroy;
end;

constructor TBESENASTNodeObjectLiteralProperty.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntOBJECTLITERALPROPERTY;
 PropertyType:=banolptNONE;
 PropertyAccessorType:=banolpatNONE;
 Name:='';
 Value:=nil;
 Container:=nil;
end;

destructor TBESENASTNodeObjectLiteralProperty.Destroy;
begin
 BESENFreeAndNil(Value);
 Name:='';
 inherited Destroy;
end;

constructor TBESENASTNodeObjectLiteral.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntOBJECTLITERAL;
 Properties:=nil;
end;

destructor TBESENASTNodeObjectLiteral.Destroy;
var i:integer;
begin
 for i:=0 to length(Properties)-1 do begin
  BESENFreeAndNil(Properties[i]);
 end;
 SetLength(Properties,0);
 inherited Destroy;
end;

constructor TBESENASTNodeReturnStatement.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntRETURNSTATEMENT;
 Expression:=nil;
end;

destructor TBESENASTNodeReturnStatement.Destroy;
begin
 Expression.Free;
 inherited Destroy;
end;

constructor TBESENASTNodeProgram.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 NodeType:=bntPROGRAM;
 Body:=TBESENASTNodeFunctionBody.Create(Instance);
 Body.IsFunction:=false;
end;

destructor TBESENASTNodeProgram.Destroy;
begin
 if assigned(Instance) then begin
  TBESEN(Instance).RemoveProgramNode(self);
 end;
 BESENFreeAndNil(Body);
 inherited Destroy;
end;

procedure TBESENASTNodeProgram.ExecuteCode(const Context:TObject;var AResult:TBESENValue);
begin
 Body.ExecuteCode(Context,AResult);
end;

end.
