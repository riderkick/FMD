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
unit BESENCodeContext;
{$i BESEN.inc}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}{$ifdef unix}dl,BaseUnix,Unix,
     UnixType,{$endif}SysUtils,Classes,Math,BESENConstants,BESENTypes,BESENValue,
     BESENBaseObject,BESENCollectorObject,BESENObject,BESENLexicalEnvironment,
     BESENStringList,BESENContext,BESENCode,
     BESENObjectPropertyDescriptor;

type TBESENCodeContextBlockType=(bccbtENUM,bccbtWITH,bccbtCATCH,bccbtCATCH2,bccbtFINALLY,bccbtFINALLY2);

     PBESENCodeContextBlock=^TBESENCodeContextBlock;

     TBESENCodeContextBlock=record
      BlockType:TBESENCodeContextBlockType;
      OldEnumBlock:PBESENCodeContextBlock;
      PropertyEnumerator:TBESENObjectPropertyEnumerator;
      OldPropertyEnumerator:TBESENObjectPropertyEnumerator;
      LastTryBlock:longword;
      Resume:longword;
      Handler:longword;
      Ident:TBESENString;
      Obj:TBESENObject;
      LexicalEnvironment:TBESENLexicalEnvironment;
      Done:longbool;
      Raised:longbool;
      Value:TBESENValue;
     end;

     TBESENCodeContextBlocks=array of TBESENCodeContextBlock;

     TBESENCodeContextOpcode=procedure(Operands:PBESENINT32Array) of object; {$ifdef UseRegister}register;{$endif}

     TBESENCodeContextOpcodePointers=array[byte] of pointer;

     TBESENCodeContextOpcodeArgs=array[byte] of longbool;

     TBESENCodeContextGetRefProc=procedure(const ARef:TBESENValue;var AResult:TBESENValue) of object;

     TBESENCodeContextGetRefProcs=array[byte] of TBESENCodeContextGetRefProc;

     TBESENCodeContextPutRefProc=procedure(const ARef,AValue:TBESENValue) of object;

     TBESENCodeContextPutRefProcs=array[byte] of TBESENCodeContextPutRefProc;

     TBESENCodeContextParameterIndices=array of integer;

     TBESENCodeContext=class(TBESENBaseObject)
      private
       LexicalEnvironment:TBESENLexicalEnvironment;
       OldLexicalEnvironment:TBESENLexicalEnvironment;
       GetRefProcs:TBESENCodeContextGetRefProcs;
       PutRefProcs:TBESENCodeContextPutRefProcs;
       LookupNames:PBESENStringArray;
       procedure OpSTOP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEW(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCALL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpEND(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpVREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNOP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPY(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNSEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpAREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTHROW(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTHIS(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpOBJECT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpARRAY(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpREGEXP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLOOKUP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpDELETE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTYPEOF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTOOBJECT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTONUMBER(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTOBOOLEAN(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTOSTRING(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTOPRIMITIVE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEG(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpINV(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNOT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpMUL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpDIV(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpMOD(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpADD(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpADDNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSUB(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSHL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSHR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpUSHR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpINSTANCEOF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpIN(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBAND(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBXOR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBOR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSWITH(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSCATCH(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpENDF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpJMP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpJZ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpJNZ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpJNULL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLOOPENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSTRYC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSTRYF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERALUNDEF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERALNULL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERALBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERALNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERALSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLITERALOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpFUNC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLINE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSTRICT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSTRICTCHECKREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpDEBUGGER(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCHECKOBJECTCOERCIBLE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTOBJVALUE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTOBJGET(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTOBJSET(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpINC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpDEC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPYBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPYNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPYSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPYOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPYREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpCOPYLOCAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUEREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUEREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALFAST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALFAST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALINDEX(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALINDEX(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALINDEXBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALINDEXBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALINDEXNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALINDEXNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALINDEXSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALINDEXSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGETVALUELOCALINDEXOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpPUTVALUELOCALINDEXOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLOOPINITCOUNT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLOOPADDCOUNT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTRACE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLTBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGTBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLEBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGEBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpEQBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEQBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLTNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGTNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpEQNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEQNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLTSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGTSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLESTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGESTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpEQSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEQSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSHLBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSHRBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBANDBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBXORBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBORBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSHLNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSHRNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpUSHRNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBANDNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBXORNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpBORNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETCUNDEF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETCNULL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETCBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETCNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETCSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpSETCOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTRACENEW(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpTRACECALL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLTNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGTNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpLENUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpGENUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpEQNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpNEQNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpJZERO(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
       procedure OpJNZERO(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
      protected
       function ConstructError(ConstructorObject:TBESENObject;const Msg:TBESENString;const Name:TBESENString=''):TBESENValue;
       function Trace(const TraceType:TBESENTraceType):boolean;
       procedure GetIdentifierReference(Lex:TBESENLexicalEnvironment;const Name:TBESENString;const IsStrict:TBESENBoolean;var AResult:TBESENValue);
       procedure GetPrimitive(const ARef:TBESENValue;var AResult:TBESENValue);
       procedure GetObject(const ARef:TBESENValue;var AResult:TBESENValue);
       procedure GetEnvRec(const ARef:TBESENValue;var AResult:TBESENValue);
       procedure GetUndefined(const ARef:TBESENValue;var AResult:TBESENValue);
       procedure GetValue(const AValue:TBESENValue;var AResult:TBESENValue);
       procedure PutPrimitive(const ARef,AValue:TBESENValue);
       procedure PutObject(const ARef,AValue:TBESENValue);
       procedure PutEnvRec(const ARef,AValue:TBESENValue);
       procedure PutUndefined(const ARef,AValue:TBESENValue);
       procedure PutValue(const ARef,AValue:TBESENValue);
       function AbstractRelational(eva,evb:PBESENValue;var BoolRes:TBESENBoolean):TBESENBoolean;
       function AbstractRelationalNumber(eva,evb:PBESENValue;var BoolRes:TBESENBoolean):TBESENBoolean;
       procedure Throw(const ThrowValue:TBESENValue);
       function ExecuteByteCode:TBESENBoolean; {$ifdef UseRegister}register;{$endif}
       function ExecuteCode:TBESENBoolean;
       procedure ExecuteTryBlockLevel;
      public
       NextCodeContext:TBESENCodeContext;
       Code:TBESENCode;
       Context:TBESENContext;
       RegisterValues:TBESENValues;
       PC:TBESENUINT32;
       Blocks:TBESENCodeContextBlocks;
       Block:PBESENCodeContextBlock;
       LoopCounters:TBESENUINT32s;
       ParamArgs:TBESENValuePointers;
       PropertyEnumerator:TBESENObjectPropertyEnumerator;
       EnumBlock:PBESENCodeContextBlock;
       Temp,AnotherTemp:TBESENValue;
       BlockLevel,NewBlockLevel:integer;
       Obj:TBESENObject;
       DontEnum,Running,BlockRunning:TBESENBoolean;
       Str:TBESENString;
       va,vb,vaa,vbb:TBESENValue;
       Descriptor,Descriptor2:TBESENObjectPropertyDescriptor;
       BaseValue:TBESENValue;
       CallThisArg:TBESENValue;
       ResultValue:TBESENValue;
       constructor Create(AInstance:TObject;ACode:TBESENCode); overload;
       destructor Destroy; override;
       procedure Execute(const AContext:TBESENContext;var AResult:TBESENValue);
     end;

var BESENCodeContextOpcodes:TBESENCodeContextOpcodePointers;

function BESENAdjustPolymorphicInlineCachePosition(Position,Index:TBESENUINT32):TBESENUINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}

implementation

uses BESEN,BESENEnvironmentRecord,BESENHashUtils,BESENUtils,BESENArrayUtils,
     BESENObjectEnvironmentRecord,BESENDeclarativeEnvironmentRecord,BESENStringUtils,
     BESENGarbageCollector,BESENASTNodes,BESENNumberUtils,BESENOpcodes,BESENErrors,
     BESENObjectDeclaredFunction{$ifdef HasJIT}{$ifdef cpu386},BESENCodeJITx86{$endif}{$ifdef cpuamd64},BESENCodeJITx64{$endif}{$endif};

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

function BESENAdjustPolymorphicInlineCachePosition(Position,Index:TBESENUINT32):TBESENUINT32; {$ifdef UseRegister}register;{$endif}{$ifdef caninline}inline;{$endif}
begin
 result:=((Position shr (Index shl 2)) and $f) or ((Position and ((1 shl (Index shl 2))-1)) shl 4) or ((Position and not ((1 shl ((Index+1) shl 2))-1)) and TBESENUINT32(0-ord(Index<7)));
end;

constructor TBESENCodeContext.Create(AInstance:TObject;ACode:TBESENCode);
var i:integer;
begin
 inherited Create(AInstance);
 Context:=nil;
 Code:=ACode;
 NextCodeContext:=nil;
 Blocks:=nil;
 Block:=nil;
 PropertyEnumerator:=nil;
 EnumBlock:=nil;
 Temp:=BESENUndefinedValue;
 AnotherTemp:=BESENUndefinedValue;
 BlockLevel:=0;
 NewBlockLevel:=0;
 PC:=0;
 Running:=true;
 BlockRunning:=true;
 RegisterValues:=nil;
 LoopCounters:=nil;
 ParamArgs:=nil;

 Str:='';
 vaa:=BESENUndefinedValue;
 vbb:=BESENUndefinedValue;
 Descriptor:=BESENUndefinedPropertyDescriptor;
 Descriptor2:=BESENUndefinedPropertyDescriptor;

 CallThisArg:=BESENUndefinedValue;

 GetRefProcs[brbvtUNDEFINED]:=GetUndefined;
 GetRefProcs[brbvtBOOLEAN]:=GetPrimitive;
 GetRefProcs[brbvtNUMBER]:=GetPrimitive;
 GetRefProcs[brbvtSTRING]:=GetPrimitive;
 GetRefProcs[brbvtOBJECT]:=GetObject;
 GetRefProcs[brbvtENVREC]:=GetEnvRec;

 PutRefProcs[brbvtUNDEFINED]:=PutUndefined;
 PutRefProcs[brbvtBOOLEAN]:=PutPrimitive;
 PutRefProcs[brbvtNUMBER]:=PutPrimitive;
 PutRefProcs[brbvtSTRING]:=PutPrimitive;
 PutRefProcs[brbvtOBJECT]:=PutObject;
 PutRefProcs[brbvtENVREC]:=PutEnvRec;

 SetLength(RegisterValues,Code.MaxRegisters);
 if length(RegisterValues)>0 then begin
  FillChar(RegisterValues[0],length(RegisterValues)*sizeof(TBESENValue),#0);
 end;

 Temp:=BESENEmptyValue;
 SetLength(Blocks,Code.MaxBlock+1);
 SetLength(LoopCounters,Code.MaxLoop+1);
 SetLength(ParamArgs,Code.MaxParamArgs+1);
 for i:=0 to length(ParamArgs)-1 do begin
  ParamArgs[i]:=nil;
 end;
 if length(Blocks)>0 then begin
  fillchar(Blocks[0],length(Blocks)*sizeof(TBESENCodeContextBlock),#0);
 end;
end;

destructor TBESENCodeContext.Destroy;
var i:integer;
begin
 SetLength(LoopCounters,0);
 SetLength(ParamArgs,0);
 SetLength(RegisterValues,0);
 for i:=0 to length(Blocks)-1 do begin
  Blocks[i].Ident:='';
  Blocks[i].Value.Str:='';
  Blocks[i].Value.ReferenceBase.Str:='';
 end;
 SetLength(Blocks,0);
 Str:='';
 inherited Destroy;
end;

function TBESENCodeContext.ConstructError(ConstructorObject:TBESENObject;const Msg:TBESENString;const Name:TBESENString=''):TBESENValue;
var v:TBESENValue;
    vv:array[0..0] of PBESENValue;
begin
 v.ValueType:=bvtSTRING;
 v.Str:=Msg;
 vv[0]:=@v;
 ConstructorObject.Construct(BESENUndefinedValue,@vv,1,result);
 TBESEN(Instance).GarbageCollector.Add(TBESENObject(result.Obj));
 if length(Name)>0 then begin
  TBESENObject(result.Obj).PutEx('name',BESENStringValue(Name),true,Descriptor,Descriptor2,AnotherTemp);
 end;
end;

function TBESENCodeContext.Trace(const TraceType:TBESENTraceType):boolean;
begin
 result:=true;
 if assigned(TBESEN(Instance).TraceHook) then begin
  if not TBESEN(Instance).TraceHook(TBESEN(Instance),Context,Code.Body,PC,TraceType) then begin
   Running:=false;
   BlockRunning:=false;
   result:=false;
  end;
 end;
end;

procedure TBESENCodeContext.GetIdentifierReference(Lex:TBESENLexicalEnvironment;const Name:TBESENString;const IsStrict:TBESENBoolean;var AResult:TBESENValue);
var EnvRec:TBESENEnvironmentRecord;
begin
 AResult.ValueType:=bvtREFERENCE;
 AResult.ReferenceBase.ValueType:=brbvtUNDEFINED;
 AResult.Str:=Name;
 AResult.ReferenceIsStrict:=IsStrict;
 AResult.ReferenceHash:=BESENHashKey(Name);
 AResult.ReferenceIndex:=-1;
 AResult.ReferenceID:=-1;
 while assigned(Lex) do begin
  EnvRec:=Lex.EnvironmentRecord;
  if assigned(EnvRec) and EnvRec.HasBindingEx(Name,Descriptor) then begin
   AResult.ReferenceBase.ValueType:=brbvtENVREC;
   AResult.ReferenceBase.EnvRec:=EnvRec;
   break;
  end;
  Lex:=Lex.Outer;
 end;
end;

procedure TBESENCodeContext.GetPrimitive(const ARef:TBESENValue;var AResult:TBESENValue);
var O:TBESENObject;
    Hash:TBESENHash;
begin
 AResult.ValueType:=bvtUNDEFINED;
 BESENReferenceBaseValueToValue(ARef.ReferenceBase,BaseValue);
 if BaseValue.ValueType=bvtOBJECT then begin
  O:=TBESENObject(BaseValue.Obj);
 end else begin
  O:=TBESEN(Instance).ToObj(BaseValue);
  TBESEN(Instance).GarbageCollector.Add(O);
 end;
 if not assigned(O) then begin
  BESENThrowNotDefined(ARef);
 end;
 O.GarbageCollectorLock;
 try
  if ARef.ReferenceID<0 then begin
   if ARef.ReferenceIndex<0 then begin
    Str:=ARef.Str;
    Hash:=ARef.ReferenceHash;
   end else begin
    Str:=BESENArrayIndexToStr(TBESENUINT32(ARef.ReferenceIndex));
    Hash:=BESENHashKey(Str);
   end;
  end else begin
   Str:=TBESEN(Instance).KeyIDManager.List[ARef.ReferenceID];
   Hash:=BESENHashKey(Str);
  end;
  if O.GetProperty(Str,Descriptor,Hash) then begin
   if Descriptor.Presents<>[] then begin
    if ([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[] then begin
     if boppVALUE in Descriptor.Presents then begin
      BESENCopyValue(AResult,Descriptor.Value);
     end;
    end else if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
     if (boppGETTER in Descriptor.Presents) and assigned(Descriptor.Getter) then begin
      TBESEN(Instance).ObjectCall(TBESENObject(Descriptor.Getter),BaseValue,nil,0,AResult);
     end;
    end;
   end;
  end;
 finally
  O.GarbageCollectorUnlock;
 end;
end;

procedure TBESENCodeContext.GetObject(const ARef:TBESENValue;var AResult:TBESENValue);
begin
 if not assigned(ARef.ReferenceBase.Obj) then begin
  BESENThrowNotDefined(ARef);
 end;
 if ARef.ReferenceID<0 then begin
  if ARef.ReferenceIndex<0 then begin
   TBESENObject(ARef.ReferenceBase.Obj).GetEx(ARef.Str,AResult,Descriptor,TBESENObject(ARef.ReferenceBase.Obj),ARef.ReferenceHash);
  end else begin
   TBESENObject(ARef.ReferenceBase.Obj).GetArrayIndex(TBESENUINT32(ARef.ReferenceIndex),AResult,TBESENObject(ARef.ReferenceBase.Obj));
  end;
 end else begin
  TBESENObject(ARef.ReferenceBase.Obj).GetIndex(ARef.ReferenceIndex,ARef.ReferenceID,AResult,TBESENObject(ARef.ReferenceBase.Obj));
 end;
end;

procedure TBESENCodeContext.GetEnvRec(const ARef:TBESENValue;var AResult:TBESENValue);
begin
 if ARef.ReferenceID<0 then begin
  if ARef.ReferenceIndex<0 then begin
   TBESENEnvironmentRecord(ARef.ReferenceBase.EnvRec).GetBindingValueEx(ARef.Str,ARef.ReferenceIsStrict,AResult,Descriptor,ARef.ReferenceHash);
  end else begin
   TBESENEnvironmentRecord(ARef.ReferenceBase.EnvRec).GetArrayIndexValue(TBESENUINT32(ARef.ReferenceIndex),ARef.ReferenceIsStrict,AResult);
  end;
 end else begin
  TBESENEnvironmentRecord(ARef.ReferenceBase.EnvRec).GetIndexValue(ARef.ReferenceIndex,ARef.ReferenceID,ARef.ReferenceIsStrict,AResult);
 end;
end;

procedure TBESENCodeContext.GetUndefined(const ARef:TBESENValue;var AResult:TBESENValue);
begin
 BESENThrowNotDefined(ARef);
end;

procedure TBESENCodeContext.GetValue(const AValue:TBESENValue;var AResult:TBESENValue);
begin
 case AValue.ValueType of
  bvtREFERENCE:begin
   GetRefProcs[AValue.ReferenceBase.ValueType](AValue,AResult);
  end;
  bvtLOCAL:begin
   Context.VariableEnvironment.EnvironmentRecord.GetIndexValue(AValue.LocalIndex,-1,TBESEN(Instance).IsStrict,AResult);
  end;
  else begin
   if @AResult<>@AValue then begin
    BESENCopyValue(AResult,AValue);
   end;
  end;
 end;
end;

procedure TBESENCodeContext.PutPrimitive(const ARef,AValue:TBESENValue);
var O:TBESENObject;
    ValuePointers:array[0..0] of PBESENValue;
    Done:boolean;
    Hash:TBESENHash;
begin
 BESENReferenceBaseValueToValue(ARef.ReferenceBase,BaseValue);
 if BaseValue.ValueType=bvtOBJECT then begin
  O:=TBESENObject(BaseValue.Obj);
 end else begin
  O:=TBESEN(Instance).ToObj(BaseValue);
  TBESEN(Instance).GarbageCollector.Add(O);
 end;
 if not assigned(O) then begin
  BESENThrowNotAccessable(ARef);
 end;
 O.GarbageCollectorLock;
 try
  if ARef.ReferenceID<0 then begin
   if ARef.ReferenceIndex<0 then begin
    Str:=ARef.Str;
    Hash:=ARef.ReferenceHash;
   end else begin
    Str:=BESENArrayIndexToStr(TBESENUINT32(ARef.ReferenceIndex));
    Hash:=BESENHashKey(Str);
   end;
  end else begin
   Str:=TBESEN(Instance).KeyIDManager.List[ARef.ReferenceID];
   Hash:=BESENHashKey(Str);
  end;
  Done:=false;
  if not O.CanPut(Str,Descriptor,Descriptor2,Hash) then begin
   if ARef.ReferenceIsStrict then begin
    BESENThrowNotAccessable(ARef);
   end else begin
    Done:=true;
   end;
  end;
  if not Done then begin
   if ([boppVALUE,boppWRITABLE]*Descriptor2.Presents)<>[] then begin
    if ARef.ReferenceIsStrict then begin
     BESENThrowNotAccessable(ARef);
    end else begin
     Done:=true;
    end;
   end;
   if not Done then begin
    if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
     if (boppSETTER in Descriptor.Presents) and assigned(Descriptor.Setter) then begin
      ValuePointers[0]:=@AValue;
      TBESEN(Instance).ObjectCall(TBESENObject(Descriptor.Setter),BaseValue,@ValuePointers,1,AnotherTemp);
     end else begin
      if ARef.ReferenceIsStrict then begin
       BESENThrowNotAccessable(ARef);
      end;
     end;
    end else begin
     if ARef.ReferenceIsStrict then begin
      BESENThrowNotAccessable(ARef);
     end;
    end;
   end;
  end;
 finally
  O.GarbageCollectorUnlock;
 end;
end;

procedure TBESENCodeContext.PutObject(const ARef,AValue:TBESENValue);
begin
 if not assigned(ARef.ReferenceBase.Obj) then begin
  BESENThrowNotAccessable(ARef);
 end;
 if ARef.ReferenceID<0 then begin
  if ARef.ReferenceIndex<0 then begin
   TBESENObject(ARef.ReferenceBase.Obj).PutEx(ARef.Str,AValue,ARef.ReferenceIsStrict,Descriptor,Descriptor2,AnotherTemp,ARef.ReferenceHash);
  end else begin
   TBESENObject(ARef.ReferenceBase.Obj).PutArrayIndex(TBESENUINT32(ARef.ReferenceIndex),AValue,ARef.ReferenceIsStrict);
  end;
 end else begin
  TBESENObject(ARef.ReferenceBase.Obj).PutIndex(ARef.ReferenceIndex,ARef.ReferenceID,AValue,ARef.ReferenceIsStrict);
 end;
end;

procedure TBESENCodeContext.PutEnvRec(const ARef,AValue:TBESENValue);
begin
 if ARef.ReferenceID<0 then begin
  if ARef.ReferenceIndex<0 then begin
   TBESENEnvironmentRecord(ARef.ReferenceBase.EnvRec).SetMutableBindingEx(ARef.Str,AValue,ARef.ReferenceIsStrict,Descriptor,Descriptor2,AnotherTemp,ARef.ReferenceHash);
  end else begin
   TBESENEnvironmentRecord(ARef.ReferenceBase.EnvRec).SetArrayIndexValue(TBESENUINT32(ARef.ReferenceIndex),AValue,ARef.ReferenceIsStrict);
  end;
 end else begin
  TBESENEnvironmentRecord(ARef.ReferenceBase.EnvRec).SetIndexValue(ARef.ReferenceIndex,ARef.ReferenceID,AValue,ARef.ReferenceIsStrict);
 end;
end;

procedure TBESENCodeContext.PutUndefined(const ARef,AValue:TBESENValue);
begin
 if ARef.ReferenceIsStrict then begin
  BESENThrowNotAccessable(ARef);
 end else begin
  TBESEN(Instance).ObjectGlobal.PutEx(ARef.Str,AValue,false,Descriptor,Descriptor2,AnotherTemp);
 end;
end;

procedure TBESENCodeContext.PutValue(const ARef,AValue:TBESENValue);
begin
 if (AValue.ValueType=bvtOBJECT) and assigned(AValue.Obj) then begin
  TBESENObject(AValue.Obj).GarbageCollectorWriteBarrier;
 end;
 case ARef.ValueType of
  bvtREFERENCE:begin
   PutRefProcs[ARef.ReferenceBase.ValueType](ARef,AValue);
  end;
  bvtLOCAL:begin
   Context.VariableEnvironment.EnvironmentRecord.SetIndexValue(ARef.LocalIndex,-1,AValue,TBESEN(Instance).IsStrict);
  end;
  else begin
   BESENThrowReference;
  end;
 end;
end;

function TBESENCodeContext.AbstractRelational(eva,evb:PBESENValue;var BoolRes:TBESENBoolean):TBESENBoolean; 
{$ifndef UseSafeOperations}
{$ifdef cpu386}
var a,b:TBESENNumber;
    br:TBESENBoolean;
{$else}
var Item:PBESENNumberCodeAbstractRelationalLookupTableItem;
{$endif}
{$endif}
begin
 result:=true;
 if eva^.ValueType=bvtOBJECT then begin
  TBESEN(Instance).ToPrimitiveValue(eva^,TBESEN(Instance).ObjectNumberConstructorValue,vaa);
  eva:=@vaa;
 end;
 if evb^.ValueType=bvtOBJECT then begin
  TBESEN(Instance).ToPrimitiveValue(evb^,TBESEN(Instance).ObjectNumberConstructorValue,vbb);
  evb:=@vbb;
 end;
 if (eva^.ValueType=bvtSTRING) and (evb^.ValueType=bvtSTRING) then begin
  BoolRes:=eva^.Str<evb^.Str;
 end else begin
  if eva^.ValueType<>bvtNUMBER then begin
   TBESEN(Instance).ToNumberValue(eva^,va);
   eva:=@va;
  end;
  if evb^.ValueType<>bvtNUMBER then begin
   TBESEN(Instance).ToNumberValue(evb^,vb);
   evb:=@vb;
  end;
{$ifdef UseSafeOperations}
  if BESENIsNaN(eva^.Num) or BESENIsNaN(evb^.Num) then begin
   result:=false;
   BoolRes:=false;
  end else if BESENIsSameValue(eva^.Num,evb^.Num) then begin
   BoolRes:=false;
  end else if (BESENIsZero(eva^.Num) and BESENIsZero(evb^.Num)) and (BESENIsNegative(eva^.Num)<>BESENIsNegative(evb^.Num)) then begin
   BoolRes:=false;
  end else if BESENIsPosInfinite(eva^.Num) then begin
   BoolRes:=false;
  end else if BESENIsPosInfinite(evb^.Num) then begin
   BoolRes:=true;
  end else if BESENIsNegInfinite(evb^.Num) then begin
   BoolRes:=false;
  end else if BESENIsNegInfinite(eva^.Num) then begin
   BoolRes:=true;
  end else begin
   BoolRes:=eva^.Num<evb^.Num;
  end;
{$else}
{$ifdef cpu386}
  a:=eva^.Num;
  b:=evb^.Num;
  asm
   fld qword ptr [a]
   fcomp qword ptr [b]
   fstsw ax
   sahf
   setb al
   setnp cl
   and al,cl
   neg al
   sbb eax,eax
   mov dword ptr br,eax
   neg cl
   sbb ecx,ecx
   mov dword ptr result,ecx
  end {$ifdef fpc}['eax','ecx']{$endif};
  BoolRes:=br;
{$else}
  Item:=@BESENNumberCodeAbstractRelationalLookupTable[((BESENNumberCodeFlags(eva^.Num) shl 4) or BESENNumberCodeFlags(evb^.Num)) and $ff];
  result:=Item^.IsNotUndefined;
  BoolRes:=result and (Item^.ResultBooleanValue or (Item^.DoCompare and (eva^.Num<evb^.Num)));
{$endif}
{$endif}
 end;
end;                              

function TBESENCodeContext.AbstractRelationalNumber(eva,evb:PBESENValue;var BoolRes:TBESENBoolean):TBESENBoolean; 
{$ifndef UseSafeOperations}
{$ifdef cpu386}
var a,b:TBESENNumber;
    br:TBESENBoolean;
{$else}
var Item:PBESENNumberCodeAbstractRelationalLookupTableItem;
{$endif}
{$endif}
begin
{$ifdef UseSafeOperations}
 result:=true;
 if BESENIsNaN(eva^.Num) or BESENIsNaN(evb^.Num) then begin
  result:=false;
  BoolRes:=false;
 end else if BESENIsSameValue(eva^.Num,evb^.Num) then begin
  BoolRes:=false;
 end else if (BESENIsZero(eva^.Num) and BESENIsZero(evb^.Num)) and (BESENIsNegative(eva^.Num)<>BESENIsNegative(evb^.Num)) then begin
  BoolRes:=false;
 end else if BESENIsPosInfinite(eva^.Num) then begin
  BoolRes:=false;
 end else if BESENIsPosInfinite(evb^.Num) then begin
  BoolRes:=true;
 end else if BESENIsNegInfinite(evb^.Num) then begin
  BoolRes:=false;
 end else if BESENIsNegInfinite(eva^.Num) then begin
  BoolRes:=true;
 end else begin
  BoolRes:=eva^.Num<evb^.Num;
 end;
{$else}
{$ifdef cpu386}
  a:=eva^.Num;
  b:=evb^.Num;
  asm
   fld qword ptr [a]
   fcomp qword ptr [b]
   fstsw ax
   sahf
   setb al
   setnp cl
   and al,cl
   neg al
   sbb eax,eax
   mov dword ptr br,eax
   neg cl
   sbb ecx,ecx
   mov dword ptr result,ecx
  end {$ifdef fpc}['eax','ecx']{$endif};
  BoolRes:=br;
{$else}
  Item:=@BESENNumberCodeAbstractRelationalLookupTable[((BESENNumberCodeFlags(eva^.Num) shl 4) or BESENNumberCodeFlags(evb^.Num)) and $ff];
  result:=Item^.IsNotUndefined;
  BoolRes:=result and (Item^.ResultBooleanValue or (Item^.DoCompare and (eva^.Num<evb^.Num)));
{$endif}
{$endif}
end;

procedure TBESENCodeContext.Throw(const ThrowValue:TBESENValue);
begin
 raise EBESENThrowException.Create('Throw',ThrowValue);
end;

procedure TBESENCodeContext.OpSTOP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Running:=false;
 BlockRunning:=false;
end;

procedure TBESENCodeContext.OpNEW(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var Counter,CountArguments:integer;
    ConstructorValue:PBESENValue;
    OldIsStrict:boolean;
begin
 OldIsStrict:=TBESEN(Instance).IsStrict;
 try
  ConstructorValue:=@RegisterValues[Operands^[1]];
  CountArguments:=Operands^[2];
  for Counter:=0 to CountArguments-1 do begin
   ParamArgs[Counter]:=@RegisterValues[Operands^[Counter+3]];
  end;
  if ConstructorValue^.ValueType<>bvtOBJECT then begin
   BESENThrowTypeErrorNotAConstructorObject;
  end else if not TBESENObject(ConstructorValue^.Obj).HasConstruct then begin
   BESENThrowTypeErrorObjectHasNoConstruct;
  end;
  TBESEN(Instance).GarbageCollector.TriggerCollect;
  TBESEN(Instance).ObjectConstruct(TBESENObject(ConstructorValue^.Obj),BESENUndefinedValue,@ParamArgs[0],CountArguments,RegisterValues[Operands^[0]]);
 finally
  TBESEN(Instance).IsStrict:=OldIsStrict;
 end;
end;

procedure TBESENCodeContext.OpCALL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var Counter,CountArguments:integer;
    Ref,Func:PBESENValue;
    OldIsStrict,DirectCall:boolean;
begin
 OldIsStrict:=TBESEN(Instance).IsStrict;
 try
  Ref:=@RegisterValues[Operands^[1]];
  Func:=@RegisterValues[Operands^[2]];
  CountArguments:=Operands^[3];
  for Counter:=0 to CountArguments-1 do begin
   ParamArgs[Counter]:=@RegisterValues[Operands^[Counter+4]];
  end;
  if Func^.ValueType<>bvtOBJECT then begin
   BESENThrowTypeErrorNotAFunction;
  end else if not (assigned(Func^.Obj) and TBESENObject(Func^.Obj).HasCall) then begin
   BESENThrowTypeErrorNotCallable;
  end;
  if Ref^.ValueType=bvtREFERENCE then begin
   BESENRefBaseValueToCallThisArgValueProcs[Ref^.ReferenceBase.ValueType](CallThisArg,Ref^.ReferenceBase);
  end else begin
   CallThisArg.ValueType:=bvtUNDEFINED;
  end;
  if Func^.Obj=TBESEN(Instance).ObjectGlobalEval then begin
   DirectCall:=((Ref^.ValueType=bvtREFERENCE) and (Ref^.ReferenceBase.ValueType=brbvtENVREC)) and TBESENEnvironmentRecord(Ref^.ReferenceBase.EnvRec).HasBindingEx('eval',Descriptor);
   TBESEN(Instance).GlobalEval(Context,CallThisArg,@ParamArgs[0],CountArguments,DirectCall,RegisterValues[Operands^[0]]);
  end else begin
   TBESEN(Instance).ObjectCall(TBESENObject(Func^.Obj),CallThisArg,@ParamArgs[0],CountArguments,RegisterValues[Operands^[0]]);
  end;
 finally
  TBESEN(Instance).IsStrict:=OldIsStrict;
 end;
end;

procedure TBESENCodeContext.OpEND(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 NewBlockLevel:=Operands^[0];
 while Running and (BlockLevel>=NewBlockLevel) do begin
  if BlockLevel=0 then begin
   Running:=false;
   BlockRunning:=false;
   break;
  end else begin
   dec(BlockLevel);
   Block:=@Blocks[BlockLevel];
   case Block^.BlockType of
    bccbtENUM:begin
     PropertyEnumerator:=Block^.OldPropertyEnumerator;
     EnumBlock:=Block^.OldEnumBlock;
     BESENFreeAndNil(Block^.PropertyEnumerator);
    end;
    bccbtWITH:begin
     Context.LexicalEnvironment:=Block^.LexicalEnvironment.Outer;
    end;
    bccbtCATCH:begin
     Block^.Done:=true;
    end;
    bccbtCATCH2:begin
     Context.LexicalEnvironment:=Block^.LexicalEnvironment.Outer;
     Block^.Ident:='';
    end;
    bccbtFINALLY:begin
     Block^.BlockType:=bccbtFINALLY2;
     Block^.Done:=true;
     Block^.Resume:=PC-2;
     Block^.Raised:=false;
     PC:=Block^.Handler;
     inc(BlockLevel);
     break;
    end;
    bccbtFINALLY2:begin
    end;
   end;
  end;
 end;
end;

procedure TBESENCodeContext.OpVREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
    EnvRec:TBESENEnvironmentRecord;
    CurrentObject:TBESENObject;
    PolymorphicInlineCacheInstruction:PBESENCodePolymorphicInlineCacheInstruction;
    StructureID:integer;
    CacheItem:PBESENCodePolymorphicInlineCacheItem;
    CacheItemIndex:integer;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtREFERENCE;
 vp^.ReferenceBase.ValueType:=brbvtENVREC;
 vp^.ReferenceBase.EnvRec:=Context.VariableEnvironment.EnvironmentRecord;
 vp^.ReferenceIsStrict:=TBESEN(Instance).IsStrict;
 vp^.ReferenceHash:=TBESENHash(Operands^[2]);

 if TBESEN(Instance).InlineCacheEnabled then begin
  EnvRec:=Context.VariableEnvironment.EnvironmentRecord;
  if assigned(EnvRec) and (EnvRec is TBESENObjectEnvironmentRecord) then begin
   CurrentObject:=TBESENObjectEnvironmentRecord(EnvRec).BindingObject;
   if assigned(CurrentObject) then begin
    PolymorphicInlineCacheInstruction:=Code.PolymorphicInlineCacheInstructions[Operands^[3]];

    // Polymorphic lookup
    if CurrentObject.StructureID<>0 then begin
     for CacheItemIndex:=0 to BESENPolymorphicInlineCacheSize-1 do begin
      CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[(PolymorphicInlineCacheInstruction^.CacheItemPositions shr (CacheItemIndex shl 2)) and 7];
      if CurrentObject.StructureID=CacheItem^.StructureID then begin
       if CacheItemIndex<>0 then begin
        PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,CacheItemIndex);
        Operands^[4]:=CacheItem^.StructureID;
        Operands^[5]:=BESENInt32BooleanValues[vp^.ReferenceIsStrict];
        Operands^[6]:=CacheItem^.Index;
        Operands^[7]:=CacheItem^.ID;
       end;
       vp^.ReferenceIndex:=CacheItem^.Index;
       vp^.ReferenceID:=CacheItem^.ID;
       exit;
      end;
     end;
    end;

    // Megamorphic lookup
    CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[(PolymorphicInlineCacheInstruction^.CacheItemPositions shr 28) and 7];
    if CurrentObject.StructureID=0 then begin
     if not CurrentObject.RebuildStructure then begin
      CurrentObject:=nil;
     end;
    end;
    if assigned(CurrentObject) then begin
     StructureID:=CurrentObject.StructureID;
     while assigned(CurrentObject) and (CurrentObject.StructureID<>0) do begin
      if CurrentObject.GetOwnProperty(Code.Variables[Operands^[1]].Name,Descriptor,vp^.ReferenceHash) then begin
       if assigned(CurrentObject.LastProp) and ((CurrentObject.LastProp.Index>=0) and (CurrentObject.LastProp.ID>=0)) and (([boppVALUE,boppGETTER,boppSETTER,boppWRITABLE]*CurrentObject.LastProp.Descriptor.Presents)<>[]) then begin
        PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,7);
        vp^.ReferenceIndex:=CurrentObject.LastProp.Index;
        vp^.ReferenceID:=CurrentObject.LastProp.ID;
        CacheItem^.StructureID:=StructureID;
        CacheItem^.Index:=vp^.ReferenceIndex;
        CacheItem^.ID:=vp^.ReferenceID;
        Operands^[4]:=CacheItem^.StructureID;
        Operands^[5]:=BESENInt32BooleanValues[vp^.ReferenceIsStrict];
        Operands^[6]:=CacheItem^.Index;
        Operands^[7]:=CacheItem^.ID;
        exit;
       end;
       break;
      end;
      CurrentObject:=CurrentObject.Prototype;
     end;
    end;

   end;
  end;
 end;

 // Normal lookup
 Operands^[4]:=-1;
 vp^.ReferenceIndex:=-1;
 vp^.ReferenceID:=-1;
 vp^.Str:=Code.Variables[Operands^[1]].Name;
end;

procedure TBESENCodeContext.OpLREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtLOCAL;
 vp^.LocalIndex:=Operands^[1];
end;

procedure TBESENCodeContext.OpNOP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
end;

procedure TBESENCodeContext.OpCOPY(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(RegisterValues[Operands^[0]],RegisterValues[Operands^[1]]);
end;

procedure TBESENCodeContext.OpNEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r,a,b:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=not TBESEN(Instance).EqualityExpressionEquals(a^,b^);
end;

procedure TBESENCodeContext.OpNSEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r,a,b:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=not BESENEqualityExpressionStrictEquals(a^,b^);
end;

procedure TBESENCodeContext.OpAREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
    DoubleIndex:PBESENNumber;
begin
 r:=@RegisterValues[Operands^[0]];
 BESENValueToReferenceBaseValue(RegisterValues[Operands^[1]],r^.ReferenceBase);
 r^.ValueType:=bvtREFERENCE;
 r^.ReferenceIsStrict:=TBESEN(Instance).IsStrict;
 r^.ReferenceHash:=TBESENHash(Operands^[3]);
 r^.ReferenceID:=-1;
 DoubleIndex:=@RegisterValues[Operands^[2]].Num;
 if (DoubleIndex^>=0) and (DoubleIndex^<2147483648.0) then begin // not 4294967296.0 due to ReferenceIndex<0 check !
  r^.ReferenceIndex:=TBESENINT32(TBESENUINT32(trunc(DoubleIndex^)));
 end else begin
  r^.ReferenceIndex:=-1;
  r^.Str:=TBESEN(Instance).ToStr(RegisterValues[Operands^[2]]);
 end;
end;

procedure TBESENCodeContext.OpTHROW(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 if Trace(bttTHROW) then begin
  Throw(RegisterValues[Operands^[0]]);
 end;
end;

procedure TBESENCodeContext.OpSETC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(ResultValue,RegisterValues[Operands^[0]]);
end;

procedure TBESENCodeContext.OpGETC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(RegisterValues[Operands^[0]],ResultValue);
end;

procedure TBESENCodeContext.OpTHIS(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(RegisterValues[Operands^[0]],Context.ThisBinding);
end;

procedure TBESENCodeContext.OpOBJECT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtOBJECT;
 vp^.Obj:=TBESEN(Instance).ObjectConstructor;
end;

procedure TBESENCodeContext.OpARRAY(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtOBJECT;
 vp^.Obj:=TBESEN(Instance).ObjectArrayConstructor;
end;

procedure TBESENCodeContext.OpREGEXP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtOBJECT;
 vp^.Obj:=TBESEN(Instance).ObjectRegExpConstructor;
end;

procedure TBESENCodeContext.OpREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
    CurrentObject:TBESENObject;
    PolymorphicInlineCacheInstruction:PBESENCodePolymorphicInlineCacheInstruction;
    CacheItemIndex:integer;
    CacheItem:PBESENCodePolymorphicInlineCacheItem;
    StructureID:integer;
    StrReg:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 BESENValueToReferenceBaseValue(RegisterValues[Operands^[1]],r^.ReferenceBase);
 r^.ValueType:=bvtREFERENCE;
 r^.ReferenceIsStrict:=TBESEN(Instance).IsStrict;
 r^.ReferenceHash:=TBESENHash(Operands^[4]);

 if TBESEN(Instance).InlineCacheEnabled and (TBESENUINT32(Operands^[3])<>$fefefefe) and (r^.ReferenceBase.ValueType=brbvtOBJECT) then begin
  CurrentObject:=TBESENObject(r^.ReferenceBase.Obj);
  if assigned(CurrentObject) then begin
   PolymorphicInlineCacheInstruction:=Code.PolymorphicInlineCacheInstructions[Operands^[5]];

   // Polymorphic lookup
   if CurrentObject.StructureID<>0 then begin
    for CacheItemIndex:=0 to BESENPolymorphicInlineCacheSize-1 do begin
     CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[(PolymorphicInlineCacheInstruction^.CacheItemPositions shr (CacheItemIndex shl 2)) and 7];
     if CurrentObject.StructureID=CacheItem^.StructureID then begin
      if CacheItemIndex<>0 then begin
       PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,CacheItemIndex);
       Operands^[6]:=CacheItem^.StructureID;
       Operands^[7]:=BESENInt32BooleanValues[r^.ReferenceIsStrict];
       Operands^[8]:=CacheItem^.Index;
       Operands^[9]:=CacheItem^.ID;
      end;
      r^.ReferenceIndex:=CacheItem^.Index;
      r^.ReferenceID:=CacheItem^.ID;
      exit;
     end;
    end;
   end;

   // Megamorphic lookup
   CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[(PolymorphicInlineCacheInstruction^.CacheItemPositions shr 28) and 7];
   if CurrentObject.StructureID=0 then begin
    if not CurrentObject.RebuildStructure then begin
     CurrentObject:=nil;
    end;
   end;
   if assigned(CurrentObject) then begin
    StrReg:=@RegisterValues[Operands^[2]];
    StructureID:=CurrentObject.StructureID;
    while assigned(CurrentObject) and (CurrentObject.StructureID<>0) do begin
     CurrentObject.LastProp:=nil;
     if CurrentObject.GetOwnProperty(StrReg^.Str,Descriptor,r^.ReferenceHash) then begin
      if assigned(CurrentObject.LastProp) and ((CurrentObject.LastProp.Index>=0) and (CurrentObject.LastProp.ID>=0)) and (([boppVALUE,boppGETTER,boppSETTER,boppWRITABLE]*CurrentObject.LastProp.Descriptor.Presents)<>[]) then begin
       PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,7);
       r^.ReferenceIndex:=CurrentObject.LastProp.Index;
       r^.ReferenceID:=CurrentObject.LastProp.ID;
       CacheItem^.StructureID:=StructureID;
       CacheItem^.Index:=r^.ReferenceIndex;
       CacheItem^.ID:=r^.ReferenceID;
       Operands^[6]:=CacheItem^.StructureID;
       Operands^[7]:=BESENInt32BooleanValues[r^.ReferenceIsStrict];
       Operands^[8]:=r^.ReferenceIndex;
       Operands^[9]:=r^.ReferenceID;
       exit;
      end;
      break;
     end;
     CurrentObject:=CurrentObject.Prototype;
    end;
   end;

  end;
 end;

 // Normal lookup
 Operands^[6]:=-1;
 r^.ReferenceIndex:=-1;
 r^.ReferenceID:=-1;
 r^.Str:=RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpGETVALUE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 case RegisterValues[Operands^[1]].ValueType of
  bvtREFERENCE:begin
   GetRefProcs[RegisterValues[Operands^[1]].ReferenceBase.ValueType](RegisterValues[Operands^[1]],RegisterValues[Operands^[0]]);
  end;
  bvtLOCAL:begin
   Context.VariableEnvironment.EnvironmentRecord.GetIndexValue(RegisterValues[Operands^[1]].LocalIndex,-1,TBESEN(Instance).IsStrict,RegisterValues[Operands^[0]]);
  end;
  else begin
   BESENCopyValue(RegisterValues[Operands^[0]],RegisterValues[Operands^[1]]);
  end;
 end;
end;

procedure TBESENCodeContext.OpLOOKUP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
    PolymorphicInlineCacheInstruction:PBESENCodePolymorphicInlineCacheInstruction;
    Lex:TBESENLexicalEnvironment;
    EnvRec:TBESENEnvironmentRecord;
    CurrentObject:TBESENObject;
    Item:PBESENDeclarativeEnvironmentRecordHashItem;
    CacheItem:PBESENCodePolymorphicInlineCacheItem;
    CacheItemIndex,NestedLevel,StructureID:integer;
    FirstObjectEnvRec,DoIt:boolean;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtREFERENCE;
 vp^.ReferenceBase.ValueType:=brbvtUNDEFINED;
 vp^.ReferenceIsStrict:=TBESEN(Instance).IsStrict;
 vp^.ReferenceHash:=TBESENHash(Operands^[2]);

 if TBESEN(Instance).InlineCacheEnabled then begin
  PolymorphicInlineCacheInstruction:=Code.PolymorphicInlineCacheInstructions[Operands^[3]];

  // Polymorphic lookup
  for CacheItemIndex:=0 to BESENPolymorphicInlineCacheSize-1 do begin
   CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[(PolymorphicInlineCacheInstruction^.CacheItemPositions shr (CacheItemIndex shl 2)) and 7];
   if CacheItem^.StructureID<>-1 then begin
    NestedLevel:=CacheItem^.NestedLevel;
    Lex:=Context.LexicalEnvironment;
    while assigned(Lex) and (NestedLevel>0) do begin
     dec(NestedLevel);
     Lex:=Lex.Outer;
    end;
    if (NestedLevel=0) and assigned(Lex) then begin
     EnvRec:=Lex.EnvironmentRecord;
     if CacheItem^.StructureID>0 then begin
      if EnvRec is TBESENObjectEnvironmentRecord then begin
       CurrentObject:=TBESENObjectEnvironmentRecord(EnvRec).BindingObject;
       if assigned(CurrentObject) and ((CurrentObject.StructureID<>0) and (CurrentObject.StructureID=CacheItem^.StructureID)) then begin
        if CacheItemIndex<>0 then begin
         PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,CacheItemIndex);
        end;
        vp^.ReferenceBase.ValueType:=brbvtENVREC;
        vp^.ReferenceBase.EnvRec:=EnvRec;
        vp^.ReferenceIndex:=CacheItem^.Index;
        vp^.ReferenceID:=CacheItem^.ID;
        exit;
       end;
      end;
     end else begin
      if EnvRec is TBESENDeclarativeEnvironmentRecord then begin
       if (CacheItem^.Index<TBESENDeclarativeEnvironmentRecord(EnvRec).HashIndexIDs.Count) and (TBESENDeclarativeEnvironmentRecord(EnvRec).HashIndexIDs[CacheItem^.Index]=CacheItem^.ID) then begin
        if CacheItemIndex<>0 then begin
         PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,CacheItemIndex);
        end;
        vp^.ReferenceBase.ValueType:=brbvtENVREC;
        vp^.ReferenceBase.EnvRec:=EnvRec;
        vp^.ReferenceIndex:=CacheItem^.Index;
        vp^.ReferenceID:=CacheItem^.ID;
        exit;
       end;
      end;
     end;
    end;
   end;
  end;

  // Megamorphic lookup
  vp^.ReferenceIndex:=-1;
  vp^.ReferenceID:=-1;
  vp^.Str:=LookupNames^[Operands^[1]];
  CacheItem:=@PolymorphicInlineCacheInstruction^.CacheItems[(PolymorphicInlineCacheInstruction^.CacheItemPositions shr 28) and 7];
  FirstObjectEnvRec:=true;
  DoIt:=true;
  NestedLevel:=0;
  Lex:=Context.LexicalEnvironment;
  while assigned(Lex) do begin
   EnvRec:=Lex.EnvironmentRecord;
   if DoIt and (EnvRec.HasMaybeDirectEval and not EnvRec.IsStrict) then begin
    // If the direct eval is in an es5-strict-mode code body (top level or function), it can't create vars in
    // that code lexical body's scope. But in an es5-non-strict code body (top level or function) we must disable
    // here the caching, because a direct eval can create here vars in that code body's lexical scope.
    DoIt:=false;
   end;
   if EnvRec is TBESENObjectEnvironmentRecord then begin
    CurrentObject:=TBESENObjectEnvironmentRecord(EnvRec).BindingObject;
    if assigned(CurrentObject) then begin
     if CurrentObject.StructureID=0 then begin
      if not CurrentObject.RebuildStructure then begin
       CurrentObject:=nil;
      end;
     end;
     if assigned(CurrentObject) then begin
      StructureID:=CurrentObject.StructureID;
      while assigned(CurrentObject) and (CurrentObject.StructureID<>0) do begin
       if CurrentObject.GetOwnProperty(vp^.Str,Descriptor,vp^.ReferenceHash) then begin
        vp^.ReferenceBase.ValueType:=brbvtENVREC;
        vp^.ReferenceBase.EnvRec:=EnvRec;
        if DoIt and FirstObjectEnvRec then begin
         FirstObjectEnvRec:=false;
         if assigned(CurrentObject.LastProp) and ((CurrentObject.LastProp.Index>=0) and (CurrentObject.LastProp.ID>=0)) and (([boppVALUE,boppGETTER,boppSETTER,boppWRITABLE]*CurrentObject.LastProp.Descriptor.Presents)<>[]) then begin
          PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,7);
          vp^.ReferenceIndex:=CurrentObject.LastProp.Index;
          vp^.ReferenceID:=CurrentObject.LastProp.ID;
          CacheItem^.StructureID:=StructureID;
          CacheItem^.Index:=vp^.ReferenceIndex;
          CacheItem^.ID:=vp^.ReferenceID;
          CacheItem^.NestedLevel:=NestedLevel;
         end;
        end;
        exit;
       end;
       CurrentObject:=CurrentObject.Prototype;
      end;
     end;
    end;
   end else begin
    Item:=TBESENDeclarativeEnvironmentRecord(EnvRec).GetKey(vp^.Str);
    if assigned(Item) then begin
     vp^.ReferenceBase.ValueType:=brbvtENVREC;
     vp^.ReferenceBase.EnvRec:=EnvRec;
     if DoIt and (Item^.Index>=0) then begin
      PolymorphicInlineCacheInstruction^.CacheItemPositions:=BESENAdjustPolymorphicInlineCachePosition(PolymorphicInlineCacheInstruction^.CacheItemPositions,7);
      vp^.ReferenceIndex:=Item^.Index;
      vp^.ReferenceID:=TBESENDeclarativeEnvironmentRecord(EnvRec).HashIndexIDs[vp^.ReferenceIndex];
      CacheItem^.StructureID:=-2;
      CacheItem^.Index:=vp^.ReferenceIndex;
      CacheItem^.ID:=vp^.ReferenceID;
      CacheItem^.NestedLevel:=NestedLevel;
     end;
     exit;
    end;
   end;
   inc(NestedLevel);
   Lex:=Lex.Outer;
  end;
 end else begin
  // Normal lookup
  vp^.ReferenceIndex:=-1;
  vp^.ReferenceID:=-1;
  vp^.Str:=LookupNames^[Operands^[1]];
  Lex:=Context.LexicalEnvironment;
  while assigned(Lex) do begin
   EnvRec:=Lex.EnvironmentRecord;
   if assigned(EnvRec) then begin
    if EnvRec is TBESENObjectEnvironmentRecord then begin
     CurrentObject:=TBESENObjectEnvironmentRecord(EnvRec).BindingObject;
     while assigned(CurrentObject) do begin
      if CurrentObject.GetOwnProperty(vp^.Str,Descriptor,vp^.ReferenceHash) then begin
       vp^.ReferenceBase.ValueType:=brbvtENVREC;
       vp^.ReferenceBase.EnvRec:=EnvRec;
       exit;
      end;
      CurrentObject:=CurrentObject.Prototype;
     end;
    end else if EnvRec is TBESENDeclarativeEnvironmentRecord then begin
     Item:=TBESENDeclarativeEnvironmentRecord(EnvRec).GetKey(vp^.Str);
     if assigned(Item) then begin
      vp^.ReferenceBase.ValueType:=brbvtENVREC;
      vp^.ReferenceBase.EnvRec:=EnvRec;
      exit;
     end;
    end;
   end;
   Lex:=Lex.Outer;
  end;
 end;
end;

procedure TBESENCodeContext.OpPUTVALUE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 PutValue(RegisterValues[Operands^[0]],RegisterValues[Operands^[1]]);
end;

procedure TBESENCodeContext.OpDELETE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
    bv:TBESENBoolean;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 case up^.ValueType of
  bvtREFERENCE:begin
   case up^.ReferenceBase.ValueType of
    brbvtBOOLEAN,brbvtNUMBER,brbvtSTRING:begin
     BESENReferenceBaseValueToValue(up^.ReferenceBase,Temp);
     if up^.ReferenceID<0 then begin
      if up^.ReferenceIndex<0 then begin
       bv:=TBESEN(Instance).ToObj(Temp).DeleteEx(up^.Str,up^.ReferenceIsStrict,Descriptor,up^.ReferenceHash);
      end else begin
       bv:=TBESEN(Instance).ToObj(Temp).DeleteArrayIndex(up^.ReferenceIndex,up^.ReferenceIsStrict);
      end;
     end else begin
      bv:=TBESEN(Instance).ToObj(Temp).DeleteIndex(up^.ReferenceIndex,up^.ReferenceID,up^.ReferenceIsStrict);
     end;
     vp^.ValueType:=bvtBOOLEAN;
     vp^.Bool:=bv;
    end;
    brbvtOBJECT:begin
     if up^.ReferenceID<0 then begin
      if up^.ReferenceIndex<0 then begin
       bv:=TBESENObject(up^.ReferenceBase.Obj).DeleteEx(up^.Str,up^.ReferenceIsStrict,Descriptor,up^.ReferenceHash);
      end else begin
       bv:=TBESENObject(up^.ReferenceBase.Obj).DeleteArrayIndex(up^.ReferenceIndex,up^.ReferenceIsStrict);
      end;
     end else begin
      bv:=TBESENObject(up^.ReferenceBase.Obj).DeleteIndex(up^.ReferenceIndex,up^.ReferenceID,up^.ReferenceIsStrict);
     end;
     vp^.ValueType:=bvtBOOLEAN;
     vp^.Bool:=bv;
    end;
    brbvtENVREC:begin
     if up^.ReferenceIsStrict then begin
      BESENThrowSyntaxError('"delete" not allowed here');
     end;
     if up^.ReferenceID<0 then begin
      if up^.ReferenceIndex<0 then begin
       bv:=TBESENEnvironmentRecord(up^.ReferenceBase.EnvRec).DeleteBindingEx(up^.Str,Descriptor,up^.ReferenceHash);
      end else begin
       bv:=TBESENEnvironmentRecord(up^.ReferenceBase.EnvRec).DeleteArrayIndex(TBESENUINT32(up^.ReferenceIndex));
      end;
     end else begin
      bv:=TBESENEnvironmentRecord(up^.ReferenceBase.EnvRec).DeleteIndex(up^.ReferenceIndex,up^.ReferenceID);
     end;
     vp^.ValueType:=bvtBOOLEAN;
     vp^.Bool:=bv;
    end;
    else begin
     if up^.ReferenceIsStrict then begin
      BESENThrowSyntaxError('"delete" not allowed here');
     end else begin
      vp^.ValueType:=bvtBOOLEAN;
      vp^.Bool:=true;
     end;
    end;
   end;
  end;
  bvtLOCAL:begin
   if TBESEN(Instance).IsStrict then begin
    BESENThrowSyntaxError('"delete" not allowed here');
   end;
   vp^.ValueType:=bvtBOOLEAN;
   vp^.Bool:=Context.VariableEnvironment.EnvironmentRecord.DeleteIndex(up^.LocalIndex,-1);
  end;
  else begin
   vp^.ValueType:=bvtBOOLEAN;
   vp^.Bool:=true;
  end;
 end;
end;

procedure TBESENCodeContext.OpTYPEOF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 if up^.ValueType=bvtREFERENCE then begin
  if not (up^.ReferenceBase.ValueType in [brbvtBOOLEAN,brbvtNUMBER,brbvtSTRING,brbvtOBJECT,brbvtENVREC]) then begin
   Temp.ValueType:=bvtUNDEFINED;
  end else begin
   GetValue(up^,Temp);
  end;
 end else if up^.ValueType=bvtLOCAL then begin
  Context.VariableEnvironment.EnvironmentRecord.GetIndexValue(up^.LocalIndex,-1,TBESEN(Instance).IsStrict,Temp);
 end else begin
  BESENCopyValueProcs[up^.ValueType](Temp,up^);
 end;
 vp^.ValueType:=bvtSTRING;
 case Temp.ValueType of
  bvtUNDEFINED:begin
   vp^.Str:='undefined';
  end;
  bvtNULL:begin
   vp^.Str:='object';
  end;
  bvtBOOLEAN:begin
   vp^.Str:='boolean';
  end;
  bvtNUMBER:begin
   vp^.Str:='number';
  end;
  bvtSTRING:begin
   vp^.Str:='string';
  end;
  bvtOBJECT:begin
   if assigned(Temp.Obj) and TBESENObject(Temp.Obj).HasCall then begin
    vp^.Str:='function';
   end else begin
    vp^.Str:='object';
   end;
  end;
  else begin
   vp^.Str:='unknown';
  end;
 end;
end;

procedure TBESENCodeContext.OpTOOBJECT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 if up^.ValueType<>bvtOBJECT then begin
  TBESEN(Instance).ToObjectValue(up^,vp^);
 end else begin
  BESENCopyValue(vp^,up^);
 end;
end;

procedure TBESENCodeContext.OpTONUMBER(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 if up^.ValueType<>bvtNUMBER then begin
  TBESEN(Instance).ToNumberValue(up^,vp^);
 end else begin
  BESENCopyValue(vp^,up^);
 end;
end;

procedure TBESENCodeContext.OpTOBOOLEAN(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 if up^.ValueType<>bvtBOOLEAN then begin
  TBESEN(Instance).ToBooleanValue(up^,vp^);
 end else begin
  BESENCopyValue(vp^,up^);
 end;
end;

procedure TBESENCodeContext.OpTOSTRING(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 if up^.ValueType<>bvtSTRING then begin
  TBESEN(Instance).ToStringValue(up^,vp^);
 end else begin
  BESENCopyValue(vp^,up^);
 end;
end;

procedure TBESENCodeContext.OpTOPRIMITIVE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up,vp:PBESENValue;
begin
 up:=@RegisterValues[Operands^[1]];
 vp:=@RegisterValues[Operands^[0]];
 if up^.ValueType=bvtOBJECT then begin
  TBESEN(Instance).ToPrimitiveValue(up^,BESENEmptyValue,vp^);
 end else begin
  BESENCopyValue(vp^,up^);
 end;
end;

procedure TBESENCodeContext.OpNEG(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
{$ifdef fpc}
 PBESENQWORD(@r^.Num)^:=PBESENQWORD(@RegisterValues[Operands^[1]].Num)^ xor qword($8000000000000000);
{$else}
 PBESENINT64(@r^.Num)^:=PBESENINT64(@RegisterValues[Operands^[1]].Num)^ xor int64($8000000000000000);
{$endif}
end;

procedure TBESENCodeContext.OpINV(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(not TBESEN(Instance).ToInt32(RegisterValues[Operands^[1]]));
end;

procedure TBESENCodeContext.OpNOT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=not RegisterValues[Operands^[1]].Bool;
end;

procedure TBESENCodeContext.OpMUL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num*RegisterValues[Operands^[2]].Num;
end;

procedure TBESENCodeContext.OpDIV(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num/RegisterValues[Operands^[2]].Num;
end;

procedure TBESENCodeContext.OpMOD(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=BESENModulo(RegisterValues[Operands^[1]].Num,RegisterValues[Operands^[2]].Num);
end;

procedure TBESENCodeContext.OpADD(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r,a,b:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 if (a^.ValueType=bvtSTRING) or (b^.ValueType=bvtSTRING) then begin
  if a^.ValueType<>bvtSTRING then begin
   TBESEN(Instance).ToStringValue(a^,va);
   a:=@va;
  end;
  if b^.ValueType<>bvtSTRING then begin
   TBESEN(Instance).ToStringValue(b^,vb);
   b:=@vb;
  end;
  r^.ValueType:=bvtSTRING;
  r^.Str:=a^.Str+b^.Str;
 end else begin
  if a^.ValueType<>bvtNUMBER then begin
   TBESEN(Instance).ToNumberValue(a^,va);
   a:=@va;
  end;
  if b^.ValueType<>bvtNUMBER then begin
   TBESEN(Instance).ToNumberValue(b^,vb);
   b:=@vb;
  end;
  r^.ValueType:=bvtNUMBER;
  r^.Num:=a^.Num+b^.Num;
 end;
end;

procedure TBESENCodeContext.OpADDNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num+RegisterValues[Operands^[2]].Num;
end;

procedure TBESENCodeContext.OpSUB(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num-RegisterValues[Operands^[2]].Num;
end;

procedure TBESENCodeContext.OpSHL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(TBESEN(Instance).ToInt32(RegisterValues[Operands^[1]]) shl (TBESEN(Instance).ToUInt32(RegisterValues[Operands^[2]]) and 31));
end;

procedure TBESENCodeContext.OpSHR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(sar(TBESEN(Instance).ToInt32(RegisterValues[Operands^[1]]),TBESEN(Instance).ToUInt32(RegisterValues[Operands^[2]]) and 31));
end;

procedure TBESENCodeContext.OpUSHR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENUINT32(TBESEN(Instance).ToUInt32(RegisterValues[Operands^[1]]) shr (TBESEN(Instance).ToUInt32(RegisterValues[Operands^[2]]) and 31));
end;

procedure TBESENCodeContext.OpLT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 AbstractRelational(@RegisterValues[Operands^[1]],@RegisterValues[Operands^[2]],r^.Bool);
end;

procedure TBESENCodeContext.OpGT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 AbstractRelational(@RegisterValues[Operands^[2]],@RegisterValues[Operands^[1]],r^.Bool);
end;

procedure TBESENCodeContext.OpLE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 if AbstractRelational(@RegisterValues[Operands^[2]],@RegisterValues[Operands^[1]],r^.Bool) then begin
  r^.Bool:=not r^.Bool;
 end;
end;

procedure TBESENCodeContext.OpGE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 if AbstractRelational(@RegisterValues[Operands^[1]],@RegisterValues[Operands^[2]],r^.Bool) then begin
  r^.Bool:=not r^.Bool;
 end;
end;

procedure TBESENCodeContext.OpINSTANCEOF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r,a,b:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 r^.ValueType:=bvtBOOLEAN;
 if b^.ValueType<>bvtOBJECT then begin
  BESENThrowTypeError('Not a object');
 end else begin
  r^.Bool:=TBESEN(Instance).ObjectInstanceOf(a^,TBESENObject(b^.Obj));
 end;
end;

procedure TBESENCodeContext.OpIN(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r,a,b:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 r^.ValueType:=bvtBOOLEAN;
 if b^.ValueType<>bvtOBJECT then begin
  BESENThrowTypeError('Not a object');
 end else begin
  r^.Bool:=TBESENObject(b^.Obj).HasPropertyEx(TBESEN(Instance).ToStr(a^),Descriptor);
 end;
end;

procedure TBESENCodeContext.OpEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=TBESEN(Instance).EqualityExpressionEquals(RegisterValues[Operands^[1]],RegisterValues[Operands^[2]]);
end;

procedure TBESENCodeContext.OpSEQ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=BESENEqualityExpressionStrictEquals(RegisterValues[Operands^[1]],RegisterValues[Operands^[2]]);
end;

procedure TBESENCodeContext.OpBAND(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(TBESEN(Instance).ToInt32(RegisterValues[Operands^[1]]) and TBESEN(Instance).ToInt32(RegisterValues[Operands^[2]]));
end;

procedure TBESENCodeContext.OpBXOR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(TBESEN(Instance).ToInt32(RegisterValues[Operands^[1]]) xor TBESEN(Instance).ToInt32(RegisterValues[Operands^[2]]));
end;

procedure TBESENCodeContext.OpBOR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(TBESEN(Instance).ToInt32(RegisterValues[Operands^[1]]) or TBESEN(Instance).ToInt32(RegisterValues[Operands^[2]]));
end;

procedure TBESENCodeContext.OpSENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
    Obj:TBESENObject;
begin
 vp:=@RegisterValues[Operands^[0]];
 case vp^.ValueType of
  bvtNULL,bvtUNDEFINED:begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)=0 then begin
    Obj:=TBESEN(Instance).ObjectEmpty;
   end else begin
    Obj:=nil;
   end;
  end;
  bvtOBJECT:begin
   Obj:=TBESENObject(vp^.Obj);
  end;
  else begin
   Obj:=nil;
  end;
 end;
 if assigned(Obj) then begin
  Block:=@Blocks[BlockLevel];
  Block^.BlockType:=bccbtENUM;
  Block^.PropertyEnumerator:=Obj.Enumerator(false,false);
  Block^.PropertyEnumerator.Reset;
  Block^.OldPropertyEnumerator:=PropertyEnumerator;
  PropertyEnumerator:=Block^.PropertyEnumerator;
  Block^.OldEnumBlock:=EnumBlock;
  EnumBlock:=Block;
  inc(BlockLevel);
 end else begin
  BESENThrowTypeError('Not an object');
 end;
end;

procedure TBESENCodeContext.OpSWITH(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 Block:=@Blocks[BlockLevel];
 Block^.BlockType:=bccbtWITH;
 Block^.LexicalEnvironment:=TBESEN(Instance).NewObjectEnvironment(TBESENObject(vp^.Obj),Context.LexicalEnvironment,TBESEN(Instance).IsStrict,Code.HasMaybeDirectEval);
 TBESEN(Instance).GarbageCollector.Add(Block^.LexicalEnvironment);
 Context.LexicalEnvironment:=Block^.LexicalEnvironment;
 TBESENObjectEnvironmentRecord(Block^.LexicalEnvironment.EnvironmentRecord).ProvideThis:=true;
 Block^.LexicalEnvironment.EnvironmentRecord.UpdateImplicitThisValue;
 inc(BlockLevel);
end;

procedure TBESENCodeContext.OpSCATCH(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Block:=@Blocks[BlockLevel-1];
 Block^.BlockType:=bccbtCATCH2;
 Block^.LexicalEnvironment:=TBESEN(Instance).NewDeclarativeEnvironment(Context.LexicalEnvironment,TBESEN(Instance).IsStrict,Code.HasMaybeDirectEval);
 TBESEN(Instance).GarbageCollector.Add(Block^.LexicalEnvironment);
 Context.LexicalEnvironment:=Block^.LexicalEnvironment;
 Block^.LexicalEnvironment.EnvironmentRecord.CreateMutableBinding(Block^.Ident,false);
 Block^.LexicalEnvironment.EnvironmentRecord.SetMutableBindingEx(Block^.Ident,Block^.Value,false,Descriptor,Descriptor2,AnotherTemp,BESENHashKey(Block^.Ident));
 Block^.Ident:='';
end;

procedure TBESENCodeContext.OpENDF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 dec(BlockLevel);
 Block:=@Blocks[BlockLevel];
 if Block^.Raised or (Block^.Resume>=longword(Code.ByteCodeLen)) then begin
  BlockRunning:=false;
 end else begin
  PC:=Block^.Resume;
 end;
end;

procedure TBESENCodeContext.OpJMP(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 PC:=Operands^[0];
end;

procedure TBESENCodeContext.OpJZ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 if RegisterValues[Operands^[1]].Bool then begin
  PC:=Operands^[0];
 end;
end;

procedure TBESENCodeContext.OpJNZ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 if not RegisterValues[Operands^[1]].Bool then begin
  PC:=Operands^[0];
 end;
end;

procedure TBESENCodeContext.OpJNULL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[1]];
 if (vp^.ValueType in [bvtUNDEFINED,bvtNULL]) or ((vp^.ValueType=bvtOBJECT) and not assigned(vp^.Obj)) then begin
  PC:=Operands^[0];
 end;
end;

procedure TBESENCodeContext.OpLOOPENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 if PropertyEnumerator.GetNext(Str) then begin
  vp:=@RegisterValues[Operands^[1]];
  vp^.ValueType:=bvtSTRING;
  vp^.Str:=Str;
  PC:=Operands^[0];
 end;
end;

procedure TBESENCodeContext.OpSTRYC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Block:=@Blocks[BlockLevel];
 inc(BlockLevel);
 Block^.BlockType:=bccbtCATCH;
 Block^.Handler:=Operands^[0];
 Block^.Ident:=RegisterValues[Operands^[1]].Str;
end;

procedure TBESENCodeContext.OpSTRYF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Block:=@Blocks[BlockLevel];
 inc(BlockLevel);
 Block^.BlockType:=bccbtFINALLY;
 Block^.Handler:=Operands^[0];
end;

procedure TBESENCodeContext.OpLITERAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(RegisterValues[Operands^[0]],Code.Literals[Operands^[1]]);
end;

procedure TBESENCodeContext.OpLITERALUNDEF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 RegisterValues[Operands^[0]].ValueType:=bvtUNDEFINED;
end;

procedure TBESENCodeContext.OpLITERALNULL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 RegisterValues[Operands^[0]].ValueType:=bvtNULL;
end;

procedure TBESENCodeContext.OpLITERALBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtBOOLEAN;
 vp^.Bool:=Operands^[1]<>0;
end;

procedure TBESENCodeContext.OpLITERALNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtNUMBER;
 vp^.Num:=Code.Literals[Operands^[1]].Num;
end;

procedure TBESENCodeContext.OpLITERALSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtSTRING;
 vp^.Str:=Code.Literals[Operands^[1]].Str;
end;

procedure TBESENCodeContext.OpLITERALOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtOBJECT;
 vp^.Obj:=Code.Literals[Operands^[1]].Obj;
end;

procedure TBESENCodeContext.OpFUNC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
    Arg:TBESENINT32;
begin
 vp:=@RegisterValues[Operands^[0]];
 Arg:=Operands^[1];
 if assigned(Code.FunctionLiteralContainers[Arg].Literal.Name) and (length(Code.FunctionLiteralContainers[Arg].Literal.Name.Name)>0) then begin
  LexicalEnvironment:=TBESEN(Instance).NewDeclarativeEnvironment(Context.LexicalEnvironment,TBESEN(Instance).IsStrict,Code.HasMaybeDirectEval);
  OldLexicalEnvironment:=Context.LexicalEnvironment;
  Context.LexicalEnvironment:=LexicalEnvironment;
  TBESEN(Instance).GarbageCollector.Add(LexicalEnvironment);
  try
   vp^:=BESENObjectValue(TBESEN(Instance).MakeFunction(Code.FunctionLiteralContainers[Arg].Literal,Code.FunctionLiteralContainers[Arg].Literal.Name.Name,Context.LexicalEnvironment));
   LexicalEnvironment.EnvironmentRecord.CreateImmutableBinding(TBESENObjectDeclaredFunction(vp^.Obj).ObjectName);
   LexicalEnvironment.EnvironmentRecord.InitializeImmutableBinding(TBESENObjectDeclaredFunction(vp^.Obj).ObjectName,vp^);
  finally
   Context.LexicalEnvironment:=OldLexicalEnvironment;
  end;
 end else begin
  if assigned(Code.FunctionLiteralContainers[Arg].Literal.Name) then begin
   vp^:=BESENObjectValue(TBESEN(Instance).MakeFunction(Code.FunctionLiteralContainers[Arg].Literal,Code.FunctionLiteralContainers[Arg].Literal.Name.Name,Context.LexicalEnvironment));
  end else begin
   vp^:=BESENObjectValue(TBESEN(Instance).MakeFunction(Code.FunctionLiteralContainers[Arg].Literal,'',Context.LexicalEnvironment));
  end;
 end;
end;

procedure TBESENCodeContext.OpLINE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 TBESEN(Instance).LineNumber:=Code.Locations[Operands^[0]].LineNumber;
end;

procedure TBESENCodeContext.OpGC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 TBESEN(Instance).GarbageCollector.TriggerCollect;
end;

procedure TBESENCodeContext.OpSTRICT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 TBESEN(Instance).IsStrict:=Operands^[0]<>0;
end;

procedure TBESENCodeContext.OpSTRICTCHECKREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var up:PBESENValue;
    der:TBESENDeclarativeEnvironmentRecord;
 procedure ThrowIt(const s:TBESENSTRING);
 begin
  BESENThrowSyntaxError('"'+s+'" not allowed here');
 end;
 procedure Check(const s:TBESENSTRING);
 begin
  if (s='eval') or (s='arguments') then begin
   ThrowIt(s);
  end;
 end;
begin
 up:=@RegisterValues[Operands^[0]];
 if up^.ReferenceIsStrict then begin
  case up^.ValueType of
   bvtREFERENCE:begin
    case up^.ReferenceBase.ValueType of
     brbvtENVREC:begin
      if up^.ReferenceID<0 then begin
       if up^.ReferenceIndex<0 then begin
        Check(up^.Str);
       end;
      end else begin
       if up^.ReferenceBase.EnvRec is TBESENDeclarativeEnvironmentRecord then begin
        der:=TBESENDeclarativeEnvironmentRecord(up^.ReferenceBase.EnvRec);
        if (up^.ReferenceIndex>=0) and (up^.ReferenceIndex<der.HashIndexNames.Count) then begin
         Check(der.HashIndexNames.FList[up^.ReferenceIndex]);
        end;
       end else begin
        if (up^.ReferenceIndex>=0) and (up^.ReferenceIndex<=TBESEN(Instance).KeyIDManager.List.Count) then begin
         Check(TBESEN(Instance).KeyIDManager.List.FList[up^.ReferenceID]);
        end;
       end;
      end;
     end;
    end;
   end;
   bvtLOCAL:begin
    der:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord);
    if (up^.LocalIndex>=0) and (up^.LocalIndex<der.HashIndexNames.Count) then begin
     Check(der.HashIndexNames.FList[up^.LocalIndex]);
    end;
   end;
  end;                                                               
 end;
end;

procedure TBESENCodeContext.OpDEBUGGER(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Trace(bttDEBUGGER);
end;

procedure TBESENCodeContext.OpCHECKOBJECTCOERCIBLE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCheckObjectCoercible(RegisterValues[Operands^[0]]);
end;

procedure TBESENCodeContext.OpPUTOBJVALUE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var ObjReg,StrReg,ValReg:PBESENValue;
begin
 ObjReg:=@RegisterValues[Operands^[0]];
 StrReg:=@RegisterValues[Operands^[1]];
 ValReg:=@RegisterValues[Operands^[2]];
 TBESENObject(ObjReg^.Obj).DefineOwnPropertyEx(StrReg^.Str,BESENDataPropertyDescriptor(ValReg^,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),false,Descriptor,BESENHashKey(StrReg^.Str));
end;

procedure TBESENCodeContext.OpPUTOBJGET(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var ObjReg,StrReg,ValReg:PBESENValue;
begin
 ObjReg:=@RegisterValues[Operands^[0]];
 StrReg:=@RegisterValues[Operands^[1]];
 ValReg:=@RegisterValues[Operands^[2]];
 TBESENObject(ObjReg^.Obj).DefineOwnPropertyEx(StrReg^.Str,BESENAccessorPropertyDescriptor(TBESENObject(ValReg^.Obj),nil,[bopaENUMERABLE,bopaCONFIGURABLE]),false,Descriptor,BESENHashKey(StrReg^.Str));
end;

procedure TBESENCodeContext.OpPUTOBJSET(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var ObjReg,StrReg,ValReg:PBESENValue;
begin
 ObjReg:=@RegisterValues[Operands^[0]];
 StrReg:=@RegisterValues[Operands^[1]];
 ValReg:=@RegisterValues[Operands^[2]];
 TBESENObject(ObjReg^.Obj).DefineOwnPropertyEx(StrReg^.Str,BESENAccessorPropertyDescriptor(nil,TBESENObject(ValReg^.Obj),[bopaENUMERABLE,bopaCONFIGURABLE]),false,Descriptor,BESENHashKey(StrReg^.Str));
end;

procedure TBESENCodeContext.OpINC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num+1;
end;

procedure TBESENCodeContext.OpDEC(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num-1;
end;

procedure TBESENCodeContext.OpCOPYBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool;
end;

procedure TBESENCodeContext.OpCOPYNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=RegisterValues[Operands^[1]].Num;
end;

procedure TBESENCodeContext.OpCOPYSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtSTRING;
 r^.Str:=RegisterValues[Operands^[1]].Str;
end;

procedure TBESENCodeContext.OpCOPYOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtOBJECT;
 r^.Obj:=RegisterValues[Operands^[1]].Obj;
end;

procedure TBESENCodeContext.OpCOPYREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r,a:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 a:=@RegisterValues[Operands^[1]];
 r^.ValueType:=bvtREFERENCE;
 BESENCopyReferenceBaseValue(r^.ReferenceBase,a^.ReferenceBase);
 r^.Str:=a^.Str;
 r^.ReferenceIsStrict:=a^.ReferenceIsStrict;
 r^.ReferenceHash:=a^.ReferenceHash;
 r^.ReferenceIndex:=a^.ReferenceIndex;
 r^.ReferenceID:=a^.ReferenceID;
end;

procedure TBESENCodeContext.OpCOPYLOCAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtLOCAL;
 r^.LocalIndex:=RegisterValues[Operands^[1]].LocalIndex;
end;

procedure TBESENCodeContext.OpGETVALUEREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 GetRefProcs[RegisterValues[Operands^[1]].ReferenceBase.ValueType](RegisterValues[Operands^[1]],RegisterValues[Operands^[0]]);
end;

procedure TBESENCodeContext.OpPUTVALUEREF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 PutRefProcs[RegisterValues[Operands^[0]].ReferenceBase.ValueType](RegisterValues[Operands^[0]],RegisterValues[Operands^[1]]);
end;

procedure TBESENCodeContext.OpGETVALUELOCAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Context.VariableEnvironment.EnvironmentRecord.GetIndexValue(RegisterValues[Operands^[1]].LocalIndex,-1,TBESEN(Instance).IsStrict,RegisterValues[Operands^[0]]);
end;

procedure TBESENCodeContext.OpPUTVALUELOCAL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Context.VariableEnvironment.EnvironmentRecord.SetIndexValue(RegisterValues[Operands^[0]].LocalIndex,-1,RegisterValues[Operands^[1]],TBESEN(Instance).IsStrict);
end;

procedure TBESENCodeContext.OpGETVALUELOCALFAST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(RegisterValues[Operands^[0]],TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[1]].LocalIndex]^);
end;

procedure TBESENCodeContext.OpPUTVALUELOCALFAST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[0]].LocalIndex]^,RegisterValues[Operands^[1]]);
end;

procedure TBESENCodeContext.OpGETVALUELOCALBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtBOOLEAN;
 vp^.Bool:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[1]].LocalIndex]^.Bool;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[0]].LocalIndex];
 hp^.ValueType:=bvtBOOLEAN;
 hp^.Bool:=RegisterValues[Operands^[1]].Bool;
end;

procedure TBESENCodeContext.OpGETVALUELOCALNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtNUMBER;
 vp^.Num:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[1]].LocalIndex].Num;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[0]].LocalIndex];
 hp^.ValueType:=bvtNUMBER;
 hp^.Num:=RegisterValues[Operands^[1]].Num;
end;

procedure TBESENCodeContext.OpGETVALUELOCALSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtSTRING;
 vp^.Str:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[1]].LocalIndex].Str;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[0]].LocalIndex];
 hp^.ValueType:=bvtSTRING;
 hp^.Str:=RegisterValues[Operands^[1]].Str;
end;

procedure TBESENCodeContext.OpGETVALUELOCALOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtOBJECT;
 vp^.Obj:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[1]].LocalIndex].Obj;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[RegisterValues[Operands^[0]].LocalIndex];
 hp^.ValueType:=bvtOBJECT;
 hp^.Obj:=RegisterValues[Operands^[1]].Obj;
 if assigned(hp^.Obj) then begin
  TBESENObject(hp^.Obj).GarbageCollectorWriteBarrier;
 end;
end;

procedure TBESENCodeContext.OpGETVALUELOCALINDEX(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(RegisterValues[Operands^[0]],TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[1]]^);
end;

procedure TBESENCodeContext.OpPUTVALUELOCALINDEX(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValue(TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[0]]^,RegisterValues[Operands^[1]]);
end;

procedure TBESENCodeContext.OpGETVALUELOCALINDEXBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtBOOLEAN;
 vp^.Bool:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[1]]^.Bool;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALINDEXBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[0]];
 hp^.ValueType:=bvtBOOLEAN;
 hp^.Bool:=RegisterValues[Operands^[1]].Bool;
end;

procedure TBESENCodeContext.OpGETVALUELOCALINDEXNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtNUMBER;
 vp^.Num:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[1]].Num;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALINDEXNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[0]];
 hp^.ValueType:=bvtNUMBER;
 hp^.Num:=RegisterValues[Operands^[1]].Num;
end;

procedure TBESENCodeContext.OpGETVALUELOCALINDEXSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtSTRING;
 vp^.Str:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[1]].Str;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALINDEXSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[0]];
 hp^.ValueType:=bvtSTRING;
 hp^.Str:=RegisterValues[Operands^[1]].Str;
end;

procedure TBESENCodeContext.OpGETVALUELOCALINDEXOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var vp:PBESENValue;
begin
 vp:=@RegisterValues[Operands^[0]];
 vp^.ValueType:=bvtOBJECT;
 vp^.Obj:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[1]].Obj;
end;

procedure TBESENCodeContext.OpPUTVALUELOCALINDEXOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var hp:PBESENValue;
begin
 hp:=TBESENDeclarativeEnvironmentRecord(Context.VariableEnvironment.EnvironmentRecord).HashValues[Operands^[0]];
 hp^.ValueType:=bvtOBJECT;
 hp^.Obj:=RegisterValues[Operands^[1]].Obj;
 if assigned(hp^.Obj) then begin
  TBESENObject(hp^.Obj).GarbageCollectorWriteBarrier;
 end;
end;

procedure TBESENCodeContext.OpLOOPINITCOUNT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
end;

procedure TBESENCodeContext.OpLOOPADDCOUNT(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
end;

procedure TBESENCodeContext.OpTRACE(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 Trace(bttSTATEMENT);
 if assigned(TBESEN(Instance).PeriodicHook) then begin
  if not TBESEN(Instance).PeriodicHook(TBESEN(Instance)) then begin
   Running:=false;
   BlockRunning:=false;
  end;
 end;
end;

procedure TBESENCodeContext.OpLTBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool<RegisterValues[Operands^[2]].Bool;
end;

procedure TBESENCodeContext.OpGTBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool>RegisterValues[Operands^[2]].Bool;
end;

procedure TBESENCodeContext.OpLEBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool<=RegisterValues[Operands^[2]].Bool;
end;

procedure TBESENCodeContext.OpGEBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool>=RegisterValues[Operands^[2]].Bool;
end;

procedure TBESENCodeContext.OpEQBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool=RegisterValues[Operands^[2]].Bool;
end;

procedure TBESENCodeContext.OpNEQBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Bool<>RegisterValues[Operands^[2]].Bool;
end;

procedure TBESENCodeContext.OpLTNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 AbstractRelationalNumber(@RegisterValues[Operands^[1]],@RegisterValues[Operands^[2]],r^.Bool);
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(RegisterValues[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num<RegisterValues[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpGTNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@RegisterValues[Operands^[2]];
 b:=@RegisterValues[Operands^[1]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setnbe al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 AbstractRelationalNumber(@RegisterValues[Operands^[2]],@RegisterValues[Operands^[1]],r^.Bool);
{$else}
r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(RegisterValues[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num>RegisterValues[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpLENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@RegisterValues[Operands^[2]];
 b:=@RegisterValues[Operands^[1]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  not al
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setbe al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 if AbstractRelationalNumber(@RegisterValues[Operands^[2]],@RegisterValues[Operands^[1]],r^.Bool) then begin
  r^.Bool:=not r^.Bool;
 end;
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(RegisterValues[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num<=RegisterValues[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpGENUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  not al
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setnb al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 if AbstractRelationalNumber(@RegisterValues[Operands^[1]],@RegisterValues[Operands^[2]],r^.Bool) then begin
  r^.Bool:=not r^.Bool;
 end;
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(RegisterValues[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num>=RegisterValues[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpEQNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef UseSafeOperations}
 r^.Bool:=BESENEqualityExpressionStrictEquals(RegisterValues[Operands^[1]],RegisterValues[Operands^[2]]);
{$else}
{$ifdef cpu386}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setz al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(RegisterValues[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num=RegisterValues[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpNEQNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef UseSafeOperations}
 r^.Bool:=not BESENEqualityExpressionStrictEquals(RegisterValues[Operands^[1]],RegisterValues[Operands^[2]]);
{$else}
{$ifdef cpu386}
 a:=@RegisterValues[Operands^[1]];
 b:=@RegisterValues[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setz al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  not eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$else}
 r^.Bool:=not ((not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(RegisterValues[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num=RegisterValues[Operands^[2]].Num));
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpLTSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Str<RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpGTSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Str>RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpLESTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Str<=RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpGESTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Str>=RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpEQSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Str=RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpNEQSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
 r^.Bool:=RegisterValues[Operands^[1]].Str<>RegisterValues[Operands^[2]].Str;
end;

procedure TBESENCodeContext.OpSHLBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(BESENNumberBooleanValues[RegisterValues[Operands^[1]].Bool] shl BESENNumberBooleanValues[RegisterValues[Operands^[2]].Bool]);
end;

procedure TBESENCodeContext.OpSHRBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(BESENNumberBooleanValues[RegisterValues[Operands^[1]].Bool] shr BESENNumberBooleanValues[RegisterValues[Operands^[2]].Bool]);
end;

procedure TBESENCodeContext.OpBANDBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=BESENNumberBooleanValues[RegisterValues[Operands^[1]].Bool and RegisterValues[Operands^[2]].Bool];
end;

procedure TBESENCodeContext.OpBXORBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=BESENNumberBooleanValues[RegisterValues[Operands^[1]].Bool xor RegisterValues[Operands^[2]].Bool];
end;

procedure TBESENCodeContext.OpBORBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=BESENNumberBooleanValues[RegisterValues[Operands^[1]].Bool or RegisterValues[Operands^[2]].Bool];
end;

procedure TBESENCodeContext.OpSHLNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(BESENToInt32Fast(@RegisterValues[Operands^[1]].Num) shl (BESENToUInt32Fast(@RegisterValues[Operands^[2]].Num) and 31));
end;

procedure TBESENCodeContext.OpSHRNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(sar(BESENToInt32Fast(@RegisterValues[Operands^[1]].Num),(BESENToUInt32Fast(@RegisterValues[Operands^[2]].Num) and 31)));
end;

procedure TBESENCodeContext.OpUSHRNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENUINT32(BESENToUInt32Fast(@RegisterValues[Operands^[1]].Num) shr (BESENToUInt32Fast(@RegisterValues[Operands^[2]].Num) and 31));
end;

procedure TBESENCodeContext.OpBANDNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(BESENToInt32Fast(@RegisterValues[Operands^[1]].Num) and BESENToInt32Fast(@RegisterValues[Operands^[2]].Num));
end;

procedure TBESENCodeContext.OpBXORNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(BESENToInt32Fast(@RegisterValues[Operands^[1]].Num) xor BESENToInt32Fast(@RegisterValues[Operands^[2]].Num));
end;

procedure TBESENCodeContext.OpBORNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtNUMBER;
 r^.Num:=TBESENINT32(BESENToInt32Fast(@RegisterValues[Operands^[1]].Num) or BESENToInt32Fast(@RegisterValues[Operands^[2]].Num));
end;

procedure TBESENCodeContext.OpSETCUNDEF(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 ResultValue.ValueType:=bvtUNDEFINED;
end;

procedure TBESENCodeContext.OpSETCNULL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 ResultValue.ValueType:=bvtNULL;
end;

procedure TBESENCodeContext.OpSETCBOOL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=RegisterValues[Operands^[0]].Bool;
end;

procedure TBESENCodeContext.OpSETCNUM(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 ResultValue.ValueType:=bvtNUMBER;
 ResultValue.Num:=RegisterValues[Operands^[0]].Num;
end;

procedure TBESENCodeContext.OpSETCSTR(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 ResultValue.ValueType:=bvtSTRING;
 ResultValue.Str:=RegisterValues[Operands^[0]].Str;
end;

procedure TBESENCodeContext.OpSETCOBJ(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 ResultValue.ValueType:=bvtOBJECT;
 ResultValue.Obj:=RegisterValues[Operands^[0]].Obj;
end;

procedure TBESENCodeContext.OpTRACENEW(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var Counter,CountArguments:integer;
    OldIsStrict:boolean;
    ConstructorValue:PBESENValue;
begin
 OldIsStrict:=TBESEN(Instance).IsStrict;
 try
  ConstructorValue:=@RegisterValues[Operands^[1]];
  CountArguments:=Operands^[2];
  for Counter:=0 to CountArguments-1 do begin
   ParamArgs[Counter]:=@RegisterValues[Operands^[Counter+3]];
  end;
  if ConstructorValue^.ValueType<>bvtOBJECT then begin
   BESENThrowTypeErrorNotAConstructorObject;
  end else if not TBESENObject(ConstructorValue^.Obj).HasConstruct then begin
   BESENThrowTypeErrorObjectHasNoConstruct;
  end;
  if Trace(bttCALL) then begin
   TBESEN(Instance).GarbageCollector.TriggerCollect;
   TBESEN(Instance).ObjectConstruct(TBESENObject(ConstructorValue^.Obj),BESENUndefinedValue,@ParamArgs[0],CountArguments,RegisterValues[Operands^[0]]);
   Trace(bttRETURN);
  end else begin
   RegisterValues[Operands^[0]].ValueType:=bvtUNDEFINED;
  end;
 finally
  TBESEN(Instance).IsStrict:=OldIsStrict;
 end;
end;

procedure TBESENCodeContext.OpTRACECALL(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var Counter,CountArguments:integer;
    Ref,Func:PBESENValue;
    OldIsStrict,DirectCall:boolean;
begin
 OldIsStrict:=TBESEN(Instance).IsStrict;
 try
  Ref:=@RegisterValues[Operands^[1]];
  Func:=@RegisterValues[Operands^[2]];
  CountArguments:=Operands^[3];
  for Counter:=0 to CountArguments-1 do begin
   ParamArgs[Counter]:=@RegisterValues[Operands^[Counter+4]];
  end;
  if Func^.ValueType<>bvtOBJECT then begin
   BESENThrowTypeErrorNotAFunction;
  end else if not (assigned(Func^.Obj) and TBESENObject(Func^.Obj).HasCall) then begin
   BESENThrowTypeErrorNotCallable;
  end;
  if Ref^.ValueType=bvtREFERENCE then begin
   BESENRefBaseValueToCallThisArgValueProcs[Ref^.ReferenceBase.ValueType](CallThisArg,Ref^.ReferenceBase);
  end else begin
   CallThisArg.ValueType:=bvtUNDEFINED;
  end;
  if Trace(bttCALL) then begin
   if Func^.Obj=TBESEN(Instance).ObjectGlobalEval then begin
    DirectCall:=((Ref^.ValueType=bvtREFERENCE) and (Ref^.ReferenceBase.ValueType=brbvtENVREC)) and TBESENEnvironmentRecord(Ref^.ReferenceBase.EnvRec).HasBindingEx('eval',Descriptor);
    TBESEN(Instance).GlobalEval(Context,CallThisArg,@ParamArgs[0],CountArguments,DirectCall,RegisterValues[Operands^[0]]);
   end else begin
    TBESEN(Instance).ObjectCall(TBESENObject(Func^.Obj),CallThisArg,@ParamArgs[0],CountArguments,RegisterValues[Operands^[0]]);
   end;
   Trace(bttRETURN);
  end;
 finally
  TBESEN(Instance).IsStrict:=OldIsStrict;
 end;
end;

procedure TBESENCodeContext.OpLTNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 AbstractRelationalNumber(@RegisterValues[Operands^[1]],@Code.Literals[Operands^[2]],r^.Bool);
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(Code.Literals[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num<Code.Literals[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpGTNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@Code.Literals[Operands^[2]];
 b:=@RegisterValues[Operands^[1]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setnbe al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 AbstractRelationalNumber(@Code.Literals[Operands^[2]],@RegisterValues[Operands^[1]],r^.Bool);
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(Code.Literals[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num>Code.Literals[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpLENUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@Code.Literals[Operands^[2]];
 b:=@RegisterValues[Operands^[1]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  not al
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setbe al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 if AbstractRelationalNumber(@Code.Literals[Operands^[2]],@RegisterValues[Operands^[1]],r^.Bool) then begin
  r^.Bool:=not r^.Bool;
 end;
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(Code.Literals[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num<=Code.Literals[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpGENUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef cpu386}
{$ifdef UseSafeOperations}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  push edi
  mov edi,dword ptr a
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  mov al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  shl al,4
  mov edi,dword ptr b
  mov ecx,dword ptr [edi+TBESENValue.Num+4]
  mov edx,ecx
  and edx,$0000ffff
  or edx,dword ptr [edi+TBESENValue.Num]
  setnz dl
  and edx,$7f
  shr ecx,16
  or al,byte ptr [offset BESENNumberCodeFlagLookupTable+ecx*2+edx]
  and eax,$ff
  lea ecx,dword ptr [offset BESENNumberCodeAbstractRelationalLookupTable+eax*4]
  mov edi,dword ptr a
  fld qword ptr [edi+TBESENValue.Num]
  mov edi,dword ptr b
  fcomp qword ptr [edi+TBESENValue.Num]
  fstsw ax
  sahf
  setb al
  neg al
  sbb eax,eax
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.DoCompare]
  or al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.ResultBooleanValue]
  not al
  and al,byte ptr [ecx+TBESENNumberCodeAbstractRelationalLookupTableItem.IsNotUndefined]
  cmp al,$01
  cmc
  sbb eax,eax
  mov edi,dword ptr r
  mov dword ptr [edi+TBESENValue.Bool],eax
  pop edi
 end {$ifdef fpc}['eax','ecx','edx','edi']{$endif};
{$else}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setnb al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$endif}
{$else}
{$ifdef UseSafeOperations}
 if AbstractRelationalNumber(@RegisterValues[Operands^[1]],@Code.Literals[Operands^[2]],r^.Bool) then begin
  r^.Bool:=not r^.Bool;
 end;
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(Code.Literals[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num>=Code.Literals[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpEQNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef UseSafeOperations}
 r^.Bool:=BESENEqualityExpressionStrictEquals(RegisterValues[Operands^[1]],Code.Literals[Operands^[2]]);
{$else}
{$ifdef cpu386}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setz al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$else}
 r^.Bool:=(not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(Code.Literals[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num=Code.Literals[Operands^[2]].Num);
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpNEQNUMCONST(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
var r{$ifdef cpu386},a,b{$endif}:PBESENValue;
begin
 r:=@RegisterValues[Operands^[0]];
 r^.ValueType:=bvtBOOLEAN;
{$ifdef UseSafeOperations}
 r^.Bool:=not BESENEqualityExpressionStrictEquals(RegisterValues[Operands^[1]],Code.Literals[Operands^[2]]);
{$else}
{$ifdef cpu386}
 a:=@RegisterValues[Operands^[1]];
 b:=@Code.Literals[Operands^[2]];
 asm
  mov edx,dword ptr a
  fld qword ptr [edx+TBESENValue.Num]
  mov edx,dword ptr b
  fcomp qword ptr [edx+TBESENValue.Num]
  fstsw ax
  sahf
  setz al
  setnp cl
  and al,cl
  neg al
  sbb eax,eax
  not eax
  mov edx,dword ptr r
  mov dword ptr [edx+TBESENValue.Bool],eax
 end {$ifdef fpc}['eax','ecx','edx']{$endif};
{$else}
 r^.Bool:=not ((not (BESENIsNaN(RegisterValues[Operands^[1]].Num) or BESENIsNaN(Code.Literals[Operands^[2]].Num))) and (RegisterValues[Operands^[1]].Num=Code.Literals[Operands^[2]].Num));
{$endif}
{$endif}
end;

procedure TBESENCodeContext.OpJZERO(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 if (not BESENIsNaN(RegisterValues[Operands^[1]].Num)) and (RegisterValues[Operands^[1]].Num=0) then begin
  PC:=Operands^[0];
 end;
end;

procedure TBESENCodeContext.OpJNZERO(Operands:PBESENINT32Array); {$ifdef UseRegister}register;{$endif}
begin
 if not ((not BESENIsNaN(RegisterValues[Operands^[1]].Num)) and (RegisterValues[Operands^[1]].Num=0)) then begin
  PC:=Operands^[0];
 end;
end;                                   

{$ifndef PurePascal}
{$define PurePascalExecuteByteCode}

{$ifdef cpu386}
{$undef PurePascalExecuteByteCode}
function TBESENCodeContext.ExecuteByteCode:TBESENBoolean; assembler; {$ifdef UseRegister}register;{$endif}
asm
 push ebx
 push esi
 push edi
  mov esi,self
  mov dword ptr [esi+TBESENCodeContext.BlockRunning],$ffffffff
  mov edi,dword ptr [esi+TBESENCodeContext.Code]
  mov edi,dword ptr [edi+TBESENCode.ByteCode]
  jmp @LoopStart
   @LoopBegin:
    mov ebx,dword ptr [esi+TBESENCodeContext.PC]
    lea edx,dword ptr [edi+ebx*4+4]
    mov ebx,dword ptr [edi+ebx*4]
    mov ecx,ebx
    and ecx,$ff
    shr ebx,8
    inc ebx
    add dword ptr [esi+TBESENCodeContext.PC],ebx
    mov eax,esi
    call dword ptr [ecx*4+offset BESENCodeContextOpcodes]
  @LoopStart:
   cmp dword ptr [esi+TBESENCodeContext.BlockRunning],0
   jnz @LoopBegin
  mov eax,dword ptr [esi+TBESENCodeContext.BlockRunning]
 pop edi
 pop esi
 pop ebx
end;
{$endif}

{$ifdef cpuamd64}
{$ifdef windows}
{$undef PurePascalExecuteByteCode}
function TBESENCodeContext.ExecuteByteCode:TBESENBoolean; {$ifdef UseRegister}register;{$endif}
begin
 BlockRunning:=true;
 asm
  mov rsi,self
  mov rdi,qword ptr [rsi+TBESENCodeContext.Code]
  mov rdi,qword ptr [rdi+TBESENCode.ByteCode]
  jmp @LoopStart
   @LoopBegin:
    xor rbx,rbx
    mov ebx,dword ptr [rsi+TBESENCodeContext.PC]
    lea rdx,qword ptr [rdi+rbx*4+4]
    mov ebx,dword ptr [rdi+rbx*4]
    mov rax,rbx
    and eax,$ff
    shr ebx,8
    inc ebx
    add dword ptr [rsi+TBESENCodeContext.PC],ebx
    mov rcx,rsi
    mov rbx,offset BESENCodeContextOpcodes
    call qword ptr [rbx+rax*8]
  @LoopStart:
   cmp dword ptr [rsi+TBESENCodeContext.BlockRunning],0
   jnz @LoopBegin
 end ['rax','rbx','rcx','rdx','rsi','rdi'];
 result:=BlockRunning;
end;
{$else}
{$undef PurePascalExecuteByteCode}
function TBESENCodeContext.ExecuteByteCode:TBESENBoolean; {$ifdef UseRegister}register;{$endif}
begin
 BlockRunning:=true;
 asm
  mov r12,self
  mov r13,qword ptr [r12+TBESENCodeContext.Code]
  mov r13,qword ptr [r13+TBESENCode.ByteCode]
  jmp @LoopStart
   @LoopBegin:
    xor rbx,rbx
    mov ebx,dword ptr [r12+TBESENCodeContext.PC]
    lea rsi,qword ptr [r13+rbx*4+4]
    mov ebx,dword ptr [r13+rbx*4]
    mov rax,rbx
    and eax,$ff
    shr ebx,8
    inc ebx
    add dword ptr [r12+TBESENCodeContext.PC],ebx
    mov rdi,r12
    mov rbx,offset BESENCodeContextOpcodes
    call qword ptr [rbx+rax*8]
  @LoopStart:
   cmp dword ptr [r12+TBESENCodeContext.BlockRunning],0
   jnz @LoopBegin
 end ['rax','rbx','rcx','rdx','rsi','rdi','r8','r9','r10','r11','r12','r13'];
 result:=BlockRunning;
end;
{$endif}
{$endif}
{$endif}

{$ifdef PurePascalExecuteByteCode}
function TBESENCodeContext.ExecuteByteCode:TBESENBoolean;
var Handler:TBESENCodeContextOpcode;
    Instruction:longword;
    ByteCode:PBESENUINT32Array;
    Operands:PBESENINT32Array;
begin
 BlockRunning:=true;
 ByteCode:=@Code.ByteCode[0];
 while BlockRunning do begin
  Instruction:=ByteCode^[PC];
  Operands:=@ByteCode^[PC+1];
  inc(PC,1+(Instruction shr 8));
  TMethod(Handler).Code:=BESENCodeContextOpcodes[Instruction and $ff];
  TMethod(Handler).Data:=self;
  Handler(Operands);
 end;
 result:=BlockRunning;
end;
{$endif}

function TBESENCodeContext.ExecuteCode:TBESENBoolean;
begin
 if PC<TBESENUINT32(Code.ByteCodeLen) then begin
{$ifdef HasJIT}
  if assigned(Code.NativeCode) then begin
   result:=BESENExecuteNativeCode(self);
  end else begin
   result:=ExecuteByteCode;
  end;
{$else}
  result:=ExecuteByteCode;
{$endif}
  if assigned(TBESEN(Instance).PeriodicHook) then begin
   if not TBESEN(Instance).PeriodicHook(TBESEN(Instance)) then begin
    Running:=false;
    BlockRunning:=false;
    result:=false;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

procedure TBESENCodeContext.ExecuteTryBlockLevel;
var Reraise:boolean;
    ExceptionValue:TBESENValue;
 function ProcessException:boolean;
 begin
  result:=true;
  while BlockLevel>0 do begin
   case Blocks[BlockLevel-1].BlockType of
    bccbtENUM:begin
     dec(BlockLevel);
     Block:=@Blocks[BlockLevel];
     PropertyEnumerator:=Block^.OldPropertyEnumerator;
     EnumBlock:=Block^.OldEnumBlock;
     BESENFreeAndNil(Block.PropertyEnumerator);
    end;
    bccbtWITH:begin
     dec(BlockLevel);
     Block:=@Blocks[BlockLevel];
     Context.LexicalEnvironment:=Block^.LexicalEnvironment.Outer;
    end;
    bccbtCATCH:begin
     Block:=@Blocks[BlockLevel-1];
     Block^.Value:=ExceptionValue;
     PC:=Block^.Handler;
     result:=false;
     break;
    end;
    bccbtCATCH2:begin
     dec(BlockLevel);
     Block:=@Blocks[BlockLevel];
     Context.LexicalEnvironment:=Block^.LexicalEnvironment.Outer;
     Block^.Ident:='';
    end;
    bccbtFINALLY:begin
     Block:=@Blocks[BlockLevel-1];
     Block^.BlockType:=bccbtFINALLY2;
     Block^.Resume:=$ffffffff;
     Block^.Raised:=true;
     PC:=Block^.Handler;
     ExecuteTryBlockLevel;
    end;
    bccbtFINALLY2:begin
     dec(BlockLevel);
    end;
   end;
  end;
 end;
begin
 ExceptionValue:=BESENEmptyValue;
 Reraise:=false;
 while Running do begin
  try
   if not ExecuteCode then begin
    break;
   end;
  except
   on e:EBESENThrowException do begin
    ExceptionValue:=E.Value;
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENEvalError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectEvalErrorConstructor,e.OriginalMessage);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENRangeError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectRangeErrorConstructor,e.OriginalMessage);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENReferenceError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectReferenceErrorConstructor,e.OriginalMessage);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENSyntaxError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectSyntaxErrorConstructor,e.OriginalMessage);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENTypeError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectTypeErrorConstructor,e.OriginalMessage);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENURIError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectURIErrorConstructor,e.OriginalMessage);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:EBESENError do begin
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectErrorConstructor,e.OriginalMessage,E.Name);
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   on e:Exception do begin
{$ifdef Delphi2009AndUp}
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectErrorConstructor,e.Message);
{$else}
    ExceptionValue:=ConstructError(TBESEN(Instance).ObjectErrorConstructor,BESENUTF8ToUTF16(BESENConvertToUTF8(e.Message)));
{$endif}
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end else begin
     E.Message:='';
    end;
   end;
   else begin
    ExceptionValue:=BESENEmptyValue;
    Reraise:=ProcessException;
    if Reraise then begin
     raise;
    end;
   end;
  end;
 end;
end;

procedure TBESENCodeContext.Execute(const AContext:TBESENContext;var AResult:TBESENValue);
var i:integer;
    IsDeclarativeEnvironmentRecord:boolean;
    EnvironmentRecord:TBESENEnvironmentRecord;
{$ifndef BESENNoFPU}
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
{$endif}
begin
 Context:=AContext;
 ResultValue.ValueType:=bvtUNDEFINED;
 PropertyEnumerator:=nil;
 EnumBlock:=nil;
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
  for i:=0 to length(RegisterValues)-1 do begin
   RegisterValues[i].ValueType:=bvtUNDEFINED;
  end;
  if Code.Body.EnableLocalsOptimization then begin
   EnvironmentRecord:=Context.VariableEnvironment.EnvironmentRecord;
   IsDeclarativeEnvironmentRecord:=EnvironmentRecord is TBESENDeclarativeEnvironmentRecord;
   if (IsDeclarativeEnvironmentRecord and not TBESENDeclarativeEnvironmentRecord(EnvironmentRecord).IndexInitialized) or not IsDeclarativeEnvironmentRecord then begin
    if IsDeclarativeEnvironmentRecord then begin
     TBESENDeclarativeEnvironmentRecord(EnvironmentRecord).IndexInitialized:=true;
    end;
    for i:=0 to length(Code.Variables)-1 do begin
     if not EnvironmentRecord.SetBindingValueIndex(Code.Variables[i].Name,i) then begin
      BESENThrowInternalError('Internal error: 201003160136-0000');
     end;
    end;
   end;
  end;
  Running:=true;
  Str:='';
  PC:=0;
  BlockLevel:=0;
  Block:=@Blocks[0];
{$ifdef HasJIT}
  if not assigned(Code.NativeCode) then begin
   if not BESENGenerateNativeCode(self) then begin
    if assigned(Code.NativeCode) then begin
     TBESEN(Instance).NativeCodeMemoryManager.FreeMemory(Code.NativeCode);
     Code.NativeCode:=nil;
     Code.NativeCodeSize:=0;
    end;
   end;
  end;
{$endif}
  LookupNames:=Code.LookupNames.FList;
  ExecuteTryBlockLevel;
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
  Context:=nil;
 end;
 BesenCopyValue(AResult,ResultValue);
end;

procedure InitOpcodes;
var i:integer;
begin
 fillchar(BESENCodeContextOpcodes,sizeof(TBESENCodeContextOpcodePointers),#0);
 for i:=0 to 255 do begin
  BESENCodeContextOpcodes[i]:=@TBESENCodeContext.OpNOP;
 end;
 BESENCodeContextOpcodes[bopSTOP]:=@TBESENCodeContext.OpSTOP;
 BESENCodeContextOpcodes[bopNEW]:=@TBESENCodeContext.OpNEW;
 BESENCodeContextOpcodes[bopCALL]:=@TBESENCodeContext.OpCALL;
 BESENCodeContextOpcodes[bopEND]:=@TBESENCodeContext.OpEND;
 BESENCodeContextOpcodes[bopVREF]:=@TBESENCodeContext.OpVREF;
 BESENCodeContextOpcodes[bopLREF]:=@TBESENCodeContext.OpLREF;
 BESENCodeContextOpcodes[bopNOP]:=@TBESENCodeContext.OpNOP;
 BESENCodeContextOpcodes[bopCOPY]:=@TBESENCodeContext.OpCOPY;
 BESENCodeContextOpcodes[bopNEQ]:=@TBESENCodeContext.OpNEQ;
 BESENCodeContextOpcodes[bopNSEQ]:=@TBESENCodeContext.OpNSEQ;
 BESENCodeContextOpcodes[bopAREF]:=@TBESENCodeContext.OpAREF;
 BESENCodeContextOpcodes[bopTHROW]:=@TBESENCodeContext.OpTHROW;
 BESENCodeContextOpcodes[bopSETC]:=@TBESENCodeContext.OpSETC;
 BESENCodeContextOpcodes[bopGETC]:=@TBESENCodeContext.OpGETC;
 BESENCodeContextOpcodes[bopTHIS]:=@TBESENCodeContext.OpTHIS;
 BESENCodeContextOpcodes[bopOBJECT]:=@TBESENCodeContext.OpOBJECT;
 BESENCodeContextOpcodes[bopARRAY]:=@TBESENCodeContext.OpARRAY;
 BESENCodeContextOpcodes[bopREGEXP]:=@TBESENCodeContext.OpREGEXP;
 BESENCodeContextOpcodes[bopREF]:=@TBESENCodeContext.OpREF;
 BESENCodeContextOpcodes[bopGETVALUE]:=@TBESENCodeContext.OpGETVALUE;
 BESENCodeContextOpcodes[bopLOOKUP]:=@TBESENCodeContext.OpLOOKUP;
 BESENCodeContextOpcodes[bopPUTVALUE]:=@TBESENCodeContext.OpPUTVALUE;
 BESENCodeContextOpcodes[bopDELETE]:=@TBESENCodeContext.OpDELETE;
 BESENCodeContextOpcodes[bopTYPEOF]:=@TBESENCodeContext.OpTYPEOF;
 BESENCodeContextOpcodes[bopTOOBJECT]:=@TBESENCodeContext.OpTOOBJECT;
 BESENCodeContextOpcodes[bopTONUMBER]:=@TBESENCodeContext.OpTONUMBER;
 BESENCodeContextOpcodes[bopTOBOOLEAN]:=@TBESENCodeContext.OpTOBOOLEAN;
 BESENCodeContextOpcodes[bopTOSTRING]:=@TBESENCodeContext.OpTOSTRING;
 BESENCodeContextOpcodes[bopTOPRIMITIVE]:=@TBESENCodeContext.OpTOPRIMITIVE;
 BESENCodeContextOpcodes[bopNEG]:=@TBESENCodeContext.OpNEG;
 BESENCodeContextOpcodes[bopINV]:=@TBESENCodeContext.OpINV;
 BESENCodeContextOpcodes[bopNOT]:=@TBESENCodeContext.OpNOT;
 BESENCodeContextOpcodes[bopMUL]:=@TBESENCodeContext.OpMUL;
 BESENCodeContextOpcodes[bopDIV]:=@TBESENCodeContext.OpDIV;
 BESENCodeContextOpcodes[bopMOD]:=@TBESENCodeContext.OpMOD;
 BESENCodeContextOpcodes[bopADD]:=@TBESENCodeContext.OpADD;
 BESENCodeContextOpcodes[bopADDNUM]:=@TBESENCodeContext.OpADDNUM;
 BESENCodeContextOpcodes[bopSUB]:=@TBESENCodeContext.OpSUB;
 BESENCodeContextOpcodes[bopSHL]:=@TBESENCodeContext.OpSHL;
 BESENCodeContextOpcodes[bopSHR]:=@TBESENCodeContext.OpSHR;
 BESENCodeContextOpcodes[bopUSHR]:=@TBESENCodeContext.OpUSHR;
 BESENCodeContextOpcodes[bopLT]:=@TBESENCodeContext.OpLT;
 BESENCodeContextOpcodes[bopGT]:=@TBESENCodeContext.OpGT;
 BESENCodeContextOpcodes[bopLE]:=@TBESENCodeContext.OpLE;
 BESENCodeContextOpcodes[bopGE]:=@TBESENCodeContext.OpGE;
 BESENCodeContextOpcodes[bopINSTANCEOF]:=@TBESENCodeContext.OpINSTANCEOF;
 BESENCodeContextOpcodes[bopIN]:=@TBESENCodeContext.OpIN;
 BESENCodeContextOpcodes[bopEQ]:=@TBESENCodeContext.OpEQ;
 BESENCodeContextOpcodes[bopSEQ]:=@TBESENCodeContext.OpSEQ;
 BESENCodeContextOpcodes[bopBAND]:=@TBESENCodeContext.OpBAND;
 BESENCodeContextOpcodes[bopBXOR]:=@TBESENCodeContext.OpBXOR;
 BESENCodeContextOpcodes[bopBOR]:=@TBESENCodeContext.OpBOR;
 BESENCodeContextOpcodes[bopSENUM]:=@TBESENCodeContext.OpSENUM;
 BESENCodeContextOpcodes[bopSWITH]:=@TBESENCodeContext.OpSWITH;
 BESENCodeContextOpcodes[bopSCATCH]:=@TBESENCodeContext.OpSCATCH;
 BESENCodeContextOpcodes[bopENDF]:=@TBESENCodeContext.OpENDF;
 BESENCodeContextOpcodes[bopJMP]:=@TBESENCodeContext.OpJMP;
 BESENCodeContextOpcodes[bopJZ]:=@TBESENCodeContext.OpJZ;
 BESENCodeContextOpcodes[bopJNZ]:=@TBESENCodeContext.OpJNZ;
 BESENCodeContextOpcodes[bopJNULL]:=@TBESENCodeContext.OpJNULL;
 BESENCodeContextOpcodes[bopLOOPENUM]:=@TBESENCodeContext.OpLOOPENUM;
 BESENCodeContextOpcodes[bopSTRYC]:=@TBESENCodeContext.OpSTRYC;
 BESENCodeContextOpcodes[bopSTRYF]:=@TBESENCodeContext.OpSTRYF;
 BESENCodeContextOpcodes[bopLITERAL]:=@TBESENCodeContext.OpLITERAL;
 BESENCodeContextOpcodes[bopLITERALUNDEF]:=@TBESENCodeContext.OpLITERALUNDEF;
 BESENCodeContextOpcodes[bopLITERALNULL]:=@TBESENCodeContext.OpLITERALNULL;
 BESENCodeContextOpcodes[bopLITERALBOOL]:=@TBESENCodeContext.OpLITERALBOOL;
 BESENCodeContextOpcodes[bopLITERALNUM]:=@TBESENCodeContext.OpLITERALNUM;
 BESENCodeContextOpcodes[bopLITERALSTR]:=@TBESENCodeContext.OpLITERALSTR;
 BESENCodeContextOpcodes[bopLITERALOBJ]:=@TBESENCodeContext.OpLITERALOBJ;
 BESENCodeContextOpcodes[bopFUNC]:=@TBESENCodeContext.OpFUNC;
 BESENCodeContextOpcodes[bopLINE]:=@TBESENCodeContext.OpLINE;
 BESENCodeContextOpcodes[bopGC]:=@TBESENCodeContext.OpGC;
 BESENCodeContextOpcodes[bopSTRICT]:=@TBESENCodeContext.OpSTRICT;
 BESENCodeContextOpcodes[bopSTRICTCHECKREF]:=@TBESENCodeContext.OpSTRICTCHECKREF;
 BESENCodeContextOpcodes[bopDEBUGGER]:=@TBESENCodeContext.OpDEBUGGER;
 BESENCodeContextOpcodes[bopCHECKOBJECTCOERCIBLE]:=@TBESENCodeContext.OpCHECKOBJECTCOERCIBLE;
 BESENCodeContextOpcodes[bopPUTOBJVALUE]:=@TBESENCodeContext.OpPUTOBJVALUE;
 BESENCodeContextOpcodes[bopPUTOBJGET]:=@TBESENCodeContext.OpPUTOBJGET;
 BESENCodeContextOpcodes[bopPUTOBJSET]:=@TBESENCodeContext.OpPUTOBJSET;
 BESENCodeContextOpcodes[bopINC]:=@TBESENCodeContext.OpINC;
 BESENCodeContextOpcodes[bopDEC]:=@TBESENCodeContext.OpDEC;
 BESENCodeContextOpcodes[bopCOPYBOOL]:=@TBESENCodeContext.OpCOPYBOOL;
 BESENCodeContextOpcodes[bopCOPYNUM]:=@TBESENCodeContext.OpCOPYNUM;
 BESENCodeContextOpcodes[bopCOPYSTR]:=@TBESENCodeContext.OpCOPYSTR;
 BESENCodeContextOpcodes[bopCOPYOBJ]:=@TBESENCodeContext.OpCOPYOBJ;
 BESENCodeContextOpcodes[bopCOPYREF]:=@TBESENCodeContext.OpCOPYREF;
 BESENCodeContextOpcodes[bopCOPYLOCAL]:=@TBESENCodeContext.OpCOPYLOCAL;
 BESENCodeContextOpcodes[bopGETVALUEREF]:=@TBESENCodeContext.OpGETVALUEREF;
 BESENCodeContextOpcodes[bopPUTVALUEREF]:=@TBESENCodeContext.OpPUTVALUEREF;
 BESENCodeContextOpcodes[bopGETVALUELOCAL]:=@TBESENCodeContext.OpGETVALUELOCAL;
 BESENCodeContextOpcodes[bopPUTVALUELOCAL]:=@TBESENCodeContext.OpPUTVALUELOCAL;
 BESENCodeContextOpcodes[bopGETVALUELOCALFAST]:=@TBESENCodeContext.OpGETVALUELOCALFAST;
 BESENCodeContextOpcodes[bopPUTVALUELOCALFAST]:=@TBESENCodeContext.OpPUTVALUELOCALFAST;
 BESENCodeContextOpcodes[bopGETVALUELOCALBOOL]:=@TBESENCodeContext.OpGETVALUELOCALBOOL;
 BESENCodeContextOpcodes[bopPUTVALUELOCALBOOL]:=@TBESENCodeContext.OpPUTVALUELOCALBOOL;
 BESENCodeContextOpcodes[bopGETVALUELOCALNUM]:=@TBESENCodeContext.OpGETVALUELOCALNUM;
 BESENCodeContextOpcodes[bopPUTVALUELOCALNUM]:=@TBESENCodeContext.OpPUTVALUELOCALNUM;
 BESENCodeContextOpcodes[bopGETVALUELOCALSTR]:=@TBESENCodeContext.OpGETVALUELOCALSTR;
 BESENCodeContextOpcodes[bopPUTVALUELOCALSTR]:=@TBESENCodeContext.OpPUTVALUELOCALSTR;
 BESENCodeContextOpcodes[bopGETVALUELOCALOBJ]:=@TBESENCodeContext.OpGETVALUELOCALOBJ;
 BESENCodeContextOpcodes[bopPUTVALUELOCALOBJ]:=@TBESENCodeContext.OpPUTVALUELOCALOBJ;
 BESENCodeContextOpcodes[bopGETVALUELOCALINDEX]:=@TBESENCodeContext.OpGETVALUELOCALINDEX;
 BESENCodeContextOpcodes[bopPUTVALUELOCALINDEX]:=@TBESENCodeContext.OpPUTVALUELOCALINDEX;
 BESENCodeContextOpcodes[bopGETVALUELOCALINDEXBOOL]:=@TBESENCodeContext.OpGETVALUELOCALINDEXBOOL;
 BESENCodeContextOpcodes[bopPUTVALUELOCALINDEXBOOL]:=@TBESENCodeContext.OpPUTVALUELOCALINDEXBOOL;
 BESENCodeContextOpcodes[bopGETVALUELOCALINDEXNUM]:=@TBESENCodeContext.OpGETVALUELOCALINDEXNUM;
 BESENCodeContextOpcodes[bopPUTVALUELOCALINDEXNUM]:=@TBESENCodeContext.OpPUTVALUELOCALINDEXNUM;
 BESENCodeContextOpcodes[bopGETVALUELOCALINDEXSTR]:=@TBESENCodeContext.OpGETVALUELOCALINDEXSTR;
 BESENCodeContextOpcodes[bopPUTVALUELOCALINDEXSTR]:=@TBESENCodeContext.OpPUTVALUELOCALINDEXSTR;
 BESENCodeContextOpcodes[bopGETVALUELOCALINDEXOBJ]:=@TBESENCodeContext.OpGETVALUELOCALINDEXOBJ;
 BESENCodeContextOpcodes[bopPUTVALUELOCALINDEXOBJ]:=@TBESENCodeContext.OpPUTVALUELOCALINDEXOBJ;
 BESENCodeContextOpcodes[bopLOOPINITCOUNT]:=@TBESENCodeContext.OpLOOPINITCOUNT;
 BESENCodeContextOpcodes[bopLOOPADDCOUNT]:=@TBESENCodeContext.OpLOOPADDCOUNT;
 BESENCodeContextOpcodes[bopTRACE]:=@TBESENCodeContext.OpTRACE;
 BESENCodeContextOpcodes[bopLTBOOL]:=@TBESENCodeContext.OpLTBOOL;
 BESENCodeContextOpcodes[bopGTBOOL]:=@TBESENCodeContext.OpGTBOOL;
 BESENCodeContextOpcodes[bopLEBOOL]:=@TBESENCodeContext.OpLEBOOL;
 BESENCodeContextOpcodes[bopGEBOOL]:=@TBESENCodeContext.OpGEBOOL;
 BESENCodeContextOpcodes[bopEQBOOL]:=@TBESENCodeContext.OpEQBOOL;
 BESENCodeContextOpcodes[bopNEQBOOL]:=@TBESENCodeContext.OpNEQBOOL;
 BESENCodeContextOpcodes[bopLTNUM]:=@TBESENCodeContext.OpLTNUM;
 BESENCodeContextOpcodes[bopGTNUM]:=@TBESENCodeContext.OpGTNUM;
 BESENCodeContextOpcodes[bopLENUM]:=@TBESENCodeContext.OpLENUM;
 BESENCodeContextOpcodes[bopGENUM]:=@TBESENCodeContext.OpGENUM;
 BESENCodeContextOpcodes[bopEQNUM]:=@TBESENCodeContext.OpEQNUM;
 BESENCodeContextOpcodes[bopNEQNUM]:=@TBESENCodeContext.OpNEQNUM;
 BESENCodeContextOpcodes[bopLTSTR]:=@TBESENCodeContext.OpLTSTR;
 BESENCodeContextOpcodes[bopGTSTR]:=@TBESENCodeContext.OpGTSTR;
 BESENCodeContextOpcodes[bopLESTR]:=@TBESENCodeContext.OpLESTR;
 BESENCodeContextOpcodes[bopGESTR]:=@TBESENCodeContext.OpGESTR;
 BESENCodeContextOpcodes[bopEQSTR]:=@TBESENCodeContext.OpEQSTR;
 BESENCodeContextOpcodes[bopNEQSTR]:=@TBESENCodeContext.OpNEQSTR;
 BESENCodeContextOpcodes[bopSHLBOOL]:=@TBESENCodeContext.OpSHLBOOL;
 BESENCodeContextOpcodes[bopSHRBOOL]:=@TBESENCodeContext.OpSHRBOOL;
 BESENCodeContextOpcodes[bopBANDBOOL]:=@TBESENCodeContext.OpBANDBOOL;
 BESENCodeContextOpcodes[bopBXORBOOL]:=@TBESENCodeContext.OpBXORBOOL;
 BESENCodeContextOpcodes[bopBORBOOL]:=@TBESENCodeContext.OpBORBOOL;
 BESENCodeContextOpcodes[bopSHLNUM]:=@TBESENCodeContext.OpSHLNUM;
 BESENCodeContextOpcodes[bopSHRNUM]:=@TBESENCodeContext.OpSHRNUM;
 BESENCodeContextOpcodes[bopUSHRNUM]:=@TBESENCodeContext.OpUSHRNUM;
 BESENCodeContextOpcodes[bopBANDNUM]:=@TBESENCodeContext.OpBANDNUM;
 BESENCodeContextOpcodes[bopBXORNUM]:=@TBESENCodeContext.OpBXORNUM;
 BESENCodeContextOpcodes[bopBORNUM]:=@TBESENCodeContext.OpBORNUM;
 BESENCodeContextOpcodes[bopSETCUNDEF]:=@TBESENCodeContext.OpSETCUNDEF;
 BESENCodeContextOpcodes[bopSETCNULL]:=@TBESENCodeContext.OpSETCNULL;
 BESENCodeContextOpcodes[bopSETCBOOL]:=@TBESENCodeContext.OpSETCBOOL;
 BESENCodeContextOpcodes[bopSETCNUM]:=@TBESENCodeContext.OpSETCNUM;
 BESENCodeContextOpcodes[bopSETCSTR]:=@TBESENCodeContext.OpSETCSTR;
 BESENCodeContextOpcodes[bopSETCOBJ]:=@TBESENCodeContext.OpSETCOBJ;
 BESENCodeContextOpcodes[bopTRACENEW]:=@TBESENCodeContext.OpTRACENEW;
 BESENCodeContextOpcodes[bopTRACECALL]:=@TBESENCodeContext.OpTRACECALL;
 BESENCodeContextOpcodes[bopLTNUMCONST]:=@TBESENCodeContext.OpLTNUMCONST;
 BESENCodeContextOpcodes[bopGTNUMCONST]:=@TBESENCodeContext.OpGTNUMCONST;
 BESENCodeContextOpcodes[bopLENUMCONST]:=@TBESENCodeContext.OpLENUMCONST;
 BESENCodeContextOpcodes[bopGENUMCONST]:=@TBESENCodeContext.OpGENUMCONST;
 BESENCodeContextOpcodes[bopEQNUMCONST]:=@TBESENCodeContext.OpEQNUMCONST;
 BESENCodeContextOpcodes[bopNEQNUMCONST]:=@TBESENCodeContext.OpNEQNUMCONST;
 BESENCodeContextOpcodes[bopJZERO]:=@TBESENCodeContext.OpJZERO;
 BESENCodeContextOpcodes[bopJNZERO]:=@TBESENCodeContext.OpJNZERO;
end;

procedure InitBESEN;
begin
 InitOpcodes;
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;
end.
