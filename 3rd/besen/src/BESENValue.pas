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
unit BESENValue;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENStringUtils,BESENCharSet,Variants;

const brbvtUNDEFINED=0;
      brbvtBOOLEAN=1;
      brbvtNUMBER=2;
      brbvtSTRING=3;
      brbvtOBJECT=4;
      brbvtENVREC=5;

      brbvtFIRST=brbvtUNDEFINED;
      brbvtLAST=brbvtENVREC;

      bvtUNDEFINED=0;
      bvtNULL=1;
      bvtBOOLEAN=2;
      bvtNUMBER=3;
      bvtSTRING=4;
      bvtOBJECT=5;
      bvtREFERENCE=6;
      bvtLOCAL=7;
      bvtENVREC=8;
      bvtNONE=9;

      bvtFIRST=bvtUNDEFINED;
      bvtLAST=bvtNONE;

type TBESENReferenceBaseValueType=ptruint;

     PBESENReferenceBaseValue=^TBESENReferenceBaseValue;
     TBESENReferenceBaseValue=record
      Str:TBESENString;
{$ifdef BESENEmbarcaderoNextGen}
      Obj:TObject;
      EnvRec:TObject;
{$endif}
      case ValueType:TBESENReferenceBaseValueType of
       brbvtUNDEFINED:(
       );
       brbvtBOOLEAN:(
        Bool:TBESENBoolean;
       );
       brbvtNUMBER:(
        Num:TBESENNumber;
       );
       brbvtSTRING:(
       );
       brbvtOBJECT:(
{$ifndef BESENEmbarcaderoNextGen}
        Obj:TObject;
{$endif}
       );
       brbvtENVREC:(
{$ifndef BESENEmbarcaderoNextGen}
        EnvRec:TObject;
{$endif}
       );
     end;

     TBESENValueType=ptruint;

     PBESENValue=^TBESENValue;
     TBESENValue=record
      Str:TBESENString;
      ReferenceBase:TBESENReferenceBaseValue;
{$ifdef BESENEmbarcaderoNextGen}
      Obj:TObject;
      EnvRec:TObject;
{$endif}
      case ValueType:TBESENValueType of
       bvtUNDEFINED:(
       );
       bvtNULL:(
       );
       bvtBOOLEAN:(
        Bool:TBESENBoolean;
       );
       bvtNUMBER:(
        Num:TBESENNumber;
       );
       bvtSTRING:(
       );
       bvtOBJECT:(
{$ifndef BESENEmbarcaderoNextGen}
        Obj:TObject;
{$endif}
       );
       bvtREFERENCE:(
        ReferenceIsStrict:longbool;
        ReferenceHash:TBESENHash;
        ReferenceIndex:TBESENINT32;
        ReferenceID:TBESENINT32;
       );
       bvtLOCAL:(
        LocalIndex:TBESENINT32;
       );
       bvtENVREC:(
{$ifndef BESENEmbarcaderoNextGen}
        EnvRec:TObject;
{$endif}
       );
       bvtNONE:(
       );
     end;

     TBESENValueTypes=array of TBESENValueType;

     TBESENValueTypesItems=array of TBESENValueTypes;
     
     TBESENValues=array of TBESENValue;

     TBESENValuePointers=array of PBESENValue;

     PPBESENValues=^TPBESENValues;
     TPBESENValues=array[0..($7fffffff div sizeof(PBESENValue))-1] of PBESENValue;

     TBESENPointerToValues=array of PBESENValue;

     TBESENCopyReferenceBaseValueProc=procedure(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

     TBESENCopyReferenceBaseValueProcs=array[brbvtFIRST..brbvtLAST] of TBESENCopyReferenceBaseValueProc;

     TBESENCopyValueProc=procedure(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}

     TBESENCopyValueProcs=array[bvtFIRST..bvtLAST] of TBESENCopyValueProc;

     TBESENValueToRefBaseValueProc=procedure(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}

     TBESENValueToRefBaseValueProcs=array[bvtFIRST..bvtLAST] of TBESENValueToRefBaseValueProc;

     TBESENRefBaseValueToValueProc=procedure(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

     TBESENRefBaseValueToValueProcs=array[brbvtFIRST..brbvtLAST] of TBESENRefBaseValueToValueProc;

     TBESENRefBaseValueToCallThisArgValueProc=procedure(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue);

     TBESENRefBaseValueToCallThisArgValueProcs=array[brbvtFIRST..brbvtLAST] of TBESENRefBaseValueToCallThisArgValueProc;

procedure BESENCopyReferenceBaseValueUndefined(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyReferenceBaseValueBoolean(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyReferenceBaseValueNumber(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyReferenceBaseValueString(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyReferenceBaseValueObject(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyReferenceBaseValueEnvRec(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyReferenceBaseValueNone(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

procedure BESENCopyReferenceBaseValue(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

procedure BESENCopyValueUndefined(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueNull(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueBoolean(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueNumber(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueString(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueObject(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueReference(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueLocal(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueEnvRec(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENCopyValueNone(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}

procedure BESENCopyValue(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}

procedure BESENValueToRefBaseValueUndefined(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueNull(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueBoolean(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueNumber(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueString(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueObject(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueReference(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueLocal(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueEnvRec(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
procedure BESENValueToRefBaseValueNone(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}

procedure BESENValueToReferenceBaseValue(const Value:TBESENValue;var AResult:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

procedure BESENRefBaseValueToValueUndefined(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToValueBoolean(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToValueNumber(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToValueString(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToValueObject(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToValueEnvRec(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

procedure BESENReferenceBaseValueToValue(const Value:TBESENReferenceBaseValue;var AResult:TBESENValue); {$ifdef UseRegister}register;{$endif}

procedure BESENRefBaseValueToCallThisArgValueUndefined(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToCallThisArgValueBoolean(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToCallThisArgValueNumber(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToCallThisArgValueString(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToCallThisArgValueObject(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
procedure BESENRefBaseValueToCallThisArgValueEnvRec(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}

const BESENCopyReferenceBaseValueProcs:TBESENCopyReferenceBaseValueProcs=(BESENCopyReferenceBaseValueUndefined,
                                                                          BESENCopyReferenceBaseValueBoolean,
                                                                          BESENCopyReferenceBaseValueNumber,
                                                                          BESENCopyReferenceBaseValueString,
                                                                          BESENCopyReferenceBaseValueObject,
                                                                          BESENCopyReferenceBaseValueEnvRec);

      BESENCopyValueProcs:TBESENCopyValueProcs=(BESENCopyValueUndefined,
                                                BESENCopyValueNull,
                                                BESENCopyValueBoolean,
                                                BESENCopyValueNumber,
                                                BESENCopyValueString,
                                                BESENCopyValueObject,
                                                BESENCopyValueReference,
                                                BESENCopyValueLocal,
                                                BESENCopyValueEnvRec,
                                                BESENCopyValueNone);

      BESENValueToRefBaseValueProcs:TBESENValueToRefBaseValueProcs=(BESENValueToRefBaseValueUndefined,
                                                                    BESENValueToRefBaseValueNull,
                                                                    BESENValueToRefBaseValueBoolean,
                                                                    BESENValueToRefBaseValueNumber,
                                                                    BESENValueToRefBaseValueString,
                                                                    BESENValueToRefBaseValueObject,
                                                                    BESENValueToRefBaseValueReference,
                                                                    BESENValueToRefBaseValueLocal,
                                                                    BESENValueToRefBaseValueEnvRec,
                                                                    BESENValueToRefBaseValueNone);

      BESENRefBaseValueToValueProcs:TBESENRefBaseValueToValueProcs=(BESENRefBaseValueToValueUndefined,
                                                                    BESENRefBaseValueToValueBoolean,
                                                                    BESENRefBaseValueToValueNumber,
                                                                    BESENRefBaseValueToValueString,
                                                                    BESENRefBaseValueToValueObject,
                                                                    BESENRefBaseValueToValueEnvRec);

      BESENRefBaseValueToCallThisArgValueProcs:TBESENRefBaseValueToCallThisArgValueProcs=(BESENRefBaseValueToCallThisArgValueUndefined,
                                                                                          BESENRefBaseValueToCallThisArgValueBoolean,
                                                                                          BESENRefBaseValueToCallThisArgValueNumber,
                                                                                          BESENRefBaseValueToCallThisArgValueString,
                                                                                          BESENRefBaseValueToCallThisArgValueObject,
                                                                                          BESENRefBaseValueToCallThisArgValueEnvRec);

function BESENValueToVariant(const v:TBESENValue):Variant;
procedure BESENVariantToValue(const vt:Variant;var v:TBESENValue);

function BESENBooleanValue(const Bool:TBESENBoolean):TBESENValue;
function BESENNumberValue(const Num:TBESENNumber):TBESENValue;
function BESENStringValue(const Str:TBESENString):TBESENValue;
{$ifndef BESENSingleStringType}
function BESENStringLocaleCharsetValue(const Str:TBESENAnsiString):TBESENValue;
{$endif}
function BESENObjectValue(const Obj:TObject):TBESENValue;
function BESENObjectValueEx(const Obj:TObject):TBESENValue;

function BESENEqualityExpressionStrictEquals(const a,b:TBESENValue):longbool; 

var BESENEmptyValue:TBESENValue;
    BESENNullValue:TBESENValue;
    BESENUndefinedValue:TBESENValue;
    BESENDummyValue:TBESENValue;

implementation

uses BESEN,BESENNumberUtils,BESENEnvironmentRecord;

procedure BESENCopyReferenceBaseValueUndefined(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtUNDEFINED;
end;

procedure BESENCopyReferenceBaseValueBoolean(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtBOOLEAN;
 Dest.Bool:=Src.Bool;
end;

procedure BESENCopyReferenceBaseValueNumber(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtNUMBER;
 Dest.Num:=Src.Num;
end;

procedure BESENCopyReferenceBaseValueString(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtSTRING;
 Dest.Str:=Src.Str;
end;

procedure BESENCopyReferenceBaseValueObject(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtOBJECT;
 Dest.Obj:=Src.Obj;
end;

procedure BESENCopyReferenceBaseValueEnvRec(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtENVREC;
 Dest.EnvRec:=Src.EnvRec;
end;

procedure BESENCopyReferenceBaseValueNone(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtNONE;
end;

procedure BESENCopyReferenceBaseValue(var Dest:TBESENReferenceBaseValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyReferenceBaseValueProcs[Src.ValueType](Dest,Src);
end;

procedure BESENCopyValueUndefined(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtUNDEFINED;
end;

procedure BESENCopyValueNull(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtNULL;
end;

procedure BESENCopyValueBoolean(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtBOOLEAN;
 Dest.Bool:=Src.Bool;
end;

procedure BESENCopyValueNumber(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtNUMBER;
 Dest.Num:=Src.Num;
end;

procedure BESENCopyValueString(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtSTRING;
 Dest.Str:=Src.Str;
end;

procedure BESENCopyValueObject(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtOBJECT;
 Dest.Obj:=Src.Obj;
end;

procedure BESENCopyValueReference(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtREFERENCE;
 BESENCopyReferenceBaseValue(Dest.ReferenceBase,Src.ReferenceBase);
 Dest.Str:=Src.Str;
 Dest.ReferenceIsStrict:=Src.ReferenceIsStrict;
 Dest.ReferenceHash:=Src.ReferenceHash;
 Dest.ReferenceIndex:=Src.ReferenceIndex;
 Dest.ReferenceID:=Src.ReferenceID;
end;

procedure BESENCopyValueLocal(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtLOCAL;
 Dest.LocalIndex:=Src.LocalIndex;
end;

procedure BESENCopyValueEnvRec(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtENVREC;
 Dest.EnvRec:=Src.EnvRec;
end;

procedure BESENCopyValueNone(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtNONE;
end;

procedure BESENCopyValue(var Dest:TBESENValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 BESENCopyValueProcs[Src.ValueType](Dest,Src);
end;

procedure BESENValueToRefBaseValueUndefined(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtUNDEFINED;
end;

procedure BESENValueToRefBaseValueNull(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtUNDEFINED;
end;

procedure BESENValueToRefBaseValueBoolean(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtBOOLEAN;
 Dest.Bool:=Src.Bool;
end;

procedure BESENValueToRefBaseValueNumber(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtNUMBER;
 Dest.Num:=Src.Num;
end;

procedure BESENValueToRefBaseValueString(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtSTRING;
 Dest.Str:=Src.Str;
end;

procedure BESENValueToRefBaseValueObject(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtOBJECT;
 Dest.Obj:=Src.Obj;
end;

procedure BESENValueToRefBaseValueReference(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtUNDEFINED;
end;

procedure BESENValueToRefBaseValueLocal(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtUNDEFINED;
end;

procedure BESENValueToRefBaseValueEnvRec(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtENVREC;
 Dest.EnvRec:=Src.EnvRec;
end;

procedure BESENValueToRefBaseValueNone(var Dest:TBESENReferenceBaseValue;const Src:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=brbvtUNDEFINED;
end;

procedure BESENValueToReferenceBaseValue(const Value:TBESENValue;var AResult:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 BESENValueToRefBaseValueProcs[Value.ValueType](AResult,Value);
end;

procedure BESENRefBaseValueToValueUndefined(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtUNDEFINED;
end;

procedure BESENRefBaseValueToValueBoolean(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtBOOLEAN;
 Dest.Bool:=Src.Bool;
end;

procedure BESENRefBaseValueToValueNumber(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtNUMBER;
 Dest.Num:=Src.Num;
end;

procedure BESENRefBaseValueToValueString(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtSTRING;
 Dest.Str:=Src.Str;
end;

procedure BESENRefBaseValueToValueObject(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtOBJECT;
 Dest.Obj:=Src.Obj;
end;

procedure BESENRefBaseValueToValueEnvRec(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtENVREC;
 Dest.EnvRec:=Src.EnvRec;
end;

procedure BESENReferenceBaseValueToValue(const Value:TBESENReferenceBaseValue;var AResult:TBESENValue); {$ifdef UseRegister}register;{$endif}
begin
 BESENRefBaseValueToValueProcs[Value.ValueType](AResult,Value);
end;

procedure BESENRefBaseValueToCallThisArgValueUndefined(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtUNDEFINED;
end;

procedure BESENRefBaseValueToCallThisArgValueBoolean(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtBOOLEAN;
 Dest.Bool:=Src.Bool;
end;

procedure BESENRefBaseValueToCallThisArgValueNumber(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtNUMBER;
 Dest.Num:=Src.Num;
end;

procedure BESENRefBaseValueToCallThisArgValueString(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtSTRING;
 Dest.Str:=Src.Str;
end;

procedure BESENRefBaseValueToCallThisArgValueObject(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
begin
 Dest.ValueType:=bvtOBJECT;
 Dest.Obj:=Src.Obj;
end;

procedure BESENRefBaseValueToCallThisArgValueEnvRec(var Dest:TBESENValue;const Src:TBESENReferenceBaseValue); {$ifdef UseRegister}register;{$endif}
var ImplicitThisValue:PBESENValue;
begin
 ImplicitThisValue:=@TBESENEnvironmentRecord(Src.EnvRec).ImplicitThisValue;
 Dest.ValueType:=ImplicitThisValue.ValueType;
 Dest.Obj:=ImplicitThisValue.Obj;
end;

function BESENValueToVariant(const v:TBESENValue):Variant;
begin
 case v.ValueType of
  bvtNULL:begin
   result:=Variants.Null;
  end;
  bvtBOOLEAN:begin
   result:=V.Bool;
  end;
  bvtSTRING:begin
   result:=V.Str;
  end;
  bvtNUMBER:begin
   result:=V.Num;
  end;
  else begin
   result:=Variants.Unassigned;
  end;
 end;
end;

procedure BESENVariantToValue(const vt:Variant;var v:TBESENValue);
begin
 try
  case VarType(vt) of
   varNull:begin
    V.ValueType:=bvtNULL;
   end;
   varSmallInt,varInteger,varShortInt,varByte,varWord,varLongWord,varInt64{$ifdef fpc},varQWord{$endif}:begin
    V.ValueType:=bvtNUMBER;
    V.Num:=vt;
   end;
   varSingle,varDouble,varDATE,varCurrency:begin
    V.ValueType:=bvtNUMBER;
    V.Num:=vt;
   end;
   varBoolean:begin
    V.ValueType:=bvtBOOLEAN;
    V.Bool:=vt;
   end;
   varString,varOleStr:begin
    V.ValueType:=bvtSTRING;
    V.Str:=vt;
   end;
   else begin
    V.ValueType:=bvtUNDEFINED;
   end;
  end;
 except
  V.ValueType:=bvtUNDEFINED;
 end;
end;

function BESENBooleanValue(const Bool:TBESENBoolean):TBESENValue;
begin
 result.ValueType:=bvtBOOLEAN;
 result.Bool:=Bool;
end;

function BESENNumberValue(const Num:TBESENNumber):TBESENValue;
begin
 result.ValueType:=bvtNUMBER;
 result.Num:=Num;
end;

function BESENStringValue(const Str:TBESENString):TBESENValue;
begin
 result.ValueType:=bvtSTRING;
 result.Str:=Str;
end;

{$ifndef BESENSingleStringType}
function BESENStringLocaleCharsetValue(const Str:TBESENAnsiString):TBESENValue;
begin
 result.ValueType:=bvtSTRING;
 result.Str:=BESENUTF8ToUTF16(BESENEncodeString(Str,BESENLocaleCharset,UTF_8));
end;
{$endif}

function BESENObjectValue(const Obj:TObject):TBESENValue;
begin
 result.ValueType:=bvtOBJECT;
 result.Obj:=Obj;
end;

function BESENObjectValueEx(const Obj:TObject):TBESENValue;
begin
 if assigned(Obj) then begin
  result.ValueType:=bvtOBJECT;
  result.Obj:=Obj;
 end else begin
  result:=BESENNullValue;
 end;
end;

function BESENObjectValueEx2(const Obj:TObject):TBESENValue;
begin
 if assigned(Obj) then begin
  result.ValueType:=bvtOBJECT;
  result.Obj:=Obj;
 end else begin
  result:=BESENUndefinedValue;
 end;
end;

function BESENEqualityExpressionStrictEquals(const a,b:TBESENValue):longbool;
begin
 if a.ValueType<>b.ValueType then begin
  result:=false;
 end else begin
  case a.ValueType of
   bvtUNDEFINED:begin
    result:=true;
   end;
   bvtNULL:begin
    result:=true;
   end;
   bvtNUMBER:begin
{$ifdef UseSafeOperations}
    if BESENIsNaN(a.Num) then begin
     result:=false;
    end else if BESENIsNaN(b.Num) then begin
     result:=false;
    end else begin
     result:=(a.Num=b.Num) or (BESENIsZero(a.Num) and BESENIsZero(b.Num));
    end;
{$else}
    result:=(not (BESENIsNaN(a.Num) or BESENIsNaN(b.Num))) and (a.Num=b.Num);
{$endif}
   end;
   bvtSTRING:begin
    result:=a.Str=b.Str;
   end;
   bvtBOOLEAN:begin
    result:=a.Bool=b.Bool;
   end;
   bvtOBJECT:begin
    result:=a.Obj=b.Obj;
   end;
   else begin
    result:=false;
   end;
  end;
 end;
end;

procedure InitBESEN;
begin
 fillchar(BESENEmptyValue,sizeof(TBESENValue),#0);
 fillchar(BESENNullValue,sizeof(TBESENValue),#0);
 fillchar(BESENUndefinedValue,sizeof(TBESENValue),#0);
 BESENEmptyValue.ValueType:=bvtUNDEFINED;
 BESENNullValue.ValueType:=bvtNULL;
 BESENUndefinedValue.ValueType:=bvtUNDEFINED;
 BESENDummyValue:=BESENEmptyValue;
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;
end.
