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
unit BESENEnvironmentRecord;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENObjectPropertyDescriptor,
     BESENSelfBalancedTree,BESENHashMap,
     BESENGarbageCollector;

const BESENEnvironmentRecordTypeDeclarative=0;
      BESENEnvironmentRecordTypeObject=longword($ffffffff);
      
type TBESENEnvironmentRecord=class(TBESENGarbageCollectorObject)
      public
       IsStrict:TBESENBoolean;
       HasMaybeDirectEval:TBESENBoolean;
       ImplicitThisValue:TBESENValue;
       RecordType:TBESENUINT32;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function HasBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function HasBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function CreateMutableBinding(const N:TBESENString;const D:TBESENBoolean=false;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function SetMutableBindingEx(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0):TBESENBoolean; virtual;
       procedure GetBindingValueEx(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0); virtual;
       function SetMutableBinding(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;Hash:TBESENHash=0):TBESENBoolean; virtual;
       procedure GetBindingValue(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;Hash:TBESENHash=0); virtual;
       function DeleteBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function DeleteBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean; virtual;
       procedure UpdateImplicitThisValue; virtual;
       function CreateImmutableBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function InitializeImmutableBinding(const N:TBESENString;const V:TBESENValue;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function SetBindingValueIndex(const N:TBESENString;const I:integer;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function SetIndexValue(const I,ID:integer;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean; virtual;
       procedure GetIndexValue(const I,ID:integer;const S:TBESENBoolean;var R:TBESENValue); virtual;
       function DeleteIndex(const I,ID:integer):TBESENBoolean; virtual;
       function SetArrayIndexValue(const I:TBESENUINT32;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean; virtual;
       procedure GetArrayIndexValue(const I:TBESENUINT32;const S:TBESENBoolean;var R:TBESENValue); virtual;
       function DeleteArrayIndex(const I:TBESENUINT32):TBESENBoolean; virtual;
     end;

implementation

uses BESEN,BESENArrayUtils,BESENHashUtils,BESENErrors;

constructor TBESENEnvironmentRecord.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 IsStrict:=TBESEN(Instance).IsStrict;
 HasMaybeDirectEval:=true;
 ImplicitThisValue:=BESENUndefinedValue;
end;

destructor TBESENEnvironmentRecord.Destroy;
begin
 inherited Destroy;
end;

{$warnings off}
function TBESENEnvironmentRecord.HasBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0000');
end;

function TBESENEnvironmentRecord.HasBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean;
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 result:=HasBindingEx(N,Descriptor,Hash);
end;

function TBESENEnvironmentRecord.CreateMutableBinding(const N:TBESENString;const D:TBESENBoolean=false;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0001');
end;

function TBESENEnvironmentRecord.SetMutableBindingEx(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0002');
end;

procedure TBESENEnvironmentRecord.GetBindingValueEx(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0);
begin
 raise EBESENInternalError.Create('201003160116-0003');
end;

function TBESENEnvironmentRecord.SetMutableBinding(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;Hash:TBESENHash=0):TBESENBoolean;
var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;
    Temp:TBESENValue;
begin
 result:=SetMutableBindingEx(N,V,S,Descriptor,OwnDescriptor,Temp,Hash);
end;

procedure TBESENEnvironmentRecord.GetBindingValue(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;Hash:TBESENHash=0);
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 GetBindingValueEx(N,S,R,Descriptor,Hash);
end;

function TBESENEnvironmentRecord.DeleteBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0004');
end;

function TBESENEnvironmentRecord.DeleteBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean;
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 result:=DeleteBindingEx(N,Descriptor,Hash);
end;

procedure TBESENEnvironmentRecord.UpdateImplicitThisValue;
begin
 raise EBESENInternalError.Create('201003160116-0005');
end;

function TBESENEnvironmentRecord.CreateImmutableBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0006');
end;

function TBESENEnvironmentRecord.InitializeImmutableBinding(const N:TBESENString;const V:TBESENValue;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0007');
end;

function TBESENEnvironmentRecord.SetBindingValueIndex(const N:TBESENString;const I:integer;Hash:TBESENHash=0):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0008');
end;

function TBESENEnvironmentRecord.SetIndexValue(const I,ID:integer;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0009');
end;

procedure TBESENEnvironmentRecord.GetIndexValue(const I,ID:integer;const S:TBESENBoolean;var R:TBESENValue);
begin
 raise EBESENInternalError.Create('201003160116-0010');
end;

function TBESENEnvironmentRecord.DeleteIndex(const I,ID:integer):TBESENBoolean;
begin
 raise EBESENInternalError.Create('201003160116-0011');
end;

function TBESENEnvironmentRecord.SetArrayIndexValue(const I:TBESENUINT32;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean;
var N:TBESENString;
begin
 N:=BESENArrayIndexToStr(I);
 result:=SetMutableBinding(N,V,S,BESENHashKey(N));
 N:='';
end;

procedure TBESENEnvironmentRecord.GetArrayIndexValue(const I:TBESENUINT32;const S:TBESENBoolean;var R:TBESENValue);
var N:TBESENString;
begin
 N:=BESENArrayIndexToStr(I);
 GetBindingValue(N,S,R,BESENHashKey(N));
 N:='';
end;

function TBESENEnvironmentRecord.DeleteArrayIndex(const I:TBESENUINT32):TBESENBoolean;
var N:TBESENString;
begin
 N:=BESENArrayIndexToStr(I);
 result:=DeleteBinding(N,BESENHashKey(N));
 N:='';
end;
{$warnings on}

end.
