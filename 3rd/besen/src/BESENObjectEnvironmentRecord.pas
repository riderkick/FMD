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
unit BESENObjectEnvironmentRecord;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENEnvironmentRecord,BESENObject,
     BESENObjectPropertyDescriptor,BESENValue;

type TBESENObjectEnvironmentRecord=class(TBESENEnvironmentRecord)
      public
       BindingObject:TBESENObject;
       ProvideThis:TBESENBoolean;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function HasBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       function CreateMutableBinding(const N:TBESENString;const D:TBESENBoolean=false;Hash:TBESENHash=0):TBESENBoolean; override;
       function SetMutableBindingEx(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0):TBESENBoolean; override;
       procedure GetBindingValueEx(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0); override;
       function DeleteBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       procedure UpdateImplicitThisValue; override;
       function SetIndexValue(const I,ID:integer;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean; override;
       procedure GetIndexValue(const I,ID:integer;const S:TBESENBoolean;var R:TBESENValue); override;
       function DeleteIndex(const I,ID:integer):TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENStringUtils,BESENErrors;

constructor TBESENObjectEnvironmentRecord.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 BindingObject:=nil;
 ProvideThis:=false;
 RecordType:=BESENEnvironmentRecordTypeObject;
end;

destructor TBESENObjectEnvironmentRecord.Destroy;
begin
 BindingObject:=nil;
 inherited Destroy;
end;

function TBESENObjectEnvironmentRecord.HasBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 result:=BindingObject.HasPropertyEx(N,Descriptor,Hash);
end;

function TBESENObjectEnvironmentRecord.CreateMutableBinding(const N:TBESENString;const D:TBESENBoolean=false;Hash:TBESENHash=0):TBESENBoolean;
begin
 if BindingObject.HasProperty(N,Hash) then begin
  BESENThrowTypeError('CreateMutableBinding for "'+N+'" failed');
 end;
 if D then begin
  result:=BindingObject.DefineOwnProperty(N,BESENDataPropertyDescriptor(BESENUndefinedValue,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),true,Hash); // ES5 errata false to true
 end else begin
  result:=BindingObject.DefineOwnProperty(N,BESENDataPropertyDescriptor(BESENUndefinedValue,[bopaWRITABLE,bopaENUMERABLE]),true,Hash); // ES5 errata false to true
 end;
end;

function TBESENObjectEnvironmentRecord.SetMutableBindingEx(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0):TBESENBoolean;
begin
 BindingObject.PutEx(N,V,S,Descriptor,OwnDescriptor,TempValue,Hash);
 result:=true;
end;

procedure TBESENObjectEnvironmentRecord.GetBindingValueEx(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0);
 procedure ThrowIt;
 begin
  BESENThrowTypeError('GetBindingValue for "'+N+'" failed');
 end;
begin
 if not BindingObject.GetEx(N,R,Descriptor,BindingObject,Hash) then begin
  if S then begin
   ThrowIt;
  end else begin
   R.ValueType:=bvtUNDEFINED;
  end;
 end;
end;

function TBESENObjectEnvironmentRecord.DeleteBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 result:=BindingObject.DeleteEx(N,false,Descriptor,Hash);
end;

procedure TBESENObjectEnvironmentRecord.UpdateImplicitThisValue;
begin
 if ProvideThis then begin
  ImplicitThisValue.ValueType:=bvtOBJECT;
  ImplicitThisValue.Obj:=BindingObject;
 end else begin
  ImplicitThisValue.ValueType:=bvtUNDEFINED;
  ImplicitThisValue.Obj:=nil;
 end;
end;

function TBESENObjectEnvironmentRecord.SetIndexValue(const I,ID:integer;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean;
begin
 BindingObject.PutIndex(I,ID,V,S);
 result:=true;
end;

procedure TBESENObjectEnvironmentRecord.GetIndexValue(const I,ID:integer;const S:TBESENBoolean;var R:TBESENValue);
 procedure ThrowIt;
 begin
  BESENThrowTypeError('GetIndexValue failed');
 end;
begin
 if not BindingObject.GetIndex(I,ID,R,BindingObject) then begin
  if S then begin
   ThrowIt;
  end else begin
   R.ValueType:=bvtUNDEFINED;
  end;
 end;
end;

function TBESENObjectEnvironmentRecord.DeleteIndex(const I,ID:integer):TBESENBoolean;
begin
 result:=BindingObject.DeleteIndex(I,iD,false);
end;

procedure TBESENObjectEnvironmentRecord.Finalize;
begin
 BindingObject:=nil;
 inherited Finalize;
end;

procedure TBESENObjectEnvironmentRecord.Mark;
begin
 TBESEN(Instance).GarbageCollector.GrayIt(BindingObject);
 inherited Mark;
end;

end.
