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
unit BESENDeclarativeEnvironmentRecord;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENEnvironmentRecord,BESENValue,
     BESENPointerList,BESENStringList,BESENIntegerList,
     BESENObjectPropertyDescriptor,BESENStringUtils;

type PBESENDeclarativeEnvironmentRecordHashItem=^TBESENDeclarativeEnvironmentRecordHashItem;
     TBESENDeclarativeEnvironmentRecordHashItem=record
      Previous,Next,HashPrevious,HashNext:PBESENDeclarativeEnvironmentRecordHashItem;
      Hash:TBESENHash;
      Mutable:TBESENBoolean;
      Initialized:TBESENBoolean;
      Deletion:TBESENBoolean;
      Key:TBESENString;
      Value:TBESENValue;
      Index:integer;
     end;

     TBESENDeclarativeEnvironmentRecordHashBucket=record
      HashFirst,HashLast:PBESENDeclarativeEnvironmentRecordHashItem;
     end;

     TBESENDeclarativeEnvironmentRecordHashBuckets=array of TBESENDeclarativeEnvironmentRecordHashBucket;

     TBESENDeclarativeEnvironmentRecordValues=array of PBESENValue;

     TBESENDeclarativeEnvironmentRecord=class(TBESENEnvironmentRecord)
      private
       LastUsedItem:PBESENDeclarativeEnvironmentRecordHashItem;
       procedure Clear;
       procedure GrowAndRehashIfNeeded;
      public
       First,Last:PBESENDeclarativeEnvironmentRecordHashItem;
       HashBuckets:TBESENDeclarativeEnvironmentRecordHashBuckets;
       HashSize:longword;
       HashSizeMask:longword;
       HashedItems:longword;
       HashBucketsUsed:longword;
       HashIndexes:TBESENPointerList;
       HashIndexNames:TBESENStringList;
       HashIndexIDs:TBESENIntegerList;
       HashValues:TBESENDeclarativeEnvironmentRecordValues;
       Touched:TBESENBoolean;
       IndexInitialized:TBESENBoolean;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       procedure Reset;
       function GetKey(const Key:TBESENString;Hash:TBESENHash=0):PBESENDeclarativeEnvironmentRecordHashItem;
       function NewKey(const Key:TBESENString;Force:boolean=false;Hash:TBESENHash=0):PBESENDeclarativeEnvironmentRecordHashItem;
       function DeleteKey(const Item:PBESENDeclarativeEnvironmentRecordHashItem):TBESENBoolean;
       function HasBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       function CreateMutableBinding(const N:TBESENString;const D:TBESENBoolean=false;Hash:TBESENHash=0):TBESENBoolean; override;
       function SetMutableBindingEx(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0):TBESENBoolean; override;
       procedure GetBindingValueEx(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0); override;
       function DeleteBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       procedure UpdateImplicitThisValue; override;
       function CreateImmutableBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean; override;
       function InitializeImmutableBinding(const N:TBESENString;const V:TBESENValue;Hash:TBESENHash=0):TBESENBoolean; override;
       function SetBindingValueIndex(const N:TBESENString;const I:integer;Hash:TBESENHash=0):TBESENBoolean; override;
       function SetIndexValue(const I,ID:integer;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean; override;
       procedure GetIndexValue(const I,ID:integer;const S:TBESENBoolean;var R:TBESENValue); override;
       function DeleteIndex(const I,ID:integer):TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENHashUtils,BESENErrors;

constructor TBESENDeclarativeEnvironmentRecord.Create(AInstance:TObject);
var Hash:TBESENHash;
begin
 inherited Create(AInstance);
 FillChar(HashBuckets,sizeof(TBESENDeclarativeEnvironmentRecordHashBuckets),#0);
 First:=nil;
 Last:=nil;
 HashSize:=256;
 HashSizeMask:=HashSize-1;
 HashedItems:=0;
 HashBucketsUsed:=0;
 SetLength(HashBuckets,HashSize);
 for Hash:=0 to HashSizeMask do begin
  HashBuckets[Hash].HashFirst:=nil;
  HashBuckets[Hash].HashLast:=nil;
 end;
 HashIndexes:=TBESENPointerList.Create;
 HashIndexNames:=TBESENStringList.Create;
 HashIndexIDs:=TBESENIntegerList.Create;
 HashValues:=nil;
 Touched:=false;
 IndexInitialized:=false;
 LastUsedItem:=nil;
 RecordType:=BESENEnvironmentRecordTypeDeclarative;
end;

destructor TBESENDeclarativeEnvironmentRecord.Destroy;
begin
 Clear;
 SetLength(HashValues,0);
 SetLength(HashBuckets,0);
 HashIndexes.Free;
 HashIndexNames.Free;
 HashIndexIDs.Free;
 inherited Destroy;
end;

procedure TBESENDeclarativeEnvironmentRecord.Clear;
var Hash:TBESENHash;
    Item,NextItem:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 Item:=First;
 while assigned(Item) do begin
  NextItem:=Item^.Next;
  Item^.Previous:=nil;
  Item^.Next:=nil;
  Item^.Key:='';
  Item^.Value.ValueType:=bvtUNDEFINED;
  Dispose(Item);
  Item:=NextItem;
 end;
 First:=nil;
 Last:=nil;
 LastUsedItem:=nil;
 HashSize:=256;
 HashSizeMask:=HashSize-1;
 HashedItems:=0;
 HashBucketsUsed:=0;
 SetLength(HashBuckets,HashSize);
 for Hash:=0 to HashSizeMask do begin
  HashBuckets[Hash].HashFirst:=nil;
  HashBuckets[Hash].HashLast:=nil;
 end;
 HashIndexes.Clear;
 HashIndexNames.Clear;
 HashIndexIDs.Clear;
 SetLength(HashValues,0);
 Touched:=false;
 IndexInitialized:=false;
end;

procedure TBESENDeclarativeEnvironmentRecord.Reset;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 Item:=First;
 while assigned(Item) do begin
  Item^.Value.ValueType:=bvtUNDEFINED;
  Item^.Initialized:=Item^.Mutable;
  Item:=Item^.Next;
 end;
 Touched:=false;
end;

procedure TBESENDeclarativeEnvironmentRecord.GrowAndRehashIfNeeded;
var Hash:TBESENHash;
    Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 if (HashSize<BESENHashMaxSize) and (HashedItems>=(HashBucketsUsed*BESENHashItemsPerBucketsThreshold)) then begin
  LastUsedItem:=nil;
  for Hash:=0 to HashSizeMask do begin
   HashBuckets[Hash].HashFirst:=nil;
   HashBuckets[Hash].HashLast:=nil;
  end;
  inc(HashSize,HashSize);
  if HashSize>BESENHashMaxSize then begin
   HashSize:=BESENHashMaxSize;
  end;
  HashSizeMask:=HashSize-1;
  SetLength(HashBuckets,HashSize);
  for Hash:=0 to HashSizeMask do begin
   HashBuckets[Hash].HashFirst:=nil;
   HashBuckets[Hash].HashLast:=nil;
  end;
  HashedItems:=0;
  Item:=First;
  while assigned(Item) do begin
   inc(HashedItems);
   Item^.HashPrevious:=nil;
   Item^.HashNext:=nil;
   Item:=Item^.Next;
  end;
  HashBucketsUsed:=0;
  Item:=First;
  while assigned(Item) do begin
   Hash:=BESENHashKey(Item^.Key) and HashSizeMask;
   Item^.Hash:=Hash;
   if assigned(HashBuckets[Hash].HashLast) then begin
    HashBuckets[Hash].HashLast^.HashNext:=Item;
    Item^.HashPrevious:=HashBuckets[Hash].HashLast;
    HashBuckets[Hash].HashLast:=Item;
    Item^.HashNext:=nil;
   end else begin
    inc(HashBucketsUsed);
    HashBuckets[Hash].HashFirst:=Item;
    HashBuckets[Hash].HashLast:=Item;
    Item^.HashPrevious:=nil;
    Item^.HashNext:=nil;
   end;
   Item:=Item^.Next;
  end;
 end;
end;

function TBESENDeclarativeEnvironmentRecord.GetKey(const Key:TBESENString;Hash:TBESENHash=0):PBESENDeclarativeEnvironmentRecordHashItem;
begin
 if assigned(LastUsedItem) and (LastUsedItem^.Key=Key) then begin
  result:=LastUsedItem;
  Hash:=result^.Hash;
 end else begin
  if Hash=0 then begin
   Hash:=BESENHashKey(Key);
  end;
  Hash:=Hash and HashSizeMask;
  result:=HashBuckets[Hash].HashFirst;
  while assigned(result) and (result^.Key<>Key) do begin
   result:=result^.HashNext;
  end;
 end;
 if assigned(result) then begin
  LastUsedItem:=result;
  if HashBuckets[Hash].HashFirst<>result then begin
   if assigned(result.HashPrevious) then begin
    result.HashPrevious.HashNext:=result.HashNext;
   end;
   if assigned(result.HashNext) then begin
    result.HashNext.HashPrevious:=result.HashPrevious;
   end else if HashBuckets[Hash].HashLast=result then begin
    HashBuckets[Hash].HashLast:=result.HashPrevious;
   end;
   HashBuckets[Hash].HashFirst.HashPrevious:=result;
   result.HashNext:=HashBuckets[Hash].HashFirst;
   result.HashPrevious:=nil;
   HashBuckets[Hash].HashFirst:=result;
  end;
 end;
end;

function TBESENDeclarativeEnvironmentRecord.NewKey(const Key:TBESENString;Force:boolean=false;Hash:TBESENHash=0):PBESENDeclarativeEnvironmentRecordHashItem;
begin
 if Force then begin
  result:=nil;
  if Hash=0 then begin
   Hash:=BESENHashKey(Key);
  end;
  Hash:=Hash and HashSizeMask;
 end else if assigned(LastUsedItem) and (LastUsedItem^.Key=Key) then begin
  result:=LastUsedItem;
  Hash:=result^.Hash;
 end else begin
  if Hash=0 then begin
   Hash:=BESENHashKey(Key);
  end;
  Hash:=Hash and HashSizeMask;
  result:=HashBuckets[Hash].HashFirst;
  if not assigned(result) then begin
   inc(HashBucketsUsed);
  end;
  while assigned(result) and (result^.Key<>Key) do begin
   result:=result^.HashNext;
  end;
 end;
 if not assigned(result) then begin
  inc(HashedItems);
  New(result);
  fillchar(result^,sizeof(TBESENDeclarativeEnvironmentRecordHashItem),#0);
  result^.Hash:=Hash;
  result^.Key:=Key;
  result^.Index:=-1;
  result^.Value.ValueType:=bvtUNDEFINED;
  //result^.Value:=BESENUndefinedValue;
  result^.Mutable:=false;
  result^.Initialized:=false;
  result^.Deletion:=false;
  if assigned(HashBuckets[Hash].HashLast) then begin
   HashBuckets[Hash].HashLast^.HashNext:=result;
   result^.HashPrevious:=HashBuckets[Hash].HashLast;
   result^.HashNext:=nil;
   HashBuckets[Hash].HashLast:=result;
  end else begin
   HashBuckets[Hash].HashFirst:=result;
   HashBuckets[Hash].HashLast:=result;
   result^.HashPrevious:=nil;
   result^.HashNext:=nil;
  end;
  if assigned(Last) then begin
   Last^.Next:=result;
   result^.Previous:=Last;
   result^.Next:=nil;
   Last:=result;
  end else begin
   First:=result;
   Last:=result;
   result^.Previous:=nil;
   result^.Next:=nil;
  end;
  LastUsedItem:=result;
 end;
 GrowAndRehashIfNeeded;
 Touched:=true;
end;

function TBESENDeclarativeEnvironmentRecord.DeleteKey(const Item:PBESENDeclarativeEnvironmentRecordHashItem):TBESENBoolean;
begin
 result:=assigned(Item);
 if result then begin
  Touched:=true;
  if LastUsedItem=Item then begin
   if assigned(Item^.Next) then begin
    LastUsedItem:=Item^.Next;
   end else begin
    LastUsedItem:=Item^.Previous;
   end;
  end;
  if assigned(Item^.Previous) then begin
   Item^.Previous^.Next:=Item^.Next;
  end else if First=Item then begin
   First:=Item^.Next;
  end;
  if assigned(Item^.Next) then begin
   Item^.Next^.Previous:=Item^.Previous;
  end else if Last=Item then begin
   Last:=Item^.Previous;
  end;
  Item^.Next:=nil;
  Item^.Previous:=nil;
  if assigned(Item^.HashPrevious) then begin
   Item^.HashPrevious^.HashNext:=Item^.HashNext;
  end else if HashBuckets[Item^.Hash].HashFirst=Item then begin
   HashBuckets[Item^.Hash].HashFirst:=Item^.HashNext;
  end;
  if assigned(Item^.HashNext) then begin
   Item^.HashNext^.HashPrevious:=Item^.HashPrevious;
  end else if HashBuckets[Item^.Hash].HashLast=Item then begin
   HashBuckets[Item^.Hash].HashLast:=Item^.HashPrevious;
  end;
  Item^.HashNext:=nil;
  Item^.HashPrevious:=nil;
  Item^.Key:='';
  Item^.Value:=BESENUndefinedValue;
  if Item^.Index>=0 then begin
   HashIndexes[Item^.Index]:=nil;
   HashValues[Item^.Index]:=@BESENDummyValue;
  end;
  Dispose(Item);
 end;
end;

function TBESENDeclarativeEnvironmentRecord.HasBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 result:=assigned(GetKey(N,Hash));
end;

function TBESENDeclarativeEnvironmentRecord.CreateMutableBinding(const N:TBESENString;const D:TBESENBoolean=false;Hash:TBESENHash=0):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 if assigned(GetKey(N,Hash)) then begin
  BESENThrowTypeError('CreateMutableBinding for "'+N+'" failed');
 end;
 Item:=NewKey(N,false,Hash);
 Item^.Value.ValueType:=bvtUNDEFINED;
 Item^.Mutable:=true;
 Item^.Initialized:=false;
 Item^.Deletion:=D;
 result:=true;
end;

function TBESENDeclarativeEnvironmentRecord.SetMutableBindingEx(const N:TBESENString;const V:TBESENValue;const S:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('SetMutableBinding for "'+N+'" failed');
 end;
begin
 result:=false;
 Item:=GetKey(N,Hash);
 if not assigned(Item) then begin
  ThrowIt;
 end else if Item^.Mutable then begin
  BESENCopyValue(Item^.Value,V);
  Item^.Initialized:=true;
  result:=true;
 end else begin
  if S then begin // will added/fixed in ES5 errata too
   ThrowIt;
  end;
 end;
end;

procedure TBESENDeclarativeEnvironmentRecord.GetBindingValueEx(const N:TBESENString;const S:TBESENBoolean;var R:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0);
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
 procedure ThrowUninitialized;
 begin
  BESENThrowReferenceError('Uninitialized immutable binding "'+N+'"');
 end;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('GetBindingValue for "'+N+'" failed');
 end;
begin
 Item:=GetKey(N,Hash);
 if assigned(Item) then begin
  if Item^.Mutable or Item^.Initialized then begin
   BESENCopyValue(R,Item^.Value);
  end else begin
   if S then begin
    ThrowUninitialized;
   end else begin
    R.ValueType:=bvtUNDEFINED;
   end;
  end;
 end else begin
  ThrowIt;
 end;
end;

function TBESENDeclarativeEnvironmentRecord.DeleteBindingEx(const N:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 Item:=GetKey(N,Hash);
 if assigned(Item) then begin
  if Item^.Mutable and Item^.Deletion then begin
   if Item^.Index>=0 then begin
    HashIndexes[Item^.Index]:=nil;
    HashValues[Item^.Index]:=@BESENDummyValue;
   end;
   DeleteKey(Item);
   result:=true;
  end else begin
   result:=false;
  end;
 end else begin
  result:=true;
 end;
end;

procedure TBESENDeclarativeEnvironmentRecord.UpdateImplicitThisValue;
begin
 ImplicitThisValue.ValueType:=bvtUNDEFINED;
 ImplicitThisValue.Obj:=nil;
end;

function TBESENDeclarativeEnvironmentRecord.CreateImmutableBinding(const N:TBESENString;Hash:TBESENHash=0):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 if assigned(GetKey(N,Hash)) then begin
  BESENThrowTypeError('CreateImmutableBinding for "'+N+'" failed');
 end;
 Item:=NewKey(N,false,Hash);
 Item^.Value.ValueType:=bvtUNDEFINED;
 Item^.Mutable:=false;
 Item^.Initialized:=false;
 Item^.Deletion:=false;
 result:=true;
end;

function TBESENDeclarativeEnvironmentRecord.InitializeImmutableBinding(const N:TBESENString;const V:TBESENValue;Hash:TBESENHash=0):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 Item:=GetKey(N,Hash);
 if assigned(Item) and not (Item^.Mutable or Item^.Initialized) then begin
  BESENCopyValue(Item^.Value,v);
  Item^.Initialized:=true;
 end else begin
  BESENThrowTypeError('InitializeImmutableBinding for "'+N+'" failed');
 end;
 result:=true;
end;

function TBESENDeclarativeEnvironmentRecord.SetBindingValueIndex(const N:TBESENString;const I:integer;Hash:TBESENHash=0):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 Item:=GetKey(N,Hash);
 if assigned(Item) then begin
  Item^.Index:=I;
  HashIndexes.Insert(I,Item);
  HashIndexNames.Insert(I,N);
  HashIndexIDs.Insert(I,TBESEN(Instance).KeyIDManager.Get(N,Hash));
  if i>=length(HashValues) then begin
   SetLength(HashValues,i+256);
  end;
  HashValues[i]:=@Item^.Value;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TBESENDeclarativeEnvironmentRecord.SetIndexValue(const I,ID:integer;const V:TBESENValue;const S:TBESENBoolean):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('SetIndexValue for "'+inttostr(I)+'" failed');
 end;
begin
 if (I>=0) and (I<HashIndexes.Count) then begin
  Item:=HashIndexes[I];
 end else begin
  Item:=nil;
 end;
 if assigned(Item) and Item^.Mutable then begin
  BESENCopyValue(Item^.Value,v);
  Item^.Initialized:=true;
 end else begin
  ThrowIt;
 end;
 result:=true;
end;

procedure TBESENDeclarativeEnvironmentRecord.GetIndexValue(const I,ID:integer;const S:TBESENBoolean;var R:TBESENValue);
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
 procedure ThrowUninitialized;
 begin
  BESENThrowReferenceError('Uninitialized immutable binding "'+Item^.Key+'"');
 end;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('GetIndexValue for "'+inttostr(I)+'" failed');
 end;
begin
 if (I>=0) and (I<HashIndexes.Count) then begin
  Item:=HashIndexes[I];
 end else begin
  Item:=nil;
 end;
 if assigned(Item) then begin
  if Item^.Mutable or Item^.Initialized then begin
   BESENCopyValue(R,Item^.Value);
  end else begin
   if S then begin
    ThrowUninitialized;
   end else begin
    R.ValueType:=bvtUNDEFINED;
   end;
  end;
 end else begin
  ThrowIt;
 end;
end;

function TBESENDeclarativeEnvironmentRecord.DeleteIndex(const I,ID:integer):TBESENBoolean;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 if (I>=0) and (I<HashIndexes.Count) then begin
  Item:=HashIndexes[I];
 end else begin
  Item:=nil;
 end;
 if assigned(Item) then begin
  if Item^.Mutable and Item^.Deletion then begin
   HashIndexes[i]:=nil;
   HashValues[i]:=@BESENDummyValue;
   DeleteKey(Item);
   result:=true;
  end else begin
   result:=false;
  end;
 end else begin
  result:=true;
 end;
end;

procedure TBESENDeclarativeEnvironmentRecord.Finalize;
begin
 Clear;
 inherited Finalize;
end;

procedure TBESENDeclarativeEnvironmentRecord.Mark;
var Item:PBESENDeclarativeEnvironmentRecordHashItem;
begin
 Item:=First;
 while assigned(Item) do begin
  TBESEN(Instance).GarbageCollector.GrayValue(Item^.Value);
  Item:=Item^.Next;
 end;
 inherited Mark;
end;

end.
