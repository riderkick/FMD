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
unit BESENObject;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENValue,BESENBaseObject,
     BESENCollectorObject,BESENObjectPropertyDescriptor,
     BESENSelfBalancedTree,BESENHashMap,
     BESENGarbageCollector,
     BESENPointerSelfBalancedTree;

type TBESENNativeFunction=procedure(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue) of object;

     TBESENObjectPropertyContainer=class;

     TBESENObjectProperty=class(TBESENCollectorObject)
      public
       PropertyContainer:TBESENObjectPropertyContainer;
       HashPrevious,HashNext,Previous,Next:TBESENObjectProperty;
       Hash:TBESENHash;
       FullHash:TBESENHash;
       Index:TBESENINT32;
       ID:TBESENINT32;
       ArrayIndex:TBESENINT32;
       PropIndex:TBESENINT32;
       Key:TBESENString;
       Descriptor:TBESENObjectPropertyDescriptor;
       IsInSelfBalancedTree:TBESENBoolean;
       SelfBalancedTreeNode:PBESENSelfBalancedTreeNode;
       constructor Create(AInstance:TObject;const APropertyMap:TBESENObjectPropertyContainer;const AHash,AFullHash:TBESENHash;const AKey:TBESENString); overload;
       destructor Destroy; override;
     end;

     TBESENObjectProperties=array of TBESENObjectProperty;

     TBESENObjectPropertyContainerHashBucket=record
      HashFirst,HashLast:TBESENObjectProperty;
     end;

     TBESENObjectPropertyContainerHashBuckets=array of TBESENObjectPropertyContainerHashBucket;

     TBESENObject=class;

     TBESENObjectPropertyEnumerator=class;

     TBESENObjectPropertiesBuckets=array of TBESENObjectProperties;

     TBESENObjectPropertyContainer=class(TBESENCollectorObject)
      private
       procedure GrowAndRehashIfNeeded;
      protected
       SelfBalancedTree:TBESENSelfBalancedTree;
       HashBuckets:TBESENObjectPropertyContainerHashBuckets;
       HashSize:longword;
       HashSizeMask:longword;
       HashedItems:longword;
       HashBucketsUsed:longword;
       EnumeratorFirst,EnumeratorLast:TBESENObjectPropertyEnumerator;
       Sorted,IsArray:longbool;
       LastUsedItem:TBESENObjectProperty;
       Obj:TBESENObject;
      public
       First,Last:TBESENObjectProperty;
       Items:TBESENObjectProperties;
       ItemCount:longint;
       ArrayBuckets:TBESENObjectPropertiesBuckets;
       ArrayItemCount:longint;
       constructor Create(AInstance:TObject;ASorted:boolean;AObj:TBESENObject); overload;
       destructor Destroy; override;
       procedure Clear;
       procedure ArraySet(Index:longint;PropertyValue:TBESENObjectProperty);
       function ArrayGet(Index:longint):TBESENObjectProperty;
       procedure Sort(AIsArray:boolean);
       function Put(const Key:TBESENString;Hash:TBESENHash=0):TBESENObjectProperty;
       function Get(const Key:TBESENString;Hash:TBESENHash=0):TBESENObjectProperty;
       procedure Remove(const Key:TBESENString;Hash:TBESENHash=0);
       function GetFirst:TBESENObjectProperty;
       function GetLast:TBESENObjectProperty;
       function GetPrevious(const Item:TBESENObjectProperty):TBESENObjectProperty;
       function GetNext(const Item:TBESENObjectProperty):TBESENObjectProperty;
     end;

     TBESENObjectPropertyEnumerator=class(TBESENCollectorObject)
      public
       Obj:TBESENObject;
       Previous,Next,Prototype:TBESENObjectPropertyEnumerator;
       NextItem:TBESENObjectProperty;
       GetOnlyOwnProperties:TBESENBoolean;
       GetAllProperties:TBESENBoolean;
       DuplicatesHashMap:TBESENHashMap;
       constructor Create(AInstance:TObject;AObj:TBESENObject;AGetOnlyOwnProperties,AGetAllProperties:TBESENBoolean); overload; virtual;
       destructor Destroy; override;
       procedure Reset;
       function GetNext(var Key:TBESENString):boolean;
     end;

     PBESENObjectStructureProperty=^TBESENObjectStructureProperty;
     TBESENObjectStructureProperty=record
      Index:TBESENINT32;
      ID:TBESENINT32;
      Attributes:TBESENObjectPropertyDescriptorAttributes;
      Presents:TBESENObjectPropertyDescriptorPresents;
     end;

     TBESENObjectStructureProperties=array of TBESENObjectStructureProperty;

     TBESENObjectStructure=class(TBESENCollectorObject)
      public
       HashPrevious,HashNext,Previous,Next:TBESENObjectStructure;
       TransitionFrom:TBESENObjectStructure;
       ID:TBESENINT32;
       Hash:TBESENHash;
       PrototypeID:TBESENINT32;
       Properties:TBESENObjectStructureProperties;
       ReferenceCounter:longint;
       constructor Create(AInstance:TObject); override;
       destructor Destroy; override;
       procedure IncRef;
       procedure DecRef;
       function IsPrefixTo(APrototypeID:TBESENINT32;const AProperties:TBESENObjectStructureProperties;var FirstNewPropIndex:longint):TBESENBoolean;
       function IsEqualTo(APrototypeID:TBESENINT32;AHash:TBESENHash;const AProperties:TBESENObjectStructureProperties):TBESENBoolean;
       function ContainsStructureID(StructureID:TBESENINT32):TBESENBoolean;
       function GetStructureIDFromKeyID(KeyID:TBESENINT32):TBESENINT32;
     end;

     TBESENObjectStructures=array of TBESENObjectStructure;

     PBESENObjectStructureIDManagerHashBucket=^TBESENObjectStructureIDManagerHashBucket;
     TBESENObjectStructureIDManagerHashBucket=record
      HashFirst,HashLast:TBESENObjectStructure;
     end;

     TBESENObjectStructureIDManagerHashBuckets=array[0..BESENObjectStructureIDManagerHashSize-1] of TBESENObjectStructureIDManagerHashBucket;

     TBESENObjectStructureIDManager=class(TBESENBaseObject)
      private
       procedure Remove(Structure:TBESENObjectStructure);
      public
       First,Last:TBESENObjectStructure;
       IDCounter:TBESENINT32;
       HashBuckets:TBESENObjectStructureIDManagerHashBuckets;
       constructor Create(AInstance:TObject); override;
       destructor Destroy; override;
       procedure Reset;
       function ResetIfNeeded:longbool;
       function Get(PrototypeID:TBESENINT32;const Properties:TBESENObjectStructureProperties;OldStructure:TBESENObjectStructure):TBESENObjectStructure;
     end;

     TBESENObject=class(TBESENGarbageCollectorObject)
      private
       procedure SetPrototypeObject(NewPrototypeObject:TBESENObject);
       procedure RebuildOwnStructure;
      protected
       procedure InvalidateStructure;
       procedure PutNew(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;Hash:TBESENHash=0);
       procedure PutSetter(Base:TBESENObject;const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;const Descriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue);
       procedure GetGetter(Base:TBESENObject;var AResult:TBESENValue;const Descriptor:TBESENObjectPropertyDescriptor);
      public
       ObjectClassName:TBESENString;
       ObjectName:TBESENString;
       Properties:TBESENObjectPropertyContainer;
       Extensible:TBESENBoolean;
       PrototypeObject:TBESENObject;
       PrototypeChildren:TBESENPointerSelfBalancedTree;
       Structure:TBESENObjectStructure;
       StructureID:TBESENINT32;
       StructureHash:TBESENHash;
       LastProp:TBESENObjectProperty;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; virtual;
       destructor Destroy; override;
       function RebuildStructure:TBESENBoolean;
       procedure RegisterNativeFunction(const AName:TBESENSTRING;const ANative:TBESENNativeFunction;const Len:longint=0;const Attributes:TBESENObjectPropertyDescriptorAttributes=[];const AHasPrototypeProperty:longbool=false);
       function HasRecursivePrototypeChain:TBESENBoolean;
       procedure PutPrototype(const V:TBESENValue;Throw:TBESENBoolean);
       function OverwriteAccessor(const P:TBESENString;const Getter:TBESENObject=nil;const Setter:TBESENObject=nil;const Attributes:TBESENObjectPropertyDescriptorAttributes=[];Throw:TBESENBoolean=false):TBESENBoolean;
       function OverwriteData(const P:TBESENString;const Value:TBESENValue;const Attributes:TBESENObjectPropertyDescriptorAttributes=[];Throw:TBESENBoolean=false):TBESENBoolean;
       function GetOwnProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean; virtual;
       function GetProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean;
       function GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean; virtual;
       function GetFull(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
       function GetIndex(const Index,ID:longint;var AResult:TBESENValue;Base:TBESENObject=nil):boolean; virtual;
       function GetArrayIndex(const Index:TBESENUINT32;var AResult:TBESENValue;Base:TBESENObject=nil):boolean; virtual;
       function Get(const P:TBESENString;var AResult:TBESENValue;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
       function CanPut(const P:TBESENString;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
       procedure PutEx(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0); virtual;
       procedure PutFull(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var Temp:TBESENValue;Hash:TBESENHash=0);
       procedure PutIndex(const Index,ID:longint;const V:TBESENValue;Throw:TBESENBoolean); virtual;
       procedure PutArrayIndex(const Index:TBESENUINT32;const V:TBESENValue;Throw:TBESENBoolean); virtual;
       procedure Put(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;Hash:TBESENHash=0);
       function HasPropertyEx(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function HasProperty(const P:TBESENString;Hash:TBESENHash=0):TBESENBoolean;
       function DeleteEx(const P:TBESENString;Throw:TBESENBoolean;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function Delete(const P:TBESENString;Throw:TBESENBoolean;Hash:TBESENHash=0):TBESENBoolean;
       function DeleteIndex(const Index,ID:longint;Throw:TBESENBoolean):TBESENBoolean; virtual;
       function DeleteArrayIndex(const Index:TBESENUINT32;Throw:TBESENBoolean):TBESENBoolean; virtual;
       function DefineOwnPropertyEx(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;var Current:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; virtual;
       function DefineOwnProperty(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;Hash:TBESENHash=0):TBESENBoolean;
       procedure DefaultValue(const AHint:TBESENValue;var AResult:TBESENValue); virtual;
       function Enumerator(GetOnlyOwnProperties,GetAllProperties:TBESENBoolean):TBESENObjectPropertyEnumerator; virtual;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var AResult:TBESENValue); virtual;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var AResult:TBESENValue); virtual;
       function HasInstance(const AInstance:TBESENValue):TBESENBoolean; virtual;
       function GetSecurityDomain:pointer; virtual;
       function HasCall:TBESENBoolean; virtual;
       function HasConstruct:TBESENBoolean; virtual;
       function HasHasInstance:TBESENBoolean; virtual;
       function HasEnumerator:TBESENBoolean; virtual;
       function HasGetSecurityDomain:TBESENBoolean; virtual;
       procedure Finalize; override;
       procedure Mark; override;
       property Prototype:TBESENObject read PrototypeObject write SetPrototypeObject;
     end;

procedure BESENCheckObjectCoercible(const v:TBESENValue); {$ifdef caninline}inline;{$endif}

function BESENIsCallable(const v:TBESENValue):TBESENBoolean; {$ifdef caninline}inline;{$endif}

implementation

uses BESEN,BESENUtils,BESENArrayUtils,BESENHashUtils,BESENStringUtils,BESENCode,BESENErrors,
     BESENObjectNativeFunction;

function SortedKeyNormal(AProp:TBESENString):TBESENString;
begin
 result:=AProp;
end;

function SortedKeyNumberic(AProp:TBESENString):TBESENString;
var i:int64;
begin
 if BESENArrayToIndex(AProp,i) then begin
  result:=AProp;
  while length(result)<16 do begin
   result:='0'+result;
  end;
  result:=widechar(word($feff))+result;
 end else begin
  result:=widechar(word($fffe))+AProp;
 end;
end;

type TSortedKeyFunc=function(AProp:TBESENString):TBESENString;

const SortedKeyFunc:array[boolean] of TSortedKeyFunc=(SortedKeyNormal,SortedKeyNumberic);

constructor TBESENObjectProperty.Create(AInstance:TObject;const APropertyMap:TBESENObjectPropertyContainer;const AHash,AFullHash:TBESENHash;const AKey:TBESENString);
var InsertAtItem:TBESENObjectProperty;
    SelfBalancedTreeValue:TBESENSelfBalancedTreeValue;
    InsertAtNode:PBESENSelfBalancedTreeNode;
    v:int64;
    i,j:longint;
    OK:boolean;
begin
 inherited Create(AInstance);
 Index:=-1;
 v:=0;
 if TBESEN(Instance).InlineCacheEnabled then begin
  if BESENArrayToIndex(AKey,v) then begin
   ID:=TBESEN(Instance).KeyIDManager.Find(AKey,AFullHash);
  end else begin
   v:=-1;
   ID:=TBESEN(Instance).KeyIDManager.Get(AKey,AFullHash);
  end;
 end else begin
  if not BESENArrayToIndex(AKey,v) then begin
   v:=-1;
  end;
  ID:=-1;
 end;
 PropIndex:=-1;
 ArrayIndex:=-1;
 PropertyContainer:=APropertyMap;
 Hash:=AHash;
 FullHash:=AFullHash;
 Key:=AKey;
 Descriptor.Value.ValueType:=bvtUNDEFINED;
 Descriptor.Getter:=nil;
 Descriptor.Setter:=nil;
 Descriptor.Attributes:=[];
 Descriptor.Presents:=[];
 IsInSelfBalancedTree:=false;
 SelfBalancedTreeNode:=nil;
 if ID>=0 then begin
  Index:=PropertyContainer.ItemCount;
  inc(PropertyContainer.ItemCount);
  if Index>=length(PropertyContainer.Items) then begin
   SetLength(PropertyContainer.Items,Index+256);
  end;
  PropertyContainer.Items[Index]:=self;
 end;
 if PropertyContainer.IsArray then begin
  if (v>=0) and (v<=$7fffffff) then begin
   ArrayIndex:=v;
   PropertyContainer.ArraySet(ArrayIndex,self);
  end;
 end;
 if assigned(PropertyContainer.HashBuckets[Hash].HashFirst) then begin
  PropertyContainer.HashBuckets[Hash].HashFirst.HashPrevious:=self;
  HashNext:=PropertyContainer.HashBuckets[Hash].HashFirst;
  HashPrevious:=nil;
  PropertyContainer.HashBuckets[Hash].HashFirst:=self;
 end else begin
  PropertyContainer.HashBuckets[Hash].HashFirst:=self;
  PropertyContainer.HashBuckets[Hash].HashLast:=self;
  HashPrevious:=nil;
  HashNext:=nil;
 end;
{if assigned(PropertyContainer.HashBuckets[Hash].HashLast) then begin
  PropertyContainer.HashBuckets[Hash].HashLast.HashNext:=self;
  HashPrevious:=PropertyContainer.HashBuckets[Hash].HashLast;
  HashNext:=nil;
  PropertyContainer.HashBuckets[Hash].HashLast:=self;
 end else begin
  PropertyContainer.HashBuckets[Hash].HashFirst:=self;
  PropertyContainer.HashBuckets[Hash].HashLast:=self;
  HashPrevious:=nil;
  HashNext:=nil;
 end;}
 if PropertyContainer.Sorted then begin
  if assigned(PropertyContainer.First) then begin
   if SortedKeyFunc[boolean(PropertyContainer.IsArray)](PropertyContainer.First.Key)>=SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key) then begin
    PropertyContainer.First.Previous:=self;
    Previous:=nil;
    Next:=PropertyContainer.First;
    PropertyContainer.First:=self;
   end else if SortedKeyFunc[boolean(PropertyContainer.IsArray)](PropertyContainer.Last.Key)<=SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key) then begin
    PropertyContainer.Last.Next:=self;
    Previous:=PropertyContainer.Last;
    Next:=nil;
    PropertyContainer.Last:=self;
   end else begin
    InsertAtNode:=PropertyContainer.SelfBalancedTree.FindNearest(SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key));
    if assigned(InsertAtNode) then begin
     InsertAtItem:=InsertAtNode^.Value.p;
     if SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key)<SortedKeyFunc[boolean(PropertyContainer.IsArray)](InsertAtItem.Key) then begin
      Previous:=InsertAtItem.Previous;
      Next:=InsertAtItem;
      InsertAtItem.Previous:=self;
      if assigned(Previous) then begin
       Previous.Next:=self;
      end else begin
       PropertyContainer.First:=self;
      end;
     end else begin
      Previous:=InsertAtItem;
      Next:=InsertAtItem.Next;
      InsertAtItem.Next:=self;
      if assigned(Next) then begin
       Next.Previous:=self;
      end else begin
       PropertyContainer.Last:=self;
      end;
     end;
    end else begin
     InsertAtItem:=PropertyContainer.First;
     while assigned(InsertAtItem) and (SortedKeyFunc[boolean(PropertyContainer.IsArray)](InsertAtItem.Key)<=SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key)) do begin
      InsertAtItem:=InsertAtItem.Next;
     end;
     if assigned(InsertAtItem) then begin
      Previous:=InsertAtItem.Previous;
      Next:=InsertAtItem;
      InsertAtItem.Previous:=self;
      if assigned(Previous) then begin
       Previous.Next:=self;
      end else begin
       PropertyContainer.First:=self;
      end;
     end else begin
      PropertyContainer.Last.Next:=self;
      Previous:=PropertyContainer.Last;
      Next:=nil;
      PropertyContainer.Last:=self;
     end;
    end;
   end;
  end else begin
   PropertyContainer.First:=self;
   PropertyContainer.Last:=self;
   Previous:=nil;
   Next:=nil;
  end;
  SelfBalancedTreeValue.p:=self;
  SelfBalancedTreeNode:=PropertyContainer.SelfBalancedTree.Insert(SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key),SelfBalancedTreeValue);
  if assigned(SelfBalancedTreeNode) then begin
   IsInSelfBalancedTree:=true;
  end;
 end else begin
  if assigned(PropertyContainer.Last) then begin
   PropertyContainer.Last.Next:=self;
   Previous:=PropertyContainer.Last;
   Next:=nil;
   PropertyContainer.Last:=self;
  end else begin
   PropertyContainer.First:=self;
   PropertyContainer.Last:=self;
   Previous:=nil;
   Next:=nil;
  end;
 end;
 if ID>=0 then begin
  PropertyContainer.Obj.InvalidateStructure;
 end;
end;

destructor TBESENObjectProperty.Destroy;
begin
 if ID>=0 then begin
  PropertyContainer.Obj.InvalidateStructure;
 end;
 if (Index>=0) and (Index<length(PropertyContainer.Items)) then begin
  PropertyContainer.Items[Index]:=nil;
 end;
 Index:=-1;
 if ArrayIndex>=0 then begin
  PropertyContainer.ArraySet(ArrayIndex,nil);
 end;
 ArrayIndex:=-1;
 if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if PropertyContainer.First=self then begin
  PropertyContainer.First:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if PropertyContainer.Last=self then begin
  PropertyContainer.Last:=Previous;
 end;
 Next:=nil;
 Previous:=nil;
 if assigned(HashPrevious) then begin
  HashPrevious.HashNext:=HashNext;
 end else if PropertyContainer.HashBuckets[Hash].HashFirst=self then begin
  PropertyContainer.HashBuckets[Hash].HashFirst:=HashNext;
 end;
 if assigned(HashNext) then begin
  HashNext.HashPrevious:=HashPrevious;
 end else if PropertyContainer.HashBuckets[Hash].HashLast=self then begin
  PropertyContainer.HashBuckets[Hash].HashLast:=HashPrevious;
 end;
 HashNext:=nil;
 HashPrevious:=nil;
 if IsInSelfBalancedTree then begin
  if assigned(SelfBalancedTreeNode) then begin
   SelfBalancedTreeNode.Value.p:=nil;
  end;
  PropertyContainer.SelfBalancedTree.Remove(SortedKeyFunc[boolean(PropertyContainer.IsArray)](Key));
  IsInSelfBalancedTree:=false;
 end;
 Key:='';
 inherited Destroy;
end;

constructor TBESENObjectPropertyContainer.Create(AInstance:TObject;ASorted:boolean;AObj:TBESENObject);
var Hash:TBESENHash;
begin
 inherited Create(AInstance);
 SelfBalancedTree:=TBESENSelfBalancedTree.Create;
 FillChar(HashBuckets,sizeof(TBESENObjectPropertyContainerHashBuckets),#0);
 Sorted:=ASorted;
 Obj:=AObj;
 Items:=nil;
 ItemCount:=0;
 ArrayBuckets:=nil;
 ArrayItemCount:=0;
 First:=nil;
 Last:=nil;
 EnumeratorFirst:=nil;
 EnumeratorLast:=nil;
 HashSize:=256;
 HashSizeMask:=HashSize-1;
 HashedItems:=0;
 HashBucketsUsed:=0;
 SetLength(HashBuckets,HashSize);
 for Hash:=0 to HashSizeMask do begin
  HashBuckets[Hash].HashFirst:=nil;
  HashBuckets[Hash].HashLast:=nil;
 end;
 LastUsedItem:=nil;
end;

destructor TBESENObjectPropertyContainer.Destroy;
var Enumerator,NextEnumerator:TBESENObjectPropertyEnumerator;
begin
 Enumerator:=EnumeratorFirst;
 while assigned(Enumerator) do begin
  NextEnumerator:=Enumerator.Next;
  Enumerator.Previous:=nil;
  Enumerator.Next:=nil;
  BESENFreeAndNil(Enumerator);
  Enumerator:=NextEnumerator;
 end;
 Clear;
 SetLength(HashBuckets,0);
 SetLength(Items,0);
 ItemCount:=0;
 SetLength(ArrayBuckets,0);
 ArrayItemCount:=0;
 SelfBalancedTree.Free;
 inherited Destroy;
end;

procedure TBESENObjectPropertyContainer.Clear;
var Hash:TBESENHash;
    Item,NextItem:TBESENObjectProperty;
    i:longint;
begin
 Item:=First;
 while assigned(Item) do begin
  NextItem:=Item.Next;
  Item.Previous:=nil;
  Item.Next:=nil;
  BESENFreeAndNil(Item);
  Item:=NextItem;
 end;
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
 for i:=0 to length(Items)-1 do begin
  Items[i]:=nil;
 end;
 ItemCount:=0;
 for i:=0 to length(ArrayBuckets)-1 do begin
  SetLength(ArrayBuckets[i],0);
 end;
 SetLength(ArrayBuckets,0);
 ArrayItemCount:=0;
 LastUsedItem:=nil;
end;

procedure TBESENObjectPropertyContainer.ArraySet(Index:longint;PropertyValue:TBESENObjectProperty);
var BucketIndex,InBucketIndex,OldLength,Counter,NewLength:longint;
begin
 if Index>=0 then begin
  BucketIndex:=Index shr 16;
  InBucketIndex:=Index and $ffff;
  if assigned(PropertyValue) then begin
   OldLength:=length(ArrayBuckets);
   if OldLength<=BucketIndex then begin
    SetLength(ArrayBuckets,BESENRoundUpToPowerOfTwo(BucketIndex+1));
    for Counter:=OldLength to length(ArrayBuckets)-1 do begin
     ArrayBuckets[Counter]:=nil;
    end;
   end;
   OldLength:=length(ArrayBuckets[BucketIndex]);
   if OldLength<=InBucketIndex then begin
    SetLength(ArrayBuckets[BucketIndex],BESENRoundUpToPowerOfTwo(InBucketIndex+1));
    for Counter:=OldLength to length(ArrayBuckets[BucketIndex])-1 do begin
     ArrayBuckets[BucketIndex,Counter]:=nil;
    end;
   end;
   ArrayBuckets[BucketIndex,InBucketIndex]:=PropertyValue;
  end else begin
   if BucketIndex<length(ArrayBuckets) then begin
    OldLength:=length(ArrayBuckets[BucketIndex]);
    if InBucketIndex<OldLength then begin
     ArrayBuckets[BucketIndex,InBucketIndex]:=nil;
     NewLength:=0;
     for Counter:=OldLength-1 downto 0 do begin
      if assigned(ArrayBuckets[BucketIndex,Counter]) then begin
       NewLength:=Counter+1;
       break;
      end;
     end;
     if NewLength=0 then begin
      SetLength(ArrayBuckets[BucketIndex],0);
      ArrayBuckets[BucketIndex]:=nil;
     end else if NewLength<>OldLength then begin
      NewLength:=BESENRoundUpToPowerOfTwo(NewLength+1);
      if NewLength<>OldLength then begin
       SetLength(ArrayBuckets[BucketIndex],NewLength);
      end;
     end;
    end;
    OldLength:=length(ArrayBuckets);
    NewLength:=0;
    for Counter:=OldLength-1 downto 0 do begin
     if assigned(ArrayBuckets[Counter]) then begin
      NewLength:=Counter+1;
      break;
     end;
    end;
    if NewLength=0 then begin
     SetLength(ArrayBuckets,0);
     ArrayBuckets:=nil;
    end else if NewLength<>OldLength then begin
     NewLength:=BESENRoundUpToPowerOfTwo(NewLength+1);
     if NewLength<>OldLength then begin
      SetLength(ArrayBuckets,NewLength);
     end;
    end;
   end;
  end;
  BucketIndex:=length(ArrayBuckets)-1;
  if BucketIndex>=0 then begin
   ArrayItemCount:=(BucketIndex shl 16)+length(ArrayBuckets[BucketIndex]);
  end else begin
   ArrayItemCount:=0;
  end;
 end;
end;

function TBESENObjectPropertyContainer.ArrayGet(Index:longint):TBESENObjectProperty;
var BucketIndex,InBucketIndex:longint;
begin
 result:=nil;
 if Index>=0 then begin
  BucketIndex:=Index shr 16;
  InBucketIndex:=Index and $ffff;
  if (BucketIndex<length(ArrayBuckets)) and (InBucketIndex<length(ArrayBuckets[BucketIndex])) then begin
   result:=ArrayBuckets[BucketIndex,InBucketIndex];
  end;
 end;
end;

procedure TBESENObjectPropertyContainer.Sort(AIsArray:boolean);
var PartA,PartB,Item:TBESENObjectProperty;
    InSize,PartASize,PartBSize,Merges:longint;
    SelfBalancedTreeValue:TBESENSelfBalancedTreeValue;
    s:TBESENString;
begin
 if not Sorted then begin
  IsArray:=AIsArray;
 end;
 if assigned(First) then begin
  InSize:=1;
  while true do begin
   PartA:=First;
   First:=nil;
   Last:=nil;
   Merges:=0;
   while assigned(PartA) do begin
    inc(Merges);
    PartB:=PartA;
    PartASize:=0;
    while PartASize<InSize do begin
     inc(PartASize);
     PartB:=PartB.Next;
     if not assigned(PartB) then begin
      break;
     end;
    end;
    PartBSize:=InSize;
    while (PartASize>0) or ((PartBSize>0) and assigned(PartB)) do begin
     if PartASize=0 then begin
      Item:=PartB;
      PartB:=PartB.Next;
      dec(PartBSize);
     end else if (PartBSize=0) or not assigned(PartB) then begin
      Item:=PartA;
      PartA:=PartA.Next;
      dec(PartASize);
     end else if SortedKeyFunc[boolean(IsArray)](PartA.Key)<=SortedKeyFunc[boolean(IsArray)](PartB.Key) then begin
      Item:=PartA;
      PartA:=PartA.Next;
      dec(PartASize);
     end else begin
      Item:=PartB;
      PartB:=PartB.Next;
      dec(PartBSize);
     end;
     if assigned(Last) then begin
      Last.Next:=Item;
     end else begin
      First:=Item;
     end;
     Item.Previous:=Last;
     Last:=Item;
    end;
    PartA:=PartB;
   end;
   Last.Next:=nil;
   if Merges<=1 then begin
    break;
   end;
   inc(InSize,InSize);
  end;
  if not Sorted then begin
   Item:=First;
   while assigned(Item) do begin
    SelfBalancedTreeValue.p:=Item;
    s:=SortedKeyFunc[boolean(IsArray)](Item.Key);
    Item.SelfBalancedTreeNode:=SelfBalancedTree.Insert(s,SelfBalancedTreeValue);
    s:='';
    Item.IsInSelfBalancedTree:=assigned(Item.SelfBalancedTreeNode);
    Item:=Item.Next;
   end;
  end;
 end;
 Sorted:=true;
end;

procedure TBESENObjectPropertyContainer.GrowAndRehashIfNeeded;
var Hash:TBESENHash;
    Item:TBESENObjectProperty;
begin
 if (HashSize<BESENHashMaxSize) and (HashedItems>=(HashBucketsUsed*BESENHashItemsPerBucketsThreshold)) then begin
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
   Item.HashPrevious:=nil;
   Item.HashNext:=nil;
   Item:=Item.Next;
  end;
  HashBucketsUsed:=0;
  Item:=First;
  while assigned(Item) do begin
   Hash:=Item.FullHash and HashSizeMask;
   Item.Hash:=Hash;
   if assigned(HashBuckets[Hash].HashLast) then begin
    HashBuckets[Hash].HashLast.HashNext:=Item;
    Item.HashPrevious:=HashBuckets[Hash].HashLast;
    HashBuckets[Hash].HashLast:=Item;
    Item.HashNext:=nil;
   end else begin
    inc(HashBucketsUsed);
    HashBuckets[Hash].HashFirst:=Item;
    HashBuckets[Hash].HashLast:=Item;
    Item.HashPrevious:=nil;
    Item.HashNext:=nil;
   end;
   Item:=Item.Next;
  end;
 end;
end;

function TBESENObjectPropertyContainer.Put(const Key:TBESENString;Hash:TBESENHash=0):TBESENObjectProperty;
var Item:TBESENObjectProperty;
    Enumerator:TBESENObjectPropertyEnumerator;
    FullHash:TBESENHash;
begin
 result:=nil;
 if assigned(LastUsedItem) and (LastUsedItem.Key=Key) then begin
  Item:=LastUsedItem;
  Hash:=Item.Hash;
  FullHash:=Item.FullHash;
 end else begin
  IF Hash=0 then begin
   Hash:=BESENHashKey(Key);
  end;
  FullHash:=Hash;
  Hash:=Hash and HashSizeMask;
  Item:=HashBuckets[Hash].HashFirst;
  if not assigned(Item) then begin
   inc(HashBucketsUsed);
  end;
  while assigned(Item) and (Item.Key<>Key) do begin
   Item:=Item.Next;
  end;
 end;
 if assigned(Item) then begin
  LastUsedItem:=Item;
  result:=Item;
 end else begin
  inc(HashedItems);
  Item:=TBESENObjectProperty.Create(Instance,self,Hash,FullHash,Key);
  if assigned(Item) then begin
   result:=Item;
   LastUsedItem:=Item;
   Enumerator:=EnumeratorFirst;
   while assigned(Enumerator) do begin
    if not assigned(Enumerator.NextItem) then begin
     Enumerator.NextItem:=Item;
    end;
    Enumerator:=Enumerator.Next;
   end;
  end;
 end;
 GrowAndRehashIfNeeded;
end;

function TBESENObjectPropertyContainer.Get(const Key:TBESENString;Hash:TBESENHash=0):TBESENObjectProperty;
begin
 if assigned(LastUsedItem) and (LastUsedItem.Key=Key) then begin
  result:=LastUsedItem;
  Hash:=result.Hash;
 end else begin
  IF Hash=0 then begin
   Hash:=BESENHashKey(Key);
  end;
  Hash:=Hash and HashSizeMask;
  result:=HashBuckets[Hash].HashFirst;
  while assigned(result) and (result.Key<>Key) do begin
   result:=result.HashNext;
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

procedure TBESENObjectPropertyContainer.Remove(const Key:TBESENString;Hash:TBESENHash=0);
var Item:TBESENObjectProperty;
    Enumerator:TBESENObjectPropertyEnumerator;
begin
 if assigned(LastUsedItem) and (LastUsedItem.Key=Key) then begin
  Item:=LastUsedItem;
 end else begin
  IF Hash=0 then begin
   Hash:=BESENHashKey(Key);
  end;
  Hash:=Hash and HashSizeMask;
  Item:=HashBuckets[Hash].HashFirst;
  while assigned(Item) and (Item.Key<>Key) do begin
   Item:=Item.HashNext;
  end;
 end;
 if assigned(Item) then begin
  if LastUsedItem=Item then begin
   if assigned(Item.Next) then begin
    LastUsedItem:=Item.Next;
   end else begin
    LastUsedItem:=Item.Previous;
   end;
  end;
  Enumerator:=EnumeratorFirst;
  while assigned(Enumerator) do begin
   if Enumerator.NextItem=Item then begin
    Enumerator.NextItem:=Item.Next;
   end;
   Enumerator:=Enumerator.Next;
  end;
  Item.Free;
  if HashedItems>0 then begin
   dec(HashedItems);
  end;
 end;
end;

function TBESENObjectPropertyContainer.GetFirst:TBESENObjectProperty;
begin
 result:=First;
end;

function TBESENObjectPropertyContainer.GetLast:TBESENObjectProperty;
begin
 result:=Last;
end;

function TBESENObjectPropertyContainer.GetPrevious(const Item:TBESENObjectProperty):TBESENObjectProperty;
begin
 if assigned(Item) then begin
  result:=Item.Previous;
 end else begin
  result:=nil;
 end;
end;

function TBESENObjectPropertyContainer.GetNext(const Item:TBESENObjectProperty):TBESENObjectProperty;
begin
 if assigned(Item) then begin
  result:=Item.Next;
 end else begin
  result:=nil;
 end;
end;

constructor TBESENObjectPropertyEnumerator.Create(AInstance:TObject;AObj:TBESENObject;AGetOnlyOwnProperties,AGetAllProperties:TBESENBoolean);
begin
 inherited Create(AInstance);
 Obj:=AObj;
 GetOnlyOwnProperties:=AGetOnlyOwnProperties;
 GetAllProperties:=AGetAllProperties;
 DuplicatesHashMap:=TBESENHashMap.Create;
 if assigned(Obj.Properties.EnumeratorLast) then begin
  Obj.Properties.EnumeratorLast.Next:=self;
  Previous:=Obj.Properties.EnumeratorLast;
  Next:=nil;
  Obj.Properties.EnumeratorLast:=self;
 end else begin
  Obj.Properties.EnumeratorFirst:=self;
  Obj.Properties.EnumeratorLast:=self;
  Previous:=nil;
  Next:=nil;
 end;
 Prototype:=nil;
 NextItem:=nil;
end;

destructor TBESENObjectPropertyEnumerator.Destroy;
begin
 BESENFreeAndNil(Prototype);
 BESENFreeAndNil(DuplicatesHashMap);
 if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if Obj.Properties.EnumeratorFirst=self then begin
  Obj.Properties.EnumeratorFirst:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if Obj.Properties.EnumeratorLast=self then begin
  Obj.Properties.EnumeratorLast:=Previous;
 end;
 Previous:=nil;
 Next:=nil;
 inherited Destroy;
end;

procedure TBESENObjectPropertyEnumerator.Reset;
begin
 if assigned(Prototype) then begin
  BESENFreeAndNil(Prototype);
 end;
 if GetOnlyOwnProperties then begin
  Prototype:=nil;
 end else begin
  if assigned(Obj.Prototype) then begin
   Prototype:=Obj.Prototype.Enumerator(GetOnlyOwnProperties,GetAllProperties);
  end else begin
   Prototype:=nil;
  end;
 end;
 NextItem:=TBESENObjectProperty(Obj.Properties.GetFirst);
end;

function TBESENObjectPropertyEnumerator.GetNext(var Key:TBESENString):boolean;
var Prop:TBESENObjectProperty;
    Hash:TBESENHash;
begin
 while assigned(Prototype) do begin
  if Prototype.GetNext(Key) then begin
   Hash:=BESENHashKey(Key);
   Prop:=Obj.Properties.Get(Key,Hash);
   if assigned(Prop) then begin
    if not assigned(DuplicatesHashMap.GetKey(Key,Hash)) then begin
     DuplicatesHashMap.NewKey(Key,true,Hash);
     if GetAllProperties or ((boppENUMERABLE in Prop.Descriptor.Presents) and (bopaENUMERABLE in Prop.Descriptor.Attributes)) then begin
      result:=true;
      exit;
     end;
    end;
   end else begin
    result:=true;
    exit;
   end;
  end else begin
   BESENFreeAndNil(Prototype);
   Prototype:=nil;
   break;
  end;
 end;
 while assigned(NextItem) do begin
  if not assigned(DuplicatesHashMap.GetKey(NextItem.Key,NextItem.FullHash)) then begin
   if GetAllProperties or ((boppENUMERABLE in NextItem.Descriptor.Presents) and (bopaENUMERABLE in NextItem.Descriptor.Attributes)) then begin
    Key:=NextItem.Key;
    NextItem:=NextItem.Next;
    result:=true;
    exit;
   end;
  end;
  NextItem:=NextItem.Next;
 end;
 Key:='';
 result:=false;
end;

constructor TBESENObjectStructure.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 HashPrevious:=nil;
 HashNext:=nil;
 Previous:=nil;
 Next:=nil;
 TransitionFrom:=nil;
 ID:=-1;
 Hash:=0;
 PrototypeID:=-1;
 Properties:=nil;
 ReferenceCounter:=0;
end;

destructor TBESENObjectStructure.Destroy;
begin
 if assigned(TransitionFrom) then begin
  TransitionFrom.DecRef;
  TransitionFrom:=nil;
 end;
 SetLength(Properties,0);
 TBESEN(Instance).ObjectStructureIDManager.Remove(self);
 inherited Destroy;
end;

procedure TBESENObjectStructure.IncRef;
begin
 inc(ReferenceCounter);
end;

procedure TBESENObjectStructure.DecRef;
begin
 dec(ReferenceCounter);
 if ReferenceCounter<=0 then begin
  Destroy;
 end;
end;

function TBESENObjectStructure.IsPrefixTo(APrototypeID:TBESENINT32;const AProperties:TBESENObjectStructureProperties;var FirstNewPropIndex:longint):TBESENBoolean;
var Transition:TBESENObjectStructure;
    Transitions:TBESENObjectStructures;
    TransitionCount,TransitionIndex,APropIndex,TransitionPropIndex:longint;
    AProp,TransitionProp:PBESENObjectStructureProperty;
begin
 result:=false;

 if PrototypeID<>APrototypeID then begin
  exit;
 end;

 TransitionCount:=0;
 Transition:=self;
 while assigned(Transition) and (Transition.PrototypeID=PrototypeID) do begin
  inc(TransitionCount);
  Transition:=Transition.TransitionFrom;
 end;
 if assigned(Transition) and (Transition.PrototypeID<>PrototypeID) then begin
  exit;
 end;

 Transitions:=nil;
 try
  SetLength(Transitions,TransitionCount);

  TransitionIndex:=TransitionCount;

  Transition:=self;
  while assigned(Transition) do begin
   dec(TransitionIndex);
   Transitions[TransitionIndex]:=Transition;
   Transition:=Transition.TransitionFrom;
  end;

  if TransitionIndex>0 then begin
   SetLength(Transitions,0);
   exit;
  end;

  APropIndex:=0;
  TransitionIndex:=0;
  while (APropIndex<length(AProperties)) and (TransitionIndex<length(Transitions)) do begin
   Transition:=Transitions[TransitionIndex];
   TransitionPropIndex:=0;
   while (APropIndex<length(AProperties)) and (TransitionPropIndex<length(Transition.Properties)) do begin
    AProp:=@AProperties[APropIndex];
    TransitionProp:=@Transition.Properties[TransitionPropIndex];
    if (AProp^.Index<>TransitionProp^.Index) or (AProp^.ID<>TransitionProp^.ID) or (AProp^.Attributes<>TransitionProp^.Attributes) or (AProp^.Presents<>TransitionProp^.Presents) then begin
     SetLength(Transitions,0);
     exit;
    end;
    inc(APropIndex);
    inc(TransitionPropIndex);
   end;
   if TransitionPropIndex<length(Transition.Properties) then begin
    SetLength(Transitions,0);
    exit;
   end;
   inc(TransitionIndex);
  end;

  if TransitionIndex<length(Transitions) then begin
   SetLength(Transitions,0);
   exit;
  end;

  FirstNewPropIndex:=APropIndex;

  result:=true;
 finally
  SetLength(Transitions,0);
 end;
end;

function TBESENObjectStructure.IsEqualTo(APrototypeID:TBESENINT32;AHash:TBESENHash;const AProperties:TBESENObjectStructureProperties):TBESENBoolean;
var Transition:TBESENObjectStructure;
    APropIndex,TransitionPropIndex:longint;
    AProp,Prop:PBESENObjectStructureProperty;
begin
 if (PrototypeID<>APrototypeID) or (Hash<>AHash) then begin
  result:=false;
  exit;
 end;
 if assigned(TransitionFrom) then begin
  APropIndex:=length(AProperties)-1;
  Transition:=self;
  result:=true;
  while assigned(Transition) and (Transition.PrototypeID=PrototypeID) and (APropIndex>=0) do begin
   TransitionPropIndex:=length(Transition.Properties)-1;
   while (APropIndex>=0) and (TransitionPropIndex>=0) do begin
    AProp:=@AProperties[APropIndex];
    Prop:=@Transition.Properties[TransitionPropIndex];
    if (AProp^.Index<>Prop^.Index) or (AProp^.ID<>Prop^.ID) or (AProp^.Attributes<>Prop^.Attributes) or (AProp^.Presents<>Prop^.Presents) then begin
     result:=false;
     exit;
    end;
    dec(TransitionPropIndex);
    dec(APropIndex);
   end;
   if TransitionPropIndex>=0 then begin
    result:=false;
    exit;
   end;
   Transition:=Transition.TransitionFrom;
  end;
  if APropIndex>=0 then begin
   result:=false;
  end;
 end else if length(Properties)=length(AProperties) then begin
  result:=true;
  for APropIndex:=0 to length(AProperties)-1 do begin
   AProp:=@AProperties[APropIndex];
   Prop:=@Properties[APropIndex];
   if (AProp^.Index<>Prop^.Index) or (AProp^.ID<>Prop^.ID) or (AProp^.Attributes<>Prop^.Attributes) or (AProp^.Presents<>Prop^.Presents) then begin
    result:=false;
    break;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

function TBESENObjectStructure.ContainsStructureID(StructureID:TBESENINT32):TBESENBoolean;
var Transition:TBESENObjectStructure;
begin
 result:=false;
 Transition:=self;
 while assigned(Transition) and (Transition.PrototypeID=PrototypeID) do begin
  if Transition.ID=StructureID then begin
   result:=true;
   exit;
  end;
  Transition:=Transition.TransitionFrom;
 end;
end;

function TBESENObjectStructure.GetStructureIDFromKeyID(KeyID:TBESENINT32):TBESENINT32;
var Transition:TBESENObjectStructure;
    PropIndex:longint;
begin
 result:=-1;
 Transition:=self;
 while assigned(Transition) and (Transition.PrototypeID=PrototypeID) do begin
  for PropIndex:=0 to length(Transition.Properties)-1 do begin
   if Transition.Properties[PropIndex].ID=KeyID then begin
    result:=Transition.ID;
    exit;
   end;
  end;
  Transition:=Transition.TransitionFrom;
 end;
end;

constructor TBESENObjectStructureIDManager.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 First:=nil;
 Last:=nil;
 IDCounter:=0;
 FillChar(HashBuckets,sizeof(TBESENObjectStructureIDManagerHashBuckets),#0);
end;

destructor TBESENObjectStructureIDManager.Destroy;
begin
 while assigned(First) do begin
  First.Free;
 end;
 inherited Destroy;
end;

procedure TBESENObjectStructureIDManager.Reset;
var Code:TBESENCode;
    GarbageCollectorObject:TBESENGarbageCollectorObject;
    Obj:TBESENObject;
begin
 try
  Code:=TBESEN(Instance).CodeFirst;
  while assigned(Code) do begin
   Code.ResetPolymorphicInlineCacheInstructions;
   Code:=Code.CodeNext;
  end;

  GarbageCollectorObject:=TBESEN(Instance).GarbageCollector.First;
  while assigned(GarbageCollectorObject) do begin
   if GarbageCollectorObject is TBESENObject then begin
    Obj:=TBESENObject(GarbageCollectorObject);
    Obj.Structure:=nil;
    Obj.StructureID:=0;
    Obj.StructureHash:=0;
   end;
   GarbageCollectorObject:=GarbageCollectorObject.GarbageCollectorNext;
  end;

  while assigned(First) do begin
   First.Free;
  end;
  IDCounter:=0;
 except
  TBESEN(Instance).InlineCacheEnabled:=false;
 end;
end;

function TBESENObjectStructureIDManager.ResetIfNeeded:longbool;
begin
 if IDCounter>=$40000000 then begin
  Reset;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TBESENObjectStructureIDManager.Get(PrototypeID:TBESENINT32;const Properties:TBESENObjectStructureProperties;OldStructure:TBESENObjectStructure):TBESENObjectStructure;
var Hash:TBESENHash;
    HashBucket:PBESENObjectStructureIDManagerHashBucket;
    PropIndex,FirstNewPropIndex:longint;
    Prop:PBESENObjectStructureProperty;
    Transitions:TBESENObjectStructures;
begin
 Transitions:=nil;
 try
  Hash:=(2166136261 xor TBESENUINT32(PrototypeID))*1664525;
  for PropIndex:=0 to length(Properties)-1 do begin
   Prop:=@Properties[PropIndex];
   Hash:=((Hash+TBESENUINT32(Prop^.Index)) xor TBESENUINT32(Prop^.ID))*16777619;
   Hash:=((Hash-TBESENUINT32(byte(Prop^.Attributes))) xor TBESENUINT32(byte(Prop^.Presents)))*1664525;
  end;
  HashBucket:=@HashBuckets[Hash and BESENObjectStructureIDManagerHashSizeMask];
  result:=HashBucket^.HashFirst;
  while assigned(result) do begin
   if result.IsEqualTo(PrototypeID,Hash,Properties) then begin
    exit;
   end;
   result:=result.HashNext;
  end;
  result:=TBESENObjectStructure.Create(Instance);
  result.PrototypeID:=PrototypeID;
  result.Hash:=Hash;
  inc(IDCounter);
  result.ID:=IDCounter;
  FirstNewPropIndex:=0;
  if assigned(OldStructure) and OldStructure.IsPrefixTo(PrototypeID,Properties,FirstNewPropIndex) then begin
   if FirstNewPropIndex<length(Properties) then begin
    result.TransitionFrom:=OldStructure;
    result.TransitionFrom.IncRef;
    result.Properties:=copy(Properties,FirstNewPropIndex,length(Properties)-FirstNewPropIndex);
   end else begin
    result.Properties:=copy(Properties,0,length(Properties));
   end;
  end else begin
   result.Properties:=copy(Properties,0,length(Properties));
  end;
  result.HashNext:=nil;
  if assigned(HashBucket^.HashLast) then begin
   HashBucket^.HashLast.HashNext:=result;
   result.HashPrevious:=HashBucket^.HashLast;
   HashBucket^.HashLast:=result;
  end else begin
   result.HashPrevious:=nil;
   HashBucket^.HashFirst:=result;
   HashBucket^.HashLast:=result;
  end;
  result.Next:=nil;
  if assigned(Last) then begin
   Last.Next:=result;
   result.Previous:=Last;
   Last:=result;
  end else begin
   result.Previous:=nil;
   First:=result;
   Last:=result;
  end;
 finally
  SetLength(Transitions,0);
 end;
end;

procedure TBESENObjectStructureIDManager.Remove(Structure:TBESENObjectStructure);
var HashBucket:PBESENObjectStructureIDManagerHashBucket;
begin
 if assigned(Structure) then begin
  HashBucket:=@HashBuckets[Structure.Hash and BESENObjectStructureIDManagerHashSizeMask];
  if assigned(Structure.HashPrevious) then begin
   Structure.HashPrevious.HashNext:=Structure.HashNext;
  end else if HashBucket^.HashFirst=Structure then begin
   HashBucket^.HashFirst:=Structure.HashNext;
  end;
  if assigned(Structure.HashNext) then begin
   Structure.HashNext.HashPrevious:=Structure.HashPrevious;
  end else if HashBucket^.HashLast=Structure then begin
   HashBucket^.HashLast:=Structure.HashPrevious;
  end;
  Structure.HashNext:=nil;
  Structure.HashPrevious:=nil;
  if assigned(Structure.Previous) then begin
   Structure.Previous.Next:=Structure.Next;
  end else if First=Structure then begin
   First:=Structure.Next;
  end;
  if assigned(Structure.Next) then begin
   Structure.Next.Previous:=Structure.Previous;
  end else if Last=Structure then begin
   Last:=Structure.Previous;
  end;
  Structure.Next:=nil;
  Structure.Previous:=nil;
 end;
end;

constructor TBESENObject.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance);
 ObjectClassName:='Object';
 ObjectName:='object';

 PrototypeChildren:=TBESENPointerSelfBalancedTree.Create;

 Structure:=nil;
 StructureID:=0;
 StructureHash:=0;

 Properties:=TBESENObjectPropertyContainer.Create(AInstance,false,self);

 PrototypeObject:=nil;
 if assigned(APrototype) then begin
  Prototype:=APrototype;
 end;

 Extensible:=true;

 LastProp:=nil;

 if AHasPrototypeProperty and assigned(Prototype) then begin
  OverwriteData('prototype',BESENObjectValueEx(Prototype),[]);
 end;

 StructureID:=0;
end;

destructor TBESENObject.Destroy;
var Node:PBESENPointerSelfBalancedTreeNode;
    Obj:TBESENObject;
begin
 TBESEN(Instance).GarbageCollector.FinalizeObjectForSweeping(self);

 BESENFreeAndNil(Properties);

 Prototype:=nil;

 if assigned(PrototypeChildren) then begin
  Node:=PrototypeChildren.FirstKey;
  while assigned(Node) do begin
   Obj:=TBESENObject(Node^.Key);
   if assigned(Obj) and (Obj.PrototypeObject=self) then begin
    Obj.PrototypeObject:=nil;
    Obj.InvalidateStructure;
   end;
   Node:=Node^.NextKey;
  end;
 end;

 BESENFreeAndNil(PrototypeChildren);

 if assigned(Structure) then begin
  Structure.DecRef;
  Structure:=nil;
 end;

 ObjectName:='';
 ObjectClassName:='';
 inherited Destroy;
end;

procedure TBESENObject.SetPrototypeObject(NewPrototypeObject:TBESENObject);
var Obj:TBESENObject;
    Value:TBESENPointerSelfBalancedTreeValue;
begin
 if assigned(NewPrototypeObject) then begin
  Obj:=NewPrototypeObject;
  while assigned(Obj) do begin
   if Obj=self then begin
    BESENThrowRcursivePrototypeChain;
   end;
   Obj:=Obj.Prototype;
  end;
 end;
 if assigned(PrototypeObject) then begin
  PrototypeObject.PrototypeChildren.Remove(self);
 end;
 PrototypeObject:=NewPrototypeObject;
 if assigned(PrototypeObject) then begin
  Value.p:=self;
  PrototypeObject.PrototypeChildren.Insert(self,Value);
 end;
 InvalidateStructure;
end;

procedure TBESENObject.InvalidateStructure;
var Node:PBESENPointerSelfBalancedTreeNode;
    Obj:TBESENObject;
begin
 if StructureID<>0 then begin
  if assigned(PrototypeChildren) then begin
   Node:=PrototypeChildren.FirstKey;
   while assigned(Node) do begin
    Obj:=TBESENObject(Node^.Key);
    if assigned(Obj) and (Obj.PrototypeObject=self) then begin
     Obj.InvalidateStructure;
    end;
    Node:=Node^.NextKey;
   end;
  end;
  StructureID:=0;
 end;
end;

procedure TBESENObject.RebuildOwnStructure;
var PrototypeID:TBESENINT64;
    Count,Index:longint;
    Prop:TBESENObjectProperty;
    StructureProperties:TBESENObjectStructureProperties;
    OldStructure:TBESENObjectStructure;
begin
 StructureProperties:=nil;
 try
  OldStructure:=Structure;
  Structure:=nil;

  StructureID:=0;
  StructureHash:=0;

  if TBESEN(Instance).InlineCacheEnabled then begin

   if assigned(Prototype) then begin
    PrototypeID:=Prototype.StructureID;
   end else begin
    PrototypeID:=-1;
   end;

   Count:=0;
   Prop:=Properties.First;
   while assigned(Prop) do begin
    if Prop.ID>=0 then begin
     inc(Count);
    end;
    Prop:=Prop.Next;
   end;
   SetLength(StructureProperties,Count);
   Index:=0;
   Prop:=Properties.First;
   while assigned(Prop) do begin
    if Prop.ID>=0 then begin
     StructureProperties[Index].Index:=Prop.Index;
     StructureProperties[Index].ID:=Prop.ID;
     StructureProperties[Index].Attributes:=Prop.Descriptor.Attributes;
     StructureProperties[Index].Presents:=Prop.Descriptor.Presents;
     inc(Index);
    end;
    Prop:=Prop.Next;
   end;

   Structure:=TBESEN(Instance).ObjectStructureIDManager.Get(PrototypeID,StructureProperties,OldStructure);
   if assigned(Structure) then begin
    Structure.IncRef;
    StructureID:=Structure.ID;
    StructureHash:=Structure.Hash;
   end;
  end;

  if assigned(OldStructure) then begin
   OldStructure.DecRef;
   OldStructure:=nil;
  end;
 finally
  SetLength(StructureProperties,0);
 end;
end;

function TBESENObject.RebuildStructure:TBESENBoolean;
 procedure RebuildObjectStructure(Obj:TBESENObject);
 begin
  if Obj.StructureID=0 then begin
   if assigned(Obj.Prototype) then begin
    RebuildObjectStructure(Obj.Prototype);
   end;
   Obj.RebuildOwnStructure;
  end;
 end;
begin
 RebuildObjectStructure(self);
 result:=not TBESEN(Instance).ObjectStructureIDManager.ResetIfNeeded;
end;

procedure TBESENObject.RegisterNativeFunction(const AName:TBESENSTRING;const ANative:TBESENNativeFunction;const Len:longint=0;const Attributes:TBESENObjectPropertyDescriptorAttributes=[];const AHasPrototypeProperty:longbool=false);
var o:TBESENObjectNativeFunction;
begin
 o:=TBESENObjectNativeFunction.Create(Instance,TBESEN(Instance).ObjectFunctionPrototype,AHasPrototypeProperty);
 o.Native:=ANative;
 o.ObjectName:=AName;
 TBESEN(Instance).GarbageCollector.Add(o);
 o.GarbageCollectorLock;
 try
  OverwriteData(AName,BESENObjectValueEx(o),Attributes,true);
  if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
   o.OverwriteData('name',BESENStringValue(AName),[]);
  end;
  o.OverwriteData('length',BESENNumberValue(Len),[]);
 finally
  o.GarbageCollectorUnlock;
 end;
end;

function TBESENObject.HasRecursivePrototypeChain:TBESENBoolean;
var Obj:TBESENObject;
begin
 result:=false;
 Obj:=Prototype;
 while assigned(Obj) do begin
  if Obj=self then begin
   result:=true;
   break;
  end;
  Obj:=Obj.Prototype;
 end;
end;

procedure TBESENObject.PutPrototype(const V:TBESENValue;Throw:TBESENBoolean);
var Obj:TBESENObject;
begin
 case V.ValueType of
  bvtUNDEFINED,bvtNULL:begin
   Prototype:=nil;
  end;
  bvtOBJECT:begin
   Obj:=TBESENObject(V.Obj);
   while assigned(Obj) do begin
    if Obj=self then begin
     if Throw then begin
      BESENThrowPutRecursivePrototypeChain;
     end;
     exit;
    end;
    Obj:=Obj.Prototype;
   end;
   Prototype:=TBESENObject(V.Obj);
  end;
  else begin
   if Throw then begin
    BESENThrowPutInvalidPrototype;
   end;
  end;
 end;
end;

function TBESENObject.OverwriteAccessor(const P:TBESENString;const Getter:TBESENObject=nil;const Setter:TBESENObject=nil;const Attributes:TBESENObjectPropertyDescriptorAttributes=[];Throw:TBESENBoolean=false):TBESENBoolean;
var Prop:TBESENObjectProperty;
begin
 Prop:=Properties.Get(P);
 if assigned(Prop) then begin
  if Prop.ID>=0 then begin
   if (Prop.Descriptor.Attributes<>(Attributes*[bopaENUMERABLE,bopaCONFIGURABLE])) or (([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[]) then begin
    InvalidateStructure;
   end;
   if assigned(Prop.Descriptor.Getter) then begin
    if not (boppGETTER in Prop.Descriptor.Presents) then begin
     InvalidateStructure;
    end;
   end;
   if assigned(Prop.Descriptor.Setter) then begin
    if not (boppSETTER in Prop.Descriptor.Presents) then begin
     InvalidateStructure;
    end;
   end;
  end;
 end else begin
  Prop:=Properties.Put(P);
 end;
 Prop.Descriptor.Value.ValueType:=bvtUNDEFINED;
 Prop.Descriptor.Getter:=Getter;
 Prop.Descriptor.Setter:=Setter;
 Prop.Descriptor.Attributes:=Attributes*[bopaENUMERABLE,bopaCONFIGURABLE];
 Prop.Descriptor.Presents:=[boppENUMERABLE,boppCONFIGURABLE];
 if assigned(Prop.Descriptor.Getter) then begin
  Prop.Descriptor.Presents:=Prop.Descriptor.Presents+[boppGETTER];
 end;
 if assigned(Prop.Descriptor.Setter) then begin
  Prop.Descriptor.Presents:=Prop.Descriptor.Presents+[boppSETTER];
 end;
 result:=true;
end;

function TBESENObject.OverwriteData(const P:TBESENString;const Value:TBESENValue;const Attributes:TBESENObjectPropertyDescriptorAttributes=[];Throw:TBESENBoolean=false):TBESENBoolean;
var Prop:TBESENObjectProperty;
begin
 Prop:=Properties.Get(P);
 if assigned(Prop) then begin
  if Prop.ID>=0 then begin
   if (Prop.Descriptor.Attributes<>Attributes) or (Prop.Descriptor.Presents<>[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE]) then begin
    InvalidateStructure;
   end;
  end;
 end else begin
  Prop:=Properties.Put(P);
 end;
 BESENCopyValue(Prop.Descriptor.Value,Value);
 Prop.Descriptor.Getter:=nil;
 Prop.Descriptor.Setter:=nil;
 Prop.Descriptor.Attributes:=Attributes;
 Prop.Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
 result:=true;
end;

function TBESENObject.GetOwnProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean;
var Prop:TBESENObjectProperty;
begin
 Prop:=Properties.Get(P,Hash);
 LastProp:=Prop;
 if assigned(Prop) then begin
  if ([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[] then begin
   BESENCopyValue(Descriptor.Value,Prop.Descriptor.Value);
   Descriptor.Getter:=nil;
   Descriptor.Setter:=nil;
   Descriptor.Attributes:=Prop.Descriptor.Attributes;
   Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
   result:=true;
  end else if ([boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[] then begin
   Descriptor.Value.ValueType:=bvtUNDEFINED;
   Descriptor.Getter:=Prop.Descriptor.Getter;
   Descriptor.Setter:=Prop.Descriptor.Setter;
   Descriptor.Attributes:=Prop.Descriptor.Attributes*[bopaENUMERABLE,bopaCONFIGURABLE];
   Descriptor.Presents:=[boppGETTER,boppSETTER,boppENUMERABLE,boppCONFIGURABLE];
   result:=true;
  end else begin
   result:=false;
  end;
 end else if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (P='__proto__') then begin
  if assigned(Prototype) then begin
   Descriptor.Value.ValueType:=bvtOBJECT;
   Descriptor.Value.Obj:=Prototype;
  end else begin
   Descriptor.Value.ValueType:=bvtNULL;
  end;
  Descriptor.Getter:=nil;
  Descriptor.Setter:=nil;
  Descriptor.Attributes:=[bopaWRITABLE];
  Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE,boppPROTO];
  result:=true;
 end else begin
  Descriptor.Presents:=[];
  result:=false;
 end;
end;

function TBESENObject.GetProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean;
var Base:TBESENObject;
begin
 if Hash=0 then begin
  Hash:=BESENHashKey(P);
 end;
 Base:=self;
 while assigned(Base) do begin
  if Base.GetOwnProperty(P,Descriptor,Hash) then begin
   result:=true;
   exit;
  end;
  Base:=Base.Prototype;
 end;
 Descriptor.Presents:=[];
 result:=false;
end;

procedure TBESENObject.GetGetter(Base:TBESENObject;var AResult:TBESENValue;const Descriptor:TBESENObjectPropertyDescriptor);
begin
 if (boppGETTER in Descriptor.Presents) and assigned(Descriptor.Getter) then begin
  TBESEN(Instance).ObjectCall(TBESENObject(Descriptor.Getter),BESENObjectValue(Base),nil,0,AResult);
 end else begin
  AResult.ValueType:=bvtUNDEFINED;
 end;
end;

function TBESENObject.GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
var Prop:TBESENObjectProperty;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 Prop:=Properties.Get(P,Hash);
 if assigned(Prop) then begin
  if ([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[] then begin
   BESENCopyValue(AResult,Prop.Descriptor.Value);
  end else if ([boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[] then begin
   GetGetter(Base,AResult,Prop.Descriptor);
  end else begin
   AResult.ValueType:=bvtUNDEFINED;
  end;
  result:=true;
 end else if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (P='__proto__') then begin
  if assigned(Prototype) then begin
   AResult.ValueType:=bvtOBJECT;
   AResult.Obj:=Prototype;
  end else begin
   AResult.ValueType:=bvtNULL;
  end;
  result:=true;
 end else begin
  if assigned(Prototype) then begin
   result:=Prototype.GetEx(P,AResult,Descriptor,Base,Hash);
  end else begin
   AResult.ValueType:=bvtUNDEFINED;
   result:=false;
  end;
 end;
end;

function TBESENObject.GetFull(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
begin
 result:=false;
 AResult.ValueType:=bvtUNDEFINED;
 if GetProperty(P,Descriptor,Hash) then begin
  if ([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[] then begin
   if boppVALUE in Descriptor.Presents then begin
    BESENCopyValue(AResult,Descriptor.Value);
   end;
  end else if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
   if assigned(Base) then begin
    GetGetter(Base,AResult,Descriptor);
   end else begin
    GetGetter(self,AResult,Descriptor);
   end;
  end;
  result:=true;
 end;
end;

function TBESENObject.GetIndex(const Index,ID:longint;var AResult:TBESENValue;Base:TBESENObject=nil):boolean;
var Prop:TBESENObjectProperty;
begin
 result:=false;
 if not assigned(Base) then begin
  Base:=self;
 end;
 if (Index>=0) and (Index<Properties.ItemCount) then begin
  Prop:=Properties.Items[Index];
  if assigned(Prop) and (Prop.ID=ID) then begin
   if ([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[] then begin
    BESENCopyValue(AResult,Prop.Descriptor.Value);
   end else if ([boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[] then begin
    GetGetter(Base,AResult,Prop.Descriptor);
   end else begin
    AResult.ValueType:=bvtUNDEFINED;
   end;
   result:=true;
   exit;
  end;
 end;
 if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (TBESEN(Instance).KeyIDManager.ProtoID=ID) then begin
  if assigned(Prototype) then begin
   AResult.ValueType:=bvtOBJECT;
   AResult.Obj:=Prototype;
  end else begin
   AResult.ValueType:=bvtNULL;
  end;
  result:=true;
  exit;
 end else if assigned(Prototype) then begin
  result:=Prototype.GetIndex(Index,ID,AResult,Base);
 end else begin
  AResult.ValueType:=bvtUNDEFINED;
 end;
end;

function TBESENObject.GetArrayIndex(const Index:TBESENUINT32;var AResult:TBESENValue;Base:TBESENObject=nil):boolean;
begin
 result:=Get(BESENArrayIndexToStr(Index),AResult,Base);
end;

function TBESENObject.Get(const P:TBESENString;var AResult:TBESENValue;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 if Hash=0 then begin
  Hash:=BESENHashKey(P);
 end;
 result:=GetEx(P,AResult,Descriptor,Base,Hash);
end;

function TBESENObject.CanPut(const P:TBESENString;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 Descriptor.Presents:=[];
 if GetOwnProperty(P,OwnDescriptor) then begin
  if ([boppGETTER,boppSETTER]*OwnDescriptor.Presents)<>[] then begin
   result:=(boppSETTER in OwnDescriptor.Presents) and assigned(OwnDescriptor.Setter);
  end else begin
   result:=(boppWRITABLE in OwnDescriptor.Presents) and (bopaWRITABLE in OwnDescriptor.Attributes);
  end;
 end else begin
  if assigned(Prototype) then begin
   if Prototype.GetProperty(P,Descriptor) then begin
    if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
     result:=(boppSETTER in Descriptor.Presents) and assigned(Descriptor.Setter);
    end else begin
     result:=Extensible and ((boppWRITABLE in Descriptor.Presents) and (bopaWRITABLE in Descriptor.Attributes));
    end;
   end else begin
    result:=Extensible;
   end;
  end else begin
   result:=Extensible;
  end;
 end;
end;

procedure TBESENObject.PutNew(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;Hash:TBESENHash=0);
var Prop:TBESENObjectProperty;
begin
 Prop:=Properties.Put(P,Hash);
 BESENCopyValue(Prop.Descriptor.Value,V);
 Prop.Descriptor.Getter:=nil;
 Prop.Descriptor.Setter:=nil;
 Prop.Descriptor.Attributes:=[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE];
 Prop.Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
 if Prop.ID>=0 then begin
  InvalidateStructure;
 end;
end;

procedure TBESENObject.PutSetter(Base:TBESENObject;const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;const Descriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue);
var ValuePointers:array[0..0] of PBESENValue;
begin
 if (boppSETTER in Descriptor.Presents) and assigned(Descriptor.Setter) then begin
  ValuePointers[0]:=@v;
  TBESEN(Instance).ObjectCall(TBESENObject(Descriptor.Setter),BESENObjectValue(Base),@ValuePointers,1,TempValue);
 end else begin
  if Throw then begin
   BESENThrowNoSetter(P);
  end;
 end;
end;

procedure TBESENObject.PutEx(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0);
var Prop:TBESENObjectProperty;
begin
 // Optimized and combined GetOwnProperty + CanPut + DefineOwnProperty
 Prop:=Properties.Get(P,Hash);
 if assigned(Prop) then begin
  if ([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[] then begin
   if bopaWRITABLE in Prop.Descriptor.Attributes then begin
    BESENCopyValue(Prop.Descriptor.Value,V);
   end else begin
    if Throw then begin
     BESENThrowPut(P);
    end;
   end;
   exit;
  end else if ([boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[] then begin
   PutSetter(self,P,V,Throw,Prop.Descriptor,TempValue);
   exit;
  end;
 end else if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (P='__proto__') then begin
  PutPrototype(V,Throw);
  exit;
 end;

 if assigned(Prototype) then begin
  if Prototype.GetProperty(P,Descriptor) then begin
   if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
    PutSetter(self,P,V,Throw,Descriptor,TempValue);
    exit;
   end else begin
    if not (Extensible and ((boppWRITABLE in Descriptor.Presents) and (bopaWRITABLE in Descriptor.Attributes))) then begin
     if Throw then begin
      BESENThrowPut(P);
     end;
     exit;
    end;
   end;
  end;
 end;

 if Extensible then begin
  PutNew(P,V,Throw,Hash);
  exit;
 end;

 if Throw then begin
  BESENThrowPut(P);
 end;
end;

procedure TBESENObject.PutFull(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var Temp:TBESENValue;Hash:TBESENHash=0);
var ValuePointers:array[0..0] of PBESENValue;
begin
 if CanPut(P,Descriptor,OwnDescriptor,Hash) then begin
  if boppPROTO in OwnDescriptor.Presents then begin
   PutPrototype(V,Throw);
  end else if ([boppVALUE,boppWRITABLE]*OwnDescriptor.Presents)<>[] then begin
   BESENCopyValue(OwnDescriptor.Value,V);
   OwnDescriptor.Getter:=nil;
   OwnDescriptor.Setter:=nil;
   OwnDescriptor.Attributes:=[];
   OwnDescriptor.Presents:=[boppVALUE];
   DefineOwnPropertyEx(P,OwnDescriptor,Throw,Descriptor,Hash);
  end else begin
   if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
    if (boppSETTER in Descriptor.Presents) and assigned(Descriptor.Setter) then begin
     ValuePointers[0]:=@V;
     TBESEN(Instance).ObjectCall(TBESENObject(Descriptor.Setter),BESENObjectValue(self),@ValuePointers,1,Temp);
    end else begin
     if Throw then begin
      BESENThrowNoSetter(P);
     end;
    end;
   end else begin
    BESENCopyValue(OwnDescriptor.Value,v);
    OwnDescriptor.Getter:=nil;
    OwnDescriptor.Setter:=nil;
    OwnDescriptor.Attributes:=[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE];
    OwnDescriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
    DefineOwnPropertyEx(P,OwnDescriptor,Throw,Descriptor,Hash);
   end;
  end;
 end else begin
  if Throw then begin
   BESENThrowPut(P);
  end;
 end;
end;

procedure TBESENObject.PutIndex(const Index,ID:longint;const V:TBESENValue;Throw:TBESENBoolean);
 procedure ThrowIt;
 begin
  BESENThrowTypeError('Put failed');
 end;
 procedure PutSetter(Obj:TBESENObject;Prop:TBESENObjectProperty);
 var TempValue:TBESENValue;
     ValuePointers:array[0..0] of PBESENValue;
 begin
  if (boppSETTER in Prop.Descriptor.Presents) and assigned(Prop.Descriptor.Setter) then begin
   ValuePointers[0]:=@V;
   TBESEN(Instance).ObjectCall(TBESENObject(Prop.Descriptor.Setter),BESENObjectValue(Obj),@ValuePointers,1,TempValue);
   exit;
  end;
  if Throw then begin
   BESENThrowNoSetter(Prop.Key);
  end;
 end;
var Obj:TBESENObject;
    Prop:TBESENObjectProperty;
begin
 if (Index>=0) and (Index<Properties.ItemCount) then begin
  Prop:=Properties.Items[Index];
  if assigned(Prop) and (Prop.ID=ID) then begin
   if ([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[] then begin
    if bopaWRITABLE in Prop.Descriptor.Attributes then begin
     BESENCopyValue(Prop.Descriptor.Value,V);
    end else begin
     if Throw then begin
      BESENThrowPut(Prop.Key);
     end;
    end;
    exit;
   end else if ([boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[] then begin
    PutSetter(self,Prop);
    exit;
   end;
  end;
 end;
 if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (ID=TBESEN(Instance).KeyIDManager.ProtoID) then begin
  PutPrototype(V,Throw);
  exit;
 end;
 Prop:=nil;
 Obj:=Prototype;
 while assigned(Obj) do begin
  if (Index>=0) and (Index<Obj.Properties.ItemCount) then begin
   Prop:=Obj.Properties.Items[Index];
   if assigned(Prop) and (Prop.ID=ID) and (([boppVALUE,boppWRITABLE,boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[]) then begin
    break;
   end;
  end;
  Obj:=Obj.Prototype;
 end;
 if assigned(Obj) and assigned(Prop) then begin
  if ([boppGETTER,boppSETTER]*Prop.Descriptor.Presents)<>[] then begin
   PutSetter(self,Prop);
   exit;
  end else begin
   if not (Extensible and ((boppWRITABLE in Prop.Descriptor.Presents) and (bopaWRITABLE in Prop.Descriptor.Attributes))) then begin
    if Throw then begin
     BESENThrowPut(Prop.Key);
    end;
    exit;
   end;
  end;
 end else begin
  Obj:=self;
 end;
 if Extensible then begin
  PutNew(TBESEN(Instance).KeyIDManager.List[ID],V,Throw);
 end;
 if Throw then begin
  ThrowIt;
 end;
end;

procedure TBESENObject.PutArrayIndex(const Index:TBESENUINT32;const V:TBESENValue;Throw:TBESENBoolean);
begin
 Put(BESENArrayIndexToStr(Index),V,Throw);
end;

procedure TBESENObject.Put(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;Hash:TBESENHash=0);
var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;
    Temp:TBESENValue;
begin
 if Hash=0 then begin
  Hash:=BESENHashKey(P);
 end;
 PutEx(P,V,Throw,Descriptor,OwnDescriptor,Temp,Hash);
end;

function TBESENObject.HasPropertyEx(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
begin
 result:=GetProperty(P,Descriptor,Hash);
end;

function TBESENObject.HasProperty(const P:TBESENString;Hash:TBESENHash=0):TBESENBoolean;
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 if Hash=0 then begin
  Hash:=BESENHashKey(P);
 end;
 result:=HasPropertyEx(P,Descriptor,Hash);
end;

function TBESENObject.DeleteEx(const P:TBESENString;Throw:TBESENBoolean;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('Delete for "'+P+'" failed');
 end;
begin
 if GetOwnProperty(P,Descriptor,Hash) then begin
  if boppPROTO in Descriptor.Presents then begin
   Prototype:=nil;
   result:=true;
  end else if (boppCONFIGURABLE in Descriptor.Presents) and (bopaCONFIGURABLE in Descriptor.Attributes) then begin
   Properties.Remove(P,Hash);
   result:=true;
  end else begin
   if Throw then begin
    ThrowIt;
   end;
   result:=false;
  end;
 end else begin
  result:=true;
 end;
end;

function TBESENObject.Delete(const P:TBESENString;Throw:TBESENBoolean;Hash:TBESENHash=0):TBESENBoolean;
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 if Hash=0 then begin
  Hash:=BESENHashKey(P);
 end;
 result:=DeleteEx(P,Throw,Descriptor,Hash);
end;

function TBESENObject.DeleteIndex(const Index,ID:longint;Throw:TBESENBoolean):TBESENBoolean;
var Descriptor:TBESENObjectPropertyDescriptor;
    Prop:TBESENObjectProperty;
begin
 if (Index>=0) and (Index<Properties.ItemCount) then begin
  Prop:=Properties.Items[Index];
  if assigned(Prop) and (Prop.ID=ID) then begin
   result:=DeleteEx(Prop.Key,Throw,Descriptor);
   exit;
  end;
 end;
 result:=DeleteEx(TBESEN(Instance).KeyIDManager.List[ID],Throw,Descriptor);
end;

function TBESENObject.DeleteArrayIndex(const Index:TBESENUINT32;Throw:TBESENBoolean):TBESENBoolean;
begin
 result:=Delete(BESENArrayIndexToStr(Index),Throw);
end;

function TBESENObject.DefineOwnPropertyEx(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;var Current:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
var Prop:TBESENObjectProperty;
    CurrentResult:boolean;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('DefineOwnProperty for "'+P+'" failed');
 end;
begin
 result:=false;
 CurrentResult:=GetOwnProperty(P,Current,Hash);
 if not CurrentResult then begin
  if Extensible then begin
   if (([boppVALUE,boppWRITABLE,boppGETTER,boppSETTER]*Descriptor.Presents)=[]) or (([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[]) then begin
    Prop:=Properties.Get(P,Hash);
    if not assigned(Prop) then begin
     Prop:=Properties.Put(P,Hash);
    end;
    BESENCopyValue(Prop.Descriptor.Value,Descriptor.Value);
    Prop.Descriptor.Getter:=nil;
    Prop.Descriptor.Setter:=nil;
    Prop.Descriptor.Attributes:=Descriptor.Attributes;
    Prop.Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
    if Prop.ID>=0 then begin
     InvalidateStructure;
    end;
    result:=true;
    exit;
   end else if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
    Prop:=Properties.Get(P,Hash);
    if not assigned(Prop) then begin
     Prop:=Properties.Put(P,Hash);
    end;
    Prop.Descriptor.Value.ValueType:=bvtUNDEFINED;
    Prop.Descriptor.Getter:=Descriptor.Getter;
    Prop.Descriptor.Setter:=Descriptor.Setter;
    Prop.Descriptor.Attributes:=Descriptor.Attributes*[bopaENUMERABLE,bopaCONFIGURABLE];
    Prop.Descriptor.Presents:=[boppGETTER,boppSETTER,boppENUMERABLE,boppCONFIGURABLE];
    if Prop.ID>=0 then begin
     InvalidateStructure;
    end;
    result:=true;
    exit;
   end;
  end else begin
   if Throw then begin
    ThrowIt;
   end;
   exit;
  end;
 end;
 if Descriptor.Presents=[] then begin
  result:=true;
  exit;
 end;
 if boppPROTO in Current.Presents then begin
  if ((([boppGETTER,boppSETTER]*Descriptor.Presents)=[]) and (([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[])) and (boppVALUE in Descriptor.Presents) then begin
   PutPrototype(Descriptor.Value,Throw);
   result:=true;
  end else begin
   if Throw then begin
    ThrowIt;
   end;
  end;
  result:=true;
  exit;
 end;
 if ((Descriptor.Presents*Current.Presents)=Descriptor.Presents) and
    (((boppVALUE in Descriptor.Presents) and TBESEN(Instance).SameValue(Descriptor.Value,Current.Value)) or not (boppVALUE in Descriptor.Presents)) and
    (((boppGETTER in Descriptor.Presents) and TBESEN(Instance).SameObject(TBESENObject(Descriptor.Getter),TBESENObject(Current.Getter))) or not (boppGETTER in Descriptor.Presents)) and
    (((boppSETTER in Descriptor.Presents) and TBESEN(Instance).SameObject(TBESENObject(Descriptor.Setter),TBESENObject(Current.Setter))) or not (boppSETTER in Descriptor.Presents)) and
    (((boppWRITABLE in Descriptor.Presents) and (bopaWRITABLE in (Descriptor.Attributes*Current.Attributes))) or not (boppWRITABLE in Descriptor.Presents)) and
    (((boppENUMERABLE in Descriptor.Presents) and (bopaENUMERABLE in (Descriptor.Attributes*Current.Attributes))) or not (boppENUMERABLE in Descriptor.Presents)) and
    (((boppCONFIGURABLE in Descriptor.Presents) and (bopaCONFIGURABLE in (Descriptor.Attributes*Current.Attributes))) or not (boppCONFIGURABLE in Descriptor.Presents)) then begin
  result:=true;
  exit;
 end;
 if not ((boppCONFIGURABLE in Current.Presents) and (bopaCONFIGURABLE in Current.Attributes)) then begin
  if (((boppCONFIGURABLE in Descriptor.Presents) and (bopaCONFIGURABLE in Descriptor.Attributes))) or
     ((((boppENUMERABLE in Descriptor.Presents) and (bopaENUMERABLE in Descriptor.Attributes)))<>(((boppENUMERABLE in Current.Presents) and (bopaENUMERABLE in Current.Attributes)))) then begin
   if Throw then begin
    ThrowIt;
   end;
   exit;
  end;
 end;
 if ([boppVALUE,boppWRITABLE,boppGETTER,boppSETTER]*Descriptor.Presents)=[] then begin
 end else if (([boppVALUE,boppWRITABLE]*Current.Presents)<>[])<>(([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[]) then begin
  if not ((boppCONFIGURABLE in Current.Presents) and (bopaCONFIGURABLE in Current.Attributes)) then begin
   if Throw then begin
    ThrowIt;
   end;
   exit;
  end;
  if ([boppVALUE,boppWRITABLE]*Current.Presents)<>[] then begin
   Current.Value.ValueType:=bvtUNDEFINED;
   Current.Getter:=nil;
   Current.Setter:=nil;
   Current.Attributes:=Current.Attributes*[bopaCONFIGURABLE,bopaENUMERABLE];
   Current.Presents:=Current.Presents*[boppCONFIGURABLE,boppENUMERABLE];
  end else begin
   Current.Value.ValueType:=bvtUNDEFINED;
   Current.Getter:=nil;
   Current.Setter:=nil;
   Current.Attributes:=Current.Attributes*[bopaCONFIGURABLE,bopaENUMERABLE];
   Current.Presents:=(Current.Presents*[boppCONFIGURABLE,boppENUMERABLE])+[boppVALUE];
  end;
 end else if (([boppVALUE,boppWRITABLE]*Current.Presents)<>[]) and (([boppVALUE,boppWRITABLE]*Descriptor.Presents)<>[]) then begin
  if not ((boppCONFIGURABLE in Current.Presents) and (bopaCONFIGURABLE in Current.Attributes)) then begin
   if (((boppWRITABLE in Descriptor.Presents) and (bopaWRITABLE in Descriptor.Attributes)) or
       ((boppVALUE in Descriptor.Presents) and not (((boppVALUE in Current.Presents) and TBESEN(Instance).SameValue(Descriptor.Value,Current.Value)) or
                                                     (TBESEN(Instance).SameValue(Descriptor.Value,BESENUndefinedValue) and not (boppVALUE in Current.Presents))))) and not
      ((boppWRITABLE in Current.Presents) and (bopaWRITABLE in Current.Attributes)) then begin
    if Throw then begin
     ThrowIt;
    end;
    exit;
   end;
  end;
 end else if (([boppGETTER,boppSETTER]*Current.Presents)<>[]) and (([boppGETTER,boppSETTER]*Descriptor.Presents)<>[]) then begin
  if not ((boppCONFIGURABLE in Current.Presents) and (bopaCONFIGURABLE in Current.Attributes)) then begin
   if boppSETTER in Descriptor.Presents then begin
    if boppSETTER in Current.Presents then begin
     if not TBESEN(Instance).SameObject(TBESENObject(Descriptor.Setter),TBESENObject(Current.Setter)) then begin
      if Throw then begin
       ThrowIt;
      end;
      exit;
     end;
    end else begin
     if Throw then begin
      ThrowIt;
     end;
     exit;
    end;
   end;
   if boppGETTER in Descriptor.Presents then begin
    if boppGETTER in Current.Presents then begin
     if not TBESEN(Instance).SameObject(TBESENObject(Descriptor.Getter),TBESENObject(Current.Getter)) then begin
      if Throw then begin
       ThrowIt;
      end;
      exit;
     end;
    end else begin
     if Throw then begin
      ThrowIt;
     end;
     exit;
    end;
   end;
  end;
 end;
 if boppVALUE in Descriptor.Presents then begin
  Current.Presents:=Current.Presents+[boppVALUE];
  BESENCopyValue(Current.Value,Descriptor.Value);
 end;
 if boppGETTER in Descriptor.Presents then begin
  Current.Presents:=Current.Presents+[boppGETTER];
  Current.Getter:=Descriptor.Getter;
 end;
 if boppSETTER in Descriptor.Presents then begin
  Current.Presents:=Current.Presents+[boppSETTER];
  Current.Setter:=Descriptor.Setter;
 end;
 if boppWRITABLE in Descriptor.Presents then begin
  Current.Presents:=Current.Presents+[boppWRITABLE];
  Current.Attributes:=(Current.Attributes-[bopaWRITABLE])+(Descriptor.Attributes*[bopaWRITABLE]);
 end;
 if boppENUMERABLE in Descriptor.Presents then begin
  Current.Presents:=Current.Presents+[boppENUMERABLE];
  Current.Attributes:=(Current.Attributes-[bopaENUMERABLE])+(Descriptor.Attributes*[bopaENUMERABLE]);
 end;
 if boppCONFIGURABLE in Descriptor.Presents then begin
  Current.Presents:=Current.Presents+[boppCONFIGURABLE];
  Current.Attributes:=(Current.Attributes-[bopaCONFIGURABLE])+(Descriptor.Attributes*[bopaCONFIGURABLE]);
 end;
 Prop:=Properties.Get(P,Hash);
 if assigned(Prop) then begin
  if (Prop.ID>=0) and ((Prop.Descriptor.Attributes<>Current.Attributes) or (Prop.Descriptor.Presents<>Current.Presents)) then begin
   InvalidateStructure;
  end;
 end else begin
  Prop:=Properties.Put(P,Hash);
 end;
 BESENCopyValue(Prop.Descriptor.Value,Current.Value);
 Prop.Descriptor.Getter:=Current.Getter;
 Prop.Descriptor.Setter:=Current.Setter;
 Prop.Descriptor.Attributes:=Current.Attributes;
 Prop.Descriptor.Presents:=Current.Presents;
 result:=true;
end;

function TBESENObject.DefineOwnProperty(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;Hash:TBESENHash=0):TBESENBoolean;
var Current:TBESENObjectPropertyDescriptor;
begin
 result:=DefineOwnPropertyEx(P,Descriptor,Throw,Current,Hash);
end;

procedure TBESENObject.DefaultValue(const AHint:TBESENValue;var AResult:TBESENValue);
var EffectiveHint:TBESENObject;
    v:TBESENValue;
 procedure ThrowIt;
 begin
  raise EBESENTypeError.Create('Bad default value');
 end;
begin            
 case AHint.ValueType of
  bvtNUMBER:begin
   EffectiveHint:=TBESEN(Instance).ObjectNumberConstructor;
  end;
  bvtSTRING:begin
   EffectiveHint:=TBESEN(Instance).ObjectStringConstructor;
  end;
  bvtOBJECT:begin
   if (AHint.Obj=TBESEN(Instance).ObjectNumberConstructor) or (AHint.Obj=TBESEN(Instance).ObjectStringConstructor) then begin
    EffectiveHint:=TBESENObject(AHint.Obj);
   end else if AHint.Obj=TBESEN(Instance).ObjectDateConstructor then begin
    EffectiveHint:=TBESEN(Instance).ObjectStringConstructor;
   end else begin
    EffectiveHint:=TBESEN(Instance).ObjectNumberConstructor;
   end;
  end else begin
   EffectiveHint:=TBESEN(Instance).ObjectNumberConstructor;
  end;
 end;
 if EffectiveHint=TBESEN(Instance).ObjectStringConstructor then begin
  Get('toString',v);
  if (v.ValueType=bvtOBJECT) and assigned(v.Obj) and TBESENObject(v.Obj).HasCall then begin
   TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),BESENObjectValue(self),nil,0,AResult);
   if AResult.ValueType<>bvtOBJECT then begin
    exit;
   end;
  end;
  Get('valueOf',v);
  if (v.ValueType=bvtOBJECT) and assigned(v.Obj) and TBESENObject(v.Obj).HasCall then begin
   TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),BESENObjectValue(self),nil,0,AResult);
   if AResult.ValueType<>bvtOBJECT then begin
    exit;
   end;
  end;
  if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
   AResult:=BESENStringValue('[object '+inttostr(ptruint(self))+']');
  end else begin
   ThrowIt;
  end;
 end else if EffectiveHint=TBESEN(Instance).ObjectNumberConstructor then begin
  Get('valueOf',v);
  if (v.ValueType=bvtOBJECT) and assigned(v.Obj) and TBESENObject(v.Obj).HasCall then begin
   TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),BESENObjectValue(self),nil,0,AResult);
   if AResult.ValueType<>bvtOBJECT then begin
    exit;
   end;
  end;
  Get('toString',v);
  if (v.ValueType=bvtOBJECT) and assigned(v.Obj) and TBESENObject(v.Obj).HasCall then begin
   TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),BESENObjectValue(self),nil,0,AResult);
   if AResult.ValueType<>bvtOBJECT then begin
    exit;
   end;
  end;
  if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
   AResult:=BESENStringValue('[object '+inttostr(ptruint(self))+']');
  end else begin
   ThrowIt;
  end;
 end else begin
  ThrowIt;
 end;
end;

function TBESENObject.Enumerator(GetOnlyOwnProperties,GetAllProperties:TBESENBoolean):TBESENObjectPropertyEnumerator;
begin
 result:=TBESENObjectPropertyEnumerator.Create(Instance,self,GetOnlyOwnProperties,GetAllProperties);
end;

procedure TBESENObject.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var AResult:TBESENValue);
begin
 AResult:=BESENUndefinedValue;
end;

procedure TBESENObject.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var AResult:TBESENValue);
begin
 AResult:=BESENUndefinedValue;
end;

function TBESENObject.HasInstance(const AInstance:TBESENValue):TBESENBoolean;
begin
 result:=false;
end;

function TBESENObject.GetSecurityDomain:pointer;
begin
 result:=nil;
end;

function TBESENObject.HasCall:TBESENBoolean;
begin
 result:=false;
end;

function TBESENObject.HasConstruct:TBESENBoolean;
begin
 result:=false;
end;

function TBESENObject.HasHasInstance:TBESENBoolean;
begin
 result:=false;
end;

function TBESENObject.HasEnumerator:TBESENBoolean;
begin
 result:=false;
end;

function TBESENObject.HasGetSecurityDomain:TBESENBoolean;
begin
 result:=false;
end;

procedure TBESENObject.Finalize;
var CurrentItem:TBESENObjectProperty;
begin
 Prototype:=nil;
 CurrentItem:=Properties.GetFirst;
 while assigned(CurrentItem) do begin
  TBESEN(Instance).GarbageCollector.FinalizeValue(CurrentItem.Descriptor.Value);
  CurrentItem:=CurrentItem.Next;
 end;
 Properties.Clear;
 inherited Finalize;
end;

procedure TBESENObject.Mark;
var CurrentItem:TBESENObjectProperty;
begin
 TBESEN(Instance).GarbageCollector.GrayIt(Prototype);
 CurrentItem:=Properties.First;
 while assigned(CurrentItem) do begin
  TBESEN(Instance).GarbageCollector.GrayValue(CurrentItem.Descriptor.Value);
  TBESEN(Instance).GarbageCollector.GrayIt(TBESENObject(CurrentItem.Descriptor.Getter));
  TBESEN(Instance).GarbageCollector.GrayIt(TBESENObject(CurrentItem.Descriptor.Setter));
  CurrentItem:=CurrentItem.Next;
 end;
 inherited Mark;
end;

procedure BESENCheckObjectCoercible(const v:TBESENValue); {$ifdef caninline}inline;{$endif}
begin
 case v.ValueType of
  bvtUNDEFINED,bvtNULL,bvtREFERENCE,bvtLOCAL:begin
   BESENThrowTypeError('CheckObjectCoercible failed');
  end;
 end;
end;

function BESENIsCallable(const v:TBESENValue):TBESENBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(v.ValueType=bvtOBJECT) and (assigned(v.Obj) and TBESENObject(v.Obj).HasCall);
end;

end.
