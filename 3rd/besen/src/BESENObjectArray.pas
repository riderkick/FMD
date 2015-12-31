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
unit BESENObjectArray;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectArray=class(TBESENObject)
      private
       ArrayLength:TBESENUINT32;
       function ToIndex(AProp:TBESENString;var v:int64):TBESENBoolean;
       function GetLen:TBESENUINT32;
       procedure SetLen(NewLen:TBESENUINT32);
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure PutEx(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0); override;
       procedure PutIndex(const Index,ID:integer;const V:TBESENValue;Throw:TBESENBoolean); override;
       function DefineOwnPropertyEx(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;var Current:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       function GetArrayIndex(const Index:TBESENUINT32;var AResult:TBESENValue;Base:TBESENObject=nil):boolean; override;
       procedure PutArrayIndex(const Index:TBESENUINT32;const V:TBESENValue;Throw:TBESENBoolean); override;
       function DeleteArrayIndex(const Index:TBESENUINT32;Throw:TBESENBoolean):TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
       procedure Push(const AValue:TBESENValue);
       property Len:TBESENUINT32 read GetLen write SetLen;
     end;

implementation

uses BESEN,BESENUtils,BESENArrayUtils,BESENHashUtils,BESENGlobals,
     BESENNumberUtils,BESENErrors;

constructor TBESENObjectArray.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Array';
 ObjectName:='array';
 Len:=0;
 ArrayLength:=0;

 Properties.Sort(true);

 inherited OverwriteData('length',BESENNumberValue(0),[bopaWRITABLE]);
end;

destructor TBESENObjectArray.Destroy;
begin
 inherited Destroy;
end;

function TBESENObjectArray.ToIndex(AProp:TBESENString;var v:int64):TBESENBoolean;
begin
 result:=BESENArrayToIndex(AProp,v);
end;

{procedure TBESENObjectArray.SetLen(const AValue:TBESENValue);
var NewLen,i:TBESENUInt32;
begin
 NewLen:=TBESEN(Instance).ToUInt32(AValue);
 if Len>NewLen then begin
  for i:=Len+1 to NewLen do begin
   inherited Delete(IndexToStr(i-1),true);
  end;
 end;
 Len:=NewLen;
end;}

function TBESENObjectArray.GetLen:TBESENUINT32;
var LenDesc:TBESENObjectPropertyDescriptor;
begin
 GetOwnProperty('length',LenDesc,BESENLengthHash);
 result:=TBESEN(Instance).ToUINT32(LenDesc.Value);
 ArrayLength:=result;
end;

procedure TBESENObjectArray.SetLen(NewLen:TBESENUINT32);
begin
 ArrayLength:=NewLen;
 inherited OverwriteData('length',BESENNumberValue(NewLen),[bopaWRITABLE]);
end;

procedure TBESENObjectArray.PutEx(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0);
var Index:int64;
    Prop:TBESENObjectProperty;
begin
 // Trying faster pre-catching implementation first
 if ToIndex(P,Index) then begin
  if (Index>=0) and (Index<ArrayLength) then begin
   Prop:=Properties.Get(P,Hash);
   if assigned(Prop) then begin
    if (([boppVALUE,boppWRITABLE]*Prop.Descriptor.Presents)<>[]) and (bopaWRITABLE in Prop.Descriptor.Attributes) then begin
     BESENCopyValue(Prop.Descriptor.Value,v);
     exit;
    end;
   end else if Extensible then begin
    Prop:=Properties.Put(P,Hash);
    if assigned(Prop) then begin
     BESENCopyValue(Prop.Descriptor.Value,v);
     Prop.Descriptor.Getter:=nil;
     Prop.Descriptor.Setter:=nil;
     Prop.Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
     Prop.Descriptor.Attributes:=[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE];
     if Prop.ID>=0 then begin
      InvalidateStructure;
     end;
     exit;
    end;
   end;
  end;
 end;

 // Fallback to specification-conformant implementation
 PutFull(P,V,Throw,Descriptor,OwnDescriptor,TempValue,Hash);
end;

procedure TBESENObjectArray.PutIndex(const Index,ID:integer;const V:TBESENValue;Throw:TBESENBoolean);
begin
 if ID=TBESEN(Instance).KeyIDManager.LengthID then begin
  Put(TBESEN(Instance).KeyIDManager.List[ID],V,Throw);
 end else if ((Index>=0) and (Index<Properties.ItemCount)) and assigned(Properties.Items[Index]) and (Properties.Items[Index].ID=ID) then begin
  inherited PutIndex(Index,ID,V,Throw);
 end else begin
  Put(TBESEN(Instance).KeyIDManager.List[ID],V,Throw);
 end;
end;

function TBESENObjectArray.DefineOwnPropertyEx(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;var Current:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
var OldLenDesc,NewLenDesc:TBESENObjectPropertyDescriptor;
    NewLen,OldLen:TBESENUInt32;
    NewWritable:boolean;
    Index:int64;
    Num:double;
begin
 GetOwnProperty('length',OldLenDesc,BESENLengthHash);
 OldLen:=TBESEN(Instance).ToUINT32(OldLenDesc.Value);
 if P='length' then begin
  if not (boppVALUE in Descriptor.Presents) then begin
   result:=inherited DefineOwnPropertyEx('length',Descriptor,Throw,Current,BESENLengthHash);
   exit;
  end;
  NewLenDesc:=Descriptor;
  NewLen:=TBESEN(Instance).ToUInt32(Descriptor.Value);
  Num:=TBESEN(Instance).ToNum(Descriptor.Value);
  if (NewLen<>Num) or not BESENIsFinite(Num) then begin
   raise EBESENRangeError.CreateUTF16('DefineOwnProperty for "'+P+'" failed');
  end;
  if NewLen>=OldLen then begin
   ArrayLength:=NewLen;
   result:=inherited DefineOwnPropertyEx('length',Descriptor,Throw,Current,BESENLengthHash);
   exit;
  end;
  if not ((boppWRITABLE in OldLenDesc.Presents) and (bopaWRITABLE in OldLenDesc.Attributes)) then begin
   if Throw then begin
    raise EBESENTypeError.CreateUTF16('DefineOwnProperty for "'+P+'" failed');
   end else begin
    result:=false;
    exit;
   end;
  end;
  if ((boppWRITABLE in NewLenDesc.Presents) and (bopaWRITABLE in NewLenDesc.Attributes)) or not (bopaWRITABLE in NewLenDesc.Attributes) then begin
   NewWritable:=true;
  end else begin
   NewWritable:=false;
   NewLenDesc.Presents:=NewLenDesc.Presents+[boppWRITABLE];
   NewLenDesc.Attributes:=NewLenDesc.Attributes+[bopaWRITABLE];
  end;
  result:=inherited DefineOwnPropertyEx('length',NewLenDesc,Throw,Current,BESENLengthHash);
  if not result then begin
   exit;
  end;
  ArrayLength:=NewLen;
  while NewLen<OldLen do begin
   dec(OldLen);
   if not Delete(BESENArrayIndexToStr(OldLen),false) then begin
    NewLenDesc.Value:=BESENNumberValue(OldLen+1);
    if not NewWritable then begin
     NewLenDesc.Attributes:=NewLenDesc.Attributes-[bopaWRITABLE];
    end;
    inherited DefineOwnPropertyEx('length',NewLenDesc,false,Current,BESENLengthHash);
    if Throw then begin
     raise EBESENTypeError.CreateUTF16('DefineOwnProperty for "'+P+'" failed');
    end else begin
     result:=false;
     exit;
    end;
   end;
  end;
  if not NewWritable then begin
   NewLenDesc:=BESENUndefinedPropertyDescriptor;
   NewLenDesc.Presents:=NewLenDesc.Presents+[boppWRITABLE];
   NewLenDesc.Attributes:=NewLenDesc.Attributes-[bopaWRITABLE];
   inherited DefineOwnPropertyEx('length',NewLenDesc,false,Current,BESENLengthHash);
  end;
  result:=true;
 end else if ToIndex(P,Index) then begin
  if (Index>=OldLen) and not ((boppWRITABLE in OldLenDesc.Presents) and (bopaWRITABLE in OldLenDesc.Attributes)) then begin
   if Throw then begin
    raise EBESENTypeError.CreateUTF16('DefineOwnProperty for "'+P+'" failed');
   end else begin
    result:=false;
    exit;
   end;
  end;
  result:=inherited DefineOwnPropertyEx(P,Descriptor,false,Current,Hash);
  if not result then begin
   if Throw then begin
    raise EBESENTypeError.CreateUTF16('DefineOwnProperty for "'+P+'" failed');
   end else begin
    result:=false;
    exit;
   end;
  end;
  if Index>=OldLen then begin
   ArrayLength:=Index+1;
   OldLenDesc.Value:=BESENNumberValue(Index+1);
   inherited DefineOwnPropertyEx('length',OldLenDesc,false,Current,BESENLengthHash);
  end;
  result:=true;
 end else begin
  result:=inherited DefineOwnPropertyEx(P,Descriptor,Throw,Current,Hash);
 end;
end;

function TBESENObjectArray.GetArrayIndex(const Index:TBESENUINT32;var AResult:TBESENValue;Base:TBESENObject=nil):boolean;
 function Fallback:boolean;
 begin
  result:=Get(BESENArrayIndexToStr(Index),AResult,Base);
 end;
var Prop:TBESENObjectProperty;
begin
 result:=false;
 if not assigned(Base) then begin
  Base:=self;
 end;
 if Index<TBESENUINT32(Properties.ArrayItemCount) then begin
  Prop:=Properties.ArrayGet(Index);
  if assigned(Prop) then begin
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
 result:=Fallback;
end;

procedure TBESENObjectArray.PutArrayIndex(const Index:TBESENUINT32;const V:TBESENValue;Throw:TBESENBoolean);
 procedure Fallback;
 begin
  Put(BESENArrayIndexToStr(Index),V,Throw);
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
var Prop:TBESENObjectProperty;
begin
 if Index<TBESENUINT32(Properties.ArrayItemCount) then begin
  Prop:=Properties.ArrayGet(Index);
  if assigned(Prop) then begin
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
 Fallback;
end;

function TBESENObjectArray.DeleteArrayIndex(const Index:TBESENUINT32;Throw:TBESENBoolean):TBESENBoolean;
 function Fallback:boolean;
 begin
  result:=Delete(BESENArrayIndexToStr(Index),Throw);
 end;
var Prop:TBESENObjectProperty;
begin
 if Index<TBESENUINT32(Properties.ArrayItemCount) then begin
  Prop:=Properties.ArrayGet(Index);
  if assigned(Prop) then begin
   result:=Delete(Prop.Key,Throw);
   exit;
  end;
 end;
 result:=Fallback;
end;

procedure TBESENObjectArray.Finalize;
begin
 inherited Finalize;
end;

procedure TBESENObjectArray.Mark;
begin
 inherited Mark;
end;

procedure TBESENObjectArray.Push(const AValue:TBESENValue);
var v,tv:TBESENValue;
    ValuePointers:array[0..0] of PBESENValue;
begin
 Get('push',v);
 if (v.ValueType=bvtOBJECT) and assigned(TBESENObject(v.Obj)) and TBESENObject(v.Obj).HasCall then begin
  ValuePointers[0]:=@AValue;
  TBESEN(Instance).ObjectCall(TBESENObject(v.Obj),BESENObjectValue(self),@ValuePointers,1,tv);
 end;
end;

end.
