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
unit BESENKeyIDManager;
{$i BESEN.inc} 

interface

uses BESENConstants,BESENTypes,BESENObject,BESENGarbageCollector,BESENHashMap,BESENStringList,
     BESENBaseObject;

type TBESENKeyIDManager=class(TBESENBaseObject)
      public
       HashMap:TBESENHashMap;
       List:TBESENStringList;
       Count:int64;
       CallerID:TBESENINT32;
       LengthID:TBESENINT32;
       ProtoID:TBESENINT32;
       constructor Create(AInstance:TObject); overload; override;
       destructor Destroy; override;
       function Get(const Key:TBESENString;Hash:TBESENHash=0):TBESENINT32;
       function GetString(const Key:TBESENString;Hash:TBESENHash=0):TBESENString;
       function Find(const Key:TBESENString;Hash:TBESENHash=0):TBESENINT32;
     end;

implementation

uses BESEN;

constructor TBESENKeyIDManager.Create(AInstance:TObject);
begin
 inherited Create(AInstance);
 HashMap:=TBESENHashMap.Create;
 List:=TBESENStringList.Create;
 Count:=0;
 CallerID:=Get('caller');
 LengthID:=Get('length');
 ProtoID:=Get('__proto__');
end;

destructor TBESENKeyIDManager.Destroy;
begin
 HashMap.Free;
 List.Free;
 inherited Destroy;
end;

function TBESENKeyIDManager.Get(const Key:TBESENString;Hash:TBESENHash=0):TBESENINT32;
var Item:PBESENHashMapItem;
begin
 Item:=HashMap.GetKey(Key,Hash);
 if TBESEN(Instance).InlineCacheEnabled and not assigned(Item) then begin
  inc(Count);
  if Count<bimMAXIDENTS then begin
   Item:=HashMap.NewKey(Key,true,Hash);
   Item^.Value:=List.Add(Key);
  end else begin
   TBESEN(Instance).InlineCacheEnabled:=false;
   List.Clear;
   HashMap.Clear;
  end;
 end;
 if assigned(Item) then begin
  result:=Item^.Value;
 end else begin
  result:=-1;
 end;
end;

function TBESENKeyIDManager.GetString(const Key:TBESENString;Hash:TBESENHash=0):TBESENString;
var Item:PBESENHashMapItem;
begin
 Item:=HashMap.GetKey(Key,Hash);
 if TBESEN(Instance).InlineCacheEnabled and assigned(Item) then begin
  result:=List[Item^.Value];
 end else begin
  result:=Key;
 end;
end;

function TBESENKeyIDManager.Find(const Key:TBESENString;Hash:TBESENHash=0):TBESENINT32;
var Item:PBESENHashMapItem;
begin
 Item:=HashMap.GetKey(Key,Hash);
 if TBESEN(Instance).InlineCacheEnabled and assigned(Item) then begin
  result:=Item^.Value;
 end else begin
  result:=-1;
 end;
end;

end.
