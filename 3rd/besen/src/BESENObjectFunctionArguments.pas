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
unit BESENObjectFunctionArguments;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectFunctionArguments=class(TBESENObject)
      public
       ParameterMap:TBESENObject;
       IsStrict:longbool;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       function GetOwnProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean; override;
       function GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean; override;
       function GetIndex(const Index,ID:integer;var AResult:TBESENValue;Base:TBESENObject=nil):boolean; override;
       function DeleteEx(const P:TBESENString;Throw:TBESENBoolean;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       procedure PutEx(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0); override;
       procedure PutIndex(const Index,ID:integer;const V:TBESENValue;Throw:TBESENBoolean); override;
       function DefineOwnPropertyEx(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;var Current:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean; override;
       procedure DefaultValue(const AHint:TBESENValue;var AResult:TBESENValue); override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENGlobals,BESENStringUtils,BESENErrors;

constructor TBESENObjectFunctionArguments.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Arguments';
 ObjectName:='Arguments';
 ParameterMap:=nil;
end;

destructor TBESENObjectFunctionArguments.Destroy;
begin
 inherited Destroy;
end;

function TBESENObjectFunctionArguments.GetOwnProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean;
var MappedDescriptor:TBESENObjectPropertyDescriptor;
begin
 if IsStrict or not assigned(ParameterMap) then begin
  result:=inherited GetOwnProperty(P,Descriptor,Hash);
 end else begin
  result:=inherited GetOwnProperty(P,Descriptor,Hash);
  if result then begin
   MappedDescriptor:=Descriptor;
   if not ParameterMap.GetOwnProperty(P,MappedDescriptor,Hash) then begin
    ParameterMap.Get(P,Descriptor.Value);
    result:=Descriptor.Presents<>[];
   end;
  end;
 end;
end;

function TBESENObjectFunctionArguments.GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 if IsStrict or not assigned(ParameterMap) then begin
  result:=inherited GetEx(P,AResult,Descriptor,Base,Hash);
 end else begin
  if ParameterMap.GetOwnProperty(P,Descriptor,Hash) then begin
   result:=ParameterMap.GetEx(P,AResult,Descriptor,Base,Hash);
  end else begin
   result:=inherited GetEx(P,AResult,Descriptor,Base,Hash);
   if IsStrict and (P='caller') then begin
    BESENThrowCaller;
   end;
  end;
 end;
end;

function TBESENObjectFunctionArguments.GetIndex(const Index,ID:integer;var AResult:TBESENValue;Base:TBESENObject=nil):boolean;
var Descriptor:TBESENObjectPropertyDescriptor;
begin
 if not assigned(Base) then begin
  Base:=self;
 end;
 if IsStrict or not assigned(ParameterMap) then begin
  result:=inherited GetIndex(Index,ID,AResult,Base);
 end else begin
  if ParameterMap.GetOwnProperty(TBESEN(Instance).KeyIDManager.List[ID],Descriptor) then begin
   result:=ParameterMap.GetEx(TBESEN(Instance).KeyIDManager.List[ID],AResult,Descriptor,Base);
  end else begin
   result:=inherited GetIndex(Index,ID,AResult,Base);
   if IsStrict and (ID=TBESEN(Instance).KeyIDManager.CallerID) then begin
    BESENThrowCaller;
   end;
  end;
 end;
end;

function TBESENObjectFunctionArguments.DeleteEx(const P:TBESENString;Throw:TBESENBoolean;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
var IsMapped:boolean;
begin
 if IsStrict or not assigned(ParameterMap) then begin
  result:=inherited DeleteEx(P,Throw,Descriptor,Hash);
 end else begin
  IsMapped:=ParameterMap.GetOwnProperty(P,Descriptor,Hash);
  result:=inherited DeleteEx(P,Throw,Descriptor,Hash);
  if result and IsMapped then begin
   ParameterMap.DeleteEx(P,false,Descriptor,Hash);
  end;
 end;
end;

procedure TBESENObjectFunctionArguments.PutEx(const P:TBESENString;const V:TBESENValue;Throw:TBESENBoolean;var Descriptor,OwnDescriptor:TBESENObjectPropertyDescriptor;var TempValue:TBESENValue;Hash:TBESENHash=0);
begin
 PutFull(P,V,false,Descriptor,OwnDescriptor,TempValue,Hash);
end;

procedure TBESENObjectFunctionArguments.PutIndex(const Index,ID:integer;const V:TBESENValue;Throw:TBESENBoolean);
begin
 Put(TBESEN(Instance).KeyIDManager.List[ID],V,Throw);
end;

function TBESENObjectFunctionArguments.DefineOwnPropertyEx(const P:TBESENString;const Descriptor:TBESENObjectPropertyDescriptor;Throw:TBESENBoolean;var Current:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):TBESENBoolean;
var IsMapped,Allowed:boolean;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('DefineOwnProperty for "'+P+'" failed');
 end;
begin
 if IsStrict or not assigned(ParameterMap) then begin
  result:=inherited DefineOwnPropertyEx(P,Descriptor,false,Current,Hash);
 end else begin
  IsMapped:=ParameterMap.GetOwnProperty(P,Current,Hash);
  Allowed:=inherited DefineOwnPropertyEx(P,Descriptor,false,Current,Hash);
  if not Allowed then begin
   if Throw then begin
    ThrowIt;
   end;
   result:=false;
   exit;
  end;
  if IsMapped then begin
   if ([boppGETTER,boppSETTER]*Descriptor.Presents)<>[] then begin
    ParameterMap.DeleteEx(p,false,Current,Hash);
   end else begin
    if boppVALUE in Descriptor.Presents then begin
     ParameterMap.Put(p,Descriptor.Value,Throw,Hash);
    end;
    if (boppWRITABLE in Descriptor.Presents) and not (bopaWRITABLE in Descriptor.Attributes) then begin
     ParameterMap.DeleteEx(p,false,Current,Hash);
    end;
   end;
  end;
  result:=true;
 end;
end;

procedure TBESENObjectFunctionArguments.DefaultValue(const AHint:TBESENValue;var AResult:TBESENValue);
var i,j:integer;
    s:TBESENString;
    v:TBESENValue;
 procedure ThrowIt;
 begin
  BESENThrowTypeError('Bad default value');
 end;
begin
 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  s:='[';
  Get('length',v,self,BESENLengthHash);
  j:=TBESEN(Instance).ToInt32(v);
  for i:=0 to j-1 do begin
   if i>0 then begin
    s:=s+', ';
   end;
   Get(IntToStr(i),v);
   s:=s+inttostr(i)+'='+TBESEN(Instance).ToStr(v);
  end;
  s:=s+']';
  AResult:=BESENStringValue(s);
 end else begin
  inherited DefaultValue(AHint,AResult);
 end;
end;

procedure TBESENObjectFunctionArguments.Finalize;
begin
 ParameterMap:=nil;
 inherited Finalize;
end;

procedure TBESENObjectFunctionArguments.Mark;
begin
 if assigned(ParameterMap) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(ParameterMap);
 end;
 inherited Mark;
end;

end.
 