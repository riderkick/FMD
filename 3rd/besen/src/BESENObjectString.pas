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
unit BESENObjectString;
{$i BESEN.inc}

interface

uses SysUtils,Math,BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectString=class(TBESENObject)
      public
       Value:TBESENString;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       function GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean; override;
       function GetOwnProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean; override;
       procedure Finalize; override;
       procedure Mark; override;
       procedure UpdateLength;
     end;

implementation

uses BESEN,BESENStringUtils,BESENNumberUtils;

constructor TBESENObjectString.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='String';
 ObjectName:='';

 Value:='';
 OverwriteData('length',BESENNumberValue(0),[]);
end;

destructor TBESENObjectString.Destroy;
begin
 Value:='';
 inherited Destroy;
end;

function TBESENObjectString.GetEx(const P:TBESENString;var AResult:TBESENValue;var Descriptor:TBESENObjectPropertyDescriptor;Base:TBESENObject=nil;Hash:TBESENHash=0):boolean;
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

function TBESENObjectString.GetOwnProperty(const P:TBESENString;var Descriptor:TBESENObjectPropertyDescriptor;Hash:TBESENHash=0):boolean;
var Index:int64;
    v:TBESENValue;
begin
 // ES5 errata fix
 Descriptor.Value.ValueType:=BESENUndefinedPropertyDescriptor.Value.ValueType;
 Descriptor.Getter:=BESENUndefinedPropertyDescriptor.Getter;
 Descriptor.Setter:=BESENUndefinedPropertyDescriptor.Setter;
 Descriptor.Attributes:=BESENUndefinedPropertyDescriptor.Attributes;
 Descriptor.Presents:=BESENUndefinedPropertyDescriptor.Presents;
 result:=inherited GetOwnProperty(P,Descriptor,Hash);
 if not result then begin
  TBESEN(Instance).ToIntegerValue(BESENStringValue(p),v);
  if BESENIsFinite(v.Num) then begin
   Index:=BESENToInt(v.Num);
   if (IntToStr(abs(Index))=P) and ((Index>=0) and (Index<length(Value))) then begin
    Descriptor.Value.ValueType:=bvtSTRING;
    Descriptor.Value.Str:=copy(Value,Index+1,1);
    Descriptor.Getter:=nil;
    Descriptor.Setter:=nil;
    Descriptor.Attributes:=[bopaENUMERABLE];
    Descriptor.Presents:=[boppVALUE,boppWRITABLE,boppENUMERABLE,boppCONFIGURABLE];
    result:=true;
   end;
  end;
 end;
end;

procedure TBESENObjectString.Finalize;
begin
 inherited Finalize;
end;

procedure TBESENObjectString.Mark;
begin
 inherited Mark;
end;

procedure TBESENObjectString.UpdateLength;
begin
 OverwriteData('length',BESENNumberValue(length(Value)),[]);
end;

end.
