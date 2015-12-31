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
unit BESENObjectErrorPrototype;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectError,BESENValue,BESENObjectPropertyDescriptor,
     BESENObjectBoolean;

type TBESENObjectErrorPrototype=class(TBESENObjectError)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENErrors;

constructor TBESENObjectErrorPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Error';
 ObjectName:='Error';

 OverwriteData('name',BESENStringValue('Error'),[bopaWRITABLE,bopaCONFIGURABLE]);
 OverwriteData('message',BESENStringValue(''),[bopaWRITABLE,bopaCONFIGURABLE]);

 RegisterNativeFunction('toString',NativeToString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);

end;

destructor TBESENObjectErrorPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectErrorPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var nv,mv:TBESENValue;
    n,m:TBESENString;
begin
 // ES5 errata fix
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 TBESENObjectError(TBESENObject(ThisArgument.Obj)).Get('name',nv);
 TBESENObjectError(TBESENObject(ThisArgument.Obj)).Get('message',mv);
 if nv.ValueType=bvtUNDEFINED then begin
  n:='Error';
 end else begin
  n:=TBESEN(Instance).ToStr(nv);
 end;
 if mv.ValueType=bvtUNDEFINED then begin
  m:='';
 end else begin
  m:=TBESEN(Instance).ToStr(mv);
 end;
 if (length(n)=0) and (length(m)=0) then begin
  ResultValue:=BESENStringValue('Error');
 end else if length(n)=0 then begin
  ResultValue:=BESENStringValue(m);
 end else if length(m)=0 then begin
  ResultValue:=BESENStringValue(n);
 end else begin
  ResultValue:=BESENStringValue(n+': '+m);
 end;
end;

end.
