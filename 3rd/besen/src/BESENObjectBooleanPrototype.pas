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
unit BESENObjectBooleanPrototype;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor,
     BESENObjectBoolean;

type TBESENObjectBooleanPrototype=class(TBESENObjectBoolean)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENErrors;

constructor TBESENObjectBooleanPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype);
 ObjectClassName:='Boolean';
 ObjectName:='Boolean';

 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;
 
 RegisterNativeFunction('toString',NativeToString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('valueOf',NativeValueOf,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectBooleanPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectBooleanPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if ThisArgument.ValueType=bvtBOOLEAN then begin
  if ThisArgument.Bool then begin
   ResultValue:=BESENStringValue('true');
  end else begin
   ResultValue:=BESENStringValue('false');
  end;
 end else if (ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj)) then begin
  if TBESENObjectBoolean(TBESENObject(ThisArgument.Obj)).Value then begin
   ResultValue:=BESENStringValue('true');
  end else begin
   ResultValue:=BESENStringValue('false');
  end;
 end else begin
  raise EBESENTypeError.Create('Not a boolean object');
 end;
end;

procedure TBESENObjectBooleanPrototype.NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if ThisArgument.ValueType=bvtBOOLEAN then begin
  BESENCopyValue(ResultValue,ThisArgument);
 end else if (ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj)) then begin
  ResultValue.ValueType:=bvtBOOLEAN;
  ResultValue.Bool:=TBESENObjectBoolean(TBESENObject(ThisArgument.Obj)).Value;
 end else begin
  raise EBESENTypeError.Create('Not a boolean object');
 end;
end;

end.
