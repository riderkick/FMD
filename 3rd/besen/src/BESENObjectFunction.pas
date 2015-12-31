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
unit BESENObjectFunction;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectFunction=class(TBESENObject)
      public
       SecurityDomain:pointer;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       function GetSecurityDomain:pointer; override;
       function HasGetSecurityDomain:TBESENBoolean; override;
       function HasInstance(const AInstance:TBESENValue):TBESENBoolean; override;
       function HasHasInstance:TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENErrors;

constructor TBESENObjectFunction.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin                    
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='function';

 SecurityDomain:=nil;

 OverwriteData('length',BESENNumberValue(1),[]);
end;

destructor TBESENObjectFunction.Destroy;
begin
 inherited Destroy;
end;

function TBESENObjectFunction.GetSecurityDomain:pointer;
begin
 result:=SecurityDomain;
end;

function TBESENObjectFunction.HasGetSecurityDomain:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectFunction.HasInstance(const AInstance:TBESENValue):TBESENBoolean;
var v,o:TBESENObject;
    ov:TBESENValue;
begin
 result:=false;
 if AInstance.ValueType<>bvtOBJECT then begin
  exit;
 end;
 v:=TBESENObject(AInstance.Obj);
 Get('prototype',ov);
 if ov.ValueType<>bvtOBJECT then begin
  raise EBESENTypeError.Create('Prototype not object');
 end;
 o:=TBESENObject(ov.Obj);
 while true do begin
  v:=v.Prototype;
  if not assigned(v) then begin
   break;
  end else if TBESEN(Instance).SameObject(v,o) then begin
   result:=true;
   break;
  end;
 end;
end;

function TBESENObjectFunction.HasHasInstance:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectFunction.Finalize;
begin
 inherited Finalize;
end;

procedure TBESENObjectFunction.Mark;
begin
 inherited Mark;
end;

end.
 