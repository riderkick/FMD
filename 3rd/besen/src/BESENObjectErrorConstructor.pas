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
unit BESENObjectErrorConstructor;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectErrorConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
     end;

implementation

uses BESEN,BESENObjectError;

constructor TBESENObjectErrorConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='Error';
end;

destructor TBESENObjectErrorConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectErrorConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObjectError;
    r2:TBESENObject;
    r3:TBESENValue;
begin
 // ES5 errata fix
 Get('prototype',r3);
 if r3.ValueType=bvtOBJECT then begin
  r2:=TBESENObject(r3.Obj);
 end else begin
  r2:=nil;
 end;
 r1:=TBESENObjectError.Create(Instance,r2,false);
 TBESEN(Instance).GarbageCollector.Add(r1);
 r1.GarbageCollectorLock;
 try
  if (CountArguments>0) and (Arguments^[0]^.ValueType<>bvtUNDEFINED) then begin
   r1.OverwriteData('message',BESENStringValue(TBESEN(Instance).ToStr(Arguments^[0]^)),[bopaWRITABLE,bopaCONFIGURABLE]);
  end else begin
   r1.OverwriteData('message',BESENStringValue(''),[bopaWRITABLE,bopaCONFIGURABLE]);
  end;
 finally
  r1.GarbageCollectorUnlock;
 end;
 AResult.ValueType:=bvtOBJECT;
 AResult.Obj:=r1;
end;

procedure TBESENObjectErrorConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 // ES5 Errate fix
 Construct(ThisArgument,Arguments,CountArguments,AResult);
end;

function TBESENObjectErrorConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectErrorConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

end.
