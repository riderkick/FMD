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
unit BESENObjectStringConstructor;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectStringConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       procedure NativeFromCharCode(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENObjectString;

constructor TBESENObjectStringConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='String';

 OverwriteData('length',BESENNumberValue(1),[]);

 RegisterNativeFunction('fromCharCode',NativeFromCharCode,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectStringConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectStringConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObjectString;
begin
 r1:=TBESENObjectString.Create(Instance,TBESEN(Instance).ObjectStringPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(r1);
 r1.GarbageCollectorLock;
 try
  if CountArguments>0 then begin
   r1.Value:=TBESEN(Instance).ToStr(Arguments^[0]^);
  end else begin
   r1.Value:='';
  end;
  r1.UpdateLength;
 finally
  r1.GarbageCollectorUnlock;
 end;
 AResult.ValueType:=bvtOBJECT;
 AResult.Obj:=r1;
end;

procedure TBESENObjectStringConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 if CountArguments<1 then begin
  AResult:=BESENStringValue('');
 end else begin
  TBESEN(Instance).ToStringValue(Arguments^[0]^,AResult);
 end;
end;

function TBESENObjectStringConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectStringConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectStringConstructor.NativeFromCharCode(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var s:TBESENString;
    i:integer;
begin
 s:='';
 for i:=0 to CountArguments-1 do begin
  s:=s+widechar(word(TBESEN(Instance).ToUInt16(Arguments^[i]^)));
 end;
 ResultValue:=BESENStringValue(s);
end;

end.
