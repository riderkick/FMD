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
unit BESENObjectArgGetterFunction;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor,BESENEnvironmentRecord;

type TBESENObjectArgGetterFunction=class(TBESENObjectFunction)
      public
       Env:TBESENEnvironmentRecord;
       ArgName:TBESENString;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasCall:TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
     end;

implementation

uses BESEN,BESENErrors,BESENHashUtils;

constructor TBESENObjectArgGetterFunction.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='ArgGetter';
 Env:=nil;
 ArgName:='';
end;

destructor TBESENObjectArgGetterFunction.Destroy;
begin
 Env:=nil;
 ArgName:='';
 inherited Destroy;
end;

procedure TBESENObjectArgGetterFunction.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 Env.GetBindingValue(ArgName,true,AResult,BESENHashKey(ArgName));
end;

function TBESENObjectArgGetterFunction.HasCall:TBESENBoolean;
begin
 result:=true;
end;

procedure TBESENObjectArgGetterFunction.Finalize;
begin
 Env:=nil;
 inherited Finalize;
end;

procedure TBESENObjectArgGetterFunction.Mark;
begin
 if assigned(Env) then begin
  TBESEN(Instance).GarbageCollector.GrayIt(Env);
 end;
 inherited Mark;
end;

end.
