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
unit BESENObjectDate;
{$i BESEN.inc}

interface

uses SysUtils,Math,BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectDate=class(TBESENObject)
      public
       Value:TBESENDate;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure DefaultValue(const AHint:TBESENValue;var AResult:TBESENValue); override;
       procedure Finalize; override;
       procedure Mark; override;
     end;
     
implementation

uses BESEN;

constructor TBESENObjectDate.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Date';
 ObjectName:='';

 Value:=Now;
end;

destructor TBESENObjectDate.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectDate.DefaultValue(const AHint:TBESENValue;var AResult:TBESENValue);
begin
 case AHint.ValueType of
  bvtNUMBER,bvtSTRING:begin
   inherited DefaultValue(AHint,AResult);
  end;
  bvtOBJECT:begin
   if AHint.Obj<>TBESEN(Instance).ObjectNumberConstructor then begin
    inherited DefaultValue(TBESEN(Instance).ObjectStringConstructorValue,AResult);
   end else begin
    inherited DefaultValue(AHint,AResult);
   end;
  end;
  else begin
   inherited DefaultValue(TBESEN(Instance).ObjectStringConstructorValue,AResult);
  end;
 end;
end;

procedure TBESENObjectDate.Finalize;
begin
 inherited Finalize;
end;

procedure TBESENObjectDate.Mark;
begin
 inherited Mark;
end;

end.
