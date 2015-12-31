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
unit BESENObjectRegExpConstructor;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectRegExpConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       function HasInstance(const AInstance:TBESENValue):TBESENBoolean; override;
       function HasHasInstance:TBESENBoolean; override;
     end;

implementation

uses BESEN,BESENObjectRegExp,BESENRegExp,BESENErrors;

constructor TBESENObjectRegExpConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='RegExp';
end;

destructor TBESENObjectRegExpConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectRegExpConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObjectRegExp;
    s,f:TBESENString;
    Flags:TBESENRegExpFlags;
    v:TBESENValue;
    i:integer;
begin
 r1:=TBESENObjectRegExp.Create(Instance,TBESEN(Instance).ObjectRegExpPrototype,false);
 TBESEN(Instance).GarbageCollector.Add(r1);
 r1.GarbageCollectorLock;
 try
  if (CountArguments>0) and (Arguments^[0]^.ValueType=bvtOBJECT) and assigned(Arguments^[0]^.Obj) and (Arguments^[0]^.Obj is TBESENObjectRegExp) then begin
   r1.Engine:=TBESENObjectRegExp(Arguments^[0]^.Obj).Engine;
  end else begin
   if (CountArguments<1) or (Arguments^[0]^.ValueType=bvtUNDEFINED) then begin
    s:='';
   end else begin
    s:=TBESEN(Instance).ToStr(Arguments^[0]^);
   end;
   if CountArguments>1 then begin
    f:=TBESEN(Instance).ToStr(Arguments^[1]^);
   end else begin
    f:='';
   end;
   Flags:=[];
   for i:=1 to length(f) do begin
    case f[i] of
     'g':begin
      if brefGLOBAL in Flags then begin
       raise EBESENSyntaxError.Create('Too many global regular expression flags');
      end else begin
       Flags:=Flags+[brefGLOBAL];
      end;
     end;
     'i':begin
      if brefIGNORECASE in Flags then begin
       raise EBESENSyntaxError.Create('Too many ignorecase regular expression flags');
      end else begin
       Flags:=Flags+[brefIGNORECASE];
      end;
     end;
     'm':begin
      if brefMULTILINE in Flags then begin
       raise EBESENSyntaxError.Create('Too many multiline regular expression flags');
      end else begin
       Flags:=Flags+[brefMULTILINE];
      end;
     end;
     else begin
      raise EBESENSyntaxError.Create('Unknown regular expression flag');
     end;
    end;
   end;
   try
    r1.Engine:=TBESEN(Instance).RegExpCache.Get(s,Flags);
   except
    raise EBESENSyntaxError.Create('Invalid regular expression');
   end;
  end;

  if assigned(r1.Engine) then begin
   r1.Engine.IncRef;
  end else begin
   raise EBESENError.Create('Fatal error');
  end;

  r1.Engine.DebugDump;

  v.ValueType:=bvtSTRING;
  v.Str:=r1.Engine.Source;
  r1.OverwriteData('source',v,[]);

  v.ValueType:=bvtBOOLEAN;
  v.Bool:=brefGLOBAL in r1.Engine.Flags;
  r1.OverwriteData('global',v,[]);

  v.ValueType:=bvtBOOLEAN;
  v.Bool:=brefIGNORECASE in r1.Engine.Flags;
  r1.OverwriteData('ignoreCase',v,[]);

  v.ValueType:=bvtBOOLEAN;
  v.Bool:=brefMULTILINE in r1.Engine.Flags;
  r1.OverwriteData('multiline',v,[]);

  v.ValueType:=bvtNUMBER;
  v.Num:=0;
  r1.OverwriteData('lastIndex',v,[bopaWRITABLE]);
 finally
  r1.GarbageCollectorUnlock;
 end;
 AResult:=BESENObjectValue(r1);
end;

procedure TBESENObjectRegExpConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 Construct(ThisArgument,Arguments,CountArguments,AResult);
end;

function TBESENObjectRegExpConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectRegExpConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectRegExpConstructor.HasInstance(const AInstance:TBESENValue):TBESENBoolean;
begin
 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  result:=(AInstance.ValueType=bvtOBJECT) and assigned(AInstance.Obj) and (AInstance.Obj is TBESENObjectRegExp);
 end else begin
  raise EBESENTypeError.Create('Has no instance');
 end;
end;

function TBESENObjectRegExpConstructor.HasHasInstance:TBESENBoolean;
begin
 result:=(TBESEN(Instance).Compatibility and COMPAT_JS)<>0;
end;

end.
