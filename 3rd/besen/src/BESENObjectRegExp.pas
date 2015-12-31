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
unit BESENObjectRegExp;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENValue,BESENObjectPropertyDescriptor,
     BESENRegExp;

type TBESENObjectRegExp=class(TBESENObject)
      public
       Engine:TBESENRegExp;
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasCall:TBESENBoolean; override;
       procedure Finalize; override;
       procedure Mark; override;
       procedure SetStatic(const Input:TBESENString;const Captures:TBESENRegExpCaptures);
     end;

implementation

uses BESEN,BESENErrors;

constructor TBESENObjectRegExp.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
var bv:TBESENValue;
begin
 inherited Create(AInstance,APrototype);
 ObjectClassName:='RegExp';
 ObjectName:='';

 Engine:=TBESEN(Instance).DefaultRegExp;

 OverwriteData('source',BESENStringValue(''),[]);

 bv.ValueType:=bvtBOOLEAN;
 bv.Bool:=brefGLOBAL in Engine.Flags;
 OverwriteData('global',bv,[]);

 bv.Bool:=brefIGNORECASE in Engine.Flags;
 OverwriteData('ignoreCase',bv,[]);

 bv.Bool:=brefMULTILINE in Engine.Flags;
 OverwriteData('multiline',bv,[]);

 OverwriteData('lastIndex',BESENNumberValue(0),[bopaWRITABLE]);
end;

destructor TBESENObjectRegExp.Destroy;
begin
 if assigned(Engine) then begin
  if Engine<>TBESEN(Instance).DefaultRegExp then begin
   Engine.DecRef;
  end;
  Engine:=nil;
 end;
 inherited Destroy;
end;

procedure TBESENObjectRegExp.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
begin
 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  TBESEN(Instance).ObjectRegExpPrototype.NativeExec(ThisArgument,Arguments,CountArguments,AResult);
 end else begin
  raise EBESENTypeError.Create('Not callable');
 end;
end;

function TBESENObjectRegExp.HasCall:TBESENBoolean;
begin
 result:=(TBESEN(Instance).Compatibility and COMPAT_JS)<>0;
end;

procedure TBESENObjectRegExp.Finalize;
begin
 inherited Finalize;
end;

procedure TBESENObjectRegExp.Mark;
begin
 inherited Mark;
end;

procedure TBESENObjectRegExp.SetStatic(const Input:TBESENString;const Captures:TBESENRegExpCaptures);
var i:integer;
    v:TBESENValue;
    pn,lastParen:TBESENString;
begin
 if (TBESEN(Instance).Compatibility and COMPAT_JS)=0 then begin
  exit;
 end;                            
 v:=BESENEmptyValue;
 lastParen:='';
 for i:=0 to 9 do begin
  case i of
   0:pn:='$&';
   1:pn:='$1';
   2:pn:='$2';
   3:pn:='$3';
   4:pn:='$4';
   5:pn:='$5';
   6:pn:='$6';
   7:pn:='$7';
   8:pn:='$8';
   9:pn:='$9';
   else pn:='$0';
  end;
  if (i<length(Captures)) and (Captures[i].e<>brecUNDEFINED) then begin
   v:=BESENStringValue(copy(Input,Captures[i].s+1,Captures[i].e-Captures[i].s));
  end else begin
   v:=BESENStringValue('');
  end;
  if (i>0) and (i<length(Captures)) then begin
   lastParen:=v.Str;
  end;
  OverwriteData(pn,v,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]);
  if i=0 then begin
   OverwriteData('lastMatch',v,[bopaWRITABLE,bopaCONFIGURABLE]);
  end;
 end;

 v.ValueType:=bvtBOOLEAN;
 v.Bool:=brefMULTILINE in Engine.Flags;
 OverwriteData('$*',v,[bopaWRITABLE,bopaCONFIGURABLE]);
 OverwriteData('multiline',v,[]);

 v:=BESENStringValue(Input);
 OverwriteData('$_',v,[bopaWRITABLE,bopaCONFIGURABLE]);
 OverwriteData('input',v,[bopaWRITABLE,bopaCONFIGURABLE]);

 v:=BESENStringValue(lastParen);
 OverwriteData('$+',v,[bopaWRITABLE,bopaCONFIGURABLE]);
 OverwriteData('leftParen',v,[bopaWRITABLE,bopaCONFIGURABLE]);

 if (length(Captures)>0) and not (brefGLOBAL in Engine.Flags) then begin
  v:=BESENStringValue(copy(Input,1,Captures[0].s));
 end else begin
  v:=BESENStringValue('');
 end;
 OverwriteData('$`',v,[bopaWRITABLE,bopaCONFIGURABLE]);
 OverwriteData('leftContext',v,[bopaWRITABLE,bopaCONFIGURABLE]);

 if (length(Captures)>0) and not (brefGLOBAL in Engine.Flags) then begin
  v:=BESENStringValue(copy(Input,Captures[0].e,(length(Input)-longint(Captures[0].e))+1));
 end else begin
  v:=BESENStringValue('');
 end;
 OverwriteData('$''',v,[bopaWRITABLE,bopaCONFIGURABLE]);
 OverwriteData('rightContext',v,[bopaWRITABLE,bopaCONFIGURABLE]);

 v.ValueType:=bvtBOOLEAN;
 v.Bool:=brefGLOBAL in Engine.Flags;
 OverwriteData('global',v,[]);

 v.ValueType:=bvtBOOLEAN;
 v.Bool:=brefIGNORECASE in Engine.Flags;
 OverwriteData('ignoreCase',v,[]);

 if (length(Captures)>0) and not (brefGLOBAL in Engine.Flags) then begin
  v:=BESENNumberValue(Captures[0].e);
 end else begin
  v:=BESENNumberValue(0);
 end;
 OverwriteData('lastIndex',v,[bopaWRITABLE]);

 OverwriteData('source',BESENStringValue(Engine.Source),[]);
end;

end.
