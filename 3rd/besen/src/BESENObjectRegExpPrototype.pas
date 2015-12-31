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
unit BESENObjectRegExpPrototype;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectRegExp,BESENValue,BESENObjectPropertyDescriptor,
     BESENRegExp;

type TBESENObjectRegExpPrototype=class(TBESENObjectRegExp)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeTest(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeExec(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;

implementation

uses BESEN,BESENErrors,BESENNumberUtils,BESENArrayUtils,BESENObjectArray;

constructor TBESENObjectRegExpPrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='RegExp';
 ObjectName:='RegExp';

 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;

 RegisterNativeFunction('toString',NativeToString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('test',NativeTest,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('exec',NativeExec,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectRegExpPrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectRegExpPrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var s:TBESENString;
    i:integer;
    c:widechar;
begin
 if ((TBESEN(Instance).Compatibility and COMPAT_JS)<>0) and (((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj)))and (TBESENObject(ThisArgument.Obj) is TBESENObjectRegExpPrototype)) then begin
  ResultValue:=BESENStringValue('RegExp.prototype');
 end else if ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) and (TBESENObject(ThisArgument.Obj) is TBESENObjectRegExp) then begin
  s:='/';
  i:=1;
  while i<=length(TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Source) do begin
   c:=TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Source[i];
   case c of
    '/':begin
     s:=s+'\/';
     inc(i);
    end;
    '\':begin
     s:=s+'\\';
     inc(i);
     if i<=length(TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Source) then begin
      c:=TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Source[i];
      s:=s+c;
      inc(i);
     end;
    end;
    else begin
     s:=s+c;
     inc(i);
    end;
   end;
  end;
  s:=s+'/';
  if brefGLOBAL in TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Flags then begin
   s:=s+'g';
  end;
  if brefIGNORECASE in TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Flags then begin
   s:=s+'i';
  end;
  if brefMULTILINE in TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Flags then begin
   s:=s+'m';
  end;
  ResultValue:=BESENStringValue(s);
 end else begin
  raise EBESENTypeError.Create('Not a RegExp object');
 end;
end;

procedure TBESENObjectRegExpPrototype.NativeTest(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v,vo,vs:TBESENValue;
    ValuePointers:array[0..0] of PBESENValue;
begin
 Get('exec',v);
 TBESEN(Instance).ToObjectValue(v,vo);
 if (vo.ValueType<>bvtOBJECT) or not (assigned(TBESENObject(vo.Obj)) and TBESENObject(vo.Obj).HasCall) then begin
  raise EBESENTypeError.Create('No callable');
 end;
 TBESENObject(vo.Obj).GarbageCollectorLock;
 try
  if CountArguments<1 then begin
   ValuePointers[0]:=@BESENUndefinedValue;
  end else begin
   ValuePointers[0]:=Arguments^[0];
  end;
  TBESEN(Instance).ObjectCall(TBESENObject(vo.Obj),ThisArgument,@ValuePointers,1,vs);
 finally
  TBESENObject(vo.Obj).GarbageCollectorUnlock;
 end;
 ResultValue.ValueType:=bvtBOOLEAN;
 ResultValue.Bool:=TBESEN(Instance).EqualityExpressionCompare(vs,BESENNullValue)<>0;
end;

procedure TBESENObjectRegExpPrototype.NativeExec(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v,vi:TBESENValue;
    i:integer;
    s:TBESENString;
    Captures:TBESENRegExpCaptures;
    o:TBESENObjectArray;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if not (TBESENObject(ThisArgument.Obj) is TBESENObjectRegExp) then begin
  raise EBESENTypeError.Create('Not a RegExp object');
 end;
 if CountArguments<1 then begin
  raise EBESENRangeError.Create('Bad argument count');
 end;

 s:=TBESEN(Instance).ToStr(Arguments^[0]^);

 i:=0;

 try
  TBESENObject(ThisArgument.Obj).Get('lastIndex',v);
  TBESEN(Instance).ToNumberValue(v,vi);
  if not (brefGLOBAL in TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Flags) then begin
   v:=BESENNumberValue(0);
  end;
  if (not BESENIsFinite(v.Num)) or (v.Num<0) or (v.Num>length(s)) then begin
   TBESENObject(ThisArgument.Obj).Put('lastIndex',BESENNumberValue(0),true);
   ResultValue:=BESENNullValue;
   exit;
  end;
  i:=trunc(vi.Num);
 except
 end;

{$ifdef UseAssert}
 Assert(TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.CountOfCaptures>0);
{$endif}
 Captures:=nil;
 try
  SetLength(Captures,TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.CountOfCaptures);
  while not TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Match(s,i,Captures) do begin
   inc(i);
   if i>length(s) then begin
    TBESENObject(ThisArgument.Obj).Put('lastIndex',BESENNumberValue(0),true);
    ResultValue:=BESENNullValue;
    for i:=0 to length(Captures)-1 do begin
     Captures[i].e:=brecUNDEFINED;
    end;
    TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).SetStatic(s,Captures);
    SetLength(Captures,0);
    exit;
   end;
  end;
  TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).SetStatic(s,Captures);
  if brefGLOBAL in TBESENObjectRegExp(TBESENObject(ThisArgument.Obj)).Engine.Flags then begin
   TBESENObject(ThisArgument.Obj).Put('lastIndex',BESENNumberValue(Captures[0].e),true);
  end;
  v:=BESENEmptyValue;
  o:=TBESENObjectArray.Create(Instance,TBESEN(Instance).ObjectArrayPrototype,false);
  TBESEN(Instance).GarbageCollector.Add(o);
  o.GarbageCollectorLock;
  try
   for i:=0 to length(Captures)-1 do begin
    if Captures[i].e=brecUNDEFINED then begin
     v:=BESENUndefinedValue;
    end else begin
     v:=BESENStringValue(copy(s,Captures[i].s+1,Captures[i].e-Captures[i].s));
    end;
    o.DefineOwnProperty(BESENArrayIndexToStr(i),BESENDataPropertyDescriptor(v,[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),true);
   end;
   o.Len:=length(Captures);
   o.DefineOwnProperty('index',BESENDataPropertyDescriptor(BESENNumberValue(Captures[0].s),[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),true);
   o.DefineOwnProperty('input',BESENDataPropertyDescriptor(BESENStringValue(s),[bopaWRITABLE,bopaENUMERABLE,bopaCONFIGURABLE]),true);
  finally
   o.GarbageCollectorUnlock;
  end;
  ResultValue:=BESENObjectValue(o);
 finally
  SetLength(Captures,0);
 end;
end;

end.
