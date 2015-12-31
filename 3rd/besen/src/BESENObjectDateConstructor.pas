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
unit BESENObjectDateConstructor;
{$i BESEN.inc}

interface

uses SysUtils,BESENConstants,BESENTypes,BESENObject,BESENObjectFunction,BESENValue,BESENObjectPropertyDescriptor;

type TBESENObjectDateConstructor=class(TBESENObjectFunction)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       procedure Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue); override;
       function HasConstruct:TBESENBoolean; override;
       function HasCall:TBESENBoolean; override;
       procedure NativeNow(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeParse(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeUTC(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
     end;
   
implementation

uses BESEN,BESENErrors,BESENASTNodes,BESENStringUtils,BESENUtils,BESENObjectDate,BESENDateUtils,BESENNumberUtils,BESENLocale;

constructor TBESENObjectDateConstructor.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Function';
 ObjectName:='Date';

 RegisterNativeFunction('now',NativeNow,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('parse',NativeParse,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('UTC',NativeUTC,7,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectDateConstructor.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectDateConstructor.Construct(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var r1:TBESENObjectDate;
    r3:TBESENValue;
    s:TBESENString;
    year,month,date,hours,minutes,seconds,ms:TBESENNumber;
begin
 r3:=BESENEmptyValue;
 r1:=TBESENObjectDate.Create(Instance,TBESEN(Instance).ObjectDatePrototype,false);
 TBESEN(Instance).GarbageCollector.Add(r1);
 r1.GarbageCollectorLock;
 try
  if CountArguments=0 then begin
   r1.Value:=BESENTimeClip(BESENDateTimeToBESENDate(BESENGetUTCDateTime));
  end else if CountArguments=1 then begin
   TBESEN(Instance).ToPrimitiveValue(Arguments^[0]^,TBESEN(Instance).ObjectNumberConstructorValue,r3);
   if r3.ValueType<>bvtSTRING then begin
    r1.Value:=BESENTimeClip(TBESEN(Instance).ToNum(r3));
   end else begin
    s:=TBESEN(Instance).ToStr(r3);
    r1.Value:=BESENParseTime(s);
    if BESENIsNaN(r1.Value) then begin
     r1.Value:=BESENParseISOTime(s);
     if BESENIsNaN(r1.Value) then begin
      r1.Value:=BESENParseNetscapeTime(s);
      if BESENIsNaN(r1.Value) then begin
       try
        r1.Value:=BESENTimeClip(BESENDateTimeToBESENDate(BESENLocalDateTimeToUTC(StrToDateTime(s{$if ((FPC_VERSION>=3) or ((FPC_VERSION>=2) and ((FPC_RELEASE>=5) or ((FPC_RELEASE>=4) and (FPC_PATCH>=1)))))},BESENLocaleFormatSettings{$ifend}))));
       except
        r1.Value:=double(pointer(@BESENDoubleNaN)^);
       end;
      end;
     end;
    end;
   end;
  end else if CountArguments>1 then begin
   year:=TBESEN(Instance).ToNum(Arguments^[0]^);
   month:=TBESEN(Instance).ToNum(Arguments^[1]^);
   date:=1;
   hours:=0;
   minutes:=0;
   seconds:=0;
   ms:=0;
   if CountArguments>2 then begin
    date:=TBESEN(Instance).ToNum(Arguments^[2]^);
    if CountArguments>3 then begin
     hours:=TBESEN(Instance).ToNum(Arguments^[3]^);
     if CountArguments>4 then begin
      minutes:=TBESEN(Instance).ToNum(Arguments^[4]^);
      if CountArguments>5 then begin
       seconds:=TBESEN(Instance).ToNum(Arguments^[5]^);
       if CountArguments>6 then begin
        ms:=TBESEN(Instance).ToNum(Arguments^[6]^);
       end;
      end;
     end;
    end;
   end;
   r1.Value:=BESENTimeClip(BESENMakeDate(BESENMakeDay(Year,Month,Date),BESENMakeTime(Hours,Minutes,Seconds,ms)));
  end;
 finally
  r1.GarbageCollectorUnlock;
 end;
 AResult:=BESENObjectValue(r1);
end;

procedure TBESENObjectDateConstructor.Call(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var AResult:TBESENValue);
var Value:TBESENDate;
begin
 Value:=BESENTimeClip(BESENGetUTCBESENDate);
 if BESENIsNaN(Value) then begin
  if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
   AResult:=BESENStringValue('Invalid Date');
  end else begin
   AResult:=BESENStringValue('NaN');
  end;
 end else begin
  AResult:=BESENStringValue(BESENFormatDateTime('ddd mmm dd yyyy hh:nn:ss',BESENUTCToLocalDateTime(BESENDateToDateTime(Value)),BESENDefaultFormatSettings)+' GMT'+BESENGetDateTimeOffsetString(BESENGetLocalDateTimeZone));
 end;
end;

procedure TBESENObjectDateConstructor.NativeNow(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 ResultValue:=BESENNumberValue(BESENGetUTCBESENDate);
end;

procedure TBESENObjectDateConstructor.NativeParse(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var v:TBESENNumber;
    s:TBESENString;
begin
 try
  if CountArguments>0 then begin
   s:=TBESEN(Instance).ToStr(Arguments^[0]^);
   v:=BESENParseTime(s);
   if BESENIsNaN(v) then begin
    v:=BESENParseISOTime(s);
    if BESENIsNaN(v) then begin
     v:=BESENParseNetscapeTime(s);
     if BESENIsNaN(v) then begin
      try
       v:=BESENTimeClip(BESENDateTimeToBESENDate(BESENLocalDateTimeToUTC(StrToDateTime(s{$if ((FPC_VERSION>=3) or ((FPC_VERSION>=2) and ((FPC_RELEASE>=5) or ((FPC_RELEASE>=4) and (FPC_PATCH>=1)))))},BESENLocaleFormatSettings{$ifend}))));
      except
       v:=double(pointer(@BESENDoubleNaN)^);
      end;
     end;
    end;
   end;
   ResultValue:=BESENNumberValue(v);
  end else begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end;
 finally
 end;
end;

procedure TBESENObjectDateConstructor.NativeUTC(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var year,month,date,hours,minutes,seconds,ms:TBESENNumber;
begin
 try
  if CountArguments>1 then begin
   year:=TBESEN(Instance).ToNum(Arguments^[0]^);
   month:=TBESEN(Instance).ToNum(Arguments^[1]^);
   date:=1;
   hours:=0;
   minutes:=0;
   seconds:=0;
   ms:=0;
   if CountArguments>2 then begin
    date:=TBESEN(Instance).ToNum(Arguments^[2]^);
    if CountArguments>3 then begin
     hours:=TBESEN(Instance).ToNum(Arguments^[3]^);
     if CountArguments>4 then begin
      minutes:=TBESEN(Instance).ToNum(Arguments^[4]^);
      if CountArguments>5 then begin
       seconds:=TBESEN(Instance).ToNum(Arguments^[5]^);
       if CountArguments>6 then begin
        ms:=TBESEN(Instance).ToNum(Arguments^[6]^);
       end;
      end;
     end;
    end;
   end;
   ResultValue:=BESENNumberValue(BESENTimeClip(BESENMakeDate(BESENMakeDay(Year,Month,Date),BESENMakeTime(Hours,Minutes,Seconds,ms))));
  end else begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end;
 finally
 end;
end;

function TBESENObjectDateConstructor.HasConstruct:TBESENBoolean;
begin
 result:=true;
end;

function TBESENObjectDateConstructor.HasCall:TBESENBoolean;
begin
 result:=true;
end;

end.
 