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
unit BESENObjectDatePrototype;
{$i BESEN.inc}

interface

uses Math,BESENConstants,BESENTypes,BESENObject,BESENObjectDate,BESENValue,BESENObjectPropertyDescriptor,
     BESENRegExp;

type TBESENObjectDatePrototype=class(TBESENObjectDate)
      public
       constructor Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false); overload; override;
       destructor Destroy; override;
       procedure NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToDateString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToTimeString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToLocaleDateString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToLocaleTimeString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToUTCString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetTime(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetDay(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCDay(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetUTCMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetTimezoneOffset(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetTime(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetUTCFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToGMTString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToISOString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeToJSON(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeGetYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
       procedure NativeSetYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
    end;

implementation

uses BESEN,BESENErrors,BESENNumberUtils,BESENArrayUtils,BESENDateUtils,BESENLocale;

constructor TBESENObjectDatePrototype.Create(AInstance:TObject;APrototype:TBESENObject=nil;AHasPrototypeProperty:longbool=false);
begin
 inherited Create(AInstance,APrototype,AHasPrototypeProperty);
 ObjectClassName:='Date';
 ObjectName:='Date';

 if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
  OverwriteData('name',BESENStringValue(ObjectName),[]);
 end;
 
 RegisterNativeFunction('toString',NativeToString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toDateString',NativeToDateString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toTimeString',NativeToTimeString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toLocaleString',NativeToLocaleString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toLocaleDateString',NativeToLocaleDateString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toLocaleTimeString',NativeToLocaleTimeString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toUTCString',NativeToUTCString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('valueOf',NativeValueOf,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getTime',NativeGetTime,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getFullYear',NativeGetFullYear,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCFullYear',NativeGetUTCFullYear,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getMonth',NativeGetMonth,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCMonth',NativeGetUTCMonth,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getDate',NativeGetDate,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCDate',NativeGetUTCDate,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getDay',NativeGetDay,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCDay',NativeGetUTCDay,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getHours',NativeGetHours,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCHours',NativeGetUTCHours,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getMinutes',NativeGetMinutes,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCMinutes',NativeGetUTCMinutes,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getSeconds',NativeGetSeconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCSeconds',NativeGetUTCSeconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getMilliseconds',NativeGetMilliseconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getUTCMilliseconds',NativeGetUTCMilliseconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getTimezoneOffset',NativeGetTimezoneOffset,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setTime',NativeSetTime,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setMilliseconds',NativeSetMilliseconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCMilliseconds',NativeSetUTCMilliseconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setSeconds',NativeSetSeconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCSeconds',NativeSetUTCSeconds,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setMinutes',NativeSetMinutes,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCMinutes',NativeSetUTCMinutes,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setHours',NativeSetHours,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCHours',NativeSetUTCHours,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setDate',NativeSetDate,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCDate',NativeSetUTCDate,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setMonth',NativeSetMonth,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCMonth',NativeSetUTCMonth,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setFullYear',NativeSetFullYear,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setUTCFullYear',NativeSetUTCFullYear,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toGMTString',NativeToGMTString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toISOString',NativeToISOString,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('toJSON',NativeToJSON,1,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('getYear',NativeGetYear,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
 RegisterNativeFunction('setYear',NativeSetYear,0,[bopaWRITABLE,bopaCONFIGURABLE],false);
end;

destructor TBESENObjectDatePrototype.Destroy;
begin
 inherited Destroy;
end;

procedure TBESENObjectDatePrototype.NativeToString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('ddd mmm dd yyyy hh:nn:ss',BESENUTCToLocalDateTime(BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)),BESENDefaultFormatSettings)+' GMT'+BESENGetDateTimeOffsetString(BESENGetLocalDateTimeZone));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToDateString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('ddd mmm dd yyy',BESENUTCToLocalDateTime(BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)),BESENDefaultFormatSettings));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToTimeString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('hh:nn:ss',BESENUTCToLocalDateTime(BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)),BESENDefaultFormatSettings));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToLocaleString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('dddddd tt',BESENUTCToLocalDateTime(BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)),BESENLocaleFormatSettings));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToLocaleDateString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('dddddd',BESENUTCToLocalDateTime(BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)),BESENLocaleFormatSettings));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToLocaleTimeString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('tt',BESENUTCToLocalDateTime(BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)),BESENLocaleFormatSettings));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToUTCString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('ddd, dd mmm yyyy hh:nn:ss',BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value),BESENDefaultFormatSettings)+' GMT');
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;

end;

procedure TBESENObjectDatePrototype.NativeValueOf(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetTime(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENYearFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENYearFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENMonthFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENMonthFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENDayFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENDayFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetDay(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENWeekDay(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCDay(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENWeekDay(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENHourFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENHourFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENMinuteFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENMinuteFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENSecondFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENSecondFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENMillisecondFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value)));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetUTCMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENMillisecondFromTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value));
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetTimezoneOffset(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue((TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value-BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value))/BESENmsPerMinute);
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetTime(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(TBESEN(Instance).ToNum(Arguments^[0]^));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   ms:=TBESEN(Instance).ToNum(Arguments^[0]^);
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENDay(t),BESENMakeTime(BESENHourFromTime(t),BESENMinuteFromTime(t),BESENSecondFromTime(t),ms))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCMilliseconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   ms:=TBESEN(Instance).ToNum(Arguments^[0]^);
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENDay(t),BESENMakeTime(BESENHourFromTime(t),BESENMinuteFromTime(t),BESENSecondFromTime(t),ms)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Seconds,ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Seconds:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    ms:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    ms:=BESENMillisecondFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENDay(t),BESENMakeTime(BESENHourFromTime(t),BESENMinuteFromTime(t),Seconds,ms))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCSeconds(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Seconds,ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   Seconds:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    ms:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    ms:=BESENMillisecondFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENDay(t),BESENMakeTime(BESENHourFromTime(t),BESENMinuteFromTime(t),Seconds,ms)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Minutes,Seconds,ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Minutes:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Seconds:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Seconds:=BESENSecondFromTime(t);
   end;
   if CountArguments>2 then begin
    ms:=TBESEN(Instance).ToNum(Arguments^[2]^);
   end else begin
    ms:=BESENMillisecondFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENDay(t),BESENMakeTime(BESENHourFromTime(t),Minutes,Seconds,ms))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCMinutes(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Minutes,Seconds,ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   Minutes:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Seconds:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Seconds:=BESENSecondFromTime(t);
   end;
   if CountArguments>2 then begin
    ms:=TBESEN(Instance).ToNum(Arguments^[2]^);
   end else begin
    ms:=BESENMillisecondFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENDay(t),BESENMakeTime(BESENHourFromTime(t),Minutes,Seconds,ms)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Hours,Minutes,Seconds,ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Hours:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Minutes:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Minutes:=BESENMinuteFromTime(t);
   end;
   if CountArguments>2 then begin
    Seconds:=TBESEN(Instance).ToNum(Arguments^[2]^);
   end else begin
    Seconds:=BESENSecondFromTime(t);
   end;
   if CountArguments>3 then begin
    ms:=TBESEN(Instance).ToNum(Arguments^[3]^);
   end else begin
    ms:=BESENMillisecondFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENDay(t),BESENMakeTime(Hours,Minutes,Seconds,ms))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCHours(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Hours,Minutes,Seconds,ms:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   Hours:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Minutes:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Minutes:=BESENMinuteFromTime(t);
   end;
   if CountArguments>2 then begin
    Seconds:=TBESEN(Instance).ToNum(Arguments^[2]^);
   end else begin
    Seconds:=BESENSecondFromTime(t);
   end;
   if CountArguments>3 then begin
    ms:=TBESEN(Instance).ToNum(Arguments^[3]^);
   end else begin
    ms:=BESENMillisecondFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENDay(t),BESENMakeTime(Hours,Minutes,Seconds,ms)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Date:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Date:=TBESEN(Instance).ToNum(Arguments^[0]^);
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENMakeDay(BESENYearFromTime(t),BESENMonthFromTime(t),Date),BESENTimeWithinDay(t))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCDate(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Date:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   Date:=TBESEN(Instance).ToNum(Arguments^[0]^);
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENMakeDay(BESENYearFromTime(t),BESENMonthFromTime(t),Date),BESENTimeWithinDay(t)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Month,Date:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Month:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Date:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Date:=BESENDayFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENMakeDay(BESENYearFromTime(t),Month,Date),BESENTimeWithinDay(t))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCMonth(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Month,Date:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   Month:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Date:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Date:=BESENDayFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENMakeDay(BESENYearFromTime(t),Month,Date),BESENTimeWithinDay(t)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Year,Month,Date:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Year:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Month:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Month:=BESENMonthFromTime(t);
   end;
   if CountArguments>2 then begin
    Date:=TBESEN(Instance).ToNum(Arguments^[2]^);
   end else begin
    Date:=BESENDayFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENMakeDay(Year,Month,Date),BESENTimeWithinDay(t))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetUTCFullYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Year,Month,Date:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value;
   Year:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if CountArguments>1 then begin
    Month:=TBESEN(Instance).ToNum(Arguments^[1]^);
   end else begin
    Month:=BESENMonthFromTime(t);
   end;
   if CountArguments>2 then begin
    Date:=TBESEN(Instance).ToNum(Arguments^[2]^);
   end else begin
    Date:=BESENDayFromTime(t);
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENMakeDate(BESENMakeDay(Year,Month,Date),BESENTimeWithinDay(t)));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToGMTString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 NativeToUTCString(ThisArgument,Arguments,CountArguments,ResultValue);
end;

procedure TBESENObjectDatePrototype.NativeToISOString(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   if (TBESEN(Instance).Compatibility and COMPAT_JS)<>0 then begin
    ResultValue:=BESENStringValue('Invalid Date');
   end else begin
    ResultValue:=BESENStringValue('NaN');
   end;
  end else begin
   ResultValue:=BESENStringValue(BESENFormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"',BESENDateToDateTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value),BESENDefaultFormatSettings));
  end;
 end else begin
  raise EBESENTypeError.Create('Not date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeToJSON(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var ov,tv:TBESENValue;
begin
 TBESEN(Instance).ToObjectValue(ThisArgument,ov);
 TBESEN(Instance).ToPrimitiveValue(ov,TBESEN(Instance).ObjectNumberConstructorValue,tv);
 if (tv.ValueType=bvtNUMBER) and not BESENIsFinite(tv.Num) then begin
  ResultValue:=BESENNullValue;
 end else begin
  TBESENObject(ov.Obj).GarbageCollectorLock;
  try
   TBESENObject(ov.Obj).Get('toISOString',tv);
   if not BESENIsCallable(tv) then begin
    raise EBESENTypeError.Create('no "toISOString" callable object');
   end;
   TBESEN(Instance).ObjectCall(TBESENObject(tv.Obj),ov,nil,0,ResultValue);
  finally
   TBESENObject(ov.Obj).GarbageCollectorUnlock;
  end;
 end;
end;

procedure TBESENObjectDatePrototype.NativeGetYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if BESENIsNaN(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value) then begin
   ResultValue:=BESENNumberValue(double(pointer(@BESENDoubleNaN)^));
  end else begin
   ResultValue:=BESENNumberValue(BESENYearFromTime(BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value))-1900);
  end;
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

procedure TBESENObjectDatePrototype.NativeSetYear(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:integer;var ResultValue:TBESENValue);
var t:TBESENDate;
    Year:TBESENNumber;
begin
 if not ((ThisArgument.ValueType=bvtOBJECT) and assigned(TBESENObject(ThisArgument.Obj))) then begin
  raise EBESENTypeError.Create('Null this object');
 end;
 if assigned(TBESENObject(ThisArgument.Obj)) and (TBESENObject(ThisArgument.Obj) is TBESENObjectDate) then begin
  if CountArguments=0 then begin
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=double(pointer(@BESENDoubleNaN)^);
  end else begin
   t:=BESENLocalTime(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
   Year:=TBESEN(Instance).ToNum(Arguments^[0]^);
   if (0<=Year) and (Year<=99) then begin
    Year:=Year+1900;
   end;
   TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value:=BESENTimeClip(BESENUTC(BESENMakeDate(BESENMakeDay(Year,BESENMonthFromTime(t),BESENDayFromTime(t)),BESENTimeWithinDay(t))));
  end;
  ResultValue:=BESENNumberValue(TBESENObjectDate(TBESENObject(ThisArgument.Obj)).Value);
 end else begin
  raise EBESENTypeError.Create('Not a date object');
 end;
end;

end.
