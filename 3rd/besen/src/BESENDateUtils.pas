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
unit BESENDateUtils;
{$i BESEN.inc}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}{$ifdef unix}dl,BaseUnix,Unix,
     UnixType,{$endif}SysUtils,Classes,BESENConstants,BESENTypes;

const BESENUnixDateDelta=25569.0; //1970/01/01

      BESENmsPerDay=86400000.0;

      BESENmsPerY1=375*BESENmsPerDay;
      BESENmsPerY4=(4*BESENmsPerY1)+BESENmsPerDay;
      BESENmsPerY100=(25*BESENmsPerY4)-BESENmsPerDay;
      BESENmsPerY400=(4*BESENmsPerY100)+BESENmsPerDay;

      BESENTime1970=(4*BESENmsPerY400)+(3*BESENmsPerY100)+(17*BESENmsPerY4)+(2*BESENmsPerY1);

      BESENmaxTime=BESENmsPerDay*100000000;
      BESENminTime=-BESENmaxTime;

      BESENHoursPerDay=24.0;
      BESENMinutesPerHour=60.0;
      BESENSecondsPerMinute=60.0;
      BESENmsPerSecond=1000.0;
      BESENmsPerMinute=BESENmsPerSecond*BESENSecondsPerMinute;
      BESENmsPerHour=BESENmsPerMinute*BESENMinutesPerHour;

function BESENDay(t:TBESENNumber):TBESENNumber;

function BESENTimeWithinDay(t:TBESENNumber):TBESENNumber;

function BESENDaysInYear(y:TBESENNumber):TBESENNumber;
function BESENDayFromYear(y:TBESENNumber):TBESENNumber;
function BESENTimeFromYear(y:TBESENNumber):TBESENNumber;

function BESENYearFromTime(t:TBESENNumber):TBESENNumber;

function BESENDayWithinYear(t:TBESENNumber):TBESENNumber;

function BESENWeekDay(t:TBESENNumber):TBESENNumber;

function BESENIsLeapYear(t:TBESENNumber):boolean;

function BESENMonthFromTime(t:TBESENNumber):TBESENNumber;
function BESENDayFromTime(t:TBESENNumber):TBESENNumber;
function BESENHourFromTime(t:TBESENNumber):TBESENNumber;
function BESENMinuteFromTime(t:TBESENNumber):TBESENNumber;
function BESENSecondFromTime(t:TBESENNumber):TBESENNumber;
function BESENMillisecondFromTime(t:TBESENNumber):TBESENNumber;
function BESENToIntegerForTime(Num:TBESENNumber):int64;
function BESENToIntForTime(Num:TBESENNumber):TBESENNumber;

function BESENMakeTime(Hour,Minute,Second,Millisecond:TBESENNumber):TBESENNumber;
function BESENMakeDay(Year,Month,Date:TBESENNumber):TBESENNumber;

function BESENMakeDate(Day,Time:TBESENNumber):TBESENNumber;

function BESENTimeClip(const v:TBESENDate):TBESENDate;

function BESENDateTimeToBESENDate(DateTime:TDateTime):TBESENDate;
function BESENDateToDateTime(BESENDate:TBESENDate):TDateTime;
function BESENSystemTimeToBESENDate(SystemTime:TSystemTime):TBESENDate;
function BESENDateToSystemTime(BESENDate:TBESENDate):TSystemTime;

function BESENGetUTCDateTime:TDateTime;
function BESENGetUTCBESENDate:TBESENDate;
function BESENGetLocalDateTimeZone:TDateTime;
function BESENGetLocalTZA:TBESENDate;

function BESENLocalTime(Date:TBESENDate):TBESENDate;
function BESENUTC(Date:TBESENDate):TBESENDate;

function BESENLocalDateTimeToUTC(DateTime:TDateTime):TDateTime;
function BESENUTCToLocalDateTime(DateTime:TDateTime):TDateTime;

function BESENGetDateTimeOffsetString(const Ofs:TDateTime):TBESENString;

function BESENParseISOTime(s:TBESENString):TBESENDate;

function BESENParseTime(s:TBESENString):TBESENDate;

function BESENParseNetscapeTime(s:TBESENString):TBESENDate;

function BESENFormatDateTime(const Format:TBESENSTRING;DateTime:TDateTime;const FormatSettings:TFormatSettings):TBESENSTRING;

implementation

uses {$ifdef BESENDelphiHasNoSystemTimeMore}System.DateUtils,{$endif}BESENNumberUtils,BESENStringUtils;

const ParserMonthNames:array[0..11] of widestring=('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec');
      ParserDayNames:array[0..6] of widestring=('sun','mon','tue','wed','thu','fri','sat');

function BESENDay(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENFloor(t/BESENmsPerDay);
end;

function BESENTimeWithinDay(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENModuloPos(t,BESENmsPerDay);
end;

function BESENDaysInYear(y:TBESENNumber):TBESENNumber;
var yi:integer;
begin
 yi:=trunc(y);
 if (yi mod 4)<>0 then begin
  result:=365;
 end else if (yi mod 100)<>0 then begin
  result:=366;
 end else if (yi mod 400)<>0 then begin
  result:=365;
 end else begin
  result:=366;
 end;
end;

function BESENDayFromYear(y:TBESENNumber):TBESENNumber;
begin
 result:=(365.0*(y-1970))+(BESENFloor((y-1969)/4)-BESENFloor((y-1901)/100))+BESENFloor((y-1601)/400);
end;

function BESENTimeFromYear(y:TBESENNumber):TBESENNumber;
begin
 result:=BESENDayFromYear(y)*BESENmsPerDay;
end;

function BESENYearFromTime(t:TBESENNumber):TBESENNumber;
var y,t2:TBESENNumber;
begin
 y:=BESENFloor(t/(BESENmsPerDay*365.2425))+1970;
 t2:=BESENTimeFromYear(y);
 if t2>t then begin
  y:=y-1;
 end else begin
  if (t2+(BESENmsPerDay*BESENDaysInYear(y)))<=t then begin
   y:=y+1;
  end;
 end;
 result:=y;
end;

function BESENDayWithinYear(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENDay(t)-BESENDayFromYear(BESENYearFromTime(t));
end;

function BESENWeekDay(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENModuloPos(BESENDay(t)+4,7.0);
end;

function BESENIsLeapYear(t:TBESENNumber):boolean;
begin
 if BESENModuloPos(t,4)<>0 then begin
  result:=false;
 end else if BESENModuloPos(t,100)<>0 then begin
  result:=true;
 end else if BESENModuloPos(t,400)<>0 then begin
  result:=false;
 end else begin
  result:=true;
 end;
end;

function BESENMonthFromTime(t:TBESENNumber):TBESENNumber;
var dwy,ily:TBESENNumber;
begin
 dwy:=BESENDayWithinYear(t);
 if BESENIsLeapYear(BESENYearFromTime(t)) then begin
  ily:=1;
 end else begin
  ily:=0;
 end;
 if dwy<31 then begin
  result:=0;
 end else if dwy<(59+ily) then begin
  result:=1;
 end else if dwy<(90+ily) then begin
  result:=2;
 end else if dwy<(120+ily) then begin
  result:=3;
 end else if dwy<(151+ily) then begin
  result:=4;
 end else if dwy<(181+ily) then begin
  result:=5;
 end else if dwy<(212+ily) then begin
  result:=6;
 end else if dwy<(243+ily) then begin
  result:=7;
 end else if dwy<(273+ily) then begin
  result:=8;
 end else if dwy<(304+ily) then begin
  result:=9;
 end else if dwy<(334+ily) then begin
  result:=10;
 end else if dwy<(365+ily) then begin
  result:=11;
 end else begin
  result:=-1;
 end;
end;

function BESENDayFromTime(t:TBESENNumber):TBESENNumber;
var dwy,ily:TBESENNumber;
begin
 dwy:=BESENDayWithinYear(t);
 if BESENIsLeapYear(BESENYearFromTime(t)) then begin
  ily:=1;
 end else begin
  ily:=0;
 end;
 case trunc(BESENMonthFromTime(t)) of
  0:begin
   result:=dwy+1;
  end;
  1:begin
   result:=dwy-30;
  end;
  2:begin
   result:=dwy-(58+ily);
  end;
  3:begin
   result:=dwy-(89+ily);
  end;
  4:begin
   result:=dwy-(119+ily);
  end;
  5:begin
   result:=dwy-(150+ily);
  end;
  6:begin
   result:=dwy-(180+ily);
  end;
  7:begin
   result:=dwy-(211+ily);
  end;
  8:begin
   result:=dwy-(242+ily);
  end;
  9:begin
   result:=dwy-(272+ily);
  end;
  10:begin
   result:=dwy-(303+ily);
  end;
  11:begin
   result:=dwy-(333+ily);
  end;
  else begin
   result:=-1;
  end;
 end;
end;

function BESENHourFromTime(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENModuloPos(BESENFloor(t/BESENmsPerHour),BESENHoursPerDay);
end;

function BESENMinuteFromTime(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENModuloPos(BESENFloor(t/BESENmsPerMinute),BESENMinutesPerHour);
end;

function BESENSecondFromTime(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENModuloPos(BESENFloor(t/BESENmsPerSecond),BESENSecondsPerMinute);
end;

function BESENMillisecondFromTime(t:TBESENNumber):TBESENNumber;
begin
 result:=BESENFloor(BESENModuloPos(t,BESENmsPerSecond));
end;

function BESENToIntegerForTime(Num:TBESENNumber):int64;
begin
 if BESENIsNaN(Num) then begin
  result:=0;
 end else if BESENIsFinite(Num) then begin
  if BESENIsNegative(Num) then begin
   result:=trunc(-BESENFloor(-Num));
  end else begin
   result:=trunc(BESENFloor(Num));
  end;
 end else begin
  result:=trunc(Num);
 end;
end;

function BESENToIntForTime(Num:TBESENNumber):TBESENNumber;
begin
 if BESENIsNaN(Num) then begin
  result:=0;
 end else if BESENIsFinite(Num) then begin
  if BESENIsNegative(Num) then begin
   result:=-BESENFloor(-Num);
  end else begin
   result:=BESENFloor(Num);
  end;
 end else begin
  result:=Num;
 end;
end;

function BESENMakeTime(Hour,Minute,Second,Millisecond:TBESENNumber):TBESENNumber;
begin
 if BESENIsFinite(Hour) and BESENIsFinite(Minute) and BESENIsFinite(Second) and BESENIsFinite(Millisecond) then begin
  result:=(BESENToIntForTime(Hour)*BESENmsPerHour)+(BESENToIntForTime(Minute)*BESENmsPerMinute)+(BESENToIntForTime(Second)*BESENmsPerSecond)+BESENToIntForTime(Millisecond);
 end else begin
  result:=double(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENMakeDay(Year,Month,Date:TBESENNumber):TBESENNumber;
const Julian:array[boolean,0..11] of integer=((1,32,60,91,121,152,182,213,244,274,305,335),(1,32,61,92,122,153,183,214,245,275,306,336));
var y,m:TBESENNumber;
begin
 if not (BESENIsFinite(Year) and BESENIsFinite(Month) and BESENIsFinite(Date)) then begin
  result:=double(pointer(@BESENDoubleNaN)^);
  exit;
 end;
 month:=BESENToIntForTime(month);
 y:=BESENToIntForTime(year)+BESENFloor(month/12);
 m:=BESENModuloPos(month,12.0);
 if (BESENDayFromYear(y)<-100000000) or (BESENDayFromYear(y)>100000000) then begin
  result:=double(pointer(@BESENDoubleNaN)^);
  exit;
 end;
 result:=BESENDay((BESENDayFromYear(y)+Julian[boolean(BESENIsLeapYear(BESENFloor(y)))][trunc(m)]-1)*BESENmsPerDay)+BESENToIntForTime(date)-1;
 if (result<-100000000) or (result>100000000) then begin 
  result:=double(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENMakeDate(Day,Time:TBESENNumber):TBESENNumber;
begin
 if BESENIsFinite(Day) and BESENIsFinite(Time) then begin
  result:=(Day*BESENmsPerDay)+Time;
 end else begin
  result:=double(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENTimeClip(const v:TBESENDate):TBESENDate;
begin
 if (not BESENIsFinite(v)) or ((v<-8.64e15) or (v>8.64e15)) then begin
  result:=double(pointer(@BESENDoubleNaN)^);
 end else begin
  result:=BESENToIntForTime(v);
 end;
end;

function BESENDateTimeToBESENDate(DateTime:TDateTime):TBESENDate;
begin
 result:=round((DateTime-BESENUnixDateDelta)*BESENmsPerDay);
end;

function BESENDateToDateTime(BESENDate:TBESENDate):TDateTime;
begin
 result:=(BESENDate/BESENmsPerDay)+BESENUnixDateDelta;
end;

function BESENSystemTimeToBESENDate(SystemTime:TSystemTime):TBESENDate;
begin
{$ifdef BESENDelphiHasNoSystemTimeMore}
 result:=EncodeDate(SystemTime.wYear,SystemTime.wMonth,SystemTime.wDay);
 result:=result+(((ord(result<0) and 1)*(-1))*EncodeTime(SystemTime.wHour,SystemTime.wMinute,SystemTime.wSecond,SystemTime.wMilliSeconds));
{$else}
 result:=BESENDateTimeToBESENDate(SystemTimeToDateTime(SystemTime));
{$endif}
end;

function BESENDateToSystemTime(BESENDate:TBESENDate):TSystemTime;
{$ifdef BESENDelphiHasNoSystemTimeMore}
var DateTime:TDateTime;
{$endif}
begin
{$ifdef BESENDelphiHasNoSystemTimeMore}
 DateTime:=BESENDateToDateTime(trunc(BESENDate));
 DecodeDateFully(DateTime,result.wYear,result.wMonth,result.wDay,result.wDayOfWeek);
 dec(result.wDayOfWeek);
 DecodeTime(DateTime,result.wHour,result.wMinute,result.wSecond,result.wMilliseconds);
{$else}
 DateTimeToSystemTime(BESENDateToDateTime(trunc(BESENDate)),result);
{$endif}
end;

function BESENGetLocalDateTime:TDateTime;
{$ifndef BESENDelphiHasNoSystemTimeMore}
var SystemTime:TSystemTime;
{$endif}
begin
{$ifdef BESENDelphiHasNoSystemTimeMore}
 result:=Now;
{$else}
 GetLocalTime(SystemTime);
 result:=SystemTimeToDateTime(SystemTime);
{$endif}
end;

function BESENGetUTCDateTime:TDateTime;
{$ifdef unix}
var TimeVal:TTimeVal;
    ia,ib:int64;
    fa,fb,fc:double;
begin
 fpGetTimeOfDay(@TimeVal,nil);
 ia:=TimeVal.tv_sec;
 ib:=TimeVal.tv_usec;
 fa:=ia*1000;
 fb:=ib*0.001;
 fc:=fa+fb;
 result:=(fc/BESENmsPerDay)+BESENUnixDateDelta;
end;
{$else}
{$ifdef BESENEmbarcaderoNextGen}
begin
 result:=TTimeZone.Local.ToUniversalTime(Now);
end;
{$else}
{$ifdef windows}
var SystemTime:TSystemTime;
begin
 GetSystemTime(SystemTime);
 result:=SystemTimeToDateTime(SystemTime);
end;
{$else}
var SystemTime:TSystemTime;
begin
 GetSystemTime(SystemTime);
 result:=SystemTimeToDateTime(SystemTime);
end;
{$endif}
{$endif}
{$endif}

function BESENGetUTCBESENDate:TBESENDate;
{$ifdef unix}
var TimeVal:TTimeVal;
    ia,ib:int64;
    fa,fb:double;
begin
 fpGetTimeOfDay(@TimeVal,nil);
 ia:=TimeVal.tv_sec;
 ib:=TimeVal.tv_usec;
 fa:=ia*1000;
 fb:=ib*0.001;
 result:=fa+fb;
end;
{$else}
{$ifdef BESENDelphiHasNoSystemTimeMore}
begin
 result:=BESENDateTimeToBESENDate(Now);
end;
{$else}
{$ifdef windows}
var SystemTime:TSystemTime;
begin
 GetSystemTime(SystemTime);
 result:=BESENDateTimeToBESENDate(SystemTimeToDateTime(SystemTime));
end;
{$else}
var SystemTime:TSystemTime;
begin
 GetSystemTime(SystemTime);
 result:=BESENDateTimeToBESENDate(SystemTimeToDateTime(SystemTime));
end;
{$endif}
{$endif}
{$endif}

function BESENGetLocalDateTimeZone:TDateTime;
{$ifdef unix}
begin
 result:=BESENGetLocalDateTime-BESENGetUTCDateTime;
end;
{$else}
{$ifdef windows}
var tz_info:TIME_ZONE_INFORMATION;
begin
 case GetTimeZoneInformation(tz_info) of
  TIME_ZONE_ID_STANDARD:result:=-(((tz_info.StandardBias+tz_info.Bias)*60000.0)/BESENmsPerDay);
  TIME_ZONE_ID_DAYLIGHT:result:=-(((tz_info.DaylightBias+tz_info.Bias)*60000.0)/BESENmsPerDay);
  else begin
   result:=BESENGetLocalDateTime-BESENGetUTCDateTime;
  end;
 end;
end;
{$else}
begin
 result:=BESENGetLocalDateTime-BESENGetUTCDateTime;
end;
{$endif}
{$endif}

function BESENGetLocalTZA:TBESENDate;
{$ifdef unix}
var TimeVal:TTimeVal;
    ia,ib:int64;
    fa,fb,fc:double;
begin
 fpGetTimeOfDay(@TimeVal,nil);
 ia:=TimeVal.tv_sec;
 ib:=TimeVal.tv_usec;
 fa:=ia*1000;
 fb:=ib*0.001;
 fc:=fa+fb;
 result:=BESENDateTimeToBESENDate(BESENGetLocalDateTime)-fc;
end;
{$else}
{$ifdef windows}
var tz_info:TIME_ZONE_INFORMATION;
begin
 case GetTimeZoneInformation(tz_info) of
  TIME_ZONE_ID_STANDARD:result:=-(tz_info.StandardBias+tz_info.Bias)*60000.0;
  TIME_ZONE_ID_DAYLIGHT:result:=-(tz_info.DaylightBias+tz_info.Bias)*60000.0;
  else begin
   result:=(BESENGetLocalDateTime-BESENGetUTCDateTime)*BESENmsPerDay;
  end;
 end;
end;
{$else}
begin
 result:=(BESENGetLocalDateTime-BESENGetUTCDateTime)*BESENmsPerDay;
end;
{$endif}
{$endif}

function BESENLocalTime(Date:TBESENDate):TBESENDate;
begin
 if BESENIsFinite(Date) then begin
  result:=Date+BESENGetLocalTZA;
 end else begin
  result:=double(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENUTC(Date:TBESENDate):TBESENDate;
begin
 if BESENIsFinite(Date) then begin
  result:=Date-BESENGetLocalTZA;
 end else begin
  result:=double(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENLocalDateTimeToUTC(DateTime:TDateTime):TDateTime;
begin
 result:=DateTime-BESENGetLocalDateTimeZone;
end;

function BESENUTCToLocalDateTime(DateTime:TDateTime):TDateTime;
begin
 result:=DateTime+BESENGetLocalDateTimeZone;
end;

function BESENGetDateTimeOffsetString(const Ofs:TDateTime):TBESENString;
var i:int64;
    a,b:TBESENString;
begin
 i:=trunc(BESENFloor(Ofs*SecsPerDay)) div 60;
 case i mod 60 of
  59:begin
   inc(i);
  end;
  1:begin
   dec(i);
  end;
 end;
 a:=IntToStr(i div 60);
 b:=IntToStr(i mod 60);
 if length(a)<2 then begin
  a:='0'+a;
 end;
 if length(b)<2 then begin
  b:='0'+b;
 end;
 if i<0 then begin
  a:='-'+a;
 end else begin
  a:='+'+a;
 end;
 result:=a+b;
end;

function BESENParseISOTime(s:TBESENString):TBESENDate;
 function IsDigit(const w:widechar):boolean;
 begin
  result:=(word(w)>=ord('0')) and (word(w)<=ord('9'));
 end;
 function ParseDay(const ss:TBESENString):TBESENDate;
 var Offset:integer;
 begin
  if (length(ss)=10) and (IsDigit(ss[1]) and IsDigit(ss[2]) and IsDigit(ss[3]) and IsDigit(ss[4]) and (ss[5]='-') and IsDigit(ss[6]) and IsDigit(ss[7]) and (ss[8]='-') and IsDigit(ss[9]) and IsDigit(ss[10])) then begin
   Offset:=1;
  end else if (length(ss)=8) and (IsDigit(ss[1]) and IsDigit(ss[2]) and IsDigit(ss[3]) and IsDigit(ss[4]) and IsDigit(ss[5]) and IsDigit(ss[6]) and IsDigit(ss[7]) and IsDigit(ss[8])) then begin
   Offset:=0;
  end else begin
   result:=double(pointer(@BESENDoubleNaN)^);
   exit;
  end;
  result:=BESENMakeDay(StrToIntDef(copy(ss,1,4),0),StrToIntDef(copy(ss,5+(1*Offset),2),0),StrToIntDef(copy(ss,7+(2*Offset),2),0));
 end;
 function ParseTime(const ss:TBESENString):TBESENDate;
 var s,ms:TBESENString;
     i,Offset,Hours,Minutes,Seconds,Milliseconds:integer;
     WithSeconds:boolean;
 begin
  i:=BESENPosChar(',',ss);
  if i=0 then begin
   i:=BESENPosChar('.',ss);
  end;
  if i=0 then begin
   s:=ss;
   ms:='';
  end else begin
   s:=copy(ss,1,i-1);
   ms:=copy(ms,i+1,(length(ss)-i)+1);
  end;
  if (length(s)=8) and (IsDigit(s[1]) and IsDigit(s[2]) and (s[3]=':') and IsDigit(s[4]) and IsDigit(s[5]) and (s[6]=':') and IsDigit(s[7]) and IsDigit(s[8])) then begin
   Offset:=1;
   WithSeconds:=true;
  end else if (length(s)=6) and (IsDigit(s[1]) and IsDigit(s[2]) and IsDigit(s[3]) and IsDigit(s[4]) and IsDigit(s[5]) and IsDigit(s[6])) then begin
   Offset:=0;
   WithSeconds:=true;
  end else if (length(s)=5) and (IsDigit(s[1]) and IsDigit(s[2]) and (s[3]=':') and IsDigit(s[4]) and IsDigit(s[5])) then begin
   Offset:=1;
   WithSeconds:=false;
  end else if (length(s)=4) and (IsDigit(s[1]) and IsDigit(s[2]) and IsDigit(s[3]) and IsDigit(s[4])) then begin
   Offset:=0;
   WithSeconds:=false;
  end else begin
   result:=double(pointer(@BESENDoubleNaN)^);
   exit;
  end;
  Hours:=StrToIntDef(copy(s,1,2),100);
  Minutes:=StrToIntDef(copy(s,3+(1*Offset),2),100);
  if WithSeconds then begin
   Seconds:=StrToIntDef(copy(s,5+(2*Offset),2),100);
  end else begin
   Seconds:=0;
   if length(ms)>0 then begin
    result:=double(pointer(@BESENDoubleNaN)^);
    exit;
   end;
  end;
  case length(ms) of
   0:Milliseconds:=0;
   3:Milliseconds:=StrToIntDef(ms,10000);
   else begin
    result:=double(pointer(@BESENDoubleNaN)^);
    exit;
   end;
  end;
  result:=BESENMakeTime(Hours,Minutes,Seconds,Milliseconds);
 end;
var i,j,k:integer;
    sz:TBESENString;
    Delta:TBESENDate;
begin
 i:=BESENPosChar('T',s);
 if i=0 then begin
  i:=BESENPosChar(' ',s);
 end;
 if i>0 then begin
  j:=BESENPosChar('Z',s);
  if j=0 then begin
   j:=length(s)+1;
   for k:=length(s) downto i+1 do begin
    if (s[k]='-') or (s[k]='+') then begin
     j:=k;
     break;
    end;
   end;
  end;
  sz:=copy(s,j,(length(s)-j)+1);
  s:=copy(s,1,j-1);
  Delta:=0;
  if (length(sz)=6) and ((sz[1]='-') or (sz[1]='+')) and IsDigit(sz[2]) and IsDigit(sz[3]) and (sz[4]=':') and IsDigit(sz[5]) and IsDigit(sz[6]) then begin
   i:=StrToIntDef(copy(sz,2,2),-1);
   j:=StrToIntDef(copy(sz,5,2),-1);
   if ((i>=0) and (i<=24)) and ((j>=0) and (j<=60)) then begin
    Delta:=(i*60)+j;
    if sz[1]='-' then begin
     Delta:=-Delta;
    end;
   end else begin
    result:=double(pointer(@BESENDoubleNaN)^);
    exit;
   end;
  end else if (length(sz)>0) and (sz<>'Z') then begin
   result:=double(pointer(@BESENDoubleNaN)^);
   exit;
  end;
  result:=BESENTimeClip(BESENMakeDate(ParseDay(copy(s,1,i-1)),ParseTime(copy(s,i+1,(length(s)-i)+1)))-(Delta*BESENmsPerMinute));
 end else begin
  result:=double(pointer(@BESENDoubleNaN)^);
 end;
end;

function BESENParseTime(s:TBESENString):TBESENDate;
var p:integer;
 function IsWhite(const w:widechar):boolean;
 begin
  result:=BESENUnicodeIsStringWhiteSpace(word(w));
 end;
 function IsLetter(const w:widechar):boolean;
 begin
  result:=((word(w)>=ord('a')) and (word(w)<=ord('z'))) or ((word(w)>=ord('Z')) and (word(w)<=ord('Z')));
 end;
 function IsDigit(const w:widechar):boolean;
 begin
  result:=(word(w)>=ord('0')) and (word(w)<=ord('9'));
 end;
 function ToLower(const w:widechar):widechar;
 begin
  result:=widechar(word(BESENUnicodeToLower(word(w))));
 end;
 procedure SkipWhite;
 begin
  while p<length(s) do begin
   if IsWhite(s[p]) then begin
    inc(p);
   end else begin
    break;
   end;
  end;
 end;
var i,d,m,y,hr,min,sec,ms,wd:integer;
    t:TBESENString;
    yneg:boolean;
begin
 result:=double(pointer(@BESENDoubleNaN)^);

 p:=1;

 SkipWhite;

 if ((p+2)<=length(s)) and (IsLetter(s[p]) and IsLetter(s[p+1]) and IsLetter(s[p+2])) then begin
  t:=BESENLowercase(copy(s,p,3));
  for wd:=low(ParserDayNames) to high(ParserDayNames) do begin
   if t=ParserDayNames[wd] then begin
    inc(p,3);
    if (p<=length(s)) and (s[p]=',') then begin
     inc(p);
    end;
    SkipWhite;
    break;
   end;
  end;
 end;

 if (p<=length(s)) and IsDigit(s[p]) then begin
  d:=0;
  while (p<=length(s)) and IsDigit(s[p]) do begin
   d:=(d*10)+(word(widechar(s[p]))-ord('0'));
   inc(p);
  end;
  if not ((p<=length(s)) and IsWhite(s[p])) then begin
   exit;
  end;
  SkipWhite;
  if not (((p+2)<=length(s)) and (IsLetter(s[p]) and IsLetter(s[p+1]) and IsLetter(s[p+2]))) then begin
   exit;
  end;
  t:=BESENLowercase(copy(s,p,3));
  inc(p,3);
  m:=0;
  for i:=low(ParserMonthNames) to high(ParserMonthNames) do begin
   if t=ParserMonthNames[i] then begin
    m:=i;
    break;
   end;
  end;
 end else begin
  if not (((p+2)<=length(s)) and (IsLetter(s[p]) and IsLetter(s[p+1]) and IsLetter(s[p+2]))) then begin
   exit;
  end;
  t:=BESENLowercase(copy(s,p,3));
  inc(p,3);
  m:=0;
  for i:=low(ParserMonthNames) to high(ParserMonthNames) do begin
   if t=ParserMonthNames[i] then begin
    m:=i;
    break;
   end;
  end;
  if not ((p<=length(s)) and IsWhite(s[p])) then begin
   exit;
  end;
  SkipWhite;
  d:=0;
  while (p<=length(s)) and IsDigit(s[p]) do begin
   d:=(d*10)+(word(widechar(s[p]))-ord('0'));
   inc(p);
  end;
 end;
 if (m<0) or ((d<1) or (d>31)) then begin
  exit;
 end;

 if not ((p<=length(s)) and IsWhite(s[p])) then begin
  exit;
 end;
 SkipWhite;

 yneg:=(p<=length(s)) and (s[p]='-');
 if yneg then begin
  inc(p);
 end;
 if not ((p<=length(s)) and IsDigit(s[p])) then begin
  exit;
 end;
 y:=0;
 while (p<=length(s)) and IsDigit(s[p]) do begin
  y:=(y*10)+(word(widechar(s[p]))-ord('0'));
  inc(p);
 end;
 if yneg then begin
  y:=-y;
 end;

 hr:=0;
 min:=0;
 sec:=0;
 ms:=0;
 if (p<=length(s)) and IsWhite(s[p]) then begin
  SkipWhite;
  if ((p+4)<=length(s)) and (IsDigit(s[p]) and IsDigit(s[p+1]) and (s[p+2]=':') and IsDigit(s[p+3]) and IsDigit(s[p+4])) then begin
   hr:=((word(widechar(s[p]))-ord('0'))*10)+(word(widechar(s[p+1]))-ord('0'));
   min:=((word(widechar(s[p+3]))-ord('0'))*10)+(word(widechar(s[p+4]))-ord('0'));
   inc(p,5);
   if ((p+2)<=length(s)) and ((s[p]=':') and IsDigit(s[p+1]) and IsDigit(s[p+2])) then begin
    sec:=((word(widechar(s[p+1]))-ord('0'))*10)+(word(widechar(s[p+2]))-ord('0'));
    inc(p,3);
   end;
   if ((p+3)<=length(s)) and (((s[p]=':') or (s[p]='.')) and IsDigit(s[p+1]) and IsDigit(s[p+2]) and IsDigit(s[p+3])) then begin
    ms:=((word(widechar(s[p+1]))-ord('0'))*100)+((word(widechar(s[p+2]))-ord('0'))*10)+(word(widechar(s[p+3]))-ord('0'));
    inc(p,4);
   end;
  end;
 end;
 if ((hr<0) or (hr>=24)) or ((min<0) or (min>=60)) or ((sec<0) or (sec>=60)) or ((ms<0) or (ms>=1000)) then begin
  exit;
 end;

 result:=BESENMakeDate(BESENMakeDay(y,m,d),BESENMakeTime(hr,min,sec,ms));

 SkipWhite;
 if ((p+2)<=length(s)) and (((s[p]='G') and (s[p+1]='M') and (s[p+2]='T')) or ((s[p]='U') and (s[p+1]='T') and (s[p+2]='C'))) then begin
  inc(p,3);
  if ((p+4)<=length(s)) and (((s[p]='-') or (s[p]='+')) and IsDigit(s[p+1]) and IsDigit(s[p+2]) and IsDigit(s[p+3]) and IsDigit(s[p+4])) then begin
   i:=((((word(widechar(s[p+1]))-ord('0'))*10)+((word(widechar(s[p+2]))-ord('0'))))*60)+(((word(widechar(s[p+3]))-ord('0'))*10)+(word(widechar(s[p+4]))-ord('0')));
   if s[p]='-' then begin
    i:=-i;
   end;
   result:=result-(BESENmsPerMinute*i);
  end;
 end else begin
  result:=result-(BESENGetLocalDateTimeZone*BESENmsPerDay);
 end;

 result:=BESENTimeClip(result);
end;

function BESENParseNetscapeTime(s:TBESENString):TBESENDate;
var p:integer;
 function IsWhite(const w:widechar):boolean;
 begin
  result:=BESENUnicodeIsStringWhiteSpace(word(w));
 end;
 function IsLetter(const w:widechar):boolean;
 begin
  result:=((word(w)>=ord('a')) and (word(w)<=ord('z'))) or ((word(w)>=ord('Z')) and (word(w)<=ord('Z')));
 end;
 function IsDigit(const w:widechar):boolean;
 begin
  result:=(word(w)>=ord('0')) and (word(w)<=ord('9'));
 end;
 function ToLower(const w:widechar):widechar;
 begin
  result:=widechar(word(BESENUnicodeToLower(word(w))));
 end;
 procedure SkipWhite;
 begin
  while p<length(s) do begin
   if IsWhite(s[p]) then begin
    inc(p);
   end else begin
    break;
   end;
  end;
 end;
var i,j,d,m,y,hr,min,sec,ms:integer;
    neg:boolean;
    n:array[0..2] of integer;
begin
 result:=double(pointer(@BESENDoubleNaN)^);

 p:=1;

 SkipWhite;

 for j:=0 to 2 do begin
  if j<>0 then begin
   SkipWhite;
   if (p<=length(s)) and (s[p]='/') then begin
    inc(p);
   end else begin
    exit;
   end;
   SkipWhite;
  end;
  n[j]:=0;
  neg:=(p<=length(s)) and (s[p]='-');
  if neg then begin
   inc(p);
  end;
  if (p<=length(s)) and IsDigit(s[p]) then begin
   i:=0;
   while (p<=length(s)) and IsDigit(s[p]) do begin
    i:=(i*10)+(word(widechar(s[p]))-ord('0'));
    inc(p);
   end;
   if neg then begin
    i:=-i;
   end;
   n[j]:=i;
  end else begin
   exit;
  end;
 end;
 if (n[0]>=70) and (n[1]>=70) then begin
  exit;
 end;
 if n[0]>=70 then begin
  y:=n[0]+1900;
  m:=n[1];
  d:=n[2];
 end else begin
  m:=n[0];
  d:=n[1];
  y:=n[2];
  if y<100 then begin
   y:=n[0]+1900;
  end;
 end;
 if (m<0) or ((d<1) or (d>31)) then begin
  exit;
 end;

 hr:=0;
 min:=0;
 sec:=0;
 ms:=0;

 if not ((p<=length(s)) and IsWhite(s[p])) then begin
  SkipWhite;

  if (p<=length(s)) and IsDigit(s[p]) then begin
   i:=0;
   while (p<=length(s)) and IsDigit(s[p]) do begin
    i:=(i*10)+(word(widechar(s[p]))-ord('0'));
    inc(p);
   end;
   hr:=i;
   SkipWhite;
   if (p<=length(s)) and (s[p]=':') then begin
    inc(p);
    SkipWhite;

    if (p<=length(s)) and IsDigit(s[p]) then begin
     i:=0;
     while (p<=length(s)) and IsDigit(s[p]) do begin
      i:=(i*10)+(word(widechar(s[p]))-ord('0'));
      inc(p);
     end;
     min:=i;
     SkipWhite;
     if (p<=length(s)) and (s[p]=':') then begin
      inc(p);
      SkipWhite;

      if (p<=length(s)) and IsDigit(s[p]) then begin
       i:=0;
       while (p<=length(s)) and IsDigit(s[p]) do begin
        i:=(i*10)+(word(widechar(s[p]))-ord('0'));
        inc(p);
       end;
       sec:=i;
       SkipWhite;
       if (p<=length(s)) and ((s[p]=':') or (s[p]='.')) then begin
        inc(p);
        SkipWhite;
        if (p<=length(s)) and IsDigit(s[p]) then begin
         i:=0;
         while (p<=length(s)) and IsDigit(s[p]) do begin
          i:=(i*10)+(word(widechar(s[p]))-ord('0'));
          inc(p);
         end;
         ms:=i;
         SkipWhite;
        end;
       end;
      end else begin
       exit;
      end;

     end;

     if ((p+1)<=length(s)) and (ToLower(s[p+1])='m') then begin
      if ToLower(s[p])='a' then begin
       if (hr<1) or (hr>12) then begin
        exit;
       end;
       hr:=(hr mod 12)+12;
      end else if ToLower(s[p])='p' then begin
       if (hr<1) or (hr>12) then begin
        exit;
       end;
       hr:=hr mod 12;
      end else begin
       exit;
      end;
      inc(p,2);
      SkipWhite;
     end;

    end else begin
     exit;
    end;

   end;
  end;

 end;

 if ((hr<0) or (hr>=24)) or ((min<0) or (min>=60)) or ((sec<0) or (sec>=60)) or ((ms<0) or (ms>=1000)) then begin
  exit;
 end;

 result:=BESENTimeClip(BESENMakeDate(BESENMakeDay(y,m,d),BESENMakeTime(hr,min,sec,ms))-(BESENGetLocalDateTimeZone*BESENmsPerDay));
end;

{$warnings off}
function BESENFormatDateTime(const Format:TBESENSTRING;DateTime:TDateTime;const FormatSettings:TFormatSettings):TBESENSTRING;
var Year,Month,Day,DayOfWeek,Hour,Minute,Second,MilliSecond:word;
    ResultString:TBESENSTRING;
 procedure Add(const s:TBESENSTRING);
 begin
  ResultString:=ResultString+s;
 end;
 procedure AddInt(Value,Digits:integer);
 var s:TBESENSTRING;
 begin
  s:=inttostr(Value);
  while length(s)<Digits do begin
   s:='0'+s;
  end;
  Add(s);
 end;
 procedure Process(const Format:TBESENSTRING;Nesting:integer;TimeFlag:boolean);
 var FormatPos,PosA,PosB,Count,t:integer;
     FormatUp:TBESENSTRING;
     CurrentChar,LastChar:TBESENWIDECHAR;
     Clock12:boolean;
 begin
  if Nesting>1 then begin
   exit;
  end;
  Clock12:=false;
  FormatUp:=uppercase(Format);
  FormatPos:=1;
  while FormatPos<=length(Format) do begin
   CurrentChar:=FormatUp[FormatPos];
   case CurrentChar of
    '''','"':begin
     inc(FormatPos);
     while (FormatPos<=length(Format)) and (CurrentChar<>Format[FormatPos]) do begin
      inc(FormatPos);
     end;
     inc(FormatPos);
    end;
    'A':begin
     if (copy(FormatUp,FormatPos,3)='A/P') or (copy(FormatUp,FormatPos,4)='AMPM') or (copy(FormatUp,FormatPos,5)='AM/PM') then begin
      Clock12:=true;
      break;
     end;
     inc(FormatPos);
    end;
    else begin
     inc(FormatPos);
    end;
   end;
  end;
  LastChar:=#0;
  FormatPos:=1;
  while FormatPos<=length(Format) do begin
   CurrentChar:=FormatUp[FormatPos];
   case CurrentChar of
    '''','"':begin
     inc(FormatPos);
     PosA:=FormatPos;
     while (FormatPos<=length(Format)) and (CurrentChar<>FormatUp[FormatPos]) do begin
      inc(FormatPos);
     end;
     PosB:=FormatPos;
     inc(FormatPos);
     Add(copy(Format,PosA,PosB-PosA));
     LastChar:=CurrentChar;
    end;
    'A':begin
     if copy(FormatUp,FormatPos,3)='A/P' then begin
      if Hour<12 then begin
       Add(Format[FormatPos]);
      end else begin
       Add(Format[FormatPos+2]);
      end;
      inc(FormatPos,3);
     end else if copy(FormatUp,FormatPos,4)='AMPM' then begin
      inc(FormatPos,4);
      if Hour<12 then begin
       Add(FormatSettings.TimeAMString);
      end else begin
       Add(FormatSettings.TimePMString);
      end;
     end else if copy(FormatUp,FormatPos,5)='AM/PM' then begin
      if Hour<12 then begin
       Add(copy(Format,FormatPos,2));
      end else begin
       Add(copy(Format,FormatPos+3,2));
      end;
      inc(FormatPos,5);
     end else begin
      raise EConvertError.Create('Illegal character in format string');
     end;
    end;
    '/':begin
     Add(FormatSettings.DateSeparator);
     inc(FormatPos);
    end;
    ':':begin
     Add(FormatSettings.TimeSeparator);
     inc(FormatPos);
    end;
    ' ':begin
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]=' ') do begin
      Add(' ');
      inc(FormatPos);
     end;
    end;
    'Y':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='Y') do begin
      inc(Count);
      inc(FormatPos);
     end;
     if Count>2 then begin
      AddInt(Year,4);
     end else begin
      AddInt(Year mod 100,2);
     end;
     LastChar:=CurrentChar;
    end;
    'M':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='M') do begin
      inc(Count);
      inc(FormatPos);
     end;
     if (LastChar='H') or TimeFlag then begin
      if Count=1 then begin
       AddInt(Minute,0);
      end else begin
       AddInt(Minute,2);
      end;
     end else begin
      case Count of
       1:begin
        AddInt(Month,0);
       end;
       2:begin
        AddInt(Month,2);
       end;
       3:begin
        Add(FormatSettings.ShortMonthNames[Month]);
       end;
       else begin
        Add(FormatSettings.LongMonthNames[Month]);
       end;
      end;
     end;
     LastChar:=CurrentChar;
    end;
    'D':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='D') do begin
      inc(Count);
      inc(FormatPos);
     end;
     case Count of
      1:begin
       AddInt(Day,0);
      end;
      2:begin
       AddInt(Day,2);
      end;
      3:begin
       Add(FormatSettings.ShortDayNames[DayOfWeek]);
      end;
      4:begin
       Add(FormatSettings.LongDayNames[DayOfWeek]);
      end;
      5:begin
       Process(FormatSettings.ShortDateFormat,Nesting+1,false);
      end;
      else begin
       Process(FormatSettings.LongDateFormat,Nesting+1,false);
      end;
     end;
     LastChar:=CurrentChar;
    end;
    'H':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='H') do begin
      inc(Count);
      inc(FormatPos);
     end;
     t:=Hour;
     if Clock12 then begin
      if t=0 then begin
       t:=12;
      end else if t>12 then begin
       dec(t,12);
      end else if t>24 then begin
       t:=t mod 12;
       if t=0 then begin
        t:=12;
       end;
      end;
     end;
     if Count=1 then begin
      AddInt(t,0);
     end else begin
      AddInt(t,2);
     end;
     LastChar:=CurrentChar;
    end;
    'N':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='N') do begin
      inc(Count);
      inc(FormatPos);
     end;
     if Count=1 then begin
      AddInt(Minute,0);
     end else begin
      AddInt(Minute,2);
     end;
     LastChar:=CurrentChar;
    end;
    'S':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='S') do begin
      inc(Count);
      inc(FormatPos);
     end;
     if Count=1 then begin
      AddInt(Second,0);
     end else begin
      AddInt(Second,2);
     end;
     LastChar:=CurrentChar;
    end;
    'Z':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='Z') do begin
      inc(Count);
      inc(FormatPos);
     end;
     if Count=1 then begin
      AddInt(MilliSecond,0);
     end else begin
      AddInt(MilliSecond,3);
     end;
     LastChar:=CurrentChar;
    end;
    'T':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='T') do begin
      inc(Count);
      inc(FormatPos);
     end;
     if Count=1 then begin
      Process(FormatSettings.ShortTimeFormat,Nesting+1,true);
     end else begin
      Process(FormatSettings.LongTimeFormat,Nesting+1,true);
     end;
     LastChar:=CurrentChar;
    end;
    'C':begin
     Count:=0;
     while (FormatPos<=length(Format)) and (FormatUp[FormatPos]='C') do begin
      inc(Count);
      inc(FormatPos);
     end;
     Process(FormatSettings.ShortDateFormat,Nesting+1,false);
     if (integer(Hour)+integer(Minute)+integer(Second))<>0 then begin
      Add(' ');
      Process(FormatSettings.LongTimeFormat,Nesting+1,true);
     end;
     LastChar:=CurrentChar;
    end;
    else begin
     Add(Format[FormatPos]);
     inc(FormatPos);
    end;
   end;
  end;
 end;
begin
 DecodeDateFully(DateTime,Year,Month,Day,DayOfWeek);
 DecodeTime(DateTime,Hour,Minute,Second,MilliSecond);
 ResultString:='';
 if length(Format)>0 then begin
  Process(Format,0,false);
 end else begin
  Process('C',0,false);
 end;
 result:=ResultString;
end;
{$warnings on}

end.

