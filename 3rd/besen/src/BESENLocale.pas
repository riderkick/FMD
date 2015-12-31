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
unit BESENLocale;
{$i BESEN.inc}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}{$ifdef unix}dl,BaseUnix,Unix,
     UnixType,{$endif}SysUtils,Classes,BESENConstants,BESENTypes,BESENBaseObject;

const BESENDefaultFormatSettings:TFormatSettings=(
{$ifdef DelphiXEAndUp}
        CurrencyString:'$';
        CurrencyFormat:1;
        CurrencyDecimals:2;
        DateSeparator:'-';
        TimeSeparator:':';
        ListSeparator:',';
        ShortDateFormat:'d/m/y';
        LongDateFormat:'dd" "mmmm" "yyyy';
        TimeAMString:'AM';
        TimePMString:'PM';
        ShortTimeFormat:'hh:nn';
        LongTimeFormat:'hh:nn:ss';
        ShortMonthNames:('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
        LongMonthNames:('January','February','March','April','May','June','July','August','September','October','November','December');
        ShortDayNames:('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
        LongDayNames:('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
        ThousandSeparator:',';
        DecimalSeparator:'.';
        TwoDigitYearCenturyWindow:50;
        NegCurrFormat:5;
{$else}
       CurrencyFormat:1;
       NegCurrFormat:5;
       ThousandSeparator:',';
       DecimalSeparator:'.';
       CurrencyDecimals:2;
       DateSeparator:'-';
       TimeSeparator:':';
       ListSeparator:',';
       CurrencyString:'$';
       ShortDateFormat:'d/m/y';
       LongDateFormat:'dd" "mmmm" "yyyy';
       TimeAMString:'AM';
       TimePMString:'PM';
       ShortTimeFormat:'hh:nn';
       LongTimeFormat:'hh:nn:ss';
       ShortMonthNames:('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
       LongMonthNames:('January','February','March','April','May','June','July','August','September','October','November','December');
       ShortDayNames:('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
       LongDayNames:('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
       TwoDigitYearCenturyWindow:50;
{$endif}
      );

var BESENLocaleFormatSettings:TFormatSettings;

implementation

uses BESENCharSet,BESENStringUtils;

{$warnings off}
procedure InitLocaleFormatSettings;
{$ifdef windows}
{$ifdef fpc}
var i:integer;
begin
 BESENLocaleCharset:=ISO_8859_1;//BESENGetCodePage('ISO-8859-1');
 BESENLocaleFormatSettings:=BESENDefaultFormatSettings;
 for i:= 1 to 12 do begin
  BESENLocaleFormatSettings.ShortMonthNames[i]:=SysUtils.ShortMonthNames[i];
  BESENLocaleFormatSettings.LongMonthNames[i]:=SysUtils.LongMonthNames[i];
 end;
 for i:=1 to 7 do begin
  BESENLocaleFormatSettings.ShortDayNames[i]:=SysUtils.ShortDayNames[i];
  BESENLocaleFormatSettings.LongDayNames[i]:=SysUtils.LongDayNames[i];
 end;
 BESENLocaleFormatSettings.DateSeparator:=SysUtils.DateSeparator;
 BESENLocaleFormatSettings.ShortDateFormat:=SysUtils.ShortDateFormat;
 BESENLocaleFormatSettings.LongDateFormat:=SysUtils.LongDateFormat;
 BESENLocaleFormatSettings.TimeSeparator:=SysUtils.TimeSeparator;
 BESENLocaleFormatSettings.TimeAMString:=SysUtils.TimeAMString;
 BESENLocaleFormatSettings.TimePMString:=SysUtils.TimePMString;
 BESENLocaleFormatSettings.ShortTimeFormat:=SysUtils.ShortTimeFormat;
 BESENLocaleFormatSettings.LongTimeFormat:=SysUtils.LongTimeFormat;
 BESENLocaleFormatSettings.CurrencyString:=SysUtils.CurrencyString;
 BESENLocaleFormatSettings.CurrencyFormat:=SysUtils.CurrencyFormat;
 BESENLocaleFormatSettings.NegCurrFormat:=SysUtils.NegCurrFormat;
 BESENLocaleFormatSettings.ThousandSeparator:=SysUtils.ThousandSeparator;
 BESENLocaleFormatSettings.DecimalSeparator:=SysUtils.DecimalSeparator;
 BESENLocaleFormatSettings.CurrencyDecimals:=SysUtils.CurrencyDecimals;
 BESENLocaleFormatSettings.ListSeparator:=SysUtils.ListSeparator;
end;
{$else}
var HourFormat,TimePrefix,TimePostfix:{$ifdef BESENSingleStringType}TBESENSTRING{$else}TBESENANSISTRING{$endif};
    LID:LCID;
    i,LCP,Day:integer;
begin
 BESENLocaleFormatSettings:=BESENDefaultFormatSettings;
 LID:=GetThreadLocale;
 if not TryStrToInt(GetLocaleStr(LID,LOCALE_IDEFAULTANSICODEPAGE,inttostr(GetACP)),LCP) then begin
  LCP:=GetACP;
 end;
{$ifndef BESENSingleStringType}
 if LCP>0 then begin
  BESENLocaleCharset:=BESENGetCodePage('WINDOWS-'+inttostr(LCP));
 end else{$endif} begin
  BESENLocaleCharset:=ISO_8859_1;
 end;
 for i:=1 to 12 do begin
  BESENLocaleFormatSettings.ShortMonthNames[i]:=GetLocaleStr(LID,LOCALE_SABBREVMONTHNAME1+i-1,BESENDefaultFormatSettings.ShortMonthNames[i]);
  BESENLocaleFormatSettings.LongMonthNames[i]:=GetLocaleStr(LID,LOCALE_SMONTHNAME1+i-1,BESENDefaultFormatSettings.LongMonthNames[i]);
 end;
 for i:=1 to 7 do begin
  Day:=(i+5) mod 7;
  BESENLocaleFormatSettings.ShortDayNames[i]:=GetLocaleStr(LID,LOCALE_SABBREVDAYNAME1+Day,BESENDefaultFormatSettings.ShortDayNames[i]);
  BESENLocaleFormatSettings.LongDayNames[i]:=GetLocaleStr(LID,LOCALE_SDAYNAME1+Day,BESENDefaultFormatSettings.LongDayNames[i]);
 end;
 BESENLocaleFormatSettings.DateSeparator:=GetLocaleChar(LID,LOCALE_SDATE,'/');
 BESENLocaleFormatSettings.ShortDateFormat:=GetLocaleStr(LID,LOCALE_SSHORTDATE,'m/d/yy');
 BESENLocaleFormatSettings.LongDateFormat:=GetLocaleStr(LID,LOCALE_SLONGDATE,'mmmm d, yyyy');
 BESENLocaleFormatSettings.TimeSeparator:=GetLocaleChar(LID,LOCALE_STIME,':');
 BESENLocaleFormatSettings.TimeAMString:=GetLocaleStr(LID,LOCALE_S1159,'AM');
 BESENLocaleFormatSettings.TimePMString:=GetLocaleStr(LID,LOCALE_S2359,'PM');
 if StrToIntDef(GetLocaleStr(LID,LOCALE_ITLZERO,'0'),0)=0 then begin
  HourFormat:='h';
 end else begin
  HourFormat:='hh';
 end;
 TimePostfix:='';
 TimePrefix:='';
 if StrToIntDef(GetLocaleStr(LID,LOCALE_ITIME,'0'),0)=0 then begin
  if StrToIntDef(GetLocaleStr(LID,LOCALE_ITIMEMARKPOSN,'0'),0)=0 then begin
   TimePostfix:=' AMPM';
  end else begin
   TimePrefix:='AMPM ';
  end;
 end;
 BESENLocaleFormatSettings.ShortTimeFormat:=TimePrefix+HourFormat+':nn'+TimePrefix;
 BESENLocaleFormatSettings.LongTimeFormat:=TimePrefix+HourFormat+':nn:ss'+TimePrefix;
 BESENLocaleFormatSettings.CurrencyString:=GetLocaleStr(LID,LOCALE_SCURRENCY,'');
 BESENLocaleFormatSettings.CurrencyFormat:=StrToIntDef(GetLocaleStr(LID,LOCALE_ICURRENCY,'0'),0);
 BESENLocaleFormatSettings.NegCurrFormat:=StrToIntDef(GetLocaleStr(LID,LOCALE_INEGCURR,'0'),0);
 BESENLocaleFormatSettings.ThousandSeparator:=GetLocaleChar(LID,LOCALE_STHOUSAND,',');
 BESENLocaleFormatSettings.DecimalSeparator:=GetLocaleChar(LID,LOCALE_SDECIMAL,'.');
 BESENLocaleFormatSettings.CurrencyDecimals:=StrToIntDef(GetLocaleStr(LID,LOCALE_ICURRDIGITS,'0'),0);
 BESENLocaleFormatSettings.ListSeparator:=GetLocaleChar(LID,LOCALE_SLIST,',');
end;
{$endif}
{$else}
var i:integer;
begin
 BESENLocaleCharset:=ISO_8859_1;//BESENGetCodePage('ISO-8859-1');
 BESENLocaleFormatSettings:=BESENDefaultFormatSettings;
 for i:= 1 to 12 do begin
  BESENLocaleFormatSettings.ShortMonthNames[i]:={$ifdef DelphiXE2AndUp}{$else}SysUtils.ShortMonthNames[i]{$endif};
  BESENLocaleFormatSettings.LongMonthNames[i]:={$ifdef DelphiXE2AndUp}{$else}SysUtils.LongMonthNames[i]{$endif};
 end;
 for i:=1 to 7 do begin
  BESENLocaleFormatSettings.ShortDayNames[i]:={$ifdef DelphiXE2AndUp}{$else}SysUtils.ShortDayNames[i]{$endif};
  BESENLocaleFormatSettings.LongDayNames[i]:={$ifdef DelphiXE2AndUp}{$else}SysUtils.LongDayNames[i]{$endif};
 end;
 BESENLocaleFormatSettings.DateSeparator:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.DateSeparator{$else}SysUtils.DateSeparator{$endif};
 BESENLocaleFormatSettings.ShortDateFormat:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.ShortDateFormat{$else}SysUtils.ShortDateFormat{$endif};
 BESENLocaleFormatSettings.LongDateFormat:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.LongDateFormat{$else}SysUtils.LongDateFormat{$endif};
 BESENLocaleFormatSettings.TimeSeparator:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.TimeSeparator{$else}SysUtils.TimeSeparator{$endif};
 BESENLocaleFormatSettings.TimeAMString:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.TimeAMString{$else}SysUtils.TimeAMString{$endif};
 BESENLocaleFormatSettings.TimePMString:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.TimePMString{$else}SysUtils.TimePMString{$endif};
 BESENLocaleFormatSettings.ShortTimeFormat:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.ShortTimeFormat{$else}SysUtils.ShortTimeFormat{$endif};
 BESENLocaleFormatSettings.LongTimeFormat:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.LongTimeFormat{$else}SysUtils.LongTimeFormat{$endif};
 BESENLocaleFormatSettings.CurrencyString:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.CurrencyString{$else}SysUtils.CurrencyString{$endif};
 BESENLocaleFormatSettings.CurrencyFormat:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.CurrencyFormat{$else}SysUtils.CurrencyFormat{$endif};
 BESENLocaleFormatSettings.NegCurrFormat:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.NegCurrFormat{$else}SysUtils.NegCurrFormat{$endif};
 BESENLocaleFormatSettings.ThousandSeparator:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.ThousandSeparator{$else}SysUtils.ThousandSeparator{$endif};
 BESENLocaleFormatSettings.DecimalSeparator:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.DecimalSeparator{$else}SysUtils.DecimalSeparator{$endif};
 BESENLocaleFormatSettings.CurrencyDecimals:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.CurrencyDecimals{$else}SysUtils.CurrencyDecimals{$endif};
 BESENLocaleFormatSettings.ListSeparator:={$ifdef DelphiXE2AndUp}SysUtils.FormatSettings.ListSeparator{$else}SysUtils.ListSeparator{$endif};
end;
{$endif}
{$warnings on}

procedure InitBESEN;
begin
 InitLocaleFormatSettings;
end;

procedure DoneBESEN;
begin
end;

initialization
 InitBESEN;
finalization
 DoneBESEN;

end.
