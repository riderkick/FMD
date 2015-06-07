{ Simple Logger Class, part of SimpleException

  Copyright (C) 2014 Nur Cholif

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit USimpleLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogType = (ERROR, WARNING, INFO, DEBUG, VERBOSE);

var
  _CS_LOG: TRTLCriticalSection;
  _LOG_ACTIVE: Boolean = False;
  _LOG_LEVEL: Integer = 2;
  _FLOGFILE: String;

const
  _LOG_SYMBOL = 'EWIDV';

  procedure SetLogFile(const LogFileName: String);
  procedure WriteLog_E(const msg: String);
  procedure Writelog_W(const msg: String);
  procedure Writelog_I(const msg: String);
  procedure Writelog_D(const msg: String);
  procedure Writelog_V(const msg: String);

implementation

procedure WriteLog(const msg: String; LogType: TLogType);
var
  s: String;
  f: TextFile;
begin
  if not _LOG_ACTIVE then Exit;
  if Integer(logType) > _LOG_LEVEL then Exit;
  EnterCriticalsection(_CS_LOG);
  try
    s := FormatDateTime('dd/mm/yyyy|hh:nn:ss.zzz ', Now);
    s := s + '[' + _LOG_SYMBOL[Integer(logType)+1] + ']';
    AssignFile(f, _FLOGFILE);
    try
      if FileExists(_FLOGFILE) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, s + ' ' + msg);
    finally
      CloseFile(f);
    end;
  finally
    LeaveCriticalsection(_CS_LOG);
  end;
end;

procedure SetLogFile(const LogFileName: String);
begin
  if Trim(LogFileName) <> '' then
    _FLOGFILE := LogFileName;
end;

procedure WriteLog_E(const msg: String);
begin
  WriteLog(msg, ERROR);
end;

procedure Writelog_W(const msg: String);
begin
  WriteLog(msg, WARNING);
end;

procedure Writelog_I(const msg: String);
begin
  WriteLog(msg, INFO);
end;

procedure Writelog_D(const msg: String);
begin
  WriteLog(msg, DEBUG);
end;

procedure Writelog_V(const msg: String);
begin
  WriteLog(msg, VERBOSE);
end;

procedure doInitialization;
var
  i: Integer;
begin
  {$IFDEF LOGACTIVE}
  _LOG_ACTIVE := True;
  _LOG_LEVEL := 4;
  {$ENDIF}
  _FLOGFILE := ChangeFileExt(ExtractFileName(ParamStr(0)), '_LOG.txt');
  for i := 1 to Paramcount do
  begin
    if UpperCase(ParamStr(i)) = '-LOGACTIVE' then
    begin
      _LOG_ACTIVE := True;
      if i < Paramcount then
      begin
        if StrToIntDef(ParamStr(i+1), -1) > -1 then
          _LOG_LEVEL := StrToInt(ParamStr(i+1));
      end;
      if _LOG_LEVEL > SizeOf(TLogType) then
        _LOG_LEVEL := SizeOf(TLogType);
    end;
  end;
  Writelog_V('Initialize Logger');
end;

initialization
  InitCriticalSection(_CS_LOG);
  doInitialization;

finalization
  Writelog_V('Finalize Logger');
  DoneCriticalsection(_CS_LOG);

end.

