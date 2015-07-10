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
  Classes, SysUtils, DbgInfoReader, LazFileUtils, LazUTF8;

type
  TLogType = (ERROR, WARNING, INFO, DEBUG, VERBOSE);

var
  _CS_LOG: TRTLCriticalSection;
  _LOG_ACTIVE: Boolean = False;
  _LOG_LEVEL: Integer = 2;
  _FLOGFILE: String;
  _HAS_DEBUG_LINE: Boolean;

const
  _LOG_SYMBOL = 'EWIDV';

  procedure SetLogFile(const LogFileName: String);
  procedure WriteLog_E(const msg: String); overload;
  procedure WriteLog_E(const msg: String; Exc: Exception; Sender: TObject = nil); overload;
  procedure Writelog_W(const msg: String);
  procedure Writelog_I(const msg: String);
  procedure Writelog_D(const msg: String);
  procedure Writelog_V(const msg: String);
  function SimpleBackTraceStr(const Addr: Pointer): String;
  function GetStackTraceInfo(const MaxStackCount: Integer = 20): string;

implementation

procedure ForceLogFile(const logfilename: string);
var
  f: String;
begin
  f := ExtractFileDir(logfilename);
  if f <> '' then
  begin
    if _LOG_ACTIVE and (not DirectoryExists(f)) then
      ForceDirectories(f);
    f := f + PathDelim + ExtractFileName(logfilename);
  end
  else
    f := logfilename;
  _FLOGFILE := f;
end;

procedure WriteLog(const msg: String; LogType: TLogType = DEBUG);
var
  s: String;
  f: TextFile;
begin
  if not _LOG_ACTIVE then Exit;
  if _FLOGFILE = '' then Exit;
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
      begin
        ForceLogFile(_FLOGFILE);
        Rewrite(f);
      end;
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
  begin
    EnterCriticalsection(_CS_LOG);
    try
      ForceLogFile(LogFileName);
    finally
      LeaveCriticalsection(_CS_LOG);
    end;
  end;
end;

procedure WriteLog_E(const msg: String);
begin
  WriteLog(msg, ERROR);
end;

procedure WriteLog_E(const msg: String; Exc: Exception; Sender: TObject);
var
  s: string;
begin
  s := '';
  if Assigned(Sender) then
    s += LineEnding +
         'Sender Class     : ' + Sender.ClassName;
  if Assigned(Exc) then
  begin
    s += LineEnding +
         'Exception Class  : ' + Exc.ClassName + LineEnding +
         'Exception Message: ' + Exc.Message;
  end;
  s += LineEnding + GetStackTraceInfo;
  WriteLog_E(msg + s);
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

function SimpleBackTraceStr(const Addr: Pointer): String;
var
  func, Source: ShortString;
  hs: String[32];
  line: longint;
begin
  Result := '$' + hexStr(Addr);
  if _HAS_DEBUG_LINE then
  begin
    try
      GetLineInfo({%H-}PtrUInt(Addr), func, Source, line);
      if func <> '' then
        Result := Result + ' ' + func;
      if Source <> '' then
      begin
        if func <> '' then
          Result := Result + ',';
        if line <> 0 then
        begin
          str(line, hs);
          Result := Result + ' line ' + hs;
        end;
        Result := Result + ' of ' + Source;
      end;
    except
      Result := Result + ' ??';
    end;
  end;
end;

function GetStackTraceInfo(const MaxStackCount: Integer): string;
var
  i, maxStack: Integer;
  cf, pcf, cAddress, cFrame: Pointer;
begin
  try
    if ExceptFrameCount > 0 then
    begin
      Result :=
        'Exception Address : $' + hexStr(ExceptAddr) + LineEnding;
      if ExceptFrameCount > MaxStackCount then
        maxStack := MaxStackCount - 1
      else
        maxStack := ExceptFrameCount - 1;
      for i := 0 to maxStack do
        Result := Result + '  ' + SimpleBackTraceStr(ExceptFrames[i]) +
          LineEnding;
    end
    else
    begin
      //cf := get_caller_frame(get_frame);
      cf := get_caller_frame(get_caller_frame(get_frame));
      if cf <> nil then
      begin
        Result :=
          'Caller Address    : ' + '$' + hexStr(cf) + LineEnding;
        try
          i := 0;
          pcf := cf - 1;
          while cf > pcf do
          begin
            cAddress := get_caller_addr(cf);
            cFrame := get_caller_frame(cf);
            if cAddress = nil then
              Break;
            Inc(i);
            if i > MaxStackCount then
              Break;
            Result := Result + '  ' + SimpleBackTraceStr(cAddress) +
              LineEnding;
            pcf := cf;
            cf := cFrame;
          end;
        finally
        end;
      end;
    end;
  except
    Result := 'Can''t get stack trace information!';
  end;
  Result := TrimRight(Result);
end;

procedure doInitialization;
var
  i: Integer;
begin
  InitCriticalSection(_CS_LOG);
  _HAS_DEBUG_LINE := OpenSymbolFile(ParamStrUTF8(0));
  {$IFDEF LOGACTIVE}
  _LOG_ACTIVE := True;
  _LOG_LEVEL := SizeOf(TLogType);
  {$ENDIF}
  _FLOGFILE := ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '_LOG.txt');
  for i := 1 to Paramcount do
  begin
    if UpperCase(ParamStrUTF8(i)) = '-LOGACTIVE' then
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
end;

procedure doFinalization;
begin
  CloseSymbolFile;
  DoneCriticalsection(_CS_LOG);
end;

initialization
  doInitialization;

finalization
  doFinalization;

end.

