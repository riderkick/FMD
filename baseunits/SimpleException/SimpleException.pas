{ SimpleException Class

  Copyright (C) 2014-2016 Nur Cholif

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit SimpleException;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, Forms, Controls, LCLVersion,
  SimpleExceptionForm,
  {$IFDEF WINDOWS}
  Windows, win32proc,
  {$ENDIF}
  {$IFDEF LINUX}
  elfreader,
  {$ENDIF}
  {$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  machoreader,
  {$ENDIF}
  fileinfo,
  MultiLog;

type

  { TSimpleException }

  TSimpleException = class
  private
    FLogFileName: String;
    FLastSender: TObject;
    FLastException: Exception;
    FApplicationInfo,
    FLastReport: String;
    FMaxStackCount: Integer;
    FSimpleCriticalSection: TRTLCriticalSection;
    FDefaultAppFlags: TApplicationFlags;
    FUnhandled: Boolean;
    procedure SetLogFileName(const AValue: String);
    procedure SetMaxStackCount(AMaxStackCount: Integer);
  protected
    function ExceptionHeaderMessage: String;
    function GetStackTraceStr: String;
    procedure CreateExceptionReport;
    procedure SaveLogToFile(const LogMsg: String);
    procedure CallExceptionHandler;
    procedure ExceptionHandler;
    procedure UnhandledException(Obj: TObject; Addr: CodePointer; FrameCount: LongInt;
      Frames: PCodePointer);
  public
    IgnoredExceptionList: TStringList;
    property ApplicationInfo: String read FApplicationInfo;
    property LogFileName: String read FLogFileName write SetLogFileName;
    property MaxStackCount: Integer read FMaxStackCount write SetMaxStackCount;
    property LastSender: TObject read FLastSender;
    property LastException: Exception read FLastException;
    property LastReport: String read FLastReport;
    procedure SimpleExceptionHandler(Sender: TObject; E: Exception);
    procedure SimpleExceptionHandlerSaveLogOnly(Sender: TObject; E: Exception);
    constructor Create(const FileName: String = '');
    destructor Destroy; override;
  end;

function GetApplicationInfo: String;
function AddIgnoredException(const EClassName: String): Boolean;
function RemoveIgnoredClass(const EClassName: String): Boolean;
procedure SetMaxStackCount(const ACount: Integer);
procedure ClearIgnoredException;
procedure ExceptionHandle(Sender: TObject; E: Exception);
procedure ExceptionHandleSaveLogOnly(Sender: TObject; E: Exception);
procedure InitSimpleExceptionHandler(const LogFileName: String = '');
procedure SetLogFileName(const LogFileName: String);
procedure DoneSimpleExceptionHandler;

var
  MainExceptionHandler: TSimpleException;

resourcestring
  SExceptionDialogTitle = 'Exception Info';
  SExceptionCaption = 'An error occured during program execution:';
  SButtonDetails = '&Show Details';
  SButtonTerminate = '&Terminate';
  SButtonContinue = '&Continue';
  SCheckBoxIgnoreException = 'Ignore this exception for next time';
  SCantHandleException = 'Can''t handle exception';

implementation

type

  { TLoggerException }

  TLoggerException = class helper for TLogger
  public
    procedure SendExceptionStr(const AText: String; AExceptionStr: String);
  end;


procedure SetMaxStackCount(const ACount: Integer);
begin
  if MainExceptionHandler <> nil then
    MainExceptionHandler.MaxStackCount := ACount;
end;

function GetApplicationInfo: String;
begin
  Result := '';
  if Assigned(MainExceptionHandler) then
    Result := MainExceptionHandler.ApplicationInfo;
end;

function AddIgnoredException(const EClassName: String): Boolean;
begin
  Result := False;
  if MainExceptionHandler <> nil then
    if MainExceptionHandler.IgnoredExceptionList.IndexOf(EClassName) < 0 then
    begin
      Result := True;
      MainExceptionHandler.IgnoredExceptionList.Add(EClassName);
    end;
end;

function RemoveIgnoredClass(const EClassName: String): Boolean;
begin
  Result := False;
  if MainExceptionHandler <> nil then
    if MainExceptionHandler.IgnoredExceptionList.IndexOf(EClassName) > -1 then
    begin
      Result := True;
      MainExceptionHandler.IgnoredExceptionList.Delete(
        MainExceptionHandler.IgnoredExceptionList.IndexOf(EClassName));
    end;
end;

procedure ClearIgnoredException;
begin
  if MainExceptionHandler <> nil then
    MainExceptionHandler.IgnoredExceptionList.Clear;
end;

procedure ExceptionHandle(Sender: TObject; E: Exception);
begin
  if not Assigned(MainExceptionHandler) then
    InitSimpleExceptionHandler;
  MainExceptionHandler.SimpleExceptionHandler(Sender, E);
end;

procedure ExceptionHandleSaveLogOnly(Sender: TObject; E: Exception);
begin
  if not Assigned(MainExceptionHandler) then
    InitSimpleExceptionHandler;
  MainExceptionHandler.SimpleExceptionHandlerSaveLogOnly(Sender, E);
end;

procedure InitSimpleExceptionHandler(const LogFileName: String);
begin
  if MainExceptionHandler = nil then
    MainExceptionHandler := TSimpleException.Create(LogFilename);
end;

procedure SetLogFileName(const LogFileName: String);
begin
  if MainExceptionHandler <> nil then
    MainExceptionHandler.LogFileName := LogFileName;
end;

procedure DoneSimpleExceptionHandler;
begin
  if MainExceptionHandler <> nil then
    FreeAndNil(MainExceptionHandler);
end;

{ TLoggerException }

procedure TLoggerException.SendExceptionStr(const AText: String; AExceptionStr: String);
begin
  SendBuffer(ltException, AText, AExceptionStr[1], Length(AExceptionStr));
end;

{ TSimpleException }

procedure TSimpleException.SetMaxStackCount(AMaxStackCount: Integer);
begin
  if FMaxStackCount <> AMaxStackCount then
  begin
    if AMaxStackCount < 1 then
      FMaxStackCount := 1
    else
    if AMaxStackCount > 255 then
      FMaxStackCount := 255
    else
      FMaxStackCount := AMaxStackCount;
  end;
end;

procedure TSimpleException.SetLogFileName(const AValue: String);
begin
  if FLogFileName = AValue then Exit;
  FLogFileName := AValue;
end;

function GetOSVer: String;
{$IFDEF WINDOWS}
var
  wdir: array [0..MAX_PATH] of Char;

  function WinLater: String;
  begin
    if (Win32MajorVersion = 6) and (Win32MinorVersion = 3) then
      Result := 'Windows 8.1'
    else if (Win32MajorVersion = 10) and (Win32MinorVersion = 0) then
      Result := 'Windows 10'
    else
      Result := Format('Windows %d.%d', [Win32MajorVersion, Win32MinorVersion]);
  end;

{$ENDIF}
begin
  {$IFDEF LCLcarbon}
  Result := 'Mac OS X 10.';
  {$ENDIF}
  {$IFDEF Linux}
  Result := 'Linux Kernel ';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := 'Unix ';
  {$ENDIF}
  {$IFDEF WINDOWS}
  case WindowsVersion of
    wv95: Result := 'Windows 95';
    wvNT4: Result := 'Windows NT v.4';
    wv98: Result := 'Windows 98';
    wvMe: Result := 'Windows ME';
    wv2000: Result := 'Windows 2000';
    wvXP: Result := 'Windows XP';
    wvServer2003: Result := 'Windows Server 2003';
    wvVista: Result := 'Windows Vista';
    wv7: Result := 'Windows 7';
    wv8: Result := 'Windows 8';
    else
      Result := WinLater;
  end;
  FillChar({%H-}wdir, SizeOf(wdir), 0);
  GetWindowsDirectory(PChar(wdir), MAX_PATH);
  if DirectoryExists(wdir + '\SysWOW64') then
    Result := Result + ' 64-bit';
  {$ENDIF}
end;

procedure TSimpleException.ExceptionHandler;
begin
  if Assigned(FLastException) then
    if (IgnoredExceptionList.IndexOf(FLastException.ClassName) > -1) then
      Exit;
  with TSimpleExceptionForm.Create(nil) do try
      MemoExceptionLog.Lines.Text := FLastReport;
      if Assigned(FLastException) then
        LabelExceptionMessage.Caption := FLastException.Message;
      if FUnhandled then
      begin
        CheckBoxIgnoreException.Visible := False;
        ButtonContinue.Visible := False;
      end;
      if ShowModal = mrIgnore then
        AddIgnoredException(FLastException.ClassName);
    finally
      Free;
    end;
end;

procedure TSimpleException.UnhandledException(Obj: TObject; Addr: CodePointer;
  FrameCount: LongInt; Frames: PCodePointer);
var
  i: Integer;
  S: String;
begin
  EnterCriticalSection(FSimpleCriticalSection);
  try
    FUnhandled := True;
    if Obj is Exception then
    begin
      FLastSender := nil;
      FLastException := Exception(Obj);
      CreateExceptionReport;
      CallExceptionHandler;
    end
    else
    begin
      FLastReport := ExceptionHeaderMessage;
      if Obj is TObject then
        FLastReport := FLastReport +
          'Sender Class      : ' + Obj.ClassName + LineEnding;
      FLastReport := FLastReport +
        'Exception Address : $' + BackTraceStrFunc(Addr) + LineEnding;
      S := '';
      if FrameCount > 0 then
        for i := 0 to FrameCount - 1 do
          S := S + '  ' + BackTraceStrFunc(Frames[i]) + LineEnding;
      FLastReport := FLastReport + S;
      if Logger.Enabled then
        Logger.SendExceptionStr('Unhandled Exception occured at $' + BackTraceStrFunc(Addr), S)
      else
        SaveLogToFile(FLastReport);
      CallExceptionHandler;
    end;
  finally
    LeaveCriticalSection(FSimpleCriticalSection);
  end;
end;

function TSimpleException.ExceptionHeaderMessage: String;
var
  i: Integer;
begin
  try
    if FUnhandled then
      Result := 'Unhandled exception!'
    else
      Result := 'Program exception!';
    Result := Result + LineEnding +
      FApplicationInfo + LineEnding +
      'Thread ID         : ' + IntToStr(GetThreadID) + LineEnding +
      'Time              : ' + FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz', Now) + LineEnding;
    if IgnoredExceptionList.Count > 0 then
    begin
      Result := Result + 'Ignored Exception : ' + IgnoredExceptionList[0];
      for i := 1 to IgnoredExceptionList.Count - 1 do
        Result := Result + ', ' + IgnoredExceptionList[i];
      Result := Result + LineEnding;
    end;
  except
    Result := '';
  end;
end;

function TSimpleException.GetStackTraceStr: String;
var
  Frames: PPointer;
  i, AMaxStackCount: Integer;
begin
  Result := '';
  try
    Result := BackTraceStrFunc(ExceptAddr);
    Frames := ExceptFrames;
    if ExceptFrameCount > FMaxStackCount then
      AMaxStackCount := FMaxStackCount
    else
      AMaxStackCount := ExceptFrameCount;
    for i := 0 to AMaxStackCount - 1 do
      Result := Result + (LineEnding + BackTraceStrFunc(Frames[i]));
  except
  end;
end;

procedure TSimpleException.CreateExceptionReport;
var
  S: String;
begin
  S := '';
  FLastReport := ExceptionHeaderMessage;
  if Assigned(FLastSender) then
    FLastReport := FLastReport +
      'Sender Class      : ' + FLastSender.ClassName + LineEnding;
  if Assigned(FLastException) then
  begin
    FLastReport := FLastReport +
      'Exception Class   : ' + FLastException.ClassName + LineEnding +
      'Message           : ' + FLastException.Message + LineEnding;
  end;
  S := GetStackTraceStr;
  FLastReport := FLastReport + S;
  if Logger.Enabled then
  begin
    if Assigned(FLastException) then
      Logger.SendExceptionStr(FLastException.ClassName + ' - ' + FLastException.Message, S)
    else
      Logger.SendExceptionStr('Program exception!', S);
  end
  else
    SaveLogToFile(FLastReport);
end;

procedure TSimpleException.SaveLogToFile(const LogMsg: String);
var
  f: TextFile;
begin
  if LogFileName <> '' then
  begin
    AssignFile(f, LogFileName);
    try
      if FileExistsUTF8(LogFileName) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, LogMsg);
    finally
      CloseFile(f);
    end;
  end;
end;

procedure TSimpleException.CallExceptionHandler;
begin
  if (ThreadID <> MainThreadID) then
    try
      {$IF FPC_FULLVERSION >= 20701}
      TThread.Synchronize(TThread.CurrentThread, @ExceptionHandler);
      {$ELSE}
      if (Sender <> nil) and (Sender is TThread) then
        TThread.Synchronize((Sender as TThread), @ExceptionHandler)
      {$ENDIF}
    except
      if Logger.Enabled then
        Logger.SendError(SCantHandleException)
      else
        SaveLogToFile(SCantHandleException);
    end
  else
    ExceptionHandler;
end;

procedure TSimpleException.SimpleExceptionHandler(Sender: TObject; E: Exception);
begin
  if E = nil then
    Exit;
  EnterCriticalsection(FSimpleCriticalSection);
  try
    FUnhandled := False;
    FLastSender := Sender;
    FLastException := E;
    CreateExceptionReport;
    CallExceptionHandler;
  finally
    LeaveCriticalsection(FSimpleCriticalSection);
  end;
end;

procedure TSimpleException.SimpleExceptionHandlerSaveLogOnly(Sender: TObject;
  E: Exception);
begin
  if E = nil then
    Exit;
  EnterCriticalsection(FSimpleCriticalSection);
  try
    FUnhandled := False;
    FLastSender := Sender;
    FLastException := E;
    CreateExceptionReport;
  finally
    LeaveCriticalsection(FSimpleCriticalSection);
  end;
end;

procedure CatchUnhandledExcept(Obj: TObject; Addr: CodePointer; FrameCount: LongInt; Frames: PCodePointer);
begin
  if Assigned(MainExceptionHandler) then
    MainExceptionHandler.UnhandledException(Obj, Addr, FrameCount, Frames);
end;

constructor TSimpleException.Create(const FileName: String);
var
  AFileVersion, AProductVersion: String;
begin
  inherited Create;
  if Trim(FileName) <> '' then
    LogFileName := FileName
  else
    LogFileName := ChangeFileExt(Application.ExeName, '.log');
  InitCriticalSection(FSimpleCriticalSection);
  IgnoredExceptionList := TStringList.Create;
  FMaxStackCount := 20;
  with TFileVersionInfo.Create(nil) do
    try
      FileName := ParamStrUTF8(0);
      if FileName = '' then
        FileName := Application.ExeName;
      try
        Enabled := True;
        if VersionStrings.Count <> 0 then
        begin
          AFileVersion := VersionStrings.Values['fileversion'];
          AProductVersion := VersionStrings.Values['productversion'];
        end;
      except
      end;
    finally
      Free;
    end;
  FApplicationInfo :=
    'Application       : ' + Application.Title + LineEnding;
  if AFileVersion <> '' then
    FApplicationInfo := FApplicationInfo +
      'Version           : ' + AFileVersion + LineEnding;
  if AProductVersion <> '' then
    FApplicationInfo := FApplicationInfo +
      'Product Version   : ' + AProductVersion + LineEnding;
  FApplicationInfo := FApplicationInfo +
    'Host Machine      : ' + GetOSVer + LineEnding +
    'Target CPU_OS     : ' + {$i %FPCTARGETCPU%} +'_' + {$i %FPCTARGETOS%} +LineEnding +
    'FPC Version       : ' + {$i %FPCVERSION%} +LineEnding +
    'LCL Version       : ' + LCLVersion.lcl_version + LineEnding +
    'Path              : ' + ParamStrUTF8(0) + LineEnding +
    'Process ID        : ' + IntToStr(GetProcessID) + LineEnding +
    'MainThread ID     : ' + IntToStr(MainThreadID);
  if Assigned(Application) then
  begin
    FDefaultAppFlags := Application.Flags;
    Application.Flags := Application.Flags + [AppNoExceptionMessages];
    Application.OnException := @SimpleExceptionHandler;
    ExceptProc := @CatchUnhandledExcept;
  end;
end;

destructor TSimpleException.Destroy;
begin
  if Assigned(Application) then
  begin
    Application.OnException := nil;
    Application.Flags := FDefaultAppFlags;
  end;
  IgnoredExceptionList.Free;
  DoneCriticalsection(FSimpleCriticalSection);
  inherited Destroy;
end;

initialization

finalization
  DoneSimpleExceptionHandler;

end.
