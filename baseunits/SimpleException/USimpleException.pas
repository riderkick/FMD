{ SimpleException Class

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

unit USimpleException;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, LCLVersion, DbgInfoReader,
  USimpleExceptionForm, USimpleLogger,
  {$IFDEF WINDOWS}
  windows, win32proc,
  {$ENDIF}
  {$IFDEF LINUX}
  elfreader,
  {$ENDIF}
  {$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  machoreader,
  {$ENDIF}
  fileinfo;

type

  { TSimpleException }

  TSimpleException = class
  private
    FAppInfo_comments,
    FAppInfo_companyname,
    FAppInfo_filedescription,
    FAppInfo_fileversion,
    FAppInfo_internalname,
    FAppInfo_legalcopyright,
    FAppInfo_legaltrademarks,
    FAppInfo_originalfilename,
    FAppInfo_productname,
    FAppInfo_productversion: String;
    FAppVerInfo: TStringList;
    FOSversion: string;
    FHasDebugLine: Boolean;
    FLastSender: TObject;
    FLastException: Exception;
    FLastReport: String;
    FMaxStackCount: Integer;
    FSimpleCriticalSection: TRTLCriticalSection;
    FDefaultAppFlags: TApplicationFlags;
    FUnhandled: Boolean;
    function OSVer: String;
    procedure SetMaxStackCount(AMaxStackCount: Integer);
  protected
    function SimpleBackTraceStr(Addr: Pointer): String;
    function ExceptionHeaderMessage: string;
    procedure CreateExceptionReport;
    procedure SaveLogToFile(LogMsg: string);
    procedure CallExceptionHandler;
    procedure ExceptionHandler;
    procedure UnhandledException(Obj : TObject; Addr: CodePointer; FrameCount: Longint; Frames: PCodePointer);
  public
    LogFilename: String;
    IgnoredExceptionList: TStringlist;
    property MaxStackCount: Integer read FMaxStackCount write SetMaxStackCount;
    property LastSender: TObject read FLastSender;
    property LastException: Exception read FLastException;
    property LastReport: String read FLastReport;
    procedure SimpleExceptionHandler(Sender: TObject; E: Exception);
    procedure SimpleExceptionHandlerSaveLogOnly(Sender: TObject; E: Exception);
    constructor Create(Filename: string = '');
    destructor Destroy; override;
  end;

function AddIgnoredException(const EClassName: String): Boolean;
function RemoveIgnoredClass(const EClassName: String): Boolean;
procedure SetMaxStackCount(const ACount: Integer);
procedure ClearIgnoredException;
procedure ExceptionHandle(Sender: TObject; E: Exception);
procedure ExceptionHandleSaveLogOnly(Sender: TObject; E: Exception);
procedure InitSimpleExceptionHandler(const LogFilename: String = '');
procedure DoneSimpleExceptionHandler;

var
  SimpleException: TSimpleException;

resourcestring
  SExceptionDialogTitle = 'Exception Info';
  SExceptionCaption = 'An error occured during program execution:';
  SButtonDetails = '&Show Details';
  SButtonTerminate = '&Terminate';
  SButtonContinue = '&Continue';
  SCheckBoxIgnoreException = 'Ignore this exception for next time';
  SCantHandleException = 'Can''t handle exception';

implementation

procedure SetMaxStackCount(const ACount : Integer);
begin
  if SimpleException <> nil then
    SimpleException.MaxStackCount := ACount;
end;

function AddIgnoredException(const EClassName : String) : Boolean;
begin
  Result := False;
  if SimpleException <> nil then
    if SimpleException.IgnoredExceptionList.IndexOf(EClassName) < 0 then
    begin
      Result := True;
      SimpleException.IgnoredExceptionList.Add(EClassName);
    end;
end;

function RemoveIgnoredClass(const EClassName : String) : Boolean;
begin
  Result := False;
  if SimpleException <> nil then
    if SimpleException.IgnoredExceptionList.IndexOf(EClassName) > -1 then
    begin
      Result := True;
      SimpleException.IgnoredExceptionList.Delete(
        SimpleException.IgnoredExceptionList.IndexOf(EClassName));
    end;
end;

procedure ClearIgnoredException;
begin
  if SimpleException <> nil then
    SimpleException.IgnoredExceptionList.Clear;
end;

procedure ExceptionHandle(Sender: TObject; E: Exception);
begin
  if not Assigned(SimpleException) then
    InitSimpleExceptionHandler;
  SimpleException.SimpleExceptionHandler(Sender, E);
end;

procedure ExceptionHandleSaveLogOnly(Sender: TObject; E: Exception);
begin
  if not Assigned(SimpleException) then
    InitSimpleExceptionHandler;
  SimpleException.SimpleExceptionHandlerSaveLogOnly(Sender, E);
end;

procedure InitSimpleExceptionHandler(const LogFilename : String);
begin
  if SimpleException = nil then
    SimpleException := TSimpleException.Create(LogFilename);
end;

procedure DoneSimpleExceptionHandler;
begin
  if SimpleException <> nil then
    FreeAndNil(SimpleException);
end;

{ TSimpleException }

procedure TSimpleException.SetMaxStackCount(AMaxStackCount: Integer);
begin
  if FMaxStackCount <> AMaxStackCount then
    if AMaxStackCount < 1 then
      FMaxStackCount := 1
    else
      FMaxStackCount := AMaxStackCount;
end;

function TSimpleException.OSVer: String;
{$IFDEF WINDOWS}
var
  wdir: array [0..MAX_PATH] of Char;
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
      Result := 'Windows';
  end;
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
  FrameCount: Longint; Frames: PCodePointer);
var
  i: Integer;
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
        'Exception Address : $' + SimpleBackTraceStr(Addr) + LineEnding;
      if FrameCount > 0 then
        for i := 0 to FrameCount-1 do
          FLastReport := FLastReport + '  ' + SimpleBackTraceStr(Frames[i]) + LineEnding;
      SaveLogToFile(FLastReport);
      CallExceptionHandler;
    end;
  finally
    LeaveCriticalSection(FSimpleCriticalSection);
  end;
end;

function TSimpleException.SimpleBackTraceStr(Addr: Pointer): String;
var
  func, Source: ShortString;
  hs: String[32];
  line: longint;
begin
  Result := '$' + hexStr(Addr);
  if FHasDebugLine then
  begin
    try
      GetLineInfo(PtrUInt(Addr), func, Source, line);
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

function TSimpleException.ExceptionHeaderMessage: string;
begin
  try
    Result := 'Program exception!' + LineEnding +
      'Application       : ' + Application.Title + LineEnding +
      'Version           : ' + FAppInfo_fileversion + LineEnding +
      'Product Version   : ' + FAppInfo_productversion + LineEnding +
      'FPC Version       : ' + {$i %FPCVERSION%} + LineEnding +
      'LCL Version       : ' + LCLVersion.lcl_version + LineEnding +
      'Target CPU_OS     : ' + {$i %FPCTARGETCPU%} +'_' + {$i %FPCTARGETOS%} +LineEnding +
      'Host Machine      : ' + FOSversion + LineEnding +
      'Path              : ' + ParamStrUTF8(0) + LineEnding +
      'Proccess Id       : ' + IntToStr(GetProcessID) + LineEnding +
      'Thread Id         : ' + IntToStr(GetThreadID) + LineEnding +
      'Time              : ' + DateTimeToStr(Now) + LineEnding;
    if IgnoredExceptionList.Count > 0 then
      Result := Result +
        'Ignored Exception : ' + IgnoredExceptionList.DelimitedText + LineEnding;
  except
    Result := '';
  end;
end;

procedure TSimpleException.CreateExceptionReport;
var
  i, maxStack: Integer;
  Report, StackTraceStr: String;
  cf, pcf, cAddress, cFrame: Pointer;
begin
  try
    if ExceptFrameCount > 0 then
    begin
      StackTraceStr :=
        'Exception Address : $' + hexStr(ExceptAddr) + LineEnding;
      if ExceptFrameCount > FMaxStackCount then
        maxStack := FMaxStackCount - 1
      else
        maxStack := ExceptFrameCount - 1;
      for i := 0 to maxStack do
        StackTraceStr := StackTraceStr + '  ' + SimpleBackTraceStr(ExceptFrames[i]) +
          LineEnding;
    end
    else
    begin
      //cf := get_caller_frame(get_frame);
      cf := get_caller_frame(get_caller_frame(get_frame));
      if cf <> nil then
      begin
        StackTraceStr :=
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
            if i > FMaxStackCount then
              Break;
            StackTraceStr := StackTraceStr + '  ' + SimpleBackTraceStr(cAddress) +
              LineEnding;
            pcf := cf;
            cf := cFrame;
          end;
        finally
        end;
      end;
    end;
    StackTraceStr := TrimRight(StackTraceStr) + LineEnding;

    Report := ExceptionHeaderMessage;
    if Assigned(FLastSender) then
      Report := Report +
        'Sender Class      : ' + FLastSender.ClassName + LineEnding;
    if Assigned(FLastException) then
    begin
      Report := Report +
        'Exception Class   : ' + FLastException.ClassName + LineEnding +
        'Message           : ' + FLastException.Message + LineEnding;
    end;
    FLastReport := Report + StackTraceStr;
  except
    FLastReport := 'Failed to create exception report!';
  end;
  SaveLogToFile(FLastReport);
end;

procedure TSimpleException.SaveLogToFile(LogMsg: string);
var
  f: TextFile;
begin
  if LogFilename <> '' then
  begin
    AssignFile(f, LogFilename);
    try
      if FileExistsUTF8(LogFilename) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, LogMsg);
    finally
      CloseFile(f);
    end;
  end;
  WriteLog_E('From ExceptionHandler:'#13#10+LogMsg);
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
      raise Exception.Create(SCantHandleException);
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

Procedure CatchUnhandledExcept(Obj : TObject; Addr: CodePointer; FrameCount: Longint; Frames: PCodePointer);
begin
  if Assigned(SimpleException) then
    SimpleException.UnhandledException(Obj, Addr, FrameCount, Frames);
end;

constructor TSimpleException.Create(Filename : string);
var
  i: Integer;
begin
  inherited Create;
  InitCriticalSection(FSimpleCriticalSection);
  FHasDebugLine := OpenSymbolFile(ParamStrUTF8(0));
  if Trim(Filename) <> '' then
    LogFilename := Filename
  else
    LogFilename := ExtractFileNameOnly(Application.ExeName) + '.log';
  FMaxStackCount := 20;
  FAppVerInfo := TStringList.Create;
  IgnoredExceptionList := TStringList.Create;
  FOSversion := OSVer;
  with TFileVersionInfo.Create(nil) do
    try
      try
        fileName := ParamStrUTF8(0);
        if fileName = '' then
          fileName := Application.ExeName;
        {$IF FPC_FULLVERSION >= 20701}
        ReadFileInfo;
        {$ENDIF}
        if VersionStrings.Count > 0 then
        begin
        {$IF FPC_FULLVERSION >= 20701}
          FAppVerInfo.Assign(VersionStrings);
        {$ELSE}
          for i := 0 to VersionStrings.Count - 1 do
            FAppVerInfo.Add(VersionCategories.Strings[i] + '=' +
              VersionStrings.Strings[i]);
        {$ENDIF}
          for i := 0 to FAppVerInfo.Count - 1 do
            FAppVerInfo.Strings[i] :=
              LowerCase(FAppVerInfo.Names[i]) + '=' + FAppVerInfo.ValueFromIndex[i];
          FAppInfo_comments := FAppVerInfo.Values['comments'];
          FAppInfo_companyname := FAppVerInfo.Values['companyname'];
          FAppInfo_filedescription := FAppVerInfo.Values['filedescription'];
          FAppInfo_fileversion := FAppVerInfo.Values['fileversion'];
          FAppInfo_internalname := FAppVerInfo.Values['internalname'];
          FAppInfo_legalcopyright := FAppVerInfo.Values['legalcopyright'];
          FAppInfo_legaltrademarks := FAppVerInfo.Values['legaltrademarks'];
          FAppInfo_originalfilename := FAppVerInfo.Values['originalfilename'];
          FAppInfo_productname := FAppVerInfo.Values['productname'];
          FAppInfo_productversion := FAppVerInfo.Values['productversion'];
        end;
      except
      end;
    finally
      Free;
    end;
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
  FAppVerInfo.Free;
  CloseSymbolFile;
  DoneCriticalsection(FSimpleCriticalSection);
  inherited Destroy;
end;

finalization
  DoneSimpleExceptionHandler;

end.
