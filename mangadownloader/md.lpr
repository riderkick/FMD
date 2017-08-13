program md;

{$mode objfpc}{$H+}

{$DEFINE MANGADOWNLOADER}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils, IniFiles, simpleipc, sqlite3dyn, FMDOptions, uBaseUnit,
  FMDVars, SimpleException, Classes, windows, sysutils, frmMain, MultiLog,
  FileChannel;

var
  CheckInstance: Boolean = True;
  AllowedToRun: Boolean = True;
  EnableLogging: Boolean = False;
  LogFileName: String = '';
  s: TStringList;

{$R *.res}

begin
  with TIniFile.Create(CONFIG_FILE) do
    try
      CheckInstance := ReadBool('general', 'OneInstanceOnly', True);
      EnableLogging := ReadBool('logger', 'Enabled', False);
      if EnableLogging then
      begin
        LogFileName := ReadString('logger', 'LogFileName', '');
        if LogFileName = '' then
          LogFileName := ChangeFileExt(Application.ExeName, '.log');
      end;
    finally
      Free;
    end;

  if CheckInstance then
  begin
    with TSimpleIPCClient.Create(nil) do
      try
        ServerID := FMD_INSTANCE;
        if ServerRunning then
        begin
          AllowedToRun := False;
          Active := True;
          SendStringMessage('BringToFront');
        end;
      finally
        Free;
      end;
  end;

  if AllowedToRun then
  begin
    {$IFDEF DEBUGLEAKS}
    if FileExistsUTF8(ChangeFileExt(Application.ExeName, '.trc')) then
      DeleteFileUTF8(ChangeFileExt(Application.ExeName, '.trc'));
    SetHeapTraceOutput(ChangeFileExt(Application.ExeName, '.trc'));
    {$ENDIF DEBUGLEAKS}
    Application.Title := 'Free Manga Downloader';
    RequireDerivedFormResource := True;
    Logger.Enabled := False;
    InitSimpleExceptionHandler(LogFileName);
    if EnableLogging then
    begin
      Logger.Enabled := True;
      if MainExceptionHandler.LogFileOK then
      begin
        FileLogger := TFileChannel.Create(LogFileName, [fcoShowHeader, fcoShowPrefix, fcoShowTime]);
        Logger.Channels.Add(FileLogger);
        Logger.Send(QuotedStrd(Application.Title)+' started with [PID:'+IntToStr(GetProcessID)+'] [HANDLE:'+IntToStr(GetCurrentProcess)+']');
      end;
      s := TStringList.Create;
      try
        s.AddText(SimpleException.GetApplicationInfo);
        Logger.Send('Application info', s);
      finally
        s.Free;
      end;
    end;
    sqlite3dyn.SQLiteDefaultLibrary := CleanAndExpandDirectory(GetCurrentDirUTF8) + 'sqlite3.dll';
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.
