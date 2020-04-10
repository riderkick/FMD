program md;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
 {$ifdef windows}
  windows,
 {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils, IniFiles, simpleipc, sqlite3dyn, FMDOptions, uBaseUnit,
  FMDVars, webp, CheckUpdate, DBUpdater, SelfUpdater, uDownloadsManager,
  LuaWebsiteModules, SimpleException, Classes, sysutils, frmMain, MultiLog,
  FileChannel, ssl_openssl_lib, blcksock, ssl_openssl;

var
  CheckInstance: Boolean = True;
  AllowedToRun: Boolean = True;
  EnableLogging: Boolean = False;
  LogFileName: String = '';
  s: TStringList;
  {$IFDEF DEBUGLEAKS}
  trcfile: String;
  {$ENDIF DEBUGLEAKS}
  i: Integer;

{$ifdef windows}
  doRestartHandle: THandle;
  doRestartHandleWaitCounter:Integer=0;
  evpathlen: Integer;
  evpath: String;

const
  {$ifdef win64}
  OpenSSLDLLSSLName='libssl-1_1-x64.dll';
  OpenSSLDLLUtilName='libcrypto-1_1-x64.dll';
  {$else}
  OpenSSLDLLSSLName='libssl-1_1.dll';
  OpenSSLDLLUtilName='libcrypto-1_1.dll';
  {$endif}
{$endif}

{$R *.res}

begin
  // read and save all params
  for i:=1 to ParamCount do
    AppParams.Add(ParamStr(i));

  {$ifdef windows}
  //wait for prev process from dorestart
  doRestartHandle:=THandle(-1);
  doRestartHandle:=THandle(StrToIntDef(AppParams.Values['--dorestart-handle'],-1));
  if Integer(doRestartHandle)<>-1 then
  begin
    // remove previous --dorestart-handle from params
    AppParams.Delete(AppParams.IndexOfName('--dorestart-handle'));
    while IsWindow(doRestartHandle) do
    begin
      Sleep(250);
      inc(doRestartHandleWaitCounter,250);
      // if previous handle takes longer than 10s, we give up
      // todo: do something if app takes longer to close
      if doRestartHandleWaitCounter>10000 then Exit;
    end;
  end;
  {$endif}

  // always execute lua modules from file, for dev purpose
  if AppParams.IndexOf('--lua-dofile')<>-1 then
    LuaWebsiteModules.AlwaysLoadLuaFromFile:=True;

  with TIniFile.Create(CONFIG_FILE) do
    try
      CheckInstance := ReadBool('general', 'OneInstanceOnly', True);
      EnableLogging := ReadBool('logger', 'Enabled', False);
      if EnableLogging then
        LogFileName := ExpandFileNameUTF8(ReadString('logger', 'LogFileName', DEFAULT_LOG_FILE), FMD_DIRECTORY);
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
  if not AllowedToRun then Exit;

  {$IFDEF DEBUGLEAKS}
  trcfile := FMD_DIRECTORY + FMD_EXENAME + '.trc';
  if FileExistsUTF8(trcfile) then
    DeleteFileUTF8(trcfile);
  SetHeapTraceOutput(trcfile);
  {$ENDIF DEBUGLEAKS}

  {$ifdef windows}
  // set environment variables
  evpathlen:=windows.GetEnvironmentVariable('PATH',nil,0);
  setlength(evpath,evpathlen-1);
  windows.GetEnvironmentVariable('PATH',pchar(evpath),evpathlen);
  evpath:=FMD_DIRECTORY+';'+evpath;
  windows.SetEnvironmentVariable('PATH',pchar(evpath));
  {$endif}

  Application.Title := 'Free Manga Downloader';
  RequireDerivedFormResource:=True;
  Logger.ThreadSafe:=True;
  Logger.Enabled:=EnableLogging;
  InitSimpleExceptionHandler(LogFileName);
  if EnableLogging then
  begin
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

  //sqlite
  if FileExists(FMD_DIRECTORY + Sqlite3Lib) then
    SQLiteDefaultLibrary := FMD_DIRECTORY + Sqlite3Lib;
  {$ifdef windows}
  //openssl
  if IsSSLloaded then
    DestroySSLInterface;
  if FileExists(FMD_DIRECTORY+OpenSSLDLLSSLName) and FileExists(FMD_DIRECTORY+OpenSSLDLLUtilName) then
  begin
    DLLSSLName:=FMD_DIRECTORY+OpenSSLDLLSSLName;
    DLLUtilName:=FMD_DIRECTORY+OpenSSLDLLUtilName;
    if InitSSLInterface then
      SSLImplementation := TSSLOpenSSL;
  end
  else if FileExists(FMD_DIRECTORY+DLLSSLName) and FileExists(FMD_DIRECTORY+DLLUtilName) then
  begin
    DLLSSLName:=FMD_DIRECTORY+DLLSSLName;
    DLLUtilName:=FMD_DIRECTORY+DLLUtilName;
  end;
  if not IsSSLloaded then
    InitSSLInterface;
  {$endif}
  //webp
  if FileExists(FMD_DIRECTORY + DLLWebPName) then
    DLLWebPName := FMD_DIRECTORY + DLLWebPName;

  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
