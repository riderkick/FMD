program md;

{$mode objfpc}{$H+}

{$DEFINE MANGADOWNLOADER}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
 {$ifdef windows}
  windows,
 {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils, IniFiles, simpleipc, sqlite3dyn, FMDOptions, uBaseUnit, FMDVars, webp,
  LuaWebsiteModules, SimpleException, Classes, sysutils, frmMain, MultiLog,
  FileChannel, ssl_openssl_lib;

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
  p: String;

{$ifdef windows}
  evpathlen: Integer;
  evpath: String;

type
  TOpenSSLVersion=function (t:integer):PAnsiChar;cdecl;

var
  _OpenSSLVersion:TOpenSSLVersion=nil;

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
  Application.Title:='Free Manga Downloader';
  RequireDerivedFormResource := True;

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
    {$ifdef windows}
    // set environment variables
    evpathlen:=windows.GetEnvironmentVariable('PATH',nil,0);
    setlength(evpath,evpathlen-1);
    windows.GetEnvironmentVariable('PATH',pchar(evpath),evpathlen);
    evpath:=FMD_DIRECTORY+';'+evpath;
    windows.SetEnvironmentVariable('PATH',pchar(evpath));
    {$endif}

    for i := 1 to ParamCount do
    begin
      p := AnsiLowerCase(ParamStr(i));
      if p = '--lua-dofile' then
         LuaWebsiteModules.AlwaysLoadLuaFromFile := True;
    end;

    Application.Scaled := True;
    with TIniFile.Create(CONFIG_FILE) do
      try
        CheckInstance := ReadBool('general', 'OneInstanceOnly', True);
        EnableLogging := ReadBool('logger', 'Enabled', False);
        if EnableLogging then
          LogFileName := ExpandFileNameUTF8(ReadString('logger', 'LogFileName', DEFAULT_LOG_FILE), FMD_DIRECTORY);
      finally
        Free;
      end;

    {$IFDEF DEBUGLEAKS}
    trcfile := FMD_DIRECTORY + FMD_EXENAME + '.trc';
    if FileExistsUTF8(trcfile) then
      DeleteFileUTF8(trcfile);
    SetHeapTraceOutput(trcfile);
    {$ENDIF DEBUGLEAKS}

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
        _OpenSSLVersion:=TOpenSSLVersion(GetProcAddress(SSLUtilHandle,PChar('OpenSSL_version')));
      if Assigned(_OpenSSLVersion) then
        OpenSSLVersion:=PAnsiChar(_OpenSSLVersion(0));
    end else
    if FileExists(FMD_DIRECTORY+DLLSSLName) and FileExists(FMD_DIRECTORY+DLLUtilName) then
    begin
      DLLSSLName:=FMD_DIRECTORY+DLLSSLName;
      DLLUtilName:=FMD_DIRECTORY+DLLUtilName;
      if InitSSLInterface then
        OpenSSLVersion:=SSLeayversion(0);
    end;
    if not IsSSLloaded then
      InitSSLInterface;
    {$endif}
    //webp
    if FileExists(FMD_DIRECTORY + DLLWebPName) then
      DLLWebPName := FMD_DIRECTORY + DLLWebPName;

    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.
