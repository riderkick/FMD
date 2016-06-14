program md;

{$mode objfpc}{$H+}

{$DEFINE MANGADOWNLOADER}

uses
 {$IFDEF DEBUGLEAKS}
  SysUtils,
 {$ENDIF}
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils, IniFiles, simpleipc, sqlite3dyn, FMDOptions, uBaseUnit,
  frmMain;

var
  CheckInstance: Boolean = True;
  AllowedToRun: Boolean = True;

{$R *.res}

begin
  with TIniFile.Create(CONFIG_FILE) do
    try
      CheckInstance := ReadBool('general', 'OneInstanceOnly', True);
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
    sqlite3dyn.SQLiteDefaultLibrary := CleanAndExpandDirectory(GetCurrentDirUTF8) + 'sqlite3.dll';
    Application.Title := 'Free Manga Downloader';
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.
