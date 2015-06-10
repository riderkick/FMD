program md;

{$mode objfpc}{$H+}

{$DEFINE MANGADOWNLOADER}

uses
  {$IFDEF DEBUGLEAKS}
  heaptrc, SysUtils,
  {$ENDIF}
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  {$ENDIF}
  Forms, Interfaces, FileUtil, simpleipc, IniFiles, uBaseUnit, frmMain;

var
  CheckInstance: Boolean = True;
  AllowedToRun: Boolean = True;

{$R *.res}

begin
  with TIniFile.Create(CorrectFilePath(GetCurrentDirUTF8) + CONFIG_FOLDER + CONFIG_FILE) do
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
    if FileExists(ChangeFileExt(Application.ExeName, '.trc')) then
      DeleteFile(ChangeFileExt(Application.ExeName, '.trc'));
    SetHeapTraceOutput(ChangeFileExt(Application.ExeName, '.trc'));
    {$ENDIF DEBUGLEAKS}
    Application.Title := 'Free Manga Downloader';
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.
