{
        File: uSubThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uSubThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Forms, uBaseUnit, uFMDThread;

type

   TSubThread = class;

  { TCheckUpdateThread }

  TCheckUpdateThread = class(TFMDThread)
  protected
    FHTTP: THTTPSendThread;
    fNewVersionNumber,
    fUpdateURL,
    fChangelog,
    FBtnCheckCaption: String;
    procedure MainThreadUpdate;
    procedure SyncStartUpdate;
    procedure SyncEndUpdate;
    procedure Execute; override;
  public
    Manager: TSubThread;
    constructor Create;
    destructor Destroy; override;
  end;

  { TSubThread }
  TSubThread = class(TFMDThread)
  protected
    FCheckUpdateThread: TCheckUpdateThread;
    procedure Execute; override;
  public
    CheckUpdate: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AbortCheckUpdate;
  end;
  
resourcestring
  RS_NewVersionFound = 'New Version found!';
  RS_CurrentVersion = 'Installed Version';
  RS_LatestVersion = 'Latest Version   ';
  RS_BtnCheckUpdates = 'Check for new version';

implementation

uses
  frmMain, frmUpdateDialog;

{ TCheckUpdateThread }

procedure TCheckUpdateThread.MainThreadUpdate;
begin
  if IsDlgCounter then Exit;
  with TUpdateDialogForm.Create(MainForm) do try
    Caption := Application.Title + ' - ' + RS_NewVersionFound;
    with mmLog.Lines do
    begin
      BeginUpdate;
      try
        Clear;
        Add(RS_CurrentVersion + ' : ' + FMD_VERSION_NUMBER);
        Add(RS_LatestVersion + ' : ' + fNewVersionNumber + LineEnding);
        AddText(fChangelog);
      finally
        EndUpdate;
      end;
    end;
    if ShowModal = mrYes then
    begin
      frmMain.FUpdateURL := fUpdateURL;
      DoAfterFMD := DO_UPDATE;
      MainForm.itMonitor.Enabled := True;
    end
    else
      MainForm.btCheckVersion.Caption := RS_BtnCheckUpdates;
  finally
    Free;
  end;
end;

procedure TCheckUpdateThread.SyncStartUpdate;
begin
  with MainForm.btCheckVersion do
  begin
    MainForm.btAbortCheckVersion.Visible := True;
    Width := Width - MainForm.btAbortCheckVersion.Width - 6;
    Caption := RS_Checking;
  end;
end;

procedure TCheckUpdateThread.SyncEndUpdate;
begin
  with MainForm.btCheckVersion do
  begin
    MainForm.btAbortCheckVersion.Visible := False;
    Width := Width + MainForm.btAbortCheckVersion.Width + 6;
    Caption := RS_BtnCheckUpdates;
  end;
end;

procedure TCheckUpdateThread.Execute;
var
  l: TStringList;
  updateFound: Boolean = False;
begin
  Synchronize(SyncStartUpdate);
  l := TStringList.Create;
  try
    fNewVersionNumber := FMD_VERSION_NUMBER;
    fUpdateURL := '';
    if not Terminated and
      GetPage(FHTTP, TObject(l), UPDATE_URL + 'update', 3) then
      if l.Count > 1 then
      begin
        l.NameValueSeparator := '=';
        if Trim(l.Values['VERSION']) <> FMD_VERSION_NUMBER then
        begin
          fNewVersionNumber := Trim(l.Values['VERSION']);
          fUpdateURL := Trim(l.Values[UpperCase(FMD_TARGETOS)]);
          if fUpdateURL <> '' then
            updateFound := True;
          FHTTP.Clear;
          l.Clear;
          if not Terminated and
            GetPage(FHTTP, TObject(l), UPDATE_URL + 'changelog.txt', 3) then
            fChangelog := l.Text;
        end;
    end;
  finally
    l.Free;
  end;
  Synchronize(SyncEndUpdate);
  if not Terminated and updateFound and (not isDlgCounter) then
    Synchronize(MainThreadUpdate);
end;

constructor TCheckUpdateThread.Create;
begin
  inherited Create(True);
  FHTTP := THTTPSendThread.Create(Self);
end;

destructor TCheckUpdateThread.Destroy;
begin
  FHTTP.Free;
  Manager.CheckUpdate := False;
  Manager.FCheckUpdateThread := nil;
  inherited Destroy;
end;

{ TSubThread }

procedure TSubThread.Execute;
begin
  MainForm.isSubthread := True;
  try
    if FileExists(fmdDirectory + 'old_updater.exe') then
      DeleteFile(fmdDirectory + 'old_updater.exe');

    if OptionAutoCheckFavStartup then
    begin
      MainForm.FavoriteManager.isAuto := True;
      MainForm.FavoriteManager.CheckForNewChapter;
    end;

    while not Terminated do
    begin
      if CheckUpdate and (FCheckUpdateThread = nil) and
        (not isDlgCounter) then
      begin
        FCheckUpdateThread := TCheckUpdateThread.Create;
        FCheckUpdateThread.Manager := Self;
        FCheckUpdateThread.Start;
      end;

      with MainForm do
      begin
        while (SilentThreadManager.MetaData.Count > 0) and
          (SilentThreadManager.Threads.Count < DLManager.maxDLThreadsPerTask) do
          SilentThreadManager.CheckOut;
      end;
      Sleep(500);
    end;
    if Assigned(FCheckUpdateThread) then
    begin
      FCheckUpdateThread.Terminate;
      FCheckUpdateThread.WaitFor;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

constructor TSubThread.Create;
begin
  inherited Create(True);
  CheckUpdate := False;
end;

destructor TSubThread.Destroy;
begin
  MainForm.isSubthread := False;
  inherited Destroy;
end;

procedure TSubThread.AbortCheckUpdate;
begin
  if Assigned(FCheckUpdateThread) then
  begin
    FCheckUpdateThread.Terminate;
    FCheckUpdateThread.WaitFor;
  end;
end;

end.
