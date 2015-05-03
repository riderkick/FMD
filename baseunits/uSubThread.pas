{
        File: uSubThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uSubThread;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, uBaseUnit, uFMDThread,
  httpsend, blcksock;

type

  { TSubThread }

  TSubThread = class(TFMDThread)
  protected
    FIsLoaded: Integer;
    FBtnCheckCaption: String;
    procedure Execute; override;
    procedure CheckLatestVersion;
    procedure MainThreadUpdate;
    procedure MainThreadSetButton;
    procedure SockOnHeartBeat(Sender: TObject);
  public
    updateCounter: Cardinal;
    isCheckForLatestVer: Boolean;
    isCanStop: Boolean;
    fNote, fNoteForThisRevision, fImportant: String;
    fNewVersionNumber, fUpdateURL, fChangelog: String;
    constructor Create;
    destructor Destroy; override;
  end;
  
resourcestring
  RS_NewVersionFound = 'New Version found!';
  RS_CurrentVersion = 'Installed Version';
  RS_LatestVersion = 'Latest Version   ';

implementation

uses
  frmMain, frmUpdateDialog;

// ----- Public methods -----

constructor TSubThread.Create;
begin
  inherited Create(True);
  fImportant := '';
  updateCounter := 1;
  isCheckForLatestVer := False;
end;

destructor TSubThread.Destroy;
begin
  MainForm.isSubthread := False;
  inherited Destroy;
end;

// ----- Protected methods -----

procedure TSubThread.MainThreadUpdate;
begin
  UpdateDialogForm.Caption := Application.Title + ' - ' + RS_NewVersionFound;
  with UpdateDialogForm.mmLog.Lines do
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
  if UpdateDialogForm.ShowModal = mrYes then
  begin
    MainForm.DoUpdateFMD := True;
    MainForm.FUpdateURL := fUpdateURL;
    MainForm.itMonitor.Enabled := True;
  end
  else
    MainForm.btCheckVersion.Caption := stUpdaterCheck;
end;

procedure TSubThread.MainThreadSetButton;
begin
  MainForm.btCheckVersion.Caption := FBtnCheckCaption;
end;

procedure TSubThread.SockOnHeartBeat(Sender: TObject);
begin
  if Terminated then
  begin
    TBlockSocket(Sender).Tag := 1;
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
end;

procedure TSubThread.Execute;
begin
  MainForm.isSubthread := True;
  try
    if FileExists(fmdDirectory + 'old_updater.exe') then
      DeleteFile(fmdDirectory + 'old_updater.exe');

    if OptionAutoCheckFavStartup then
    begin
      MainForm.favorites.isAuto := True;
      MainForm.favorites.isShowDialog := MainForm.cbOptionShowFavoriteDialog.Checked;
      MainForm.favorites.Run;
    end;

    while not Terminated do
    begin
      if isCheckForLatestVer then
        CheckLatestVersion;

      with MainForm do
      begin
        while (SilentThreadManager.MetaData.Count > 0) and
          (SilentThreadManager.Threads.Count < DLManager.maxDLThreadsPerTask) do
          SilentThreadManager.CheckOut;
      end;
      Sleep(500);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSubThread.CheckLatestVersion;
var
  l: TStringList;
  FHTTP: THTTPSend;
  updateFound: Boolean = False;
begin
  fNewVersionNumber := FMD_VERSION_NUMBER;
  fUpdateURL := '';
  FBtnCheckCaption := stFavoritesChecking;
  Synchronize(@MainThreadSetButton);
  l := TStringList.Create;
  FHTTP := THTTPSend.Create;
  try
    FHTTP.Sock.OnHeartbeat := @SockOnHeartBeat;
    FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
    if not Terminated and
      GetPage(Self, FHTTP, TObject(l), UPDATE_URL + 'update', 3, False) then
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
            GetPage(Self, FHTTP, TObject(l), UPDATE_URL + 'changelog.txt', 3, False) then
            fChangelog := l.Text;

        end;
      FBtnCheckCaption := stUpdaterCheck;
      Synchronize(@MainThreadSetButton);
    end;
  finally
    FHTTP.Free;
    l.Free;
  end;
  if not Terminated and updateFound then
    Synchronize(@MainThreadUpdate);

  Inc(updateCounter);
  isCheckForLatestVer := False;
end;

end.
