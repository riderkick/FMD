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
  { Tasks will be done by this thread:
    - Calls TGetMangaInfosThread for getting manga information.
    - Auto check for new version
    - [SilentThread] Pop metadata from queue and execute it
  }

  { TSubThread }

  TSubThread = class(TFMDThread)
  protected
    FIsLoaded: Integer;
    FBtnCheckCaption: String;
    procedure Execute; override;
    procedure MainThreadUpdate;
    procedure MainThreadSetButton;
    procedure MainThreadPopSilentThreadQueue;
    procedure DoTerminate; override;
    procedure SockOnHeartBeat(Sender: TObject);
  public
    updateCounter: Cardinal;
    isCheckForLatestVer: Boolean;
    isCanStop: Boolean;
    fNote, fNoteForThisRevision, fImportant: String;
    fNewVersionNumber, fUpdateURL, fChangelog: String;
    constructor Create;
  end;

implementation

uses
  frmMain, frmUpdateDialog,
  uSilentThread;

// ----- Public methods -----

constructor TSubThread.Create;
begin
  inherited Create(True);
  fImportant := '';
  updateCounter := 1;
  isCheckForLatestVer := False;
  isCanStop := False;
end;

// ----- Protected methods -----

procedure TSubThread.MainThreadUpdate;
begin
  UpdateDialogForm.Caption := Application.Title + ' - New version found!';
  with UpdateDialogForm.mmLog.Lines do
  begin
    BeginUpdate;
    try
      Clear;
      Add('Current Version : ' + FMD_VERSION_NUMBER);
      Add('New Version     : ' + fNewVersionNumber + LineEnding);
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

procedure TSubThread.MainThreadPopSilentThreadQueue;
var
  meta: TSilentThreadMetaData;
begin
  if MainForm.SilentThreadQueue.Count = 0 then
    Exit;
  meta := TSilentThreadMetaData(MainForm.SilentThreadQueue.Pop);
  if meta <> nil then
  begin
    meta.Run;
    meta.Free;
  end;
end;

procedure TSubThread.DoTerminate;
begin
  MainForm.isSubthread := False;
  inherited DoTerminate;
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
var
  l: TStringList;
  FHTTP: THTTPSend;
  updateFound: Boolean = False;
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
      if (MainForm.silentThreadCount > 0) and
        (MainForm.currentActiveSilentThreadCount < maxActiveSilentThread) then
      begin
        Synchronize(@MainThreadPopSilentThreadQueue);
      end;

      if isCheckForLatestVer then
      begin
        fNewVersionNumber := FMD_VERSION_NUMBER;
        fUpdateURL := '';
        FBtnCheckCaption := stFavoritesChecking;
        Synchronize(@MainThreadSetButton);
        l := TStringList.Create;
        FHTTP := THTTPSend.Create;
        FHTTP.Sock.OnHeartbeat := @SockOnHeartBeat;
        FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
        try
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

      isCanStop := False;
      isCanStop := True;
      Sleep(700);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
