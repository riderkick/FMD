unit SelfUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, BaseThread, FMDOptions, process, ComCtrls, Controls,
  Dialogs, StdCtrls, Buttons, Forms, blcksock;

type

  { TSelfUpdaterThread }

  TSelfUpdaterThread = class(TBaseThread)
  private
    FStatusBar: TStatusBar;
    FProgressBar: TProgressBar;
    FButtonCancel: TSpeedButton;
    FHTTP: THTTPSendThread;
    FTotalSize: Integer;
    FCurrentSize: Integer;
    FFailedMessage: String;
    FStatusText: String;
  protected
    procedure ButtonCancelClick(Sender: TObject);
    procedure HTTPSockOnStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
  protected
    procedure SyncStart;
    procedure SyncFinal;
    procedure SyncStartDownload;
    procedure SyncUpdateProgress;
    procedure SyncUpdateStatus;
    procedure SyncShowFailed;
    procedure SyncFinishRestart;
    procedure ProceedUpdate;
    procedure UpdateStatusText(const S: String);
    procedure Execute; override;
  public
    UpdateURL: String;
    NewVersionString: String;
    Filename: String;
    DownloadSuccess: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_Downloading = 'Downloading new version %s';
  RS_FailedTitle = 'Failed';
  RS_FailedDownload = 'Failed to download new version %s'#13#10#13#10'%d %s';
  RS_FailedToSave = 'Failed to save %s';
  RS_MissingZipExe = 'Missing %s';
  RS_FailedExtract = 'Failed to extract %s, exitstatus = %d';
  RS_ButtonCancel = 'Abort';
  RS_FinishRestartTitle = 'Download finished';
  RS_FinishRestart = 'Download update package finished, restart to proceed?';

implementation

uses FMDVars;

{ TSelfUpdaterThread }

procedure TSelfUpdaterThread.ButtonCancelClick(Sender: TObject);
begin
  Self.Terminate;
end;

procedure TSelfUpdaterThread.HTTPSockOnStatus(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
begin
  if Terminated then
    Exit;
  if Reason = HR_ReadCount then
  begin
    if FTotalSize = 0 then
      FTotalSize := StrToIntDef(Trim(FHTTP.Headers.Values['Content-Length']), 0);
    Inc(FCurrentSize, StrToInt(Value));
    Synchronize(@SyncUpdateProgress);
  end
  else
  if Reason = HR_Connect then
  begin
    FCurrentSize := 0;
    FTotalSize := 0;
  end;
end;

procedure TSelfUpdaterThread.SyncStart;
begin
  SelfUpdaterThread := Self;

  FStatusBar := TStatusBar.Create(FormMain);
  with FStatusBar do
  begin
    Parent := FormMain;
    SimplePanel := False;
    with Panels.Add do        // panel for progress bar
      Width := 100;
    Panels.Add;               // panel for progress text
    Panels.Add;               // panel for status text
  end;

  FProgressBar := TProgressBar.Create(FormMain);
  with FProgressBar do
  begin
    Parent := FStatusBar;
    Align := alNone;
    Smooth := True;
    Style := pbstNormal;
    Min := 0;
    Width := FStatusBar.Panels[0].Width - 10;
    Anchors := [akTop, akLeft, akBottom];
    AnchorSideTop.Control := FStatusBar;
    AnchorSideTop.Side := asrTop;
    AnchorSideLeft.Control := FStatusBar;
    AnchorSideLeft.Side := asrTop;
    AnchorSideBottom.Control := FStatusBar;
    AnchorSideBottom.Side := asrBottom;
    BorderSpacing.Top := 2;
    BorderSpacing.Left := 5;
    BorderSpacing.Bottom := 2;
  end;

  FButtonCancel := TSpeedButton.Create(FormMain);
  with FButtonCancel do
  begin
    Parent := FStatusBar;
    Align := alNone;
    AutoSize := True;
    Caption := RS_ButtonCancel;
    ShowCaption := True;
    Flat := True;
    Anchors := [akTop, akRight, akBottom];
    AnchorSideTop.Control := FStatusBar;
    AnchorSideTop.Side := asrTop;
    AnchorSideRight.Control := FStatusBar;
    AnchorSideRight.Side := asrRight;
    AnchorSideBottom.Control := FStatusBar;
    AnchorSideBottom.Side := asrBottom;
    BorderSpacing.Top := 2;
    BorderSpacing.Right := 5;
    BorderSpacing.Bottom := 2;
    OnClick := @ButtonCancelClick;
  end;
end;

procedure TSelfUpdaterThread.SyncFinal;
begin
  FHTTP.Sock.OnStatus := nil;
  FreeAndNil(FStatusBar);
  FreeAndNil(FProgressBar);
  FreeAndNil(FButtonCancel);
  SelfUpdaterThread := nil;
end;

procedure TSelfUpdaterThread.SyncStartDownload;
begin
  FCurrentSize := 0;
  FTotalSize := 0;
  FProgressBar.Max := 0;
  FProgressBar.Position := 0;
  FStatusBar.Panels[1].Text := '';
  FStatusBar.Panels[1].Width := 0;
  SyncUpdateStatus;
end;

procedure TSelfUpdaterThread.SyncUpdateProgress;
var
  s: String;
begin
  if FStatusBar = nil then
    Exit;
  if FProgressBar.Max <> FTotalSize then
    FProgressBar.Max := FTotalSize;
  if FProgressBar.Position <> FCurrentSize then
    FProgressBar.Position := FCurrentSize;

  s := FormatByteSize(FCurrentSize);
  if FTotalSize <> 0 then
    s += '/' + FormatByteSize(FTotalSize);
  FStatusBar.Panels[1].Width := FStatusBar.Canvas.TextWidth(s) + 10;
  FStatusBar.Panels[1].Text := s;
end;

procedure TSelfUpdaterThread.SyncUpdateStatus;
begin
  FStatusBar.Panels[2].Text := FStatusText;
end;

procedure TSelfUpdaterThread.SyncShowFailed;
begin
  MessageDlg(RS_FailedTitle, FFailedMessage, mtError, [mbOK], 0);
end;

procedure TSelfUpdaterThread.SyncFinishRestart;
begin
  if MessageDlg(RS_FinishRestartTitle, RS_FinishRestart, mtConfirmation,
    mbYesNo, 0) = mrYes then
    ProceedUpdate;
end;

procedure TSelfUpdaterThread.ProceedUpdate;
begin
  if DownloadSuccess then
  begin
    if FileExists(OLD_CURRENT_UPDATER_EXE) then
      DeleteFile(OLD_CURRENT_UPDATER_EXE);
    if FileExists(CURRENT_UPDATER_EXE) then
      RenameFile(CURRENT_UPDATER_EXE, OLD_CURRENT_UPDATER_EXE);
    if not FileExists(OLD_CURRENT_UPDATER_EXE) then
      Exit;
    with TProcess.Create(nil) do
      try
        InheritHandles := False;
        CurrentDirectory := FMD_DIRECTORY;
        Executable := OLD_CURRENT_UPDATER_EXE;
        Parameters.Add(Application.ExeName);
        Parameters.Add(CURRENT_ZIP_EXE);
        Parameters.Add(Self.Filename);
        Parameters.Add(FMD_DIRECTORY);
        Execute;
      finally
        Free;
      end;
    DoAfterFMD := DO_UPDATE;
    FormMain.tmExitCommand.Interval := 32;
    FormMain.tmExitCommand.Enabled := True;
  end;
end;

procedure TSelfUpdaterThread.UpdateStatusText(const S: String);
begin
  if FStatusText = S then
    Exit;
  FStatusText := S;
  Synchronize(@SyncUpdateStatus);
end;

procedure TSelfUpdaterThread.Execute;
begin
  DownloadSuccess := False;
  if UpdateURL = '' then
    Exit;
  try
    FStatusText := Format(RS_Downloading, [UpdateURL]);
    Synchronize(@SyncStartDownload);
    if FHTTP.GET(UpdateURL) and (FHTTP.ResultCode < 300) then // should be success
    begin
      DownloadSuccess := True;
      Filename := FMD_DIRECTORY + UPDATE_PACKAGE_NAME;
      if FileExists(Filename) then
        DeleteFile(Filename);
      if not FileExists(Filename) then
        FHTTP.Document.SaveToFile(Filename);

      if FileExists(Filename) then
        DeleteFile(Filename);
      if not FileExists(Filename) then
      begin
        FHTTP.Document.SaveToFile(Filename);
        if not FileExists(Filename) then
        begin
          FFailedMessage := Format(RS_FailedToSave, [Filename]);
          DownloadSuccess := False;
        end;
      end
      else
      begin
        FFailedMessage := Format(RS_FailedToSave, [Filename]);
        DownloadSuccess := False;
      end;

      if DownloadSuccess and (not FileExists(CURRENT_ZIP_EXE)) then
      begin
        FFailedMessage := Format(RS_MissingZipExe, [CURRENT_ZIP_EXE]);
        DownloadSuccess := False;
      end;
    end
    else
      FFailedMessage := Format(RS_FailedDownload, [NewVersionString,
        FHTTP.ResultCode, FHTTP.ResultString]);
  except
    on E: Exception do
      FFailedMessage := E.Message;
  end;
end;

constructor TSelfUpdaterThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFailedMessage := '';
  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.UserAgent := UserAgentCURL;
  FHTTP.Sock.OnStatus := @HTTPSockOnStatus;
  Synchronize(@SyncStart);
end;

destructor TSelfUpdaterThread.Destroy;
begin
  if (not Terminated) and (FFailedMessage <> '') then
    Synchronize(@SyncShowFailed)
  else
  if DownloadSuccess then
    Synchronize(@SyncFinishRestart);
  Synchronize(@SyncFinal);
  FHTTP.Free;
  inherited Destroy;
end;

end.
