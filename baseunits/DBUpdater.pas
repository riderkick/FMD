unit DBUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, BaseThread, FMDOptions, process, ComCtrls,
  Controls, Dialogs, StdCtrls, Buttons, blcksock;

type

  { TDBUpdaterThread }

  TDBUpdaterThread = class(TBaseThread)
  private
    FStatusBar: TStatusBar;
    FProgressBar: TProgressBar;
    FButtonCancel: TSpeedButton;
    FHTTP: THTTPSendThread;
    FTotalSize: Integer;
    FCurrentSize: Integer;
    FCurrentName: String;
    FFailedList: TStringList;
    FCurrentId: Integer;
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
    procedure SyncUpdateHint;
    procedure SyncShowFailed;
    procedure SyncCloseUsed;
    procedure SyncReopenUsed;
    procedure SyncRemoveAttached;
    procedure UpdateStatusText(const S: String);
    procedure Execute; override;
  public
    Items: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Add(const S: String); overload;
    procedure Add(const S: TStrings); overload;
    procedure UpdateStatus;    // should be called from mainthread
  end;

resourcestring
  RS_Downloading = 'Downloading %s';
  RS_FailedItemsTitle = 'Failed';
  RS_FailedItems = 'Failed to finish:'#13#10#13#10'%s';
  RS_FailedDownload = '%s: %d %s';
  RS_FailedToSave = '%s: failed to save';
  RS_MissingZipExe = '%s: Missing %s';
  RS_Extracting = 'Extracting %s';
  RS_FailedExtract = '%s: failed to extract, exitstatus = %d';
  RS_ButtonCancel = 'Abort';

implementation

uses FMDVars, SourceForge;

function GetDBURL(const AName: String): String;
begin
  if Pos('<website>', AnsiLowerCase(DB_URL)) <> -1 then
    Result := StringReplace(DB_URL, '<website>', AName, [rfIgnoreCase, rfReplaceAll])
  else
    Result := AName;
end;

{ TDBUpdaterThread }

procedure TDBUpdaterThread.ButtonCancelClick(Sender: TObject);
begin
  Self.Terminate;
end;

procedure TDBUpdaterThread.HTTPSockOnStatus(Sender: TObject;
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

procedure TDBUpdaterThread.SyncStart;
begin
  DBUpdaterThread := Self;

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

procedure TDBUpdaterThread.SyncFinal;
begin
  DBUpdaterThread := nil;
  FHTTP.Sock.OnStatus := nil;
  FreeAndNil(FStatusBar);
  FreeAndNil(FProgressBar);
  FreeAndNil(FButtonCancel);
end;

procedure TDBUpdaterThread.SyncStartDownload;
begin
  FCurrentSize := 0;
  FTotalSize := 0;
  FProgressBar.Max := 0;
  FProgressBar.Position := 0;
  FStatusBar.Panels[1].Text := '';
  FStatusBar.Panels[1].Width := 0;
  FStatusBar.Panels[2].Text := Format('[%d/%d] ' + RS_Downloading,
    [FCurrentId + 1, Items.Count,
    FCurrentName + DBDATA_EXT]);
end;

procedure TDBUpdaterThread.SyncUpdateProgress;
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

procedure TDBUpdaterThread.SyncUpdateStatus;
begin
  FStatusBar.Panels[2].Text := FStatusText;
end;

procedure TDBUpdaterThread.SyncUpdateHint;
begin
  FStatusBar.Hint := Trim(Items.Text);
end;

procedure TDBUpdaterThread.SyncShowFailed;
begin
  MessageDlg(RS_FailedItemsTitle, Format(RS_FailedItems, [FFailedList.Text]),
    mtError, [mbOK], 0);
end;

procedure TDBUpdaterThread.SyncCloseUsed;
begin
  FormMain.edMangaListSearch.Clear;
  FormMain.vtMangaList.Clear;
  dataProcess.Close;
end;

procedure TDBUpdaterThread.SyncReopenUsed;
begin
  FormMain.OpenDataDB(FCurrentName);
end;

procedure TDBUpdaterThread.SyncRemoveAttached;
begin
  dataProcess.RemoveFilter;
end;

procedure TDBUpdaterThread.UpdateStatusText(const S: String);
begin
  if FStatusText = S then
    Exit;
  FStatusText := S;
  Synchronize(@SyncUpdateStatus);
end;

procedure TDBUpdaterThread.Execute;
var
  currentfilename, lurl: String;
  cont: Boolean;
  used: Boolean;
begin
  FCurrentId := 0;
  Synchronize(@SyncUpdateHint);
  while FCurrentId < Items.Count do
  begin
    if Terminated then
      Break;
    try
      FCurrentName := Items[FCurrentId];
      Synchronize(@SyncStartDownload);
      lurl := GetDBURL(FCurrentName);
      if Pos('sourceforge.net', AnsiLowerCase(lurl)) <> 0 then
        cont := SourceForge.Download(FHTTP, lurl)
      else
        cont := FHTTP.GET(GetDBURL(FCurrentName)) and (FHTTP.ResultCode < 300);
      if cont then
      begin
        cont := True;
        // save to data folder
        currentfilename := DATA_FOLDER + FCurrentName + DBDATA_SERVER_EXT;
        if FileExists(currentfilename) then
          DeleteFile(currentfilename);
        if not FileExists(currentfilename) then
        begin
          FHTTP.Document.SaveToFile(currentfilename);
          if not FileExists(currentfilename) then
          begin
            FFailedList.Add(Format(RS_FailedToSave, [FCurrentName]));
            cont := False;
          end;
        end
        else
        begin
          FFailedList.Add(Format(RS_FailedToSave, [FCurrentName]));
          cont := False;
        end;

        if cont and (not FileExists(CURRENT_ZIP_EXE)) then
        begin
          FFailedList.Add(Format(RS_MissingZipExe, [FCurrentName, ZIP_EXE]));
          cont := False;
        end;

        if cont then
        begin
          // close and reopen current used
          used := FCurrentName =
            FormMain.cbSelectManga.Items[FormMain.cbSelectManga.ItemIndex];

          if used then
            Synchronize(@SyncCloseUsed)
          else
          if dataProcess.WebsiteLoaded(FCurrentName) then
            Synchronize(@SyncRemoveAttached);
          with TProcess.Create(nil) do
            try
              UpdateStatusText(Format('[%d/%d] ' + RS_Extracting,
                [FCurrentId + 1, Items.Count,
                FCurrentName + DBDATA_EXT]));
              Executable := CURRENT_ZIP_EXE;
              CurrentDirectory := FMD_DIRECTORY;
              Parameters.Add('x');                                     // extract
              Parameters.Add(currentfilename);                         // input
              Parameters.Add('-o' + AnsiQuotedStr(DATA_FOLDER, '"'));  // destination
              Parameters.Add('-aoa');                                  // overwrite all
              Options := Options + [poWaitOnExit];
              ShowWindow := swoNone;
              Execute;
              cont := ExitStatus = 0;
              if cont then
                DeleteFile(currentfilename)
              else
                FFailedList.Add(RS_FailedExtract, [FCurrentName, ExitStatus]);
            finally
              Free;
            end;
          if cont and used then
            Synchronize(@SyncReopenUsed);
        end;
      end
      else
        FFailedList.Add(Format(RS_FailedDownload, [FCurrentName, FHTTP.ResultCode,
          FHTTP.ResultString]));
    except
      on E: Exception do
        FFailedList.Add(E.Message);
    end;
    Inc(FCurrentId);
  end;
end;

constructor TDBUpdaterThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFailedList := TStringList.Create;
  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.Sock.OnStatus := @HTTPSockOnStatus;
  Items := TStringList.Create;
  Synchronize(@SyncStart);
end;

destructor TDBUpdaterThread.Destroy;
begin
  if (not Terminated) and (FFailedList.Count <> 0) then
    Synchronize(@SyncShowFailed);
  Synchronize(@SyncFinal);
  FHTTP.Free;
  FFailedList.Free;
  FreeAndNil(Items);
  inherited Destroy;
end;

procedure TDBUpdaterThread.Add(const S: String);
var
  i: Integer;
begin
  if Items = nil then Exit;
  // search on not sorted
  for i := 0 to Items.Count - 1 do
    if S = Items[i] then
      Exit;
  Items.Add(S);
  UpdateStatus;
end;

procedure TDBUpdaterThread.Add(const S: TStrings);
var
  i, j, jmax: Integer;
begin
  if Items = nil then Exit;
  // search on not sorted
  jmax := Items.Count;
  for i := 0 to S.Count - 1 do
  begin
    j := 0;
    while j < jmax do
      if S[i] = Items[j] then
        Break
      else
        Inc(j);
    if j = jmax then
      Items.Add(S[i]);
  end;
  UpdateStatus;
end;

procedure TDBUpdaterThread.UpdateStatus;
begin
  SyncUpdateStatus;
  SyncUpdateHint;
end;

end.
