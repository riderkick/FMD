unit DBUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, FMDOptions, StatusBarDownload,
  process, ComCtrls, Controls, Dialogs, Buttons;

type

  { TDBUpdaterThread }

  TDBUpdaterThread = class(TStatusBarDownload)
  private
    FCurrentId: Integer;
    FCurrentName: String;
    FFailedList: TStringList;
  protected
    procedure HTTPRedirected(const AHTTP: THTTPSendThread; const URL: String);
  protected
    procedure SyncStart;
    procedure SyncFinal;
    procedure SyncUpdateHint;
    procedure SyncShowFailed;
    procedure SyncCloseUsed;
    procedure SyncReopenUsed;
    procedure SyncRemoveAttached;
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

uses FMDVars, LazFileUtils;

function GetDBURL(const AName: String): String;
begin
  if Pos('<website>', AnsiLowerCase(DB_URL)) <> -1 then
    Result := StringReplace(DB_URL, '<website>', AName, [rfIgnoreCase, rfReplaceAll])
  else
    Result := AName;
end;

{ TDBUpdaterThread }

procedure TDBUpdaterThread.HTTPRedirected(const AHTTP: THTTPSendThread;
  const URL: String);
begin
  UpdateStatusText(Format('[%d/%d] ' + RS_Downloading,
    [FCurrentId + 1, Items.Count, FCurrentName + DBDATA_EXT + ' ' + URL]));
end;

procedure TDBUpdaterThread.SyncStart;
begin
  DBUpdaterThread := Self;
end;

procedure TDBUpdaterThread.SyncFinal;
begin
  DBUpdaterThread := nil;
end;

procedure TDBUpdaterThread.SyncUpdateHint;
begin
  StatusBar.Hint := Trim(Items.Text);
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

procedure TDBUpdaterThread.Execute;
var
  currentfilename: String;
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
      UpdateStatusText(Format('[%d/%d] ' +
        RS_Downloading, [FCurrentId + 1, Items.Count, FCurrentName + DBDATA_EXT]));
      if HTTP.GET(GetDBURL(FCurrentName)) and (HTTP.ResultCode < 300) then
      begin
        cont := True;
        // save to data folder
        ForceDirectoriesUTF8(DATA_FOLDER);
        currentfilename := DATA_FOLDER + FCurrentName + DBDATA_SERVER_EXT;
        if FileExists(currentfilename) then
          DeleteFile(currentfilename);
        if not FileExists(currentfilename) then
        begin
          HTTP.Document.SaveToFile(currentfilename);
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
              ShowWindow := swoHIDE;
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
        FFailedList.Add(Format(RS_FailedDownload, [FCurrentName, HTTP.ResultCode,
          HTTP.ResultString]));
    except
      on E: Exception do
        FFailedList.Add(E.Message);
    end;
    Inc(FCurrentId);
  end;
end;

constructor TDBUpdaterThread.Create;
begin
  inherited Create(True, FormMain, FormMain.IconList, 24);
  FFailedList := TStringList.Create;
  HTTP.OnRedirected := @HTTPRedirected;
  Items := TStringList.Create;
  Synchronize(@SyncStart);
end;

destructor TDBUpdaterThread.Destroy;
begin
  if (not Terminated) and (FFailedList.Count <> 0) then
    Synchronize(@SyncShowFailed);
  Synchronize(@SyncFinal);
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
  SyncUpdateHint;
end;

end.
