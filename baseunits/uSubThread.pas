{
        File: uSubThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uSubThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, lclintf,
  uBaseUnit, uFMDThread;

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
    FURL: String;

    procedure Execute; override;

    procedure MainThreadShowLog;
    procedure MainThreadUpdate;
    procedure MainThreadUpdateRequire;
    procedure MainThreadImportant;
    procedure MainThreadLatestVer;
    procedure MainThreadSetButton;
    procedure MainThreadPopSilentThreadQueue;
    procedure DoTerminate; override;
  public
    updateCounter: Cardinal;
    isCheckForLatestVer: Boolean;
    isCanStop: Boolean;
    fNote, fNoteForThisRevision, fImportant: String;
    constructor Create;
  end;

implementation

uses
  FileUtil,
  frmMain, frmLog,
  uSilentThread;

var
  LRequireRevision: Cardinal = 1;
  LRevision: Cardinal;
  LVersion: String;

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

procedure TSubThread.MainThreadShowLog;
begin
  Log := TLog.Create(MainForm);
  Log.ShowModal;
  Log.Free;
end;

procedure TSubThread.MainThreadUpdate;
begin
  if MessageDlg('', Format(stDlgNewVersion + #10#13#10#13 + fNote,
    [LVersion, LRevision]),
    mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    CopyFile(fmdDirectory + 'updater.exe', fmdDirectory + 'old_updater.exe');
    CopyFile(fmdDirectory + CONFIG_FOLDER + CONFIG_FILE,
      fmdDirectory + CONFIG_FOLDER + CONFIG_FILE + '.tmp');
    MainForm.CloseNow;
    fmdRunAsAdmin(fmdDirectory + 'old_updater.exe', '1', False);
    Halt;
  end
  else
    MainForm.btCheckVersion.Caption := stUpdaterCheck;
end;

procedure TSubThread.MainThreadUpdateRequire;
begin
  if MessageDlg('', Format(stDlgUpdaterVersionRequire, [LRequireRevision]),
    mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    OpenURL('http://akarink.wordpress.com/');
  end;
end;

procedure TSubThread.MainThreadImportant;
begin
  MessageDlg('', fImportant, mtInformation, [mbYes], 0);
  // MainForm.CloseNow;
  // Halt;
end;

procedure TSubThread.MainThreadLatestVer;
begin
  MessageDlg('', stDlgLatestVersion, mtInformation, [mbYes], 0);
end;

procedure TSubThread.MainThreadSetButton;
begin
  MainForm.btCheckVersion.Caption := stUpdaterCheck;
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

procedure TSubThread.Execute;
var
  l: TStringList;
  i: Cardinal;
  s: String;
begin
  MainForm.isSubthread := True;
  LRevision := 0;
  try
    if FileExists(WORK_FOLDER + LOG_FILE) then
      Synchronize(MainThreadShowLog);
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
        Synchronize(MainThreadPopSilentThreadQueue);
      end;

      isCheckForLatestVer := False;
      { TODO -oCholif : Temporary disable latest version check }

      if isCheckForLatestVer then
      begin
        Sleep(2000);
        l := TStringList.Create;

        l.NameValueSeparator := '=';
        case Random(2) of
          0:
            s := UPDATE_URL + 'updates.i';
          1:
            s := SourceForgeURL(
              'http://sourceforge.net/projects/fmd/files/FMD/updates/updates.i/download');
        end;
        if (GetPage(TObject(l), s, 0)) and (l.Count > 0) then
        begin
          fNote := '';
          fNoteForThisRevision := '';
          for i := 0 to l.Count - 1 do
          begin
            if l.Names[i] = IntToStr(Revision) then
              fNoteForThisRevision := l.ValueFromIndex[i];
            if l.Names[i] = 'Note' then
              fNote := l.ValueFromIndex[i];
            if l.Names[i] = 'Revision' then
              LRevision := StrToInt(l.ValueFromIndex[i]);
            if l.Names[i] = 'RequireRevision' then
              LRequireRevision := StrToInt(l.ValueFromIndex[i]);
            if l.Names[i] = 'Version' then
              LVersion := l.ValueFromIndex[i];
            if l.Names[i] = 'Important' then
            begin
              fImportant := l.ValueFromIndex[i];
              Synchronize(MainThreadImportant);
            end;
          end;

          if LRequireRevision > Revision then
            Synchronize(MainThreadUpdateRequire)
          else
          if LRevision > Revision then
          begin
            Synchronize(MainThreadUpdate);
          end
          else
          begin
            if updateCounter > 0 then
            begin
              Synchronize(MainThreadLatestVer);
            end;
            Synchronize(MainThreadSetButton);
          end;
        end;
        Inc(updateCounter);

        isCheckForLatestVer := False;
        l.Free;
      end;

      isCanStop := False;
      isCanStop := True;
      Sleep(400);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
