{
        File: uSubThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uSubThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, fgl, Graphics, Process, lclintf,
  uBaseUnit, uData, uDownloadsManager, uFMDThread, uGetMangaInfosThread;

type
  { Tasks will be done by this thread:
    - Calls TGetMangaInfosThread for getting manga information.
    - Auto check for new version
    - [SilentThread] Pop metadata from queue and execute it
  }
  TSubThread = class(TFMDThread)
  protected
    FIsLoaded: Integer;
    FURL     : String;
    FGetMangaInfosThread: TGetMangaInfosThread;

    procedure   Execute; override;

    procedure   MainThreadShowLog;
    procedure   MainThreadUpdate;
    procedure   MainThreadUpdateRequire;
    procedure   MainThreadImportant;
    procedure   MainThreadLatestVer;
    procedure   MainThreadSetButton;
    procedure   MainThreadPopSilentThreadQueue;
  public
    updateCounter: Cardinal;
    isCheckForLatestVer: Boolean;
    mangaListPos: Integer;
    isHasCover,
    isCanStop   : Boolean;

    fNote, fNoteForThisRevision,
    fImportant   : String;

    constructor Create;
    destructor  Destroy; override;

    procedure   GetMangaInfos(const title, website, link: String);
  end;

implementation

uses
  FileUtil,
  frmMain, frmLog,
  uSilentThread;

var
  LRequireRevision: Cardinal = 1;
  LRevision: Cardinal;
  LVersion : String;

// ----- Public methods -----

constructor TSubThread.Create;
begin
  inherited Create(FALSE);
  fImportant     := '';
  updateCounter  := 1;
  isCheckForLatestVer:= FALSE;
  isCanStop      := FALSE;
  FGetMangaInfosThread:= nil;
end;

destructor  TSubThread.Destroy;
begin
  inherited Destroy;
end;

procedure   TSubThread.GetMangaInfos(const title, website, link: String);
begin
  if (FGetMangaInfosThread <> nil) AND
     (FGetMangaInfosThread.IsTerminated = FALSE) then
     FGetMangaInfosThread.IsFlushed:= TRUE;
  FGetMangaInfosThread:= TGetMangaInfosThread.Create;
  FGetMangaInfosThread.Title:= title;
  FGetMangaInfosThread.Website:= website;
  FGetMangaInfosThread.Link:= link;
  FGetMangaInfosThread.MangaListPos:= mangaListPos;

  // Execute FGetMangaInfosThread.
  FGetMangaInfosThread.IsSuspended:= FALSE;
end;

// ----- Protected methods -----

procedure   TSubThread.MainThreadShowLog;
begin
  Log:= TLog.Create(MainForm);
  Log.ShowModal;
  Log.Free;
end;

procedure   TSubThread.MainThreadUpdate;
var
  Process: TProcess;
begin
  if MessageDlg('', Format(stDlgNewVersion + #10#13#10#13 + fNote, [LVersion, LRevision]),
                    mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    CopyFile(fmdDirectory + 'updater.exe', fmdDirectory + 'old_updater.exe');
    CopyFile(fmdDirectory + CONFIG_FOLDER + CONFIG_FILE,
             fmdDirectory + CONFIG_FOLDER + CONFIG_FILE + '.tmp');
    Process:= TProcess.Create(nil);
    Process.CommandLine:= fmdDirectory + 'old_updater.exe 1';
    MainForm.CloseNow;
    Process.Execute;
    Process.Free;
    Halt;
  end
  else
    MainForm.btCheckVersion.Caption:= stUpdaterCheck;
end;

procedure   TSubThread.MainThreadUpdateRequire;
begin
  if MessageDlg('', Format(stDlgUpdaterVersionRequire, [LRequireRevision]), mtInformation, [mbYes, mbNo], 0)=mrYes then
  begin
    OpenURL('http://akarink.wordpress.com/');
  end;
end;

procedure   TSubThread.MainThreadImportant;
var
  Process: TProcess;
begin
  MessageDlg('', fImportant, mtInformation, [mbYes], 0);
 // MainForm.CloseNow;
 // Halt;
end;

procedure   TSubThread.MainThreadLatestVer;
begin
  MessageDlg('', stDlgLatestVersion, mtInformation, [mbYes], 0);
end;

procedure   TSubThread.MainThreadSetButton;
begin
  MainForm.btCheckVersion.Caption:= stUpdaterCheck;
end;

procedure   TSubThread.MainThreadPopSilentThreadQueue;
var
  meta: TSilentThreadMetaData;
begin
  if uSilentThread.SilentThreadQueue.Count = 0 then
    exit;
  meta:= TSilentThreadMetaData(uSilentThread.SilentThreadQueue.Pop);
  if meta <> nil then
  begin
    meta.Run;
    meta.Free;
  end;
end;

procedure   TSubThread.Execute;
var
  l: TStringList;
  i: Cardinal;
  s: String;
begin
  LRevision:= 0;
  while isSuspended do Sleep(32);
  Sleep(2000);
  if FileExists(WORK_FOLDER + LOG_FILE) then
    Synchronize(MainThreadShowLog);
  if FileExists(fmdDirectory + 'old_updater.exe') then
    DeleteFile(fmdDirectory + 'old_updater.exe');
  Sleep(2000);
  if OptionAutoCheckFavStartup then
  begin
    MainForm.favorites.isAuto:= TRUE;
    MainForm.favorites.isShowDialog:= MainForm.cbOptionShowFavoriteDialog.Checked;
    MainForm.favorites.Run;
  end;
  while NOT Terminated do
  begin
    if ((MainForm.silentThreadCount > 0)) AND
       (MainForm.currentActiveSilentThreadCount < 2) then
    begin
      Synchronize(MainThreadPopSilentThreadQueue);
    end;

    if isCheckForLatestVer then
    begin
      Sleep(2000);
      l:= TStringList.Create;

      l.NameValueSeparator:= '=';
      case Random(2) of
        0:
          s:= UPDATE_URL + 'updates.i';
        1:
          s:= SourceForgeURL('http://sourceforge.net/projects/fmd/files/FMD/updates/updates.i/download');
      end;
      if (GetPage(TObject(l), s, 0)) AND (l.Count > 0) then
      begin
        fNote:= '';
        fNoteForThisRevision:= '';
        for i:= 0 to l.Count-1 do
        begin
          if l.Names[i] = IntToStr(Revision) then
            fNoteForThisRevision:= l.ValueFromIndex[i];
          if l.Names[i] = 'Note' then
            fNote:= l.ValueFromIndex[i];
          if l.Names[i] = 'Revision' then
            LRevision:= StrToInt(l.ValueFromIndex[i]);
          if l.Names[i] = 'RequireRevision' then
            LRequireRevision:= StrToInt(l.ValueFromIndex[i]);
          if l.Names[i] = 'Version' then
            LVersion:= l.ValueFromIndex[i];
          if l.Names[i] = 'Important' then
          begin
            fImportant:= l.ValueFromIndex[i];
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

      isCheckForLatestVer:= FALSE;
      l.Free;
    end;

    isCanStop:= FALSE;

    isCanStop:= TRUE;
    Sleep(64);
  end;
end;

end.

