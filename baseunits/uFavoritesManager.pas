{
        File: uFavoritesManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFavoritesManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, Dialogs, IniFiles, lazutf8classes, LazFileUtils,
  FileUtil, uBaseUnit, uData, uDownloadsManager, uFMDThread, uMisc, WebsiteModules,
  FMDOptions, httpsendthread, SimpleException;

type
  TFavoriteManager = class;
  TFavoriteTask = class;
  TfavoriteContainer = class;

  { TFavoriteThread }

  TFavoriteThread = class(TFMDThread)
  protected
    procedure SyncStatus;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    WorkId: Cardinal;
    Task: TFavoriteTask;
    Container: TfavoriteContainer;
    constructor Create;
  end;

  TFavoriteThreads = TFPGList<TFavoriteThread>;

  { TFavoriteTask }

  TFavoriteTask = class(TFMDThread)
  private
    FBtnCaption: String;
    FPendingCount: Integer;
  protected
    procedure SyncStartChecking;
    procedure SyncFinishChecking;
    procedure SyncUpdateBtnCaption;
    procedure Checkout;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    CS_Threads: TRTLCriticalSection;
    Manager: TFavoriteManager;
    Threads: TFavoriteThreads;
    procedure UpdateBtnCaption(Cap: String);
    constructor Create;
    destructor Destroy; override;
  end;

  { TFavoriteContainer }

  TFavoriteContainer = class
  private
    FModuleId: Integer;
    FWebsite: String;
    procedure SetWebsite(AValue: String);
  public
    FavoriteInfo: TFavoriteInfo;
    NewMangaInfo: TMangaInfo;
    NewMangaInfoChaptersPos: TCardinalList;
    Thread: TFavoriteThread;
    Manager: TFavoriteManager;
    Status: TFavoriteStatusType;
    constructor Create;
    destructor Destroy; override;
    property ModuleId: Integer read FModuleId;
    property Website: String read FWebsite write SetWebsite;
  end;

  TFavoriteContainers = TFPGList<TFavoriteContainer>;

  { TFavoriteManager }

  TFavoriteManager = class
  private
    CS_Favorites: TRTLCriticalSection;
    FSortColumn: Integer;
    FSortDirection, FIsAuto, FIsRunning: Boolean;
    function GetFavoritesCount: Integer; inline;
    function GetFavorite(const Index: Integer): TFavoriteContainer;
  public
    Items: TFavoriteContainers;
    favoritesFile: TIniFileRun;
    taskthread: TFavoriteTask;
    DLManager: TDownloadManager;
    OnUpdateFavorite: procedure of object;
    OnUpdateDownload: procedure of object;

    constructor Create;
    destructor Destroy; override;

    //Check favorites
    procedure CheckForNewChapter(FavoriteIndex: Integer = -1);
    procedure StopChekForNewChapter(WaitFor: Boolean = True; FavoriteIndex: Integer = -1);

    // Show notification form after checking completed
    procedure ShowResult;
    // Return true if a manga exist in FFavorites
    function IsMangaExist(const ATitle, AWebsite: String): Boolean;
    function IsMangaExistURL(const AWebsite, AURL: String): Boolean;
    // Add new manga to the list
    procedure Add(const ATitle, ACurrentChapter, ADownloadedChapterList, AWebsite, ASaveTo, ALink: String);
    // Merge manga information with a title that already exist in FFavorites
    procedure AddMerge(const ATitle, ACurrentChapter, ADownloadedChapterList, AWebsite,
      ASaveTo, ALink: String);
    // Merge a FFavorites.ini with another FFavorites.ini
    procedure MergeWith(const APath: String);
    // Remove a manga from FFavorites
    procedure Remove(const Pos: Integer; const isBackup: Boolean = True);
    // Restore information from FFavorites.ini
    procedure Restore;
    // Backup to FFavorites.ini
    procedure Backup;
    // Add FFavorites downloadedchapterlist
    procedure AddToDownloadedChaptersList(const AWebsite, ALink, AValue: String);
      overload;
    procedure AddToDownloadedChaptersList(const AWebsite, Alink: String; AValue: TStrings); overload;
    // sorting
    procedure Sort(const AColumn: Integer);
    // critical section
    procedure Lock;
    procedure LockRelease;

    property Count: Integer read GetFavoritesCount;
    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property isAuto: Boolean read FIsAuto write FIsAuto;
    property isRunning: Boolean read FIsRunning write FIsRunning;
    property Favorite[const Index: Integer]: TFavoriteContainer read GetFavorite; default;
  end;

resourcestring
  RS_DlgFavoritesCheckIsRunning = 'Favorites check is running!';
  RS_DlgNewChapterCaption = '%d manga(s) have new chapter(s)';
  RS_LblNewChapterFound = 'Found %d new chapter from %d manga(s):';
  RS_FavoriteHasNewChapter = '%s <%s> has %d new chapter(s).';
  RS_BtnDownload = '&Download';
  RS_BtnAddToQueue = '&Add to queue';
  RS_BtnCancel = '&Cancel';
  RS_DlgCompletedMangaCaption = 'Found %d completed manga';
  RS_LblMangaWillBeRemoved = 'Completed manga will be removed:';
  RS_BtnRemove = '&Remove';
  RS_BtnCheckFavorites = 'Check for new chapter';

implementation

uses
  frmMain, frmNewChapter;

{ TFavoriteContainer }

procedure TFavoriteContainer.SetWebsite(AValue: String);
begin
  if FWebsite = AValue then Exit;
  FWebsite := AValue;
  FavoriteInfo.Website := AValue;
  FModuleId := Modules.LocateModule(FavoriteInfo.Website);
end;

constructor TFavoriteContainer.Create;
begin
  FModuleId := -1;
end;

destructor TFavoriteContainer.Destroy;
begin
  if Assigned(Thread) then
  begin
    Thread.Terminate;
    Thread.WaitFor;
    Thread := nil;
  end;
  if Assigned(NewMangaInfo) then
  begin
    NewMangaInfo.Free;
    NewMangaInfoChaptersPos.Free;
  end;
  inherited Destroy;
end;

{ TFavoriteThread }

procedure TFavoriteThread.SyncStatus;
begin
  if MainForm.pcMain.ActivePage = MainForm.tsFavorites then
    MainForm.vtFavorites.Repaint;
end;

procedure TFavoriteThread.Execute;
var
  DLChapters: TStringList;
  i: Integer;
begin
  if (Container.FavoriteInfo.Link) = '' then Exit;

  Synchronize(SyncStatus);
  with Container do
    try
      // get new manga info
      with TMangaInformation.Create(Self) do
        try
          isGetByUpdater := False;
          mangaInfo.title := FavoriteInfo.Title;
          GetInfoFromURL(FavoriteInfo.Website, FavoriteInfo.Link, DefaultRetryCount);
          if not Terminated then
          begin
            NewMangaInfoChaptersPos := TCardinalList.Create;
            NewMangaInfo := mangaInfo;
            mangaInfo := nil;
          end;
        finally
          Free;
        end;

      // check for new chapters
      if (not Terminated) and Assigned(NewMangaInfo) then
      begin
        // update current chapters count immedietly
        FavoriteInfo.CurrentChapter := IntToStr(NewMangaInfo.chapterLinks.Count);
        if NewMangaInfo.chapterLinks.Count > 0 then
          try
            DLChapters := TStringList.Create;
            DLChapters.Sorted := False;
            GetParams(DLChapters, FavoriteInfo.DownloadedChapterList);
            DLChapters.Sorted := True;
            for i := 0 to NewMangaInfo.chapterLinks.Count - 1 do
              if DLChapters.IndexOf(NewMangaInfo.chapterLinks[i]) = -1 then
                NewMangaInfoChaptersPos.Add(i);
          finally
            DLChapters.Free;
          end;

        // free unneeded objects
        if (NewMangaInfoChaptersPos.Count = 0) and
          (NewMangaInfo.status <> MangaInfo_StatusCompleted) then
        begin
          FreeAndNil(NewMangaInfo);
          FreeAndNil(NewMangaInfoChaptersPos);
        end;
      end;
    except
      on E: Exception do
        ExceptionHandle(Self, E);
    end;
end;

procedure TFavoriteThread.DoTerminate;
begin
  EnterCriticalsection(Container.Manager.CS_Favorites);
  try
    if Terminated then
    begin
      Container.Status := STATUS_IDLE;
      // free unused objects
      if Assigned(Container.NewMangaInfo) then
      begin
        FreeAndNil(Container.NewMangaInfo);
        FreeAndNil(Container.NewMangaInfoChaptersPos);
      end;
    end
    else
      Container.Status := STATUS_CHECKED;
    Container.Thread := nil;

    EnterCriticalsection(Task.CS_Threads);
    try
      Modules.DecActiveConnectionCount(Container.ModuleId);
      Task.Threads.Remove(Self);
    finally
      LeaveCriticalsection(Task.CS_Threads);
    end;
  finally
    LeaveCriticalsection(Container.Manager.CS_Favorites);
  end;
  if not Terminated then
    Synchronize(SyncStatus);
  inherited DoTerminate;
end;

constructor TFavoriteThread.Create;
begin
  inherited Create(True);
end;

{ TFavoriteTask }

procedure TFavoriteTask.SyncStartChecking;
begin
  with MainForm do begin
    btCancelFavoritesCheck.Visible := True;
    btFavoritesCheckNewChapter.Width :=
      btFavoritesCheckNewChapter.Width - btCancelFavoritesCheck.Width - 6;
    btFavoritesCheckNewChapter.Caption := RS_Checking;
  end;
end;

procedure TFavoriteTask.SyncFinishChecking;
begin
  with MainForm do
  begin
    btCancelFavoritesCheck.Visible := False;
    btFavoritesCheckNewChapter.Width := btFavoritesCheckNewChapter.Width +
      btCancelFavoritesCheck.Width + 6;
    btFavoritesCheckNewChapter.Caption := RS_BtnCheckFavorites;
    vtFavorites.Repaint;
  end;
  try
    EnterCriticalsection(Manager.CS_Favorites);
    Manager.isRunning := False;
    Manager.taskthread := nil;
  finally
    LeaveCriticalsection(Manager.CS_Favorites);
  end;
end;

procedure TFavoriteTask.SyncUpdateBtnCaption;
begin
  MainForm.btFavoritesCheckNewChapter.Caption := FBtnCaption;
end;

procedure TFavoriteTask.Checkout;
var
  i: Integer;
begin
  if Terminated then Exit;
  if Manager.Items.Count = 0 then Exit;

  FPendingCount := 0;
  for i := 0 to Manager.Items.Count - 1 do
  begin
    if Terminated then Break;
    with Manager.Items[i] do
      if (Status = STATUS_CHECK) then
      begin
        if (Threads.Count < OptionMaxThreads) and
          Modules.CanCreateConnection(ModuleId) then
        begin
          EnterCriticalsection(CS_Threads);
          try
            Modules.IncActiveConnectionCount(ModuleId);
            Status := STATUS_CHECKING;
            Threads.Add(TFavoriteThread.Create);
            Thread := Threads.Last;
            Thread.Task := Self;
            Thread.Container := Manager.Items[i];
            Thread.WorkId := i;
            Thread.Start;
          finally
            LeaveCriticalsection(CS_Threads);
          end
        end
        else
          Inc(FPendingCount);
      end;
  end;
end;

procedure TFavoriteTask.Execute;
var
  cthread,
  cmaxthreads: Integer;
begin
  Manager.isRunning := True;
  Synchronize(SyncStartChecking);
  try
    while not Terminated do
    begin
      cmaxthreads := OptionMaxThreads;
      // if current thread count > max Threads allowed we wait until thread count decreased
      while (not Terminated) and (Threads.Count >= cmaxthreads) do
        Sleep(SOCKHEARTBEATRATE);
      Checkout;
      // if there is concurent connection limit applied and no more possible item to check
      // we will wait until thread count decreased
      // break wait if OptionMaxThreads changed
      cthread := Threads.Count;
      while (not Terminated) and (Threads.Count > 0) and (Threads.Count = cthread) and
        (cmaxthreads = OptionMaxThreads) do
        Sleep(SOCKHEARTBEATRATE);
      // if there is no more item need to be checked, but thread count still > 0 we will wait for it
      // we will also wait if there is new item pushed, so we will check it after it
      while (not Terminated) and (FPendingCount = 0) and (Threads.Count > 0) do
        Sleep(SOCKHEARTBEATRATE);
      if FPendingCount = 0 then Break;
    end;

    while (not Terminated) and (Threads.Count > 0) do
      Sleep(SOCKHEARTBEATRATE);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TFavoriteTask.DoTerminate;
var
  i: Integer;
begin
  // reset all status
  EnterCriticalsection(Manager.CS_Favorites);
  try
    for i := 0 to Manager.Items.Count - 1 do
      Manager.Items[i].Status := STATUS_IDLE;
  finally
    LeaveCriticalsection(Manager.CS_Favorites);
  end;

  // terminate all threads and wait
  EnterCriticalsection(CS_Threads);
  try
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        Threads[i].Terminate;
  finally
    LeaveCriticalsection(CS_Threads);
  end;
  while Threads.Count > 0 do
    Sleep(100);

  // reset the ui
  Synchronize(SyncFinishChecking);

  if (not Terminated) and (not isDlgCounter) then
    Synchronize(Manager.ShowResult)
  else
  // free unused unit
  begin
    EnterCriticalsection(Manager.CS_Favorites);
    try
      for i := 0 to Manager.Items.Count - 1 do
        with Manager.Items[i] do
          if Assigned(NewMangaInfo) then
          begin
            FreeAndNil(NewMangaInfo);
            FreeAndNil(NewMangaInfoChaptersPos);
          end;
    finally
      LeaveCriticalsection(Manager.CS_Favorites);
    end;
  end;
  inherited DoTerminate;
end;

procedure TFavoriteTask.UpdateBtnCaption(Cap: String);
begin
  FBtnCaption := Cap;
  Synchronize(SyncUpdateBtnCaption);
end;

constructor TFavoriteTask.Create;
begin
  inherited Create(True);
  InitCriticalSection(CS_Threads);
  Threads := TFavoriteThreads.Create;
end;

destructor TFavoriteTask.Destroy;
begin
  Threads.Free;
  DoneCriticalsection(CS_Threads);
  inherited Destroy;
end;

{ TFavoriteManager }

function TFavoriteManager.GetFavoritesCount: Integer;
begin
  Result := Items.Count;
end;

function TFavoriteManager.GetFavorite(const Index: Integer): TFavoriteContainer;
begin
  Result := Items[Index];
end;

constructor TFavoriteManager.Create;
begin
  inherited Create;
  ForceDirectoriesUTF8(WORK_FOLDER);
  InitCriticalSection(CS_Favorites);
  isRunning := False;
  favoritesFile := TIniFileRun.Create(FAVORITES_FILE);
  Items := TFavoriteContainers.Create;
  Restore;
end;

destructor TFavoriteManager.Destroy;
begin
  Backup;
  favoritesFile.Free;
  if Items.Count > 0 then begin
    StopChekForNewChapter;
    while Items.Count > 0 do begin
      Items.Last.Free;
      Items.Remove(Items.Last);
    end;
  end;
  Items.Free;
  DoneCriticalsection(CS_Favorites);
  inherited Destroy;
end;

procedure TFavoriteManager.CheckForNewChapter(FavoriteIndex: Integer);
var
  i: Integer;
begin
  if isDlgCounter then Exit;
  try
    if FavoriteIndex > -1 then
    begin
      with Items[FavoriteIndex] do
        if Status = STATUS_IDLE then
        begin
          Status := STATUS_CHECK;
          if Assigned(taskthread) then
            taskthread.FPendingCount := InterLockedIncrement(taskthread.FPendingCount);
        end;
    end
    else
    if isRunning then
    begin
      if not isAuto then
        MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbOK], 0);
    end
    else
    begin
      EnterCriticalsection(CS_Favorites);
      try
        for i := 0 to Items.Count - 1 do
          with Items[i] do
            if (Status = STATUS_IDLE) and (Trim(FavoriteInfo.Link) <> '') then
              Status := STATUS_CHECK;
      finally
        LeaveCriticalsection(CS_Favorites);
      end;
    end;
    if taskthread = nil then
    begin
      taskthread := TFavoriteTask.Create;
      taskthread.Manager := Self;
      taskthread.Start;
    end;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TFavoriteManager.StopChekForNewChapter(WaitFor: Boolean; FavoriteIndex: Integer);
begin
  if isRunning then
    if FavoriteIndex > -1 then
    begin
      with Items[FavoriteIndex] do begin
        if Thread <> nil then
        begin
          Thread.Terminate;
          if WaitFor then
            Thread.WaitFor;
        end;
        if Status <> STATUS_IDLE then
          Status := STATUS_IDLE;
      end;
    end
    else
    begin
      taskthread.Terminate;
      if WaitFor then
        taskthread.WaitFor;
    end;
end;

procedure TFavoriteManager.ShowResult;
var
  i, j,
  numOfNewChapters,
  numOfMangaNewChapters,
  numOfCompleted: Integer;
  LNCResult: TNewChapterResult = ncrCancel;
  newChapterListStr: String = '';
  removeListStr: String = '';
begin
  if isDlgCounter then Exit;
  if (Self.DLManager = nil) and Assigned(MainForm.DLManager) then
    Self.DLManager := MainForm.DLManager;
  if Self.DLManager = nil then Exit;

  EnterCriticalsection(CS_Favorites);
  try
    numOfNewChapters := 0;
    numOfMangaNewChapters := 0;
    numOfCompleted := 0;

    try
      // check for all favorites
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if Assigned(NewMangaInfo) then
          begin
            // new chapters add to notification
            if NewMangaInfoChaptersPos.Count > 0 then
            begin
              newChapterListStr += LineEnding + '- ' + Format(
                RS_FavoriteHasNewChapter, [FavoriteInfo.Title, FavoriteInfo.Website,
                NewMangaInfoChaptersPos.Count]);
              Inc(numOfMangaNewChapters);
              Inc(numOfNewChapters, NewMangaInfoChaptersPos.Count);
            end
            else
            // completed series add to notification
            if OptionAutoCheckFavRemoveCompletedManga and
              (NewMangaInfo.status = MangaInfo_StatusCompleted) then
            begin
              removeListStr += LineEnding + Format('- %s <%s>',
                [FavoriteInfo.Title, FavoriteInfo.Website]);
              Inc(numOfCompleted);
            end;
          end;

      // if there is completed mangas, show dialog
      if numOfCompleted > 0 then
      begin
        with TNewChapter.Create(MainForm) do
          try
            Caption := Format(RS_DlgCompletedMangaCaption, [numOfCompleted]);
            lbNotification.Caption := RS_LblMangaWillBeRemoved;
            mmMemo.Lines.Text := Trim(removeListStr);
            btDownload.Caption := RS_BtnRemove;
            btCancel.Caption := RS_BtnCancel;
            btDownload.Show;
            btCancel.Show;
            btQueue.Hide;
            ShowModal;
            LNCResult := FormResult;
          finally
            Free;
          end;

        //delete complete FFavorites
        if LNCResult = ncrDownload then
        begin
          i := 0;
          while i < Items.Count do
            with Items[i] do
            begin
              if Assigned(NewMangaInfo) and
                (NewMangaInfoChaptersPos.Count = 0) and
                (NewMangaInfo.status = MangaInfo_StatusCompleted) then
              begin
                Items[i].Free;
                Items.Delete(i);
              end
              else
                Inc(i);
            end;
        end;
        Backup;
      end;

      // if there is new chapters
      if numOfNewChapters > 0 then
      begin
        if OptionAutoCheckFavDownload then
          LNCResult := ncrDownload
        else
          with TNewChapter.Create(MainForm) do
            try
              Caption := Format(RS_DlgNewChapterCaption, [numOfNewChapters]);
              lbNotification.Caption :=
                Format(RS_LblNewChapterFound, [numOfNewChapters, numOfMangaNewChapters]);
              mmMemo.Lines.Text := Trim(newChapterListStr);
              btDownload.Caption := RS_BtnDownload;
              btQueue.Caption := RS_BtnAddToQueue;
              btCancel.Caption := RS_BtnCancel;
              btDownload.Show;
              btQueue.Show;
              btCancel.Show;
              ShowModal;
              LNCResult := FormResult;
            finally
              Free;
            end;

        // generate download task
        if LNCResult <> ncrCancel then
        begin
          while DLManager.isRunningBackup do
            Sleep(100);

          for i := 0 to Items.Count - 1 do
            with Items[i] do
              if Assigned(NewMangaInfo) and
                (NewMangaInfoChaptersPos.Count > 0) then
                try
                  EnterCriticalSection(DLManager.CS_Task);
                  DLManager.Items.Add(TTaskContainer.Create);
                  with DLManager.Items.Last do
                  begin
                    Manager := DLManager;
                    CurrentDownloadChapterPtr := 0;
                    Website := FavoriteInfo.Website;
                    DownloadInfo.Link := FavoriteInfo.Link;
                    DownloadInfo.Title := FavoriteInfo.Title;
                    DownloadInfo.SaveTo := FavoriteInfo.SaveTo;
                    DownloadInfo.dateTime := Now;

                    for j := 0 to NewMangaInfoChaptersPos.Count - 1 do
                    begin
                      ChapterLinks.Add(NewMangaInfo.chapterLinks[NewMangaInfoChaptersPos[j]]);
                      ChapterName.Add(CustomRename(
                        OptionChapterCustomRename,
                        FavoriteInfo.Website,
                        FavoriteInfo.Title,
                        NewMangaInfo.authors,
                        NewMangaInfo.artists,
                        NewMangaInfo.chapterName[NewMangaInfoChaptersPos[j]],
                        Format('%.4d', [NewMangaInfoChaptersPos[j] + 1]),
                        OptionChangeUnicodeCharacter));
                    end;

                    if LNCResult = ncrDownload then
                    begin
                      DownloadInfo.Status := RS_Waiting;
                      Status := STATUS_WAIT;
                    end
                    else
                    begin
                      DownloadInfo.Status := RS_Stopped;
                      Status := STATUS_STOP;
                    end;
                  end;
                  // add to downloaded chapter list
                  FavoriteInfo.downloadedChapterList += SetParams(NewMangaInfo.chapterLinks);
                  // add to downloaded chapter list in downloadmanager
                  DLManager.DownloadedChapters.Chapters[FavoriteInfo.Website + FavoriteInfo.Link] :=
                    NewMangaInfo.chapterLinks.Text;

                  // free unused objects
                  FreeAndNil(NewMangaInfo);
                  FreeAndNil(NewMangaInfoChaptersPos);
                finally
                  LeaveCriticalSection(DLManager.CS_Task);
                end;

          Backup;
          if LNCResult = ncrDownload then
          begin
            DLManager.CheckAndActiveTask;
            MainForm.pcMain.ActivePage := MainForm.tsDownload;
          end;
          if Assigned(OnUpdateDownload) then
            OnUpdateDownload;
          if Assigned(OnUpdateFavorite) then
            OnUpdateFavorite;
        end;
      end;

    except
      on E: Exception do
        ExceptionHandle(Self, E);
    end;

    // check again for unused objects and free them
    for i := 0 to Items.Count - 1 do
      with Items[i] do
        if Assigned(NewMangaInfo) then
        begin
          FreeAndNil(NewMangaInfo);
          FreeAndNil(NewMangaInfoChaptersPos);
        end;
  finally
    LeaveCriticalsection(CS_Favorites);
  end;
end;

function TFavoriteManager.IsMangaExist(const ATitle, AWebsite: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Items.Count > 0 then
    for i := 0 to Items.Count - 1 do
      with Items[i].FavoriteInfo do
        if SameText(ATitle, Title) and SameText(AWebsite, Website) then
          Exit(True);
end;

function TFavoriteManager.IsMangaExistURL(const AWebsite, AURL: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Items.Count > 0 then
    for i := 0 to Items.Count - 1 do
      with Items[i].FavoriteInfo do
        if SameText(AWebsite, Website) and SameText(AURL, Link) then
          Exit(True);
end;

procedure TFavoriteManager.Add(const ATitle, ACurrentChapter, ADownloadedChapterList,
  AWebsite, ASaveTo, ALink: String);
begin
  if IsMangaExist(ATitle, AWebsite) then Exit;
  EnterCriticalsection(CS_Favorites);
  try
    Items.Add(TFavoriteContainer.Create);
    with Items.Last do begin
      Manager := Self;
      Website := AWebsite;
      with FavoriteInfo do begin
        Title := ATitle;
        CurrentChapter := ACurrentChapter;
        SaveTo := ASaveTo;
        Link := ALink;
        DownloadedChapterList := ADownloadedChapterList;
      end;
      Status := STATUS_IDLE;
    end;
    if not isRunning then
    begin
      Sort(SortColumn);
      Backup;
    end;
  finally
    LeaveCriticalsection(CS_Favorites);
  end;
end;

procedure TFavoriteManager.AddMerge(const ATitle, ACurrentChapter, ADownloadedChapterList,
  AWebsite, ASaveTo, ALink: String);
begin
  if IsMangaExist(ATitle, AWebsite) then
    Exit;
  EnterCriticalsection(CS_Favorites);
  try
    Items.Add(TFavoriteContainer.Create);
    with Items.Last do begin
      Manager := Self;
      Website := AWebsite;
      with FavoriteInfo do begin
        Title := ATitle;
        CurrentChapter := ACurrentChapter;
        SaveTo := ASaveTo;
        Link := ALink;
        DownloadedChapterList := ADownloadedChapterList;
      end;
    end;
  except
    LeaveCriticalsection(CS_Favorites);
  end;
end;

procedure TFavoriteManager.MergeWith(const APath: String);
var
  mergeFile: TIniFile;
  fstream: TFileStreamUTF8;
  l, i: Cardinal;
  infos: array of TFavoriteInfo;
begin
  if isRunning then
    Exit;
  if not FileExistsUTF8(APath) then
    Exit;
  isRunning := True;

  fstream := TFileStreamUTF8.Create(APath, fmOpenRead);
  mergeFile := TIniFile.Create(fstream);
  try
    with mergeFile do begin
      l := mergeFile.ReadInteger('general', 'NumberOfFavorites', 0);
      if l > 0 then
      begin
        SetLength(infos, l);
        for i := 0 to l - 1 do
          with infos[i] do begin
            Title := ReadString(IntToStr(i), 'Title', '');
            currentChapter := ReadString(IntToStr(i), 'CurrentChapter', '0');
            downloadedChapterList := ReadString(IntToStr(i), 'DownloadedChapterList', '');
            Website := ReadString(IntToStr(i), 'Website', '');
            SaveTo := ReadString(IntToStr(i), 'SaveTo', '');
            Link := ReadString(IntToStr(i), 'Link', '');

            AddMerge(Title, currentChapter, downloadedChapterList, Website, SaveTo, Link);
          end;
      end;
    end;
    Sort(SortColumn);
    Backup;
  finally
    fStream.Free;
    mergeFile.Free;
  end;
  SetLength(infos, 0);
  isRunning := False;
end;

procedure TFavoriteManager.Remove(const Pos: Integer; const isBackup: Boolean);
begin
  if (not isRunning) and (Pos < Items.Count) then
  begin
    EnterCriticalsection(CS_Favorites);
    try
      Items[Pos].Free;
      Items.Delete(Pos);
      if isBackup then
        Backup;
    finally
      LeaveCriticalsection(CS_Favorites);
    end;
  end;
end;

procedure TFavoriteManager.Restore;
var
  i, c: Integer;
  s: TStringList;
begin
  c := favoritesFile.ReadInteger('general', 'NumberOfFavorites', 0);
  if c = 0 then Exit;
  s := TStringList.Create;
  try
    s.Sorted := True;
    s.Duplicates := dupIgnore;
    for i := 0 to c - 1 do begin
      Items.Add(TFavoriteContainer.Create);
      with Items.Last do begin
        Manager := Self;
        with favoritesFile, FavoriteInfo do begin
          currentChapter := ReadString(IntToStr(i), 'CurrentChapter', '0');
          Title := ReadString(IntToStr(i), 'Title', '');
          Website := ReadString(IntToStr(i), 'Website', '');
          SaveTo := CorrectPathSys(ReadString(IntToStr(i), 'SaveTo', ''));
          Link := ReadString(IntToStr(i), 'Link', '');
          Website := Website;
          Status := STATUS_IDLE;

          downloadedChapterList := ReadString(IntToStr(i), 'DownloadedChapterList', '');
          // remove duplicate
          GetParams(s, DownloadedChapterList);
          DownloadedChapterList := SetParams(s);
          s.Clear;
        end;
      end;
    end;
  finally
    s.Free;
  end;
end;

procedure TFavoriteManager.Backup;
var
  i: Integer;
begin
  with favoritesFile do begin
    // delete old info
    if ReadInteger('general', 'NumberOfFavorites', 0) > 0 then
      for i := 0 to ReadInteger('general', 'NumberOfFavorites', 0) - 1 do
        EraseSection(IntToStr(i));

    WriteInteger('general', 'NumberOfFavorites', Items.Count);
    if Items.Count > 0 then
      for i := 0 to Items.Count - 1 do
        with Items[i].FavoriteInfo do begin
          WriteString(IntToStr(i), 'Title', Title);
          WriteString(IntToStr(i), 'CurrentChapter', currentChapter);
          WriteString(IntToStr(i), 'DownloadedChapterList', downloadedChapterList);
          WriteString(IntToStr(i), 'Website', Website);
          WriteString(IntToStr(i), 'SaveTo', SaveTo);
          WriteString(IntToStr(i), 'Link', Link);
        end;
    UpdateFile;
  end;
end;

procedure TFavoriteManager.AddToDownloadedChaptersList(const AWebsite, ALink, AValue: String);
var
  st: TStringList;
begin
  if (AWebsite <> '') and (ALink <> '') and (AValue <> '') then
  begin
    st := TStringList.Create;
    try
      GetParams(st, AValue);
      AddToDownloadedChaptersList(AWebsite, ALink, st);
    finally
      St.Free;
    end;
  end;
end;

procedure TFavoriteManager.AddToDownloadedChaptersList(const AWebsite, Alink: String; AValue: TStrings);
var
  i, p, q: Integer;
  Ch, dlCh: TStringList;
begin
  if Count = 0 then
    Exit;
  if (AWebsite <> '') and (Alink <> '') and (AValue.Count > 0) then
  begin
    Ch := TStringList.Create;
    dlCh := TStringList.Create;
    EnterCriticalsection(CS_Favorites);
    try
      p := -1;
      //locate the link
      if Items.Count > 1 then
        for i := 0 to Items.Count - 1 do
          with Items[i].FavoriteInfo do
            if SameText(AWebsite, Website) and SameText(Alink, Link) then
            begin
              p := i;
              GetParams(dlCh, downloadedChapterList);
              Break;
            end;

      //if found the FavoriteItem
      if p > -1 then
      begin
        //remove if links found on downloadedchapterlist
        Ch.Assign(AValue);
        if dlCh.Count > 0 then
        begin
          dlCh.Sorted := True;
          i := 0;
          while i < Ch.Count do
            if dlCh.Find(Ch[i], q) then
              Ch.Delete(i)
            else
              Inc(i);
        end;

        //merge the links
        with Items[p].FavoriteInfo do begin
          downloadedChapterList := downloadedChapterList + SetParams(ch);
          currentChapter := IntToStr(dlCh.Count + ch.Count);
        end;
        MainForm.UpdateVtFavorites;
      end;
    finally
      LeaveCriticalsection(CS_Favorites);
    end;
    dlCh.Free;
    Ch.Free;
  end;
end;

function CompareFavoriteContainer(const Item1, Item2: TFavoriteContainer): Integer;

  function GetStr(ARow: TFavoriteContainer): String;
  begin
    with ARow.FavoriteInfo do
      case ARow.Manager.SortColumn of
        1: Result := Title;
        2: Result := currentChapter;
        3: Result := website;
        4: Result := SaveTo;
        else
          Result := '';
      end;
  end;

begin
  if Item1.Manager.SortDirection then
    Result := NaturalCompareStr(GetStr(Item2), GetStr(Item1))
  else
    Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
end;

procedure TFavoriteManager.Sort(const AColumn: Integer);
begin
  if Items.Count < 2 then Exit;
  EnterCriticalsection(CS_Favorites);
  try
    SortColumn := AColumn;
    Items.Sort(CompareFavoriteContainer);
  finally
    LeaveCriticalsection(CS_Favorites);
  end;
end;

procedure TFavoriteManager.Lock;
begin
  EnterCriticalsection(CS_Favorites);
end;

procedure TFavoriteManager.LockRelease;
begin
  LeaveCriticalsection(CS_Favorites);
end;

end.
