{
        File: uFavoritesManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFavoritesManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, IniFiles, syncobjs, lazutf8classes, LazFileUtils,
  uBaseUnit, uData, uDownloadsManager, uFMDThread, uMisc, WebsiteModules,
  SimpleException;

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
    workCounter: Cardinal;
    getInfo: TMangaInformation;
    task: TFavoriteTask;
    container: TfavoriteContainer;
    constructor Create;
    destructor Destroy; override;
  end;

  { TFavoriteTask }

  TFavoriteTask = class(TFMDThread)
  private
    FBtnCaption: String;
    statuscheck: Integer;
  protected
    procedure SyncStartChecking;
    procedure SyncFinishChecking;
    procedure SyncUpdateBtnCaption;
    procedure Checkout;
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    manager: TFavoriteManager;
    threads: TFPList;
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
    MangaInfo: TMangaInfo;
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

  { TFavoriteManager }

  TFavoriteManager = class
  private
    CS_Favorites: TCriticalSection;
    FSortColumn: Integer;
    FSortDirection, FIsAuto, FIsRunning: Boolean;
    FFavorites: TFPList;
  protected
    function GetFavoritesCount: Integer;
  public
    favoritesFile: TIniFile;
    taskthread: TFavoriteTask;
    DLManager: TDownloadManager;
    OnUpdateFavorite: procedure of object;
    OnUpdateDownload: procedure of object;

    constructor Create;
    destructor Destroy; override;

    function FavoriteItem(const Index: Integer): TFavoriteContainer;

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
    procedure Remove(const pos: Integer; const isBackup: Boolean = True);
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
  if Assigned(MangaInfo) then
    MangaInfo.Free;
  if Assigned(NewMangaInfo) then
    NewMangaInfo.Free;
  if Assigned(NewMangaInfoChaptersPos) then
    NewMangaInfoChaptersPos.Free;
  inherited Destroy;
end;

{ TFavoriteThread }

procedure TFavoriteThread.SyncStatus;
begin
  if MainForm.pcMain.ActivePage = MainForm.tsFavorites then
    MainForm.vtFavorites.Repaint;
end;

procedure TFavoriteThread.Execute;
begin
  if (container.FavoriteInfo.Link) = '' then Exit;
  //Modules.IncActiveConnectionCount(container.ModuleId);
  try
    Synchronize(SyncStatus);
    getInfo.mangaInfo.title := container.FavoriteInfo.Title;
    getInfo.GetInfoFromURL(container.FavoriteInfo.Website,
      container.FavoriteInfo.Link, OptionConnectionMaxRetry);
    if container.MangaInfo = nil then
      container.MangaInfo := TMangaInfo.Create;
    TransferMangaInfo(container.MangaInfo, getInfo.mangaInfo);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
  if Self.Terminated then
    container.Status := STATUS_IDLE
  else
    container.Status := STATUS_CHECKED;
  Synchronize(SyncStatus);
end;

procedure TFavoriteThread.DoTerminate;
begin
  LockCreateConnection;
  try
    Modules.DecActiveConnectionCount(container.ModuleId);
    container.Thread := nil;
    task.threads.Remove(Self);
  finally
    UnlockCreateConnection;
  end;
  inherited DoTerminate;
end;

constructor TFavoriteThread.Create;
begin
  inherited Create(True);
  getInfo := TMangaInformation.Create(Self);
  getInfo.isGetByUpdater := False;
end;

destructor TFavoriteThread.Destroy;
begin
  getInfo.Free;
  inherited Destroy;
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
  with MainForm do begin
    btCancelFavoritesCheck.Visible := False;
    btFavoritesCheckNewChapter.Width :=
      btFavoritesCheckNewChapter.Width + btCancelFavoritesCheck.Width + 6;
    btFavoritesCheckNewChapter.Caption := RS_BtnCheckFavorites;
    vtFavorites.Repaint;
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
  if manager.FFavorites.Count = 0 then Exit;
  manager.CS_Favorites.Acquire;
  try
    statuscheck := 0;
    for i := 0 to manager.FFavorites.Count - 1 do
    begin
      if Terminated then Break;
      with TFavoriteContainer(manager.FFavorites[i]) do
        if Status = STATUS_CHECK then
        begin
          LockCreateConnection;
          try
            if (threads.Count < OptionMaxThreads) and
              Modules.CanCreateConnection(ModuleId) then
            begin
              Modules.IncActiveConnectionCount(ModuleId);
              Thread := TFavoriteThread.Create;
              threads.Add(Thread);
              Status := STATUS_CHECKING;
              with thread do
              begin
                task := Self;
                container := manager.FavoriteItem(i);
                workCounter := i;
                Start;
              end;
            end
            else
              Inc(statuscheck);
          finally
            UnlockCreateConnection;
          end;
        end;
    end;
  finally
    manager.CS_Favorites.Release;
  end;
end;

procedure TFavoriteTask.DoTerminate;
begin
  manager.isRunning := False;
  manager.taskthread := nil;
  inherited DoTerminate;
end;

procedure TFavoriteTask.Execute;
var
  i, cthread, cmaxthreads: Integer;
begin
  manager.isRunning := True;
  Synchronize(SyncStartChecking);
  try
    while not Terminated do
    begin
      cmaxthreads := OptionMaxThreads;
      // if current thread count > max threads allowed we wait until thread count decreased
      while (not Terminated) and (threads.Count >= cmaxthreads) do
        Sleep(SOCKHEARTBEATRATE);
      Checkout;
      // if there is concurent connection limit applied and no more possible item to check
      // we will wait until thread count decreased
      // break wait if OptionMaxThreads changed
      cthread := threads.Count;
      while (not Terminated) and (threads.Count > 0) and (threads.Count = cthread) and
        (cmaxthreads = OptionMaxThreads) do
        Sleep(SOCKHEARTBEATRATE);
      // if there is no more item need to be checked, but thread count still > 0 we will wait for it
      // we will also wait if there is new item pushed, so we will check it after it
      while (not Terminated) and (statuscheck = 0) and (threads.Count > 0) do
        Sleep(SOCKHEARTBEATRATE);
      if statuscheck = 0 then Break;
    end;

    while (not Terminated) and (threads.Count > 0) do
      Sleep(SOCKHEARTBEATRATE);

    if Terminated and (threads.Count > 0) then
    begin
      LockCreateConnection;
      try
        for i := 0 to threads.Count - 1 do
          TFavoriteThread(threads[i]).Terminate;
      finally
        UnlockCreateConnection;
      end;
      while threads.Count > 0 do
        Sleep(100);
    end;

    if (not Terminated) and (not isDlgCounter) then
      Synchronize(manager.ShowResult);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
  manager.CS_Favorites.Acquire;
  try
    for i := 0 to manager.FFavorites.Count - 1 do
      if TFavoriteContainer(manager.FFavorites[i]).Status <> STATUS_IDLE then
        TFavoriteContainer(manager.FFavorites[i]).Status := STATUS_IDLE;
  finally
    manager.CS_Favorites.Release;
  end;
  Synchronize(SyncFinishChecking);
end;

procedure TFavoriteTask.UpdateBtnCaption(Cap: String);
begin
  FBtnCaption := Cap;
  Synchronize(SyncUpdateBtnCaption);
end;

constructor TFavoriteTask.Create;
begin
  inherited Create(True);
  threads := TFPList.Create;
end;

destructor TFavoriteTask.Destroy;
begin
  threads.Free;
  inherited Destroy;
end;

{ TFavoriteManager }

function TFavoriteManager.GetFavoritesCount: Integer;
begin
  CS_Favorites.Acquire;
  try
    Result := FFavorites.Count;
  finally
    CS_Favorites.Release;
  end;
end;

constructor TFavoriteManager.Create;
begin
  inherited Create;
  CS_Favorites := TCriticalSection.Create;
  isRunning := False;
  favoritesFile := TIniFile.Create(WORK_FOLDER + FAVORITES_FILE);
  favoritesFile.CacheUpdates := True;
  FFavorites := TFPList.Create;
  Restore;
end;

destructor TFavoriteManager.Destroy;
begin
  Backup;
  favoritesFile.UpdateFile;
  favoritesFile.Free;
  if FFavorites.Count > 0 then begin
    StopChekForNewChapter;
    while FFavorites.Count > 0 do begin
      TFavoriteContainer(FFavorites.Last).Free;
      FFavorites.Remove(FFavorites.Last);
    end;
  end;
  FFavorites.Free;
  CS_Favorites.Free;
  inherited Destroy;
end;

function TFavoriteManager.FavoriteItem(const Index: Integer): TFavoriteContainer;
begin
  if (Index < 0) or (Index >= FFavorites.Count) then
    Exit(nil);
  Result := TFavoriteContainer(FFavorites.Items[Index]);
end;

procedure TFavoriteManager.CheckForNewChapter(FavoriteIndex: Integer);
var
  i: Integer;
begin
  if isDlgCounter then Exit;
  try
    if FavoriteIndex > -1 then
    begin
      with TFavoriteContainer(FFavorites[FavoriteIndex]) do
        if Status = STATUS_IDLE then
        begin
          Status := STATUS_CHECK;
          if Assigned(taskthread) then
            taskthread.statuscheck := InterLockedIncrement(taskthread.statuscheck);
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
      CS_Favorites.Acquire;
      try
        for i := 0 to FFavorites.Count - 1 do
          with TFavoriteContainer(FFavorites[i]) do
            if (Status = STATUS_IDLE) and (Trim(FavoriteInfo.Link) <> '') then
              Status := STATUS_CHECK;
      finally
        CS_Favorites.Release;
      end;
    end;
    if taskthread = nil then
    begin
      taskthread := TFavoriteTask.Create;
      taskthread.manager := Self;
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
      with TFavoriteContainer(FFavorites[FavoriteIndex]) do begin
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
  i, p, counter, numOfNewChapters, numOfMangaNewChapters, numOfCompleted: Integer;
  dlChapters: TStringList;
  LNCResult: TNewChapterResult = ncrCancel;
  newChapterListStr: String = '';
  removeListStr: String = '';
  favDelete: Boolean;
begin
  if isDlgCounter then Exit;
  if (Self.DLManager = nil) and Assigned(MainForm.DLManager) then
    Self.DLManager := MainForm.DLManager;
  if Self.DLManager = nil then Exit;
  try
    CS_Favorites.Acquire;
    dlChapters := TStringList.Create;
    try
      numOfNewChapters := 0;
      numOfMangaNewChapters := 0;
      numOfCompleted := 0;
      counter := 0;
      while counter < FFavorites.Count do
        with FavoriteItem(counter) do try
            if Assigned(MangaInfo) then
              if MangaInfo.chapterLinks.Count > 0 then
              begin
                //compare new mangainfo's chapters with downloadedchapter from favorites
                NewMangaInfo := TMangaInfo.Create;
                NewMangaInfoChaptersPos := TCardinalList.Create;
                TransferMangaInfo(NewMangaInfo, MangaInfo);
                NewMangaInfo.chapterLinks.Clear;
                NewMangaInfo.chapterName.Clear;
                dlChapters.Clear;
                dlChapters.Sorted := False;
                GetParams(dlChapters, FavoriteInfo.downloadedChapterList);
                dlChapters.Sorted := True;
                for i := 0 to MangaInfo.chapterLinks.Count - 1 do
                  if not dlChapters.Find(MangaInfo.chapterLinks[i], p) then
                  begin
                    NewMangaInfo.chapterLinks.Add(MangaInfo.chapterLinks[i]);
                    NewMangaInfo.chapterName.Add(MangaInfo.chapterName[i]);
                    NewMangaInfoChaptersPos.Add(i);
                    Inc(numOfNewChapters);
                  end;

                //add to notification
                if NewMangaInfo.chapterLinks.Count > 0 then
                begin
                  newChapterListStr := newChapterListStr + LineEnding + '- ' +
                    Format(RS_FavoriteHasNewChapter,
                    [FavoriteInfo.Title, FavoriteItem(counter).FavoriteInfo.Website,
                    NewMangaInfo.chapterLinks.Count]);
                  Inc(numOfMangaNewChapters);
                end;

                //add completed manga
                if (OptionAutoCheckFavRemoveCompletedManga) and
                  (NewMangaInfo.status = '0') then
                begin
                  removeListStr := removeListStr + LineEnding +
                    Format('- %s <%s>', [FavoriteInfo.Title, FavoriteInfo.Website]);
                  Inc(numOfCompleted);
                end;
              end;
          finally
            Inc(counter);
          end;

      dlChapters.Clear;

      if numOfNewChapters = 0 then
      begin
        // If there's no new chapter, but there're completed mangas, show dialog
        if numOfCompleted > 0 then
        begin
          with TNewChapter.Create(MainForm) do try
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
            counter := 0;
            while counter < FFavorites.Count do
            begin
              favDelete := False;
              with FavoriteItem(counter) do if Assigned(NewMangaInfo) then
                  if (NewMangaInfo.chapterLinks.Count = 0) and
                    (NewMangaInfo.status = '0') then
                  begin
                    FavoriteItem(counter).Free;
                    FFavorites.Delete(counter);
                    favDelete := True;
                  end;
              if not favDelete then
                Inc(counter);
            end;
          end;
          Backup;
          if Assigned(OnUpdateFavorite) then
            OnUpdateFavorite;
        end;
      end
      else
      begin
        //if there's new chapters
        if OptionAutoCheckFavDownload then
          LNCResult := ncrDownload
        else
          with TNewChapter.Create(MainForm) do try
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

        if LNCResult <> ncrCancel then
          //generate new download task
        begin
          while DLManager.isRunningBackup do
            Sleep(100);
          counter := 0;
          while counter < FFavorites.Count do
          begin
            with FavoriteItem(counter) do if Assigned(NewMangaInfo) then
                if NewMangaInfo.chapterLinks.Count > 0 then
                begin
                  DLManager.CS_DownloadManager_Task.Acquire;
                  try
                    DLManager.containers.Add(TTaskContainer.Create);
                    with TTaskContainer(DLManager.Containers.Last) do begin
                      Manager := DLManager;
                      CurrentDownloadChapterPtr := 0;
                      Website := FavoriteInfo.Website;
                      with DownloadInfo do begin
                        Link := FavoriteInfo.Link;
                        Title := FavoriteInfo.Title;
                        SaveTo := FavoriteInfo.SaveTo;
                        dateTime := Now;
                      end;
                      ChapterLinks.Assign(NewMangaInfo.chapterLinks);
                      for i := 0 to NewMangaInfo.chapterLinks.Count - 1 do
                        ChapterName.Add(CustomRename(
                          OptionChapterCustomRename,
                          FavoriteInfo.Website,
                          FavoriteInfo.Title,
                          NewMangaInfo.authors,
                          NewMangaInfo.artists,
                          NewMangaInfo.chapterName[i],
                          Format('%.4d', [NewMangaInfoChaptersPos[i] + 1]),
                          OptionChangeUnicodeCharacter));
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
                    FavoriteInfo.currentChapter :=
                      IntToStr(MangaInfo.chapterLinks.Count);
                  finally
                    DLManager.CS_DownloadManager_Task.Release;
                  end;
                  //mark downloaded
                  FavoriteInfo.downloadedChapterList :=
                    FavoriteInfo.downloadedChapterList +
                    SetParams(NewMangaInfo.chapterLinks);
                  //save to downloaded chapter list from dlmanager.
                  DLManager.AddToDownloadedChaptersList(
                    FavoriteInfo.Website + FavoriteInfo.Link, NewMangaInfo.chapterLinks);
                end;
            Inc(counter);
          end;
          Backup;
          if Assigned(OnUpdateDownload) then
            OnUpdateDownload;
          if LNCResult = ncrDownload then
          begin
            DLManager.CheckAndActiveTask;
            MainForm.pcMain.ActivePage := MainForm.tsDownload;
          end;
          if Assigned(OnUpdateFavorite) then
            OnUpdateFavorite;
        end;
      end;
    finally
      //free used memory
      counter := 0;
      while counter < FFavorites.Count do
      begin
        with FavoriteItem(counter) do begin
          if Assigned(MangaInfo) then
            FreeAndNil(MangaInfo);
          if Assigned(NewMangaInfo) then
            FreeAndNil(NewMangaInfo);
          if Assigned(NewMangaInfoChaptersPos) then
            FreeAndNil(NewMangaInfoChaptersPos);
        end;
        Inc(counter);
      end;
      FreeAndNil(dlChapters);
      CS_Favorites.Release;
    end;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

function TFavoriteManager.IsMangaExist(const ATitle, AWebsite: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FFavorites.Count > 0 then
    for i := 0 to FFavorites.Count - 1 do
      with TFavoriteContainer(FFavorites[i]).FavoriteInfo do
        if SameText(ATitle, Title) and SameText(AWebsite, Website) then
          Exit(True);
end;

function TFavoriteManager.IsMangaExistURL(const AWebsite, AURL: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FFavorites.Count > 0 then
    for i := 0 to FFavorites.Count - 1 do
      with TFavoriteContainer(FFavorites[i]).FavoriteInfo do
        if SameText(AWebsite, Website) and SameText(AURL, Link) then
          Exit(True);
end;

procedure TFavoriteManager.Add(const ATitle, ACurrentChapter, ADownloadedChapterList,
  AWebsite, ASaveTo, ALink: String);
begin
  if IsMangaExist(ATitle, AWebsite) then Exit;
  CS_Favorites.Acquire;
  try
    FFavorites.Add(TFavoriteContainer.Create);
    with TFavoriteContainer(FFavorites.Last) do begin
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
    CS_Favorites.Release;
  end;
end;

procedure TFavoriteManager.AddMerge(const ATitle, ACurrentChapter, ADownloadedChapterList,
  AWebsite, ASaveTo, ALink: String);
begin
  if IsMangaExist(ATitle, AWebsite) then
    Exit;
  CS_Favorites.Acquire;
  try
    FFavorites.Add(TFavoriteContainer.Create);
    with TFavoriteContainer(FFavorites.Last) do begin
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
    CS_Favorites.Release;
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

procedure TFavoriteManager.Remove(const pos: Integer; const isBackup: Boolean);
begin
  if (not isRunning) and (pos < FFavorites.Count) then
  begin
    CS_Favorites.Acquire;
    try
      TFavoriteContainer(FFavorites[pos]).Free;
      FFavorites.Delete(pos);
      if isBackup then
        Backup;
    finally
      CS_Favorites.Release;
    end;
  end;
end;

procedure TFavoriteManager.Restore;
var
  i, c: Integer;
begin
  c := favoritesFile.ReadInteger('general', 'NumberOfFavorites', 0);
  if c > 0 then
    for i := 0 to c - 1 do
    begin
      FFavorites.Add(TFavoriteContainer.Create);
      with TFavoriteContainer(FFavorites.Last) do begin
        Manager := Self;
        with favoritesFile, FavoriteInfo do begin
          Title := ReadString(IntToStr(i), 'Title', '');
          currentChapter := ReadString(IntToStr(i), 'CurrentChapter', '0');
          downloadedChapterList := ReadString(IntToStr(i), 'DownloadedChapterList', '');
          Website := ReadString(IntToStr(i), 'Website', '');
          SaveTo := CorrectPathSys(ReadString(IntToStr(i), 'SaveTo', ''));
          Link := ReadString(IntToStr(i), 'Link', '');
          Website := Website;
          Status := STATUS_IDLE;
        end;
      end;
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

    WriteInteger('general', 'NumberOfFavorites', FFavorites.Count);
    if FFavorites.Count > 0 then
      for i := 0 to FFavorites.Count - 1 do
        with TFavoriteContainer(FFavorites[i]).FavoriteInfo do begin
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
    CS_Favorites.Acquire;
    try
      p := -1;
      //locate the link
      if FFavorites.Count > 1 then
        for i := 0 to FFavorites.Count - 1 do
          with TFavoriteContainer(FFavorites[i]).FavoriteInfo do
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
        with TFavoriteContainer(FFavorites[p]).FavoriteInfo do begin
          downloadedChapterList := downloadedChapterList + SetParams(ch);
          currentChapter := IntToStr(dlCh.Count + ch.Count);
        end;
        MainForm.UpdateVtFavorites;
      end;
    finally
      CS_Favorites.Release;
    end;
    dlCh.Free;
    Ch.Free;
  end;
end;

function CompareFavoriteContainer(Item1, Item2: Pointer): Integer;

  function GetStr(ARow: Pointer): String;
  begin
    with TFavoriteContainer(ARow).FavoriteInfo do
      case TFavoriteContainer(ARow).Manager.SortColumn of
        1: Result := Title;
        2: Result := currentChapter;
        3: Result := website;
        4: Result := SaveTo;
        else
          Result := '';
      end;
  end;

var
  ItemT: Pointer;
begin
  if TFavoriteContainer(Item1).Manager.SortDirection then
  begin
    ItemT := Item1;
    Item1 := Item2;
    Item2 := ItemT;
  end;
  Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
end;

procedure TFavoriteManager.Sort(const AColumn: Integer);
begin
  if FFavorites.Count < 2 then Exit;
  CS_Favorites.Acquire;
  try
    SortColumn := AColumn;
    FFavorites.Sort(CompareFavoriteContainer);
  finally
    CS_Favorites.Release;
  end;
end;

procedure TFavoriteManager.Lock;
begin
  CS_Favorites.Acquire;
end;

procedure TFavoriteManager.LockRelease;
begin
  CS_Favorites.Release;
end;

end.
