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
  uBaseUnit, uData, uDownloadsManager, uFMDThread, uMisc, WebsiteModules;

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
    procedure SyncShowResult;
    procedure Checkout;
    procedure Execute; override;
  public
    manager: TFavoriteManager;
    threads: TFPList;
    procedure PushNewCheck;
    procedure UpdateBtnCaption(Cap: String);
    constructor Create;
    destructor Destroy; override;
  end;

  { TFavoriteContainer }

  TFavoriteContainer = Class
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
    CS_Favorites : TCriticalSection;
    FSortColumn: Integer;
    FSortDirection,
    FIsAuto,
    FIsRunning: Boolean;
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
    procedure StopChekForNewChapter(WaitFor: Boolean = True;
      FavoriteIndex: Integer = -1);

    // Show notification form after checking completed
    procedure ShowResult;
    // Return true if a manga exist in FFavorites
    function IsMangaExist(const title, website: String): Boolean;
    function IsMangaExistURL(const website, URL: String): Boolean;
    // Add new manga to the list
    procedure Add(const Atitle, AcurrentChapter, AdownloadedChapterList,
      Awebsite, AsaveTo, Alink: String);
    // Merge manga information with a title that already exist in FFavorites
    procedure AddMerge(const Atitle, AcurrentChapter, AdownloadedChapterList,
      Awebsite, AsaveTo, Alink: String);
    // Merge a FFavorites.ini with another FFavorites.ini
    procedure MergeWith(const APath: String);
    // Remove a manga from FFavorites
    procedure Remove(const pos: Integer; const isBackup: Boolean = True);
    // Restore information from FFavorites.ini
    procedure Restore;
    // Backup to FFavorites.ini
    procedure Backup;
    // Add FFavorites downloadedchapterlist
    procedure AddToDownloadedChaptersList(const AWebsite, ALink, AValue: String); overload;
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
      MainForm.ExceptionHandler(Self, E);
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

procedure TFavoriteTask.SyncShowResult;
begin
  manager.ShowResult;
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

procedure TFavoriteTask.Execute;
var
  i, cthread: Integer;
begin
  manager.isRunning := True;
  Synchronize(SyncStartChecking);
  try
    Checkout;
    while (not Terminated) and (statuscheck > 0) do
    begin
      Sleep(SOCKHEARTBEATRATE);
      // if current thread count > max threads allowed we wait until thread count decreased
      while (not Terminated) and (threads.Count >= OptionMaxThreads) do
        Sleep(SOCKHEARTBEATRATE);
      Checkout;
      // if there is concurent connection limit applied and no more possible item to check
      // we will wait until thread count decresed
      cthread := threads.Count;
      while (not Terminated) and (threads.Count > 0) and (threads.Count = cthread) do
        Sleep(SOCKHEARTBEATRATE);
      // if there is no more item need to be checked, but thread count still > 0 we will wait for it
      // we will also wait if there is new item pushed, so we will check it after it
      while (not Terminated) and (statuscheck = 0) and (threads.Count > 0) do
        Sleep(SOCKHEARTBEATRATE);
    end;

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
      Synchronize(SyncShowResult);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  manager.CS_Favorites.Acquire;
  try
    for i := 0 to manager.FFavorites.Count-1 do
      if TFavoriteContainer(manager.FFavorites[i]).Status <> STATUS_IDLE then
        TFavoriteContainer(manager.FFavorites[i]).Status := STATUS_IDLE;
  finally
    manager.CS_Favorites.Release;
  end;
  Synchronize(SyncFinishChecking);
end;

procedure TFavoriteTask.PushNewCheck;
begin
  Inc(statuscheck);
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
  manager.taskthread := nil;
  manager.isRunning := False;
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
      TFavoriteContainer(FFavorites[FavoriteIndex]).Status := STATUS_CHECK
    else
    begin
      if isRunning then
      begin
        if not isAuto then
          MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbOK], 0);
      end
      else
      begin
        CS_Favorites.Acquire;
        try
          for i := 0 to FFavorites.Count-1 do
             with TFavoriteContainer(FFavorites[i]) do
               if (Status = STATUS_IDLE) and (Trim(FavoriteInfo.Link) <> '') then
                 Status := STATUS_CHECK;
        finally
          CS_Favorites.Release;
        end;
      end;
    end;
    if taskthread = nil then
    begin
      taskthread := TFavoriteTask.Create;
      taskthread.manager := Self;
      taskthread.Start;
    end
    else
      taskthread.PushNewCheck;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TFavoriteManager.StopChekForNewChapter(WaitFor: Boolean;
  FavoriteIndex: Integer);
begin
  if isRunning then
  begin
    if FavoriteIndex > -1 then
    begin
      if TFavoriteContainer(FFavorites[FavoriteIndex]).Thread <> nil then
      begin
        TFavoriteContainer(FFavorites[FavoriteIndex]).Thread.Terminate;
        if WaitFor then
          TFavoriteContainer(FFavorites[FavoriteIndex]).Thread.WaitFor;
      end;
      if TFavoriteContainer(FFavorites[FavoriteIndex]).Status <> STATUS_IDLE then
        TFavoriteContainer(FFavorites[FavoriteIndex]).Status := STATUS_IDLE;
    end
    else
    begin
      taskthread.Terminate;
      if WaitFor then
        taskthread.WaitFor;
    end;
  end;
end;

procedure TFavoriteManager.ShowResult;
var
  i, p, counter,
  numOfNewChapters,
  numOfMangaNewChapters,
  numOfCompleted        : Integer;
  dlChapters            : TStringList;
  LNCResult             : TNewChapterResult = ncrCancel;
  newChapterListStr     : String = '';
  removeListStr         : String = '';
  favDelete             : Boolean;
begin
  if isDlgCounter then Exit;
  try
    CS_Favorites.Acquire;
    dlChapters := TStringList.Create;
    try
      numOfNewChapters := 0;
      numOfMangaNewChapters := 0;
      numOfCompleted := 0;
      counter := 0;
      while counter < FFavorites.Count do
      begin
        //compare new mangainfo's chapters with downloadedchapter from favorites
        with FavoriteItem(counter) do try
          if Assigned(MangaInfo) then
          begin
            if MangaInfo.chapterLinks.Count > 0 then
            begin
              NewMangaInfo := TMangaInfo.Create;
              NewMangaInfoChaptersPos := TCardinalList.Create;
              TransferMangaInfo(NewMangaInfo, MangaInfo);
              NewMangaInfo.chapterLinks.Clear;
              NewMangaInfo.chapterName.Clear;
              dlChapters.Clear;
              GetParams(dlChapters, FavoriteInfo.downloadedChapterList);
              dlChapters.Sort;
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
              if (OptionAutoCheckFavRemoveCompletedManga) and (NewMangaInfo.status = '0') then
              begin
                removeListStr := removeListStr + LineEnding +
                  Format('- %s <%s>', [FavoriteInfo.Title, FavoriteInfo.Website]);
                Inc(numOfCompleted);
              end;
            end;
          end;
        finally
          Inc(counter);
        end;
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
              with FavoriteItem(counter) do begin
                if Assigned(NewMangaInfo) then
                  if (NewMangaInfo.chapterLinks.Count = 0) and (NewMangaInfo.status = '0') then
                  begin
                    FavoriteItem(counter).Free;
                    FFavorites.Delete(counter);
                    favDelete := True;
                  end;
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
        begin
          if MainForm.cbAddAsStopped.Checked then
            LNCResult := ncrQueue
          else
            LNCResult := ncrDownload;
        end
        else
        begin
          with TNewChapter.Create(MainForm) do try
            Caption := Format(RS_DlgNewChapterCaption, [numOfNewChapters]);
            lbNotification.Caption := Format(RS_LblNewChapterFound, [numOfNewChapters, numOfMangaNewChapters]);
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
        end;

        if LNCResult <> ncrCancel then
        //generate new download task
        begin
          while DLManager.isRunningBackup do
            Sleep(100);
          counter := 0;
          while counter < FFavorites.Count do
          begin
            with FavoriteItem(counter) do begin
              if Assigned(NewMangaInfo) then
                if NewMangaInfo.chapterLinks.Count > 0 then
                begin
                  DLManager.CS_DownloadManager_Task.Acquire;
                  try
                    DLManager.containers.Add(TTaskContainer.Create);
                    with TTaskContainer(DLManager.Containers.Last) do begin
                      Manager := DLManager;
                      CurrentDownloadChapterPtr := 0;
                      Website := Website;
                      with DownloadInfo do begin
                        Website := FavoriteInfo.Website;
                        Link := FavoriteInfo.Link;
                        Title := FavoriteInfo.Title;
                        SaveTo := FavoriteInfo.SaveTo;
                        dateTime := Now;
                      end;
                      ChapterLinks.Assign(NewMangaInfo.chapterLinks);
                      for i := 0 to NewMangaInfo.chapterLinks.Count - 1 do begin
                        ChapterName.Add(CustomRename(
                          OptionCustomRename,
                          FavoriteInfo.Website,
                          FavoriteInfo.Title,
                          NewMangaInfo.authors,
                          NewMangaInfo.artists,
                          NewMangaInfo.chapterName[i],
                          Format('%.4d', [NewMangaInfoChaptersPos[i] + 1]),
                          MainForm.cbOptionPathConvert.Checked));
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
                    FavoriteInfo.currentChapter := IntToStr(MangaInfo.chapterLinks.Count);
                  finally
                    DLManager.CS_DownloadManager_Task.Release;
                  end;
                  //mark downloaded
                  FavoriteInfo.downloadedChapterList :=
                    FavoriteInfo.downloadedChapterList + SetParams(NewMangaInfo.chapterLinks);
                  //save to downloaded chapter list from dlmanager.
                  DLManager.AddToDownloadedChaptersList(
                    FavoriteInfo.Website + FavoriteInfo.Link, NewMangaInfo.chapterLinks);
                end;
            end;
            Inc(counter);
          end;
          Backup;
          DLManager.Backup;
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
        Inc(counter)
      end;
      FreeAndNil(dlChapters);
      CS_Favorites.Release;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

function TFavoriteManager.IsMangaExist(const title, website: String): Boolean;
var
  i: Integer;
begin
  if FFavorites.Count > 0 then
    for i := 0 to FFavorites.Count - 1 do
      if (CompareText(FavoriteItem(i).FavoriteInfo.Title, title) = 0) and
        (CompareText(FavoriteItem(i).FavoriteInfo.website, website) = 0) then
        Exit(True);
  Result := False;
end;

function TFavoriteManager.IsMangaExistURL(const website, URL : String): Boolean;
Var
  i: Integer;
begin
  Result := False;
  if FFavorites.Count > 0 then
    for i := 0 to FFavorites.Count - 1 do
      if SameText(website, FavoriteItem(i).FavoriteInfo.Website) and
        SameText(URL, FavoriteItem(i).FavoriteInfo.Link) then
        Exit(True);
  Result := False;
end;

procedure TFavoriteManager.Add(const Atitle, AcurrentChapter,
  AdownloadedChapterList, Awebsite, AsaveTo, Alink: String);
begin
  if IsMangaExist(Atitle, Awebsite) then Exit;
  CS_Favorites.Acquire;
  try
    FFavorites.Add(TFavoriteContainer.Create);
    with TFavoriteContainer(FFavorites.Last) do begin
      Manager := Self;
      FavoriteInfo.Title := Atitle;
      FavoriteInfo.currentChapter := AcurrentChapter;
      FavoriteInfo.website := Awebsite;
      FavoriteInfo.saveTo := AsaveTo;
      FavoriteInfo.Link := ALink;
      FavoriteInfo.downloadedChapterList := AdownloadedChapterList;
      Website := AWebsite;
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

procedure TFavoriteManager.AddMerge(const Atitle, AcurrentChapter,
  AdownloadedChapterList, Awebsite, AsaveTo, Alink: String);
begin
  if IsMangaExist(Atitle, Awebsite) then
    Exit;
  CS_Favorites.Acquire;
  try
    FFavorites.Add(TFavoriteContainer.Create);
    with TFavoriteContainer(FFavorites.Last) do begin
      Manager := Self;
      FavoriteInfo.Title := Atitle;
      FavoriteInfo.currentChapter := AcurrentChapter;
      FavoriteInfo.website := Awebsite;
      FavoriteInfo.saveTo := AsaveTo;
      FavoriteInfo.Link := Alink;
      FavoriteInfo.downloadedChapterList := AdownloadedChapterList;
      Website := Awebsite;
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

  l := mergeFile.ReadInteger('general', 'NumberOfFavorites', 0);
  if l > 0 then
  begin
    SetLength(infos, l);
    for i := 0 to l - 1 do
    begin
      infos[i].Title := mergeFile.ReadString(IntToStr(i), 'Title', '');
      infos[i].currentChapter :=
        mergeFile.ReadString(IntToStr(i), 'CurrentChapter', '0');
      infos[i].downloadedChapterList :=
        mergeFile.ReadString(IntToStr(i), 'DownloadedChapterList', '');
      infos[i].website := mergeFile.ReadString(IntToStr(i), 'Website', '');
      infos[i].SaveTo := mergeFile.ReadString(IntToStr(i), 'SaveTo', '');
      infos[i].link := mergeFile.ReadString(IntToStr(i), 'Link', '');

      AddMerge(infos[i].Title,
        infos[i].currentChapter,
        infos[i].downloadedChapterList,
        infos[i].website,
        infos[i].SaveTo,
        infos[i].link);
    end;
  end;
  Sort(SortColumn);
  Backup;

  SetLength(infos, 0);
  fStream.Free;
  mergeFile.Free;
  isRunning := False;
end;

procedure TFavoriteManager.Remove(const pos: Integer; const isBackup: Boolean);
begin
  if (not isRunning) and (pos < FFavorites.Count) then
  begin
    CS_Favorites.Acquire;
    try
      FavoriteItem(pos).Free;
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
        FavoriteInfo.Title := favoritesFile.ReadString(IntToStr(i), 'Title', '');
        FavoriteInfo.currentChapter :=
          favoritesFile.ReadString(IntToStr(i), 'CurrentChapter', '0');
        FavoriteInfo.downloadedChapterList :=
          favoritesFile.ReadString(IntToStr(i), 'DownloadedChapterList', '');
        FavoriteInfo.website := favoritesFile.ReadString(IntToStr(i), 'Website', '');
        FavoriteInfo.SaveTo :=
          CorrectPathSys(favoritesFile.ReadString(IntToStr(i), 'SaveTo', ''));
        FavoriteInfo.link := favoritesFile.ReadString(IntToStr(i), 'Link', '');
        Website := FavoriteInfo.Website;
        Status := STATUS_IDLE;
      end;
    end;
end;

procedure TFavoriteManager.Backup;
var
  i: Integer;
begin
  // delete old info
  if favoritesFile.ReadInteger('general', 'NumberOfFavorites', 0) > 0 then
    for i := 0 to favoritesFile.ReadInteger('general', 'NumberOfFavorites', 0) - 1 do
      favoritesFile.EraseSection(IntToStr(i));

  favoritesFile.WriteInteger('general', 'NumberOfFavorites', FFavorites.Count);
  if FFavorites.Count > 0 then
    for i := 0 to FFavorites.Count - 1 do
    begin
      favoritesFile.WriteString(IntToStr(i), 'Title', FavoriteItem(i).FavoriteInfo.Title);
      favoritesFile.WriteString(IntToStr(i), 'CurrentChapter',
        FavoriteItem(i).FavoriteInfo.currentChapter);
      favoritesFile.WriteString(IntToStr(i), 'DownloadedChapterList',
        FavoriteItem(i).FavoriteInfo.downloadedChapterList);
      favoritesFile.WriteString(IntToStr(i), 'Website', FavoriteItem(i).FavoriteInfo.Website);
      favoritesFile.WriteString(IntToStr(i), 'SaveTo', FavoriteItem(i).FavoriteInfo.SaveTo);
      favoritesFile.WriteString(IntToStr(i), 'Link', FavoriteItem(i).FavoriteInfo.link);
    end;
  favoritesFile.UpdateFile;
end;

procedure TFavoriteManager.AddToDownloadedChaptersList(const AWebsite, ALink,
  AValue: String);
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

procedure TFavoriteManager.AddToDownloadedChaptersList(const AWebsite,
  Alink: String; AValue: TStrings);
var
  i, p, q: Integer;
  Ch, dlCh: TStringList;
begin
  if Count = 0 then
    Exit;
  if (AWebsite <> '') and (Alink <> '') and  (AValue.Count > 0) then
  begin
    CS_Favorites.Acquire;
    Ch := TStringList.Create;
    dlCh := TStringList.Create;
    try
      p := -1;
      //locate the link
      if FFavorites.Count > 1 then
        for i := 0 to FFavorites.Count - 1 do
          if SameText(AWebsite, FavoriteItem(i).FavoriteInfo.Website) and
            SameText(Alink, FavoriteItem(i).FavoriteInfo.Link) then
          begin
            p := i;
            GetParams(dlCh, FavoriteItem(i).FavoriteInfo.downloadedChapterList);
            Break;
          end;

      //if found the FavoriteItem
      if p > -1 then
      begin
        //remove if links found on downloadedchapterlist
        Ch.Assign(AValue);
        if dlCh.Count > 0 then
        begin
          dlCh.Sort;
          i := 0;
          while i < Ch.Count do
          begin
            if dlCh.Find(Ch[i], q) then
              Ch.Delete(i)
            else
              Inc(i);
          end;
        end;

        //merge the links
        with FavoriteItem(p).FavoriteInfo do begin
          downloadedChapterList := downloadedChapterList + SetParams(ch);
          currentChapter := IntToStr(dlCh.Count + ch.Count);
        end;
        MainForm.UpdateVtFavorites;
      end;
    finally
      dlCh.Free;
      Ch.Free;
      CS_Favorites.Release;
    end;
  end;
end;

procedure TFavoriteManager.Sort(const AColumn: Integer);

  function GetStr(ARow: Pointer): String;
  begin
    case AColumn of
      1: Result := TFavoriteContainer(ARow).FavoriteInfo.Title;
      2: Result := TFavoriteContainer(ARow).FavoriteInfo.currentChapter;
      3: Result := TFavoriteContainer(ARow).FavoriteInfo.website;
      4: Result := TFavoriteContainer(ARow).FavoriteInfo.SaveTo;
    end;
  end;

  function Compare(Item1, Item2: Pointer): Integer;
  var
    ItemT: Pointer;
  begin
    if SortDirection then
    begin
      ItemT := Item1;
      Item1 := Item2;
      Item2 := ItemT;
    end;
    Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
  end;

  procedure QSort(FList: TFPList; L, R: Integer);
  var
    I, J : Longint;
    P, Q : Pointer;
  begin
   repeat
     I := L;
     J := R;
     P := FList[ (L + R) div 2 ];
     repeat
       while Compare(P, FList[i]) > 0 do
         I := I + 1;
       while Compare(P, FList[J]) < 0 do
         J := J - 1;
       If I <= J then
       begin
         Q := FList[I];
         Flist[I] := FList[J];
         FList[J] := Q;
         I := I + 1;
         J := J - 1;
       end;
     until I > J;
     if J - L < R - I then
     begin
       if L < J then
         QSort(FList, L, J);
       L := I;
     end
     else
     begin
       if I < R then
         QSort(FList, I, R);
       R := J;
     end;
   until L >= R;
  end;

begin
  if FFavorites.Count < 2 then Exit;
  CS_Favorites.Acquire;
  try
    SortColumn := AColumn;
    QSort(FFavorites, 0, FFavorites.Count - 1);
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
