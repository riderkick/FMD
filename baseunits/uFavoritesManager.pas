{
        File: uFavoritesManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFavoritesManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, IniFiles, syncobjs, lazutf8classes, FileUtil, fgl,
  uBaseUnit, uData, uDownloadsManager, uFMDThread, uMisc, blcksock;

type
  TFavoriteManager = class;
  TFavoriteTask = class;
  TfavoriteContainer = class;

  { TFavoriteThread }

  TFavoriteThread = class(TFMDThread)
  protected
    procedure SockOnHeartBeat(Sender: TObject);
    procedure Execute; override;
  public
    workCounter: Cardinal;
    getInfo: TMangaInformation;
    task: TFavoriteTask;
    container: TfavoriteContainer;
    constructor Create;
    destructor Destroy; override;
  end;

  TFavoriteThreadList = TFPGList<TFavoriteThread>;

  { TFavoriteTask }

  TFavoriteTask = class(TFMDThread)
  private
    FBtnCaption: String;
  protected
    function GetThreadCount: Integer;
    procedure SyncUpdateBtnCaption;
    procedure SyncShowResult;
    procedure Execute; override;
  public
    CS_Threads: TCriticalSection;
    manager: TFavoriteManager;
    threads: TFavoriteThreadList;
    workCounter: Cardinal;
    property ThreadCount: Integer read GetThreadCount;
    procedure UpdateBtnCaption(Cap: String);
    constructor Create;
    destructor Destroy; override;
  end;

  { TFavoriteContainer }

  TFavoriteContainer = Class
  public
    FavoriteInfo: TFavoriteInfo;
    MangaInfo: TMangaInfo;
    NewMangaInfo: TMangaInfo;
    NewMangaInfoChaptersPos: TCardinalList;
    Thread: TFavoriteThread;
    Manager: TFavoriteManager;
    destructor Destroy; override;
  end;

  TFavoriteContainerList = TFPGList<TFavoriteContainer>;

  { TFavoriteManager }

  TFavoriteManager = class
  private
    CS_Favorites : TCriticalSection;
    FSortDirection: Boolean;
    FSortColumn: Cardinal;
    FIsAuto, FIsShowDialog,
    FIsRunning: Boolean;
  protected
    function GetFavoritesCount: Integer;
  public
    favoritesFile: TIniFile;
    Favorites: TFavoriteContainerList;
    taskthread: TFavoriteTask;
    DLManager: TDownloadManager;
    OnUpdateFavorite: procedure of object;
    OnUpdateDownload: procedure of object;

    constructor Create;
    destructor Destroy; override;

    procedure Run;
    // Show notification form after checking completed
    procedure ShowResult;
    // Return true if a manga exist in Favorites
    function IsMangaExist(const title, website: String): Boolean;
    function IsMangaExistURL(const website, URL: String): Boolean;
    // Add new manga to the list
    procedure Add(const title, currentChapter, downloadedChapterList,
      website, saveTo, link: String);
    // Merge manga information with a title that already exist in Favorites
    procedure AddMerge(const title, currentChapter, downloadedChapterList,
      website, saveTo, link: String);
    // Merge a favorites.ini with another favorites.ini
    procedure MergeWith(const APath: String);
    // Remove a manga from Favorites
    procedure Remove(const pos: Integer; const isBackup: Boolean = True);
    // Restore information from favorites.ini
    procedure Restore;
    // Backup to favorites.ini
    procedure Backup;
    // Abort favorites check
    procedure StopAllAndWait;

    // sorting
    procedure Sort(const AColumn: Cardinal);
    procedure SortNatural(const AColumn: Integer);

    property Count: Integer read GetFavoritesCount;
    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Cardinal read FSortColumn write FSortColumn;
    property isAuto: Boolean read FIsAuto write FIsAuto;
    property isShowDialog: Boolean read FIsShowDialog write FIsShowDialog;
    property isRunning: Boolean read FIsRunning write FIsRunning;
  end;

resourcestring
  RS_DlgFavoritesAlreadyChecking = 'Favorites already checking!';
  RS_DlgNewChapterCaption = '%d manga(s) have new chapter(s)';
  RS_LblNewChapterFound = 'Found %d new chapter from %d manga(s):';
  RS_FavoriteHasNewChapter = '%s <%s> has %d new chapter(s).';
  RS_BtnDownload = 'Download';
  RS_BtnAddToQueue = 'Add to queue';
  RS_BtnCancel = 'Cancel';
  RS_DlgCompletedMangaCaption = 'Found %d completed manga';
  RS_LblMangaWillBeRemoved = 'Completed manga will be removed:';
  RS_BtnRemove = 'Remove';

implementation

uses
  frmMain, frmNewChapter;

{ TFavoriteContainer }

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

procedure TFavoriteThread.SockOnHeartBeat(Sender: TObject);
begin
  if Terminated then
  begin
    TBlockSocket(Sender).Tag := 1;
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
end;

procedure TFavoriteThread.Execute;
begin
  if (container.FavoriteInfo.Link) = '' then Exit;
  try
    getInfo.mangaInfo.title := container.FavoriteInfo.Title;
    getInfo.GetInfoFromURL(container.FavoriteInfo.Website,
      container.FavoriteInfo.Link, container.Manager.DLManager.retryConnect);
    if container.MangaInfo = nil then
      container.MangaInfo := TMangaInfo.Create;
    TransferMangaInfo(container.MangaInfo, getInfo.mangaInfo);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

constructor TFavoriteThread.Create;
begin
  inherited Create(True);
  getInfo := TMangaInformation.Create;
  getInfo.FHTTP.Sock.OnHeartbeat := SockOnHeartBeat;
  getInfo.FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
  getInfo.isGetByUpdater := False;
end;

destructor TFavoriteThread.Destroy;
begin
  task.CS_Threads.Acquire;
  try
    container.Thread := nil;
    task.threads.Remove(Self);
  finally
    task.CS_Threads.Release;
  end;
  getInfo.Free;
  inherited Destroy;
end;

{ TFavoriteTask }

function TFavoriteTask.GetThreadCount: Integer;
begin
  CS_Threads.Acquire;
  try
    Result := threads.Count;
  finally
    CS_Threads.Release;
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

procedure TFavoriteTask.Execute;
var
  workCounter: Integer;
  i: Integer;
begin
  try
    workCounter := 0;
    while workCounter < manager.Favorites.Count do
    begin
      while threads.Count > manager.DLManager.maxDLThreadsPerTask do
      begin
        if Terminated then Break;
        Sleep(250);
      end;
      if Terminated then Break;
      if threads.Count < manager.DLManager.maxDLThreadsPerTask then
      begin
        if Trim(manager.Favorites[workCounter].FavoriteInfo.Title) <> '' then
          CS_Threads.Acquire;
          try
            with manager.Favorites[workCounter] do begin
              if Thread = nil then begin
                Thread := TFavoriteThread.Create;
                Thread.task := Self;
                Thread.container := manager.Favorites[workCounter];
                Thread.workCounter := workCounter;
                threads.Add(Thread);
                Thread.Start;
              end;
            end;
            UpdateBtnCaption(Format('%s <%s>',
              [stFavoritesChecking, manager.Favorites[workCounter].FavoriteInfo.Title]));
          finally
            CS_Threads.Release;
          end;
        Inc(workCounter);
      end;
    end;
    if Terminated and (ThreadCount > 0) then
      for i := 0 to ThreadCount - 1 do
        threads[i].Terminate;
    while threads.Count > 0 do
      Sleep(100);
    UpdateBtnCaption(stFavoritesCheck);
    if not Terminated then
      Synchronize(SyncShowResult);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  manager.isRunning := False;
end;

procedure TFavoriteTask.UpdateBtnCaption(Cap: String);
begin
  FBtnCaption := Cap;
  Synchronize(SyncUpdateBtnCaption);
end;

constructor TFavoriteTask.Create;
begin
  inherited Create(True);
  CS_Threads := TCriticalSection.Create;
  threads := TFavoriteThreadList.Create;
end;

destructor TFavoriteTask.Destroy;
begin
  manager.isRunning := False;
  threads.Free;
  CS_Threads.Free;
  inherited Destroy;
end;

{ TFavoriteManager }

function TFavoriteManager.GetFavoritesCount: Integer;
begin
  CS_Favorites.Acquire;
  try
    Result := Favorites.Count;
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
  Favorites := TFavoriteContainerList.Create;
  Restore;
end;

destructor TFavoriteManager.Destroy;
begin
  Backup;
  favoritesFile.UpdateFile;
  favoritesFile.Free;
  if Favorites.Count > 0 then begin
    StopAllAndWait;
    while Favorites.Count > 0 do begin
      Favorites.Last.Free;
      Favorites.Remove(Favorites.Last);
    end;
  end;
  Favorites.Free;
  CS_Favorites.Free;
  inherited Destroy;
end;

procedure TFavoriteManager.Run;
begin
  try
    if isRunning then
    begin
      MessageDlg('', RS_DlgFavoritesAlreadyChecking, mtInformation, [mbOK], 0);
      Exit;
    end
    else
    begin
      MainForm.btFavoritesCheckNewChapter.Caption := stFavoritesChecking;
      isRunning := True;
      taskthread := TFavoriteTask.Create;
      taskthread.manager := Self;
      taskthread.Start;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
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
  favDelete,
  isDownloadNow         : Boolean;
begin
  try
    CS_Favorites.Acquire;
    dlChapters := TStringList.Create;
    try
      numOfNewChapters := 0;
      numOfMangaNewChapters := 0;
      numOfCompleted := 0;
      counter := 0;
      while counter < Favorites.Count do
      begin
        //compare new mangainfo's chapters with downloadedchapter from favorites
        with Favorites[counter] do try
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
                  [FavoriteInfo.Title, Favorites[counter].FavoriteInfo.Website,
                   NewMangaInfo.chapterLinks.Count]);
                Inc(numOfMangaNewChapters);
              end;

              //add completed manga
              if (OptionAutoRemoveCompletedManga) and (NewMangaInfo.status = '0') then
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
        if (numOfCompleted > 0) and (isShowDialog) then
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

          //delete complete favorites
          if LNCResult = ncrDownload then
          begin
            counter := 0;
            while counter < Favorites.Count do
            begin
              favDelete := False;
              with Favorites[counter] do begin
                if Assigned(NewMangaInfo) then
                  if (NewMangaInfo.chapterLinks.Count = 0) and (NewMangaInfo.status = '0') then
                  begin
                    Favorites[counter].Free;
                    Favorites.Delete(counter);
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
        if OptionAutoDlFav then
          isDownloadNow := True
        else
        if isShowDialog then
        begin
          with TNewChapter.Create(MainForm) do try
            Caption := Format(RS_DlgNewChapterCaption, [numOfNewChapters]);
            lbNotification.Caption := Format(RS_LblNewChapterFound, [numOfNewChapters, numOfMangaNewChapters]);
            mmMemo.Lines.Text := Trim(newChapterListStr);
            btDownload.Caption := RS_BtnDownload;
            btQueue.Caption := RS_BtnAddToQueue;
            btCancel.Caption := RS_BtnAddToQueue;
            btDownload.Show;
            btQueue.Show;
            btCancel.Show;
            ShowModal;
            LNCResult := FormResult;
          finally
            Free;
          end;

          if LNCResult = ncrDownload then
          begin
            isDownloadNow := True;
            if MainForm.pcMain.PageIndex <> 0 then
              MainForm.pcMain.PageIndex := 0;
          end
          else
            isDownloadNow := False;
        end;

        if LNCResult <> ncrCancel then
        //generate new download task
        begin
          while DLManager.isRunningBackup do
            Sleep(100);
          counter := 0;
          while counter < Favorites.Count do
          begin
            with Favorites[counter] do begin
              if Assigned(NewMangaInfo) then
                if NewMangaInfo.chapterLinks.Count > 0 then
                begin
                  DLManager.CS_DownloadManager_Task.Acquire;
                  try
                    DLManager.containers.Add(TTaskThreadContainer.Create);
                    with DLManager.containers.Last do begin
                      Manager := DLManager;
                      CurrentDownloadChapterPtr := 0;
                      MangaSiteID := GetMangaSiteID(FavoriteInfo.Website);
                      with DownloadInfo do begin
                        Website := FavoriteInfo.Website;
                        title := FavoriteInfo.Title;
                        SaveTo := FavoriteInfo.SaveTo;
                        dateTime := Now;
                      end;
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
                      if isDownloadNow then
                      begin
                        DownloadInfo.Status := stWait;
                        Status := STATUS_WAIT;
                      end
                      else
                      begin
                        DownloadInfo.Status := stStop;
                        Status := STATUS_STOP;
                      end;
                    end;
                    FavoriteInfo.currentChapter := IntToStr(MangaInfo.chapterLinks.Count);
                  finally
                    DLManager.CS_DownloadManager_Task.Release;
                  end;
                  //mark downloaded
                  dlChapters.Clear;
                  GetParams(dlChapters, FavoriteInfo.downloadedChapterList);
                  dlChapters.AddStrings(NewMangaInfo.chapterLinks);
                  FavoriteInfo.downloadedChapterList := SetParams(dlChapters);
                end;
            end;
            Inc(counter);
          end;
          Backup;
          DLManager.Backup;
          if Assigned(OnUpdateDownload) then
          begin
            DLManager.SortNatural(DLManager.SortColumn);
            OnUpdateDownload;
          end;
          if LNCResult = ncrDownload then
            DLManager.CheckAndActiveTask;
          if Assigned(OnUpdateFavorite) then
            OnUpdateFavorite;
        end;
      end;
    finally
      //free used memory
      counter := 0;
      while counter < Favorites.Count do
      begin
        with Favorites[counter] do begin
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
  if Favorites.Count > 0 then
    for i := 0 to Favorites.Count - 1 do
      if (CompareText(Favorites[i].FavoriteInfo.Title, title) = 0) and
        (CompareText(Favorites[i].FavoriteInfo.website, website) = 0) then
        Exit(True);
  Result := False;
end;

function TFavoriteManager.IsMangaExistURL(const website, URL : String): Boolean;
Var
  i: Integer;
begin
  Result := False;
  if Favorites.Count > 0 then
    for i := 0 to Favorites.Count - 1 do
      if SameText(website, Favorites[i].FavoriteInfo.Website) and
        SameText(URL, Favorites[i].FavoriteInfo.Link) then
        Exit(True);
  Result := False;
end;

procedure TFavoriteManager.Add(
  const title, currentChapter, downloadedChapterList, website, saveTo, link: String);
begin
  if IsMangaExist(title, website) then Exit;
  CS_Favorites.Acquire;
  try
    Favorites.Add(TFavoriteContainer.Create);
    Favorites.Last.Manager := Self;
    Favorites.Last.FavoriteInfo.Title := title;
    Favorites.Last.FavoriteInfo.currentChapter := currentChapter;
    Favorites.Last.FavoriteInfo.website := website;
    Favorites.Last.FavoriteInfo.saveTo := saveTo;
    Favorites.Last.FavoriteInfo.Link := Link;
    Favorites.Last.FavoriteInfo.downloadedChapterList := downloadedChapterList;
    if not isRunning then
    begin
      Sort(sortColumn);
      Backup;
    end;
  finally
    CS_Favorites.Release;
  end;
end;

procedure TFavoriteManager.AddMerge(
  const title, currentChapter, downloadedChapterList, website, saveTo, link: String);
begin
  if IsMangaExist(title, website) then
    Exit;
  CS_Favorites.Acquire;
  try
    Favorites.Add(TFavoriteContainer.Create);
    Favorites.Last.Manager := Self;
    Favorites.Last.FavoriteInfo.Title := title;
    Favorites.Last.FavoriteInfo.currentChapter := currentChapter;
    Favorites.Last.FavoriteInfo.website := website;
    Favorites.Last.FavoriteInfo.saveTo := saveTo;
    Favorites.Last.FavoriteInfo.Link := Link;
    Favorites.Last.FavoriteInfo.downloadedChapterList := downloadedChapterList;
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
  Sort(sortColumn);
  Backup;

  SetLength(infos, 0);
  fStream.Free;
  mergeFile.Free;
  isRunning := False;
end;

procedure TFavoriteManager.Remove(const pos: Integer; const isBackup: Boolean);
begin
  if (not isRunning) and (pos < Favorites.Count) then
  begin
    CS_Favorites.Acquire;
    try
      Favorites[pos].Free;
      Favorites.Delete(pos);
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
      Favorites.Add(TFavoriteContainer.Create);
      Favorites.Last.Manager := Self;
      Favorites.Last.FavoriteInfo.Title := favoritesFile.ReadString(IntToStr(i), 'Title', '');
      Favorites.Last.FavoriteInfo.currentChapter :=
        favoritesFile.ReadString(IntToStr(i), 'CurrentChapter', '0');
      Favorites.Last.FavoriteInfo.downloadedChapterList :=
        favoritesFile.ReadString(IntToStr(i), 'DownloadedChapterList', '');
      Favorites.Last.FavoriteInfo.website := favoritesFile.ReadString(IntToStr(i), 'Website', '');
      Favorites.Last.FavoriteInfo.SaveTo :=
        CorrectPathSys(favoritesFile.ReadString(IntToStr(i), 'SaveTo', ''));
      Favorites.Last.FavoriteInfo.link := favoritesFile.ReadString(IntToStr(i), 'Link', '');
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

  favoritesFile.WriteInteger('general', 'NumberOfFavorites', Favorites.Count);
  if Favorites.Count > 0 then
    for i := 0 to Favorites.Count - 1 do
    begin
      favoritesFile.WriteString(IntToStr(i), 'Title', Favorites[i].FavoriteInfo.Title);
      favoritesFile.WriteString(IntToStr(i), 'CurrentChapter',
        Favorites[i].FavoriteInfo.currentChapter);
      favoritesFile.WriteString(IntToStr(i), 'DownloadedChapterList',
        Favorites[i].FavoriteInfo.downloadedChapterList);
      favoritesFile.WriteString(IntToStr(i), 'Website', Favorites[i].FavoriteInfo.Website);
      favoritesFile.WriteString(IntToStr(i), 'SaveTo', Favorites[i].FavoriteInfo.SaveTo);
      favoritesFile.WriteString(IntToStr(i), 'Link', Favorites[i].FavoriteInfo.link);
    end;
  favoritesFile.UpdateFile;
end;

procedure TFavoriteManager.StopAllAndWait;
begin
  if isRunning then
  begin
    taskthread.Terminate;
    taskthread.WaitFor;
  end;
end;

procedure TFavoriteManager.Sort(const AColumn: Cardinal);

  function GetStr(const ARow: Cardinal): String;
  begin
    case AColumn of
      1: Result := Favorites[ARow].FavoriteInfo.Title;
      2: Result := Favorites[ARow].FavoriteInfo.currentChapter;
      3: Result := Favorites[ARow].FavoriteInfo.website;
      4: Result := Favorites[ARow].FavoriteInfo.SaveTo;
    end;
  end;

  procedure QSort(L, R: Cardinal);
  var
    i, j: Cardinal;
    X: String;
  begin
    X := GetStr((L + R) div 2);
    i := L;
    j := R;
    while i <= j do
    begin
      case sortDirection of
        False:
        begin
          case AColumn of
            2:
            begin
              while StrToInt(GetStr(i)) < StrToInt(X) do
                Inc(i);
              while StrToInt(GetStr(j)) > StrToInt(X) do
                Dec(j);
            end
            else
            begin
              while StrComp(PChar(GetStr(i)), PChar(X)) < 0 do
                Inc(i);
              while StrComp(PChar(GetStr(j)), PChar(X)) > 0 do
                Dec(j);
            end;
          end;
        end;
        True:
        begin
          case AColumn of
            2:
            begin
              while StrToInt(GetStr(i)) > StrToInt(X) do
                Inc(i);
              while StrToInt(GetStr(j)) < StrToInt(X) do
                Dec(j);
            end
            else
            begin
              while StrComp(PChar(GetStr(i)), PChar(X)) > 0 do
                Inc(i);
              while StrComp(PChar(GetStr(j)), PChar(X)) < 0 do
                Dec(j);
            end;
          end;
        end;
      end;
      if i <= j then
      begin
        Favorites.Exchange(i,j);
        Inc(i);
        if j > 0 then
          Dec(j);
      end;
    end;
    if L < j then
      QSort(L, j);
    if i < R then
      QSort(i, R);
  end;

begin
  sortColumn := AColumn;
  QSort(0, Favorites.Count - 1);
end;

procedure TFavoriteManager.SortNatural(const AColumn: Integer);

  function Swap(const id1, id2: Integer): Boolean;
  begin
    if (id1 >= Favorites.Count) or (id2 >= Favorites.Count) then
      Exit(False);
    Favorites.Exchange(id1, id2);
    Result := True;
  end;

  function GetStr(const ARow: Cardinal): String;
  begin
    case AColumn of
      1: Result := Favorites[ARow].FavoriteInfo.Title;
      2: Result := Favorites[ARow].FavoriteInfo.currentChapter;
      3: Result := Favorites[ARow].FavoriteInfo.website;
      4: Result := Favorites[ARow].FavoriteInfo.SaveTo;
      else
        Result := '';
    end;
  end;

  procedure QuickSortA(L, R: Integer);
  var
    Pivot, vL, vR: Integer;
    PivotStr: String;
  begin
    if R - L <= 1 then
    begin // a little bit of time saver
      if L < R then
        if SortDirection then
          if AnsiNaturalCompare(GetStr(L), GetStr(R)) > 0 then
            Swap(L, R)
          else
          if AnsiNaturalCompare(GetStr(L), GetStr(R)) < 0 then
            Swap(L, R);
      Exit;
    end;
    vL := L;
    vR := R;
    Pivot := L + Random(R - L); // they say random is best
    PivotStr := GetStr(Pivot);
    while vL < vR do
    begin
      if SortDirection then
      begin
        while (vL < Pivot) and (AnsiNaturalCompare(GetStr(vL), PivotStr) > 0) do
          Inc(vL);
        while (vR > Pivot) and (AnsiNaturalCompare(GetStr(vR), PivotStr) <= 0) do
          Dec(vR);
      end
      else
      begin
        while (vL < Pivot) and (AnsiNaturalCompare(GetStr(vL), PivotStr) <= 0) do
          Inc(vL);
        while (vR > Pivot) and (AnsiNaturalCompare(GetStr(vR), PivotStr) > 0) do
          Dec(vR);
      end;
      Swap(vL, vR);
      if Pivot = vL then // swap pivot if we just hit it from one side
      begin
        Pivot := vR;
        PivotStr := GetStr(Pivot);
      end
      else
      if Pivot = vR then
      begin
        Pivot := vL;
        PivotStr := GetStr(Pivot);
      end;
    end;
    if Pivot - 1 >= L then
      QuickSortA(L, Pivot - 1);
    if Pivot + 1 <= R then
      QuickSortA(Pivot + 1, R);
  end;

begin
  sortColumn := AColumn;
  QuickSortA(0, Favorites.Count - 1);
end;

end.
