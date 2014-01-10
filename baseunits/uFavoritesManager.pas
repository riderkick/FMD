{
        File: uFavoritesManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFavoritesManager;

{$mode delphi}

{
  TODO:
    Improve multithreading feature.
}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, lazutf8classes, FileUtil, fgl,
  uBaseUnit, uData, uDownloadsManager, uFMDThread;

type
  TFavoriteManager = class;
  TFavoriteThread = class(TFMDThread)
  protected
    FWebsite, FURL: String;
    FPass0        : Boolean;
    // Temporarily not allow other threads update information
    procedure EnableLock;
    // Allows other threads to do the job
    procedure DisableLock;
    procedure UpdateName;
    procedure EndPass0;
    procedure Execute; override;
  public
    currentManga  : String;
    // ID of the thread (0 will be the leader)
    threadID      : Cardinal;
    // starting point
    workCounter   : Cardinal;

    getInfo       : TMangaInformation;
    // Owner (Manager) of this thread
    manager       : TFavoriteManager;
    constructor Create;
    destructor  Destroy; override;

    property    Pass0: Boolean read FPass0 write FPass0;
  end;

  TFavoriteThreadList = TFPGList<TFavoriteThread>;

  TFavoriteManager = class
  private
    FSortDirection: Boolean;
    FSortColumn   : Cardinal;
    FIsAuto,
    FIsShowDialog,
    // Return true if Favorites is checking
    FIsRunning      : Boolean;

    // Move all the task to the left, starting from pos
    procedure   MoveLeft(const pos: Cardinal);
    // Move all the task to the right, starting from pos
    procedure   MoveRight(const pos: Cardinal);
  public
    // store manga name that has new chapter after checking
    newMangaStr    : String;

    Lock            : Cardinal;
    // Number of mangas in Favorites
    Count           : Cardinal;
    // Number of mangas in Favorites - Before checking
    CountBeforeChecking: Cardinal;
    //
    favorites      : TIniFile;
    // All Favorites information
    favoriteInfo   : array of TFavoriteInfo;
    // mangaInfo for generating download tasks
    mangaInfo      : array of TMangaInfo;

    // Number of working thread
    // For now we always set it to 1
    numberOfThreads: Cardinal;
    // Working threads
    threads        : TFavoriteThreadList;
    // Download Manager (passed from mainunit.pas)
    // After favorites run completed, all download jobs will be add to DLManager
    DLManager      : TDownloadManager;

    OnUpdateFavorite: procedure of object;
    OnUpdateDownload: procedure of object;

    constructor Create;
    destructor  Destroy; override;

    procedure   Run;
    // Show notification form after checking completed
    procedure   ShowResult;
    // Return true if a manga exist in Favorites
    function    IsMangaExist(const title, website: String): Boolean;
    // Add new manga to the list
    procedure   Add(const title, currentChapter, downloadedChapterList, website, saveTo, link: String);
    // Merge manga information with a title that already exist in Favorites
    procedure   AddMerge(const title, currentChapter, downloadedChapterList, website, saveTo, link: String);
    // Merge a favorites.ini with another favorites.ini
    procedure   MergeWith(const APath: String);
    // Remove a manga from Favorites
    procedure   Remove(const pos: Cardinal; const isBackup: Boolean = TRUE);
    // Restore information from favorites.ini
    procedure   Restore;
    // Backup to favorites.ini
    procedure   Backup;

    // sorting
    procedure   Sort(const AColumn: Cardinal);

    property    SortDirection: Boolean read FSortDirection write FSortDirection;
    property    SortColumn: Cardinal read FSortColumn write FSortColumn;
    property    isAuto: Boolean read FIsAuto write FIsAuto;
    property    isShowDialog: Boolean read FIsShowDialog write FIsShowDialog;
    property    isRunning: Boolean read FIsRunning write FIsRunning;
  end;

implementation

uses
  frmMain, frmNewChapter;

// ----- TFavoriteThread -----

constructor TFavoriteThread.Create;
begin
  isTerminated:= FALSE;
  pass0:= FALSE;
  FreeOnTerminate:= TRUE;
  getInfo := TMangaInformation.Create;
  getInfo.isGetByUpdater:= FALSE;
  isSuspended:= TRUE;
  inherited Create(FALSE);
end;

destructor  TFavoriteThread.Destroy;
begin
  getInfo.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure TFavoriteThread.EnableLock;
begin
  Inc(manager.Lock);
end;

procedure TFavoriteThread.DisableLock;
begin
  Dec(manager.Lock);
end;

procedure   TFavoriteThread.UpdateName;
begin
  MainForm.btFavoritesCheckNewChapter.Caption:= stFavoritesChecking + ' '+currentManga;
end;

procedure   TFavoriteThread.EndPass0;
begin
  if threadID = 0 then
    manager.ShowResult;
end;

procedure   TFavoriteThread.Execute;
var
  i, count: Cardinal;
begin
  while isSuspended do Sleep(16);
  workCounter:= threadID;
  while (NOT Terminated) AND (workCounter < manager.CountBeforeChecking) do
  begin
    if manager.favoriteInfo[workCounter].website = WebsiteRoots[MANGASTREAM_ID, 0] then
      getInfo.mangaInfo.title:= manager.favoriteInfo[workCounter].title;
    // Put the title of the site that doesn't allow multi-connections in here
    if manager.favoriteInfo[workCounter].website = WebsiteRoots[EATMANGA_ID, 0] then
    begin
      while manager.Lock <> 0 do Sleep(32);
      Synchronize(EnableLock);
    end;
    currentManga:= manager.favoriteInfo[workCounter].title + ' <' + manager.favoriteInfo[workCounter].website + '>';
    Synchronize(UpdateName);
    getInfo.GetInfoFromURL(manager.favoriteInfo[workCounter].website,
                           manager.favoriteInfo[workCounter].link, 3);
    // Put the title of the site that doesn't allow multi-connections in here
    if manager.favoriteInfo[workCounter].website = WebsiteRoots[EATMANGA_ID, 0] then
    begin
      Synchronize(DisableLock);
    end;
    manager.mangaInfo[workCounter].chapterName := TStringList.Create;
    manager.mangaInfo[workCounter].chapterLinks:= TStringList.Create;
    TransferMangaInfo(manager.mangaInfo[workCounter], getInfo.mangaInfo);
    Inc(workCounter, manager.numberOfThreads);
  end;

  pass0:= TRUE;
  repeat
    Sleep(16);
    count:= 0;
    for i:= 0 to manager.threads.Count-1 do
      if (manager.threads[i].pass0) then
        Inc(count);
  until count = manager.numberOfThreads;

  Synchronize(EndPass0);

  isSuspended:= FALSE;
  while NOT isSuspended do Sleep(16);
end;

// ----- TFavoriteManager -----

procedure   TFavoriteManager.MoveLeft(const pos: Cardinal);
var
  i: Cardinal;
begin
  if pos < Count-1 then
    for i:= pos+1 to Count-1 do
      favoriteInfo[i-1]:= favoriteInfo[i];
end;

procedure   TFavoriteManager.MoveRight(const Pos: Cardinal);
var
  i: Cardinal;
begin
  if pos < Count-1 then
    for i:= Count-1 downto pos+1 do
      favoriteInfo[i]:= favoriteInfo[i-1];
end;

// ----- public methods -----

constructor TFavoriteManager.Create;
begin
  numberOfThreads:= 4;
  Lock:= 0;
  isRunning:= FALSE;
  favorites:= TIniFile.Create(WORK_FOLDER + FAVORITES_FILE);
  favorites.CacheUpdates:= TRUE;
  threads:= TFavoriteThreadList.Create;
  Restore;
  inherited Create;
end;

destructor  TFavoriteManager.Destroy;
begin
  Backup;
  threads.Free;
  favorites.UpdateFile;
  favorites.Free;
  SetLength(favoriteInfo, 0);
  SetLength(favoriteInfo, 0);
  inherited Destroy;
end;

procedure   TFavoriteManager.Run;
var
  i          : Cardinal;
  newMangaStr: String;
begin
  if (NOT isAuto) AND ((isRunning) OR (MainForm.silentThreadCount > 0)) then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
                mtInformation, [mbYes, mbNo], 0);
    exit;
  end
  else
  if (isAuto) AND ((isRunning) OR (MainForm.silentThreadCount > 0)) then
    exit;
  if Count = 0 then exit;
  if threads.Count > 0 then exit;
  MainForm.btFavoritesCheckNewChapter.Caption:= stFavoritesChecking;
  isRunning:= TRUE;

  CountBeforeChecking:= Count;
  SetLength(mangaInfo, Count);
  for i:= 0 to numberOfThreads-1 do
  begin
    threads.Add(TFavoriteThread.Create);
    threads.Items[i].threadID:= i;
    threads.Items[i].manager := self;
    threads.Items[i].isSuspended:= FALSE;
  end;
end;

procedure   TFavoriteManager.ShowResult;
var
  LNewChapter     : TNewChapter;
  LNCResult       : TNewChapterResult;
  // List of new chapter number for each title
  numberOfNewChapters: array of Cardinal;
  newChapterList  : array of TCardinalList;
  newChapterNames,
  newChapterURLs  : array of TStringList;
  l               : TStringList;
  isHasNewChapter : Boolean = FALSE;
  s, s1           : String;
  removeListStr   : String = '';
  hh, mm, ss, ms,
  day, month, year: Word;
  currentChapter,
  newChapter,
  pos,
  i, j, k: Cardinal;
  newC   : Cardinal = 0;
  isNow  : Boolean;

  // Check if a string is exists a list
  function  Check(const list: TStringList; const s: String): Boolean;
  var
    i: Cardinal;
  begin
    Result:= FALSE;
    for i:= 0 to list.Count-1 do
      if CompareStr(list.Strings[i], s)=0 then
        exit(TRUE);
  end;

  // Free all list, pointers when the checking is done
  procedure FreeLists;
  var
    i: Cardinal;
  begin
    if Length(newChapterURLs) = 0 then exit;
    for i:= 0 to CountBeforeChecking-1 do
      if (numberOfNewChapters[i] > 0) AND (Assigned(newChapterURLs[i])) then
      begin
        newChapterList[i].Free;
        newChapterURLs[i].Free;
        newChapterNames[i].Free;
      end;
    SetLength(newChapterList, 0);
    SetLength(numberOfNewChapters, 0);
    SetLength(newChapterURLs, 0);
    SetLength(newChapterNames, 0);
    SetLength(mangaInfo, 0);
  end;

begin
  MainForm.btFavoritesCheckNewChapter.Caption:= stFavoritesCheck;
  l:= TStringList.Create;
  SetLength(newChapterList, CountBeforeChecking);
  SetLength(numberOfNewChapters, CountBeforeChecking);
  SetLength(newChapterURLs, CountBeforeChecking);
  SetLength(newChapterNames, CountBeforeChecking);
  // check the result to see if theres any new chapter
  for i:= 0 to CountBeforeChecking-1 do
  begin
    numberOfNewChapters[i]:= 0;
    begin
      l.Clear;
      GetParams(l, favoriteInfo[i].downloadedChapterList);
      if (l.Count = 0) AND (mangaInfo[i].chapterLinks.Count <> 0) then
      begin
       // isNewChapters[i]:= TRUE;
       // Inc(newC);
      end
      else
      if mangaInfo[i].chapterLinks.Count <> 0 then
      begin
        for j:= 0 to mangaInfo[i].chapterLinks.Count-1 do
        begin
          if NOT Check(l, mangaInfo[i].chapterLinks.Strings[j]) then
          begin
            if numberOfNewChapters[i] = 0 then
            begin
              Inc(newC);
              newChapterList[i]:= TCardinalList.Create;
              newChapterURLs[i]:= TStringList.Create;
              newChapterNames[i]:= TStringList.Create;
            end;
            Inc(numberOfNewChapters[i]);
            newChapterList[i].Add(j);
            newChapterURLs[i].Add(mangaInfo[i].chapterLinks.Strings[j]);
            newChapterNames[i].Add(mangaInfo[i].chapterName.Strings[j]);
          //  break;
          end;
        end;
      end;
    end;

    // generate remove completed manga string
    if (OptionAutoRemoveCompletedManga) AND (mangaInfo[i].status = '0') then
    begin
      if removeListStr = '' then
        removeListStr:= removeListStr + #13#13 + stDlgRemoveCompletedManga;
      removeListStr:= removeListStr + #13 + '- ' + favoriteInfo[i].title + ' <'+mangaInfo[i].Website +'> ';
    end;
  end;

  if (newC = 0) then
  begin
    if (removeListStr <> '') AND (isShowDialog) then
    begin
      frmMain.MainForm.Show;
      LNewChapter:= TNewChapter.Create(MainForm);
      LNewChapter.lbNotification.Caption:= Format(stDlgHasNewChapter, [newC]);
      LNewChapter.mmMemo.Lines.Add(TrimLeft(removeListStr));
      LNewChapter.btDownload.Visible:= FALSE;
      LNewChapter.btQueue.Visible:= FALSE;
      LNewChapter.ShowModal;
      LNCResult:= LNewChapter.FormResult;
      LNewChapter.Free;
    end;
  end
  else
  begin
    newMangaStr:= '';

    // generate string for new chapter notification
    for i:= 0 to CountBeforeChecking-1 do
    begin
      currentChapter:= StrToInt(favoriteInfo[i].currentChapter);
      newChapter    := mangaInfo[i].numChapter;
      if numberOfNewChapters[i] > 0 then
      begin
        newMangaStr:= newMangaStr + #13+ '- '+Format(stFavoritesHasNewChapter, [favoriteInfo[i].title, favoriteInfo[i].Website, numberOfNewChapters[i]]);
      end;
    end;

    if isShowDialog then
    begin
      frmMain.MainForm.Show;
      LNewChapter:= TNewChapter.Create(MainForm);
      LNewChapter.lbNotification.Caption:= Format(stDlgHasNewChapter, [newC]);
      LNewChapter.mmMemo.Lines.Add(TrimLeft(newMangaStr) + #13#13 + TrimLeft(removeListStr));
      LNewChapter.ShowModal;
      LNCResult:= LNewChapter.FormResult;
      LNewChapter.Free;
      if LNCResult = ncrDownload then
      begin
        isNow:= TRUE;
        if MainForm.pcMain.PageIndex <> 0 then
          MainForm.pcMain.PageIndex:= 0;
      end
      else
      if LNCResult = ncrCancel then
      begin
        // TODO: Bad coding - need improments
        for i:= 0 to CountBeforeChecking-1 do
        begin
          mangaInfo[i].chapterName .Free;
          mangaInfo[i].chapterLinks.Free;
        end;
        while threads.Count > 0 do
        begin
          threads.Items[0].Terminate;
          threads.Items[0]:= nil;
          threads.Delete(0);
        end;
        isRunning:= FALSE;
        FreeLists;
        exit;
        // end of bad code
      end
      else
      if LNCResult = ncrQueue then
        isNow:= FALSE;
    end
    else
      isNow:= TRUE;

    DLManager.isRunningBackup:= TRUE;

    for i:= 0 to CountBeforeChecking-1 do
    begin
      currentChapter:= StrToInt(favoriteInfo[i].currentChapter);

      if numberOfNewChapters[i] > 0 then
      begin
        isHasNewChapter:= TRUE;
        newMangaStr:= newMangaStr + #10#13+ ' - '+favoriteInfo[i].title;
        DLManager.AddTask;
        pos:= DLManager.containers.Count-1;
        DLManager.containers.Items[pos].mangaSiteID:= GetMangaSiteID(mangaInfo[i].website);

        // generate download link

        if newChapterURLs[i].Count>0 then
          for j:= 0 to newChapterURLs[i].Count-1 do
          begin
            s:= CustomRename(OptionCustomRename,
                             mangaInfo[i].website,
                             favoriteInfo[i].title,
                             newChapterNames[i].Strings[j],
                             Format('%.4d', [newChapterList[i].Items[j]+1]),
                             MainForm.cbOptionPathConvert.Checked);

            DLManager.containers.Items[pos].chapterName .Add(s);
            DLManager.containers.Items[pos].chapterLinks.Add(newChapterURLs[i].Strings[j]);
          end;

        // mark downloaded chapters
        s:= '';
        if mangaInfo[i].chapterLinks.Count = 0 then exit;
        newChapter:= mangaInfo[i].chapterLinks.Count;
        if currentChapter < newChapter-1 then
        begin
          for k:= currentChapter to newChapter-1 do
          begin
            s:= s+IntToStr(k) + SEPERATOR;
          end;
          if s <> '' then
            DLManager.AddToDownloadedChaptersList(favoriteInfo[i].website + favoriteInfo[i].link, s);
        end;

        if NOT isNow then
        begin
          DLManager.containers.Items[pos].downloadInfo.Status:= stStop;
          DLManager.containers.Items[pos].Status:= STATUS_STOP;
        end
        else
        begin
          DLManager.containers.Items[pos].downloadInfo.Status:= stWait;
          DLManager.containers.Items[pos].Status:= STATUS_WAIT;
        end;
        DLManager.containers.Items[pos].currentDownloadChapterPtr:= 0;
        // DLManager.activeThreadsPerTask.Add(DLManager.maxDLThreadsPerTask);
        DLManager.containers.Items[pos].downloadInfo.title  := favoriteInfo[i].title;
        DLManager.containers.Items[pos].downloadInfo.Website:= favoriteInfo[i].website;
        DLManager.containers.Items[pos].downloadInfo.SaveTo := favoriteInfo[i].SaveTo;
        DecodeDate(Now, year, month, day);
        DecodeTime(Time, hh, mm, ss, ms);
        DLManager.containers.Items[pos].downloadInfo.dateTime:= IntToStr(Month)+'/'+IntToStr(Day)+'/'+IntToStr(Year)+' '+IntToStr(hh)+':'+IntToStr(mm)+':'+IntToStr(ss);

        // TODO: bad coding - update favorites's current chapter, and free pointers
        // should in here
        favoriteInfo[i].currentChapter:= IntToStr(mangaInfo[i].numChapter);
        Sleep(4);
      end;
    end;
    DLManager.isRunningBackup:= FALSE;
    if Assigned(OnUpdateDownload) then
    begin
      MainForm.DLManager.Sort(MainForm.vtDownload.Header.SortColumn);
      OnUpdateDownload;
    end;
    if (isHasNewChapter) AND (isNow) then
    begin
      DLManager.CheckAndActiveTask;
      Sleep(64);
      DLManager.Backup;
    end;
  end;

  // update favorites's current chapter, and free pointers
  for i:= 0 to CountBeforeChecking-1 do
  begin
    if mangaInfo[i].numChapter>0 then
    begin
      favoriteInfo[i].currentChapter:= IntToStr(mangaInfo[i].numChapter);
      favoriteInfo[i].downloadedChapterList:= '';
      for j:= 0 to mangaInfo[i].numChapter-1 do
        favoriteInfo[i].downloadedChapterList:= favoriteInfo[i].downloadedChapterList+mangaInfo[i].chapterLinks.Strings[j]+SEPERATOR;
    end;
    mangaInfo[i].chapterName .Free;
    mangaInfo[i].chapterLinks.Free;
  end;

  while threads.Count > 0 do
  begin
    threads.Items[0].Terminate;
    threads.Items[0]:= nil;
    threads.Delete(0);
  end;
  isRunning:= FALSE;

  i:= 0; j:= 0;
  if OptionAutoRemoveCompletedManga then
  begin
    while i < CountBeforeChecking do
    begin
      if mangaInfo[j].status = '0' then
      begin
        Remove(i);
        Dec(CountBeforeChecking);
      end
      else
        Inc(i);
      Inc(j);
    end;
  end;

  if Assigned(OnUpdateFavorite) then
    OnUpdateFavorite;

  FreeLists;
  l.Free;

  // Sort the list before backing up
  //Sort(self.SortColumn);

  Backup;
end;

function    TFavoriteManager.IsMangaExist(const title, website: String): Boolean;
var
  i: Cardinal;
begin
  if Count > 0 then
    for i:= 0 to Count-1 do
      if (CompareStr(favoriteInfo[i].title, title) = 0) AND
         (CompareStr(favoriteInfo[i].website, website) = 0) then
        exit(TRUE);
  Result:= FALSE;
end;

procedure   TFavoriteManager.Add(const title, currentChapter, downloadedChapterList, website,
                                 saveTo, link: String);
var
  i: Cardinal;
begin
  try
    if IsMangaExist(title, website) then
      exit;
    Inc(Count);
    SetLength(favoriteInfo, Count);
    favoriteInfo[Count-1].title         := title;
    favoriteInfo[Count-1].currentChapter:= currentChapter;
    favoriteInfo[Count-1].website       := website;
    favoriteInfo[Count-1].saveTo        := saveTo;
    favoriteInfo[Count-1].Link          := Link;
    favoriteInfo[Count-1].downloadedChapterList:= downloadedChapterList;
    if ((MainForm.silentAddToFavThreadCount <= 2) OR (Random(50)=0)) AND
       (NOT isRunning)then
    begin
      Sort(sortColumn);
      Backup;
    end;
  except

  end;
end;

procedure   TFavoriteManager.AddMerge(const title, currentChapter, downloadedChapterList, website,
                                      saveTo, link: String);
var
  i: Cardinal;
begin
  try
    if IsMangaExist(title, website) then
      exit;
    Inc(Count);
    SetLength(favoriteInfo, Count);
    favoriteInfo[Count-1].title         := title;
    favoriteInfo[Count-1].currentChapter:= currentChapter;
    favoriteInfo[Count-1].website       := website;
    favoriteInfo[Count-1].saveTo        := saveTo;
    favoriteInfo[Count-1].Link          := Link;
    favoriteInfo[Count-1].downloadedChapterList:= downloadedChapterList;
  except
    on E: Exception do ;
  end;
end;

procedure   TFavoriteManager.MergeWith(const APath: String);
var
  mergeFile: TIniFile;
  fstream  : TFileStreamUTF8;
  l, i     : Cardinal;
  infos    : array of TFavoriteInfo;
begin
  if isRunning then exit;
  if NOT FileExistsUTF8(APath) then exit;
  isRunning:= TRUE;

  fstream:= TFileStreamUTF8.Create(APath, fmOpenRead);
  mergeFile:= TIniFile.Create(fstream);

  l:= mergeFile.ReadInteger('general', 'NumberOfFavorites', 0);
  if l > 0 then
  begin
    SetLength(infos, l);
    for i:= 0 to l-1 do
    begin
      infos[i].title         := mergeFile.ReadString(IntToStr(i), 'Title', '');
      infos[i].currentChapter:= mergeFile.ReadString(IntToStr(i), 'CurrentChapter', '0');
      infos[i].downloadedChapterList:= mergeFile.ReadString(IntToStr(i), 'DownloadedChapterList', '');
      infos[i].website       := mergeFile.ReadString(IntToStr(i), 'Website', '');
      infos[i].SaveTo        := mergeFile.ReadString(IntToStr(i), 'SaveTo', '');
      infos[i].link          := mergeFile.ReadString(IntToStr(i), 'Link', '');

      AddMerge(infos[i].title,
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
  isRunning:= FALSE;
end;

procedure   TFavoriteManager.Remove(const pos: Cardinal; const isBackup: Boolean = TRUE);
begin
  if isRunning then exit;
  if pos >= Count then exit;
  MoveLeft(pos);
  SetLength(favoriteInfo, Count-1);
  Dec(Count);
  if isBackup then
    Backup;
end;

procedure   TFavoriteManager.Restore;
var
  i: Cardinal;
begin
  Count:= favorites.ReadInteger('general', 'NumberOfFavorites', 0);
  SetLength(favoriteInfo, Count);
  if Length(favoriteInfo) = 0 then exit;
  for i:= 0 to Length(favoriteInfo) - 1 do
  begin
    favoriteInfo[i].title         := favorites.ReadString(IntToStr(i), 'Title', '');
    favoriteInfo[i].currentChapter:= favorites.ReadString(IntToStr(i), 'CurrentChapter', '0');
    favoriteInfo[i].downloadedChapterList:= favorites.ReadString(IntToStr(i), 'DownloadedChapterList', '');
    favoriteInfo[i].website       := favorites.ReadString(IntToStr(i), 'Website', '');
    favoriteInfo[i].SaveTo        := favorites.ReadString(IntToStr(i), 'SaveTo', '');
    favoriteInfo[i].link          := favorites.ReadString(IntToStr(i), 'Link', '');
  end;
end;

procedure   TFavoriteManager.Backup;
var
  i: Cardinal;
begin
  // delete olf info
  if favorites.ReadInteger('general', 'NumberOfFavorites', 0)<> 0 then
    for i:= 0 to favorites.ReadInteger('general', 'NumberOfFavorites', 0) - 1 do
      favorites.EraseSection(IntToStr(i));

  favorites.WriteInteger('general', 'NumberOfFavorites', Length(favoriteInfo));
  if Length(favoriteInfo) = 0 then exit;
  for i:= 0 to Length(favoriteInfo)-1 do
  begin
    favorites.WriteString(IntToStr(i), 'Title',          favoriteInfo[i].title);
    favorites.WriteString(IntToStr(i), 'CurrentChapter', favoriteInfo[i].currentChapter);
    favorites.WriteString(IntToStr(i), 'DownloadedChapterList', favoriteInfo[i].downloadedChapterList);
    favorites.WriteString(IntToStr(i), 'Website',        favoriteInfo[i].Website);
    favorites.WriteString(IntToStr(i), 'SaveTo',         favoriteInfo[i].SaveTo);
    favorites.WriteString(IntToStr(i), 'Link',           favoriteInfo[i].link);
  end;
  favorites.UpdateFile;
end;

procedure   TFavoriteManager.Sort(const AColumn: Cardinal);

  function  GetStr(const ARow: Cardinal): String;
  begin
    case AColumn of
      0: Result:= favoriteInfo[ARow].title;
      1: Result:= favoriteInfo[ARow].currentChapter;
      2: Result:= favoriteInfo[ARow].website;
    end;
  end;

  procedure QSort(L, R: Cardinal);
  var i, j: Cardinal;
         X: String;
       tmp: TFavoriteInfo;
  begin
    X:= GetStr((L+R) div 2);
    i:= L;
    j:= R;
    while i<=j do
    begin
      case sortDirection of
        FALSE:
          begin
            case AColumn of
              1:
                begin
                  while StrToInt(GetStr(i)) < StrToInt(X) do Inc(i);
                  while StrToInt(GetStr(j)) > StrToInt(X) do Dec(j);
                end
              else
                begin
                  while StrComp(PChar(GetStr(i)), PChar(X))<0 do Inc(i);
                  while StrComp(PChar(GetStr(j)), PChar(X))>0 do Dec(j);
                end;
            end;
          end;
        TRUE:
          begin
            case AColumn of
              1:
                begin
                  while StrToInt(GetStr(i)) > StrToInt(X) do Inc(i);
                  while StrToInt(GetStr(j)) < StrToInt(X) do Dec(j);
                end
              else
                begin
                  while StrComp(PChar(GetStr(i)), PChar(X))>0 do Inc(i);
                  while StrComp(PChar(GetStr(j)), PChar(X))<0 do Dec(j);
                end;
            end;
          end;
      end;
      if i<=j then
      begin
        tmp:= favoriteInfo[i];
        favoriteInfo[i]:= favoriteInfo[j];
        favoriteInfo[j]:= tmp;
        Inc(i);
        if j > 0 then
          Dec(j);
      end;
    end;
    if L < j then QSort(L, j);
    if i < R then QSort(i, R);
  end;

var
  i: Cardinal;

begin
  sortColumn:= AColumn;
  QSort(0, Length(favoriteInfo)-1);
end;

end.

