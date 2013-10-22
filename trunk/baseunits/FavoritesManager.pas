{
        File: FavoritesManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit FavoritesManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, lazutf8classes, FileUtil,
  baseunit, data, fgl, DownloadsManager, FMDThread;

type
  TFavoriteManager = class;
  TFavoriteThread = class(TFMDThread)
  protected
    FWebsite, FURL: String;
    procedure UpdateName;
    procedure EndPass0;
    procedure Execute; override;
  public
    currentManga  : String;
    // ID of the thread (0 will be the leader)
    threadID      : Cardinal;
    // starting point
    workPtr       : Cardinal;

    getInfo       : TMangaInformation;

    pass0         : Boolean;

    manager       : TFavoriteManager;
    constructor Create;
    destructor  Destroy; override;
  end;

  TFavoriteThreadList = TFPGList<TFavoriteThread>;

  TFavoriteManager = class
  private
    FSortDirection: Boolean;
    FSortColumn   : Cardinal;

    procedure   Left(const pos: Cardinal);
    procedure   Right(const pos: Cardinal);
  public
    // store manga name that has new chapter after checking
    newMangaStr    : String;
    isAuto,
    isShowDialog,
    isRunning      : Boolean;
    Count          : Cardinal;
    favorites      : TIniFile;
    favoriteInfo   : array of TFavoriteInfo;
    // mangaInfo for generating download tasks
    mangaInfo      : array of TMangaInfo;
    newChapter     : TCardinalList;

    numberOfThreads: Cardinal;
    threads        : TFavoriteThreadList;

    DLManager      : TDownloadManager;

    OnUpdateFavorite: procedure of object;
    OnUpdateDownload: procedure of object;

    constructor Create;
    destructor  Destroy; override;

    procedure   Run;
    procedure   ShowResult;
    function    IsMangaExist(const title, website: String): Boolean;
    procedure   Add(const title, currentChapter, downloadedChapterList, website, saveTo, link: String);
    procedure   AddMerge(const title, currentChapter, downloadedChapterList, website, saveTo, link: String);
    procedure   MergeWith(const APath: String);
    procedure   Remove(const pos: Cardinal; const isBackup: Boolean = TRUE);
    procedure   Restore;
    procedure   Backup;

    // sorting
    procedure   Sort(const AColumn: Cardinal);

    property    SortDirection: Boolean read FSortDirection write FSortDirection;
    property    SortColumn: Cardinal read FSortColumn write FSortColumn;
  end;

implementation

uses
  mainunit, NewChapterForm;

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
  workPtr:= threadID;
  while (NOT Terminated) AND (workPtr <= manager.Count-1) do
  begin
    currentManga:= manager.favoriteInfo[workPtr].title + ' <' + manager.favoriteInfo[workPtr].website + '>';
    Synchronize(UpdateName);
    if manager.favoriteInfo[workPtr].website = MANGASTREAM_NAME then
      getInfo.mangaInfo.title:= manager.favoriteInfo[workPtr].title;
    getInfo.GetInfoFromURL(manager.favoriteInfo[workPtr].website,
                              manager.favoriteInfo[workPtr].link, 4);
    manager.mangaInfo[workPtr].chapterName := TStringList.Create;
    manager.mangaInfo[workPtr].chapterLinks:= TStringList.Create;
    TransferMangaInfo(manager.mangaInfo[workPtr], getInfo.mangaInfo);
    Inc(workPtr, manager.numberOfThreads);
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

procedure   TFavoriteManager.Left(const pos: Cardinal);
var
  i: Cardinal;
begin
  if pos < Count-1 then
    for i:= pos+1 to Count-1 do
    begin
      favoriteInfo[i-1]:= favoriteInfo[i];
      mangaInfo   [i-1]:= mangaInfo   [i];
    end;
end;

procedure   TFavoriteManager.Right(const Pos: Cardinal);
var
  i: Cardinal;
begin
  if pos < Count-1 then
    for i:= Count-1 downto pos+1 do
    begin
      favoriteInfo[i]:= favoriteInfo[i-1];
      mangaInfo   [i]:= mangaInfo   [i-1];
    end;
end;

// ----- public methods -----

constructor TFavoriteManager.Create;
begin
  numberOfThreads:= 1;
  newChapter:= TCardinalList.Create;
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
  newChapter.Free;
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
  for i:= 0 to numberOfThreads-1 do
  begin
    threads.Add(TFavoriteThread.Create);
    threads.Items[i].threadID:= i;
    threads.Items[i].manager := self;
    threads.Items[i].isSuspended:= FALSE;
  end;
end;

// SubManga & MangaStream use a different checking method
procedure   TFavoriteManager.ShowResult;
var
  LNewChapter     : TNewChapter;
  LNCResult       : TNewChapterResult;
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

  function  Check(const list: TStringList; const s: String): Boolean;
  var
    i: Cardinal;
  begin
    Result:= FALSE;
    for i:= 0 to list.Count-1 do
      if CompareStr(list.Strings[i], s)=0 then
        exit(TRUE);
  end;

  procedure FreeLists;
  var
    i: Cardinal;
  begin
    if Length(newChapterURLs) = 0 then exit;
    for i:= 0 to Count-1 do
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
  end;

begin
  MainForm.btFavoritesCheckNewChapter.Caption:= stFavoritesCheck;
  l:= TStringList.Create;
  SetLength(newChapterList, Count);
  SetLength(numberOfNewChapters, Count);
  SetLength(newChapterURLs, Count);
  SetLength(newChapterNames, Count);
  // check the result to see if theres any new chapter
  for i:= 0 to Count-1 do
  begin
    numberOfNewChapters[i]:= 0;
    begin
      l.Clear;
      GetParams(l, favoriteInfo[i].downloadedChapterList);
      // for mangafox only (necessary ?)
      if (favoriteInfo[i].Website = MANGAFOX_NAME) AND (l.Count > 0) then
        for j:= 0 to l.Count-1 do
          l.Strings[j]:= StringReplace(l.Strings[j], WebsiteRoots[MANGAFOX_ID,1], '', []);

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

  if {(isShowDialog) AND }(newC = 0) then
   // MessageDlg('', Format(stDlgNoNewChapter + '%s', [removeListStr]),
   //            mtInformation, [mbOk], 0)
  begin
    if (removeListStr <> '') AND (isShowDialog) then
    begin
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
    for i:= 0 to Count-1 do
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
        if (MainForm.tvDownloadFilter.Selected.AbsoluteIndex <> 0) OR
           (MainForm.tvDownloadFilter.Selected.AbsoluteIndex <> 2) then
          MainForm.tvDownloadFilter.Items[2].Selected:= TRUE;
      end
      else
      if LNCResult = ncrCancel then
      begin
        // TODO: Bad coding - need improments
        for i:= 0 to Count-1 do
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

    for i:= 0 to Count-1 do
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
  for i:= 0 to Count-1 do
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

  i:= 0;
  if OptionAutoRemoveCompletedManga then
  begin
    while i < Count do
    begin
      if mangaInfo[i].status = '0' then
      begin
        Remove(i);
      end
      else
        Inc(i);
    end;
  end;

  if Assigned(OnUpdateFavorite) then
    OnUpdateFavorite;

  FreeLists;
  l.Free;
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
    if isRunning then exit;
    if IsMangaExist(title, website) then
      exit;
    Inc(Count);
    SetLength(favoriteInfo, Count);
    SetLength(mangaInfo, Count);
    favoriteInfo[Count-1].title         := title;
    favoriteInfo[Count-1].currentChapter:= currentChapter;
    favoriteInfo[Count-1].website       := website;
    favoriteInfo[Count-1].saveTo        := saveTo;
    favoriteInfo[Count-1].Link          := Link;
    favoriteInfo[Count-1].downloadedChapterList:= downloadedChapterList;
    if (MainForm.silentAddToFavThreadCount <= 2) OR (Random(50)=0) then
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
    SetLength(mangaInfo, Count);
    favoriteInfo[Count-1].title         := title;
    favoriteInfo[Count-1].currentChapter:= currentChapter;
    favoriteInfo[Count-1].website       := website;
    favoriteInfo[Count-1].saveTo        := saveTo;
    favoriteInfo[Count-1].Link          := Link;
    favoriteInfo[Count-1].downloadedChapterList:= downloadedChapterList;
  except

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
  Left(pos);
  SetLength(favoriteInfo, Count-1);
  SetLength(mangaInfo   , Count-1);
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
  SetLength(mangaInfo   , Count);
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
      tmp2: TMangaInfo;
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

        tmp2:= mangaInfo[i];
        mangaInfo[i]:= mangaInfo[j];
        mangaInfo[j]:= tmp2;
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

