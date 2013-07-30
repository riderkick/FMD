{
        File: favorites.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit favorites;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, baseunit, data, fgl, downloads;

type
  TFavoriteManager = class;
  TFavoriteThread = class(TThread)
  protected
    FWebsite, FURL: String;
    procedure UpdateName;
    procedure EndPass0;
    procedure CallMainFormGetBatotoInfo;
    procedure Execute; override;
  public
    currentManga  : String;

    isTerminated,

    isSuspended   : Boolean;
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
    procedure   Remove(const pos: Cardinal; const isBackup: Boolean = TRUE);
    procedure   Restore;
    procedure   Backup;
  end;

implementation

uses mainunit;

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

procedure   TFavoriteThread.CallMainFormGetBatotoInfo;
begin
  getInfo.GetInfoFromURL(FWebsite, FURL, 5);
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
    if (manager.favoriteInfo[workPtr].website <> BATOTO_NAME) OR
       (NOT OptionBatotoUseIEChecked) then
    begin
      if manager.favoriteInfo[workPtr].website = MANGASTREAM_NAME then
        getInfo.mangaInfo.title:= manager.favoriteInfo[workPtr].title;
      if getInfo.GetInfoFromURL(manager.favoriteInfo[workPtr].website,
                                manager.favoriteInfo[workPtr].link, 5) = NET_PROBLEM then
        break;
    end
    else
    begin
      FWebsite:= manager.favoriteInfo[workPtr].website;
      FURL    := manager.favoriteInfo[workPtr].link;
      Synchronize(CallMainFormGetBatotoInfo);
    end;
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

constructor TFavoriteManager.Create;
begin
  numberOfThreads:= 1;
  newChapter:= TCardinalList.Create;
  isRunning:= FALSE;
  favorites:= TIniFile.Create(WORK_FOLDER + FAVORITES_FILE);
  favorites.CacheUpdates:= FALSE;
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
  isNewChapters   : array of Boolean;
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

begin
  MainForm.btFavoritesCheckNewChapter.Caption:= stFavoritesCheck;
  l:= TStringList.Create;
  SetLength(isNewChapters, Count);
  // check the result to see if theres any new chapter
  for i:= 0 to Count-1 do
  begin
    isNewChapters[i]:= FALSE;
    if (mangaInfo[i].website <> MANGASTREAM_NAME) AND
       (mangaInfo[i].website <> SUBMANGA_NAME) AND
       (mangaInfo[i].numChapter > StrToInt(favoriteInfo[i].currentChapter)) then
    begin
      isNewChapters[i]:= TRUE;
      Inc(newC);
    end
    else
    if (mangaInfo[i].website = MANGASTREAM_NAME) OR
       (mangaInfo[i].website = SUBMANGA_NAME) then
    begin
      l.Clear;
      GetParams(l, favoriteInfo[i].downloadedChapterList);
      if l.Count = 0 then
      begin
        isNewChapters[i]:= TRUE;
        Inc(newC);
      end
      else
      if mangaInfo[i].chapterLinks.Count <> 0 then
      begin
        for j:= 0 to mangaInfo[i].chapterLinks.Count-1 do
        begin
          if NOT Check(l, mangaInfo[i].chapterLinks.Strings[j]) then
          begin
            isNewChapters[i]:= TRUE;
            Inc(newC);
            break;
          end;
        end;
      end;
    end;

    if mangaInfo[i].status = '0' then
    begin
      if removeListStr = '' then
        removeListStr:= removeListStr + #10#13#10#13 + stDlgRemoveCompletedManga;
      removeListStr:= removeListStr + #10#13 + ' - ' + favoriteInfo[i].title + ' <'+mangaInfo[i].Website +'> ';
    end;
  end;

  if (isShowDialog) AND (newC = 0) then
    MessageDlg('', Format(stDlgNoNewChapter + '%s', [removeListStr]),
               mtInformation, [mbOk], 0)
  else
  begin
    newMangaStr:= '';

    // generate string for new chapter notification
    for i:= 0 to Count-1 do
    begin
      currentChapter:= StrToInt(favoriteInfo[i].currentChapter);
      newChapter    := mangaInfo[i].numChapter;
      if isNewChapters[i] then
      begin
        if favoriteInfo[i].Website <> MANGASTREAM_NAME then
          newMangaStr:= newMangaStr + #10#13+ ' - '+favoriteInfo[i].title + ' <'+ favoriteInfo[i].Website +'> ' + favoriteInfo[i].currentChapter+' -> '+IntToStr(newChapter)
        else
          newMangaStr:= newMangaStr + #10#13+ ' - '+favoriteInfo[i].title + ' <'+ favoriteInfo[i].Website +'>';
      end;
    end;

    if isShowDialog then
    begin
      if MessageDlg('',
                   Format(stDlgHasNewChapter + #10#13 + newMangaStr + '%s', [newC, removeListStr]),
                   mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        isNow:= TRUE;
        if MainForm.pcMain.PageIndex = 3 then
          MainForm.pcMain.PageIndex:= 0;
      end
      else
        isNow:= FALSE;
    end
    else
      isNow:= TRUE;
    for i:= 0 to Count-1 do
    begin
      newChapter:= mangaInfo[i].numChapter;
      if (favoriteInfo[i].Website <> MANGASTREAM_NAME) AND
         (favoriteInfo[i].website <> SUBMANGA_NAME) then
      begin
        currentChapter:= StrToInt(favoriteInfo[i].currentChapter);
      end
      else
      begin
        l.Clear;
        GetParams(l, favoriteInfo[i].downloadedChapterList);
        if l.Count <> 0 then
        begin
          currentChapter:= newChapter;
          for j:= 0 to mangaInfo[i].chapterLinks.Count-1 do
          begin
            if NOT Check(l, mangaInfo[i].chapterLinks.Strings[j]) then
            begin
              currentChapter:= j;
              break;
            end;
          end;
        end
        else
          currentChapter:= 0;
      end;

      if isNewChapters[i] then
      begin
        isHasNewChapter:= TRUE;
        newMangaStr:= newMangaStr + #10#13+ ' - '+favoriteInfo[i].title;
        DLManager.AddTask;
        pos:= DLManager.containers.Count-1;
        DLManager.containers.Items[pos].mangaSiteID:= GetMangaSiteID(mangaInfo[i].website);

        // generate download link
        for j:= currentChapter to newChapter-1 do
        begin
          s:= CustomRename(OptionCustomRename,
                           mangaInfo[i].website,
                           mangaInfo[i].title,
                           mangaInfo[i].chapterName.Strings[j],
                           Format('%.4d', [j+1]),
                           MainForm.cbOptionPathConvert.Checked);

          DLManager.containers.Items[pos].chapterName .Add(s);
          DLManager.containers.Items[pos].chapterLinks.Add(mangaInfo[i].chapterLinks.Strings[j]);
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

        // bad coding - update favorites's current chapter, and free pointers
        favoriteInfo[i].currentChapter:= IntToStr(mangaInfo[i].numChapter);
      end;
    end;
    if Assigned(OnUpdateDownload) then
      OnUpdateDownload;
    DLManager.Backup;
    if (isHasNewChapter) AND (isNow) then
      DLManager.CheckAndActiveTask;
  end;

  // update favorites's current chapter, and free pointers
  for i:= 0 to Count-1 do
  begin
    if mangaInfo[i].numChapter>0 then
    begin
      favoriteInfo[i].currentChapter:= IntToStr(mangaInfo[i].numChapter);
      if (mangaInfo[i].website = MANGASTREAM_NAME) OR
         (mangaInfo[i].website = SUBMANGA_NAME) then
      begin
        favoriteInfo[i].downloadedChapterList:= '';
        for j:= 0 to mangaInfo[i].numChapter-1 do
          favoriteInfo[i].downloadedChapterList:= favoriteInfo[i].downloadedChapterList+mangaInfo[i].chapterLinks.Strings[j]+SEPERATOR;
      end;
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
  while i < Count do
  begin
    if mangaInfo[i].status = '0' then
    begin
      Remove(i);
    end
    else
      Inc(i);
  end;

  if Assigned(OnUpdateFavorite) then
    OnUpdateFavorite;

  SetLength(isNewChapters, 0);
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
  if website = MANGASTREAM_NAME then
    favoriteInfo[Count-1].downloadedChapterList:= downloadedChapterList
  else
    favoriteInfo[Count-1].downloadedChapterList:= '';
  Backup;
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
end;

end.

