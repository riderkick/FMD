{
        File: downloads.pas
        License: GPLv3
        This unit is part of Free Manga Downloader
}

unit downloads;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, baseunit, data, fgl, zip;

type
  TDownloadManager = class;
  TTaskThread = class;

  // this class will replace the old TDownloadManager
  TNewDownloadThread = class(TThread)
  protected
    function    GetLinkPageFromURL(const URL: AnsiString): Boolean;
    function    GetPageNumberFromURL(const URL: AnsiString): Boolean;
    procedure   Execute; override;
  public
    checkStyle    : Cardinal;

    isTerminated,
    isSuspended   : Boolean;
    // ID of the site
    workPtr       : Cardinal;
    manager       : TTaskThread;
    constructor Create;
    destructor  Destroy; override;
  end;

  TTaskThread = class(TThread)

  public
    isTerminated,
    isSuspended: Boolean;

    workPtr    : Cardinal;
    mangaSiteID: Cardinal;
    pageNumber : Cardinal;
    pageLink   : TStringList;
    manager    : TDownloadManager;
    threads    : array of TNewDownloadThread;

    constructor Create;
    destructor  Destroy; override;
    procedure   StopAllTasks;
  end;

  // deprecated - will be replace later
  TDownloadThread = class(TThread)
  protected
    procedure   Execute; override;

    // en: Get page number from a chapter
    // vi: Lấy số trang từ 1 chương
    function    GetLinkPageFromURL(const URL: AnsiString): Boolean;
    function    GetPageNumberFromURL(const URL: AnsiString): Boolean;
    function    DownloadPage: Boolean;

    procedure   Compress;
    procedure   IncreasePrepareProgress;
    procedure   IncreaseDownloadProgress;
  public
    isTerminate   : Boolean;
    chapterPtr    : Cardinal;
    // ID of the thread (0 will be the leader)
    threadID      : Cardinal;
    // ID of the task it served
    taskID        : Cardinal;
    // ID of the site
    mangaSiteID   : Cardinal;
    // starting point
    workPtr       : Cardinal;
    // number of pages of a chapter
    pageNumber    : Cardinal;
    // The pointer to the manager
    manager       : TDownloadManager;

    pass0,
    pass1,
    pass2,
    lastPass      : Boolean;

    constructor Create;
  end;

  TDownloadThreadList = TFPGList<TDownloadThread>;

  TDownloadManager = class(TObject)
  private
   // procedure   LeftThread(const Pos: Cardinal);
   // procedure   RightThread(const Pos: Cardinal);
    procedure   LeftTask(const Pos: Cardinal);
    procedure   RightTask(const Pos: Cardinal);
  public

    isFinishTaskAccessed: Boolean;

    compress,
    // number of tasks
    retryConnect,
    maxDLTasks,
    maxDLThreadsPerTask,
    numberOfTasks       : Cardinal;

    chapterName,
    chapterLinks,
    pageLinks           : array of TStringList;
    // task status (finished, paused, ect)
    taskStatus          : TByteList;
    // number of active thread per task
    activeThreadsPerTask: TByteList;

    // current chapterLinks which thread is processing
    pageNumbers,
    chapterPtr          : TCardinalList;
    threads             : TDownloadThreadList;//array of TDownloadThread;

    ini                 : TIniFile;

    downloadInfo        : array of TDownloadInfo;
    constructor Create;
    destructor  Destroy; override;

    procedure   Restore;
    procedure   Backup;

    procedure   AddTask;
    procedure   CheckAndActiveTask;
    procedure   ActiveTask(const taskID: Cardinal);
    procedure   StopTask(const taskID: Cardinal);
    procedure   StopAllTasks;
    procedure   FinishTask(const taskID: Cardinal);

    procedure   RemoveThread(const pos: Cardinal);
    procedure   RemoveTask(const taskID: Cardinal);
    procedure   RemoveFinishedTasks;
  end;

implementation

uses mainunit;

// ----- TNewDownloadThread -----

constructor TNewDownloadThread.Create;
begin
  isTerminated:= FALSE;
  isSuspended := TRUE;
  FreeOnTerminate:= TRUE;
  inherited Create(FALSE);
end;

destructor  TNewDownloadThread.Destroy;
begin
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TNewDownloadThread.Execute;
begin
  while isSuspended do Sleep(100);
end;

function    TNewDownloadThread.GetPageNumberFromURL(const URL: AnsiString): Boolean;
  function GetAnimeAPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l), ANIMEA_ROOT +
                                 StringReplace(URL, '.html', '', []) +
                                 '-page-1.html',
                                 manager.manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('Page 1 of ', l.Strings[i])<>0) then
      begin
        manager.pageNumber:= StrToInt(GetString(l.Strings[i], 'Page 1 of ', '<'));
        break;
      end;
    l.Free;
  end;

begin
  manager.pageNumber:= 0;
  if manager.mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeAPageNumber;
end;

function    TNewDownloadThread.GetLinkPageFromURL(const URL: AnsiString): Boolean;
  function GetAnimeALinkPage: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     ANIMEA_ROOT +
                     StringReplace(URL, '.html', '', []) +
                     '-page-'+IntToStr(workPtr+1)+'.html',
                     manager.manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('class="mangaimg', l.Strings[i])<>0) then
      begin
        manager.pageLink.Strings[workPtr]:= GetString(l.Strings[i], '<img src="', '"');
        break;
      end;
    l.Free;
  end;

begin
  if manager.mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeALinkPage;
end;

// ----- TTaskThread -----

constructor TTaskThread.Create;
begin
  isTerminated:= FALSE;
  isSuspended := TRUE;
  FreeOnTerminate:= TRUE;
  SetLength(threads, 0);
  inherited Create(FALSE);
end;

destructor  TTaskThread.Destroy;
begin
  StopAllTasks;
  pageLink.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TTaskThread.StopAllTasks;
var
  i, l: Cardinal;
begin
  l:= Length(threads);
  if l > 0 then
  begin
    for i:= 0 to l-1 do
      threads[i].Terminate;
    SetLength(threads, 0);
  end;
end;

// ----- TDownloadThread -----

constructor TDownloadThread.Create;
begin
  FreeOnTerminate:= TRUE;
  inherited Create(TRUE);
end;

procedure   TDownloadThread.Compress;
var
  Compresser: TCompress;
begin
  if (threadID = 0) AND (manager.compress = 1) then
  begin
    Sleep(100);
    Compresser:= TCompress.Create;
    Compresser.Path:= manager.downloadInfo[taskID].SaveTo+'/'+
                      manager.chapterName[taskID].Strings[manager.chapterPtr.Items[taskID]-1];
    Compresser.Execute;
    Compresser.Free;
  end;
end;

procedure   TDownloadThread.IncreasePrepareProgress;
begin
  manager.downloadInfo[taskID].Progress:=
    IntToStr(manager.downloadInfo[taskID].iProgress+1)+'/'+IntToStr(pageNumber);
  Inc(manager.downloadInfo[taskID].iProgress);
  MainForm.vtDownload.Repaint;
end;

procedure   TDownloadThread.IncreaseDownloadProgress;
begin
  manager.downloadInfo[taskID].Progress:=
    IntToStr(manager.downloadInfo[taskID].iProgress+1)+'/'+IntToStr(manager.pageLinks[taskID].Count);
  Inc(manager.downloadInfo[taskID].iProgress);
  MainForm.vtDownload.Repaint;
end;

procedure   TDownloadThread.Execute;
var
  i, count, sum: Cardinal;
begin
  i:= manager.activeThreadsPerTask.Count;
  pass0:= FALSE;
  pass1:= FALSE;
  pass2:= FALSE;
  lastPass:= FALSE;
  while manager.chapterPtr.Items[taskID] < manager.chapterLinks[taskID].Count do
  begin
    // prepare
    chapterPtr:= manager.chapterPtr.Items[taskID];
    // pass 0
    pass0:= TRUE;
    repeat
      Sleep(16);
      count:= 0;
      for i:= 0 to manager.threads.Count-1 do
        if (manager.threads[i].taskID = taskID) AND
           (manager.threads[i].pass0) then
          Inc(count);
    until count = manager.activeThreadsPerTask.Items[taskID];
    // pass0:= FALSE;

    // pass 1
    if manager.pageLinks[taskID].Count = 0 then
    begin
      if threadID = 0 then
      begin
        CheckPath(manager.downloadInfo[taskID].SaveTo+
                  '/'+
                  manager.chapterName[taskID].Strings[manager.chapterPtr.Items[taskID]]);
        GetPageNumberFromURL(manager.chapterLinks[taskID].Strings[manager.chapterPtr.Items[taskID]]);
        for i:= 0 to pageNumber-1 do
          manager.pageLinks[taskID].Add('');
        // sync page number to all the thread have the same taskID
        for i:= 0 to manager.threads.Count - 1 do
          if taskID = manager.threads[i].taskID then
          begin
            manager.threads[i].pageNumber:= pageNumber;
            manager.threads[i].pass1:= TRUE;
          end;
      end;

      if threadID = 0 then
      begin
        manager.downloadInfo[taskID].iProgress:= 0;
        manager.downloadInfo[taskID].Status:= Format('%s (%d/%d)', [stPreparing, chapterPtr+1, manager.chapterLinks[taskID].Count]);
      end;
      repeat
        Sleep(16);
        count:= 0;
        for i:= 0 to manager.threads.Count-1 do
          if (manager.threads[i].taskID = taskID) AND
             (manager.threads[i].pass1) then
            Inc(count);
      until count = manager.activeThreadsPerTask.Items[taskID];

      workPtr:= threadID;
      while (NOT Terminated) AND (workPtr <= pageNumber-1) do
      begin
        GetLinkPageFromURL(manager.chapterLinks[taskID].Strings[manager.chapterPtr.Items[taskID]]);
        Sleep(16);
        Inc(workPtr, manager.activeThreadsPerTask.Items[taskID]);
        Synchronize(IncreasePrepareProgress);
      end;
   // pass1:= FALSE;
      pass2:= TRUE;

    // pass 2

      repeat
        if threadID = 0 then
          manager.downloadInfo[taskID].iProgress:= 0;
        Sleep(16);
        count:= 0;
        for i:= 0 to manager.threads.Count-1 do
          if (manager.threads[i].taskID = taskID) AND
             (manager.threads[i].pass2) then
            Inc(count);
      until count = manager.activeThreadsPerTask.Items[taskID];
    end
    else
    begin
      pass1:= TRUE;
      pass2:= TRUE;
    end;
    if threadID = 0 then
      manager.downloadInfo[taskID].Status:= Format('%s (%d/%d)', [stDownloading, chapterPtr+1, manager.chapterLinks[taskID].Count]);

    workPtr:= threadID;
    while (NOT Terminated) AND (workPtr <= manager.pageLinks[taskID].Count-1) do
    begin
      DownloadPage;
      Sleep(16);
      Inc(workPtr, manager.activeThreadsPerTask.Items[taskID]);
      Synchronize(IncreaseDownloadProgress);
    end;

    // prepare to download another chapter or exit

    pass0:= FALSE;
    pass1:= FALSE;
    repeat
      if threadID = 0 then
        Inc(manager.chapterPtr.Items[taskID]);
      Sleep(16);
      count:= 0;
      for i:= 0 to manager.threads.Count-1 do
        if (manager.threads[i].taskID = taskID) AND
           (manager.threads[i].workPtr > manager.pageLinks[taskID].Count-1) AND
           (chapterPtr <> manager.chapterPtr.Items[taskID]) then
          Inc(count);
    until count = manager.activeThreadsPerTask.Items[taskID];
    Compress;
    if threadID = 0 then
      manager.pageLinks[taskID].Clear;
    pass2:= FALSE;
  end;

  // last pass - raise finish flag
  lastPass:= TRUE;
  isTerminate:= TRUE;
  repeat
    Sleep(16);
    count:= 0;
    for i:= 0 to manager.threads.Count-1 do
      if (manager.threads[i].taskID = taskID) AND
         (manager.threads[i].lastPass) then
        Inc(count);
  until count = manager.activeThreadsPerTask.Items[taskID];

  // ID 0 will do the finish part
  if threadID = 0 then
  begin
    Sleep(1000);
    manager.downloadInfo[taskID].Progress:= '';
    while manager.isFinishTaskAccessed do Sleep(100);
    manager.isFinishTaskAccessed:= TRUE;
    manager.FinishTask(taskID);
    Terminate;
  end
  else
    Suspend;
end;

function    TDownloadThread.GetPageNumberFromURL(const URL: AnsiString): Boolean;
  function GetAnimeAPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l), ANIMEA_ROOT +
                                 StringReplace(URL, '.html', '', []) +
                                 '-page-1.html',
                                 manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('Page 1 of ', l.Strings[i])<>0) then
      begin
        pageNumber:= StrToInt(GetString(l.Strings[i], 'Page 1 of ', '<'));
        break;
      end;
    l.Free;
  end;

begin
  pageNumber:= 0;
  if mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeAPageNumber;
end;

function    TDownloadThread.GetLinkPageFromURL(const URL: AnsiString): Boolean;
  function GetAnimeALinkPage: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     ANIMEA_ROOT +
                     StringReplace(URL, '.html', '', []) +
                     '-page-'+IntToStr(workPtr+1)+'.html',
                     manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('class="mangaimg', l.Strings[i])<>0) then
      begin
        manager.pageLinks[taskID].Strings[workPtr]:= GetString(l.Strings[i], '<img src="', '"');
        break;
      end;
    l.Free;
  end;

begin
  if mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeALinkPage;
end;

function    TDownloadThread.DownloadPage: Boolean;
var
  s, ext: String;
begin
  if manager.pageLinks[taskID].Strings[workPtr] = '' then exit;
  s:= manager.pageLinks[taskID].Strings[workPtr];
  if (Pos('.jpeg', LowerCase(s))<>0) OR (Pos('.jpg', LowerCase(s))<>0) then
    ext:= '.jpg'
  else
  if Pos('.png', LowerCase(s))<>0 then
    ext:= '.png'
  else
  if Pos('.gif', LowerCase(s))<>0 then
    ext:= '.gif';
  SetCurrentDir(oldDir);
  SavePage(manager.pageLinks[taskID].Strings[workPtr],
           Format('%s/%.3d%s',
                  [manager.downloadInfo[taskID].SaveTo+
                  '/'+manager.chapterName[taskID].Strings[chapterPtr],
                  workPtr+1, ext]), manager.retryConnect);
  manager.pageLinks[taskID].Strings[workPtr]:= '';
  SetCurrentDir(oldDir);
end;

// ----- TDownloadManager -----

constructor TDownloadManager.Create;
begin
  // Create INI file
  ini:= TIniFile.Create(WORK_FOLDER + WORK_FILE);
  ini.CacheUpdates:= FALSE;

  // Some of these maybe unecessary because they are already in Restore
  numberOfTasks:= 0;
  SetLength(chapterLinks, 0);
  SetLength(chapterName, 0);
  SetLength(pageLinks, 0);
  taskStatus          := TByteList.Create;
  activeThreadsPerTask:= TByteList.Create;
  chapterPtr          := TCardinalList.Create;
  threads             := TDownloadThreadList.Create;
  SetLength(downloadInfo, 0);

  isFinishTaskAccessed:= FALSE;

  inherited Create;

  // Restore old INI file
  Restore;
end;

destructor  TDownloadManager.Destroy;
var i: Cardinal;
begin
  if Length(chapterLinks) <> 0 then
    for i:= 0 to Length(chapterLinks)-1 do
      chapterLinks[i].Free;
  if Length(chapterName) <> 0 then
    for i:= 0 to Length(chapterName)-1 do
      chapterName[i].Free;
  if threads.Count <> 0 then
    for i:= 0 to threads.Count-1 do
      if NOT threads.Items[i].isTerminate then
        threads.Items[i].Terminate;

  SetLength(chapterLinks, 0);
  SetLength(chapterName, 0);
  SetLength(pageLinks, 0);
  threads.Free;
  SetLength(downloadInfo, 0);
  taskStatus.Free;
  activeThreadsPerTask.Free;
  chapterPtr.Free;
  inherited Destroy;
end;

procedure   TDownloadManager.Restore;
var
  s: AnsiString;
  i: Cardinal;
begin
  // Restore general information first
  numberOfTasks        := ini.ReadInteger('general', 'NumberOfTasks', 0);
  SetLength(chapterLinks, ini.ReadInteger('general', 'NumberOfChapterLinks', 0));
  SetLength(chapterName , ini.ReadInteger('general', 'NumberOfChapterName', 0));
  SetLength(pageLinks   , ini.ReadInteger('general', 'NumberOfpageLinks', 0));

  if numberOfTasks > 0 then
  begin
    SetLength(downloadInfo, numberOfTasks);

    // Restore chapter links, chapter name and page links
    if Length(chapterLinks) > 0 then
    begin
      for i:= 0 to Length(chapterLinks)-1 do
      begin
        if NOT Assigned(chapterLinks[i]) then
          chapterLinks[i]:= TStringList.Create;
        if NOT Assigned(chapterName [i]) then
          chapterName [i]:= TStringList.Create;
        if NOT Assigned(pageLinks [i]) then
          pageLinks   [i]:= TStringList.Create;
        chapterLinks[i].Clear;
        chapterName [i].Clear;
        pageLinks   [i].Clear;
        s:= ini.ReadString('ChapterLinks', 'Strings'+IntToStr(i), '');
        if s <> '' then
          GetParams(chapterLinks[i], s);
        s:= ini.ReadString('ChapterName', 'Strings'+IntToStr(i), '');
        if s <> '' then
          GetParams(chapterName[i], s);
        s:= ini.ReadString('PageLinks', 'Strings'+IntToStr(i), '');
        if s <> '' then
          GetParams(pageLinks[i], s);
      end;
    end;
    activeThreadsPerTask.Clear;
    taskStatus.Clear;
    chapterPtr.Clear;

    // Restore task information
    for i:= 0 to numberOfTasks-1 do
    begin
      activeThreadsPerTask.Add(1);

      taskStatus.Add(ini.ReadInteger('task'+IntToStr(i), 'TaskStatus', 0));
      chapterPtr.Add(ini.ReadInteger('task'+IntToStr(i), 'ChapterPtr', 0));

      downloadInfo[i].title    := ini.ReadString('task'+IntToStr(i), 'Title', '');
      downloadInfo[i].status   := ini.ReadString('task'+IntToStr(i), 'Status', '');
     // downloadInfo[i].fProgress:= ini.ReadFloat ('task'+IntToStr(i), 'Progress', 0);
      downloadInfo[i].website  := ini.ReadString('task'+IntToStr(i), 'Website', '');
      downloadInfo[i].saveTo   := ini.ReadString('task'+IntToStr(i), 'SaveTo', '');
      downloadInfo[i].dateTime := ini.ReadString('task'+IntToStr(i), 'DateTime', '');
    end;
  end;
end;

procedure   TDownloadManager.Backup;
var
  i: Cardinal;
begin
  // Erase all sections
  ini.EraseSection('ChapterLinks');
  ini.EraseSection('ChapterName');
  ini.EraseSection('PageLinks');
  for i:= 0 to ini.ReadInteger('general', 'NumberOfTasks', 0) do
    ini.EraseSection('task'+IntToStr(i));
  ini.EraseSection('general');

  // backup
  if numberOfTasks > 0 then
  begin
    ini.WriteInteger('general', 'NumberOfTasks', numberOfTasks);
    ini.WriteInteger('general', 'NumberOfChapterLinks', Length(chapterLinks));
    ini.WriteInteger('general', 'NumberOfChapterName', Length(chapterName));
    ini.WriteInteger('general', 'NumberOfpageLinks', Length(pageLinks));

    for i:= 0 to Length(chapterLinks)-1 do
      ini.WriteString('ChapterLinks', 'Strings'+IntToStr(i), SetParams(chapterLinks[i]));
    for i:= 0 to Length(chapterName)-1 do
      ini.WriteString('ChapterName', 'Strings'+IntToStr(i), SetParams(ChapterName[i]));
    if Length(pageLinks) > 0 then
      for i:= 0 to Length(pageLinks)-1 do
        ini.WriteString('PageLinks', 'Strings'+IntToStr(i), SetParams(pageLinks[i]));
    for i:= 0 to numberOfTasks-1 do
    begin
      ini.WriteInteger('task'+IntToStr(i), 'TaskStatus', taskStatus.Items[i]);
      ini.WriteInteger('task'+IntToStr(i), 'ChapterPtr', chapterPtr.Items[i]);

      ini.WriteString ('task'+IntToStr(i), 'Title', downloadInfo[i].title);
      ini.WriteString ('task'+IntToStr(i), 'Status', downloadInfo[i].status);
     // ini.WriteFloat  ('task'+IntToStr(i), 'Progress', downloadInfo[i].fProgress);
      ini.WriteString ('task'+IntToStr(i), 'Website', downloadInfo[i].website);
      ini.WriteString ('task'+IntToStr(i), 'SaveTo', downloadInfo[i].saveTo);
      ini.WriteString ('task'+IntToStr(i), 'DateTime', downloadInfo[i].dateTime);
    end;
  end;
  ini.UpdateFile;
end;

procedure   TDownloadManager.AddTask;
var
  pos: Cardinal;
begin
  Inc(numberOfTasks);
  pos:= numberOfTasks-1;
  SetLength(chapterLinks, numberOfTasks);
  SetLength(chapterName , numberOfTasks);
  SetLength(downloadInfo, numberOfTasks);
  SetLength(pageLinks   , numberOfTasks);
  if NOT Assigned(pageLinks[numberOfTasks-1]) then
    pageLinks[numberOfTasks-1]:= TStringList.Create;

  chapterLinks[pos]:= TStringList.Create;
  chapterName [pos]:= TStringList.Create;
end;

procedure   TDownloadManager.CheckAndActiveTask;
var
  i    : Cardinal;
  count: Cardinal = 0;
begin
  if numberOfTasks>0 then
    for i:= 0 to numberOfTasks-1 do
    begin
      if taskStatus.Items[i] = STATUS_WAIT then
      begin
        ActiveTask(i);
        Inc(count);
        if count >= maxDLTasks then
          exit;
      end;
    end;
end;

procedure   TDownloadManager.ActiveTask(const taskID: Cardinal);
var
  i, pos: Cardinal;
begin
  // conditions
  if taskID >= numberOfTasks then exit;
  if (taskStatus.Items[taskID] = STATUS_DOWNLOAD) AND
     (taskStatus.Items[taskID] = STATUS_PREPARE) AND
     (taskStatus.Items[taskID] = STATUS_FINISH) then exit;
  if numberOfTasks>0 then
  begin
    pos:= 0;
    for i:= 0 to numberOfTasks-1 do
    begin
      if taskStatus.Items[i] = STATUS_DOWNLOAD then
        Inc(pos);
    end;
  end;
  if pos >= maxDLTasks then exit;

  // check if the manager had any active task yet
  if threads.Count <> 0 then
    for i:= 0 to threads.Count-1 do
      if (threads.Items[i].taskID = taskID) AND (NOT threads.Items[i].Suspended) then
        exit;

  if activeThreadsPerTask.Count-1<taskID then
    activeThreadsPerTask.Add(maxDLThreadsPerTask)
  else
    activeThreadsPerTask.Items[taskID]:= maxDLThreadsPerTask;
  taskStatus.Items[taskID]:= STATUS_DOWNLOAD;
  for i:= 0 to maxDLThreadsPerTask-1 do
  begin
    pos:= threads.Count;
    threads.Add(TDownloadThread.Create);
    threads.Items[pos].manager    := self;
    threads.Items[pos].taskID     := taskID;
    threads.Items[pos].threadID   := i;
    threads.Items[pos].workPtr    := i;
    threads.Items[pos].mangaSiteID:= GetMangaSiteID(downloadInfo[taskID].website);
    threads.Items[pos].Resume;
  end;
  // TODO
end;

procedure   TDownloadManager.StopTask(const taskID: Cardinal);
var
  i: Cardinal;
begin
  // conditions
  if taskID >= numberOfTasks then exit;
  if (taskStatus.Items[taskID] <> STATUS_DOWNLOAD) AND
     (taskStatus.Items[taskID] <> STATUS_PREPARE) AND
     (taskStatus.Items[taskID] <> STATUS_WAIT) then exit;
  // check and stop any active thread
  if (taskStatus.Items[taskID] = STATUS_DOWNLOAD) then
    if threads.Count <> 0 then
    begin
      for i:= 0 to threads.Count -1 do
        if threads.Items[i].taskID = taskID then
          threads.Items[i].Suspend;
      i:= 0;
      repeat
        if threads.Items[i].taskID = taskID then
          RemoveThread(i)
        else
          Inc(i);
      until i >= threads.Count;
    end;
  downloadInfo[taskID].Progress:= '';
  downloadInfo[taskID].Status  := stStop;
  taskStatus.Items[taskID]:= STATUS_STOP;
  Backup;
  Sleep(1000);
  CheckAndActiveTask;
end;

procedure   TDownloadManager.StopAllTasks;
var
  i: Cardinal;
begin
  if numberOfTasks = 0 then exit;
  // check and stop any active thread
  if threads.Count > 0 then
  begin
    for i:= 0 to threads.Count -1 do
      threads.Items[i].Suspend;
    repeat
      RemoveThread(0)
    until threads.Count = 0;
  end;

  for i:= 0 to numberOfTasks-1 do
  begin
    if (taskStatus.Items[i] = STATUS_DOWNLOAD) then
    begin
      downloadInfo[i].Progress:= '';
      downloadInfo[i].Status  := stStop;
      taskStatus.Items[i]     := STATUS_STOP;
    end;
  end;
  Backup;
end;

procedure   TDownloadManager.FinishTask(const taskID: Cardinal);
var
  i: Cardinal;
begin
  if taskID >= numberOfTasks then exit;
  if (taskStatus.Items[taskID] <> STATUS_DOWNLOAD) then exit;

  if threads.Count = 0 then exit;

  // remove finished threads
  i:= 0;
  repeat
    if (threads.Items[i].taskID = taskID) then
      RemoveThread(i)
    else
      Inc(i);
  until i >= threads.Count;

  // set information
  pageLinks[taskID].Clear;
  taskStatus.Items[taskID]:= STATUS_FINISH;
  downloadInfo[taskID].Status:= stFinish;
  MainForm.vtDownload.Repaint;
  Backup;
  CheckAndActiveTask;
  isFinishTaskAccessed:= FALSE;
end;

procedure   TDownloadManager.RemoveThread(const pos: Cardinal);
begin
  if pos < threads.Count then
  begin
    if threads.Items[pos] <> nil then
    begin
      threads.Items[pos].Terminate;
      threads.Items[pos]:= nil;
    end;
    threads.Delete(pos);
  end;
end;

procedure   TDownloadManager.RemoveTask(const taskID: Cardinal);
var
  i: Cardinal;
begin
  if taskID >= numberOfTasks then exit;
  // remove and sync thread's taskID
  if threads.Count > 0 then
  begin
    for i:= 0 to threads.Count-1 do
      threads.Items[i].Suspend;
    i:= 0;
    repeat
      if threads.Items[i].taskID = taskID then
        RemoveThread(i)
      else
        Inc(i);
    until i >= threads.Count;
    if threads.Count > 0 then
      for i:= 0 to threads.Count-1 do
        if threads.Items[i].taskID > taskID then
          Dec(threads.Items[i].taskID);
  end;
  // remove other infos
  if chapterName[taskID] <> nil then
  begin
    chapterName [taskID].Free; chapterName [taskID]:= nil;
    chapterLinks[taskID].Free; chapterLinks[taskID]:= nil;
  end;
  if pageLinks[taskID] <> nil then
  begin
    pageLinks[taskID].Free; pageLinks[taskID]:= nil;
  end;
  LeftTask(taskID);
  Dec(numberOfTasks);
  SetLength(chapterName , numberOfTasks);
  SetLength(chapterLinks, numberOfTasks);
  SetLength(pageLinks   , numberOfTasks);
  SetLength(downloadInfo, numberOfTasks);

  taskStatus.Delete(taskID);
  activeThreadsPerTask.Delete(taskID);
  chapterPtr.Delete(taskID);
  // remuse threads
  if threads.Count > 0 then
    for i:= 0 to threads.Count-1 do
      threads.Items[i].Resume;
end;

procedure   TDownloadManager.RemoveFinishedTasks;
var
  i, j: Cardinal;
begin
  if numberOfTasks = 0 then exit;
  // remove and sync thread's taskID
  if threads.Count > 0 then
  begin
    for i:= 0 to threads.Count-1 do
      threads.Items[i].Suspend;
  end;
  // remove other infos
  i:= 0;
  repeat
    if taskStatus.Items[i] = STATUS_FINISH then
    begin
      if chapterName[i] <> nil then
      begin
        chapterName [i].Free; chapterName [i]:= nil;
        chapterLinks[i].Free; chapterLinks[i]:= nil;
      end;
      if pageLinks[i] <> nil then
      begin
        pageLinks[i].Free; pageLinks[i]:= nil;
      end;
      LeftTask(i);
      Dec(numberOfTasks);
      SetLength(chapterName , numberOfTasks);
      SetLength(chapterLinks, numberOfTasks);
      SetLength(pageLinks   , numberOfTasks);
      SetLength(downloadInfo, numberOfTasks);

      taskStatus.Delete(i);
      activeThreadsPerTask.Delete(i);
      chapterPtr.Delete(i);

      if threads.Count > 0 then
        for j:= 0 to threads.Count-1 do
          if threads.Items[j].taskID > i then
            Dec(threads.Items[j].taskID);
    end
    else
      Inc(i);
  until i >= numberOfTasks;

  // remuse threads
  if threads.Count > 0 then
    for i:= 0 to threads.Count-1 do
      threads.Items[i].Resume;
end;

{procedure   TDownloadManager.LeftThread(const pos: Cardinal);
var
  i, m: Cardinal;
begin
  m:= Length(threads);
  if Pos+1 < m-1 then
    for i:= Pos+1 to m-1 do
      threads[i-1]:= threads[i];
end;

procedure   TDownloadManager.RightThread(const pos: Cardinal);
var
  i, m: Cardinal;
begin
  m:= Length(threads);
  if Pos+1 < m-1 then
    for i:= m-1 downto Pos+1 do
      threads[i]:= threads[i-1];
end;}

procedure   TDownloadManager.LeftTask(const pos: Cardinal);
var
  i: Cardinal;
begin
  if pos < numberOfTasks-1 then
    for i:= pos+1 to numberOfTasks-1 do
    begin
      chapterLinks[i-1]:= chapterLinks[i];
      chapterName[i-1]:= chapterName[i];
      pageLinks[i-1]:= pageLinks[i];
      downloadInfo[i-1]:= downloadInfo[i];
    end;
end;

procedure   TDownloadManager.RightTask(const Pos: Cardinal);
var
  i: Cardinal;
begin
  if pos < numberOfTasks-1 then
    for i:= numberOfTasks-1 downto pos+1 do
    begin
      chapterLinks[i]:= chapterLinks[i-1];
      chapterName[i]:= chapterName[i-1];
      pageLinks[i]:= pageLinks[i-1];
      downloadInfo[i]:= downloadInfo[i-1];
    end;
end;

end.

