{
        File: uDownloadsManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uDownloadsManager;

{$mode delphi}

{$IF FPC_FULLVERSION >= 20701}
  {$DEFINE FPC271}
{$ENDIF}

interface

uses
  LazFileUtils, FastHTMLParser, HTMLUtil, SynaCode, RegExpr, IniFiles, Classes,
  SysUtils, ExtCtrls, typinfo, fgl, blcksock, MultiLog, uBaseUnit, uPacker,
  uMisc, DownloadedChaptersDB, FMDOptions, httpsendthread, DownloadsDB,
  BaseThread, dateutils, strutils;

type
  TDownloadStatusType = (
    STATUS_STOP,
    STATUS_WAIT,
    STATUS_PREPARE,
    STATUS_DOWNLOAD,
    STATUS_FINISH,
    STATUS_COMPRESS,
    STATUS_PROBLEM,
    STATUS_FAILED,
    STATUS_NONE        // devault value oncreate, don't use
    );
  TDownloadStatusTypes = set of TDownloadStatusType;

  TDownloadManager = class;
  TTaskContainer = class;
  TTaskThread = class;

  { TDownloadThread }

  TDownloadThread = class(TBaseThread)
  private
    parse: TStringList;
  public
    // Get download link from URL
    function GetLinkPageFromURL(const URL: String): Boolean;
    // Get number of download link from URL
    function GetPageNumberFromURL(const URL: String): Boolean;
    // Download image
    function DownloadImage: Boolean;

    procedure OnTag({%H-}NoCaseTag, ActualTag: String);
    procedure OnText(Text: String);

    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    function GetPage(var output: TObject; URL: String;
      const Reconnect: Integer = 0): Boolean; inline;

    function SaveImage(const mangaSiteID: Integer; URL: String;
      const Path, Name: String; const Reconnect: Integer = 0): Boolean; overload;

    procedure Execute; override;
  public
    FHTTP: THTTPSendThread;
    Task: TTaskThread;
    WorkId: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TDownloadThreads = TFPGList<TDownloadThread>;

  { TTaskThread }

  TTaskThread = class(TBaseThread)
  private
    FCS_THREADS: TRTLCriticalSection;
    FCheckAndActiveTaskFlag: Boolean;
    FCurrentWorkingDir: String;
    {$IFDEF Windows}
    FCurrentMaxFileNameLength: Integer;
    {$ENDIF}
    FCurrentCustomFileName: String;
    FIsForDelete: Boolean;
    procedure SetCurrentWorkingDir(AValue: String);
    procedure SetIsForDelete(AValue: Boolean);
    procedure SyncShowBallonHint;
  protected
    procedure CheckOut;
    procedure Execute; override;
    procedure Compress;
    procedure SyncStop;
    // show notification when download completed
    procedure ShowBalloonHint;
  public
    //additional parameter
    httpCookies: String;
    Flag: TFlagType;
    // container (for storing information)
    Container: TTaskContainer;
    // download threads
    Threads: TDownloadThreads;
    constructor Create;
    destructor Destroy; override;
    function GetFileName(const AWorkId: Integer): String;
    property CurrentWorkingDir: String read FCurrentWorkingDir write SetCurrentWorkingDir;
    property CurrentMaxFileNameLength: Integer read FCurrentMaxFileNameLength;
    // current custom filename with only %FILENAME% left intact
    property CurrentCustomFileName: String read FCurrentCustomFileName write FCurrentCustomFileName;
    property IsForDelete: Boolean read FIsForDelete write SetIsForDelete;
  end;

  { TTaskContainer }

  TTaskContainer = class
  private
    FWebsite: String;
    FStatus: TDownloadStatusType;
    FEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetStatus(AValue: TDownloadStatusType);
    procedure SetWebsite(AValue: String);
  public
    DlId: Integer;
    // critical section
    CS_Container: TRTLCriticalSection;
    // read count for transfer rate
    ReadCount: Integer;
    // task thread of this container
    Task: TTaskThread;
    // download manager
    Manager: TDownloadManager;
    DownloadInfo: TDownloadInfo;
    // current link index
    CurrentPageNumber,
    // current chapter index
    CurrentDownloadChapterPtr,
    WorkCounter,
    DownCounter,
    PageNumber: Integer;
    MangaSiteID: Integer;
    ModuleId: Integer;
    //Status: TDownloadStatusType;
    ThreadState: Boolean;
    ChapterName,
    ChapterLinks,
    FailedChapterName,
    FailedChapterLinks,
    PageContainerLinks,
    PageLinks: TStringList;
    FileNames: TStringList;
    // custom filename
    CustomFileName: String;
    constructor Create;
    destructor Destroy; override;
    procedure IncReadCount(const ACount: Integer);
    procedure SaveToDB(const AOrder: Integer = -1);
  public
    Visible: Boolean;
    property Website: String read FWebsite write SetWebsite;
    property Status: TDownloadStatusType read FStatus write SetStatus;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TTaskContainers = TFPGList<TTaskContainer>;

  { TDownloadManager }

  TDownloadManager = class
  private
    FSortDirection: Boolean;
    FSortColumn: Integer;
    FDownloadsDB: TDownloadsDB;
    procedure AddItemsActiveTask(const Item: TTaskContainer);
    procedure RemoveItemsActiveTask(const Item: TTaskContainer);
    function GetTask(const TaskId: Integer): TTaskContainer;
    function ConvertToDB: Boolean;
  protected
    function GetTaskCount: Integer; inline;
    function GetTransferRate: Integer;
    procedure ChangeStatusCount(const OldStatus, NewStatus: TDownloadStatusType);
    procedure DecStatusCount(const Status: TDownloadStatusType);
    procedure IncStatusCount(const Status: TDownloadStatusType);
  public
    CS_Task,
    CS_ItemsActiveTask: TRTLCriticalSection;
    Items,
    ItemsActiveTask: TTaskContainers;
    isRunningBackup, isFinishTaskAccessed, isRunningBackupDownloadedChaptersList,
    isReadyForExit: Boolean;

    // status count
    CS_StatusCount: TRTLCriticalSection;
    StatusCount: array [TDownloadStatusType] of Integer;
    // disabled count
    DisabledCount: Integer;

    compress, retryConnect,
    // max. active tasks
    maxDLTasks,
    // max. download threads per task
    maxDLThreadsPerTask: Integer;

    //downloaded chapter list database
    DownloadedChapters: TDownloadedChaptersDB;

    //exit counter
    ExitWaitOK: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetTaskCount;

    procedure Restore;
    procedure Backup;

    // These methods relate to highlight downloaded chapters.
    procedure GetDownloadedChaptersState(const Alink: String;
      var Chapters: array of TChapterStateItem);

    // Add new task to the list.
    function AddTask: Integer;
    // Check and active previous work-in-progress tasks.
    procedure CheckAndActiveTaskAtStartup;
    // Check and active waiting tasks.
    procedure CheckAndActiveTask(const isCheckForFMDDo: Boolean = False);
    // Active a stopped task.
    procedure SetTaskActive(const taskID: Integer);
    procedure ActiveTask(const taskID: Integer);
    // Stop a download/wait task.
    procedure StopTask(const taskID: Integer; const isCheckForActive: Boolean =
      True; isWaitFor: Boolean = False);
    // Start all task
    procedure StartAllTasks;
    // Stop all download/wait tasks.
    procedure StopAllTasks;
    // Stop all download task inside a task before terminate the program.
    procedure StopAllDownloadTasksForExit;
    // Free then delete task without any check, use with caution
    procedure FreeAndDelete(const TaskId: Integer);
    // Remove a task from list.
    procedure RemoveTask(const TaskID: Integer);
    // Remove all finished tasks.
    procedure RemoveAllFinishedTasks;
    // check status of task
    function TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
    // enable task
    procedure EnableTask(const TaskId: Integer);
    procedure DisableTask(const TaskId: Integer);

    // Sort
    procedure Sort(const AColumn: Integer);

    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property TransferRate: Integer read GetTransferRate;
    property Task[const TaskId: Integer]: TTaskContainer read GetTask; default;
  end;

resourcestring
  RS_FailedToCreateDir = 'Failed to create directory!';
  RS_FailedTryResumeTask = 'Failed, try resuming this task!';
  RS_Preparing = 'Preparing';
  RS_Downloading = 'Downloading';
  RS_Stopped = 'Stopped';
  RS_Finish = 'Completed';
  RS_Waiting = 'Waiting...';
  RS_Compressing = 'Compressing...';
  RS_Failed = 'Failed';
  RS_Disabled = 'Disabled';

implementation

uses
  frmMain, WebsiteModules, FMDVars;

function IntToStr(Value: Cardinal): String;
begin
  Result := SysUtils.IntToStr(QWord(Value));
end;

{ TDownloadThread }

procedure TDownloadThread.OnTag(NoCaseTag, ActualTag: String);
begin
  parse.Add(ActualTag);
end;

procedure TDownloadThread.OnText(Text: String);
begin
  parse.Add(Text);
end;

procedure TDownloadThread.SockOnStatus(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
begin
  if Reason = HR_ReadCount then
    Task.Container.IncReadCount(StrToIntDef(Value, 0));
end;

constructor TDownloadThread.Create;
begin
  inherited Create(True);
  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.Headers.NameValueSeparator := ':';
  FHTTP.Sock.OnStatus := SockOnStatus;
end;

destructor TDownloadThread.Destroy;
begin
  EnterCriticalsection(Task.FCS_THREADS);
  try
    Modules.DecActiveConnectionCount(Task.Container.ModuleId);
    Task.Threads.Remove(Self);
  finally
    LeaveCriticalsection(Task.FCS_THREADS);
  end;
  FHTTP.Free;
  inherited Destroy;
end;

function TDownloadThread.GetPage(var output: TObject; URL: String;
  const Reconnect: Integer): Boolean;
begin
  if FHTTP.Sock.Tag <> 100 then
    FHTTP.Clear;
  Result := uBaseUnit.GetPage(FHTTP, output, URL, Reconnect);
end;

function TDownloadThread.SaveImage(const mangaSiteID: Integer; URL: String;
  const Path, Name: String; const Reconnect: Integer): Boolean;
begin
  Result := uBaseUnit.SaveImage(FHTTP, mangaSiteID, URL, Path, Name, Reconnect);
end;

procedure TDownloadThread.Execute;
var
  Reslt: Boolean = False;
begin
  try
    case Task.Flag of
      // Get number of images.
      CS_GETPAGENUMBER:
      begin
        Reslt := GetPageNumberFromURL(
          Task.Container.ChapterLinks.Strings[
          Task.Container.CurrentDownloadChapterPtr]);
        // Prepare 'space' for storing image url.
        if (not Terminated) and
          (Task.Container.PageNumber > 0) then
        begin
          while Task.Container.PageLinks.Count < Task.Container.PageNumber do
            Task.Container.PageLinks.Add('W');
        end
        else
          Reslt := False;
      end;
      // Get image urls.
      CS_GETPAGELINK:
      begin
        Reslt := GetLinkPageFromURL(
          Task.Container.ChapterLinks.Strings[
          Task.Container.CurrentDownloadChapterPtr]);
      end;
      // Download images.
      CS_DOWNLOAD:
      begin
        Reslt := DownloadImage;
      end;
    end;

    if not Terminated and Reslt then
    begin
      EnterCriticalSection(Task.Container.CS_Container);
      try
        Task.Container.DownCounter := InterLockedIncrement(Task.Container.DownCounter);
        Task.Container.DownloadInfo.Progress :=
          Format('%d/%d', [Task.Container.DownCounter, Task.Container.PageNumber]);
      finally
        LeaveCriticalSection(Task.Container.CS_Container);
      end;
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + LineEnding +
        '  In TDownloadThread.Execute : ' + GetEnumName(TypeInfo(TFlagType), Integer(Task.Flag)) +
        LineEnding +
        '  Website : ' + Task.Container.DownloadInfo.Website + LineEnding +
        '  URL     : ' + FillMangaSiteHost(Task.Container.MangaSiteID,
        Task.Container.ChapterLinks[Task.Container.CurrentDownloadChapterPtr]) + LineEnding +
        '  Title   : ' + Task.Container.DownloadInfo.title + LineEnding +
        '  Chapter : ' + Task.Container.ChapterName[Task.Container.CurrentDownloadChapterPtr] +
        LineEnding;
      MainForm.ExceptionHandler(Self, E);
    end;
  end;
end;

function TDownloadThread.GetPageNumberFromURL(const URL: String): Boolean;
var
  Parser: THTMLParser;

  {$I includes/AnimeStory/chapter_page_number.inc}

  {$I includes/AnimExtremist/chapter_page_number.inc}

  {$I includes/EGScans/chapter_page_number.inc}

  {$I includes/EsMangaHere/chapter_page_number.inc}

  {$I includes/Kivmanga/chapter_page_number.inc}

  {$I includes/MangaAe/chapter_page_number.inc}

  {$I includes/MangaAr/chapter_page_number.inc}

  {$I includes/MeinManga/chapter_page_number.inc}

  {$I includes/S2Scans/chapter_page_number.inc}

  {$I includes/Starkana/chapter_page_number.inc}

  {$I includes/Turkcraft/chapter_page_number.inc}

  {$I includes/AnimeA/chapter_page_number.inc}

  {$I includes/LectureEnLigne/chapter_page_number.inc}

  {$I includes/JapanShin/chapter_page_number.inc}

  {$I includes/CentrumMangi_PL/chapter_page_number.inc}

  {$I includes/MangaLib_PL/chapter_page_number.inc}

  {$I includes/OneManga/chapter_page_number.inc}

  {$I includes/MangaTown/chapter_page_number.inc}

  {$I includes/MangaOku/chapter_page_number.inc}

  {$I includes/IKomik/chapter_page_number.inc}

  {$I includes/NHentai/chapter_page_number.inc}

  {$I includes/UnixManga/chapter_page_number.inc}

  {$I includes/ExtremeMangas/chapter_page_number.inc}

  {$I includes/MangaHost/chapter_page_number.inc}

  {$I includes/MangaKu/chapter_page_number.inc}

  {$I includes/MangaAt/chapter_page_number.inc}

  {$I includes/Dynasty-Scans/chapter_page_number.inc}

begin
  Result := False;
  Task.Container.PageNumber := 0;

  if Modules.ModuleAvailable(Task.Container.ModuleId, MMGetPageNumber) then
    Result := Modules.GetPageNumber(Self, URL, Task.Container.ModuleId)
  else
  begin
    if Task.Container.MangaSiteID = ANIMEA_ID then
      Result := GetAnimeAPageNumber
    else
    if Task.Container.MangaSiteID = STARKANA_ID then
      Result := GetStarkanaPageNumber
    else
    if Task.Container.MangaSiteID = S2SCAN_ID then
      Result := GetS2scanPageNumber
    else
    if Task.Container.MangaSiteID = MEINMANGA_ID then
      Result := GetMeinMangaPageNumber
    else
    if Task.Container.MangaSiteID = ESMANGAHERE_ID then
      Result := GetEsMangaHerePageNumber
    else
    if Task.Container.MangaSiteID = ANIMEEXTREMIST_ID then
      Result := GetAnimeExtremistPageNumber
    else
    if Task.Container.MangaSiteID = ANIMESTORY_ID then
      Result := GetAnimeStoryPageNumber
    else
    if Task.Container.MangaSiteID = TURKCRAFT_ID then
      Result := GetTurkcraftPageNumber
    else
    if Task.Container.MangaSiteID = MANGAAE_ID then
      Result := GetMangaAePageNumber
    else
    if Task.Container.MangaSiteID = KIVMANGA_ID then
      Result := GetKivmangaPageNumber
    else
    if Task.Container.MangaSiteID = LECTUREENLIGNE_ID then
      Result := GetLectureEnLignePageNumber
    else
    if Task.Container.MangaSiteID = JAPANSHIN_ID then
      Result := GetJapanShinPageNumber
    else
    if Task.Container.MangaSiteID = CENTRUMMANGI_PL_ID then
      Result := GetCentrumMangi_PLPageNumber
    else
    if Task.Container.MangaSiteID = MANGALIB_PL_ID then
      Result := GetMangaLib_PLPageNumber
    else
    if Task.Container.MangaSiteID = ONEMANGA_ID then
      Result := GetOneMangaPageNumber
    else
    if Task.Container.MangaSiteID = MANGATOWN_ID then
      Result := GetMangaTownPageNumber
    else
    if Task.Container.MangaSiteID = MANGAOKU_ID then
      Result := GetMangaOkuPageNumber
    else
    if Task.Container.MangaSiteID = IKOMIK_ID then
      Result := GetIKomikPageNumber
    else
    if Task.Container.MangaSiteID = NHENTAI_ID then
      Result := GetNHentaiPageNumber
    else
    if Task.Container.MangaSiteID = UNIXMANGA_ID then
      Result := GetUnixMangaPageNumber
    else
    if Task.Container.MangaSiteID = EXTREMEMANGAS_ID then
      Result := GetExtremeMangasPageNumber
    else
    if Task.Container.MangaSiteID = MANGAHOST_ID then
      Result := GetMangaHostPageNumber
    else
    if Task.Container.MangaSiteID = MANGAKU_ID then
      Result := GetMangaKuPageNumber
    else
    if Task.Container.MangaSiteID = MANGAAT_ID then
      Result := GetMangaAtPageNumber
    else
    if Task.Container.MangaSiteID = DYNASTYSCANS_ID then
      Result := GetDynastyScansPageNumber;
  end;
  if Task.Container.PageLinks.Count > 0 then
    TrimStrings(Task.Container.PageLinks);
end;

function TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
var
  Parser: THTMLParser;

  {$I includes/AnimeStory/image_url.inc}

  {$I includes/AnimExtremist/image_url.inc}

  {$I includes/CentralDeMangas/image_url.inc}

  {$I includes/EGScans/image_url.inc}

  {$I includes/EsMangaHere/image_url.inc}

  {$I includes/Kivmanga/image_url.inc}

  {$I includes/MangaAe/image_url.inc}

  {$I includes/MangaAr/image_url.inc}

  {$I includes/MangaREADER_POR/image_url.inc}

  {$I includes/MangasPROJECT/image_url.inc}

  {$I includes/ScanManga/image_url.inc}

  {$I includes/Starkana/image_url.inc}

  {$I includes/Turkcraft/image_url.inc}

  {$I includes/VnSharing/image_url.inc}

  {$I includes/AnimeA/image_url.inc}

  {$I includes/LectureEnLigne/image_url.inc}

  {$I includes/JapanShin/image_url.inc}

  {$I includes/CentrumMangi_PL/image_url.inc}

  {$I includes/MangaLib_PL/image_url.inc}

  {$I includes/OneManga/image_url.inc}

  {$I includes/MangaTown/image_url.inc}

  {$I includes/MangaOku/image_url.inc}

  {$I includes/IKomik/image_url.inc}

  {$I includes/NHentai/image_url.inc}

  {$I includes/UnixManga/image_url.inc}

  {$I includes/MangaHost/image_url.inc}

  {$I includes/MangaAt/image_url.inc}

begin
  Result := False;
  if Task.Container.PageLinks[WorkId] <> 'W' then Exit;
  if Modules.ModuleAvailable(Task.Container.ModuleId, MMGetImageURL) then
    Result := Modules.GetImageURL(Self, URL, Task.Container.ModuleId)
  else
  begin
    if Task.Container.MangaSiteID = ANIMEA_ID then
      Result := GetAnimeAImageURL
    else
    if Task.Container.MangaSiteID = VNSHARING_ID then
      Result := GetVnSharingImageURL
    else
    if Task.Container.MangaSiteID = STARKANA_ID then
      Result := GetStarkanaImageURL
    else
    if Task.Container.MangaSiteID = EGSCANS_ID then
      Result := GetEGScansImageURL
    else
    if Task.Container.MangaSiteID = ESMANGAHERE_ID then
      Result := GetEsMangaHereImageURL
    else
    if Task.Container.MangaSiteID = ANIMEEXTREMIST_ID then
      Result := GetAnimeExtremistImageURL
    else
    if Task.Container.MangaSiteID = ANIMESTORY_ID then
      Result := GetAnimeStoryImageURL
    else
    if Task.Container.MangaSiteID = SCANMANGA_ID then
      Result := GetScanMangaImageURL
    else
    if Task.Container.MangaSiteID = TURKCRAFT_ID then
      Result := GetTurkcraftImageURL
    else
    if Task.Container.MangaSiteID = MANGAAR_ID then
      Result := GetMangaArImageURL
    else
    if Task.Container.MangaSiteID = MANGAAE_ID then
      Result := GetMangaAeImageURL
    else
    if Task.Container.MangaSiteID = CENTRALDEMANGAS_ID then
      Result := GetCentralDeMangasImageURL
    else
    if Task.Container.MangaSiteID = KIVMANGA_ID then
      Result := GetKivmangaImageURL
    else
    if Task.Container.MangaSiteID = MANGASPROJECT_ID then
      Result := GetMangasPROJECTImageURL
    else
    if Task.Container.MangaSiteID = MANGAREADER_POR_ID then
      Result := GetMangaREADER_PORImageURL
    else
    if Task.Container.MangaSiteID = LECTUREENLIGNE_ID then
      Result := GeLectureEnligneImageURL
    else
    if Task.Container.MangaSiteID = JAPANSHIN_ID then
      Result := GetJapanShinImageURL
    else
    if Task.Container.MangaSiteID = CENTRUMMANGI_PL_ID then
      Result := GetCentrumMangi_PLImageURL
    else
    if Task.Container.MangaSiteID = MANGALIB_PL_ID then
      Result := GetMangaLib_PLImageURL
    else
    if Task.Container.MangaSiteID = ONEMANGA_ID then
      Result := GetOneMangaImageURL
    else
    if Task.Container.MangaSiteID = MANGATOWN_ID then
      Result := GetMangaTownImageURL
    else
    if Task.Container.MangaSiteID = MANGAOKU_ID then
      Result := GetMangaOkuImageURL
    else
    if Task.Container.MangaSiteID = IKOMIK_ID then
      Result := GetIKomikImageURL
    else
    if Task.Container.MangaSiteID = NHENTAI_ID then
      Result := GetNHentaiImageURL
    else
    if Task.Container.MangaSiteID = UNIXMANGA_ID then
      Result := GetUnixMangaImageURL
    else
    if Task.Container.MangaSiteID = MANGAHOST_ID then
      Result := GetMangaHostImageURL
    else
    if Task.Container.MangaSiteID = MANGAAT_ID then
      Result := GetMangaAtImageURL;
  end;
end;

// ----- TTaskThread -----

constructor TTaskThread.Create;
begin
  inherited Create(True);
  InitCriticalSection(FCS_THREADS);
  Threads := TDownloadThreads.Create;
  FCheckAndActiveTaskFlag := True;
  FIsForDelete := False;
  httpCookies := '';
  FCurrentWorkingDir := '';
  FCurrentCustomFileName := '';
  FCurrentMaxFileNameLength := 0;
end;

destructor TTaskThread.Destroy;
var
  i: Integer;
begin
  EnterCriticalsection(FCS_THREADS);
  try
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        Threads[i].Terminate;
  finally
    LeaveCriticalsection(FCS_THREADS);
  end;
  while Threads.Count > 0 do
    Sleep(32);

  Modules.DecActiveTaskCount(Container.ModuleId);
  with Container do
  begin
    ThreadState := False;
    Manager.RemoveItemsActiveTask(Container);
    Task := nil;
    if not (IsForDelete or Manager.isReadyForExit) then
    begin
      Container.ReadCount := 0;
      DownloadInfo.TransferRate := '';
      if Status <> STATUS_STOP then
      begin
        if (WorkCounter >= PageLinks.Count) and
           (CurrentDownloadChapterPtr >= ChapterLinks.Count) and
           (FailedChapterLinks.Count = 0) then
        begin
          Status := STATUS_FINISH;
          DownloadInfo.Status := Format('[%d/%d] %s',[Container.ChapterLinks.Count,Container.ChapterLinks.Count,RS_Finish]);
          DownloadInfo.Progress := '';
        end
        else
        if not (Status in [STATUS_FAILED, STATUS_PROBLEM]) then
        begin
          Status := STATUS_STOP;
          DownloadInfo.Status :=
            Format('[%d/%d] %s', [CurrentDownloadChapterPtr + 1,
            ChapterLinks.Count, RS_Stopped]);
          FCheckAndActiveTaskFlag := False;
        end;
        if not isExiting then
          Synchronize(SyncStop);
      end;
    end;
  end;
  Threads.Free;
  DoneCriticalsection(FCS_THREADS);
  inherited Destroy;
end;

function TTaskThread.GetFileName(const AWorkId: Integer): String;
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  Result := '';
  if (Container.FileNames.Count = Container.PageLinks.Count) and
    (AWorkId < Container.FileNames.Count) then
    Result := Container.FileNames[AWorkId];
  if Result = '' then
    Result := Format('%.3d', [AWorkId + 1]);
  Result := StringReplace(CurrentCustomFileName, CR_FILENAME, Result, [rfReplaceAll]);
  {$IFDEF WINDOWS}
  s := UTF8Decode(Result);
  if Length(s) > FCurrentMaxFileNameLength then
  begin
    Delete(s, 1, Length(s) - FCurrentMaxFileNameLength);
    Result := UTF8Encode(s);
  end;
  {$ENDIF}
end;

procedure TTaskThread.Compress;
var
  uPacker: TPacker;
  i: Integer;
  s: String;
begin
  if (Container.Manager.compress >= 1) then
  begin
    Container.DownloadInfo.Status :=
      Format('[%d/%d] %s', [Container.CurrentDownloadChapterPtr + 1,
      Container.ChapterLinks.Count, RS_Compressing]);
    uPacker := TPacker.Create;
    try
      case Container.Manager.compress of
        1: uPacker.Format := pfZIP;
        2: uPacker.Format := pfCBZ;
        3: uPacker.Format := pfPDF;
      end;
      uPacker.CompressionQuality := OptionPDFQuality;
      uPacker.Path := CurrentWorkingDir;
      uPacker.FileName := Container.DownloadInfo.SaveTo +
        Container.ChapterName[Container.CurrentDownloadChapterPtr];
      for i := 0 to Container.PageLinks.Count - 1 do
      begin
        s := FindImageFile(CurrentWorkingDir + GetFileName(i));
        if s <> '' then
          uPacker.FileList.Add(s);
      end;
      uPacker.Execute;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
    uPacker.Free;
  end;
end;

procedure TTaskThread.SyncStop;
begin
  Container.Manager.CheckAndActiveTask(FCheckAndActiveTaskFlag);
end;

procedure TTaskThread.ShowBalloonHint;
begin
  if OptionShowBalloonHint then
    Synchronize(SyncShowBallonHint);
end;

function TDownloadThread.DownloadImage: Boolean;
var
  workFilename,
  workURL,
  savedFilename: String;

  {$I includes/MeinManga/image_url.inc}

begin
  Result := True;

  // check download path
  if not ForceDirectoriesUTF8(Task.CurrentWorkingDir) then
  begin
    Task.Container.Status := STATUS_FAILED;
    Task.Container.DownloadInfo.Status := Format('[%d/%d] %s', [
      Task.Container.CurrentDownloadChapterPtr,
      Task.Container.ChapterLinks.Count,
      RS_FailedToCreateDir]);
    Result := False;
    Exit;
  end;

  // check pagelinks url
  workURL := Task.Container.PageLinks[WorkId];
  if (workURL = '') or
     (workURL = 'W') or
     (workURL = 'D') then
    Exit;

  FHTTP.Clear;

  // prepare filename
  workFilename := Task.GetFileName(WorkId);

  // download image
  savedFilename := '';

  if Modules.ModuleAvailable(Task.Container.ModuleId, MMDownloadImage) and
    (Task.Container.PageNumber = Task.Container.PageContainerLinks.Count) and
    (WorkId < Task.Container.PageContainerLinks.Count) then
      workURL := Task.Container.PageContainerLinks[WorkId];

  // call beforedownloadimage if available
  if Modules.ModuleAvailable(Task.Container.ModuleId, MMBeforeDownloadImage) then
    Result := Modules.BeforeDownloadImage(Self, workURL, Task.Container.ModuleId);

  if Result then
  begin
    if Modules.ModuleAvailable(Task.Container.ModuleId, MMDownloadImage) then
        Result := Modules.DownloadImage(
          Self,
          workURL,
          Task.CurrentWorkingDir,
          workFilename,
          Task.Container.ModuleId)
    else
    if Task.Container.MangaSiteID = MEINMANGA_ID then
      Result := GetMeinMangaImageURL
    else
      Result := uBaseUnit.SaveImage(
        FHTTP,
        Task.Container.MangaSiteID,
        workURL,
        Task.CurrentWorkingDir,
        workFilename,
        savedFilename,
        Task.Container.Manager.retryConnect);
  end;
  if Terminated then Exit(False);
  if Result then
  begin
    Task.Container.PageLinks[WorkId] := 'D';

    if Modules.ModuleAvailable(Task.Container.ModuleId, MMAfterImageSaved) then
      Modules.AfterImageSaved(savedFilename, Task.Container.ModuleId);
  end;
end;

procedure TTaskThread.SetCurrentWorkingDir(AValue: String);
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  if FCurrentWorkingDir = AValue then Exit;
  FCurrentWorkingDir := CorrectPathSys(AValue);
  {$IFDEF Windows}
  s := UTF8Decode(FCurrentWorkingDir);
  FCurrentMaxFileNameLength := FMDMaxImageFilePath - Length(s);
  {$ENDIF}
end;

procedure TTaskThread.SetIsForDelete(AValue: Boolean);
begin
  if FIsForDelete = AValue then Exit;
  FIsForDelete := AValue;
end;

procedure TTaskThread.SyncShowBallonHint;
begin
  with MainForm.TrayIcon, Container.DownloadInfo do
  begin
    if Container.Status = STATUS_FAILED then
    begin
      BalloonFlags := bfError;
      BalloonHint := QuotedStrd(Title);
      if Status = '' then
        BalloonHint := BalloonHint + ' - ' + RS_Failed
      else
        BalloonHint := BalloonHint + LineEnding + Status;
    end
    else
    if Container.Status = STATUS_FINISH then
    begin
      BalloonFlags := bfInfo;
      BalloonHint :=
        '"' + Container.DownloadInfo.title + '" - ' + RS_Finish;
    end;
    ShowBalloonHint;
  end;
end;

procedure TTaskThread.CheckOut;
var
  currentMaxThread: Integer;
  s: String;
  mt: Integer;
begin
  if Terminated then Exit;

  //load advanced config if any
  mt := advancedfile.ReadInteger('DownloadMaxThreadsPerTask',
    Container.DownloadInfo.Website, -1);
  if (mt > 0) then
  begin
    if mt > MAX_CONNECTIONPERHOSTLIMIT then
      mt := MAX_CONNECTIONPERHOSTLIMIT;
    currentMaxThread := mt;
  end
  else
  begin
    if Modules.MaxConnectionLimit[Container.ModuleId] > 0 then
      currentMaxThread := Modules.MaxConnectionLimit[Container.ModuleId]
    else
      currentMaxThread := Container.Manager.maxDLThreadsPerTask;
    if currentMaxThread > Container.Manager.maxDLThreadsPerTask then
      currentMaxThread := Container.Manager.maxDLThreadsPerTask;
  end;

  if Container.PageLinks.Count > 0 then
  begin
    s := Trim(Container.PageLinks[Container.WorkCounter]);
    if ((Flag = CS_GETPAGELINK) and (s <> 'W')) or
      ((Flag = CS_DOWNLOAD) and (s = 'D')) then
    begin
      Container.WorkCounter := InterLockedIncrement(Container.WorkCounter);
      Container.DownCounter := InterLockedIncrement(Container.DownCounter);
      Container.DownloadInfo.Progress :=
        Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
      if Flag = CS_GETPAGELINK then
        Container.CurrentPageNumber := InterLockedIncrement(Container.CurrentPageNumber);
      Exit;
    end;
  end;

  if Modules.MaxConnectionLimit[Container.ModuleId] > 0 then
    while (not Terminated) and (Modules.ActiveConnectionCount[Container.ModuleId] >= currentMaxThread) do
      Sleep(SOCKHEARTBEATRATE)
  else
    while (not Terminated) and (Threads.Count >= currentMaxThread) do
      Sleep(SOCKHEARTBEATRATE);

  if (not Terminated) and (Threads.Count < currentMaxThread) then
  begin
    EnterCriticalsection(FCS_THREADS);
    try
      if Modules.ActiveConnectionCount[Container.ModuleId] >= currentMaxThread then Exit;
      Modules.IncActiveConnectionCount(Container.ModuleId);
      Threads.Add(TDownloadThread.Create);
      with TDownloadThread(Threads.Last) do begin
        Task := Self;
        WorkId := Container.WorkCounter;
        //load User-Agent from advancedfile
        AdvanceLoadHTTPConfig(FHTTP, Container.DownloadInfo.Website);
        Start;
        Container.WorkCounter := InterLockedIncrement(Container.WorkCounter);
      end;
      if Flag = CS_GETPAGELINK then
        Container.CurrentPageNumber := InterLockedIncrement(Container.CurrentPageNumber);
    finally
      LeaveCriticalsection(FCS_THREADS);
    end;
  end;
end;

procedure TTaskThread.Execute;

  function CheckForPrepare: Boolean;
  var
    i: Integer;
  begin
    if Container.PageLinks.Count = 0 then
      Exit(True);
    Result := False;
    if Container.PageLinks.Count > 0 then
      for i := 0 to Container.PageLinks.Count - 1 do
        if (Trim(Container.PageLinks[i]) = 'W') or
          (Trim(Container.PageLinks[i]) = '') then
          Exit(True);
  end;

  function CheckForFinish: Boolean;
  var
    i, c: Integer;
  begin
    if Container.PageLinks.Count > 0 then
      Result := True
    else
    begin
      Result := False;
      Exit;
    end;

    c := 0;
    for i := 0 to Container.PageLinks.Count - 1 do
    begin
      if Trim(Container.PageLinks[i]) <> 'D' then
        Inc(c);
    end;
    if c > 0 then begin
      Logger.SendWarning(Format('%s, checkforfinish failed=%d/%d "%s" > "%s"',
        [Self.ClassName,
        c,
        Container.PageLinks.Count,
        Container.DownloadInfo.Title,
        Container.ChapterName[Container.CurrentDownloadChapterPtr]]));
      Result := False;
    end;
  end;

  procedure WaitForThreads;
  begin
    while (not Terminated) and (Threads.Count > 0) do
      Sleep(SOCKHEARTBEATRATE);
  end;

var
  j: Integer;
  DynamicPageLink: Boolean;
begin
  Container.ThreadState := True;
  Container.DownloadInfo.TransferRate := FormatByteSize(Container.ReadCount, true);
  try
    if (Container.Website = '') and (Container.DownloadInfo.Website <> '') then
      Container.Website := Container.DownloadInfo.Website;
    if Container.ModuleId > -1 then
      DynamicPageLink := Modules.Module[Container.ModuleId].DynamicPageLink
    else
      DynamicPageLink := False;

    if Trim(Container.CustomFileName) = '' then
      Container.CustomFileName := OptionFilenameCustomRename;
    if Trim(Container.CustomFileName) = '' then
      Container.CustomFileName := DEFAULT_FILENAME_CUSTOMRENAME;

    while Container.CurrentDownloadChapterPtr < Container.ChapterLinks.Count do
    begin
      WaitForThreads;
      if Terminated then Exit;

      //check path
      if OptionGenerateChapterFolder then
        CurrentWorkingDir := Container.DownloadInfo.SaveTo +
          Container.ChapterName[Container.CurrentDownloadChapterPtr]
      else
        CurrentWorkingDir := Container.DownloadInfo.SaveTo;
      if not ForceDirectoriesUTF8(CurrentWorkingDir) then
      begin
        Logger.SendError(Format('Failed to create dir(%d) = %s', [Length(CurrentWorkingDir), CurrentWorkingDir]));
        Container.Status := STATUS_FAILED;
        Container.DownloadInfo.Status := Format('[%d/%d] %s', [
          Container.CurrentDownloadChapterPtr,
          Container.ChapterLinks.Count,
          RS_FailedToCreateDir]);
        ShowBalloonHint;
        Exit;
      end;

      if Container.ModuleId > -1 then
        Modules.TaskStart(Container, Container.ModuleId);

      // set current working custom filename
      CurrentCustomFileName :=  CustomRename(Container.CustomFileName,
        Container.DownloadInfo.Website,
        Container.DownloadInfo.Title,
        '',
        '',
        Container.ChapterName[Container.CurrentDownloadChapterPtr],
        '',
        OptionChangeUnicodeCharacter,
        OptionChangeUnicodeCharacterStr,
        CR_FILENAME);

      // Get page number.
      if Container.PageLinks.Count = 0 then
      begin
        Container.PageNumber := 0;
        Flag := CS_GETPAGENUMBER;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress := '0/0';
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Preparing,
          Container.ChapterName[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_PREPARE;
        CheckOut;
        WaitForThreads;
        if Terminated then begin
          Container.PageLinks.Clear;
          Container.PageNumber := 0;
          Exit;
        end;
      end;

      //Check file, if exist set mark 'D', otherwise 'W' or 'G' for dynamic image url
      if Container.PageLinks.Count > 0 then
      begin
        for j := 0 to Container.PageLinks.Count - 1 do
        begin
          if ImageFileExist(CurrentWorkingDir + GetFileName(j)) then
            Container.PageLinks[j] := 'D'
          else
          if Container.PageLinks[j] = 'D' then
          begin
            if DynamicPageLink then
              Container.PageLinks[j] := 'G'
            else
              Container.PageLinks[j] := 'W';
          end;
        end;
      end;

      //Get page links
      if Container.PageLinks.Count = 0 then
        Container.PageLinks.Add('W');
      Container.PageNumber := Container.PageLinks.Count;
      if (not DynamicPageLink) and CheckForPrepare then
      begin
        Flag := CS_GETPAGELINK;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Preparing,
          Container.ChapterName[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_PREPARE;
        while Container.WorkCounter < Container.PageNumber do
        begin
          if Terminated then Exit;
          Checkout;
          Container.DownloadInfo.iProgress :=
            InterLockedIncrement(Container.DownloadInfo.iProgress);
        end;
        WaitForThreads;
        if Terminated then Exit;

        //check if pagelink is found. Else set again to 'W'(some script return '')
        if Container.PageLinks.Count > 0 then
        begin
          for j := 0 to Container.PageLinks.Count - 1 do
          begin
            if Trim(Container.PageLinks[j]) = '' then
              Container.PageLinks[j] := 'W';
          end;
        end;
      end;
      if Terminated then Exit;

      // download pages
      // If Container doesn't have any image, we will skip the loop. Otherwise
      // download them
      Container.PageNumber := Container.PageLinks.Count;
      if (Container.PageLinks.Count > 0) then
      begin
        Flag := CS_DOWNLOAD;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        Container.Status := STATUS_DOWNLOAD;
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Downloading,
          Container.ChapterName[Container.CurrentDownloadChapterPtr]]);
        while Container.WorkCounter < Container.PageLinks.Count do
        begin
          if Terminated then Exit;
          Checkout;
          Container.DownloadInfo.iProgress :=
            InterLockedIncrement(Container.DownloadInfo.iProgress);
        end;
        WaitForThreads;
        if Terminated then Exit;

        //check if all page is downloaded
        if CheckForFinish then
        begin
          Container.Status := STATUS_COMPRESS;
          Container.DownloadInfo.Progress := '';
          Compress;
        end
        else
          Container.Status := STATUS_FAILED;
      end
      else begin
        Logger.SendWarning(Format('%s, failed download image PageLinks=%d "%s" > "%s"',
          [Self.ClassName,
          Container.PageLinks.Count,
          Container.DownloadInfo.Title,
          Container.ChapterName[Container.CurrentDownloadChapterPtr]]));
        Container.Status := STATUS_FAILED;
      end;

      if (Container.Status = STATUS_FAILED) and
        (not FindStrLinear(Container.FailedChapterLinks,
        Container.ChapterName[Container.CurrentDownloadChapterPtr])) then
      begin
        Container.FailedChapterName.Add(Container.ChapterName[Container.CurrentDownloadChapterPtr]);
        Container.FailedChapterLinks.Add(Container.ChapterLinks[Container.CurrentDownloadChapterPtr]);
      end;

      Container.CurrentPageNumber := 0;
      Container.PageLinks.Clear;
      Container.PageContainerLinks.Clear;
      Container.CurrentDownloadChapterPtr :=
        InterLockedIncrement(Container.CurrentDownloadChapterPtr);
    end;

    if Container.FailedChapterLinks.Count > 0 then
    begin
      Container.Status := STATUS_FAILED;
      Container.DownloadInfo.Status := Format('[%d/%d] %s', [
        Container.CurrentDownloadChapterPtr,
        Container.ChapterLinks.Count,
        RS_FailedTryResumeTask]);
      Container.DownloadInfo.Progress := '';
      Container.CurrentDownloadChapterPtr := 0;

      Container.ChapterName.Assign(Container.FailedChapterName);
      Container.ChapterLinks.Assign(Container.FailedChapterLinks);
      Container.FailedChapterName.Clear;
      Container.FailedChapterLinks.Clear;
    end
    else
    begin
      Container.Status := STATUS_FINISH;
      Container.DownloadInfo.Status := Format('[%d/%d] %s',[Container.ChapterLinks.Count,Container.ChapterLinks.Count,RS_Finish]);
      Container.DownloadInfo.Progress := '';
    end;
    ShowBalloonHint;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

{ TTaskContainer }

procedure TTaskContainer.SetWebsite(AValue: String);
begin
  if FWebsite = AValue then Exit;
  FWebsite := AValue;
  DownloadInfo.Website := AValue;
  MangaSiteID := GetMangaSiteID(AValue);
  ModuleId := Modules.LocateModule(AValue);
end;

procedure TTaskContainer.SetStatus(AValue: TDownloadStatusType);
begin
  if FStatus = AValue then Exit;
  if Assigned(Manager) then
    Manager.ChangeStatusCount(FStatus, AValue);
  FStatus := AValue;
end;

procedure TTaskContainer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if Assigned(Manager) then
  begin
    if Enabled then
      Dec(Manager.DisabledCount)
    else
      Inc(Manager.DisabledCount);
  end;
end;

constructor TTaskContainer.Create;
begin
  inherited Create;
  DlId := -1;
  InitCriticalSection(CS_Container);
  ThreadState := False;
  ChapterLinks := TStringList.Create;
  ChapterName := TStringList.Create;
  FailedChapterName := TStringList.Create;
  FailedChapterLinks := TStringList.Create;
  PageLinks := TStringList.Create;
  PageContainerLinks := TStringList.Create;
  FileNames := TStringList.Create;
  FWebsite := '';
  ModuleId := -1;
  MangaSiteID := High(WebsiteRoots) + 1;
  ReadCount := 0;
  WorkCounter := 0;
  CurrentPageNumber := 0;
  CurrentDownloadChapterPtr := 0;
  CustomFileName := OptionFilenameCustomRename;
  FStatus := STATUS_NONE;
  FEnabled := True;
  Visible := True;
end;

destructor TTaskContainer.Destroy;
begin
  FileNames.Free;
  PageContainerLinks.Free;
  PageLinks.Free;
  ChapterName.Free;
  ChapterLinks.Free;
  FailedChapterName.Free;
  FailedChapterLinks.Free;
  DoneCriticalsection(CS_Container);
  if Assigned(Manager) then
    Manager.DecStatusCount(Status);
  inherited Destroy;
end;

procedure TTaskContainer.IncReadCount(const ACount: Integer);
begin
  EnterCriticalSection(CS_Container);
  try
    Inc(ReadCount, ACount);
  finally
    LeaveCriticalSection(CS_Container);
  end;
end;

procedure TTaskContainer.SaveToDB(const AOrder: Integer);
var
  i: Integer;
begin
  if AOrder = -1 then
    i := Manager.Items.IndexOf(Self)
  else
    i := AOrder;
  Manager.FDownloadsDB.Add(DlId,
    FEnabled,
    i,
    Integer(Status),
    CurrentDownloadChapterPtr,
    PageNumber,
    CurrentPageNumber,
    Website,
    DownloadInfo.Link,
    DownloadInfo.Title,
    DownloadInfo.Status,
    DownloadInfo.Progress,
    DownloadInfo.SaveTo,
    DownloadInfo.DateTime,
    ChapterLinks.Text,
    ChapterName.Text,
    PageLinks.Text,
    PageContainerLinks.Text,
    FileNames.Text,
    CustomFileName,
    FailedChapterLinks.Text,
    FailedChapterName.Text);
end;

{ TDownloadManager }

procedure TDownloadManager.AddItemsActiveTask(const Item: TTaskContainer);
begin
  EnterCriticalsection(CS_ItemsActiveTask);
  try
    ItemsActiveTask.Add(Item);
  finally
    LeaveCriticalsection(CS_ItemsActiveTask);
  end;
end;

procedure TDownloadManager.RemoveItemsActiveTask(const Item: TTaskContainer);
begin
  EnterCriticalsection(CS_ItemsActiveTask);
  try
    ItemsActiveTask.Remove(Item);
  finally
    LeaveCriticalsection(CS_ItemsActiveTask);
  end;
end;

function TDownloadManager.GetTask(const TaskId: Integer): TTaskContainer;
begin
  Result := Items[TaskId];
end;

function TDownloadManager.GetTaskCount: Integer;
begin
  Result := Items.Count;
end;

function TDownloadManager.GetTransferRate: Integer;
var
  i: Integer;
begin
  Result := 0;
  if ItemsActiveTask.Count = 0 then Exit;
  EnterCriticalSection(CS_ItemsActiveTask);
  try
    for i := 0 to ItemsActiveTask.Count - 1 do
      with ItemsActiveTask[i] do
      begin
        EnterCriticalSection(CS_Container);
        try
          DownloadInfo.TransferRate := FormatByteSize(ReadCount, True);
          Inc(Result, ReadCount);
          ReadCount := 0;
        finally
          LeaveCriticalSection(CS_Container);
        end;
      end;
  finally
    LeaveCriticalSection(CS_ItemsActiveTask);
  end;
end;

procedure TDownloadManager.ChangeStatusCount(const OldStatus,
  NewStatus: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    if OldStatus = NewStatus then Exit;
    if StatusCount[OldStatus] > 0 then
      Dec(StatusCount[OldStatus]);
    Inc(StatusCount[NewStatus]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

procedure TDownloadManager.DecStatusCount(const Status: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    if StatusCount[Status] > 0 then
      Dec(StatusCount[Status]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

procedure TDownloadManager.IncStatusCount(const Status: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    Inc(StatusCount[Status]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

constructor TDownloadManager.Create;
var
  ds: TDownloadStatusType;
begin
  inherited Create;
  InitCriticalSection(CS_Task);
  InitCriticalSection(CS_ItemsActiveTask);
  ForceDirectoriesUTF8(WORK_FOLDER);
  DownloadedChapters := TDownloadedChaptersDB.Create;
  DownloadedChapters.Filename := DOWNLOADEDCHAPTERSDB_FILE;
  DownloadedChapters.OnError := MainForm.ExceptionHandler;
  DownloadedChapters.Open;
  if FileExistsUTF8(DOWNLOADEDCHAPTERS_FILE) then
    if DownloadedChapters.ImportFromIni(DOWNLOADEDCHAPTERS_FILE) then
      DeleteFileUTF8(DOWNLOADEDCHAPTERS_FILE);

  Items := TTaskContainers.Create;
  ItemsActiveTask := TTaskContainers.Create;
  isFinishTaskAccessed := False;
  isRunningBackup := False;
  isRunningBackupDownloadedChaptersList := False;
  isReadyForExit := False;

  InitCriticalSection(CS_StatusCount);
  for ds := Low(StatusCount) to High(StatusCount) do
    StatusCount[ds] := 0;
  DisabledCount := 0;
  FDownloadsDB := TDownloadsDB.Create(WORK_FILEDB);
  FDownloadsDB.Open;
end;

destructor TDownloadManager.Destroy;
begin
  while Items.Count > 0 do
  begin
    Items.Last.Free;
    Items.Remove(Items.Last);
  end;
  Items.Free;
  ItemsActiveTask.Free;
  DownloadedChapters.Free;
  FDownloadsDB.Free;
  DoneCriticalsection(CS_ItemsActiveTask);
  DoneCriticalsection(CS_Task);
  DoneCriticalsection(CS_StatusCount);
  inherited Destroy;
end;

function TDownloadManager.ConvertToDB: Boolean;
var
  i, d: Integer;
  s: String;
begin
  Result := False;
  if not FileExistsUTF8(WORK_FILE) then Exit;
  with TIniFile.Create(WORK_FILE) do
  try
    i := ReadInteger('general', 'NumberOfTasks', 0);
    if i = 0 then Exit;
    for i := 0 to i - 1 do
    begin
      d := -1;
      s := 'task' + IntToStr(i);
      FDownloadsDB.Add(d,
        ReadBool(s, 'Enabled', True),
        i,
        GetEnumValue(TypeInfo(TDownloadStatusType), ReadString(s, 'TaskStatus', GetEnumName(TypeInfo(TDownloadStatusType), 0))),
        ReadInteger(s, 'ChapterPtr', 0),
        ReadInteger(s, 'NumberOfPages', 0),
        ReadInteger(s, 'CurrentPage', 0),
        ReadString(s, 'Website', ''),
        ReadString(s, 'Link', ''),
        ReadString(s, 'Title', ''),
        ReadString(s, 'Status', ''),
        ReadString(s, 'Progress', ''),
        ReadString(s, 'SaveTo', ''),
        StrToFloatDef(ReadString(s, 'DateTime', ''), Now, FMDFormatSettings),
        GetParams(ReadString(s, 'ChapterLinks', '')),
        GetParams(ReadString(s, 'ChapterName', '')),
        GetParams(ReadString(s, 'PageLinks', '')),
        GetParams(ReadString(s, 'PageContainerLinks', '')),
        GetParams(ReadString(s, 'Filenames', '')),
        ReadString(s, 'CustomFileName', DEFAULT_FILENAME_CUSTOMRENAME),
        GetParams(ReadString(s, 'FailedChapterLinks', '')),
        GetParams(ReadString(s, 'FailedChapterName', '')));
    end;
    FDownloadsDB.Commit;
    Result := True;
  finally
    Free;
  end;
  if Result then
    Result := DeleteFileUTF8(WORK_FILE);
end;

procedure TDownloadManager.Restore;
begin
  if not FDownloadsDB.Connection.Connected then Exit;
  ConvertToDB;
  if FDownloadsDB.OpenTable(False) then
  try
    if FDownloadsDB.RecordCount = 0 then Exit;
    EnterCriticalsection(CS_Task);
      try
        FDownloadsDB.Table.First;
        while not FDownloadsDB.Table.EOF do
        begin
          Items.Add(TTaskContainer.Create);
          with Items.Last, FDownloadsDB.Table do
          begin
            Manager := Self;
            DlId                      := Fields[f_dlid].AsInteger;
            Enabled                   := Fields[f_enabled].AsBoolean;
            Status                    := TDownloadStatusType(Fields[f_taskstatus].AsInteger);
            CurrentDownloadChapterPtr := Fields[f_chapterptr].AsInteger;
            PageNumber                := Fields[f_numberofpages].AsInteger;
            CurrentPageNumber         := Fields[f_currentpage].AsInteger;
            Website                   := Fields[f_website].AsString;
            DownloadInfo.Website      := Website;
            DownloadInfo.Link         := Fields[f_link].AsString;
            DownloadInfo.Title        := Fields[f_title].AsString;
            DownloadInfo.Status       := Fields[f_status].AsString;
            DownloadInfo.Progress     := Fields[f_progress].AsString;
            if Pos('/', DownloadInfo.Progress) <> 0 then
              DownCounter := StrToIntDef(Trim(ExtractWord(1, DownloadInfo.Progress, ['/'])), 0);
            DownloadInfo.SaveTo       := Fields[f_saveto].AsString;
            DownloadInfo.DateTime     := Fields[f_datetime].AsDateTime;
            ChapterLinks.Text         := Fields[f_chapterslinks].AsString;
            ChapterName.Text          := Fields[f_chaptersnames].AsString;
            PageLinks.Text            := Fields[f_pagelinks].AsString;
            PageContainerLinks.Text   := Fields[f_pagecontainerlinks].AsString;
            FileNames.Text            := Fields[f_filenames].AsString;
            CustomFileName            := Fields[f_customfilenames].AsString;
            FailedChapterLinks.Text   := Fields[f_failedchapterlinks].AsString;
            FailedChapterName.Text    := Fields[f_failedchapternames].AsString;
          end;
          FDownloadsDB.Table.Next;
        end;
      finally
        LeaveCriticalsection(CS_Task);
      end;
    finally
      FDownloadsDB.CloseTable;
    end;
end;

procedure TDownloadManager.Backup;
var
  i: Integer;
begin
  if isRunningBackup then Exit;
  if Items.Count = 0 then Exit;
  if not FDownloadsDB.Connection.Connected then Exit;
  try
    EnterCriticalSection(CS_Task);
    isRunningBackup := True;
    for i := 0 to Items.Count - 1 do
      Items[i].SaveToDB(i);
    FDownloadsDB.Commit;
  finally
    LeaveCriticalSection(CS_Task);
  end;
  isRunningBackup := False;
end;

procedure TDownloadManager.GetDownloadedChaptersState(const Alink: String;
  var Chapters: array of TChapterStateItem);
var
  s: TStringList;
  i, p: Integer;
begin
  s := TStringList.Create;
  try
    s.Sorted := True;
    s.AddText(DownloadedChapters.Chapters[Alink]);
    if s.Count > 0 then
      for i := Low(Chapters) to High(Chapters) do
        Chapters[i].Downloaded := s.Find(LowerCase(Chapters[i].Link), p)
    else
      for i := Low(Chapters) to High(Chapters) do
        Chapters[i].Downloaded := False;
  finally
    s.Free;
  end;
end;

function TDownloadManager.AddTask: Integer;
begin
  Result := -1;
  EnterCriticalSection(CS_Task);
  try
    Result := Items.Add(TTaskContainer.Create);
    with Items[Result] do
    begin
      Manager := Self;
      Status := STATUS_STOP;
    end;
  finally
    LeaveCriticalSection(CS_Task);
  end;
end;

procedure TDownloadManager.CheckAndActiveTask(const isCheckForFMDDo: Boolean);
var
  i, tcount: Integer;
begin
  if Items.Count = 0 then Exit;
  EnterCriticalSection(CS_Task);
  try
    tcount := 0;
    for i := 0 to Items.Count - 1 do
      if Items[i].ThreadState then
        Inc(tcount);

    if tcount < maxDLTasks then
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if (tcount < maxDLTasks) and
            (Status = STATUS_WAIT) and
            Modules.CanCreateTask(ModuleId) then
          begin
            ActiveTask(i);
            Inc(tcount);
          end;
  finally
    LeaveCriticalSection(CS_Task);
  end;

  try
    if tcount > 0 then
    begin
      if not MainForm.tmRefreshDownloadsInfo.Enabled then
        MainForm.tmRefreshDownloadsInfo.Enabled := True;
      MainForm.UpdateVtDownload;
    end
    else
    begin
      MainForm.tmRefreshDownloadsInfo.Enabled := False;
      MainForm.UpdateVtDownload;
      if isCheckForFMDDo and (OptionLetFMDDo <> DO_NOTHING) then begin
        DoAfterFMD := OptionLetFMDDo;
        MainForm.DoExitWaitCounter;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TDownloadManager.SetTaskActive(const taskID: Integer);
begin
  if not Items[taskID].Enabled then Exit;
  with Items[taskID] do
    if not (ThreadState or (Status in [STATUS_FINISH, STATUS_WAIT])) then
    begin
      Status := STATUS_WAIT;
      DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
    end;
end;

procedure TDownloadManager.CheckAndActiveTaskAtStartup;
var
  i, tcount: Integer;
begin
  if Items.Count = 0 then Exit;
  tcount := 0;
  for i := 0 to Items.Count - 1 do
    with Items[i] do
      if Status in [STATUS_DOWNLOAD, STATUS_PREPARE] then
        if (tcount < maxDLTasks) and
          Modules.CanCreateTask(ModuleId) then
        begin
          Inc(tcount);
          ActiveTask(i);
        end
        else
        begin
          Status := STATUS_WAIT;
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
        end;
  //force to check task if all task loaded is STATUS_WAIT
  if tcount = 0 then
    CheckAndActiveTask
  else if MainForm.tmRefreshDownloadsInfo.Enabled = False then
    MainForm.tmRefreshDownloadsInfo.Enabled := True;
  MainForm.UpdateVtDownload;
end;

procedure TDownloadManager.ActiveTask(const taskID: Integer);
begin
  if not Items[taskID].Enabled then Exit;
  if Items[taskID].Status = STATUS_FINISH then Exit;
  with Items[taskID] do
    if not ThreadState then
    begin
      if not (Status in [STATUS_DOWNLOAD, STATUS_PREPARE]) then
      begin
        Status := STATUS_DOWNLOAD;
        DownloadInfo.Status := RS_Downloading;
      end;
      Modules.IncActiveTaskCount(ModuleId);
      Task := TTaskThread.Create;
      Task.Container := Items[taskID];
      AddItemsActiveTask(Task.Container);
      Task.Start;
    end;
end;

procedure TDownloadManager.StopTask(const taskID: Integer;
  const isCheckForActive: Boolean; isWaitFor: Boolean);
begin
  with Items[taskID] do
  begin
    if Status = STATUS_WAIT then
    begin
      Status := STATUS_STOP;
      DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
    end
    else if ThreadState then
    begin
      Task.Terminate;
      if isWaitFor then
        Task.WaitFor;
    end;
    if isCheckForActive then
      CheckAndActiveTask();
  end;
end;

procedure TDownloadManager.StartAllTasks;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    for i := 0 to Items.Count - 1 do
      with Items[i] do
        if  Enabled and (Status <> STATUS_FINISH) and (not ThreadState) then
        begin
          Status := STATUS_WAIT;
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
        end;
    CheckAndActiveTask;
  end;
end;

procedure TDownloadManager.StopAllTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  for i := 0 to Items.Count - 1 do
    StopTask(i, False, False);
end;

procedure TDownloadManager.StopAllDownloadTasksForExit;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    isReadyForExit := True;
    try
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if ThreadState then
            Task.Terminate;
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if ThreadState then
            Task.WaitFor;
    finally
      isReadyForExit := False;
    end;
  end;
end;

procedure TDownloadManager.FreeAndDelete(const TaskId: Integer);
begin
  FDownloadsDB.Delete(Items[TaskID].DlId);
  Items[TaskID].Free;
  Items.Delete(taskID);
end;

procedure TDownloadManager.RemoveTask(const TaskID: Integer);
begin
  EnterCriticalSection(CS_Task);
  try
    with Items[TaskID] do
      if ThreadState then begin
        Task.Terminate;
        Task.WaitFor;
      end;
    FreeAndDelete(TaskID);
  finally
    LeaveCriticalSection(CS_Task);
  end;
  CheckAndActiveTask;
end;

procedure TDownloadManager.RemoveAllFinishedTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  EnterCriticalsection(CS_Task);
  try
    for i := Items.Count - 1 downto 0 do
      if Items[i].Status = STATUS_FINISH then
        FreeAndDelete(i);
  finally
    LeaveCriticalsection(CS_Task);
  end;
end;

function TDownloadManager.TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Items.Count > 0 then
  begin
    EnterCriticalSection(CS_Task);
    try
      for i := 0 to Items.Count - 1 do
        if Items[i].Status in Stats then begin
          Result := True;
          Break;
        end;
    finally
      LeaveCriticalSection(CS_Task);
    end;
  end;
end;

procedure TDownloadManager.EnableTask(const TaskId: Integer);
begin
  If not Items[TaskId].Enabled then
    Items[TaskId].Enabled := True;
end;

procedure TDownloadManager.DisableTask(const TaskId: Integer);
begin
  with Items[TaskId] do
  begin
    if ThreadState then
      StopTask(TaskId, False);
    if Enabled then
    begin
      if Status = STATUS_WAIT then
      begin
        Status := STATUS_STOP;
        DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
      end;
      Enabled := False;
    end;
  end;
end;

function CompareTaskContainer(const Item1, Item2: TTaskContainer): Integer;

  function GetStr(ARow: TTaskContainer): String;
  begin
    with ARow.DownloadInfo do
      case ARow.Manager.SortColumn of
        0: Result := Title;
        1: Result := Status;
        2: Result := Progress;
        3: Result := TransferRate;
        4: Result := Website;
        5: Result := SaveTo;
        else
          Result := '';
      end;
  end;

  function GetDateTime(ARow: TTaskContainer): TDateTime;
  begin
    Result := ARow.DownloadInfo.DateTime;
  end;

begin
  if Item1.Manager.SortColumn = 6 then
  begin
    if Item1.Manager.SortDirection then
      Result := CompareDateTime(GetDateTime(Item2), GetDateTime(Item1))
    else
      Result := CompareDateTime(GetDateTime(Item1), GetDateTime(Item2));
  end
  else
  begin
    if Item1.Manager.SortDirection then
      Result := NaturalCompareStr(GetStr(Item2), GetStr(Item1))
    else
      Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
  end;
end;

procedure TDownloadManager.Sort(const AColumn: Integer);
begin
  if Items.Count < 2 then Exit;
  EnterCriticalSection(CS_Task);
  try
    SortColumn := AColumn;
    Items.Sort(CompareTaskContainer);
  finally
    LeaveCriticalSection(CS_Task);
  end;
end;

end.
