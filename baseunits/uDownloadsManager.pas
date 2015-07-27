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
  lazutf8classes, LazFileUtils, jsHTMLUtil, FastHTMLParser, HTMLUtil, SynaCode,
  RegExpr, Imaging, ImagingTypes, ImagingCanvases, Classes, SysUtils, Dialogs,
  ExtCtrls, IniFiles, typinfo, syncobjs, httpsend, blcksock, uBaseUnit, uPacker,
  uFMDThread, uMisc, USimpleLogger, dateutils;

type
  TDownloadManager = class;
  TTaskContainer = class;
  TTaskThread = class;

  { TDownloadThread }

  TDownloadThread = class(TFMDThread)
  private
    parse: TStringList;
    checkStyle: TFlagType;
    ModuleId: Integer;
  public
    workCounter: Integer;
    FSortColumn: Cardinal;
    FMessage, FAnotherURL: String;
    FHTTP: THTTPSend;

    procedure MainThreadMessageDialog;
    // Helper method allows to merge 2 images into 1 image.
    // Final image format: png. (too big!)
    procedure Merge2Images(const path, imgName1, imgName2, finalName: String);
    // wait for changing directoet completed
    procedure SetChangeDirectoryFalse;
    procedure SetChangeDirectoryTrue;
    // Get download link from URL
    function GetLinkPageFromURL(const URL: String): Boolean;
    // Get number of download link from URL
    function GetPageNumberFromURL(const URL: String): Boolean;
    // Download image
    function DownloadImage(const prefix: String = ''): Boolean;

    procedure OnTag({%H-}NoCaseTag, ActualTag: string);
    procedure OnText(Text: String);

    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    procedure SockOnHeartBeat(Sender: TObject);
    function GetPage(var output: TObject; URL: String;
      const Reconnect: Cardinal): Boolean; overload;

    function SaveImage(const mangaSiteID: Integer; URL: String;
      const Path, Name, prefix: String; const Reconnect: Cardinal): Boolean; overload;

    procedure Execute; override;
    procedure DoTerminate; override;
  public
    // ID of the site
    manager: TTaskThread;
    constructor Create;
    destructor Destroy; override;

    property SortColumn: Cardinal read FSortColumn write FSortColumn;
    property AnotherURL: String read FAnotherURL write FAnotherURL;
    property GetworkCounter: Integer read workCounter;
  end;

  { TTaskThread }

  TTaskThread = class(TFMDThread)
  private
    ModuleId: Integer;
  protected
    FMessage, FAnotherURL: String;
    procedure CheckOut;
    procedure MainThreadCompressRepaint;
    procedure MainThreadMessageDialog;
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure Compress;
    // show notification when download completed
    procedure ShowBaloon;
    procedure Stop(const check: Boolean = True);
  public
    //additional parameter
    httpCookies: String;
    Flag: TFlagType;
    // container (for storing information)
    container: TTaskContainer;
    // download threads
    threads: TFPList;
    CS_threads: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    property AnotherURL: String read FAnotherURL write FAnotherURL;
  end;

  { TTaskContainer }

  TTaskContainer = class
  private
    FReadCount: Integer;
    CS_FReadCount: TCriticalSection;
  public
    // task thread of this container
    Thread: TTaskThread;
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
    MangaSiteID: Cardinal;
    Status: TDownloadStatusType;
    ThreadState: Boolean;
    ChapterName,
    ChapterLinks,
    FailedChapterName,
    FailedChapterLinks,
    PageContainerLinks,
    PageLinks: TStringList;
    procedure IncReadCount(const ACount: Integer);
    constructor Create;
    destructor Destroy; override;
  end;

  { TDownloadManager }

  TDownloadManager = class
  private
    FTotalReadCount: Integer;
    FSortDirection: Boolean;
    FSortColumn: Integer;
    DownloadManagerFile: TIniFile;
  protected
    function GetTaskCount: Integer;
    function GetTransferRate: Integer;
  public
    CS_DownloadManager_Task: TCriticalSection;
    CS_DownloadedChapterList: TCriticalSection;
    Containers: TFPList;
    isRunningBackup, isFinishTaskAccessed, isRunningBackupDownloadedChaptersList,
    isReadyForExit: Boolean;

    compress, retryConnect,
    // max. active tasks
    maxDLTasks,
    // max. download threads per task
    maxDLThreadsPerTask: Integer;
    // current chapterLinks which thread is processing

    DownloadedChaptersListFile: TStringList;

    //exit counter
    ExitWaitOK: Boolean;

    constructor Create;
    destructor Destroy; override;

    function TaskItem(const Index: Integer): TTaskContainer;
    property Count: Integer read GetTaskCount;

    procedure BackupDownloadedChaptersList;

    procedure Restore;
    procedure Backup;

    // TransferRate
    procedure ClearTransferRate;

    // These methods relate to highlight downloaded chapters.
    procedure AddToDownloadedChaptersList(const ALink, AValue: String); overload;
    procedure AddToDownloadedChaptersList(const Alink: String; AValue: TStrings); overload;
    procedure GetDownloadedChaptersState(const Alink: String;
      var Chapters: array of TChapterStateItem);

    // Add new task to the list.
    function AddTask:Integer;
    // Check and active previous work-in-progress tasks.
    procedure CheckAndActiveTaskAtStartup;
    // Check and active waiting tasks.
    procedure CheckAndActiveTask(const isCheckForFMDDo: Boolean = False;
      {%H-}SenderThread: TThread = nil);
    // Check if we can active another wating task or not.
    function CanActiveTask(const pos : Integer) : Boolean;
    // Active a stopped task.
    procedure ActiveTask(const taskID : Integer);
    // Stop a download/wait task.
    procedure StopTask(const taskID : Integer; const isCheckForActive : Boolean =
      True; isWaitFor: Boolean = False);
    // Start all task
    procedure StartAllTasks;
    // Stop all download/wait tasks.
    procedure StopAllTasks;
    // Stop all download task inside a task before terminate the program.
    procedure StopAllDownloadTasksForExit;
    // Remove a task from list.
    procedure RemoveTask(const taskID : Integer);
    // Remove all finished tasks.
    procedure RemoveAllFinishedTasks;
    // show exit counter
    procedure doExitWaitCounter;
    // check status of task
    function TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;

    // Sort
    procedure Sort(const AColumn: Integer);

    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property TransferRate: Integer read GetTransferRate;
  end;

resourcestring
  RS_FailedToCreateDirTooLong = 'Failed to create dir! Too long?';
  RS_FailedTryResumeTask = 'Failed, try resuming this task!';
  RS_Preparing = 'Preparing';
  RS_Downloading = 'Downloading';
  RS_Stopped = 'Stopped';
  RS_Finish = 'Completed';
  RS_Waiting = 'Waiting...';
  RS_Compressing = 'Compressing...';
  RS_Failed = 'Failed';

implementation

uses
  frmMain, WebsiteModules;

function IntToStr(Value: Cardinal): string;
begin
  Result := SysUtils.IntToStr(QWord(Value));
end;

{ TDownloadThread }

procedure TDownloadThread.OnTag(NoCaseTag, ActualTag : string);
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
    manager.container.IncReadCount(StrToIntDef(Value, 0));
end;

constructor TDownloadThread.Create;
begin
  inherited Create(True);
  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.Headers.NameValueSeparator := ':';
  FHTTP.Sock.OnStatus := SockOnStatus;
  ModuleId := -1;
end;

destructor TDownloadThread.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

procedure TDownloadThread.SockOnHeartBeat(Sender: TObject);
begin
  if Terminated then
  begin
    TBlockSocket(Sender).Tag := 1;
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
end;

function TDownloadThread.GetPage(var output: TObject; URL: String;
  const Reconnect: Cardinal): Boolean;
begin
  if FHTTP.Sock.Tag <> 100 then
    FHTTP.Clear;
  Result := uBaseUnit.GetPage(FHTTP, output, URL, Reconnect);
end;

function TDownloadThread.SaveImage(const mangaSiteID: Integer; URL: String;
  const Path, Name, prefix: String; const Reconnect: Cardinal): Boolean;
begin
  Result := uBaseUnit.SaveImage(FHTTP, mangaSiteID, URL, Path, Name, prefix, Reconnect);
end;

procedure TDownloadThread.Execute;
var
  Reslt: Boolean = False;
begin
  try
    case checkStyle of
      // Get number of images.
      CS_GETPAGENUMBER:
      begin
        Reslt := GetPageNumberFromURL(
          manager.container.ChapterLinks.Strings[
          manager.container.CurrentDownloadChapterPtr]);
        // Prepare 'space' for storing image url.
        if (not Terminated) and
          (manager.container.PageNumber > 0) then
        begin
          while manager.container.PageLinks.Count < manager.container.PageNumber do
            manager.container.PageLinks.Add('W');
        end
        else
          Reslt := False
      end;
      // Get image urls.
      CS_GETPAGELINK:
      begin
        Reslt := GetLinkPageFromURL(
          manager.container.ChapterLinks.Strings[
          manager.container.CurrentDownloadChapterPtr]);
      end;
      // Download images.
      CS_DOWNLOAD:
      begin
        Reslt := DownloadImage;
      end;
    end;

    if not Terminated and Reslt then
    begin
      manager.container.DownCounter := InterLockedIncrement(manager.container.DownCounter);
      manager.container.DownloadInfo.Progress :=
        Format('%d/%d', [manager.container.DownCounter, manager.container.PageNumber]);
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + LineEnding +
        '  In TDownloadThread.Execute : ' + GetEnumName(TypeInfo(TFlagType), integer(checkStyle)) + LineEnding +
        '  Website : ' + manager.container.DownloadInfo.Website + LineEnding +
        '  URL     : ' + FillMangaSiteHost(manager.container.MangaSiteID,
          manager.container.ChapterLinks[manager.container.CurrentDownloadChapterPtr]) + LineEnding +
        '  Title   : ' + manager.container.DownloadInfo.title + LineEnding +
        '  Chapter : ' + manager.container.ChapterName[manager.container.CurrentDownloadChapterPtr] + LineEnding;
      MainForm.ExceptionHandler(Self, E);
    end;
  end;
end;

procedure TDownloadThread.DoTerminate;
begin
  manager.CS_threads.Acquire;
  try
    manager.threads.Remove(Self);
  finally
    manager.CS_threads.Release;
  end;
  inherited DoTerminate;
end;

function TDownloadThread.GetPageNumberFromURL(const URL: String): Boolean;
var
  Parser: THTMLParser;

  {$I includes/AnimeStory/chapter_page_number.inc}

  {$I includes/AnimExtremist/chapter_page_number.inc}

  {$I includes/EatManga/chapter_page_number.inc}

  {$I includes/EGScans/chapter_page_number.inc}

  {$I includes/EHentai/chapter_page_number.inc}

  {$I includes/EsMangaHere/chapter_page_number.inc}

  {$I includes/Hentai2Read/chapter_page_number.inc}

  {$I includes/HugeManga/chapter_page_number.inc}

  {$I includes/KissManga/chapter_page_number.inc}

  {$I includes/Kivmanga/chapter_page_number.inc}

  {$I includes/Komikid/chapter_page_number.inc}

  {$I includes/Manga2u/chapter_page_number.inc}

  {$I includes/MangaAe/chapter_page_number.inc}

  {$I includes/MangaAr/chapter_page_number.inc}

  {$I includes/Mangacow/chapter_page_number.inc}

  {$I includes/MangaEden/chapter_page_number.inc}

  {$I includes/MangaFox/chapter_page_number.inc}

  {$I includes/MangaFrame/chapter_page_number.inc}

  {$I includes/MangaGo/chapter_page_number.inc}

  {$I includes/MangaHere/chapter_page_number.inc}

  {$I includes/MangaInn/chapter_page_number.inc}

  {$I includes/MangaPanda/chapter_page_number.inc}

  {$I includes/MangaPark/chapter_page_number.inc}

  {$I includes/MangaReader/chapter_page_number.inc}

  {$I includes/MangaStream/chapter_page_number.inc}

  {$I includes/MangaStreamTo/chapter_page_number.inc}

  {$I includes/MangaTraders/chapter_page_number.inc}

  {$I includes/MangaVadisi/chapter_page_number.inc}

  {$I includes/MeinManga/chapter_page_number.inc}

  {$I includes/PecintaKomik/chapter_page_number.inc}

  {$I includes/Pururin/chapter_page_number.inc}

  {$I includes/RedHawkScans/chapter_page_number.inc}

  {$I includes/S2Scans/chapter_page_number.inc}

  {$I includes/SenManga/chapter_page_number.inc}

  {$I includes/Starkana/chapter_page_number.inc}

  {$I includes/SubManga/chapter_page_number.inc}

  {$I includes/Turkcraft/chapter_page_number.inc}

  {$I includes/AnimeA/chapter_page_number.inc}

  {$I includes/NineManga/chapter_page_number.inc}

  {$I includes/LectureEnLigne/chapter_page_number.inc}

  {$I includes/JapanShin/chapter_page_number.inc}

  {$I includes/Japscan/chapter_page_number.inc}

  {$I includes/CentrumMangi_PL/chapter_page_number.inc}

  {$I includes/MangaLib_PL/chapter_page_number.inc}

  {$I includes/OneManga/chapter_page_number.inc}

  {$I includes/MangaTown/chapter_page_number.inc}

  {$I includes/ReadHentaiManga/chapter_page_number.inc}

  {$I includes/MangaOku/chapter_page_number.inc}

  {$I includes/MyReadingMangaInfo/chapter_page_number.inc}

  {$I includes/IKomik/chapter_page_number.inc}

  {$I includes/NHentai/chapter_page_number.inc}

  {$I includes/UnionMangas/chapter_page_number.inc}

  {$I includes/MangaMint/chapter_page_number.inc}

  {$I includes/UnixManga/chapter_page_number.inc}

  {$I includes/HakiHome/chapter_page_number.inc}

  {$I includes/ExtremeMangas/chapter_page_number.inc}

  {$I includes/MangaHost/chapter_page_number.inc}

  {$I includes/PornComix/chapter_page_number.inc}

  {$I includes/MangaSee/chapter_page_number.inc}

  {$I includes/MangaKu/chapter_page_number.inc}

  {$I includes/AcademyVN/chapter_page_number.inc}

  {$I includes/MangaAt/chapter_page_number.inc}

  {$I includes/SenMangaRAW/chapter_page_number.inc}

  {$I includes/ReadMangaToday/chapter_page_number.inc}

  {$I includes/LoneManga/chapter_page_number.inc}

  {$I includes/Dynasty-Scans/chapter_page_number.inc}

  {$I includes/Madokami/chapter_page_number.inc}

begin
  Result := False;
  manager.container.PageNumber := 0;

  if Modules.ModuleAvailable(ModuleId, MMGetPageNumber) then
    Result := Modules.GetPageNumber(Self, URL, ModuleId)
  else
  begin
    if manager.container.MangaSiteID = ANIMEA_ID then
      Result := GetAnimeAPageNumber
    else
    if manager.container.MangaSiteID = MANGAHERE_ID then
      Result := GetMangaHerePageNumber
    else
    if manager.container.MangaSiteID = MANGAINN_ID then
      Result := GetMangaInnPageNumber
    else
    if manager.container.MangaSiteID = MANGAFOX_ID then
      Result := GetMangaFoxPageNumber
    else
    if manager.container.MangaSiteID = MANGAREADER_ID then
      Result := GetMangaReaderPageNumber
    else
    if manager.container.MangaSiteID = MANGATRADERS_ID then
      Result := GetMangaTradersPageNumber
    else
    if manager.container.MangaSiteID = STARKANA_ID then
      Result := GetStarkanaPageNumber
    else
    if manager.container.MangaSiteID = EATMANGA_ID then
      Result := GetEatMangaPageNumber
    else
    if manager.container.MangaSiteID = MANGAPANDA_ID then
      Result := GetMangaPandaPageNumber
    else
    if manager.container.MangaSiteID = MANGAPARK_ID then
      Result := GetMangaParkPageNumber
    else
    if manager.container.MangaSiteID = MANGAGO_ID then
      Result := GetMangaGoPageNumber
    else
    if manager.container.MangaSiteID = MANGASTREAM_ID then
      Result := GetMangaStreamPageNumber
    else
    if manager.container.MangaSiteID = REDHAWKSCANS_ID then
      Result := GetRedHawkScansPageNumber
    else
    if manager.container.MangaSiteID = S2SCAN_ID then
      Result := GetS2scanPageNumber
    else
    if manager.container.MangaSiteID = MEINMANGA_ID then
      Result := GetMeinMangaPageNumber
    else
    if manager.container.MangaSiteID = MANGA2U_ID then
      Result := GetManga2uPageNumber
    else
    if manager.container.MangaSiteID = ESMANGAHERE_ID then
      Result := GetEsMangaHerePageNumber
    else
    if manager.container.MangaSiteID = SUBMANGA_ID then
      Result := GetSubMangaPageNumber
    else
    if manager.container.MangaSiteID = ANIMEEXTREMIST_ID then
      Result := GetAnimeExtremistPageNumber
    else
    if manager.container.MangaSiteID = KOMIKID_ID then
      Result := GetKomikidPageNumber
    else
    if manager.container.MangaSiteID = PECINTAKOMIK_ID then
      Result := GetPecintaKomikPageNumber
    else
    if manager.container.MangaSiteID = PURURIN_ID then
      Result := GetPururinPageNumber
    else
    if manager.container.MangaSiteID = HUGEMANGA_ID then
      Result := GetHugeMangaPageNumber
    else
    if manager.container.MangaSiteID = ANIMESTORY_ID then
      Result := GetAnimeStoryPageNumber
    else
    if manager.container.MangaSiteID = TURKCRAFT_ID then
      Result := GetTurkcraftPageNumber
    else
    if manager.container.MangaSiteID = MANGAVADISI_ID then
      Result := GetMangaVadisiPageNumber
    else
    if manager.container.MangaSiteID = MANGAFRAME_ID then
      Result := GetMangaFramePageNumber
    else
    if manager.container.MangaSiteID = MANGAAE_ID then
      Result := GetMangaAePageNumber
    else
    if manager.container.MangaSiteID = MANGACOW_ID then
      Result := GetMangaCowPageNumber
    else
    if manager.container.MangaSiteID = SENMANGA_ID then
      Result := GetSenMangaPageNumber
    else
    if (manager.container.MangaSiteID = MANGAEDEN_ID) or
      (manager.container.MangaSiteID = PERVEDEN_ID) then
      Result := GetMangaEdenPageNumber
    else
    if manager.container.MangaSiteID = KISSMANGA_ID then
      Result := GetKissMangaPageNumber
    else
    if manager.container.MangaSiteID = KIVMANGA_ID then
      Result := GetKivmangaPageNumber
    else
    if manager.container.MangaSiteID = HENTAI2READ_ID then
      Result := GetHentai2ReadPageNumber
    else
    if manager.container.MangaSiteID = EHENTAI_ID then
      Result := GetEHentaiPageNumber
    else
    if manager.container.MangaSiteID = MANGASTREAMTO_ID then
      Result := GetMangaStreamToPageNumber
    else
    if (manager.container.MangaSiteID = NINEMANGA_ID) or
      (manager.container.MangaSiteID = NINEMANGA_ES_ID) or
      (manager.container.MangaSiteID = NINEMANGA_CN_ID) or
      (manager.container.MangaSiteID = NINEMANGA_RU_ID) or
      (manager.container.MangaSiteID = NINEMANGA_DE_ID) or
      (manager.container.MangaSiteID = NINEMANGA_IT_ID) or
      (manager.container.MangaSiteID = NINEMANGA_BR_ID) then
      Result := GetNineMangaPageNumber
    else
    if manager.container.MangaSiteID = LECTUREENLIGNE_ID then
      Result := GetLectureEnLignePageNumber
    else
    if manager.container.MangaSiteID = JAPANSHIN_ID then
      Result := GetJapanShinPageNumber
    else
    if manager.container.MangaSiteID = JAPSCAN_ID then
      Result := GetJapscanPageNumber
    else
    if manager.container.MangaSiteID = CENTRUMMANGI_PL_ID then
      Result := GetCentrumMangi_PLPageNumber
    else
    if manager.container.MangaSiteID = MANGALIB_PL_ID then
      Result := GetMangaLib_PLPageNumber
    else
    if manager.container.MangaSiteID = ONEMANGA_ID then
      Result := GetOneMangaPageNumber
    else
    if manager.container.MangaSiteID = MANGATOWN_ID then
      Result := GetMangaTownPageNumber
    else
    if manager.container.MangaSiteID = READHENTAIMANGA_ID then
      Result := GetReadHentaiMangaPageNumber
    else
    if manager.container.MangaSiteID = MANGAOKU_ID then
      Result := GetMangaOkuPageNumber
    else
    if manager.container.MangaSiteID = MYREADINGMANGAINFO_ID then
      Result := GetMyReadingMangaInfoPageNumber
    else
    if manager.container.MangaSiteID = IKOMIK_ID then
      Result := GetIKomikPageNumber
    else
    if manager.container.MangaSiteID = NHENTAI_ID then
      Result := GetNHentaiPageNumber
    else
    if manager.container.MangaSiteID = UNIONMANGAS_ID then
      Result := GetUnionMangasPageNumber
    else
    if manager.container.MangaSiteID = MANGAMINT_ID then
      Result := GetMangaMintPageNumber
    else
    if manager.container.MangaSiteID = UNIXMANGA_ID then
      Result := GetUnixMangaPageNumber
    else
    if manager.container.MangaSiteID = HAKIHOME_ID then
      Result := GetHakiHomePageNumber
    else
    if manager.container.MangaSiteID = EXTREMEMANGAS_ID then
      Result := GetExtremeMangasPageNumber
    else
    if manager.container.MangaSiteID = MANGAHOST_ID then
      Result := GetMangaHostPageNumber
    else
    if (manager.container.MangaSiteID = PORNCOMIX_ID) or
      (manager.container.MangaSiteID = XXCOMICS_ID) or
      (manager.container.MangaSiteID = XXCOMICSMT_ID) or
      (manager.container.MangaSiteID = XXCOMICS3D_ID) or
      (manager.container.MangaSiteID = PORNCOMIXRE_ID) or
      (manager.container.MangaSiteID = PORNCOMIXIC_ID) or
      (manager.container.MangaSiteID = PORNXXXCOMICS_ID) then
      Result := GetPornComixPageNumber(manager.container.MangaSiteID)
    else
    if manager.container.MangaSiteID = MANGASEE_ID then
      Result := GetMangaSeePageNumber
    else
    if manager.container.MangaSiteID = MANGAKU_ID then
      Result := GetMangaKuPageNumber
    else
    if manager.container.MangaSiteID = ACADEMYVN_ID then
      Result := GetAcademyVNPageNumber
    else
    if manager.container.MangaSiteID = MANGAAT_ID then
      Result := GetMangaAtPageNumber
    else
    if manager.container.MangaSiteID = SENMANGARAW_ID then
      Result := GetSenMangaRAWPageNumber
    else
    if manager.container.MangaSiteID = READMANGATODAY_ID then
      Result := GetReadMangaTodayPageNumber
    else
    if manager.container.MangaSiteID = LONEMANGA_ID then
      Result := GetLoneMangaPageNumber
    else
    if manager.container.MangaSiteID = DYNASTYSCANS_ID then
      Result := GetDynastyScansPageNumber
    else
    if manager.container.MangaSiteID = MADOKAMI_ID then
      Result := GetMadokamiPageNumber;
  end;
end;

function TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
var
  Parser: THTMLParser;

  {$I includes/AnimeStory/image_url.inc}

  {$I includes/AnimExtremist/image_url.inc}

  {$I includes/BlogTruyen/image_url.inc}

  {$I includes/CentralDeMangas/image_url.inc}

  {$I includes/EatManga/image_url.inc}

  {$I includes/EGScans/image_url.inc}

  {$I includes/EsMangaHere/image_url.inc}

  {$I includes/Fakku/image_url.inc}

  {$I includes/Hentai2Read/image_url.inc}

  {$I includes/HugeManga/image_url.inc}

  {$I includes/Kivmanga/image_url.inc}

  {$I includes/Komikid/image_url.inc}

  {$I includes/Mabuns/image_url.inc}

  {$I includes/Manga24h/image_url.inc}

  {$I includes/Manga2u/image_url.inc}

  {$I includes/MangaAe/image_url.inc}

  {$I includes/MangaAr/image_url.inc}

  {$I includes/Mangacan/image_url.inc}

  {$I includes/Mangacow/image_url.inc}

  {$I includes/MangaEden/image_url.inc}

  {$I includes/MangaEsta/image_url.inc}

  {$I includes/MangaFox/image_url.inc}

  {$I includes/MangaFrame/image_url.inc}

  {$I includes/MangaGo/image_url.inc}

  {$I includes/MangaHere/image_url.inc}

  {$I includes/MangaInn/image_url.inc}

  {$I includes/MangaPanda/image_url.inc}

  //{$I includes/MangaPark/image_url.inc}

  {$I includes/MangaReader/image_url.inc}

  {$I includes/MangaREADER_POR/image_url.inc}

  {$I includes/MangasPROJECT/image_url.inc}

  {$I includes/MangaStream/image_url.inc}

  {$I includes/MangaStreamTo/image_url.inc}

  {$I includes/MangaTraders/image_url.inc}

  {$I includes/MangaVadisi/image_url.inc}

  {$I includes/PecintaKomik/image_url.inc}

  {$I includes/Pururin/image_url.inc}

  {$I includes/RedHawkScans/image_url.inc}

  {$I includes/ScanManga/image_url.inc}

  {$I includes/SenManga/image_url.inc}

  {$I includes/Starkana/image_url.inc}

  {$I includes/SubManga/image_url.inc}

  {$I includes/TruyenTranhTuan/image_url.inc}

  {$I includes/Turkcraft/image_url.inc}

  {$I includes/VnSharing/image_url.inc}

  {$I includes/AnimeA/image_url.inc}

  {$I includes/NineManga/image_url.inc}

  {$I includes/LectureEnLigne/image_url.inc}

  {$I includes/JapanShin/image_url.inc}

  {$I includes/Japscan/image_url.inc}

  {$I includes/CentrumMangi_PL/image_url.inc}

  {$I includes/MangaLib_PL/image_url.inc}

  {$I includes/OneManga/image_url.inc}

  {$I includes/MangaTown/image_url.inc}

  {$I includes/ReadHentaiManga/image_url.inc}

  {$I includes/MangaOku/image_url.inc}

  {$I includes/IKomik/image_url.inc}

  {$I includes/NHentai/image_url.inc}

  {$I includes/MangaMint/image_url.inc}

  {$I includes/UnixManga/image_url.inc}

  {$I includes/HakiHome/image_url.inc}

  {$I includes/MangaHost/image_url.inc}

  {$I includes/PornComix/image_url.inc}

  {$I includes/MangaAt/image_url.inc}

begin
  Result := False;
  if (manager.container.PageLinks.Count > 0) and
   (manager.container.PageLinks.Strings[workCounter] <> 'W') then
    Exit;

  if Modules.ModuleAvailable(ModuleId, MMGetImageURL) then
    Result := Modules.GetImageURL(Self, URL, ModuleId)
  else
  begin
    if manager.container.MangaSiteID = ANIMEA_ID then
      Result := GetAnimeAImageURL
    else
    if manager.container.MangaSiteID = MANGATRADERS_ID then
      Result := GetMangaTradersImageURL
    else
    if manager.container.MangaSiteID = MANGAHERE_ID then
      Result := GetMangaHereImageURL
    else
    if manager.container.MangaSiteID = MANGAINN_ID then
      Result := GetMangaInnImageURL
    else
    if manager.container.MangaSiteID = MANGA2U_ID then
      Result := GetManga2uImageURL
    else
    if manager.container.MangaSiteID = MANGA24H_ID then
      Result := GetManga24hImageURL
    else
    if manager.container.MangaSiteID = VNSHARING_ID then
      Result := GetVnSharingImageURL
    else
    if manager.container.MangaSiteID = HENTAI2READ_ID then
      Result := GetHentai2ReadImageURL
    else
    if manager.container.MangaSiteID = FAKKU_ID then
      Result := GetFakkuImageURL
    else
    if manager.container.MangaSiteID = MANGAREADER_ID then
      Result := GetMangaReaderImageURL
    else
    //if manager.container.MangaSiteID = MANGAPARK_ID then
    //  Result := GetMangaParkImageURL
    //else
    if manager.container.MangaSiteID = MANGAFOX_ID then
      Result := GetMangaFoxImageURL
    else
    if manager.container.MangaSiteID = STARKANA_ID then
      Result := GetStarkanaImageURL
    else
    if manager.container.MangaSiteID = EATMANGA_ID then
      Result := GetEatMangaImageURL
    else
    if manager.container.MangaSiteID = MANGAPANDA_ID then
      Result := GetMangaPandaImageURL
    else
    if manager.container.MangaSiteID = MANGAGO_ID then
      Result := GetMangaGoImageURL
    else
    if manager.container.MangaSiteID = MANGASTREAM_ID then
      Result := GetMangaStreamImageURL
    else
    if manager.container.MangaSiteID = REDHAWKSCANS_ID then
      Result := GetRedHawkScansImageURL
    else
    if manager.container.MangaSiteID = EGSCANS_ID then
      Result := GetEGScansImageURL
    else
    if manager.container.MangaSiteID = ESMANGAHERE_ID then
      Result := GetEsMangaHereImageURL
    else
    if manager.container.MangaSiteID = SUBMANGA_ID then
      Result := GetSubMangaImageURL
    else
    if manager.container.MangaSiteID = ANIMEEXTREMIST_ID then
      Result := GetAnimeExtremistImageURL
    else
    if manager.container.MangaSiteID = KOMIKID_ID then
      Result := GetKomikidImageURL
    else
    if manager.container.MangaSiteID = PECINTAKOMIK_ID then
      Result := GetPecintaKomikImageURL
    else
    if manager.container.MangaSiteID = MABUNS_ID then
      Result := GetMabunsImageURL
    else
    if manager.container.MangaSiteID = MANGAESTA_ID then
      Result := GetMangaEstaImageURL
    else
    if manager.container.MangaSiteID = PURURIN_ID then
      Result := GetPururinImageURL
    else
    if manager.container.MangaSiteID = HUGEMANGA_ID then
      Result := GetHugeMangaImageURL
    else
    if manager.container.MangaSiteID = ANIMESTORY_ID then
      Result := GetAnimeStoryImageURL
    else
    if manager.container.MangaSiteID = SCANMANGA_ID then
      Result := GetScanMangaImageURL
    else
    if manager.container.MangaSiteID = TURKCRAFT_ID then
      Result := GetTurkcraftImageURL
    else
    if manager.container.MangaSiteID = MANGAVADISI_ID then
      Result := GetMangaVadisiImageURL
    else
    if manager.container.MangaSiteID = MANGAFRAME_ID then
      Result := GetMangaFrameImageURL
    else
    if manager.container.MangaSiteID = MANGAAR_ID then
      Result := GetMangaArImageURL
    else
    if manager.container.MangaSiteID = MANGAAE_ID then
      Result := GetMangaAeImageURL
    else
    if manager.container.MangaSiteID = CENTRALDEMANGAS_ID then
      Result := GetCentralDeMangasImageURL
    else
    if manager.container.MangaSiteID = MANGACOW_ID then
      Result := GetMangaCowImageURL
    else
    if manager.container.MangaSiteID = SENMANGA_ID then
      Result := GetSenMangaImageURL
    else
    if manager.container.MangaSiteID = TRUYENTRANHTUAN_ID then
      Result := GetTruyenTranhTuanImageURL
    else
    if manager.container.MangaSiteID = BLOGTRUYEN_ID then
      Result := GetBlogTruyenImageURL
    else
    if (manager.container.MangaSiteID = MANGAEDEN_ID) or
      (manager.container.MangaSiteID = PERVEDEN_ID) then
      Result := GetMangaEdenImageURL
    else
    if manager.container.MangaSiteID = KIVMANGA_ID then
      Result := GetKivmangaImageURL
    else
    if manager.container.MangaSiteID = MANGACAN_ID then
      Result := GetMangacanImageURL
    else
    if manager.container.MangaSiteID = MANGASPROJECT_ID then
      Result := GetMangasPROJECTImageURL
    else
    if manager.container.MangaSiteID = MANGAREADER_POR_ID then
      Result := GetMangaREADER_PORImageURL
    else
    if manager.container.MangaSiteID = MANGASTREAMTO_ID then
      Result := GetMangaStreamToImageURL
    else
    if (manager.container.MangaSiteID = NINEMANGA_ID) or
      (manager.container.MangaSiteID = NINEMANGA_ES_ID) or
      (manager.container.MangaSiteID = NINEMANGA_CN_ID) or
      (manager.container.MangaSiteID = NINEMANGA_RU_ID) or
      (manager.container.MangaSiteID = NINEMANGA_DE_ID) or
      (manager.container.MangaSiteID = NINEMANGA_IT_ID) or
      (manager.container.MangaSiteID = NINEMANGA_BR_ID) then
      Result := GetNineMangaImageURL
    else
    if manager.container.MangaSiteID = LECTUREENLIGNE_ID then
      Result := GeLectureEnligneImageURL
    else
    if manager.container.MangaSiteID = JAPANSHIN_ID then
      Result := GetJapanShinImageURL
    else
    if manager.container.MangaSiteID = JAPSCAN_ID then
      Result := GetJapscanImageURL
    else
    if manager.container.MangaSiteID = CENTRUMMANGI_PL_ID then
      Result := GetCentrumMangi_PLImageURL
    else
    if manager.container.MangaSiteID = MANGALIB_PL_ID then
      Result := GetMangaLib_PLImageURL
    else
    if manager.container.MangaSiteID = ONEMANGA_ID then
      Result := GetOneMangaImageURL
    else
    if manager.container.MangaSiteID = MANGATOWN_ID then
      Result := GetMangaTownImageURL
    else
    if manager.container.MangaSiteID = READHENTAIMANGA_ID then
      Result := GetReadHentaiMangaImageURL
    else
    if manager.container.MangaSiteID = MANGAOKU_ID then
      Result := GetMangaOkuImageURL
    else
    if manager.container.MangaSiteID = IKOMIK_ID then
      Result := GetIKomikImageURL
    else
    if manager.container.MangaSiteID = NHENTAI_ID then
      Result := GetNHentaiImageURL
    else
    if manager.container.MangaSiteID = MANGAMINT_ID then
      Result := GetMangaMintImageURL
    else
    if manager.container.MangaSiteID = UNIXMANGA_ID then
      Result := GetUnixMangaImageURL
    else
    if manager.container.MangaSiteID = HAKIHOME_ID then
      Result := GetHakiHomeImageURL
    else
    if manager.container.MangaSiteID = MANGAHOST_ID then
      Result := GetMangaHostImageURL
    else
    if (manager.container.MangaSiteID = PORNCOMIX_ID) or
      (manager.container.MangaSiteID = XXCOMICS_ID) or
      (manager.container.MangaSiteID = XXCOMICSMT_ID) or
      (manager.container.MangaSiteID = XXCOMICS3D_ID) or
      (manager.container.MangaSiteID = PORNCOMIXRE_ID) or
      (manager.container.MangaSiteID = PORNCOMIXIC_ID) or
      (manager.container.MangaSiteID = PORNXXXCOMICS_ID) then
      Result := GetPornComixImageURL
    else
    if manager.container.MangaSiteID = MANGAAT_ID then
      Result := GetMangaAtImageURL;
  end;
end;

procedure TDownloadThread.MainThreadMessageDialog;
begin
  MessageDlg('TDownloadThread', FMessage, mtInformation, [mbOK], '');
end;

procedure TDownloadThread.Merge2Images(
  const path, imgName1, imgName2, finalName: String);
var
  rect: TRect;
  img1, img2, finalImg: TImageData;
  cv1, cv2, canvas: TImagingCanvas;
  stream: TFileStreamUTF8;
  fullImgName1, fullImgName2, fullFinalImgName, fext: String;
begin
  fullImgName1 := Path + '/' + imgName1;
  fullImgName2 := Path + '/' + imgName2;
  fullFinalImgName := Path + '/' + finalName;

  if (not FileExistsUTF8(fullImgName1)) or (not FileExistsUTF8(fullImgName2)) then
    Exit;

  Initialize(img1);
  Initialize(img2);
  Initialize(finalImg);
  // Load first image to stream.
  stream := TFileStreamUTF8.Create(fullImgName1, fmOpenRead);
  LoadImageFromStream(stream, img1);
  stream.Free;
  cv1 := TImagingCanvas.CreateForData(@img1);

  // Load second image to stream.
  stream := TFileStreamUTF8.Create(fullImgName2, fmOpenRead);
  LoadImageFromStream(stream, img2);
  stream.Free;
  cv2 := TImagingCanvas.CreateForData(@img2);

  // Create new buffer for merging images ...
  NewImage(img1.Width, img1.Height + img2.Height, ifR8G8B8, finalImg);
  canvas := TImagingCanvas.CreateForData(@finalImg);

  // Merge images.
  rect.Left := 0;
  rect.Top := 0;
  rect.Right := img1.Width;
  rect.Bottom := img1.Height;
  cv1.DrawBlend(rect, canvas, 0, 0, bfOne, bfZero);

  rect.Left := 0;
  rect.Top := 0;
  rect.Right := img2.Width;
  rect.Bottom := img2.Height;
  cv2.DrawBlend(rect, canvas, 0, img1.Height, bfOne, bfZero);

  // Save final image.
  if FileExistsUTF8(fullFinalImgName) then
    DeleteFileUTF8(fullFinalImgName);
  stream := TFileStreamUTF8.Create(fullFinalImgName, fmCreate);
  //SaveImageToStream('png', stream, finalImg);
  fext := ExtractFileExt(finalName);
  if fext[1] = '.' then
    fext := Copy(fext, 2, Length(fext) - 1);
  SaveImageToStream(fext, stream, finalImg);
  stream.Free;

  // Remove old images.
  DeleteFileUTF8(fullImgName1);
  DeleteFileUTF8(fullImgName2);

  // Free memory.
  cv1.Free;
  cv2.Free;
  canvas.Free;
  FreeImage(img1);
  FreeImage(img2);
  FreeImage(finalImg);
end;

procedure TDownloadThread.SetChangeDirectoryFalse;
begin
  isChangeDirectory := False;
end;

procedure TDownloadThread.SetChangeDirectoryTrue;
begin
  isChangeDirectory := True;
end;

// ----- TTaskThread -----

constructor TTaskThread.Create;
begin
  inherited Create(True);
  CS_threads := TCriticalSection.Create;
  threads := TFPList.Create;
  ModuleId := -1;
  anotherURL := '';
  httpCookies := '';
end;

destructor TTaskThread.Destroy;
begin
  threads.Free;
  CS_threads.Free;
  inherited Destroy;
end;

procedure TTaskThread.MainThreadCompressRepaint;
begin
  container.DownloadInfo.Status :=
    Format('%s (%d/%d)', [RS_Compressing, container.CurrentDownloadChapterPtr +
    1, container.ChapterLinks.Count]);
  MainForm.vtDownload.Repaint;
end;

procedure TTaskThread.MainThreadMessageDialog;
begin
  MessageDlg('TTaskThread', FMessage, mtInformation, [mbOK], '');
end;

procedure TTaskThread.Compress;
var
  uPacker: TPacker;
begin
  if (container.Manager.compress >= 1) then
  begin
    Synchronize(MainThreadCompressRepaint);
    uPacker := TPacker.Create;
    try
      case container.Manager.compress of
        1: uPacker.ext := '.zip';
        2: uPacker.ext := '.cbz';
        3: uPacker.ext := '.pdf';
      end;
      uPacker.CompressionQuality := OptionPDFQuality;
      uPacker.Path := CorrectPathSys(container.DownloadInfo.SaveTo) +
        container.ChapterName.Strings[container.CurrentDownloadChapterPtr];
      uPacker.Execute;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
    uPacker.Free;
  end;
end;

procedure TTaskThread.ShowBaloon;
begin
  if container.Status = STATUS_FAILED then
  begin
    MainForm.TrayIcon.BalloonFlags := bfError;
    MainForm.TrayIcon.BalloonHint :=
      '"' + container.DownloadInfo.title + '" - ' + RS_Failed;;
  end
  else
  if container.Status = STATUS_FINISH then
  begin
    MainForm.TrayIcon.BalloonFlags := bfInfo;
    MainForm.TrayIcon.BalloonHint :=
      '"' + container.DownloadInfo.title + '" - ' + RS_Finish;
  end;
  MainForm.TrayIcon.ShowBalloonHint;
end;

function TDownloadThread.DownloadImage(const prefix: String): Boolean;
var
  TURL, lpath: String;

  {$I includes/EHentai/image_url.inc}

  {$I includes/MeinManga/image_url.inc}

  {$I includes/SenMangaRAW/image_url.inc}

begin
  lpath := CleanAndExpandDirectory(CorrectPathSys(manager.container.DownloadInfo.SaveTo +
    manager.container.ChapterName[manager.container.CurrentDownloadChapterPtr]));
  if not DirectoryExistsUTF8(lpath) then
  begin
    if not ForceDirectoriesUTF8(lpath) then
    begin
      manager.container.Status := STATUS_FAILED;
      manager.container.DownloadInfo.Status := 'Failed to create dir! Too long?';
      Result := False;
      Exit;
    end;
  end;

  Result := True;
  TURL := manager.container.PageLinks[workCounter];
  if (TURL = '') or (TURL = 'W') or (TURL = 'D') then
    Exit;

  FHTTP.Clear;
  if manager.container.MangaSiteID = SENMANGARAW_ID then
    Result := GetSenMangaRAWImageURL;

  TURL := DecodeURL(TURL); //decode first to avoid double encoded
  TURL := EncodeTriplet(TURL, '%', URLSpecialChar + ['#']);

  if manager.container.MangaSiteID = EHENTAI_ID then
    Result := getEHentaiImageURL
  else
  if manager.container.MangaSiteID = MEINMANGA_ID then
    Result := GetMeinMangaImageURL
  else
  if Result then
    Result := SaveImage(
      manager.container.MangaSiteID,
      TURL,
      lpath,
      Format('%.3d', [workCounter + 1]),
      prefix,
      manager.container.Manager.retryConnect);

  if Terminated then Exit(False);
  if Result then
    manager.container.PageLinks[workCounter] := 'D';
end;

procedure TTaskThread.CheckOut;
var
  currentMaxThread: Integer;
  s: String;
  mt: Integer;
begin
  if Terminated then Exit;

  //load advanced config if any
  mt := INIAdvanced.ReadInteger('DownloadMaxThreadsPerTask',
    container.DownloadInfo.Website, -1);
  if (mt > 0) then
  begin
    if (mt > 32) then
      mt := 32;
    currentMaxThread := mt;
  end
  else
  begin
    case container.MangaSiteID of
      PECINTAKOMIK_ID : currentMaxThread := 1;
      EHENTAI_ID      : currentMaxThread := 2;
      else
        currentMaxThread := container.Manager.maxDLThreadsPerTask;
    end;
    if currentMaxThread > container.Manager.maxDLThreadsPerTask then
      currentMaxThread := container.Manager.maxDLThreadsPerTask;
  end;

  if container.PageLinks.Count > 0 then
  begin
    s := Trim(container.PageLinks[container.WorkCounter]);
    if ((Flag = CS_GETPAGELINK) and (s <> 'W')) or
      ((Flag = CS_DOWNLOAD) and (s = 'D')) then
    begin
      container.WorkCounter := InterLockedIncrement(container.WorkCounter);
      container.DownCounter := InterLockedIncrement(container.DownCounter);
      container.DownloadInfo.Progress :=
        Format('%d/%d', [container.DownCounter, container.PageNumber]);
      if Flag = CS_GETPAGELINK then
        container.CurrentPageNumber := InterLockedIncrement(container.CurrentPageNumber);
      Exit;
    end;

    while (not Terminated) and (threads.Count >= currentMaxThread) do
      Sleep(SOCKHEARTBEATRATE);
  end;

  if (not Terminated) and (threads.Count < currentMaxThread) then
  begin
    CS_threads.Acquire;
    try
      threads.Add(TDownloadThread.Create);
      with TDownloadThread(threads.Last) do begin
        manager := Self;
        ModuleId := Self.ModuleId;
        workCounter := container.WorkCounter;
        checkStyle := Flag;
        //load User-Agent from INIAdvanced
        if container.DownloadInfo.Website <> '' then
          FHTTP.UserAgent := INIAdvanced.ReadString('UserAgent',
            container.DownloadInfo.Website, '');
        Start;
        container.WorkCounter := InterLockedIncrement(container.WorkCounter);
      end;
      if Flag = CS_GETPAGELINK then
        container.CurrentPageNumber := InterLockedIncrement(container.CurrentPageNumber);
    finally
      CS_threads.Release;
    end;
  end;
end;

procedure TTaskThread.Execute;

  function CheckForPrepare: Boolean;
  var
    i: Integer;
  begin
    if container.PageLinks.Count = 0 then
      Exit(True);
    Result := False;
    if container.PageLinks.Count > 0 then
      for i := 0 to container.PageLinks.Count - 1 do
        if (Trim(container.PageLinks[i]) = 'W') or
          (Trim(container.PageLinks[i]) = '') then
          Exit(True);
  end;

  function CheckForFinish: Boolean;
  var
    i, c: Integer;
  begin
    if container.PageLinks.Count > 0 then
      Result := True
    else
    begin
      Result := False;
      Exit;
    end;

    c := 0;
    for i := 0 to container.PageLinks.Count - 1 do
    begin
      if Trim(container.PageLinks[i]) <> 'D' then
        Inc(c);
    end;
    if c > 0 then
      Result := False;
  end;

  procedure WaitForThreads;
  begin
    while (not Terminated) and (threads.Count > 0) do
      Sleep(SOCKHEARTBEATRATE);
  end;

var
  j: Integer;
  S, P: String;
begin
  INIAdvanced.Reload;
  ModuleId := Modules.LocateModule(container.DownloadInfo.Website);
  container.ThreadState := True;
  container.DownloadInfo.TransferRate := '';
  try
    while container.CurrentDownloadChapterPtr < container.ChapterLinks.Count do
    begin
      WaitForThreads;
      if Terminated then Exit;

      //strip
      container.DownloadInfo.SaveTo := CorrectPathSys(container.DownloadInfo.SaveTo);
      S := CorrectPathSys(container.DownloadInfo.SaveTo +
        container.ChapterName[container.CurrentDownloadChapterPtr]);
      //check path
      if not DirectoryExistsUTF8(S) then
      begin
        if not ForceDirectoriesUTF8(S) then
        begin
          container.Status := STATUS_FAILED;
          container.DownloadInfo.Status := RS_FailedToCreateDirTooLong;
          Exit;
        end;
      end;

      //if no total page number found, we reset pagelinks here
      if container.MangaSiteID = SUBMANGA_ID then
        container.PageLinks.Clear;

      // Get page number.
      if container.PageLinks.Count = 0 then
      begin
        container.PageNumber := 0;
        Flag := CS_GETPAGENUMBER;
        container.WorkCounter := 0;
        container.DownCounter := 0;
        container.DownloadInfo.iProgress := 0;
        container.DownloadInfo.Progress := '0/0';
        container.DownloadInfo.Status :=
          Format('%s (%d/%d [%s])',
          [RS_Preparing,
          container.CurrentDownloadChapterPtr + 1,
          container.ChapterLinks.Count,
          container.ChapterName.Strings[container.CurrentDownloadChapterPtr]]);
        container.Status := STATUS_PREPARE;
        CheckOut;
        WaitForThreads;
        if Terminated then Exit;
      end;

      //Check file, if exist set mark 'D', otherwise 'W' or 'G' for dynamic image url
      if container.PageLinks.Count > 0 then
      begin
        for j := 0 to container.PageLinks.Count - 1 do
        begin
          P := S + PathDelim + Format('%.3d', [j + 1]);
          if (FileExistsUTF8(P + '.jpg')) or
            (FileExistsUTF8(P + '.png')) or
            (FileExistsUTF8(P + '.gif')) then
            container.PageLinks[j] := 'D'
          else
          if container.PageLinks[j] = 'D' then
            if SitesWithoutPageLink(WebsiteRoots[container.MangaSiteID, 0]) then
              container.PageLinks[j] := 'G'
            else
              container.PageLinks[j] := 'W';
        end;
      end;

      //Get page links
      if container.PageLinks.Count = 0 then
        container.PageLinks.Add('W');
      container.PageNumber := container.PageLinks.Count;
      if not SitesWithoutPageLink(container.DownloadInfo.Website) and
        CheckForPrepare then
      begin
        Flag := CS_GETPAGELINK;
        container.WorkCounter := 0;
        container.DownCounter := 0;
        container.DownloadInfo.iProgress := 0;
        container.DownloadInfo.Progress :=
          Format('%d/%d', [container.DownCounter, container.PageNumber]);
        container.DownloadInfo.Status :=
          Format('%s (%d/%d [%s])',
          [RS_Preparing,
          container.CurrentDownloadChapterPtr + 1,
          container.ChapterLinks.Count,
          container.ChapterName[container.CurrentDownloadChapterPtr]]);
        container.Status := STATUS_PREPARE;
        while container.WorkCounter < container.PageNumber do
        begin
          if Terminated then Exit;
          Checkout;
          container.DownloadInfo.iProgress :=
            InterLockedIncrement(container.DownloadInfo.iProgress);
        end;
        WaitForThreads;
        if Terminated then Exit;

        //check if pagelink is found. Else set again to 'W'(some script return '')
        if container.PageLinks.Count > 0 then
        begin
          for j := 0 to container.PageLinks.Count - 1 do
          begin
            if Trim(container.PageLinks[j]) = '' then
              container.PageLinks[j] := 'W';
          end;
        end;
      end;
      if Terminated then Exit;

      // download pages
      // If container doesn't have any image, we will skip the loop. Otherwise
      // download them
      container.PageNumber := container.PageLinks.Count;
      if (container.PageLinks.Count > 0) then
      begin
        Flag := CS_DOWNLOAD;
        container.WorkCounter := 0;
        container.DownCounter := 0;
        container.DownloadInfo.iProgress := 0;
        container.DownloadInfo.Progress :=
          Format('%d/%d', [container.DownCounter, container.PageNumber]);
        container.Status := STATUS_DOWNLOAD;
        container.DownloadInfo.Status :=
          Format('%s (%d/%d) [%s]',
          [RS_Downloading,
          container.CurrentDownloadChapterPtr + 1,
          container.ChapterLinks.Count,
          container.ChapterName.Strings[container.CurrentDownloadChapterPtr]]);
        while container.WorkCounter < container.PageLinks.Count do
        begin
          if Terminated then Exit;
          Checkout;
          container.DownloadInfo.iProgress :=
            InterLockedIncrement(container.DownloadInfo.iProgress);
        end;
        WaitForThreads;
        if Terminated then Exit;

        //check if all page is downloaded
        if CheckForFinish then
        begin
          container.Status := STATUS_COMPRESS;
          container.DownloadInfo.Progress := '';
          Compress;
        end
        else
          container.Status := STATUS_FAILED;
      end
      else
        container.Status := STATUS_FAILED;

      if (container.Status = STATUS_FAILED) and
         (not FindStrLinear(container.FailedChapterLinks,
          container.ChapterName[container.CurrentDownloadChapterPtr])) then
      begin
        container.FailedChapterName.Add(container.ChapterName[container.CurrentDownloadChapterPtr]);
        container.FailedChapterLinks.Add(container.ChapterLinks[container.CurrentDownloadChapterPtr]);
      end;

      container.CurrentPageNumber := 0;
      container.PageLinks.Clear;
      container.PageContainerLinks.Clear;
      container.CurrentDownloadChapterPtr :=
        InterLockedIncrement(container.CurrentDownloadChapterPtr);
    end;

    if container.FailedChapterLinks.Count > 0 then
    begin
      container.Status := STATUS_FAILED;
      container.DownloadInfo.Status := RS_FailedTryResumeTask;
      container.DownloadInfo.Progress := '';
      container.CurrentDownloadChapterPtr := 0;

      container.ChapterName.Assign(container.FailedChapterName);
      container.ChapterLinks.Assign(container.FailedChapterLinks);
      container.FailedChapterName.Clear;
      container.FailedChapterLinks.Clear;
    end
    else
    begin
      container.Status := STATUS_FINISH;
      container.DownloadInfo.Status := RS_Finish;
      container.DownloadInfo.Progress := '';
    end;
    Synchronize(ShowBaloon);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TTaskThread.DoTerminate;
var
  i: Integer;
begin
  if threads.Count > 0 then
  begin
    CS_threads.Acquire;
    try
      for i := 0 to threads.Count - 1 do
        TDownloadThread(threads[i]).Terminate;
    finally
      CS_threads.Release;
    end;
    while threads.Count > 0 do
      Sleep(100);
  end;
  Stop;
  container.DownloadInfo.TransferRate := '';
  container.ThreadState := False;
  inherited DoTerminate;
end;

procedure TTaskThread.Stop(const check: Boolean = True);
begin
  if container.Status = STATUS_STOP then
    Exit;
  if check and not (container.Manager.isReadyForExit) then
  begin
    if (container.WorkCounter >= container.PageLinks.Count) and
      (container.CurrentDownloadChapterPtr >= container.ChapterLinks.Count)
      and (container.FailedChapterLinks.Count = 0) then
    begin
      container.Status := STATUS_FINISH;
      container.DownloadInfo.Status := RS_Finish;
      container.DownloadInfo.Progress := '';
      container.Manager.CheckAndActiveTask(True, Self);
    end
    else
    if (container.Status in [STATUS_PROBLEM, STATUS_FAILED]) then
      container.Manager.CheckAndActiveTask(True, Self)
    else
    begin
      container.Status := STATUS_STOP;
      container.DownloadInfo.Status :=
        Format('%s (%d/%d)', [RS_Stopped, container.CurrentDownloadChapterPtr +
        1, container.ChapterLinks.Count]);
      container.Manager.CheckAndActiveTask(False, Self);
    end;
  end;
end;

{ TTaskContainer }

procedure TTaskContainer.IncReadCount(const ACount: Integer);
begin
  CS_FReadCount.Acquire;
  try
    Inc(FReadCount, ACount);
  finally
    CS_FReadCount.Release;
  end;
end;

constructor TTaskContainer.Create;
begin
  inherited Create;
  ThreadState := False;
  CS_FReadCount := TCriticalSection.Create;
  ChapterLinks := TStringList.Create;
  ChapterName := TStringList.Create;
  FailedChapterName := TStringList.Create;
  FailedChapterLinks := TStringList.Create;
  PageLinks := TStringList.Create;
  PageContainerLinks := TStringList.Create;
  FReadCount := 0;
  WorkCounter := 0;
  CurrentPageNumber := 0;
  CurrentDownloadChapterPtr := 0;
end;

destructor TTaskContainer.Destroy;
begin
  PageContainerLinks.Free;
  PageLinks.Free;
  ChapterName.Free;
  ChapterLinks.Free;
  FailedChapterName.Free;
  FailedChapterLinks.Free;
  CS_FReadCount.Free;
  inherited Destroy;
end;

{ TDownloadManager }

function TDownloadManager.GetTaskCount: Integer;
begin
  Result := Containers.Count;
end;

function TDownloadManager.GetTransferRate: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Containers.Count > 0 then
  begin
    CS_DownloadManager_Task.Acquire;
    try
      FTotalReadCount := 0;
      for i := 0 to Containers.Count - 1 do
        with TTaskContainer(Containers[i]) do
          if ThreadState then
          begin
            CS_FReadCount.Acquire;
            try
              if Status = STATUS_COMPRESS then
                DownloadInfo.TransferRate := ''
              else
              begin
                DownloadInfo.TransferRate := FormatByteSize(FReadCount, True);
                Inc(FTotalReadCount, FReadCount);
              end;
              FReadCount := 0;
            finally
              CS_FReadCount.Release;
            end;
          end;
      Result := FTotalReadCount;
    finally
      CS_DownloadManager_Task.Release;
    end;
  end;
end;

constructor TDownloadManager.Create;
begin
  inherited Create;
  CS_DownloadManager_Task := TCriticalSection.Create;
  CS_DownloadedChapterList := TCriticalSection.Create;

  DownloadManagerFile := TIniFile.Create(WORK_FOLDER + WORK_FILE);
  DownloadManagerFile.CacheUpdates := True;

  DownloadedChaptersListFile := TStringList.Create;

  if FileExists(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE) then
    DownloadedChaptersListFile.LoadFromFile(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE);

  Containers := TFPList.Create;
  isFinishTaskAccessed := False;
  isRunningBackup := False;
  isRunningBackupDownloadedChaptersList := False;
  isReadyForExit := False;
end;

destructor TDownloadManager.Destroy;
begin
  CS_DownloadManager_Task.Acquire;
  try
    while Containers.Count > 0 do
    begin
      TTaskContainer(Containers.Last).Free;
      Containers.Remove(Containers.Last);
    end;
  finally
    CS_DownloadManager_Task.Release;
  end;
  FreeAndNil(Containers);
  FreeAndNil(DownloadedChaptersListFile);
  FreeAndNil(DownloadManagerFile);
  CS_DownloadedChapterList.Free;
  CS_DownloadManager_Task.Free;
  inherited Destroy;
end;

function TDownloadManager.TaskItem(const Index: Integer): TTaskContainer;
begin
  if (Index < 0) or (Containers.Count < 0) then
    Exit(nil);
  Result := TTaskContainer(Containers[Index]);
end;

procedure TDownloadManager.BackupDownloadedChaptersList;
begin
  if CS_DownloadedChapterList.TryEnter then
  begin
    CS_DownloadedChapterList.Acquire;
    try
      DownloadedChaptersListFile.SaveToFile(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE);
    finally
      CS_DownloadedChapterList.Release;
    end;
  end;
end;

procedure TDownloadManager.Restore;
var
  tid, s: String;
  tmp, i, j: Integer;
begin
  CS_DownloadManager_Task.Acquire;
  try
    while Containers.Count > 0 do
    begin
      TTaskContainer(Containers.Last).Free;
      Containers.Remove(Containers.Last);
    end;

    tmp := DownloadManagerFile.ReadInteger('general', 'NumberOfTasks', 0);
    if tmp = 0 then
      Exit;
    for i := 0 to tmp - 1 do
    begin
      // Restore chapter links, chapter name and page links
      Containers.Add(TTaskContainer.Create);
      with DownloadManagerFile, TTaskContainer(Containers.Last) do begin
        tid := 'task' + IntToStr(i);
        Manager := Self;
        DownloadInfo.Website := ReadString(tid, 'Website', 'NULL');
        DownloadInfo.Link := ReadString(tid, 'Link', '');
        DownloadInfo.Title := ReadString(tid, 'Title', 'NULL');
        DownloadInfo.SaveTo := CorrectPathSys(ReadString(tid, 'SaveTo', 'NULL'));
        DownloadInfo.Status := ReadString(tid, 'Status', 'NULL');
        DownloadInfo.Progress := ReadString(tid, 'Progress', 'NULL');
        s := ReadString(tid, 'ChapterLinks', '');
        if s <> '' then GetParams(ChapterLinks, s);
        s := ReadString(tid, 'ChapterName', '');
        if s <> '' then GetParams(ChapterName, s);
        s := ReadString(tid, 'FailedChapterLinks', '');
        if s <> '' then GetParams(FailedChapterLinks, s);
        s := ReadString(tid, 'FailedChapterName', '');
        if s <> '' then GetParams(FailedChapterName, s);
        s := ReadString(tid, 'PageLinks', '');
        if s <> '' then GetParams(PageLinks, s);
        s := ReadString(tid, 'PageContainerLinks', '');
        if s <> '' then GetParams(PageContainerLinks, s);        //deprecated, for old config
        j := ReadInteger(tid, 'TaskStatus', -1);
        if j >= 0 then
          Status := TDownloadStatusType(j)
        else
        begin
          s := ReadString(tid, 'TaskStatus', 'STATUS_STOP');
          Status := TDownloadStatusType(GetEnumValue(TypeInfo(TDownloadStatusType), s));
          if Status = STATUS_COMPRESS then
            Status := STATUS_WAIT;
        end;
        CurrentDownloadChapterPtr := ReadInteger(tid, 'ChapterPtr', 0);
        PageNumber := ReadInteger(tid, 'NumberOfPages', 0);
        CurrentPageNumber := ReadInteger(tid, 'CurrentPage', 0);
        if Status = STATUS_COMPRESS then
          DownloadInfo.Status := RS_Waiting;
        s := ReadString(tid, 'DateTime', '');
        //for old config
        if (Pos('/', s) > 0) or (Pos('\', s) > 0) then
          DownloadInfo.dateTime := StrToDateDef(s, Now)
        else
        begin
          s := StringReplace(s, ',', FMDFormatSettings.DecimalSeparator, [rfReplaceAll]);
          s := StringReplace(s, '.', FMDFormatSettings.DecimalSeparator, [rfReplaceAll]);
          DownloadInfo.dateTime := StrToFloatDef(s, Now, FMDFormatSettings);
        end;
        if DownloadInfo.dateTime > Now then DownloadInfo.dateTime := Now;

        MangaSiteID := GetMangaSiteID(DownloadInfo.website);
        ThreadState := False;

        //validating
        if (CurrentDownloadChapterPtr > 0) and (CurrentDownloadChapterPtr >= ChapterLinks.Count) then
          CurrentDownloadChapterPtr := ChapterLinks.Count - 1;
      end;
    end;
  finally
    CS_DownloadManager_Task.Release;
  end;
end;

procedure TDownloadManager.Backup;
var
  i: Integer;
  tid: String;
begin
  if isRunningBackup then
    Exit;

  CS_DownloadManager_Task.Acquire;
  isRunningBackup := True;
  try
    DownloadManagerFile.CacheUpdates := True;
    // Erase all sections
    for i := 0 to DownloadManagerFile.ReadInteger('general', 'NumberOfTasks', 0) do
      DownloadManagerFile.EraseSection('task' + IntToStr(i));
    DownloadManagerFile.EraseSection('general');

    // backup
    if Containers.Count > 0 then
    begin
      DownloadManagerFile.WriteInteger('general', 'NumberOfTasks', Containers.Count);
      for i := 0 to Containers.Count - 1 do
      begin
        tid := 'task' + IntToStr(i);
        with DownloadManagerFile, TTaskContainer(TaskItem(i)) do begin
          WriteString(tid, 'Website', DownloadInfo.Website);
          WriteString(tid, 'Link', DownloadInfo.Link);
          WriteString(tid, 'Title', DownloadInfo.Title);
          WriteString(tid, 'SaveTo', DownloadInfo.SaveTo);
          WriteString(tid, 'Status', DownloadInfo.Status);
          WriteString(tid, 'Progress', DownloadInfo.Progress);
          WriteString(tid, 'DateTime', FloatToStr(DownloadInfo.dateTime, FMDFormatSettings));
          WriteString(tid, 'ChapterLinks', SetParams(ChapterLinks));
          WriteString(tid, 'ChapterName', SetParams(ChapterName));
          if FailedChapterLinks.Count > 0 then
            WriteString(tid, 'FailedChapterLinks', SetParams(FailedChapterLinks));
          if FailedChapterName.Count > 0 then
            WriteString(tid, 'FailedChapterName', SetParams(FailedChapterName));
          if PageLinks.Count > 0 then
            WriteString(tid, 'PageLinks', SetParams(PageLinks));
          if PageContainerLinks.Count > 0 then
            WriteString(tid, 'PageContainerLinks', SetParams(PageContainerLinks));
          WriteString(tid, 'TaskStatus', GetEnumName(TypeInfo(TDownloadStatusType), integer(Status)));
          WriteInteger(tid, 'ChapterPtr', CurrentDownloadChapterPtr);
          WriteInteger(tid, 'NumberOfPages', PageNumber);
          WriteInteger(tid, 'CurrentPage', CurrentPageNumber);
        end;
      end;
    end;
    DownloadManagerFile.UpdateFile;
  finally
    isRunningBackup := False;
    CS_DownloadManager_Task.Release;
  end;
end;

procedure TDownloadManager.ClearTransferRate;
var
  i: Integer;
begin
  if Containers.Count > 0 then
  begin
    CS_DownloadManager_Task.Acquire;
    try
      FTotalReadCount := 0;
      for i := 0 to Containers.Count - 1 do
        with TTaskContainer(Containers[i]) do
        begin
          CS_FReadCount.Acquire;
          try
            FReadCount := 0;
            DownloadInfo.TransferRate := '';
          finally
            CS_FReadCount.Release;
          end;
        end;
    finally
      CS_DownloadManager_Task.Release;
    end;
  end;
end;

procedure TDownloadManager.AddToDownloadedChaptersList(const ALink, AValue: String);
var
  st: TStringList;
begin
  if (ALink <> '') and (AValue <> '') then
  begin
    st := TStringList.Create;
    try
      GetParams(st, AValue);
      AddToDownloadedChaptersList(ALink, st);
    finally
      St.Free;
    end;
  end;
end;

procedure TDownloadManager.AddToDownloadedChaptersList(const Alink: String;
  AValue: TStrings);
var
  i, p, q: Integer;
  Ch, dlCh: TStringList;
begin
  if (Alink <> '') and  (AValue.Count > 0) then
  begin
    CS_DownloadedChapterList.Acquire;
    Ch := TStringList.Create;
    dlCh := TStringList.Create;
    try
      p := -1;
      //locate the link
      if DownloadedChaptersListFile.Count > 1 then
        for i := 0 to DownloadedChaptersListFile.Count - 1 do
          if SameText(Alink, DownloadedChaptersListFile[i]) then
          begin
            p := i;
            if i + 1 < DownloadedChaptersListFile.Count then
              GetParams(dlCh, DownloadedChaptersListFile[i + 1]);
            Break;
          end;

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
      if p > -1 then
      begin
        if p + 1 < DownloadedChaptersListFile.Count then
          DownloadedChaptersListFile[p + 1] := DownloadedChaptersListFile[p + 1] + SetParams(Ch)
        else
          DownloadedChaptersListFile.Add(SetParams(Ch));
      end
      else
      begin
        //if it's new data
        DownloadedChaptersListFile.Add(Alink);
        DownloadedChaptersListFile.Add(SetParams(Ch));
      end;
    finally
      dlCh.Free;
      Ch.Free;
      CS_DownloadedChapterList.Release;
    end;
  end;
end;

procedure TDownloadManager.GetDownloadedChaptersState(const Alink: String;
  var Chapters: array of TChapterStateItem);
var
  dlCh   : TStringList;
  i, p, q: Integer;
begin
  if (Alink <> '') and (Length(Chapters) > 0) and
    (DownloadedChaptersListFile.Count > 0) then
  begin
    CS_DownloadedChapterList.Acquire;
    try
      p := -1;
      //locate the link
      for i := 0 to DownloadedChaptersListFile.Count - 1 do
        if SameText(Alink, DownloadedChaptersListFile[i]) then
        begin
          if i + 1 < DownloadedChaptersListFile.Count then
            if DownloadedChaptersListFile[i + 1] <> '' then
              p := i + 1;
          Break;
        end;

      //compare the content
      if p > -1 then
      begin
        dlCh := TStringList.Create;
        try
          GetParams(dlCh, DownloadedChaptersListFile[p]);
          if dlCh.Count > 0 then
          begin
            dlCh.Sort;
            for i := Low(Chapters) to High(Chapters) do
              Chapters[i].Downloaded := dlCh.Find(Chapters[i].Link, q);
          end;
        finally
          dlCh.Free;
        end;
      end
      else
      begin
        for i := Low(Chapters) to High(Chapters) do
          Chapters[i].Downloaded := False;
      end;
    finally
      CS_DownloadedChapterList.Release;
    end;
  end;
end;

function TDownloadManager.AddTask : Integer;
begin
  Result := -1;
  CS_DownloadManager_Task.Acquire;
  try
    Result := Containers.Add(TTaskContainer.Create);
    TTaskContainer(Containers.Last).Manager := Self;
  finally
    CS_DownloadManager_Task.Release;
  end;
end;

procedure TDownloadManager.CheckAndActiveTask(const isCheckForFMDDo : Boolean;
  SenderThread : TThread);
var
  EHENTAI_Count: Integer = 0;
  Count: Integer = 0;
  i: Integer;

  procedure ShowExitCounter;
  begin
    if OptionLetFMDDo in [DO_NOTHING, DO_UPDATE] then Exit;
    if ThreadID <> MainThreadID then
    begin
      {$IFDEF FPC271}
      TThread.Synchronize(TThread.CurrentThread, doExitWaitCounter);
      {$ELSE}
      if SenderThread <> nil then
        TThread.Synchronize(SenderThread, doExitWaitCounter);
      {$ENDIF}
    end
    else
      doExitWaitCounter;
  end;

begin
  if Containers.Count = 0 then
    Exit;
  CS_DownloadManager_Task.Acquire;
  try
    try
      for i := 0 to Containers.Count - 1 do
      begin
        if (TaskItem(i).Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_COMPRESS]) then
        begin
          Inc(Count);
          if TaskItem(i).MangaSiteID = EHENTAI_ID then
            Inc(EHENTAI_Count);
        end;
      end;

      if Count < maxDLTasks then
      begin
        for i := 0 to Containers.Count - 1 do
        begin
          if Count >= maxDLTasks then
            Break;
          if (TaskItem(i).Status = STATUS_WAIT) then
          begin
            if (TaskItem(i).MangaSiteID = EHENTAI_ID) and
              (EHENTAI_Count < EHENTAI_maxDLTask) then
            begin
              Inc(EHENTAI_Count);
              Inc(Count);
              ActiveTask(i);
            end
            else
            begin
              ActiveTask(i);
              Inc(Count);
            end;
          end;
        end;
      end;

      if Count > 0 then
      begin
        if not MainForm.itRefreshDLInfo.Enabled then
          MainForm.itRefreshDLInfo.Enabled := True;
      end
      else
        MainForm.itRefreshDLInfo.Enabled := False;

      Self.Backup;
      MainForm.vtDownloadFilters;

      if (Count = 0) and (isCheckForFMDDo) then
        ShowExitCounter;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
  finally
    CS_DownloadManager_Task.Release;
  end;
end;

function TDownloadManager.CanActiveTask(const pos: Integer): Boolean;
var
  EHENTAI_Count: Integer = 0;
  i: Integer;
  Count: Integer = 0;
begin
  Result := True;

  if (Containers.Count = 0) or (pos >= Containers.Count) then
    Exit(False);

  for i := 0 to Containers.Count - 1 do
  begin
    if (TaskItem(i).Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_COMPRESS]) and
      (i <> pos) then
    begin
      Inc(Count);
      if (TaskItem(i).MangaSiteID = EHENTAI_ID) then
        Inc(EHENTAI_Count);
    end;
  end;

  if (TaskItem(pos).MangaSiteID = EHENTAI_ID) and
    (EHENTAI_Count >= EHENTAI_maxDLTask) then
    Result := False
  else
  if Count >= maxDLTasks then
    Result := False;
end;

procedure TDownloadManager.CheckAndActiveTaskAtStartup;

  procedure ActiveTaskAtStartup(const taskID: Integer);
  begin
    if taskID >= Containers.Count then
      Exit;
    if (not Assigned(TaskItem(taskID))) then
      Exit;
    if (TaskItem(taskID).Status = STATUS_WAIT) or
      (TaskItem(taskID).Status = STATUS_STOP) or
      (TaskItem(taskID).Status = STATUS_FINISH) then
      Exit;
    TaskItem(taskID).Status := STATUS_DOWNLOAD;
    TaskItem(taskID).Thread := TTaskThread.Create;
    TaskItem(taskID).Thread.container := TaskItem(taskID);
    TaskItem(taskID).Thread.Start;
  end;

var
  i: Integer;
  Count: Integer = 0;
begin
  if Containers.Count = 0 then
    Exit;
  for i := 0 to Containers.Count - 1 do
  begin
    if (TaskItem(i).Status = STATUS_DOWNLOAD) or
      (TaskItem(i).Status = STATUS_PREPARE) then
    begin
      if Count < maxDLTasks then
      begin
        ActiveTaskAtStartup(i);
        Inc(Count);
      end
      else
      begin
        TaskItem(i).Status := STATUS_WAIT;
        TaskItem(i).DownloadInfo.Status := RS_Waiting;
      end;
    end;
  end;
  MainForm.vtDownloadFilters;
  //force to check task if all task loaded is STATUS_WAIT
  CheckAndActiveTask;
end;

procedure TDownloadManager.ActiveTask(const taskID: Integer);
begin
  if taskID < Containers.Count then
  begin
    if Assigned(TaskItem(taskID)) then
    begin
      if not((TaskItem(taskID).Status = STATUS_DOWNLOAD) or
        (TaskItem(taskID).Status = STATUS_PREPARE) or
        (TaskItem(taskID).Status = STATUS_FINISH)) then
      begin
        TaskItem(taskID).Status := STATUS_DOWNLOAD;
        TaskItem(taskID).DownloadInfo.Status := RS_Downloading;
        TaskItem(taskID).Thread := TTaskThread.Create;
        TaskItem(taskID).Thread.container := TaskItem(taskID);
        TaskItem(taskID).Thread.Start;
        MainForm.vtDownload.Repaint;
        MainForm.vtDownloadFilters;
      end;
    end;
    if not MainForm.itRefreshDLInfo.Enabled then
      MainForm.itRefreshDLInfo.Enabled := True;
  end;
end;

procedure TDownloadManager.StopTask(const taskID: Integer;
  const isCheckForActive: Boolean; isWaitFor: Boolean);
begin
  if taskID < Containers.Count then
  begin
    if TaskItem(taskID).Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT] then
    begin
      isReadyForExit := False;
      if TaskItem(taskID).Status = STATUS_WAIT then
      begin
        TaskItem(taskID).Status := STATUS_STOP;
        TaskItem(taskID).DownloadInfo.Status := RS_Stopped;
      end;
      if TaskItem(taskID).ThreadState then
      begin
        TaskItem(taskID).Thread.Terminate;
        if isWaitFor then
          TaskItem(taskID).Thread.WaitFor;
      end;
      if isCheckForActive then
      begin
        Backup;
        CheckAndActiveTask;
      end;
      MainForm.vtDownload.Repaint;
      MainForm.vtDownloadFilters;
    end;
  end;
end;

procedure TDownloadManager.StartAllTasks;
var
  i: Integer;
begin
  if Containers.Count > 0 then
  begin
    for i := 0 to Containers.Count - 1 do
    begin
      if TaskItem(i).Status in [STATUS_STOP, STATUS_FAILED, STATUS_PROBLEM] then
      begin
        TaskItem(i).Status := STATUS_WAIT;
        TaskItem(i).DownloadInfo.Status := RS_Waiting;
      end;
    end;
    Backup;
    CheckAndActiveTask;
    MainForm.vtDownload.Repaint;
    MainForm.vtDownloadFilters;
  end;
end;

procedure TDownloadManager.StopAllTasks;
var
  i: Integer;
begin
  if Containers.Count > 0 then
  begin
    isReadyForExit := False;
    for i := 0 to Containers.Count - 1 do
    begin
      if TaskItem(i).Status = STATUS_WAIT then
      begin
        TaskItem(i).Status := STATUS_STOP;
        TaskItem(i).DownloadInfo.Status := RS_Stopped;
      end;
      if TaskItem(i).ThreadState then
        TaskItem(i).Thread.Terminate;
    end;
    Backup;
    MainForm.vtDownload.Repaint;
    MainForm.vtDownloadFilters;
  end;
end;

procedure TDownloadManager.StopAllDownloadTasksForExit;
var
  i: Integer;
begin
  if Containers.Count > 0 then
  begin
    try
      isReadyForExit := True;
      for i := 0 to Containers.Count - 1 do
        if TaskItem(i).ThreadState then
          TaskItem(i).Thread.Terminate;
      for i := 0 to Containers.Count - 1 do
        if TaskItem(i).ThreadState then
          TaskItem(i).Thread.WaitFor;
    finally
      Backup;
    end;
  end;
end;

procedure TDownloadManager.RemoveTask(const taskID: Integer);
begin
  if taskID < Containers.Count then
  begin
    CS_DownloadManager_Task.Acquire;
    try
      if TaskItem(taskID).ThreadState then
      begin
        TaskItem(taskID).Status := STATUS_STOP;
        TaskItem(taskID).Thread.Terminate;
        TaskItem(taskID).Thread.WaitFor;
      end;
      TaskItem(taskID).Free;
      Containers.Delete(taskID);
    finally
      CS_DownloadManager_Task.Release;
    end;
    CheckAndActiveTask;
  end;
end;

procedure TDownloadManager.RemoveAllFinishedTasks;
var
  i: Integer;
begin
  if Containers.Count = 0 then
    Exit;
  // remove
  i := 0;
  repeat
    if TaskItem(i).Status = STATUS_FINISH then
    begin
      Containers.Delete(i);
    end
    else
      Inc(i);
  until i >= Containers.Count;
end;

procedure TDownloadManager.doExitWaitCounter;
begin
  frmMain.DoAfterFMD := OptionLetFMDDo;
  MainForm.itMonitor.Enabled := True;
end;

function TDownloadManager.TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Containers.Count > 0 then
  begin
    CS_DownloadManager_Task.Acquire;
    try
      for i := 0 to Containers.Count - 1 do
      begin
        if TaskItem(i).Status in Stats then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      CS_DownloadManager_Task.Release;
    end;
  end;
end;

procedure TDownloadManager.Sort(const AColumn: Integer);

  function GetStr(ARow: Pointer): String;
  begin
    case AColumn of
      0: Result := TTaskContainer(ARow).DownloadInfo.title;
      1: Result := TTaskContainer(ARow).DownloadInfo.Status;
      2: Result := TTaskContainer(ARow).DownloadInfo.Progress;
      3: Result := TTaskContainer(ARow).DownloadInfo.TransferRate;
      4: Result := TTaskContainer(ARow).DownloadInfo.Website;
      5: Result := TTaskContainer(ARow).DownloadInfo.SaveTo;
      6: Result := FloatToStr(TTaskContainer(ARow).DownloadInfo.dateTime, FMDFormatSettings);
    end;
  end;

  function GetAddedDate(ARow: Pointer): TDateTime;
  begin
    Result := TTaskContainer(ARow).DownloadInfo.DateTime;
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
    case AColumn of
      6 : Result := CompareDateTime(GetAddedDate(Item1), GetAddedDate(Item2));
      else
        Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
    end;
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
  if Containers.Count < 2 then Exit;
  CS_DownloadManager_Task.Acquire;
  try
    SortColumn := AColumn;
    QSort(Containers, 0, Containers.Count - 1);
  finally
    CS_DownloadManager_Task.Release;
  end;
  MainForm.vtDownload.Repaint;
  MainForm.vtDownloadFilters;
end;

end.
