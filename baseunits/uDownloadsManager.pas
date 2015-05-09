{
        File: uDownloadsManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uDownloadsManager;

{$mode delphi}

interface

uses
  lazutf8classes, jsHTMLUtil, FastHTMLParser, HTMLUtil, SynaCode, FileUtil,
  Controls, VirtualTrees, RegExpr, Imaging, ImagingTypes, ImagingCanvases,
  Classes, SysUtils, Dialogs, ExtCtrls, IniFiles, fgl, typinfo, syncobjs,
  httpsend, blcksock, uBaseUnit, uPacker, uFMDThread, uMisc, frmShutdownCounter;

type
  TDownloadManager = class;
  TTaskThreadContainer = class;
  TTaskThread = class;

  // this class will replace the old TDownloadThread

  { TDownloadThread }

  TDownloadThread = class(TFMDThread)
  protected
    parse: TStringList;
    checkStyle: TFlagType;
    workCounter: Cardinal;
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
    function DownloadImage(const AHTTP: THTTPSend = nil;
      const prefix: String = ''): Boolean;

    procedure OnTag(NoCaseTag, ActualTag: string);
    procedure OnText(Text: String);

    procedure SockOnHeartBeat(Sender: TObject);
    //Need recheck later|wrapper
    function GetPage(const AHTTP: THTTPSend; var output: TObject;
      URL: String; const Reconnect: Cardinal; const isByPassHTTP: Boolean): Boolean;
      overload;
    function GetPage(const AHTTP: THTTPSend; var output: TObject;
      URL: String; const Reconnect: Cardinal): Boolean; overload;
    function GetPage(var output: TObject; URL: String; const Reconnect: Cardinal;
      const isByPassHTTP: Boolean): Boolean; overload;
    function GetPage(var output: TObject; URL: String;
      const Reconnect: Cardinal): Boolean; overload;

    function SaveImage(const AOwner: TObject; const AHTTP: THTTPSend;
      const mangaSiteID: Integer; URL: String; const Path, Name, prefix: String;
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
    property GetworkCounter: Cardinal read workCounter;
  end;

  TDownloadThreadList = TFPGList<TDownloadThread>;

  { TTaskThread }

  TTaskThread = class(TFMDThread)
  protected
    FMessage, FAnotherURL: String;

    procedure CheckOut;
    procedure MainThreadCompressRepaint;
    procedure MainThreadRepaint;
    procedure MainThreadMessageDialog;
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure Compress;
    // show notification when download completed
    procedure ShowBaloon;
  public
    //additional parameter
    httpCookies: String;
    Flag: TFlagType;
    // container (for storing information)
    container: TTaskThreadContainer;
    // download threads
    threads: TDownloadThreadList;
    CS_threads: TCriticalSection;

    constructor Create;
    destructor Destroy; override;
    procedure Stop(const check: Boolean = True);

    property AnotherURL: String read FAnotherURL write FAnotherURL;
  end;

  TTaskThreadContainer = class
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
    Status: TStatusType;
    ThreadState: Boolean;

    ChapterName, ChapterLinks, FailedChapterName, FailedChapterLinks, PageContainerLinks, PageLinks: TStringList;

    constructor Create;
    destructor Destroy; override;
  end;

  TTaskThreadContainerList = TFPGList<TTaskThreadContainer>;

  { TDownloadManager }

  TDownloadManager = class
  private
    FSortDirection: Boolean;
    FSortColumn: Cardinal;
  public
    CS_DownloadManager_Task: TCriticalSection;
    isRunningBackup, isFinishTaskAccessed, isRunningBackupDownloadedChaptersList,
    isReadyForExit: Boolean;

    compress, retryConnect,
    // max. active tasks
    maxDLTasks,
    // max. download threads per task
    maxDLThreadsPerTask: Integer;
    // current chapterLinks which thread is processing
    containers: TTaskThreadContainerList;

    downloadedChaptersList: TStringList;
    ini: TIniFile;

    // for highlight downloaded chapters
    DownloadedChapterList: TList;

    //exit counter
    ExitType: TExitType;
    ExitWaitOK: Boolean;

    // downloadInfo        : array of TDownloadInfo;
    constructor Create;
    destructor Destroy; override;

    procedure BackupDownloadedChaptersList;

    procedure Restore;
    procedure Backup;

    // These methods relate to highlight downloaded chapters.
    procedure AddToDownloadedChaptersList(const ALink: String); overload;
    procedure AddToDownloadedChaptersList(const ALink, AValue: String); overload;
    procedure ReturnDownloadedChapters(const ALink: String);

    // Add new task to the list.
    function AddTask:Integer;
    // Check and active previous work-in-progress tasks.
    procedure CheckAndActiveTaskAtStartup;
    // Check and active waiting tasks.
    procedure CheckAndActiveTask(const isCheckForFMDDo: Boolean = False;
      SenderThread: TThread = nil);
    // Check if we can active another wating task or not.
    function CanActiveTask(const pos : Integer) : Boolean;
    // Active a stopped task.
    procedure ActiveTask(const taskID : Integer);
    // Stop a download/wait task.
    procedure StopTask(const taskID : Integer; const isCheckForActive : Boolean =
      True);
    // Stop all download/wait tasks.
    procedure StopAllTasks;
    // Stop all download task inside a task before terminate the program.
    procedure StopAllDownloadTasksForExit;
    // Swap 2 tasks.
    function Swap(const id1, id2 : Integer) : Boolean;
    // move a task up.
    function MoveUp(const taskID : Integer) : Boolean;
    // move a task down.
    function MoveDown(const taskID : Integer) : Boolean;
    // Remove a task from list.
    procedure RemoveTask(const taskID : Integer);
    // Remove all finished tasks.
    procedure RemoveAllFinishedTasks;
    // show exit counter
    procedure doExitWaitCounter;

    // Sort.
    procedure Sort(const AColumn: Cardinal);
    procedure SortNatural(const AColumn: Integer);

    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Cardinal read FSortColumn write FSortColumn;
  end;

resourcestring
  RS_FailedToCreateDirTooLong = 'Failed to create dir! Too long?';
  RS_FailedTryResumeTask = 'Failed, try resuming this task!';

implementation

uses
  frmMain;

// ----- TDownloadThread -----

procedure TDownloadThread.OnTag(NoCaseTag, ActualTag : string);
begin
  parse.Add(ActualTag);
end;

procedure TDownloadThread.OnText(Text: String);
begin
  parse.Add(Text);
end;

constructor TDownloadThread.Create;
begin
  inherited Create(True);
  FHTTP := THTTPSend.Create;
  FHTTP.Headers.NameValueSeparator := ':';
  FHTTP.Sock.OnHeartbeat := SockOnHeartBeat;
  FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
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

function TDownloadThread.GetPage(const AHTTP: THTTPSend; var output: TObject;
  URL: String; const Reconnect: Cardinal; const isByPassHTTP: Boolean): Boolean;
  overload;
begin
  FHTTP.Clear;
  Result := uBaseUnit.GetPage(Self, FHTTP, output, URL, Reconnect, isByPassHTTP);
end;

function TDownloadThread.GetPage(const AHTTP: THTTPSend; var output: TObject;
  URL: String; const Reconnect: Cardinal): Boolean;
begin
  Result := GetPage(AHTTP, output, URL, Reconnect, False);
end;

function TDownloadThread.GetPage(var output: TObject; URL: String;
  const Reconnect: Cardinal; const isByPassHTTP: Boolean): Boolean;
begin
  Result := GetPage(nil, output, URL, Reconnect, isByPassHTTP);
end;

function TDownloadThread.GetPage(var output: TObject; URL: String;
  const Reconnect: Cardinal): Boolean;
begin
  Result := GetPage(nil, output, URL, Reconnect, False);
end;

function TDownloadThread.SaveImage(const AOwner: TObject; const AHTTP: THTTPSend;
  const mangaSiteID: Integer; URL: String; const Path, Name, prefix: String;
  const Reconnect: Cardinal): Boolean;
begin
  Result := uBaseUnit.SaveImage(Self, FHTTP, mangaSiteID, URL, Path,
    Name, prefix, Reconnect);
end;

function TDownloadThread.SaveImage(const mangaSiteID: Integer; URL: String;
  const Path, Name, prefix: String; const Reconnect: Cardinal): Boolean;
begin
  Result := SaveImage(nil, nil, mangaSiteID, URL, Path, Name, prefix, Reconnect);
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
      Synchronize(manager.container.Thread.MainThreadRepaint);
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

  {$I includes/Batoto/chapter_page_number.inc}

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

begin
  manager.container.PageNumber := 0;
  if manager.container.MangaSiteID = ANIMEA_ID then
    Result := GetAnimeAPageNumber
  else
  if manager.container.MangaSiteID = MANGAHERE_ID then
    Result := GetMangaHerePageNumber
  else
  if manager.container.MangaSiteID = MANGAINN_ID then
    Result := GetMangaInnPageNumber
  else
  if manager.container.MangaSiteID = BATOTO_ID then
    Result := GetBatotoPageNumber
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
    Result := False;
end;

function TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
var
  Parser: THTMLParser;

  {$I includes/AnimeStory/image_url.inc}

  {$I includes/AnimExtremist/image_url.inc}

  {$I includes/Batoto/image_url.inc}

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
  if (manager.container.PageLinks.Count > 0) and
   (manager.container.PageLinks.Strings[workCounter] <> 'W') then
    Exit;

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
  if manager.container.MangaSiteID = BATOTO_ID then
    Result := GetBatotoImageURL
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
    Result := GetMangaAtImageURL
  else
    Result := False;
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
  threads := TDownloadThreadList.Create;
  anotherURL := '';
  httpCookies := '';
end;

destructor TTaskThread.Destroy;
begin
  threads.Free;
  CS_threads.Free;
  container.ThreadState := False;
  inherited Destroy;
end;

procedure TTaskThread.MainThreadRepaint;
begin
  MainForm.isCanRefreshForm := True;
end;

procedure TTaskThread.MainThreadCompressRepaint;
begin
  container.DownloadInfo.Status :=
    Format('%s (%d/%d)', [stIsCompressing, container.CurrentDownloadChapterPtr +
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
      '"' + container.DownloadInfo.title + '" - ' + stFailed;;
  end
  else
  if container.Status = STATUS_FINISH then
  begin
    MainForm.TrayIcon.BalloonFlags := bfInfo;
    MainForm.TrayIcon.BalloonHint :=
      '"' + container.DownloadInfo.title + '" - ' + stFinish;
  end;
  MainForm.TrayIcon.ShowBalloonHint;
end;

function TDownloadThread.DownloadImage(const AHTTP: THTTPSend = nil;
  const prefix: String = ''): Boolean;
var
  TURL, lpath: String;

  {$I includes/EHentai/image_url.inc}

  {$I includes/MeinManga/image_url.inc}

  {$I includes/SenMangaRAW/image_url.inc}

begin
  lpath := CorrectPathSys(manager.container.DownloadInfo.SaveTo +
    manager.container.ChapterName[manager.container.CurrentDownloadChapterPtr]);
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
    Result := SaveImage(Self,
      AHTTP,
      manager.container.MangaSiteID,
      TURL,
      lpath,
      Format('%.3d', [workCounter + 1]),
      prefix,
      manager.container.Manager.retryConnect);

  SetCurrentDirUTF8(fmdDirectory);
  if Terminated then
    Result := False;

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
  // TODO: Ugly code, need to be fixed later

  //load advanced config if any
  INIAdvanced.Reload;
  mt := INIAdvanced.ReadInteger('DownloadMaxThreadsPerTask',
    WebsiteRoots[container.MangaSiteID, 0], -1);
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
      Sleep(250);
  end;

  if (not Terminated) and (threads.Count < currentMaxThread) then
  begin
    CS_threads.Acquire;
    try
      threads.Add(TDownloadThread.Create);
      threads.Last.manager := Self;
      threads.Last.workCounter := container.WorkCounter;
      threads.Last.checkStyle := Flag;
      threads.Last.Start;
      container.WorkCounter := InterLockedIncrement(container.WorkCounter);
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
      Sleep(250);
  end;

var
  j: Integer;
  S, P: String;
begin
  container.ThreadState := True;
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
          Synchronize(MainThreadRepaint);
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
          [stPreparing,
          container.CurrentDownloadChapterPtr + 1,
          container.ChapterLinks.Count,
          container.ChapterName.Strings[container.CurrentDownloadChapterPtr]]);
        container.Status := STATUS_PREPARE;
        Synchronize(MainThreadRepaint);
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
      if not SitesWithoutPageLink(WebsiteRoots[container.MangaSiteID, 0]) and
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
          [stPreparing,
          container.CurrentDownloadChapterPtr + 1,
          container.ChapterLinks.Count,
          container.ChapterName[container.CurrentDownloadChapterPtr]]);
        container.Status := STATUS_PREPARE;
        Synchronize(MainThreadRepaint);
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
          [stDownloading,
          container.CurrentDownloadChapterPtr + 1,
          container.ChapterLinks.Count,
          container.ChapterName.Strings[container.CurrentDownloadChapterPtr]]);
        Synchronize(MainThreadRepaint);
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
      container.DownloadInfo.Status := stFinish;
      container.DownloadInfo.Progress := '';
    end;
    Synchronize(MainThreadRepaint);
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
        threads[i].Terminate;
    finally
      CS_threads.Release;
    end;
    while threads.Count > 0 do
      Sleep(16);
  end;
  Stop;
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
      container.DownloadInfo.Status := stFinish;
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
        Format('%s (%d/%d)', [stStop, container.CurrentDownloadChapterPtr +
        1, container.ChapterLinks.Count]);
      container.Manager.CheckAndActiveTask(False, Self);
    end;
    Synchronize(MainThreadRepaint);
  end;
end;

// ----- TTaskThreadContainer -----

constructor TTaskThreadContainer.Create;
begin
  inherited Create;
  ThreadState := False;
  ChapterLinks := TStringList.Create;
  ChapterName := TStringList.Create;
  FailedChapterName := TStringList.Create;
  FailedChapterLinks := TStringList.Create;
  PageLinks := TStringList.Create;
  PageContainerLinks := TStringList.Create;
  WorkCounter := 0;
  CurrentPageNumber := 0;
  CurrentDownloadChapterPtr := 0;
end;

destructor TTaskThreadContainer.Destroy;
begin
  PageContainerLinks.Free;
  PageLinks.Free;
  ChapterName.Free;
  ChapterLinks.Free;
  FailedChapterName.Free;
  FailedChapterLinks.Free;
  inherited Destroy;
end;

// ----- TDownloadManager -----

constructor TDownloadManager.Create;
begin
  inherited Create;
  CS_DownloadManager_Task := TCriticalSection.Create;

  ini := TIniFile.Create(WORK_FOLDER + WORK_FILE);
  ini.CacheUpdates := True;

  downloadedChaptersList := TStringList.Create;
  DownloadedChapterList := TList.Create;

  if FileExists(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE) then
    downloadedChaptersList.LoadFromFile(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE);

  containers := TTaskThreadContainerList.Create;
  isFinishTaskAccessed := False;
  isRunningBackup := False;
  isRunningBackupDownloadedChaptersList := False;
  isReadyForExit := False;
end;

destructor TDownloadManager.Destroy;
begin
  CS_DownloadManager_Task.Acquire;
  try
    while containers.Count > 0 do
    begin
      containers.Last.Free;
      containers.Remove(containers.Last);
    end;
  finally
    CS_DownloadManager_Task.Release;
  end;
  FreeAndNil(containers);
  FreeAndNil(downloadedChaptersList);
  FreeAndNil(DownloadedChapterList);
  FreeAndNil(ini);
  CS_DownloadManager_Task.Free;
  inherited Destroy;
end;

procedure TDownloadManager.BackupDownloadedChaptersList;
begin
  if isRunningBackupDownloadedChaptersList then
    Exit;
  isRunningBackupDownloadedChaptersList := True;
  try
    downloadedChaptersList.SaveToFile(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE);
  finally
    isRunningBackupDownloadedChaptersList := False;
  end;
end;

procedure TDownloadManager.Restore;
var
  s: String;
  tmp, i, j: Integer;
begin
  CS_DownloadManager_Task.Acquire;
  try
    while containers.Count > 0 do
    begin
      containers.Last.Free;
      containers.Remove(containers.Last);
    end;

    tmp := ini.ReadInteger('general', 'NumberOfTasks', 0);
    if tmp = 0 then
      Exit;
    for i := 0 to tmp - 1 do
    begin
      containers.Add(TTaskThreadContainer.Create);
      containers.Last.Manager := Self;

      // Restore chapter links, chapter name and page links
      s := ini.ReadString('task' + IntToStr(i), 'ChapterLinks', '');
      if s <> '' then
        GetParams(containers.Last.ChapterLinks, s);
      s := ini.ReadString('task' + IntToStr(i), 'ChapterName', '');
      if s <> '' then
        GetParams(containers.Last.ChapterName, s);
      s := ini.ReadString('task' + IntToStr(i), 'FailedChapterLinks', '');
      if s <> '' then
        GetParams(containers.Last.FailedChapterLinks, s);
      s := ini.ReadString('task' + IntToStr(i), 'FailedChapterName', '');
      if s <> '' then
        GetParams(containers.Last.FailedChapterName, s);
      s := ini.ReadString('task' + IntToStr(i), 'PageLinks', '');
      if s <> '' then
        GetParams(containers.Last.PageLinks, s);
      s := ini.ReadString('task' + IntToStr(i), 'PageContainerLinks', '');
      if s <> '' then
        GetParams(containers.Last.PageContainerLinks, s);

      //deprecated, for old config
      j := ini.ReadInteger('task' + IntToStr(i), 'TaskStatus', -1);
      if j >= 0 then
        containers.Last.Status := TStatusType(j)
      else
      begin
        s := ini.ReadString('task' + IntToStr(i), 'TaskStatus', 'STATUS_STOP');
        containers.Last.Status :=
          TStatusType(GetEnumValue(TypeInfo(TStatusType), s));

        if containers.Last.Status = STATUS_COMPRESS then
          containers.Last.Status := STATUS_WAIT;
      end;
      containers.Last.CurrentDownloadChapterPtr :=
        ini.ReadInteger('task' + IntToStr(i), 'ChapterPtr', 0);
      containers.Last.PageNumber :=
        ini.ReadInteger('task' + IntToStr(i), 'NumberOfPages', 0);
      containers.Last.CurrentPageNumber :=
        ini.ReadInteger('task' + IntToStr(i), 'CurrentPage', 0);
      containers.Last.DownloadInfo.title :=
        ini.ReadString('task' + IntToStr(i), 'Title', 'NULL');
      containers.Last.DownloadInfo.status :=
        ini.ReadString('task' + IntToStr(i), 'Status', 'NULL');
      containers.Last.DownloadInfo.Progress :=
        ini.ReadString('task' + IntToStr(i), 'Progress', 'NULL');
      containers.Last.DownloadInfo.website :=
        ini.ReadString('task' + IntToStr(i), 'Website', 'NULL');
      containers.Last.DownloadInfo.saveTo :=
        CorrectPathSys(ini.ReadString('task' + IntToStr(i), 'SaveTo', 'NULL'));

      if containers.Last.Status = STATUS_COMPRESS then
        containers.Last.DownloadInfo.Status := stWait;

      s := ini.ReadString('task' + IntToStr(i), 'DateTime', '');
      //for old config
      if (Pos('/', s) > 0) or (Pos('\', s) > 0) then
        containers.Last.DownloadInfo.dateTime := StrToDateDef(s, Now)
      else
      begin
        s := StringReplace(s, ',', FMDFormatSettings.DecimalSeparator, [rfReplaceAll]);
        s := StringReplace(s, '.', FMDFormatSettings.DecimalSeparator, [rfReplaceAll]);
        containers.Last.DownloadInfo.dateTime := StrToFloatDef(s, Now, FMDFormatSettings);
      end;
      if containers.Last.DownloadInfo.dateTime > Now then
        containers.Last.DownloadInfo.dateTime := Now;

      containers.Last.MangaSiteID :=
        GetMangaSiteID(containers.Last.DownloadInfo.website);
      containers.Last.ThreadState := False;

      //validating
      if (containers.Last.CurrentDownloadChapterPtr > 0) and
        (containers.Last.CurrentDownloadChapterPtr >=
        containers.Last.ChapterLinks.Count) then
        containers.Last.CurrentDownloadChapterPtr := containers.Last.ChapterLinks.Count - 1;
    end;
  finally
    CS_DownloadManager_Task.Release;
  end;
end;

procedure TDownloadManager.Backup;
var
  i: Integer;
begin
  if isRunningBackup then
    Exit;

  CS_DownloadManager_Task.Acquire;
  isRunningBackup := True;
  try
    ini.CacheUpdates := True;
    // Erase all sections
    for i := 0 to ini.ReadInteger('general', 'NumberOfTasks', 0) do
      ini.EraseSection('task' + IntToStr(i));
    ini.EraseSection('general');

    // backup
    if containers.Count > 0 then
    begin
      ini.WriteInteger('general', 'NumberOfTasks', containers.Count);

      for i := 0 to containers.Count - 1 do
      begin
        ini.WriteString('task' + IntToStr(i), 'ChapterLinks',
          SetParams(containers.Items[i].ChapterLinks));
        ini.WriteString('task' + IntToStr(i), 'ChapterName',
          SetParams(containers.Items[i].ChapterName));
        if containers.Items[i].FailedChapterLinks.Count > 0 then
          ini.WriteString('task' + IntToStr(i), 'FailedChapterLinks',
            SetParams(containers.Items[i].FailedChapterLinks));
        if containers.Items[i].FailedChapterName.Count > 0 then
          ini.WriteString('task' + IntToStr(i), 'FailedChapterName',
            SetParams(containers.Items[i].FailedChapterName));
        if containers.Items[i].PageLinks.Count > 0 then
          ini.WriteString('task' + IntToStr(i), 'PageLinks',
            SetParams(containers.Items[i].PageLinks));
        if containers.Items[i].PageContainerLinks.Count > 0 then
          ini.WriteString('task' + IntToStr(i), 'PageContainerLinks',
            SetParams(containers.Items[i].PageContainerLinks));

        ini.WriteString('task' + IntToStr(i), 'TaskStatus',
          GetEnumName(TypeInfo(TStatusType), integer(containers.Items[i].Status)));

        ini.WriteInteger('task' + IntToStr(i), 'ChapterPtr',
          containers.Items[i].CurrentDownloadChapterPtr);
        ini.WriteInteger('task' + IntToStr(i), 'NumberOfPages',
          containers.Items[i].PageNumber);
        ini.WriteInteger('task' + IntToStr(i), 'CurrentPage',
          containers.Items[i].CurrentPageNumber);

        ini.WriteString('task' + IntToStr(i), 'Title',
          containers.Items[i].DownloadInfo.title);
        ini.WriteString('task' + IntToStr(i), 'Status',
          containers.Items[i].DownloadInfo.status);
        ini.WriteString('task' + IntToStr(i), 'Progress',
          containers.Items[i].DownloadInfo.Progress);
        ini.WriteString('task' + IntToStr(i), 'Website',
          containers.Items[i].DownloadInfo.website);
        ini.WriteString('task' + IntToStr(i), 'SaveTo',
          containers.Items[i].DownloadInfo.saveTo);

        ini.WriteString('task' + IntToStr(i), 'DateTime',
          FloatToStr(containers.Items[i].DownloadInfo.dateTime, FMDFormatSettings));
      end;
    end;
    ini.UpdateFile;
  finally
    isRunningBackup := False;
    CS_DownloadManager_Task.Release;
  end;
end;

procedure TDownloadManager.AddToDownloadedChaptersList(const ALink: String);
var
  i: Cardinal;
  LValue: String;
  Node: PVirtualNode;
begin
  // generate LValue string
  LValue := '';
  if frmMain.MainForm.clbChapterList.RootNodeCount = 0 then
    Exit;
  Node := frmMain.MainForm.clbChapterList.GetFirst;
  for i := 0 to frmMain.MainForm.clbChapterList.RootNodeCount - 1 do
  begin
    if Node.CheckState = csCheckedNormal then
      LValue := LValue + IntToStr(i) + SEPERATOR;
    Node := frmMain.MainForm.clbChapterList.GetNext(Node);
  end;
  if LValue = '' then
    Exit;

  if DownloadedChaptersList.Count > 0 then
  begin
    i := 0;
    while i < DownloadedChaptersList.Count do
    begin
      if CompareText(ALink, DownloadedChaptersList.Strings[i]) = 0 then
      begin
        DownloadedChaptersList.Strings[i] := ALink;
        DownloadedChaptersList.Strings[i + 1] :=
          RemoveDuplicateNumbersInString(DownloadedChaptersList.Strings[i + 1] + LValue);
        Exit;
      end;
      Inc(i, 2);
    end;
  end;
  if DownloadedChaptersList.Count > 4000 then
  begin
    DownloadedChaptersList.Delete(0);
    DownloadedChaptersList.Delete(0);
  end;
  DownloadedChaptersList.Add(ALink);
  DownloadedChaptersList.Add(LValue);
end;

procedure TDownloadManager.AddToDownloadedChaptersList(const ALink, AValue: String);
var
  i: Cardinal;
begin
  if DownloadedChaptersList.Count > 0 then
  begin
    i := 0;
    while i < DownloadedChaptersList.Count do
    begin
      if CompareText(ALink, DownloadedChaptersList.Strings[i]) = 0 then
      begin
        DownloadedChaptersList.Strings[i] := ALink;
        DownloadedChaptersList.Strings[i + 1] :=
          RemoveDuplicateNumbersInString(DownloadedChaptersList.Strings[i + 1] + AValue);
        Exit;
      end;
      Inc(i, 2);
    end;
  end;
  if DownloadedChaptersList.Count > 4000 then
  begin
    DownloadedChaptersList.Delete(0);
    DownloadedChaptersList.Delete(0);
  end;
  DownloadedChaptersList.Add(ALink);
  DownloadedChaptersList.Add(AValue);
end;

procedure TDownloadManager.ReturnDownloadedChapters(const ALink: String);
var
  i: Cardinal;
begin
  // clear the list
  DownloadedChapterList.Clear;

  if DownloadedChaptersList.Count > 0 then
  begin
    i := 0;
    while i < DownloadedChaptersList.Count do
    begin
      if CompareText(ALink, DownloadedChaptersList.Strings[i]) = 0 then
      begin
        GetParams(DownloadedChapterList, DownloadedChaptersList.Strings[i + 1]);
        Exit;
      end;
      Inc(i, 2);
    end;
  end;
end;

function TDownloadManager.AddTask : Integer;
begin
  Result := -1;
  CS_DownloadManager_Task.Acquire;
  try
    Result := containers.Add(TTaskThreadContainer.Create);
    containers.Last.Manager := Self;
  finally
    CS_DownloadManager_Task.Release;
  end;
end;

procedure TDownloadManager.CheckAndActiveTask(const isCheckForFMDDo : Boolean;
  SenderThread : TThread);
var
  EHENTAI_Count: Cardinal = 0;
  Count: Cardinal = 0;
  i: Integer;

  procedure ShowExitCounter;
  begin
    if ThreadID <> MainThreadID then
    begin
      {$IF FPC_FULLVERSION >= 20701}
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
  if containers.Count = 0 then
    Exit;
  CS_DownloadManager_Task.Acquire;
  try
    try
      for i := 0 to containers.Count - 1 do
      begin
        if (containers.Items[i].Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_COMPRESS]) then
        begin
          Inc(Count);
          if containers.Items[i].MangaSiteID = EHENTAI_ID then
            Inc(EHENTAI_Count);
        end;
      end;

      if Count < maxDLTasks then
      begin
        for i := 0 to containers.Count - 1 do
        begin
          if Count >= maxDLTasks then
            Break;
          if (containers.Items[i].Status = STATUS_WAIT) then
          begin
            if (containers.Items[i].MangaSiteID = EHENTAI_ID) and
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

      if (Count = 0) and (isCheckForFMDDo) then
        if MainForm.cbOptionLetFMDDo.ItemIndex > 0 then
        begin
          case MainForm.cbOptionLetFMDDo.ItemIndex of
            DO_EXIT_FMD: ExitType := etExit;
            DO_TURNOFF: ExitType := etShutdown;
            DO_HIBERNATE: ExitType := etHibernate;
          end;
          ShowExitCounter;
        end;
      MainForm.vtDownloadFilters;
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

  if (containers.Count = 0) or (pos >= containers.Count) then
    Exit(False);

  for i := 0 to containers.Count - 1 do
  begin
    if (containers.Items[i].Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_COMPRESS]) and
      (i <> pos) then
    begin
      Inc(Count);
      if (containers.Items[i].MangaSiteID = EHENTAI_ID) then
        Inc(EHENTAI_Count);
    end;
  end;

  if (containers.Items[pos].MangaSiteID = EHENTAI_ID) and
    (EHENTAI_Count >= EHENTAI_maxDLTask) then
    Result := False
  else
  if Count >= maxDLTasks then
    Result := False;
end;

procedure TDownloadManager.CheckAndActiveTaskAtStartup;

  procedure ActiveTaskAtStartup(const taskID: Integer);
  begin
    if taskID >= containers.Count then
      Exit;
    if (not Assigned(containers.Items[taskID])) then
      Exit;
    if (containers.Items[taskID].Status = STATUS_WAIT) or
      (containers.Items[taskID].Status = STATUS_STOP) or
      (containers.Items[taskID].Status = STATUS_FINISH) then
      Exit;
    containers.Items[taskID].Status := STATUS_DOWNLOAD;
    containers.Items[taskID].Thread := TTaskThread.Create;
    containers.Items[taskID].Thread.container := containers.Items[taskID];
    containers.Items[taskID].Thread.Start;
  end;

var
  i: Integer;
  Count: Integer = 0;
begin
  if containers.Count = 0 then
    Exit;
  for i := 0 to containers.Count - 1 do
  begin
    if (containers.Items[i].Status = STATUS_DOWNLOAD) or
      (containers.Items[i].Status = STATUS_PREPARE) then
    begin
      if Count < maxDLTasks then
      begin
        ActiveTaskAtStartup(i);
        Inc(Count);
      end
      else
      begin
        containers.Items[i].Status := STATUS_WAIT;
        containers.Items[i].DownloadInfo.Status := stWait;
      end;
    end;
  end;
  MainForm.vtDownloadFilters;
  //force to check task if all task loaded is STATUS_WAIT
  CheckAndActiveTask;
end;

procedure TDownloadManager.ActiveTask(const taskID: Integer);
begin
  // conditions
  if taskID >= containers.Count then
    Exit;
  if (not Assigned(containers.Items[taskID])) then
    Exit;
  if (containers.Items[taskID].Status = STATUS_DOWNLOAD) or
    (containers.Items[taskID].Status = STATUS_PREPARE) or
    (containers.Items[taskID].Status = STATUS_FINISH) then
    Exit;
  containers.Items[taskID].Status := STATUS_DOWNLOAD;
  containers.Items[taskID].DownloadInfo.Status := stDownloading;
  containers.Items[taskID].Thread := TTaskThread.Create;
  containers.Items[taskID].Thread.container := containers.Items[taskID];
  containers.Items[taskID].Thread.Start;
  // TODO
  MainForm.vtDownloadFilters;
end;

procedure TDownloadManager.StopTask(const taskID: Integer;
  const isCheckForActive: Boolean = True);
begin
  if taskID < containers.Count then
  begin
    isReadyForExit := False;
    containers.Items[taskID].Status := STATUS_STOP;
    containers.Items[taskID].DownloadInfo.Status := stStop;
    if containers.Items[taskID].ThreadState then
    begin
      containers.Items[taskID].Thread.Terminate;
      //containers.Items[taskID].Thread.WaitFor;
    end;
    if isCheckForActive then
    begin
      Backup;
      CheckAndActiveTask;
    end;
    MainForm.vtDownloadFilters;
  end;
end;

procedure TDownloadManager.StopAllTasks;
var
  i: Integer;
begin
  if containers.Count > 0 then
  begin
    isReadyForExit := False;
    for i := 0 to containers.Count - 1 do
    begin
      containers.Items[i].Status := STATUS_STOP;
      containers.Items[i].DownloadInfo.Status := stStop;
      // stop any active threads
      if containers.Items[i].ThreadState then
        containers.Items[i].Thread.Terminate;
    end;
    // wait for threads
    for i := 0 to containers.Count - 1 do
      if containers.Items[i].ThreadState then
        containers.Items[i].Thread.WaitFor;
    Backup;
    MainForm.vtDownload.Repaint;
    MainForm.vtDownloadFilters;
  end;
end;

procedure TDownloadManager.StopAllDownloadTasksForExit;
var
  i: Integer;
begin
  if containers.Count > 0 then
  begin
    try
      isReadyForExit := True;
      for i := 0 to containers.Count - 1 do
        if containers.Items[i].ThreadState then
          containers.Items[i].Thread.Terminate;
      for i := 0 to containers.Count - 1 do
        if containers.Items[i].ThreadState then
          containers.Items[i].Thread.WaitFor;
    finally
      Backup;
    end;
  end;
end;

// swap 2 task
function TDownloadManager.Swap(const id1, id2 : Integer) : Boolean;
var
  tmp: TTaskThreadContainer;
begin
  if (id1 >= containers.Count) or (id2 >= containers.Count) then
    Exit(False);
  tmp := containers.Items[id1];
  containers.Items[id1] := containers.Items[id2];
  containers.Items[id2] := tmp;
  Result := True;
end;

// move a task down
function TDownloadManager.MoveDown(const taskID: Integer): Boolean;
var
  tmp: TTaskThreadContainer;
begin
  if (taskID < containers.Count - 1) then
  begin
    tmp := containers.Items[taskID];
    containers.Items[taskID] := containers.Items[taskID + 1];
    containers.Items[taskID + 1] := tmp;
    Result := True;
  end
  else
    Result := False;
  MainForm.vtDownloadFilters;
end;

// move a task up
function TDownloadManager.MoveUp(const taskID: Integer): Boolean;
var
  tmp: TTaskThreadContainer;
begin
  if (taskID > 0) and (taskID <= containers.Count - 1) then
  begin
    tmp := containers.Items[taskID];
    containers.Items[taskID] := containers.Items[taskID - 1];
    containers.Items[taskID - 1] := tmp;
    Result := True;
  end
  else
    Result := False;
  MainForm.vtDownloadFilters;
end;

procedure TDownloadManager.RemoveTask(const taskID: Integer);
begin
  if taskID < containers.Count then
  begin
    CS_DownloadManager_Task.Acquire;
    try
      if containers.Items[taskID].ThreadState then
      begin
        containers.Items[taskID].Status := STATUS_STOP;
        containers.Items[taskID].Thread.Terminate;
        containers.Items[taskID].Thread.WaitFor;
      end;
      containers.Items[taskID].Free;
      containers.Delete(taskID);
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
  if containers.Count = 0 then
    Exit;
  // remove
  i := 0;
  repeat
    if containers.Items[i].Status = STATUS_FINISH then
    begin
      containers.Delete(i);
    end
    else
      Inc(i);
  until i >= containers.Count;
end;

procedure TDownloadManager.doExitWaitCounter;
begin
  with TShutdownCounterForm.Create(MainForm) do try
    case Self.ExitType of
      etShutdown: WaitTimeout := 60;
      etHibernate: WaitTimeout := 30;
      etExit: WaitTimeout := 5;
    end;
    frmExitType := Self.ExitType;
    ExitWaitOK := False;
    if ShowModal = mrOK then
      ExitWaitOK := True;
  finally
    Free;
  end;

  if ExitWaitOK then
  begin
    case Self.ExitType of
      etShutdown: DoAfterFMD := DoFMDNothing;
      etHibernate: DoAfterFMD := DoFMDHibernate;
      etExit: DoAfterFMD := DoFMDExit;
    end;
    MainForm.itMonitor.Enabled := True;
  end;
end;

procedure TDownloadManager.Sort(const AColumn: Cardinal);

  function GetStr(const ARow: Cardinal): String;
  begin
    case AColumn of
      0: Result := containers.Items[ARow].DownloadInfo.title;
      3: Result := containers.Items[ARow].DownloadInfo.Website;
      4: Result := containers.Items[ARow].DownloadInfo.SaveTo;
      5: Result := FloatToStr(containers.Items[ARow].DownloadInfo.dateTime);
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
      case SortDirection of
        False:
        begin
          case AColumn of
            5:
            begin
              while StrToFloat(GetStr(i)) < StrToFloat(X) do
                Inc(i);
              while StrToFloat(GetStr(j)) > StrToFloat(X) do
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
            5:
            begin
              while StrToFloat(GetStr(i)) > StrToFloat(X) do
                Inc(i);
              while StrToFloat(GetStr(j)) < StrToFloat(X) do
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
        Swap(i, j);
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
  if containers.Count <= 2 then
    Exit;
  sortColumn := AColumn;
  QSort(0, containers.Count - 1);
  MainForm.vtDownload.Repaint;
  MainForm.vtDownloadFilters;
end;

procedure TDownloadManager.SortNatural(const AColumn: Integer);

  function GetStr(const ARow: Integer): String;
  begin
    case AColumn of
      0: Result := containers.Items[ARow].DownloadInfo.title;
      1: Result := containers.Items[ARow].DownloadInfo.Status;
      2: Result := containers.Items[ARow].DownloadInfo.Progress;
      3: Result := containers.Items[ARow].DownloadInfo.Website;
      4: Result := containers.Items[ARow].DownloadInfo.SaveTo;
      5: Result := FloatToStr(containers.Items[ARow].DownloadInfo.dateTime, FMDFormatSettings);
      else
        Result := '';
    end;
  end;

  function GetFloat(const ARow: Integer): Double;
  begin
    Result := containers.Items[ARow].DownloadInfo.dateTime;
  end;

  procedure QuickSortA(L, R: Integer);
  var
    Pivot, vL, vR: Integer;
    PivotDbl: Double;
    PivotStr: String;
  begin
    if R - L <= 1 then  // a little bit of time saver
    begin
      if L < R then
        if SortDirection then
          if AnsiNaturalCompare(GetStr(L), GetStr(R)) < 0 then
            Swap(L, R)
          else
          if AnsiNaturalCompare(GetStr(L), GetStr(R)) > 0 then
            Swap(L, R);
      Exit;
    end;
    vL := L;
    vR := R;
    Pivot := L + Random(R - L); // they say random is best
    if AColumn = 5 then
      PivotDbl := GetFloat(Pivot)
    else
      PivotStr := GetStr(Pivot);
    while vL < vR do
    begin
      if SortDirection then
      begin
        if AColumn = 5 then //sorting datetime
        begin
          while (vL < Pivot) and (GetFloat(vL) > PivotDbl) do
            Inc(vL);
          while (vR > Pivot) and (GetFloat(vR) <= PivotDbl) do
            Dec(vR);
        end
        else
        begin
          while (vL < Pivot) and (AnsiNaturalCompare(GetStr(vL), PivotStr) > 0) do
            Inc(vL);
          while (vR > Pivot) and (AnsiNaturalCompare(GetStr(vR), PivotStr) <= 0) do
            Dec(vR);
        end;
      end
      else
      begin
        if AColumn = 5 then //sorting datetime
        begin
          while (vL < Pivot) and (GetFloat(vL) <= PivotDbl) do
            Inc(vL);
          while (vR > Pivot) and (GetFloat(vR) > PivotDbl) do
            Dec(vR);
        end
        else
        begin
          while (vL < Pivot) and (AnsiNaturalCompare(GetStr(vL), PivotStr) <= 0) do
            Inc(vL);
          while (vR > Pivot) and (AnsiNaturalCompare(GetStr(vR), PivotStr) > 0) do
            Dec(vR);
        end;
      end;
      Swap(vL, vR);
      if Pivot = vL then // swap pivot if we just hit it from one side
      begin
        Pivot := vR;
        if AColumn = 5 then
          PivotDbl := GetFloat(Pivot)
        else
          PivotStr := GetStr(Pivot);
      end
      else
      if Pivot = vR then
      begin
        Pivot := vL;
        if AColumn = 5 then
          PivotDbl := GetFloat(Pivot)
        else
          PivotStr := GetStr(Pivot);
      end;
    end;
    if Pivot - 1 >= L then
      QuickSortA(L, Pivot - 1);
    if Pivot + 1 <= R then
      QuickSortA(Pivot + 1, R);
  end;

begin
  if containers.Count > 1 then
  begin;
    sortColumn := AColumn;
    QuickSortA(0, containers.Count - 1);
    MainForm.vtDownload.Repaint;
    MainForm.vtDownloadFilters;
  end;
end;

end.
