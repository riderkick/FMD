{
        File: downloads.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uDownloadsManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, ExtCtrls, Graphics, dateutils,
  uBaseUnit, uData, fgl, uPacker, uFMDThread;

type
  TDownloadManager = class;
  TTaskThreadContainer = class;
  TTaskThread = class;

  // this class will replace the old TDownloadThread
  TDownloadThread = class(TFMDThread)
  protected
    parse        : TStringList;
    workCounter      : Cardinal;

    FSortColumn  : Cardinal;
    FAnotherURL  : String;

    // wait for changing directoet completed
    procedure   SetChangeDirectoryFalse;
    procedure   SetChangeDirectoryTrue;
    // Get download link from URL
    function    GetLinkPageFromURL(const URL: String): Boolean;
    // Get number of download link from URL
    function    GetPageNumberFromURL(const URL: String): Boolean;
    // Download page - links are from link list
    function    DownloadPage: Boolean;
    procedure   Execute; override;
    procedure   OnTag(tag: String);
    procedure   OnText(text: String);
  public
    checkStyle    : Cardinal;
    // ID of the site
    manager       : TTaskThread;

    constructor Create;
    destructor  Destroy; override;

    property    SortColumn: Cardinal read FSortColumn write FSortColumn;
    property    AnotherURL: String read FAnotherURL write FAnotherURL;
  end;

  TDownloadThreadList = TFPGList<TDownloadThread>;

  TTaskThread = class(TFMDThread)
  protected
    FAnotherURL  : String;

    procedure   CheckOut;
    procedure   MainThreadCompressRepaint;
    procedure   MainThreadRepaint;
    procedure   MainThreadRepaintImm;
    procedure   Execute; override;
    procedure   Compress;
    // show notification when download completed
    procedure   ShowBaloon;
  public
    Flag: Cardinal;
    // container (for storing information)
    container  : TTaskThreadContainer;
    // download threads
    threads    : TDownloadThreadList;

    constructor Create;
    destructor  Destroy; override;
    procedure   Stop(const check: Boolean = TRUE);

    property    AnotherURL: String read FAnotherURL write FAnotherURL;
  end;

  TTaskThreadContainer = class
    // task thread of this container
    thread : TTaskThread;
    // download manager
    manager: TDownloadManager;

    downloadInfo: TDownloadInfo;

    // current link index
    currentPageNumber,
    // current chapter index
    currentDownloadChapterPtr,
    activeThreadCount,
    Status     : Cardinal;
    workCounter    : Cardinal;
    mangaSiteID: Cardinal;
    pageNumber : Cardinal;

    chapterName,
    chapterLinks,
    pageContainerLinks,
    pageLinks  : TStringList;

    constructor Create;
    destructor  Destroy; override;
  end;

  TTaskThreadContainerList = TFPGList<TTaskThreadContainer>;

  TDownloadManager = class(TObject)
  private
    FSortDirection: Boolean;
    FSortColumn   : Cardinal;
  public
    isRunningBackup,
    isFinishTaskAccessed,
    isRunningBackupDownloadedChaptersList: Boolean;

    compress,
    //
    retryConnect,
    // max. active tasks
    maxDLTasks,
    // max. download threads per task
    maxDLThreadsPerTask : Cardinal;
    // current chapterLinks which thread is processing
    containers          : TTaskThreadContainerList;

    downloadedChaptersList: TStringList;
    ini                 : TIniFile;

    // for highlight downloaded chapters
    DownloadedChapterList: TList;

   // downloadInfo        : array of TDownloadInfo;
    constructor Create;
    destructor  Destroy; override;

    procedure   BackupDownloadedChaptersList;

    procedure   Restore;
    procedure   Backup;
    procedure   SaveJobList;

    procedure   AddToDownloadedChaptersList(const ALink: String); overload;
    procedure   AddToDownloadedChaptersList(const ALink, AValue: String); overload;
    procedure   ReturnDownloadedChapters(const ALink: String);

    // Add new task to the list
    procedure   AddTask;
    // Check and active previous work-in-progress tasks
    procedure   CheckAndActiveTaskAtStartup;
    // Check and active waiting tasks
    procedure   CheckAndActiveTask(const isCheckForFMDDo: Boolean = FALSE);
    // Check if we can active another wating task or not
    function    CanActiveTask(const pos: Cardinal): Boolean;
    // Active a stopped task
    procedure   ActiveTask(const taskID: Cardinal);
    // Stop a download/wait task
    procedure   StopTask(const taskID: Cardinal; const isCheckForActive: Boolean = TRUE);
    // Stop all download/wait tasks
    procedure   StopAllTasks;
    // Stop all download task inside a task before terminate the program
    procedure   StopAllDownloadTasksForExit;
    // Mark the task as "Finished"
    procedure   FinishTask(const taskID: Cardinal);
    // Swap 2 tasks
    function    Swap(const id1, id2: Cardinal): Boolean;
    // move a task up
    function    MoveUp(const taskID: Cardinal): Boolean;
    // move a task down
    function    MoveDown(const taskID: Cardinal): Boolean;
    // Remove a task from list
    procedure   RemoveTask(const taskID: Cardinal);
    // Remove all finished tasks
    procedure   RemoveAllFinishedTasks;

    // sorting
    procedure   Sort(const AColumn: Cardinal);

    property    SortDirection: Boolean read FSortDirection write FSortDirection;
    property    SortColumn: Cardinal read FSortColumn write FSortColumn;
  end;

implementation

uses
  lazutf8classes, FastHTMLParser, HTMLUtil, LConvEncoding,
  SynaCode, FileUtil, HTTPSend, VirtualTrees, frmMain;

// ----- TDownloadThread -----

procedure   TDownloadThread.OnTag(tag: String);
begin
  parse.Add(tag);
end;

procedure   TDownloadThread.OnText(text: String);
begin
  parse.Add(text);
end;

constructor TDownloadThread.Create;
begin
  isTerminated:= FALSE;
  isSuspended := TRUE;
  FreeOnTerminate:= TRUE;
  inherited Create(FALSE);
end;

destructor  TDownloadThread.Destroy;
begin
  // TODO: Need recheck
  try
   // if NOT Terminated2 then
    Dec(manager.container.activeThreadCount);
  except
  end;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TDownloadThread.Execute;
var
  i: Cardinal;
begin
  while isSuspended do
    Sleep(100);
  case checkStyle of
    // get page number, and prepare number of pagelinks for save links
    CS_GETPAGENUMBER:
      begin
        GetPageNumberFromURL(manager.container.chapterLinks.Strings[manager.container.currentDownloadChapterPtr]);
        // prepare 'space' for link updater
       // if manager.container.mangaSiteID <> GEHENTAI_ID then
        if (NOT Terminated) AND
           (manager.container.pageNumber > 0) then
          for i:= 0 to manager.container.pageNumber-1 do
            manager.container.pageLinks.Add('W');
      end;
    // get page link
    CS_GETPAGELINK:
      begin
        if (NOT Terminated) then
          GetLinkPageFromURL(manager.container.chapterLinks.Strings[manager.container.currentDownloadChapterPtr]);
      end;
    // download page
    CS_DOWNLOAD:
      begin
        if (NOT Terminated) then
          DownloadPage;
      end;
  end;
  Terminate;
end;

function    TDownloadThread.GetPageNumberFromURL(const URL: String): Boolean;
var
  Parser  : TjsFastHTMLParser;

  // Get chapter's number of pages
  {$I includes/AnimeA/chapter_page_number.inc}

  {$I includes/MangaHere/chapter_page_number.inc}

  {$I includes/EsMangaHere/chapter_page_number.inc}

  {$I includes/SubManga/chapter_page_number.inc}

  {$I includes/AnimExtremist/chapter_page_number.inc}

  {$I includes/MangaInn/chapter_page_number.inc}

  {$I includes/Batoto/chapter_page_number.inc}

  {$I includes/Hentai2Read/chapter_page_number.inc}

  {$I includes/MangaReader/chapter_page_number.inc}

  {$I includes/MangaPark/chapter_page_number.inc}

  {$I includes/MangaFox/chapter_page_number.inc}

  {$I includes/Starkana/chapter_page_number.inc}

  {$I includes/EatManga/chapter_page_number.inc}

  {$I includes/MangaPanda/chapter_page_number.inc}

  {$I includes/MangaGo/chapter_page_number.inc}

  {$I includes/RedHawkScans/chapter_page_number.inc}

  {$I includes/S2scans/chapter_page_number.inc}

  {$I includes/EGScans/chapter_page_number.inc}

  {$I includes/MangaTraders/chapter_page_number.inc}

  {$I includes/MangaStream/chapter_page_number.inc}

  {$I includes/Komikid/chapter_page_number.inc}

  {$I includes/PecintaKomik/chapter_page_number.inc}

  {$I includes/Pururin/chapter_page_number.inc}

  {$I includes/HugeManga/chapter_page_number.inc}

  {$I includes/AnimeStory/chapter_page_number.inc}

  {$I includes/Turkcraft/chapter_page_number.inc}

  {$I includes/MangaVadisi/chapter_page_number.inc}

  {$I includes/MangaFrame/chapter_page_number.inc}

  {$I includes/MangaAr/chapter_page_number.inc}

  {$I includes/MangaAe/chapter_page_number.inc}
     //mangacow page number
  {$I includes/Mangacow/chapter_page_number.inc}

  {$I includes/SenManga/chapter_page_number.inc}

  {$I includes/MangaEden/chapter_page_number.inc}

  {$I includes/Kivmanga/chapter_page_number.inc}

  function GetGEHentaiPageNumber(const lURL: String; const isGetLinkPage: Boolean): Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= gehGetPage(TObject(l),
                        URL,
                        manager.container.manager.retryConnect, lURL);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        if (isGetLinkPage) AND (Pos(' @ ', parse.Strings[i])>0) then
        begin
          s:= GetString(' '+parse.Strings[i], ' ', ' @ ');
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
        end;
        if Pos('background:transparent url', parse.Strings[i])>0 then
        begin
          manager.anotherURL:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href='));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
    Sleep(300);
  end;

var
  i: Cardinal;

begin
  manager.container.pageNumber:= 0;
  if manager.container.mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeAPageNumber
  else
  if manager.container.mangaSiteID = MANGAHERE_ID then
    Result:= GetMangaHerePageNumber
  else
  if manager.container.mangaSiteID = MANGAINN_ID then
    Result:= GetMangaInnPageNumber
  else
  if manager.container.mangaSiteID = BATOTO_ID then
    Result:= GetBatotoPageNumber
  else
  if manager.container.mangaSiteID = MANGAFOX_ID then
    Result:= GetMangaFoxPageNumber
  else
  if manager.container.mangaSiteID = MANGAREADER_ID then
    Result:= GetMangaReaderPageNumber
  else
  if manager.container.mangaSiteID = MANGATRADERS_ID then
    Result:= GetMangaTradersPageNumber
  else
  if manager.container.mangaSiteID = STARKANA_ID then
    Result:= GetStarkanaPageNumber
  else
  if manager.container.mangaSiteID = EATMANGA_ID then
    Result:= GetEatMangaPageNumber
  else
  if manager.container.mangaSiteID = MANGAPANDA_ID then
    Result:= GetMangaPandaPageNumber
  else
  if manager.container.mangaSiteID = MANGAGO_ID then
    Result:= GetMangaGoPageNumber
  else
  if manager.container.mangaSiteID = MANGASTREAM_ID then
    Result:= GetMangaStreamPageNumber
  else
  if manager.container.mangaSiteID = REDHAWKSCANS_ID then
    Result:= GetRedHawkScansPageNumber
  else
  if manager.container.mangaSiteID = S2SCAN_ID then
    Result:= GetS2scanPageNumber
  else
  if manager.container.mangaSiteID = ESMANGAHERE_ID then
    Result:= GetEsMangaHerePageNumber
  else
  if manager.container.mangaSiteID = SUBMANGA_ID then
    Result:= GetSubMangaPageNumber
  else
  if manager.container.mangaSiteID = ANIMEEXTREMIST_ID then
    Result:= GetAnimeExtremistPageNumber
  else
  if manager.container.mangaSiteID = KOMIKID_ID then
    Result:= GetKomikidPageNumber
  else
  if manager.container.mangaSiteID = PECINTAKOMIK_ID then
    Result:= GetPecintaKomikPageNumber
  else
  if manager.container.mangaSiteID = PURURIN_ID then
    Result:= GetPururinPageNumber
  else
  if manager.container.mangaSiteID = HUGEMANGA_ID then
    Result:= GetHugeMangaPageNumber
  else
  if manager.container.mangaSiteID = ANIMESTORY_ID then
    Result:= GetAnimeStoryPageNumber
  else
  if manager.container.mangaSiteID = TURKCRAFT_ID then
    Result:= GetTurkcraftPageNumber
  else
  if manager.container.mangaSiteID = MANGAVADISI_ID then
    Result:= GetMangaVadisiPageNumber
  else
  if manager.container.mangaSiteID = MANGAFRAME_ID then
    Result:= GetMangaFramePageNumber
  else
  if manager.container.mangaSiteID = MANGAAR_ID then
    Result:= GetMangaArPageNumber
  else
  if manager.container.mangaSiteID = MANGAAE_ID then
    Result:= GetMangaAePageNumber
  else
  if manager.container.mangaSiteID = MANGACOW_ID then
    Result:= GetMangaCowPageNumber
  else
  if manager.container.mangaSiteID = SENMANGA_ID then
    Result:= GetSenMangaPageNumber
  else
  if (manager.container.mangaSiteID = MANGAEDEN_ID) OR
     (manager.container.mangaSiteID = PERVEDEN_ID) then
    Result:= GetMangaEdenPageNumber
  else
  if manager.container.mangaSiteID = KIVMANGA_ID then
    Result:= GetKivmangaPageNumber
  else
  if manager.container.mangaSiteID = GEHENTAI_ID then
  begin
    Result:= GetGEHentaiPageNumber('', TRUE);
  end
  else
  if (manager.container.mangaSiteID = KISSMANGA_ID) OR
     (manager.container.mangaSiteID = BLOGTRUYEN_ID) OR
     (manager.container.mangaSiteID = MANGAPARK_ID) OR
     (manager.container.mangaSiteID = MANGA24H_ID) OR
     (manager.container.mangaSiteID = VNSHARING_ID) OR
     (manager.container.mangaSiteID = MABUNS_ID) OR
     (manager.container.mangaSiteID = EGSCANS_ID) OR
     (manager.container.mangaSiteID = PURURIN_ID) OR
     (manager.container.mangaSiteID = MANGAESTA_ID) OR
     (manager.container.mangaSiteID = TRUYEN18_ID) OR
     (manager.container.mangaSiteID = TRUYENTRANHTUAN_ID) OR
     (manager.container.mangaSiteID = SCANMANGA_ID) OR
     (manager.container.mangaSiteID = FAKKU_ID) OR
     (manager.container.mangaSiteID = MANGACAN_ID) OR
     (manager.container.mangaSiteID = CENTRALDEMANGAS_ID) then
  begin
    // all of image urls are in a html page
    Result:= TRUE;
    manager.container.pageNumber:= 1;
  end
  else
  if manager.container.mangaSiteID = HENTAI2READ_ID then
    Result:= GetHentai2ReadPageNumber;
end;

function    TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
var
  Parser  : TjsFastHTMLParser;

  {$I includes/AnimeA/image_url.inc}

  {$I includes/MangaHere/image_url.inc}

  {$I includes/EsMangaHere/image_url.inc}

  {$I includes/MangaInn/image_url.inc}

  {$I includes/KissManga/image_url.inc}

  {$I includes/Batoto/image_url.inc}

  {$I includes/Manga24h/image_url.inc}

  {$I includes/VnSharing/image_url.inc}

  {$I includes/Hentai2Read/image_url.inc}

  {$I includes/Fakku/image_url.inc}

  {$I includes/MangaReader/image_url.inc}

  {$I includes/MangaPark/image_url.inc}

  {$I includes/MangaFox/image_url.inc}

  {$I includes/Starkana/image_url.inc}

  {$I includes/EatManga/image_url.inc}

  {$I includes/SubManga/image_url.inc}

  {$I includes/AnimExtremist/image_url.inc}

  {$I includes/MangaPanda/image_url.inc}

  {$I includes/MangaGo/image_url.inc}

  {$I includes/RedHawkScans/image_url.inc}

  {$I includes/S2scans/image_url.inc}

  {$I includes/EGScans/image_url.inc}

  {$I includes/MangaStream/image_url.inc}

  {$I includes/TruyenTranhTuan/image_url.inc}

  {$I includes/BlogTruyen/image_url.inc}

  {$I includes/Komikid/image_url.inc}

  {$I includes/PecintaKomik/image_url.inc}

  {$I includes/Mabuns/image_url.inc}

  {$I includes/MangaEsta/image_url.inc}

  {$I includes/Pururin/image_url.inc}

  {$I includes/HugeManga/image_url.inc}

  {$I includes/AnimeStory/image_url.inc}

  {$I includes/ScanManga/image_url.inc}

  {$I includes/Turkcraft/image_url.inc}

  {$I includes/MangaVadisi/image_url.inc}

  {$I includes/MangaFrame/image_url.inc}

  {$I includes/MangaAe/image_url.inc}

  {$I includes/MangaAr/image_url.inc}

  {$I includes/CentralDeMangas/image_url.inc}

  // Mangacow link page
  {$I includes/Mangacow/image_url.inc}

  {$I includes/SenManga/image_url.inc}

  {$I includes/MangaTraders/image_url.inc}

  {$I includes/MangaEden/image_url.inc}

  {$I includes/Kivmanga/image_url.inc}

  {$I includes/Mangacan/image_url.inc}

  function GetGEHentaiImageURL: Boolean;
  var
    s1,s2,
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     URL,// + IntToStr(workCounter+1),
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('http://ehgt.org/g/n.png', parse.Strings[i])>0 then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href='));
          s1:= manager.anotherURL+' ';
          s2:= s+' ';
          Delete(s1, 1, 13);
          Delete(s2, 1, 13);
          // compare 2 strings to determine new URL
          if StrToInt(GetString(s1, '-', ' ')) < StrToInt(GetString(s2, '-', ' ')) then
          begin
            manager.anotherURL:= s;
          end;
        end;
        if (Pos('<div id="i3">', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workCounter]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
          manager.container.pageLinks.Strings[workCounter]:= StringReplace(manager.container.pageLinks.Strings[workCounter], '&amp;', '&', [rfReplaceAll]);
          // s:= manager.container.pageLinks.Strings[workCounter];
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

var
  s: String;

begin
  if manager.container.pageLinks.Strings[workCounter] <> 'W' then exit;
  if manager.container.mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeAImageURL
  else
  if manager.container.mangaSiteID = MANGATRADERS_ID then
    Result:= GetMangaTradersImageURL
  else
  if manager.container.mangaSiteID = MANGAHERE_ID then
    Result:= GetMangaHereImageURL
  else
  if manager.container.mangaSiteID = MANGAINN_ID then
    Result:= GetMangaInnImageURL
  else
  if manager.container.mangaSiteID = KISSMANGA_ID then
    Result:= GetKissMangaImageURL
  else
  if manager.container.mangaSiteID = BATOTO_ID then
    Result:= GetBatotoImageURL
  else
  if manager.container.mangaSiteID = MANGA24H_ID then
    Result:= GetManga24hImageURL
  else
  if manager.container.mangaSiteID = VNSHARING_ID then
    Result:= GetVnSharingImageURL
  else
  if manager.container.mangaSiteID = HENTAI2READ_ID then
    Result:= GetHentai2ReadImageURL
  else
  if manager.container.mangaSiteID = FAKKU_ID then
    Result:= GetFakkuImageURL
  else
  if manager.container.mangaSiteID = MANGAREADER_ID then
    Result:= GetMangaReaderImageURL
  else
  if manager.container.mangaSiteID = MANGAPARK_ID then
    Result:= GetMangaParkImageURL
  else
  if manager.container.mangaSiteID = MANGAFOX_ID then
    Result:= GetMangaFoxImageURL
  else
  if manager.container.mangaSiteID = STARKANA_ID then
    Result:= GetStarkanaImageURL
  else
  if manager.container.mangaSiteID = EATMANGA_ID then
    Result:= GetEatMangaImageURL
  else
  if manager.container.mangaSiteID = MANGAPANDA_ID then
    Result:= GetMangaPandaImageURL
  else
  if manager.container.mangaSiteID = MANGAGO_ID then
    Result:= GetMangaGoImageURL
  else
  if manager.container.mangaSiteID = MANGASTREAM_ID then
    Result:= GetMangaStreamImageURL
  else
  if manager.container.mangaSiteID = REDHAWKSCANS_ID then
    Result:= GetRedHawkScansImageURL
  else
  if manager.container.mangaSiteID = S2SCAN_ID then
    Result:= GetS2scanImageURL
  else
  if manager.container.mangaSiteID = EGSCANS_ID then
    Result:= GetEGScansImageURL
  else
  if manager.container.mangaSiteID = ESMANGAHERE_ID then
    Result:= GetEsMangaHereImageURL
  else
  if manager.container.mangaSiteID = SUBMANGA_ID then
    Result:= GetSubMangaImageURL
  else
  if manager.container.mangaSiteID = ANIMEEXTREMIST_ID then
    Result:= GetAnimeExtremistImageURL
  else
  if manager.container.mangaSiteID = KOMIKID_ID then
    Result:= GetKomikidImageURL
  else
  if manager.container.mangaSiteID = PECINTAKOMIK_ID then
    Result:= GetPecintaKomikImageURL
  else
  if manager.container.mangaSiteID = MABUNS_ID then
    Result:= GetMabunsImageURL
  else
  if manager.container.mangaSiteID = MANGAESTA_ID then
    Result:= GetMangaEstaImageURL
  else
  if manager.container.mangaSiteID = PURURIN_ID then
    Result:= GetPururinImageURL
  else
  if manager.container.mangaSiteID = HUGEMANGA_ID then
    Result:= GetHugeMangaImageURL
  else
  if manager.container.mangaSiteID = ANIMESTORY_ID then
    Result:= GetAnimeStoryImageURL
  else
  if manager.container.mangaSiteID = SCANMANGA_ID then
    Result:= GetScanMangaImageURL
  else
  if manager.container.mangaSiteID = TURKCRAFT_ID then
    Result:= GetTurkcraftImageURL
  else
  if manager.container.mangaSiteID = MANGAVADISI_ID then
    Result:= GetMangaVadisiImageURL
  else
  if manager.container.mangaSiteID = MANGAFRAME_ID then
    Result:= GetMangaFrameImageURL
  else
  if manager.container.mangaSiteID = MANGAAR_ID then
    Result:= GetMangaArImageURL
  else
  if manager.container.mangaSiteID = MANGAAE_ID then
    Result:= GetMangaAeImageURL
  else
  if manager.container.mangaSiteID = CENTRALDEMANGAS_ID then
    Result:= GetCentralDeMangasImageURL
  else
  if manager.container.mangaSiteID = MANGACOW_ID then
    Result:= GetMangaCowImageURL
  else
  if manager.container.mangaSiteID = SENMANGA_ID then
    Result:= GetSenMangaImageURL
  else
  if manager.container.mangaSiteID = TRUYENTRANHTUAN_ID then
    Result:= GetTruyenTranhTuanImageURL
  else
  if manager.container.mangaSiteID = BLOGTRUYEN_ID then
    Result:= GetBlogTruyenImageURL
  else
  if (manager.container.mangaSiteID = MANGAEDEN_ID) OR
     (manager.container.mangaSiteID = PERVEDEN_ID) then
    Result:= GetMangaEdenImageURL
  else
  if manager.container.mangaSiteID = KIVMANGA_ID then
    Result:= GetKivmangaImageURL
  else
  if manager.container.mangaSiteID = MANGACAN_ID then
    Result:= GetMangacanImageURL
  else
  if manager.container.mangaSiteID = GEHENTAI_ID then
    Result:= GetGEHentaiImageURL;
end;

procedure   TDownloadThread.SetChangeDirectoryFalse;
begin
  isChangeDirectory:= FALSE;
end;

procedure   TDownloadThread.SetChangeDirectoryTrue;
begin
  isChangeDirectory:= TRUE;
end;

function    TDownloadThread.DownloadPage: Boolean;
var
  fileSize: Cardinal;

  function  SavePage(URL: String; const Path, name: String; const Reconnect: Cardinal): Boolean;
  var
    header  : array [0..3] of Byte;
    ext     : String;
    HTTP    : THTTPSend;
    i       : Cardinal;
    counter : Cardinal = 0;
    s       : String;
    dest,
    source  : TPicture;
    fstream : TFileStreamUTF8;

  begin
    if (FileExists(Path+'/'+name+'.jpg')) OR
       (FileExists(Path+'/'+name+'.png')) OR
       (FileExists(Path+'/'+name+'.gif')) OR
       (Pos('http', URL) = 0) then
    begin
      Result:= TRUE;
      exit;
    end;
    Result:= FALSE;
    HTTP:= THTTPSend.Create;
    HTTP.ProxyHost:= Host;
    HTTP.ProxyPort:= Port;
    HTTP.ProxyUser:= User;
    HTTP.ProxyPass:= Pass;

    if manager.container.mangaSiteID <> MANGAAR_ID then
      HTTP.UserAgent:='curl/7.21.0 (i686-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18';

    if manager.container.mangaSiteID = HENTAI2READ_ID then
      HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[HENTAI2READ_ID,1]+'/')
    else
    if manager.container.mangaSiteID = MANGAGO_ID then
      HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[MANGAGO_ID,1]+'/')
    else
    if manager.container.mangaSiteID = ANIMEEXTREMIST_ID then
    begin
      HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[ANIMEEXTREMIST_ID,1]+'/');
    end
    else
    if manager.container.mangaSiteID = KISSMANGA_ID then
      HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[KISSMANGA_ID,1]+'/')
    else
    if manager.container.mangaSiteID = CENTRALDEMANGAS_ID then
      HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[CENTRALDEMANGAS_ID,1]+'/')
    else
    if manager.container.mangaSiteID = VNSHARING_ID then
      HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[VNSHARING_ID,1]+'/')
    else
    if  manager.container.mangaSiteID = GEHENTAI_ID then
      HTTP.Headers.Insert(0, 'Referer:'+manager.container.pageLinks.Strings[workCounter]);
    while (NOT HTTP.HTTPMethod('GET', URL)) OR
          (HTTP.ResultCode >= 500) OR
          (HTTP.ResultCode = 403) do
    begin
      if Reconnect <> 0 then
      begin
        if Reconnect <= counter then
        begin
          HTTP.Free;
          exit;
        end;
        Inc(counter);
      end;
      HTTP.Clear;
      Sleep(500);
    end;

    while (HTTP.ResultCode = 302) OR (HTTP.ResultCode = 301) do
    begin
      URL:= CheckRedirect(HTTP);
      HTTP.Clear;
      HTTP.RangeStart:= 0;
      if Pos(HENTAI2READ_ROOT, URL) <> 0 then
        HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/')
      else
      if Pos('bp.blogspot.com', URL) <> 0 then
        HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[KISSMANGA_ID,1]+'/')
      else
      if Pos('mangas.centraldemangas.com', URL) <> 0 then
        HTTP.Headers.Insert(0, 'Referer:'+WebsiteRoots[CENTRALDEMANGAS_ID,1]+'/');
      while (NOT HTTP.HTTPMethod('GET', URL)) OR
            (HTTP.ResultCode >= 500) do
      begin
        if Reconnect <> 0 then
        begin
          if Reconnect <= counter then
          begin
            HTTP.Free;
            exit;
          end;
          Inc(counter);
        end;
        HTTP.Clear;
        Sleep(500);
      end;
    end;
    HTTP.Document.Seek(0, soBeginning);
    HTTP.Document.Read(header[0], 4);
    if (header[0] = JPG_HEADER[0]) AND
       (header[1] = JPG_HEADER[1]) AND
       (header[2] = JPG_HEADER[2]) then
      ext:= '.jpg'
    else
    if (header[0] = PNG_HEADER[0]) AND
       (header[1] = PNG_HEADER[1]) AND
       (header[2] = PNG_HEADER[2]) then
      ext:= '.png'
    else
    if (header[0] = GIF_HEADER[0]) AND
       (header[1] = GIF_HEADER[1]) AND
       (header[2] = GIF_HEADER[2]) then
      ext:= '.gif'
    else
      ext:= '';
    fstream:= TFileStreamUTF8.Create(Path+'/'+name+ext, fmCreate);
    HTTP.Document.SaveToStream(fstream);
    fstream.Free;
  //    HTTP.Document.SaveToFile(Path+'/'+name+ext);
    HTTP.Free;
    Result:= TRUE;
  end;

var
  lastTime, curTime  : Cardinal;
  s: String;
label
  start;

begin
start:
  if manager.container.mangaSiteID = GEHENTAI_ID then
  begin
    Sleep(500);
    lastTime:= fmdGetTickCount;

   // anotherURLBackup:= manager.anotherURL;
    GetLinkPageFromURL(anotherURL);
    curTime:= fmdGetTickCount-lastTime;
    if curTime<3000 then
      Sleep(3000-curTime)
    else
      Sleep(300);
  end;

  if (manager.container.pageLinks.Strings[workCounter] = '') OR
     (manager.container.pageLinks.Strings[workCounter] = 'W') then exit;
  SavePage(manager.container.pageLinks.Strings[workCounter],
           manager.container.downloadInfo.SaveTo+
           '/'+manager.container.chapterName.Strings[manager.container.currentDownloadChapterPtr],
           Format('%.3d', [workCounter+1]),
           manager.container.manager.retryConnect);

  SetCurrentDirUTF8(fmdDirectory);
  if NOT Terminated then
    manager.container.pageLinks.Strings[workCounter]:= '';
end;

// ----- TTaskThread -----

constructor TTaskThread.Create;
begin
  anotherURL  := '';
  isTerminated:= FALSE;
  isSuspended := TRUE;
  FreeOnTerminate:= TRUE;
  threads     := TDownloadThreadList.Create;
  inherited Create(FALSE);
end;

destructor  TTaskThread.Destroy;
begin
  Stop;
  threads.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TTaskThread.MainThreadRepaint;
begin
  if MainForm.isCanRefreshForm then
  begin
    MainForm.vtDownload.Repaint;
    MainForm.isCanRefreshForm:= FALSE;
  end;
end;

procedure   TTaskThread.MainThreadCompressRepaint;
begin
  container.downloadInfo.Status:= Format('%s (%d/%d)', [stIsCompressing, container.currentDownloadChapterPtr, container.chapterLinks.Count]);
  MainForm.vtDownload.Repaint;
end;

procedure   TTaskThread.MainThreadRepaintImm;
begin
  MainForm.vtDownload.Repaint;
  MainForm.isCanRefreshForm:= FALSE;
end;

procedure   TTaskThread.Compress;
var
  uPacker: TPacker;
begin
  if (container.manager.compress >= 1) then
  begin
    Sleep(100);
    Synchronize(MainThreadCompressRepaint);
    uPacker:= TPacker.Create;
    case container.manager.compress of
      1: uPacker.ext:= '.zip';
      2: uPacker.ext:= '.cbz';
      3: uPacker.ext:= '.pdf';
    end;
    uPacker.CompressionQuality:= OptionPDFQuality;
    uPacker.Path:= container.downloadInfo.SaveTo+'/'+
                  container.chapterName.Strings[container.currentDownloadChapterPtr];
    uPacker.Execute;
    uPacker.Free;
  end;
end;

procedure   TTaskThread.ShowBaloon;
begin
  MainForm.TrayIcon.BalloonHint:= '"'+container.downloadInfo.title+'" - '+stFinish;
  MainForm.TrayIcon.ShowBalloonHint;
end;

procedure   TTaskThread.Checkout;
var
  i, currentMaxThread: Cardinal;
  s: String;
begin
  // ugly code, need to be fixed later

  if (container.mangaSiteID = GEHENTAI_ID) AND (container.manager.maxDLThreadsPerTask>4) then
    currentMaxThread:= 4
  else
  if (container.mangaSiteID = EATMANGA_ID) then
    currentMaxThread:= 1
  else
    currentMaxThread:= container.manager.maxDLThreadsPerTask;

  if container.mangaSiteID = GEHENTAI_ID then
  begin
    if (container.workCounter <> 0) then
    begin
      repeat
        Sleep(32);
        s:= anotherURL;
        Delete(s, 1, 13);
        if (s <> '') AND
           (StrToInt(GetString(s+' ', '-', ' ')) = (container.workCounter+1)) then
          break;
      until FALSE;
    end;
    Sleep(500*currentMaxThread);
  end
  else
    Sleep(100);

  // main body of method
  // Each thread will be assigned job based on the counter
  if container.activeThreadCount > currentMaxThread then exit;
  for i:= 0 to currentMaxThread-1 do
  begin
    if i >= threads.Count then
    begin
      while isSuspended do Sleep(100);
      Inc(container.activeThreadCount);
      threads.Add(TDownloadThread.Create);
      if container.mangaSiteID = GEHENTAI_ID then
        threads.Items[threads.Count-1].anotherURL:= anotherURL;
      threads.Items[threads.Count-1].manager:= self;
      threads.Items[threads.Count-1].workCounter:= container.workCounter;
      threads.Items[threads.Count-1].checkStyle:= Flag;
      threads.Items[threads.Count-1].isSuspended:= FALSE;
      Inc(container.workCounter);
      if Flag = CS_GETPAGELINK then
        Inc(container.currentPageNumber);
      exit;
    end
    else
    if (threads.Items[i].isTerminated) then
    begin
      while isSuspended do Sleep(100);
      Inc(container.activeThreadCount);
      threads.Items[i]:= TDownloadThread.Create;
      if container.mangaSiteID = GEHENTAI_ID then
        threads.Items[i].anotherURL:= anotherURL;
      threads.Items[i].manager:= self;
      threads.Items[i].workCounter:= container.workCounter;
      threads.Items[i].checkStyle:= Flag;
      threads.Items[i].isSuspended:= FALSE;
      Inc(container.workCounter);
      if Flag = CS_GETPAGELINK then
        Inc(container.currentPageNumber);
      exit;
    end;
  end;
end;

procedure   TTaskThread.Execute;

  procedure  WaitFor;
  var
    done: Boolean;
    i   : Cardinal;
  begin
    repeat
      done:= TRUE;
      for i:= 0 to threads.Count-1 do
       // if threads[i].manager = @self then
          if NOT threads[i].isTerminated then
          begin
            done:= FALSE;
            sleep(100);
          end;
    until done;
  end;

var
  i, count: Cardinal;
begin
  while isSuspended do Sleep(100);

  while container.currentDownloadChapterPtr < container.chapterLinks.Count do
  begin
    if Terminated then exit;
    container.activeThreadCount:= 1;
    while isSuspended do Sleep(100);

    // get page number
    if container.currentPageNumber = 0 then
    begin
      if Terminated then exit;
      Stop(FALSE);
      threads.Add(TDownloadThread.Create);
      i:= threads.Count-1;
       // container.Status:= STATUS_PREPARE;
      threads.Items[threads.Count-1].manager:= self;
      threads.Items[threads.Count-1].workCounter:= container.workCounter;
      threads.Items[threads.Count-1].checkStyle:= CS_GETPAGENUMBER;
      threads.Items[threads.Count-1].isSuspended:= FALSE;
      CheckPath(container.downloadInfo.SaveTo+
                '/'+
                container.chapterName.Strings[container.currentDownloadChapterPtr]);
      while (isSuspended) OR (NOT threads.Items[threads.Count-1].isTerminated) do
        Sleep(100);
    end;

    //get page links
    if (container.mangaSiteID <> GEHENTAI_ID) then
    begin
      container.workCounter:= 0;
      container.downloadInfo.iProgress:= 0;
      while container.workCounter < container.pageLinks.Count do
      begin
        if Terminated then exit;
        Flag:= CS_GETPAGELINK;
        Checkout;
        container.downloadInfo.Progress:= Format('%d/%d', [container.workCounter, container.pageNumber]);
        container.downloadInfo.Status  :=
          Format('%s (%d/%d [%s])',
            [stPreparing,
             container.currentDownloadChapterPtr,
             container.chapterLinks.Count,
             container.chapterName.Strings[container.currentDownloadChapterPtr]]);
        Inc(container.downloadInfo.iProgress);
        {$IFDEF WIN32}
        MainForm.vtDownload.Repaint;
        {$ELSE}
        Synchronize(MainThreadRepaint);
        {$ENDIF}
      end;
      WaitFor;
    end;

    //download pages
    container.workCounter:= 0;
    container.downloadInfo.iProgress:= 0;

    // If container doesn't have any image, we will skip the loop. Otherwise
    // download them
    if (container.pageLinks.Count > 0) then
    begin
      while container.workCounter < container.pageLinks.Count do
      begin
        if Terminated then exit;
        Flag:= CS_DOWNLOAD;
        Checkout;
        container.downloadInfo.Progress:= Format('%d/%d', [container.workCounter, container.pageLinks.Count]);
        container.downloadInfo.Status  :=
          Format('%s (%d/%d [%s])',
            [stDownloading,
             container.currentDownloadChapterPtr,
             container.chapterLinks.Count,
             container.chapterName.Strings[container.currentDownloadChapterPtr]]);
        Inc(container.downloadInfo.iProgress);
        {$IFDEF WIN32}
        MainForm.vtDownload.Repaint;
        {$ELSE}
        Synchronize(MainThreadRepaint);
        {$ENDIF}
      end;
      WaitFor;
     // Synchronize(Compress);
      Compress;
    end;

    if Terminated then exit;
    container.currentPageNumber:= 0;
    container.pageLinks.Clear;
    Inc(container.currentDownloadChapterPtr);
  end;
  Synchronize(ShowBaloon);
  Terminate;
end;

procedure   TTaskThread.Stop(const check: Boolean = TRUE);
var
  i: Cardinal;
begin
  if check then
  begin
    if (container.workCounter >= container.pageLinks.Count) AND
       (container.currentDownloadChapterPtr >= container.chapterLinks.Count) then
    begin
      container.downloadInfo.Status  := stFinish;
      container.downloadInfo.Progress:= '';
      container.Status:= STATUS_FINISH;
      container.manager.CheckAndActiveTask(TRUE);
      {$IFDEF WIN32}
      MainForm.vtDownload.Repaint;
      {$ELSE}
      Synchronize(MainThreadRepaintImm);
      {$ENDIF}
    end
    else
    begin
      container.downloadInfo.Status  := Format('%s (%d/%d)', [stStop, container.currentDownloadChapterPtr, container.chapterLinks.Count]);
      container.Status:= STATUS_STOP;
      container.manager.CheckAndActiveTask;
      {$IFDEF WIN32}
      MainForm.vtDownload.Repaint;
      {$ELSE}
      Synchronize(MainThreadRepaintImm);
      {$ENDIF}
    end;
  end;
  threads.Clear;
end;

// ----- TTaskThreadContainer -----

constructor TTaskThreadContainer.Create;
begin
  chapterLinks     := TStringList.Create;
  chapterName      := TStringList.Create;
  pageLinks        := TStringList.Create;
  pageContainerLinks:= TStringList.Create;
  workCounter:= 0;
  currentPageNumber:= 0;
  currentDownloadChapterPtr:= 0;
  inherited Create;
end;

destructor  TTaskThreadContainer.Destroy;
begin
  // TODO: Need recheck
  repeat
    Sleep(64);
  until activeThreadCount = 0;
  thread.Terminate;
  pageContainerLinks.Free;
  pageLinks.Free;
  chapterName.Free;
  chapterLinks.Free;
  inherited Destroy;
end;

// ----- TDownloadManager -----

constructor TDownloadManager.Create;
begin
  inherited Create;

  // Create INI file
  ini:= TIniFile.Create(WORK_FOLDER + WORK_FILE);
  ini.CacheUpdates:= TRUE;

  downloadedChaptersList:= TStringList.Create;
  if FileExists(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE) then
    downloadedChaptersList.LoadFromFile(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE);

  containers:= TTaskThreadContainerList.Create;
  isFinishTaskAccessed:= FALSE;
  isRunningBackup     := FALSE;
  isRunningBackupDownloadedChaptersList:= FALSE;

  DownloadedChapterList := TList.Create;

  // Restore old INI file
  Restore;
end;

destructor  TDownloadManager.Destroy;
var i: Cardinal;
begin
  if containers.Count <> 0 then
    for i:= 0 to containers.Count-1 do
      if NOT containers.Items[i].thread.isTerminated then
        containers.Items[i].thread.Terminate;
  ini.Free;

  BackupDownloadedChaptersList;
  downloadedChaptersList.Free;

  DownloadedChapterList.Free;

  inherited Destroy;
end;

procedure   TDownloadManager.BackupDownloadedChaptersList;
begin
  if isRunningBackupDownloadedChaptersList then
    exit;
  isRunningBackupDownloadedChaptersList:= TRUE;
  downloadedChaptersList.SaveToFile(WORK_FOLDER + DOWNLOADEDCHAPTERS_FILE);
  isRunningBackupDownloadedChaptersList:= FALSE;
end;

procedure   TDownloadManager.Restore;
var
  s: String;
  tmp,
  i: Cardinal;
begin
  // Restore general information first
  if containers.Count > 0 then
  begin
    for i:= 0 to containers.Count-1 do
    begin
      containers.Items[i].Destroy;
    end;
    containers.Clear;
  end;
  tmp:= ini.ReadInteger('general', 'NumberOfTasks', 0);
  if tmp = 0 then exit;
  for i:= 0 to tmp-1 do
  begin
    containers.Add(TTaskThreadContainer.Create);
    containers.Items[i].manager:= self;
  end;

  // Restore chapter links, chapter name and page links
  for i:= 0 to containers.Count-1 do
  begin
    s:= ini.ReadString('task'+IntToStr(i), 'ChapterLinks', '');
    if s <> '' then
      GetParams(containers.Items[i].chapterLinks, s);
    s:= ini.ReadString('task'+IntToStr(i), 'ChapterName', '');
    if s <> '' then
      GetParams(containers.Items[i].chapterName, s);
    s:= ini.ReadString('task'+IntToStr(i), 'PageLinks', '');
    if s <> '' then
      GetParams(containers.Items[i].pageLinks, s);
    s:= ini.ReadString('task'+IntToStr(i), 'PageContainerLinks', '');
    if s <> '' then
      GetParams(containers.Items[i].pageContainerLinks, s);
    containers.Items[i].Status                   := ini.ReadInteger('task'+IntToStr(i), 'TaskStatus', 0);
    containers.Items[i].currentDownloadChapterPtr:= ini.ReadInteger('task'+IntToStr(i), 'ChapterPtr', 0);
    containers.Items[i].pageNumber               := ini.ReadInteger('task'+IntToStr(i), 'NumberOfPages', 0);
    containers.Items[i].currentPageNumber        := ini.ReadInteger('task'+IntToStr(i), 'CurrentPage', 0);

    containers.Items[i].downloadInfo.title   := ini.ReadString('task'+IntToStr(i), 'Title', 'NULL');
    containers.Items[i].downloadInfo.status  := ini.ReadString('task'+IntToStr(i), 'Status', 'NULL');
    containers.Items[i].downloadInfo.Progress:= ini.ReadString('task'+IntToStr(i), 'Progress', 'NULL');
    containers.Items[i].downloadInfo.website := ini.ReadString('task'+IntToStr(i), 'Website', 'NULL');
    containers.Items[i].downloadInfo.saveTo  := ini.ReadString('task'+IntToStr(i), 'SaveTo', 'NULL');
    containers.Items[i].downloadInfo.dateTime:= ini.ReadString('task'+IntToStr(i), 'DateTime', 'NULL');
    containers.Items[i].mangaSiteID:= GetMangaSiteID(containers.Items[i].downloadInfo.website);
  end;
  i:= 0;
  while i < containers.Count do
  begin
    if CompareStr(containers.Items[i].downloadInfo.dateTime, 'NULL') = 0 then
      containers.Delete(i)
    else
      Inc(i);
  end;
end;

procedure   TDownloadManager.Backup;
var
  i: Cardinal;
begin
  if isRunningBackup then exit;
  isRunningBackup:= TRUE;
  // Erase all sections
  for i:= 0 to ini.ReadInteger('general', 'NumberOfTasks', 0) do
    ini.EraseSection('task'+IntToStr(i));
  ini.EraseSection('general');

  // backup
  if containers.Count > 0 then
  begin
    ini.WriteInteger('general', 'NumberOfTasks', containers.Count);

    for i:= 0 to containers.Count-1 do
    begin
     // ini.WriteInteger('task'+IntToStr(i), 'NumberOfChapterLinks', containers.Items[i].chapterLinks.Count);
     // ini.WriteInteger('task'+IntToStr(i), 'NumberOfChapterName', containers.Items[i].chapterName.Count);
     // ini.WriteInteger('task'+IntToStr(i), 'NumberOfPageLinks', containers.Items[i].pageLinks.Count);

      ini.WriteString('task'+IntToStr(i), 'ChapterLinks', SetParams(containers.Items[i].chapterLinks));
      ini.WriteString('task'+IntToStr(i), 'ChapterName', SetParams(containers.Items[i].ChapterName));
      if containers.Items[i].pageLinks.Count > 0 then
        ini.WriteString('task'+IntToStr(i), 'PageLinks', SetParams(containers.Items[i].pageLinks));
      if containers.Items[i].pageContainerLinks.Count > 0 then
        ini.WriteString('task'+IntToStr(i), 'PageContainerLinks', SetParams(containers.Items[i].pageContainerLinks));

      ini.WriteInteger('task'+IntToStr(i), 'TaskStatus', containers.Items[i].Status);
      ini.WriteInteger('task'+IntToStr(i), 'ChapterPtr', containers.Items[i].currentDownloadChapterPtr);
      ini.WriteInteger('task'+IntToStr(i), 'NumberOfPages', containers.Items[i].pageNumber);
      ini.WriteInteger('task'+IntToStr(i), 'CurrentPage', containers.Items[i].currentPageNumber);

      ini.WriteString ('task'+IntToStr(i), 'Title', containers.Items[i].downloadInfo.title);
      ini.WriteString ('task'+IntToStr(i), 'Status', containers.Items[i].downloadInfo.status);
      ini.WriteString ('task'+IntToStr(i), 'Progress', containers.Items[i].downloadInfo.Progress);
      ini.WriteString ('task'+IntToStr(i), 'Website', containers.Items[i].downloadInfo.website);
      ini.WriteString ('task'+IntToStr(i), 'SaveTo', containers.Items[i].downloadInfo.saveTo);
      ini.WriteString ('task'+IntToStr(i), 'DateTime', containers.Items[i].downloadInfo.dateTime);
    end;
  end;
 // ini.UpdateFile;
  isRunningBackup:= FALSE;
end;

procedure   TDownloadManager.SaveJobList;
begin
  if isRunningBackup then while TRUE do Sleep(32);
  isRunningBackup:= TRUE;
  ini.UpdateFile;
  isRunningBackup:= FALSE;
end;

procedure   TDownloadManager.AddToDownloadedChaptersList(const ALink: String);
var
  i: Cardinal;
  LValue: String;
  Node  : PVirtualNode;
begin
  // generate LValue string
  LValue:= '';
  if frmMain.MainForm.clbChapterList.RootNodeCount = 0 then exit;
  Node:= frmMain.MainForm.clbChapterList.GetFirst;
  for i:= 0 to frmMain.MainForm.clbChapterList.RootNodeCount-1 do
  begin
    if Node.CheckState = csCheckedNormal then
      LValue:= LValue+IntToStr(i) + SEPERATOR;
    Node:= frmMain.MainForm.clbChapterList.GetNext(Node);
  end;
  if LValue = '' then exit;

  if DownloadedChaptersList.Count > 0 then
  begin
    i:= 0;
    while i < DownloadedChaptersList.Count do
    begin
      if CompareStr(ALink, DownloadedChaptersList.Strings[i]) = 0 then
      begin
        DownloadedChaptersList.Strings[i  ]:= ALink;
        DownloadedChaptersList.Strings[i+1]:=
          RemoveDuplicateNumbersInString(DownloadedChaptersList.Strings[i+1] + LValue);
        exit;
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

procedure   TDownloadManager.AddToDownloadedChaptersList(const ALink, AValue: String);
var
  i: Cardinal;
begin
  if DownloadedChaptersList.Count > 0 then
  begin
    i:= 0;
    while i < DownloadedChaptersList.Count do
    begin
      if CompareStr(ALink, DownloadedChaptersList.Strings[i]) = 0 then
      begin
        DownloadedChaptersList.Strings[i  ]:= ALink;
        DownloadedChaptersList.Strings[i+1]:=
          RemoveDuplicateNumbersInString(DownloadedChaptersList.Strings[i+1] + AValue);
        exit;
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

procedure   TDownloadManager.ReturnDownloadedChapters(const ALink: String);
var
  i: Cardinal;
begin
  // clear the list
  DownloadedChapterList.Clear;

  if DownloadedChaptersList.Count > 0 then
  begin
    i:= 0;
    while i < DownloadedChaptersList.Count do
    begin
      if CompareStr(ALink, DownloadedChaptersList.Strings[i]) = 0 then
      begin
        GetParams(DownloadedChapterList, DownloadedChaptersList.Strings[i+1]);
        exit;
      end;
      Inc(i, 2);
    end;
  end;
end;

procedure   TDownloadManager.AddTask;
begin
  containers.Add(TTaskThreadContainer.Create);
  containers.Items[containers.Count-1].manager:= self;
end;

procedure   TDownloadManager.CheckAndActiveTask(const isCheckForFMDDo: Boolean = FALSE);
var
  eatMangaCount: Cardinal = 0;
  batotoCount: Cardinal = 0;
  geCount    : Cardinal = 0;
  otherCount : Cardinal = 0;
  i          : Cardinal;
  count      : Cardinal = 0;
begin
  if containers.Count = 0 then exit;
  for i:= 0 to containers.Count-1 do
  begin
    if (containers.Items[i].Status = STATUS_DOWNLOAD) then
    begin
      if (containers.Items[i].mangaSiteID = GEHENTAI_ID) then
        Inc(geCount)
      else
      if (containers.Items[i].mangaSiteID = BATOTO_ID) then
        Inc(batotoCount)
      else
      if (containers.Items[i].mangaSiteID = EATMANGA_ID) then
        Inc(eatMangaCount);
      Inc(otherCount);
    end;
  end;

  if otherCount >= maxDLTasks then
    exit;

  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
    begin
      Inc(count);
    end
    else
    if containers.Items[i].Status = STATUS_WAIT then
    begin
      if containers.Items[i].mangaSiteID = GEHENTAI_ID then
      begin
        if geCount = 0 then
        begin
          ActiveTask(i);
          Inc(geCount);
          Inc(count);
        end;
      end
      else
      if containers.Items[i].mangaSiteID = EATMANGA_ID then
      begin
        if eatMangaCount = 0 then
        begin
          ActiveTask(i);
          Inc(eatMangaCount);
          Inc(count);
        end;
      end
      else
      begin
        ActiveTask(i);
        Inc(count);
      end;
    end;
    if count >= maxDLTasks then
      exit;
  end;

  if (count = 0) AND (isCheckForFMDDo) then
  begin
    case MainForm.cbOptionLetFMDDo.ItemIndex of
      DO_EXIT_FMD:
        begin
          MainForm.CloseNow;
          Sleep(2000);
          Halt;
        end;
      DO_TURNOFF:
        begin
          MainForm.CloseNow;
          Sleep(3000);
          fmdPowerOff;
          Halt;
        end;
      DO_HIBERNATE:
        begin
          Sleep(3000);
          fmdHibernate;
          Sleep(1000);
        end;
    end;
  end;
  MainForm.vtDownloadFilters;
end;

function    TDownloadManager.CanActiveTask(const pos: Cardinal): Boolean;
var
  eatMangaCount: Cardinal = 0;
  batotoCount: Cardinal = 0;
  geCount    : Cardinal = 0;
  i    : Cardinal;
  count: Cardinal = 0;
begin
  Result:= FALSE;

  if containers.Count = 0 then exit;
  if pos >= containers.Count then exit;

  for i:= 0 to containers.Count-1 do
  begin
    if (containers.Items[i].Status = STATUS_DOWNLOAD) AND (i<>pos) then
    begin
      if (containers.Items[i].mangaSiteID = GEHENTAI_ID) then
        Inc(geCount)
      else
      if (containers.Items[i].mangaSiteID = EATMANGA_ID) then
        Inc(eatMangaCount);
    end;
  end;

  if (containers.Items[pos].mangaSiteID = GEHENTAI_ID) AND (geCount > 0) then
    exit
  else
  if (containers.Items[pos].mangaSiteID = EATMANGA_ID) AND (eatMangaCount > 0) then
    exit;

  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
      Inc(count);
    if count >= maxDLTasks then
      exit;
  end;
  Result:= TRUE;
end;

procedure   TDownloadManager.CheckAndActiveTaskAtStartup;

  procedure   ActiveTaskAtStartup(const taskID: Cardinal);
  var
    i, pos: Cardinal;
  begin
    i:= maxDLTasks;
    if taskID >= containers.Count then exit;
    if (NOT Assigned(containers.Items[taskID])) then exit;
    if (containers.Items[taskID].Status = STATUS_WAIT) AND
       (containers.Items[taskID].Status = STATUS_STOP) AND
       (containers.Items[taskID].Status = STATUS_FINISH) then exit;
    containers.Items[taskID].Status:= STATUS_DOWNLOAD;
    containers.Items[taskID].thread:= TTaskThread.Create;
    containers.Items[taskID].thread.container:= containers.Items[taskID];
    containers.Items[taskID].thread.isSuspended:= FALSE;
  end;

var
  i    : Cardinal;
  count: Cardinal = 0;
begin
  if containers.Count = 0 then exit;
  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
    begin
      ActiveTaskAtStartup(i);
      Inc(count);
    end;
  end;
  MainForm.vtDownloadFilters;
end;

procedure   TDownloadManager.ActiveTask(const taskID: Cardinal);
var
  i, pos: Cardinal;
begin
  i:= maxDLTasks;
  // conditions
 // if pos >= maxDLTasks then exit;
  if taskID >= containers.Count then exit;
  if (NOT Assigned(containers.Items[taskID])) then exit;
  if (containers.Items[taskID].Status = STATUS_DOWNLOAD) AND
     (containers.Items[taskID].Status = STATUS_PREPARE) AND
     (containers.Items[taskID].Status = STATUS_FINISH) then exit;
  containers.Items[taskID].Status:= STATUS_DOWNLOAD;
  containers.Items[taskID].thread:= TTaskThread.Create;
  containers.Items[taskID].thread.container:= containers.Items[taskID];
  containers.Items[taskID].thread.isSuspended:= FALSE;
  // TODO
  MainForm.vtDownloadFilters;
end;

procedure   TDownloadManager.StopTask(const taskID: Cardinal; const isCheckForActive: Boolean = TRUE);
var
  i: Cardinal;
begin
  // conditions
  if taskID >= containers.Count then exit;
  if (containers.Items[taskID].Status <> STATUS_DOWNLOAD) AND
     (containers.Items[taskID].Status <> STATUS_WAIT) then exit;
  // check and stop any active thread
  if containers.Items[taskID].Status = STATUS_DOWNLOAD then
  begin
    for i:= 0 to containers.Items[taskID].thread.threads.Count-1 do
      if Assigned(containers.Items[taskID].thread.threads[i]) then
        containers.Items[taskID].thread.threads[i].Terminate;
    containers.Items[taskID].thread.Terminate;
    Sleep(250);
  end;
 // containers.Items[taskID].downloadInfo.Status:= Format('%s (%d/%d)', [stStop, containers.Items[taskID].currentDownloadChapterPtr, containers.Items[taskID].chapterLinks.Count]);
  containers.Items[taskID].downloadInfo.Status:= stStop;
  containers.Items[taskID].Status:= STATUS_STOP;

  if isCheckForActive then
  begin
    Backup;
    Sleep(1000);
    CheckAndActiveTask;
  end;
  MainForm.vtDownloadFilters;
end;

procedure   TDownloadManager.StopAllTasks;
var
  i, j: Cardinal;
begin
  if containers.Count = 0 then exit;
  // check and stop any active thread
  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
    begin
      for j:= 0 to containers.Items[i].thread.threads.Count-1 do
        if Assigned(containers.Items[i].thread.threads[j]) then
          containers.Items[i].thread.threads[j].Terminate;
      containers.Items[i].thread.Terminate;
      Sleep(250);
      containers.Items[i].Status:= STATUS_STOP;
    end
    else
    if containers.Items[i].Status = STATUS_WAIT then
    begin
      containers.Items[i].downloadInfo.Status:= stStop;
      containers.Items[i].Status:= STATUS_STOP;
    end;
  end;
  Backup;
  MainForm.vtDownload.Repaint;
  MainForm.vtDownloadFilters;
end;

procedure   TDownloadManager.StopAllDownloadTasksForExit;
var
  i, j: Cardinal;
begin
  if containers.Count = 0 then exit;
  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
    begin
      for j:= 0 to containers.Items[i].thread.threads.Count-1 do
        if Assigned(containers.Items[i].thread.threads[j]) then
          containers.Items[i].thread.threads[j].Terminate;
      containers.Items[i].thread.Terminate;
    end;
  end;
  Backup;
  MainForm.vtDownload.Repaint;
end;

procedure   TDownloadManager.FinishTask(const taskID: Cardinal);
begin
end;

// swap 2 task
function    TDownloadManager.Swap(const id1, id2: Cardinal): Boolean;
var
  tmp: TTaskThreadContainer;
begin
  if (id1 >= containers.Count) OR (id2 >= containers.Count) then exit(FALSE);
  tmp:= containers.Items[id1];
  containers.Items[id1]:= containers.Items[id2];
  containers.Items[id2]:= tmp;
  Result:= TRUE;
end;

// move a task down
function    TDownloadManager.MoveDown(const taskID: Cardinal): Boolean;
var
  tmp: TTaskThreadContainer;
begin
  if (taskID >= 0) AND (taskID < containers.Count-1) then
  begin
    tmp:= containers.Items[taskID];
    containers.Items[taskID]:= containers.Items[taskID+1];
    containers.Items[taskID+1]:= tmp;
    Result:= TRUE;
  end
  else
    Result:= FALSE;
  MainForm.vtDownloadFilters;
end;

// move a task up
function    TDownloadManager.MoveUp(const taskID: Cardinal): Boolean;
var
  tmp: TTaskThreadContainer;
begin
  if (taskID > 0) AND (taskID <= containers.Count-1) then
  begin
    tmp:= containers.Items[taskID];
    containers.Items[taskID]:= containers.Items[taskID-1];
    containers.Items[taskID-1]:= tmp;
    Result:= TRUE;
  end
  else
    Result:= FALSE;
  MainForm.vtDownloadFilters;
end;

procedure   TDownloadManager.RemoveTask(const taskID: Cardinal);
var
  i, j: Cardinal;
begin
  if taskID >= containers.Count then exit;
  // check and stop any active thread
  if containers.Items[taskID].Status = STATUS_DOWNLOAD then
  begin
    for i:= 0 to containers.Items[taskID].thread.threads.Count-1 do
      if Assigned(containers.Items[taskID].thread.threads[i]) then
        containers.Items[taskID].thread.threads[i].Terminate;
    containers.Items[taskID].thread.Terminate;
    Sleep(250);
    containers.Items[taskID].Status:= STATUS_STOP;
  end
  else
  if containers.Items[taskID].Status = STATUS_WAIT then
  begin
    containers.Items[taskID].downloadInfo.Status:= stStop;
    containers.Items[taskID].Status:= STATUS_STOP;
  end;
  containers.Delete(taskID);
end;

procedure   TDownloadManager.RemoveAllFinishedTasks;
var
  i, j: Cardinal;
begin
  if containers.Count = 0 then exit;
  // remove
  i:= 0;
  repeat
    if containers.Items[i].Status = STATUS_FINISH then
    begin
      containers.Delete(i);
    end
    else
      Inc(i);
  until i >= containers.Count;
end;

procedure   TDownloadManager.Sort(const AColumn: Cardinal);
  function  GetStr(const ARow: Cardinal): String;
  var
    tmp: Int64;
    dt : TDateTime;
  begin
    case AColumn of
      0: Result:= containers.Items[ARow].downloadInfo.title;
      3: Result:= containers.Items[ARow].downloadInfo.Website;
      4: Result:= containers.Items[ARow].downloadInfo.SaveTo;
      5: begin
           Result:= containers.Items[ARow].downloadInfo.dateTime;
           if TryStrToDateTime(Result, dt, FMDFormatSettings) then
             tmp:= DateTimeToUnix(dt)
           else
             tmp:= 0;
           Result:= IntToStr(tmp);
         end;
    end;
  end;

  procedure QSort(L, R: Cardinal);
  var i, j: Cardinal;
         X: String;
  begin
    X:= GetStr((L+R) div 2);
    i:= L;
    j:= R;
    while i<=j do
    begin
      case SortDirection of
        FALSE:
          begin
            case AColumn of
              5:
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
              5:
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
        Swap(i, j);
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
  if containers.Count <= 2 then
    exit;
  sortColumn:= AColumn;
  QSort(0, containers.Count-1);
  MainForm.vtDownloadFilters;
end;

end.

