{
        File: downloads.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit downloads;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, baseunit, data, fgl, zip, ExtCtrls, Graphics;

type
  TDownloadManager = class;
  TTaskThreadContainer = class;
  TTaskThread = class;

  // this class will replace the old TDownloadThread
  TDownloadThread = class(TThread)
  protected
    // wait for changing directoet completed
    procedure   SetChangeDirectoryFalse;
    procedure   SetChangeDirectoryTrue;
    // Get download link from URL
    function    GetLinkPageFromURL(const URL: String): Boolean;
    // Get number of download link from URL
    function    GetPageNumberFromURL(const URL: String): Boolean;
    // Download page - link from link list
    function    DownloadPage: Boolean;
    procedure   Execute; override;
    procedure   OnTag(tag: String);
    procedure   OnText(text: String);
  public
    anotherURL    : String;
    checkStyle    : Cardinal;

    isTerminated,
    isSuspended   : Boolean;
    // ID of the site
    workPtr       : Cardinal;
    manager       : TTaskThread;
    parse         : TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;

  TDownloadThreadList = TFPGList<TDownloadThread>;

  TTaskThread = class(TThread)
  protected
    procedure   CheckOut;
    procedure   CallMainFormRepaint;
    procedure   CallMainFormRepaintImm;
    procedure   Execute; override;
    procedure   Compress;
    // show notification when download completed
    procedure   ShowBaloon;
  public
    anotherURL : String;

    isTerminated,
    isSuspended: Boolean;

    Flag: Cardinal;
    // container (for storing information)
    container  : TTaskThreadContainer;
    // download threads
    threads    : TDownloadThreadList;

    constructor Create;
    destructor  Destroy; override;
    procedure   Stop(const check: Boolean = TRUE);
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
    workPtr    : Cardinal;
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
  public
    isRunningBackup,
    isFinishTaskAccessed: Boolean;

    compress,
    //
    retryConnect,
    // max. active tasks
    maxDLTasks,
    // max. download threads per task
    maxDLThreadsPerTask : Cardinal;
    // current chapterLinks which thread is processing
    containers          : TTaskThreadContainerList;

    ini                 : TIniFile;

   // downloadInfo        : array of TDownloadInfo;
    constructor Create;
    destructor  Destroy; override;

    procedure   Restore;
    procedure   Backup;

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
  end;

implementation

uses
  lazutf8classes, mainunit, HTMLParser, FastHTMLParser, HTMLUtil, SynaCode,
  FileUtil, HTTPSend, VirtualTrees;

// utility

procedure picScale(P1: TPicture; var P2: TPicture; const x, y: Integer);
var
  ARect: TRect;
begin
  P2.Clear;
  P2.BitMap.Width := X;
  P2.BitMap.Height:= Y;
  Arect:= Rect(0, 0, X, Y);
  P2.BitMap.Canvas.StretchDraw(ARect, P1.BitMap);
end;

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
  Dec(manager.container.activeThreadCount);
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
       if manager.container.pageNumber > 0 then
          for i:= 0 to manager.container.pageNumber-1 do
            manager.container.pageLinks.Add('W');
      end;
    // get page link
    CS_GETPAGELINK:
      begin
        GetLinkPageFromURL(manager.container.chapterLinks.Strings[manager.container.currentDownloadChapterPtr]);
      end;
    // download page
    CS_DOWNLOAD:
      begin
        DownloadPage;
      end;
  end;
  Terminate;
end;

function    TDownloadThread.GetPageNumberFromURL(const URL: String): Boolean;
var
  myParser: THTMLParser;
  Parser  : TjsFastHTMLParser;

  function GetAnimeAPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l), ANIMEA_ROOT +
                                 StringReplace(URL, '.html', '', []) +
                                 '-page-1.html',
                                 manager.container.manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('Page 1 of ', l.Strings[i])<>0) then
      begin
        manager.container.pageNumber:= StrToInt(GetString(l.Strings[i], 'Page 1 of ', '<'));
        break;
      end;
    l.Free;
  end;

  function GetMangaHerePageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAHERE_ROOT + URL,
                     manager.container.manager.retryConnect);
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
        if GetTagName(parse.Strings[i]) = 'option' then
        begin
          j:= i;
          while GetTagName(parse.Strings[j]) = 'option' do
          begin
            Inc(manager.container.pageNumber);
            Inc(j, 4);
          end;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetEsMangaHerePageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     ESMANGAHERE_ROOT + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= parse.Count-1 downto 4 do
      begin
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(parse.Strings[i-3])));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetSubMangaPageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     SUBMANGA_ROOT + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= parse.Count-1 downto 3 do
      begin
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(parse.Strings[i-2])));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetAnimeExtremistPageNumber: Boolean;
  var
    i, j: Cardinal;
    l   : TStringList;
    s   : String;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= StringReplace(ANIMEEXTREMIST_ROOT + URL, '.html', '', []) + '-1.html';
    Result:= GetPage(TObject(l),
                     StringReplace(ANIMEEXTREMIST_ROOT + URL, '.html', '', []) + '-1.html',
                     manager.container.manager.retryConnect);
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
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.pageNumber:= StrToInt(GetString(TrimLeft(TrimRight(parse.Strings[i-3]+'~!@')), 'Pagina ', '~!@'));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaInnPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAINN_ROOT + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 1;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('Previous', parse.Strings[i]) <> 0 then
         // if Pos('Page', parse.Strings[i+2]) <> 0 then
        begin
          j:= i+7;
          s:= parse.Strings[j];
          while GetTagName(parse.Strings[j]) = 'option' do
          begin
            Inc(manager.container.pageNumber);
            Inc(j, 3);
          end;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetOurMangaPageNumber: Boolean;
  // OurManga is a lot different than other site
  var
    isExtractpageContainerLinks: Boolean = FALSE;
    correctURL,
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    // pass 1: Find correct chapter
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     OURMANGA_ROOT + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 1;
      for i:= 0 to parse.Count-1 do
      begin
        if (GetTagName(parse.Strings[i]) = 'a') AND
           (Pos(OURMANGA_ROOT + URL, parse.Strings[i]) <> 0) then
          correctURL:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
      end;
    end;
    parse.Clear;
    l.Clear;

    // pass 2: Find number of pages

    Result:= GetPage(TObject(l),
                     correctURL,
                     manager.container.manager.retryConnect);
    manager.container.pageContainerLinks.Clear;
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
        if NOT isExtractpageContainerLinks then
        begin
          if (GetTagName(parse.Strings[i]) = 'select') AND
             (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'name=')) = 'page') then
            isExtractpageContainerLinks:= TRUE;
        end
        else
        begin
          if (GetTagName(parse.Strings[i]) = 'option') then
          begin
            manager.container.pageContainerLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value=')));
            Inc(manager.container.pageNumber);
          end
          else
          if Pos('</select>', parse.Strings[i])<>0 then
            break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetBatotoPageNumber: Boolean;
  var
    isGoOn: Boolean = FALSE;
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  label
    reload;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
  reload:
    parse.Clear;
    l.Clear;
    Result:= GetPage(TObject(l),
                     BATOTO_ROOT + URL + '/1',
                     manager.container.manager.retryConnect);

    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.SlowExec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('page_select', parse.Strings[i])<>0) then
        begin
          isGoOn:= TRUE;
          break;
        end;
      end;
    end;
    if NOT isGoOn then
    begin
      Sleep(3000);
      goto reload;
    end;

   // parse.Add(BATOTO_ROOT + URL + '/1');
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('page_select', parse.Strings[i]) <> 0 then
         // if Pos('Page', parse.Strings[i+2]) <> 0 then
        begin
          j:= i+2;
          s:= parse.Strings[j];
          while GetTagName(parse.Strings[j]) = 'option' do
          begin
            Inc(manager.container.pageNumber);
            Inc(j, 3);
          end;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetHentai2ReadPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     HENTAI2READ_ROOT + URL,
                     manager.container.manager.retryConnect);
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
        if (GetTagName(parse.Strings[i]) = 'select') AND
           (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cbo_wpm_pag') then
        begin
          j:= i+1;
          while GetTagName(parse.Strings[j]) = 'option' do
          begin
            Inc(manager.container.pageNumber);
            Inc(j, 3);
          end;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaReaderPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAREADER_ROOT + URL,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    s:= MANGAREADER_ROOT + URL;
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('</select>', parse.Strings[i])>0) AND
           (Pos('</div>', parse.Strings[i+2])>0) then
        begin
          s:= parse.Strings[i+1];
          Delete(s, Pos(' of ', s), 4);
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaParkPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAPARK_ROOT + URL + '1',
                     manager.container.manager.retryConnect);
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
        if (Pos('1 of ', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i];
          Delete(s, Pos('1 of ', s), 5);
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaFoxPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(URL + '/1.html');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('option value="0"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-3];
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetStarkanaPageNumber: Boolean;
  var
    s    : String;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(STARKANA_ROOT + URL);
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= parse.Count-1 downto 5 do
      begin
        if (Pos('</option>', parse.Strings[i])>0) then
        begin
          s:= TrimLeft(TrimRight(parse.Strings[i-1]));
          manager.container.pageNumber:= StrToInt(s);
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetEatMangaPageNumber: Boolean;
  var
    s    : String;
    count: Cardinal = 0;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(EATMANGA_ROOT + URL);
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('</select>', parse.Strings[i])>0) then
          if count > 0 then
          begin
            s:= parse.Strings[i-2];
            manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
            break;
          end
          else
            Inc(count);
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaPandaPageNumber: Boolean;
  var
    s    : String;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(MANGAPANDA_ROOT + URL);
    if (Pos('.html', URL) > 0) AND (Pos(SEPERATOR, URL) > 0) then
      s:= StringReplace(s, SEPERATOR, '-1/', []);
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 1 to parse.Count-1 do
      begin
        if (Pos(' of ', parse.Strings[i])>0) AND
           (Pos('select', parse.Strings[i-1])>0) then
        begin
          s:= GetString(parse.Strings[i]+'~!@', ' of ', '~!@');
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetRedHawkScansPageNumber: Boolean;
  var
    s    : String;
    i, j : Cardinal;
    l    : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(REDHAWKSCANS_ROOT + URL +'page/1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= 1 to parse.Count-1 do
      begin
        if (Pos('class="topbar_right"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i+4];
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaTradersPageNumber: Boolean;
  var
    isStartGetPageNumber: Boolean = FALSE;
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGATRADERS_ROOT + URL,
                     manager.container.manager.retryConnect);
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
        if (NOT isStartGetPageNumber) AND
           (Pos('option value="1"  selected="selected"', parse.Strings[i]) > 0) then
          isStartGetPageNumber:= TRUE;
        if (isStartGetPageNumber) AND
           (Pos('</option>', parse.Strings[i])>0) then
          Inc(manager.container.pageNumber);
        if (isStartGetPageNumber) AND
           (Pos('</select>', parse.Strings[i])>0) then
          break;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaStreamPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(MANGASTREAM_ROOT2 + URL + '/1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('Last Page (', parse.Strings[i])>0) then
        begin
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(GetString(parse.Strings[i], 'Last Page (', ')'))));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetKomikidPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(KOMIKID_ROOT + URL + '/1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('title="Next Page"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-6];
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetPecintaKomikPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(PECINTAKOMIK_ROOT + URL + '/1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageNumber:= 0;
      for i:= parse.Count-1 downto 5 do
      begin
        if (Pos('</option>', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-1];
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetTurkcraftPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(TURKCRAFT_ROOT + URL + '/1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('title="Next Page"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-5];
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaVadisiPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(MANGAVADISI_ROOT + MANGAVADISI_BROWSER + URL + '/1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('title="Sonraki Sayfa"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-6];
          manager.container.pageNumber:= StrToInt(GetString(s, '"', '"'));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaFramePageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    s:= DecodeUrl(MANGAFRAME_ROOT + URL + '1');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('class="divider"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-8];
          manager.container.pageNumber:= StrToInt(GetString(s, '/page/', '"'));
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaEdenPageNumber: Boolean;
  var
    s   : String;
    i, j: Cardinal;
    l   : TStringList;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
    if manager.container.mangaSiteID = MANGAEDEN_ID then
      s:= DecodeUrl(MANGAEDEN_ROOT + URL + '1/')
    else
      s:= DecodeUrl(PERVEDEN_ROOT + URL + '1/');
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
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
        if (Pos('span class="next"', parse.Strings[i])>0) then
        begin
          s:= parse.Strings[i-3];
          manager.container.pageNumber:= StrToInt(TrimLeft(TrimRight(s)));
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

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
 { if manager.container.mangaSiteID = OURMANGA_ID then
    Result:= GetOurMangaPageNumber
  else }
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
  if manager.container.mangaSiteID = MANGASTREAM_ID then
    Result:= GetMangaStreamPageNumber
  else
  if manager.container.mangaSiteID = REDHAWKSCANS_ID then
    Result:= GetRedHawkScansPageNumber
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
  if manager.container.mangaSiteID = TURKCRAFT_ID then
    Result:= GetTurkcraftPageNumber
  else
  if manager.container.mangaSiteID = MANGAVADISI_ID then
    Result:= GetMangaVadisiPageNumber
  else
  if manager.container.mangaSiteID = MANGAFRAME_ID then
    Result:= GetMangaFramePageNumber
  else
  if (manager.container.mangaSiteID = MANGAEDEN_ID) OR
     (manager.container.mangaSiteID = PERVEDEN_ID) then
    Result:= GetMangaEdenPageNumber
  else
  if manager.container.mangaSiteID = GEHENTAI_ID then
  begin
    Result:= GetGEHentaiPageNumber('', TRUE);
   { if manager.container.pageNumber > 20 then
      for i:=1 to ((manager.container.pageNumber-1) div 20) do
        Result:= GetGEHentaiPageNumber('?page=' + IntToStr(i), FALSE); }
   // manager.container.pageLinks.SaveToFile('log.txt');
  end
  else
  if (manager.container.mangaSiteID = KISSMANGA_ID) OR
     (manager.container.mangaSiteID = BLOGTRUYEN_ID) OR
     (manager.container.mangaSiteID = MANGAPARK_ID) OR
     (manager.container.mangaSiteID = MANGA24H_ID) OR
     (manager.container.mangaSiteID = VNSHARING_ID) OR
     (manager.container.mangaSiteID = TRUYEN18_ID) OR
     (manager.container.mangaSiteID = TRUYENTRANHTUAN_ID) OR
     (manager.container.mangaSiteID = FAKKU_ID) OR
     (manager.container.mangaSiteID = MANGAKU_ID) then
  begin
    // all of the page links are in a html page
    Result:= TRUE;
    manager.container.pageNumber:= 1;
  end
  else
  if manager.container.mangaSiteID = HENTAI2READ_ID then
    Result:= GetHentai2ReadPageNumber;
end;

function    TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
var
  myParser: THTMLParser;
  Parser  : TjsFastHTMLParser;

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
                     manager.container.manager.retryConnect);
    for i:= 0 to l.Count-1 do
      if (Pos('class="mangaimg', l.Strings[i])<>0) then
      begin
        manager.container.pageLinks.Strings[workPtr]:= GetString(l.Strings[i], '<img src="', '"');
        break;
      end;
    l.Free;
  end;

  function GetMangaHereLinkPage: Boolean;
  var
    c: Char;
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if workPtr > 0 then
      Result:= GetPage(TObject(l),
                       MANGAHERE_ROOT + URL + IntToStr(workPtr+1)+'.html',
                       manager.container.manager.retryConnect)
    else
      Result:= GetPage(TObject(l),
                       MANGAHERE_ROOT + URL,
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
        for c:= 'a' to 'z' do
          if (Pos('http://'+c+'.mhcdn.net/store/', parse.Strings[i])<>0) then
          begin
            manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            parse.Free;
            l.Free;
            exit;
          end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetEsMangaHereLinkPage: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if workPtr > 0 then
      Result:= GetPage(TObject(l),
                       ESMANGAHERE_ROOT + URL + IntToStr(workPtr+1)+'.html',
                       manager.container.manager.retryConnect)
    else
      Result:= GetPage(TObject(l),
                       ESMANGAHERE_ROOT + URL,
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
        if (Pos('class="read_img"', parse.Strings[i])<>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+6], 'src='));
          parse.Free;
          l.Free;
          exit;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaInnLinkPage: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAINN_ROOT + URL + '/page_'+IntToStr(workPtr+1),
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
        if GetTagName(parse.Strings[i]) = 'img' then
          if GetAttributeValue(GetTagAttribute(parse.Strings[i], 'id='))='imgPage' then
          begin
            manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            break;
          end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetOurMangaLinkPage: Boolean;
  var
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     OURMANGA_ROOT + URL + '/' + manager.container.pageContainerLinks.Strings[workPtr],
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
        if (GetTagName(parse.Strings[i]) = 'div') AND
           (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class=')) = 'prev_next_top') then
        begin
          j:= i;
          repeat
            Dec(j);
            if GetTagName(parse.Strings[j]) = 'img' then
            begin
              manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[j], 'src='));
              parse.Free;
              l.Free;
              exit;
            end;
          until j = 0;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetKissMangaLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     KISSMANGA_ROOT + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('lstImages.push("', parse.Strings[i]) > 0 then
        begin
          s:= parse.Strings[i];
          repeat
            j:= Pos('lstImages.push("', s);
            manager.container.pageLinks.Add(EncodeUrl(GetString(s, 'lstImages.push("', '");')));
            Delete(s, Pos('lstImages.push("', s), 16);
            j:= Pos('lstImages.push("', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetBatotoLinkPage: Boolean;
  var
    isGoOn: Boolean = FALSE;
    i: Cardinal;
    l: TStringList;
    s: String;
  label
    reload;
  begin
    l:= TStringList.Create;
    parse:= TStringList.Create;
  reload:
    parse.Clear;
    l.Clear;
    Result:= GetPage(TObject(l),
                     BATOTO_ROOT + URL + '/'+IntToStr(workPtr+1),
                     manager.container.manager.retryConnect);

    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.SlowExec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      for i:= 0 to parse.Count-1 do
      begin
        if (Pos('page_select', parse.Strings[i])<>0) then
        begin
          isGoOn:= TRUE;
          break;
        end;
      end;
    end;
    if NOT isGoOn then
    begin
      Sleep(3000);
      goto reload;
    end;

    if parse.Count>0 then
    begin
      for i:= 0 to parse.Count-1 do
        if GetTagName(parse.Strings[i]) = 'img' then
          if (Pos('batoto.net/comics', parse.Strings[i])>0) AND
             (Pos('z-index: 1003', parse.Strings[i])>0) then
          begin
            manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            break;
          end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetManga24hLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGA24H_ROOT + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if (GetTagName(parse.Strings[i]) = 'img') AND
           (Pos('style="border:3px', parse.Strings[i])<>0) then
          // (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class=')) = 'm_picture') then
        begin
          manager.container.pageLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetVnSharingLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     VNSHARING_ROOT + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('lstImages.push("', parse.Strings[i]) > 0 then
        begin
          s:= parse.Strings[i];
          repeat
            j:= Pos('lstImages.push("', s);
            manager.container.pageLinks.Add(EncodeUrl(GetString(s, 'lstImages.push("', '");')));
            Delete(s, Pos('lstImages.push("', s), 16);
            j:= Pos('lstImages.push("', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetHentai2ReadLinkPage: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     HENTAI2READ_ROOT + URL + IntToStr(workPtr+1)+'/',
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
        if (GetTagName(parse.Strings[i]) = 'img') AND
           (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'id='))='img_mng_enl') then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetFakkuLinkPage: Boolean;
  var
    i, j  : Cardinal;
    l     : TStringList;
    imgURL: String;
  begin
    l:= TStringList.Create;
    // get number of pages
    Result:= GetPage(TObject(l),
                     FAKKU_ROOT + StringReplace(URL, '/read', '', []){ + '#page' + IntToStr(workPtr+1)},
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    j:= 0;
    if parse.Count>0 then
    begin
      i:= 0;
      manager.container.pageLinks.Clear;
      while i < parse.Count-1 do
      begin
        if (Pos('favorites', parse.Strings[i])>0) AND
           (Pos('pages', parse.Strings[i+4])>0) then
        begin
          j:= StrToInt(TrimRight(TrimLeft(parse.Strings[i+2])));
          break;
        end;
        Inc(i);
      end;
    end;
    // get link pages
    l.Clear;
    Result:= GetPage(TObject(l),
                     FAKKU_ROOT + URL + '#page' + IntToStr(workPtr+1),
                     manager.container.manager.retryConnect);
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      i:= 0;
      manager.container.pageLinks.Clear;
      while i < parse.Count-1 do
      begin
        if (Pos('return ''http://c.fakku.net/', parse.Strings[i])>0) then
        begin
        //  manager.container.pageLinks.Strings[workPtr]:=
          imgURL:= 'http://c.fakku.net/' + GetString(parse.Strings[i], '''http://c.fakku.net/', '''');
          break;
        end
        else
        if (Pos('return ''http://cdn.fakku.net/', parse.Strings[i])>0) then
        begin
        //  manager.container.pageLinks.Strings[workPtr]:=
          imgURL:= 'http://cdn.fakku.net/' + GetString(parse.Strings[i], '''http://cdn.fakku.net/', '''');
          break;
        end;
        Inc(i);
      end;
    end;
    // build page files
    for i:= 1 to j do
    begin
     // s:= imgURL + Format('%3.3d.jpg', [i]);
      manager.container.pageLinks.Add(imgURL + Format('%3.3d.jpg', [i]));
    end;
    parse.Free;
    l.Free;
  end;

  function GetTruyen18LinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     TRUYEN18_ROOT + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('[IMG]http://', parse.Strings[i]) > 0 then
        begin
          s:= parse.Strings[i];
          repeat
            j:= Pos('[IMG]http://', s);
            manager.container.pageLinks.Add(EncodeUrl(GetString(s, '[IMG]', '[/IMG];')));
            Delete(s, Pos('[IMG]http://', s), 16);
            j:= Pos('[IMG]http://', s);
          until j = 0;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaReaderLinkPage: Boolean;
  var
    realURL,
    s: String;
    j,
    i: Cardinal;
    l: TStringList;

    procedure  BreakURL;
    var
      isSlashed: Boolean = FALSE;
      i,
      oldI     : Cardinal;
    begin
      if Pos('.html', URL) = 0 then
      begin
        realURL:= URL + '/' + IntToStr(workPtr+1);
        exit;
      end;
      i:= 2;
      realURL:= '/';
      while i <= Length(URL) do
      begin
        if (NOT isSlashed) AND (URL[i] = '/') then
        begin
          isSlashed:= TRUE;
          oldI:= i;
          for i:= i-1 downto 1 do
          begin
            if URL[i] <> '-' then
            begin
              SetLength(realURL, Length(realURL)-1);
            end
            else
            begin
              realURL:= realURL + IntToStr(workPtr+1);
              break;
            end;
          end;
          i:= oldI;
         // realURL:= realURL + '/';
        end
        else
        begin
          realURL:= realURL + URL[i];
          Inc(i);
        end;
      end;
    end;

  begin
    l:= TStringList.Create;
    BreakURL;
    Result:= GetPage(TObject(l),
                     MANGAREADER_ROOT + realURL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
     // manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if GetTagName(parse.Strings[i]) = 'img' then
        begin
          if //(Pos(realURL, parse.Strings[i])>0) AND
             (Pos('alt=', parse.Strings[i])>0) then
          begin
            manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
            break;
          end;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaParkLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAPARK_ROOT + URL + 'all',//IntToStr(workPtr+1),
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
       // if GetTagName(parse.Strings[i]) = 'img' then
        if (Pos('a target="_blank"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
      //    break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaFoxLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(URL + '/' + IntToStr(workPtr+1) + '.html');
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('onclick="return enlarge()"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetStarkanaLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(STARKANA_ROOT + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= parse.Count-1 downto 5 do
        if (Pos('style="cursor: pointer;"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetEatMangaLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(EATMANGA_ROOT + URL + 'page-' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('<div id="prefetchimg"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetSubMangaLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(SUBMANGA_ROOT + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('type="text/javascript"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i-3], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetAnimeExtremistLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(StringReplace(ANIMEEXTREMIST_ROOT + URL, '.html', '', []) + '-' + IntToStr(workPtr+1) + '.html');
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('id="photo"', parse.Strings[i])>0) then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaPandaLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;

    if (Pos('.html', URL) > 0) AND (Pos(SEPERATOR, URL) > 0) then
    begin
      s:= DecodeUrl(MANGAPANDA_ROOT + URL);
      s:= StringReplace(s, SEPERATOR, '-' + IntToStr(workPtr+1) + '/', [])
    end
    else
      s:= DecodeUrl(MANGAPANDA_ROOT + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('"imgholder"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetRedHawkScansLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;

    s:= DecodeUrl(REDHAWKSCANS_ROOT + URL + 'page/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('class="open"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaStreamLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(MANGASTREAM_ROOT2 + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('id="manga-page"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetTruyenTranhTuanLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     TRUYENTRANHTUAN_ROOT + URL + 'doc-truyen/',
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('var slides2=["', parse.Strings[i]) > 0 then
        begin
          s:= parse.Strings[i];
          repeat
            j:= Pos('"/manga/', s);
            manager.container.pageLinks.Add(EncodeUrl(TRUYENTRANHTUAN_ROOT + '/manga/' + GetString(s, '"/manga/', '"')));
            Delete(s, Pos('"/manga/', s), 10);
            j:= Pos('"/manga/', s);
          until j = 0;
          break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetBlogTruyenLinkPage: Boolean;
  var
    isExtrackLink: Boolean = FALSE;
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     BLOGTRUYEN_ROOT + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if NOT (isExtrackLink) AND (Pos('<div id="noidungchuong">', parse.Strings[i]) > 0) then
          isExtrackLink:= TRUE;
        if (isExtrackLink) AND (GetTagName(parse.Strings[i]) = 'img') then
          manager.container.pageLinks.Add(EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='))))
        else
        if (isExtrackLink) AND (Pos('</div>', parse.Strings[i])>0) then
          break;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetKomikidLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(KOMIKID_ROOT + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('class="lazyload_ad"', parse.Strings[i])>0) then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i-8], 'src='));
          if Pos('http://', s) = 0 then
            s:= KOMIKID_ROOT + KOMIKID_BROWSER + s;
          manager.container.pageLinks.Strings[workPtr]:= EncodeURL(s);
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetPecintaKomikLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(PECINTAKOMIK_ROOT + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('mangas/', parse.Strings[i])>0) then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          if Pos('/manga/', s) = 0 then
            s:= PECINTAKOMIK_ROOT + '/manga/' + s
          else
            s:= PECINTAKOMIK_ROOT + PECINTAKOMIK_BROWSER + s;
          manager.container.pageLinks.Strings[workPtr]:= EncodeURL(s);
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangakuLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     MANGAKU_ROOT + URL,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count>0 then
    begin
      manager.container.pageLinks.Clear;
      for i:= 0 to parse.Count-1 do
      begin
        if Pos('class="separator"', parse.Strings[i]) > 0 then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
          manager.container.pageLinks.Add(EncodeUrl(s));
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetTurkcraftLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(TURKCRAFT_ROOT + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('class="picture"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= EncodeURL(TURKCRAFT_ROOT + TURKCRAFT_BROWSER + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaVadisiLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(MANGAVADISI_ROOT + MANGAVADISI_BROWSER + URL + '/' + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('class="picture"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= EncodeURL(MANGAVADISI_ROOT + MANGAVADISI_BROWSER + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaFrameLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= DecodeUrl(MANGAFRAME_ROOT + URL + IntToStr(workPtr+1));
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('class="open"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= EncodeURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaTradersLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    s:= MANGATRADERS_ROOT + URL + '/page/' + IntToStr(workPtr+1);
    Result:= GetPage(TObject(l),
                     s,
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
        if (Pos('"image_display"', parse.Strings[i])>0) then
        begin
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+4], 'src='));
          if s <> '' then
            manager.container.pageLinks.Strings[workPtr]:= s
          else
            manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+12], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetMangaEdenLinkPage: Boolean;
  var
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    if manager.container.mangaSiteID = MANGAEDEN_ID then
      s:= MANGAEDEN_ROOT + URL + IntToStr(workPtr+1) + '/'
    else
      s:= PERVEDEN_ROOT + URL + IntToStr(workPtr+1) + '/';
    Result:= GetPage(TObject(l),
                     s,
                     manager.container.manager.retryConnect);
    parse:= TStringList.Create;
    Parser:= TjsFastHTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count>0 then
    begin
      for i:= parse.Count-1 downto 0 do
        if (Pos('"mainImg"', parse.Strings[i])>0) then
        begin
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src='));
          break;
        end;
    end;
    parse.Free;
    l.Free;
  end;

  function GetGEHentaiLinkPage: Boolean;
  var
    s1,s2,
    s: String;
    j,
    i: Cardinal;
    l: TStringList;
  begin
    l:= TStringList.Create;
    Result:= GetPage(TObject(l),
                     URL,// + IntToStr(workPtr+1),
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
          manager.container.pageLinks.Strings[workPtr]:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
          manager.container.pageLinks.Strings[workPtr]:= StringReplace(manager.container.pageLinks.Strings[workPtr], '&amp;', '&', [rfReplaceAll]);
          // s:= manager.container.pageLinks.Strings[workPtr];
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
  if manager.container.pageLinks.Strings[workPtr] <> 'W' then exit;
  if manager.container.mangaSiteID = ANIMEA_ID then
    Result:= GetAnimeALinkPage
  else
  if manager.container.mangaSiteID = MANGATRADERS_ID then
    Result:= GetMangaTradersLinkPage
  else
  if manager.container.mangaSiteID = MANGAHERE_ID then
    Result:= GetMangaHereLinkPage
  else
  if manager.container.mangaSiteID = MANGAINN_ID then
    Result:= GetMangaInnLinkPage
  else
  if manager.container.mangaSiteID = OURMANGA_ID then
    Result:= GetOurMangaLinkPage
  else
  if manager.container.mangaSiteID = KISSMANGA_ID then
    Result:= GetKissMangaLinkPage
  else
  if manager.container.mangaSiteID = BATOTO_ID then
    Result:= GetBatotoLinkPage
  else
  if manager.container.mangaSiteID = MANGA24H_ID then
    Result:= GetManga24hLinkPage
  else
  if manager.container.mangaSiteID = VNSHARING_ID then
    Result:= GetVnSharingLinkPage
  else
  if manager.container.mangaSiteID = HENTAI2READ_ID then
    Result:= GetHentai2ReadLinkPage
  else
  if manager.container.mangaSiteID = FAKKU_ID then
    Result:= GetFakkuLinkPage
  else
  if manager.container.mangaSiteID = TRUYEN18_ID then
    Result:= GetTruyen18LinkPage
  else
  if manager.container.mangaSiteID = MANGAREADER_ID then
    Result:= GetMangaReaderLinkPage
  else
  if manager.container.mangaSiteID = MANGAPARK_ID then
    Result:= GetMangaParkLinkPage
  else
  if manager.container.mangaSiteID = MANGAFOX_ID then
    Result:= GetMangaFoxLinkPage
  else
  if manager.container.mangaSiteID = STARKANA_ID then
    Result:= GetStarkanaLinkPage
  else
  if manager.container.mangaSiteID = EATMANGA_ID then
    Result:= GetEatMangaLinkPage
  else
  if manager.container.mangaSiteID = MANGAPANDA_ID then
    Result:= GetMangaPandaLinkPage
  else
  if manager.container.mangaSiteID = MANGASTREAM_ID then
    Result:= GetMangaStreamLinkPage
  else
  if manager.container.mangaSiteID = REDHAWKSCANS_ID then
    Result:= GetRedHawkScansLinkPage
  else
  if manager.container.mangaSiteID = ESMANGAHERE_ID then
    Result:= GetEsMangaHereLinkPage
  else
  if manager.container.mangaSiteID = SUBMANGA_ID then
    Result:= GetSubMangaLinkPage
  else
  if manager.container.mangaSiteID = ANIMEEXTREMIST_ID then
    Result:= GetAnimeExtremistLinkPage
  else
  if manager.container.mangaSiteID = KOMIKID_ID then
    Result:= GetKomikidLinkPage
  else
  if manager.container.mangaSiteID = PECINTAKOMIK_ID then
    Result:= GetPecintaKomikLinkPage
  else
  if manager.container.mangaSiteID = MANGAKU_ID then
    Result:= GetMangakuLinkPage
  else
  if manager.container.mangaSiteID = TURKCRAFT_ID then
    Result:= GetTurkcraftLinkPage
  else
  if manager.container.mangaSiteID = MANGAVADISI_ID then
    Result:= GetMangaVadisiLinkPage
  else
  if manager.container.mangaSiteID = MANGAFRAME_ID then
    Result:= GetMangaFrameLinkPage
  else
  if manager.container.mangaSiteID = TRUYENTRANHTUAN_ID then
    Result:= GetTruyenTranhTuanLinkPage
  else
  if manager.container.mangaSiteID = BLOGTRUYEN_ID then
    Result:= GetBlogTruyenLinkPage
  else
  if (manager.container.mangaSiteID = MANGAEDEN_ID) OR
     (manager.container.mangaSiteID = PERVEDEN_ID) then
    Result:= GetMangaEdenLinkPage
  else
  if manager.container.mangaSiteID = GEHENTAI_ID then
    Result:= GetGEHentaiLinkPage;
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
    HTTP.UserAgent:='curl/7.21.0 (i686-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18';
    HTTP.ProxyHost:= Host;
    HTTP.ProxyPort:= Port;
    HTTP.ProxyUser:= User;
    HTTP.ProxyPass:= Pass;
    if manager.container.mangaSiteID = HENTAI2READ_ID then
      HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/')
    else
    if manager.container.mangaSiteID = ANIMEEXTREMIST_ID then
    begin
      HTTP.Headers.Insert(0, 'Referer:'+ANIMEEXTREMIST_ROOT+'/');
    end
    else
    if manager.container.mangaSiteID = KISSMANGA_ID then
      HTTP.Headers.Insert(0, 'Referer:'+KISSMANGA_ROOT+'/')
    else
    if manager.container.mangaSiteID = VNSHARING_ID then
      HTTP.Headers.Insert(0, 'Referer:'+VNSHARING_ROOT+'/')
    else
    if  manager.container.mangaSiteID = GEHENTAI_ID then
      HTTP.Headers.Insert(0, 'Referer:'+manager.container.pageLinks.Strings[workPtr]);
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

    while HTTP.ResultCode = 302 do
    begin
      URL:= CheckRedirect(HTTP);
      HTTP.Clear;
      HTTP.RangeStart:= 0;
      if Pos(HENTAI2READ_ROOT, URL) <> 0 then
        HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/')
      else
      if Pos(KISSMANGA_ROOT, URL) <> 0 then
        HTTP.Headers.Insert(0, 'Referer:'+KISSMANGA_ROOT+'/');
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

   // while isChangeDirectory do
   //   Sleep(16);
   // Synchronize(SetChangeDirectoryTrue);
   // SetCurrentDirUTF8(Path);

   { if manager.container.mangaSiteID = GEHENTAI_ID then
    begin
      for i:=0 to http.Headers.Count-1 do
      begin
        s:= UpperCase(HTTP.Headers[i]);
        if pos('CONTENT-LENGTH:', s)>0 then
        begin
          fileSize:= StrToIntDef(Copy(s,pos(':', s)+1, Length(s)),0);
          break;
        end;
      end;
      if fileSize <> 925 then
        HTTP.Document.SaveToFile(Path+'/'+name+ext);
    end
    else }

    {if (ext='.png') OR (ext='.jpg') then
    begin
      source:= TPicture.Create;
      dest  := TPicture.Create;

      source.LoadFromStream(HTTP.Document);
      picScale(source, dest, 100, 100);

      dest  .SaveToFile(Path+'/'+name+ext);

      dest  .Clear;
      dest  .Free;
      source.Clear;
      source.Free;
    end
    else}

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
 // s:= manager.container.pageLinks.Strings[workPtr];
 // manager.container.pageLinks.SaveToFile('log.txt');
  if (manager.container.pageLinks.Strings[workPtr] = '') OR
     (manager.container.pageLinks.Strings[workPtr] = 'W') then exit;
  SavePage(manager.container.pageLinks.Strings[workPtr],
           manager.container.downloadInfo.SaveTo+
           '/'+manager.container.chapterName.Strings[manager.container.currentDownloadChapterPtr],
           Format('%.3d', [workPtr+1]),
           manager.container.manager.retryConnect);
//  s:= manager.container.pageLinks.Strings[workPtr];
 { if manager.container.mangaSiteID = GEHENTAI_ID then
    if fileSize = 925 then
    begin
      anotherURL:= anotherURLBackup;
      goto start;
    end; }
  manager.container.pageLinks.Strings[workPtr]:= '';
  SetCurrentDirUTF8(oldDir);

 // Synchronize(SetChangeDirectoryFalse);
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

procedure   TTaskThread.CallMainFormRepaint;
begin
  if MainForm.isCanRefreshForm then
  begin
    MainForm.vtDownload.Repaint;
    MainForm.isCanRefreshForm:= FALSE;
  end;
end;

procedure   TTaskThread.CallMainFormRepaintImm;
begin
  MainForm.vtDownload.Repaint;
  MainForm.isCanRefreshForm:= FALSE;
end;

procedure   TTaskThread.Compress;
var
  Compresser: TCompress;
begin
  if (container.manager.compress >= 1) then
  begin
    Sleep(100);
    container.downloadInfo.Status:= Format('%s (%d/%d)', [stIsCompressing, container.currentDownloadChapterPtr, container.chapterLinks.Count]);
    MainForm.vtDownload.Repaint;
    Compresser:= TCompress.Create;
    case container.manager.compress of
      1: Compresser.ext:= '.zip';
      2: Compresser.ext:= '.cbz';
      3: Compresser.ext:= '.pdf';
    end;
    Compresser.CompressionQuality:= OptionPDFQuality;
    Compresser.Path:= container.downloadInfo.SaveTo+'/'+
                      container.chapterName.Strings[container.currentDownloadChapterPtr];
    Compresser.Execute;
    Compresser.Free;
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
  if (container.mangaSiteID = BATOTO_ID) then
  begin
  //  Sleep(150);
  {  if Flag = CS_GETPAGELINK then
    begin
      if container.manager.maxDLThreadsPerTask>2 then
        currentMaxThread:= 2;
    end
    else  }
    {if container.workPtr < 3 then
      currentMaxThread:= 3
    else}
  //    currentMaxThread:= 1;
    currentMaxThread:= container.manager.maxDLThreadsPerTask;
  end
  else
  if (container.mangaSiteID = EATMANGA_ID) then
    currentMaxThread:= 1
  else
    currentMaxThread:= container.manager.maxDLThreadsPerTask;

  if container.mangaSiteID = GEHENTAI_ID then
  begin
    if (container.workPtr <> 0) then
    begin
      repeat
        Sleep(32);
        s:= anotherURL;
        Delete(s, 1, 13);
       // if container.workPtr >= (container.pageNumber-1) then
       //   exit;
        if (s <> '') AND
           (StrToInt(GetString(s+' ', '-', ' ')) = (container.workPtr+1)) then
          break;
      until FALSE;
    end;
    Sleep(500*currentMaxThread);
  end
  else
    Sleep(100);

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
      threads.Items[threads.Count-1].workPtr:= container.workPtr;
      threads.Items[threads.Count-1].checkStyle:= Flag;
      threads.Items[threads.Count-1].isSuspended:= FALSE;
      Inc(container.workPtr);
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
      threads.Items[i].workPtr:= container.workPtr;
      threads.Items[i].checkStyle:= Flag;
      threads.Items[i].isSuspended:= FALSE;
      Inc(container.workPtr);
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
      threads.Items[threads.Count-1].workPtr:= container.workPtr;
      threads.Items[threads.Count-1].checkStyle:= CS_GETPAGENUMBER;
      threads.Items[threads.Count-1].isSuspended:= FALSE;
      CheckPath(container.downloadInfo.SaveTo+
                '/'+
                container.chapterName.Strings[container.currentDownloadChapterPtr]);
      while (isSuspended) OR (NOT threads.Items[threads.Count-1].isTerminated) do
        Sleep(100);
    end;

    //get page links
   // if container.currentPageNumber < container.pageNumber then
    if (container.mangaSiteID <> GEHENTAI_ID) then
    begin
      container.workPtr:= 0;//container.currentPageNumber;
   //   if container.workPtr > 0 then
   //     Dec(container.workPtr);
      container.downloadInfo.iProgress:= 0;
      while container.workPtr < container.pageLinks.Count{container.pageNumber} do
      begin
        if Terminated then exit;
        Flag:= CS_GETPAGELINK;
        Checkout;
        container.downloadInfo.Progress:= Format('%d/%d', [container.workPtr, container.pageNumber]);
        container.downloadInfo.Status  := Format('%s (%d/%d)', [stPreparing, container.currentDownloadChapterPtr, container.chapterLinks.Count]);
        Inc(container.downloadInfo.iProgress);
        {$IFDEF WIN32}
        MainForm.vtDownload.Repaint;
        {$ELSE}
        Synchronize(CallMainFormRepaint);
        {$ENDIF}
      end;
      WaitFor;
    end;

    //download pages
   // container.Status:= STATUS_DOWNLOAD;
    container.workPtr:= 0;
    container.downloadInfo.iProgress:= 0;

    // will bypass the download section if links = nil
    if (container.pageLinks.Count > 0) then
    begin
      while container.workPtr < container.pageLinks.Count do
      begin
        if Terminated then exit;
        Flag:= CS_DOWNLOAD;
        Checkout;
        container.downloadInfo.Progress:= Format('%d/%d', [container.workPtr, container.pageLinks.Count]);
        container.downloadInfo.Status  := Format('%s (%d/%d)', [stDownloading, container.currentDownloadChapterPtr, container.chapterLinks.Count]);
        Inc(container.downloadInfo.iProgress);
        {$IFDEF WIN32}
        MainForm.vtDownload.Repaint;
        {$ELSE}
        Synchronize(CallMainFormRepaint);
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
    if (container.workPtr >= container.pageLinks.Count) AND
       (container.currentDownloadChapterPtr >= container.chapterLinks.Count) then
    begin
      container.downloadInfo.Status  := stFinish;
      container.downloadInfo.Progress:= '';
      container.Status:= STATUS_FINISH;
      container.manager.CheckAndActiveTask(TRUE);
      {$IFDEF WIN32}
      MainForm.vtDownload.Repaint;
      {$ELSE}
      Synchronize(CallMainFormRepaintImm);
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
      Synchronize(CallMainFormRepaintImm);
      {$ENDIF}
    end;
  end;
  {if check then
  begin
    if threads.Count = 0 then exit;
    for i:= 0 to threads.Count-1 do
      if (Assigned(threads.Items[i])) AND (NOT threads.Items[i].isTerminated) then
      begin
       // threads.Items[i].Suspend;
        threads.Items[i].Terminate;
       // threads.Items[i].Free;
      end;
  end;}
  threads.Clear;
end;

// ----- TTaskThreadContainer -----

constructor TTaskThreadContainer.Create;
begin
  chapterLinks     := TStringList.Create;
  chapterName      := TStringList.Create;
  pageLinks        := TStringList.Create;
  pageContainerLinks:= TStringList.Create;
  workPtr:= 0;
  currentPageNumber:= 0;
  currentDownloadChapterPtr:= 0;
  inherited Create;
end;

destructor  TTaskThreadContainer.Destroy;
begin
  thread.Terminate;
  pageContainerLinks.Free;
  pageLinks.Free;
  chapterName.Free;
  chapterLinks.Free;
  inherited Destroy;
end;

// ----- TDownloadThread -----

{constructor TDownloadThread.Create;
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

function    TDownloadThread.GetPageNumberFromURL(const URL: String): Boolean;
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

function    TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
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
    }
// ----- TDownloadManager -----

constructor TDownloadManager.Create;
begin
  inherited Create;

  // Create INI file
  ini:= TIniFile.Create(WORK_FOLDER + WORK_FILE);
  ini.CacheUpdates:= FALSE;
  containers:= TTaskThreadContainerList.Create;
  isFinishTaskAccessed:= FALSE;
  isRunningBackup     := FALSE;

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
  inherited Destroy;
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
  ini.UpdateFile;
  isRunningBackup:= FALSE;
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
    end;
  end;

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
      end {
      else
      if containers.Items[i].mangaSiteID = BATOTO_ID then
      begin
        if batotoCount = 0 then
        begin
          ActiveTask(i);
          Inc(batotoCount);
          Inc(count);
        end;
      end }
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
    end;
  end;
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
     { else
      if (containers.Items[i].mangaSiteID = BATOTO_ID) then
        Inc(batotoCount) }
      else
      if (containers.Items[i].mangaSiteID = EATMANGA_ID) then
        Inc(eatMangaCount);
    end;
  end;

  if (containers.Items[pos].mangaSiteID = GEHENTAI_ID) AND (geCount > 0) then
    exit
 { else
  if (containers.Items[pos].mangaSiteID = BATOTO_ID) AND (batotoCount > 0) then
    exit }
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
    containers.Items[taskID].thread.Terminate;
 {   containers.Items[taskID].downloadInfo.Status:= stStop;
    containers.Items[taskID].Status:= STATUS_STOP;
  end
  else
  if containers.Items[taskID].Status = STATUS_WAIT then
  begin
    containers.Items[taskID].downloadInfo.Status:= stStop;
    containers.Items[taskID].Status:= STATUS_STOP; }
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
end;

procedure   TDownloadManager.StopAllTasks;
var
  i: Cardinal;
begin
  if containers.Count = 0 then exit;
  // check and stop any active thread
  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
    begin
      containers.Items[i].thread.Terminate;
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
end;

procedure   TDownloadManager.StopAllDownloadTasksForExit;
var
  i: Cardinal;
begin
  if containers.Count = 0 then exit;
  for i:= 0 to containers.Count-1 do
  begin
    if containers.Items[i].Status = STATUS_DOWNLOAD then
      containers.Items[i].thread.Terminate;
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
end;

procedure   TDownloadManager.RemoveTask(const taskID: Cardinal);
begin
  if taskID >= containers.Count then exit;
  // check and stop any active thread
  if containers.Items[taskID].Status = STATUS_DOWNLOAD then
  begin
    containers.Items[taskID].thread.Terminate;
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

end.

