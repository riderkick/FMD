{
        File: SilentThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit SilentThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, baseunit, data, fgl, DownloadsManager,
  Graphics, Process, lclintf, contnrs, FMDThread;

type
  // metadata
  TSilentThreadMeta = class
  protected
    FType: Integer;
    FWebsite,
    FManga,
    FURL,
    FPath: String;
  public
    constructor Create(const AType: Integer; const AWebsite, AManga, AURL, APath: String);
    procedure   Run;
  end;

  TSilentThread = class(TFMDThread)
  protected
    FSavePath: String;

    procedure   CallMainFormAfterChecking; virtual;
    procedure   CallMainFormIncreaseThreadCount; virtual;
    procedure   CallMainFormDecreaseThreadCount; virtual;
    procedure   Execute; override;
  public
    Info        : TMangaInformation;

    // manga information from main thread
    title,
    website, URL: String;

    constructor Create;
    destructor  Destroy; override;

    property    SavePath: String read FSavePath write FSavePath;
  end;

  // for "Download all" feature
  TSilentGetInfoThread = class(TSilentThread)

  end;

  // for "Add to Favorites" feature
  TSilentAddToFavThread = class(TSilentThread)
  protected
    procedure   CallMainFormAfterChecking; override;
    procedure   CallMainFormDecreaseThreadCount; override;
  public
  end;

procedure CreateDownloadAllThread(const AWebsite, AManga, AURL: String; ASavePath: String = '');
procedure CreateAddToFavThread(const AWebsite, AManga, AURL: String; ASavePath: String = '');

var
  SilentThreadQueue: TQueue;

implementation

uses
  mainunit;

// ----- TSilentThreadMeta -----

constructor TSilentThreadMeta.Create(const AType: Integer; const AWebsite, AManga, AURL, APath: String);
begin
  inherited Create;
  FType   := AType;
  FWebsite:= AWebsite;
  FManga  := AManga;
  FURL    := AURL;
  FPath   := APath;
end;

procedure   TSilentThreadMeta.Run;
var
  silentThread: TSilentThread;
begin
  case FType of
    0: silentThread:= TSilentThread.Create;
    1: silentThread:= TSilentAddToFavThread.Create;
  end;
  if (FType in [0..1]) then
  begin
    silentThread.SavePath:= FPath;
    silentThread.website:= FWebsite;
    silentThread.URL:= FURL;
    silentThread.title:= FManga;
    silentThread.isSuspended:= FALSE;
  end;
end;

// ----- TSilentThread -----

procedure   TSilentThread.CallMainFormAfterChecking;
var
  hh, mm, ss, ms,
  day, month, year: Word;
  s, s1 : String;
  i, pos: Cardinal;
begin
  if Info.mangaInfo.numChapter = 0 then exit;
  with MainForm do
  begin
    // add a new download task
    DLManager.AddTask;
    pos:= DLManager.containers.Count-1;
    DLManager.containers.Items[pos].mangaSiteID:= GetMangaSiteID(website);

    for i:= 0 to Info.mangaInfo.numChapter-1 do
    begin
      // generate folder name
       s:= CustomRename(OptionCustomRename,
                       Info.mangaInfo.website,
                       Info.mangaInfo.title,
                       Info.mangaInfo.chapterName.Strings[i],
                       Format('%.4d', [i+1]),
                       cbOptionPathConvert.Checked);
      DLManager.containers.Items[pos].chapterName .Add(s);
      DLManager.containers.Items[pos].chapterLinks.Add(Info.mangaInfo.chapterLinks.Strings[i]);
    end;

    DLManager.containers.Items[pos].downloadInfo.Status:= stWait;
    DLManager.containers.Items[pos].Status:= STATUS_WAIT;

    DLManager.containers.Items[pos].currentDownloadChapterPtr:= 0;
    DLManager.containers.Items[pos].downloadInfo.title  := Info.mangaInfo.title;
    DLManager.containers.Items[pos].downloadInfo.Website:= website;
    if edSaveTo.Text = '' then
      edSaveTo.Text:= options.ReadString('saveto', 'SaveTo', '');
    s:= CorrectFile(edSaveTo.Text);
    if s[Length(s)] = '/' then
      Delete(s, Length(s), 1);

    // save to
    if cbOptionGenerateMangaFolderName.Checked then
    begin
      if NOT cbOptionPathConvert.Checked then
        s:= s + '/' + RemoveSymbols(Info.mangaInfo.title)
      else
        s:= s + '/' + RemoveSymbols(UnicodeRemove(Info.mangaInfo.title));
    end;
    DLManager.containers.Items[pos].downloadInfo.SaveTo:= s;

    // time
    DecodeDate(Now, year, month, day);
    DecodeTime(Time, hh, mm, ss, ms);
    DLManager.containers.Items[pos].downloadInfo.dateTime:= IntToStr(Month)+'/'+IntToStr(Day)+'/'+IntToStr(Year)+' '+IntToStr(hh)+':'+IntToStr(mm)+':'+IntToStr(ss);

    DLManager.Sort(vtDownload.Header.SortColumn);
    UpdateVtDownload;

    DLManager.Backup;
    DLManager.CheckAndActiveTask;

    // generate downloaded chapters
    s:= '';
    if Info.mangaInfo.chapterLinks.Count = 0 then exit;
    for i:= 0 to Info.mangaInfo.chapterLinks.Count-1 do
    begin
      s:= s+IntToStr(i) + SEPERATOR;
    end;
    if s <> '' then
      DLManager.AddToDownloadedChaptersList(Info.mangaInfo.website + URL, s);
  end;
end;

procedure   TSilentThread.CallMainFormIncreaseThreadCount;
begin
  Inc(MainForm.currentActiveSilentThreadCount);
end;

procedure   TSilentThread.CallMainFormDecreaseThreadCount;
var
  meta: TSilentThreadMeta;
begin
  with MainForm do
  begin
    Dec(silentThreadCount);
    Dec(currentActiveSilentThreadCount);
    // change status
    if silentThreadCount > 0 then
      sbMain.Panels[1].Text:= 'Loading: '+IntToStr(silentThreadCount)
    else
      sbMain.Panels[1].Text:= '';
  end;
  // TODO
  if SilentThreadQueue.Count > 0 then
  begin
    meta:= TSilentThreadMeta(SilentThreadQueue.Pop);
    if meta <> nil then
    begin
      meta.Run;
      meta.Free;
    end;
  end;
end;

procedure   TSilentThread.Execute;
var
  times: Cardinal;
  s    : String;
begin
  while isSuspended do Sleep(32);

  while MainForm.currentActiveSilentThreadCount > 2 do
    Sleep(250);
  Synchronize(CallMainFormIncreaseThreadCount);

  // some of the code was taken from subthreads's GetMangaInfo
  // since it's multi-thread, we cannot call IE for fetching info from Batoto
  if website = GEHENTAI_NAME then
    times:= 4
  else
    times:= 3;
  s:= URL;
  s:= webSite;
  Info.mangaInfo.title:= title;
  if Info.GetInfoFromURL(website, URL, times)<>NO_ERROR then
  begin
    Info.Free;
    exit;
  end;
  Synchronize(CallMainFormAfterChecking);
  Synchronize(CallMainFormDecreaseThreadCount);
end;

constructor TSilentThread.Create;
begin
  isSuspended    := TRUE;
  isTerminated   := FALSE;
  FreeOnTerminate:= TRUE;
  Info:= TMangaInformation.Create;
  SavePath:= '';

  inherited Create(FALSE);
end;

destructor  TSilentThread.Destroy;
begin
  Info.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

// ----- TSilentAddToFavThread -----

procedure   TSilentAddToFavThread.CallMainFormAfterChecking;
var
  s, s2: String;
  i    : Cardinal;
begin
  with MainForm do
  begin
    if FSavePath = '' then
    begin
      if edSaveTo.Text = '' then
        edSaveTo.Text:= options.ReadString('saveto', 'SaveTo', '');
      s:= CorrectFile(edSaveTo.Text);
    end
    else
      s:= FSavePath;
    if s[Length(s)] = '/' then
      Delete(s, Length(s), 1);

    if cbOptionGenerateMangaFolderName.Checked then
    begin
      if NOT cbOptionPathConvert.Checked then
        s:= s + '/' + RemoveSymbols(title)
      else
        s:= s + '/' + RemoveSymbols(UnicodeRemove(title));
    end;

    s2:= '';
    if (Info.mangaInfo.numChapter > 0) then
    begin
      for i:= 0 to Info.mangaInfo.numChapter-1 do
        s2:= s2 + Info.mangaInfo.chapterLinks.Strings[i] + SEPERATOR;
    end;

    favorites.Add(title,
                  IntToStr(Info.mangaInfo.numChapter),
                  s2,
                  website,
                  s,
                  URL);
    UpdateVtFavorites;
  end;
end;

procedure   TSilentAddToFavThread.CallMainFormDecreaseThreadCount;
begin
  inherited;
  Dec(MainForm.silentAddToFavThreadCount);
end;

procedure   CreateDownloadAllThread(const AWebsite, AManga, AURL: String; ASavePath: String = '');
var
  meta: TSilentThreadMeta;
begin
  meta:= TSilentThreadMeta.Create(0, AWebsite, AManga, AURL, ASavePath);
  SilentThreadQueue.Push(meta);
  Inc(MainForm.silentThreadCount);
end;

procedure   CreateAddToFavThread(const AWebsite, AManga, AURL: String; ASavePath: String = '');
var
  meta: TSilentThreadMeta;
begin
  meta:= TSilentThreadMeta.Create(1, AWebsite, AManga, AURL, ASavePath);
  SilentThreadQueue.Push(meta);
  Inc(MainForm.silentThreadCount);
  Inc(MainForm.silentAddToFavThreadCount);
end;

initialization
  SilentThreadQueue:= TQueue.Create;

finalization
  SilentThreadQueue.Free;

end.

