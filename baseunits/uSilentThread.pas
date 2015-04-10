{
        File: uSilentThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
        ---------------------------------------------
        As the name "silent" suggests, the job of theses classes is to get
        manga information from the site and add them to download list or
        favorites silently.
}

unit uSilentThread;

{$mode delphi}

interface

uses
  SysUtils,
  uBaseUnit, uData, uFMDThread;

type
  // metadata - Store manga information in the queue, so that it will be
  // processed by silent thread later
  TSilentThreadMetaData = class
  protected
    // to determine the job (add to download list or favorites)
    FType: Integer;
    FWebsite, FManga, FURL, FPath: String;
  public
    constructor Create(const AType: Integer;
      const AWebsite, AManga, AURL, APath: String);
    procedure Run;
  end;

  { TSilentThread }

  TSilentThread = class(TFMDThread)
  protected
    FSavePath: String;
    Info: TMangaInformation;
    Freconnect: Cardinal;

    // manga information from main thread
    title, website, URL: String;

    procedure MainThreadAfterChecking; virtual;
    procedure MainThreadIncreaseThreadCount; virtual;
    procedure MainThreadDecreaseThreadCount; virtual;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property SavePath: String read FSavePath write FSavePath;
  end;

  // for "Download all" feature
  TSilentGetInfoThread = class(TSilentThread)

  end;

  // for "Add to Favorites" feature
  TSilentAddToFavThread = class(TSilentThread)
  protected
    procedure MainThreadAfterChecking; override;
    procedure MainThreadDecreaseThreadCount; override;
  public
  end;

procedure CreateDownloadAllThread(const AWebsite, AManga, AURL: String;
  ASavePath: String = '');
procedure CreateAddToFavThread(const AWebsite, AManga, AURL: String;
  ASavePath: String = '');

const
  maxActiveSilentThread = 2;

implementation

uses
  frmMain;

// ----- TSilentThreadMetaData -----

constructor TSilentThreadMetaData.Create(const AType: Integer;
  const AWebsite, AManga, AURL, APath: String);
begin
  inherited Create;
  FType := AType;
  FWebsite := AWebsite;
  FManga := AManga;
  FURL := AURL;
  FPath := APath;
end;

procedure TSilentThreadMetaData.Run;
var
  silentThread: TSilentThread;
begin
  case FType of
    0: silentThread := TSilentThread.Create;
    1: silentThread := TSilentAddToFavThread.Create;
  end;
  if (FType in [0..1]) then
  begin
    silentThread.SavePath := FPath;
    silentThread.website := FWebsite;
    silentThread.URL := FURL;
    silentThread.title := FManga;
    silentThread.Start;
  end;
end;

// ----- TSilentThread -----

procedure TSilentThread.MainThreadAfterChecking;
var
  s: String;
  i, pos: Integer;
begin
  if Info.mangaInfo.numChapter = 0 then
    Exit;
  try
    with MainForm do
    begin
      // add a new download task
      pos := DLManager.AddTask;
      DLManager.containers.Items[pos].mangaSiteID := GetMangaSiteID(website);

      for i := 0 to Info.mangaInfo.numChapter - 1 do
      begin
        // generate folder name
        s := CustomRename(OptionCustomRename,
          Info.mangaInfo.website,
          Info.mangaInfo.title,
          info.mangaInfo.authors,
          Info.mangaInfo.artists,
          Info.mangaInfo.chapterName.Strings[i],
          Format('%.4d', [i + 1]),
          cbOptionPathConvert.Checked);
        DLManager.containers.Items[pos].chapterName.Add(s);
        DLManager.containers.Items[pos].chapterLinks.Add(
          Info.mangaInfo.chapterLinks.Strings[i]);
      end;

      if cbAddAsStopped.Checked then
      begin
        DLManager.containers.Items[pos].Status := STATUS_STOP;
        DLManager.containers.Items[pos].downloadInfo.Status := stStop;
      end
      else
      begin
        DLManager.containers.Items[pos].downloadInfo.Status := stWait;
        DLManager.containers.Items[pos].Status := STATUS_WAIT;
      end;

      DLManager.containers.Items[pos].currentDownloadChapterPtr := 0;
      DLManager.containers.Items[pos].downloadInfo.title := Info.mangaInfo.title;
      DLManager.containers.Items[pos].downloadInfo.Website := website;
	  if Trim(edSaveTo.Text) = '' then
        edSaveTo.Text := options.ReadString('saveto', 'SaveTo', DEFAULT_PATH);
      if Trim(edSaveTo.Text) = '' then
        edSaveTo.Text := DEFAULT_PATH;
      edSaveTo.Text := CorrectPathSys(edSaveTo.Text);
      s := edSaveTo.Text;
      // save to
      if cbOptionGenerateMangaFolderName.Checked then
      begin
        if not cbOptionPathConvert.Checked then
          s := s + RemoveSymbols(Info.mangaInfo.title)
        else
          s := s + RemoveSymbols(UnicodeRemove(Info.mangaInfo.title));
      end;
      s := CorrectPathSys(s);

      DLManager.containers.Items[pos].downloadInfo.SaveTo := s;

      // time
      DLManager.containers.Items[pos].downloadInfo.dateTime := Now;

      //DLManager.Sort(vtDownload.Header.SortColumn);
      //DLManager.SortNatural(vtDownload.Header.SortColumn);
      UpdateVtDownload;

      DLManager.Backup;
      DLManager.CheckAndActiveTask(False, Self);

      // generate downloaded chapters
      s := '';
      if Info.mangaInfo.chapterLinks.Count = 0 then
        Exit;
      for i := 0 to Info.mangaInfo.chapterLinks.Count - 1 do
      begin
        s := s + IntToStr(i) + SEPERATOR;
      end;
      if s <> '' then
        DLManager.AddToDownloadedChaptersList(Info.mangaInfo.website + URL, s);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentThread.MainThreadIncreaseThreadCount;
begin
  MainForm.CS_SilentThread_ThreadCount.Acquire;
  try
    Inc(MainForm.currentActiveSilentThreadCount);
  finally
    MainForm.CS_SilentThread_ThreadCount.Release;
  end;
end;

procedure TSilentThread.MainThreadDecreaseThreadCount;
var
  meta: TSilentThreadMetaData;
begin
  MainForm.CS_SilentThread_ThreadCount.Acquire;
  try
    with MainForm do
    begin
      Dec(MainForm.silentThreadCount);
      Dec(currentActiveSilentThreadCount);
      // change status
      if silentThreadCount > 0 then
        sbMain.Panels[1].Text := 'Loading: ' + IntToStr(silentThreadCount)
      else
        sbMain.Panels[1].Text := '';
    end;
    // TODO
    if MainForm.SilentThreadQueue.Count > 0 then
    begin
      meta := TSilentThreadMetaData(MainForm.SilentThreadQueue.Pop);
      if meta <> nil then
      begin
        meta.Run;
        meta.Free;
      end;
    end;

  finally
    MainForm.CS_SilentThread_ThreadCount.Release;
  end;
end;

procedure TSilentThread.Execute;
begin
  try
    while MainForm.currentActiveSilentThreadCount > maxActiveSilentThread do
      Sleep(250);
    Synchronize(MainThreadIncreaseThreadCount);
    Info.mangaInfo.title := title;
    if Info.GetInfoFromURL(website, URL, Freconnect) = NO_ERROR then
      if not Self.Terminated then
        Synchronize(MainThreadAfterChecking);
    Synchronize(MainThreadDecreaseThreadCount);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

constructor TSilentThread.Create;
begin
  inherited Create(True);
  Freconnect := 3;
  Info := TMangaInformation.Create;
  SavePath := '';
end;

destructor TSilentThread.Destroy;
begin
  Info.Free;
  inherited Destroy;
end;

// ----- TSilentAddToFavThread -----

procedure TSilentAddToFavThread.MainThreadAfterChecking;
var
  s, s2: String;
  i: Cardinal;
begin
  try
    with MainForm do
    begin
      if FSavePath = '' then
      begin
	    if Trim(edSaveTo.Text) = '' then
          edSaveTo.Text := options.ReadString('saveto', 'SaveTo', DEFAULT_PATH);
        if Trim(edSaveTo.Text) = '' then
          edSaveTo.Text := DEFAULT_PATH;
        edSaveTo.Text := CorrectPathSys(edSaveTo.Text);
        s := edSaveTo.Text;
      end
      else
        s := CorrectPathSys(FSavePath);

      if cbOptionGenerateMangaFolderName.Checked then
      begin
        if not cbOptionPathConvert.Checked then
          s := s + RemoveSymbols(title)
        else
          s := s + RemoveSymbols(UnicodeRemove(title));
      end;
      s := CorrectPathSys(s);

      s2 := '';
      if (Info.mangaInfo.numChapter > 0) then
      begin
        for i := 0 to Info.mangaInfo.numChapter - 1 do
          s2 := s2 + Info.mangaInfo.chapterLinks.Strings[i] + SEPERATOR;
      end;

      favorites.Add(title,
        IntToStr(Info.mangaInfo.numChapter),
        s2,
        website,
        s,
        URL);
      UpdateVtFavorites;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentAddToFavThread.MainThreadDecreaseThreadCount;
begin
  inherited;
  Dec(MainForm.silentAddToFavThreadCount);
end;

procedure CreateDownloadAllThread(const AWebsite, AManga, AURL: String;
  ASavePath: String = '');
var
  meta: TSilentThreadMetaData;
begin
  meta := TSilentThreadMetaData.Create(0, AWebsite, AManga, AURL, ASavePath);
  MainForm.SilentThreadQueue.Push(meta);
  Inc(MainForm.silentThreadCount);
end;

procedure CreateAddToFavThread(const AWebsite, AManga, AURL: String;
  ASavePath: String = '');
var
  meta: TSilentThreadMetaData;
begin
  meta := TSilentThreadMetaData.Create(1, AWebsite, AManga, AURL, ASavePath);
  MainForm.SilentThreadQueue.Push(meta);
  Inc(MainForm.silentThreadCount);
  Inc(MainForm.silentAddToFavThreadCount);
end;

end.
