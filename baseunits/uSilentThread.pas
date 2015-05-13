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
  SysUtils, fgl, uBaseUnit, uData, uFMDThread, uDownloadsManager,
  blcksock, syncobjs;

type

  TMetaDataType = (MD_DownloadAll, MD_AddToFavorites);

  TSilentThreadMetaData = class
  public
    MetaDataType: TMetaDataType;
    Website,
    Title,
    URL,
    SaveTo: String;
    constructor Create(const AType: TMetaDataType; const AWebsite, AManga, AURL, APath: String);
  end;

  TSilentThreadManager = class;

  { TSilentThread }

  TSilentThread = class(TFMDThread)
  protected
    FSavePath: String;
    Info: TMangaInformation;
    Freconnect: Cardinal;

    // manga information from main thread
    title, website, URL: String;
    procedure SockOnHeartBeat(Sender: TObject);
    procedure MainThreadAfterChecking; virtual;
    procedure MainThreadUpdateStatus;
    procedure Execute; override;
  public
    Manager: TSilentThreadManager;
    constructor Create;
    destructor Destroy; override;

    property SavePath: String read FSavePath write FSavePath;
  end;

  // for "Add to Favorites" feature
  TSilentAddToFavThread = class(TSilentThread)
  protected
    procedure MainThreadAfterChecking; override;
  end;

  TMetaDataList = TFPGList<TSilentThreadMetaData>;
  TThreadList = TFPGList<TSilentThread>;

  { TSilentThreadManager }

  TSilentThreadManager = class
  protected
    function GetItemCount: Integer;
  public
    CS_Threads: TCriticalSection;
    DLManager: TDownloadManager;
    MetaData: TMetaDataList;
    Threads: TThreadList;
    procedure Add(AType: TMetaDataType; AWebsite, AManga, AURL: String;
      ASavePath: String = '');
    procedure CheckOut;
    procedure StopAllAndWait;
    procedure UpdateLoadStatus;
    property ItemCount: Integer read GetItemCount;

    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_SilentThreadLoadStatus = 'Loading: %d/%d';

implementation

uses
  frmMain;

{ TSilentThreadManager }

function TSilentThreadManager.GetItemCount: Integer;
begin
  CS_Threads.Acquire;
  try
    Result := MetaData.Count + Threads.Count;
  finally
    CS_Threads.Release;
  end;
end;

procedure TSilentThreadManager.Add(AType: TMetaDataType; AWebsite, AManga,
  AURL: String; ASavePath: String = '');
begin
  if not ((AType = MD_AddToFavorites) and
    (MainForm.FavoriteManager.IsMangaExist(AManga, AWebsite))) then
  begin
    CS_Threads.Acquire;
    try
        MetaData.Add(TSilentThreadMetaData.Create(
          AType, AWebsite, AManga, AURL, ASavePath));
    finally
      CS_Threads.Release;
    end;
    UpdateLoadStatus;
  end;
end;

procedure TSilentThreadManager.CheckOut;
begin
  CS_Threads.Acquire;
  try
    if MetaData.Count > 0 then
    begin
      case MetaData.First.MetaDataType of
        MD_DownloadAll: Threads.Add(TSilentThread.Create);
        MD_AddToFavorites: Threads.Add(TSilentAddToFavThread.Create);
      end;
      Threads.Last.Manager := Self;
      Threads.Last.website := MetaData.First.Website;
      Threads.Last.title := MetaData.First.Title;
      Threads.Last.URL := MetaData.First.URL;
      Threads.Last.SavePath := MetaData.First.SaveTo;
      Threads.Last.Start;
      MetaData.First.Free;
      MetaData.Remove(MetaData.First);
    end;
  finally
    CS_Threads.Release;
  end;
end;

procedure TSilentThreadManager.StopAllAndWait;
var
  i: Integer;
begin
  if MetaData.Count or Threads.Count > 0 then
  begin
    CS_Threads.Acquire;
    try
      while MetaData.Count > 0 do
      begin
        MetaData.Last.Free;
        MetaData.Remove(MetaData.Last);
      end;
      if Threads.Count > 0 then
        for i := 0 to Threads.Count - 1 do
         Threads[i].Terminate;
    finally
      CS_Threads.Release;
    end;
    while ItemCount > 0 do
      Sleep(100);
  end;
end;

procedure TSilentThreadManager.UpdateLoadStatus;
begin
  if ItemCount > 0 then
    MainForm.sbMain.Panels[1].Text :=
      Format(RS_SilentThreadLoadStatus, [Threads.Count, ItemCount])
  else
    MainForm.sbMain.Panels[1].Text := '';
end;

constructor TSilentThreadManager.Create;
begin
  inherited Create;
  CS_Threads := TCriticalSection.Create;
  MetaData := TMetaDataList.Create;
  Threads := TThreadList.Create;
end;

destructor TSilentThreadManager.Destroy;
var
  i: Integer;
begin
  if ItemCount > 0 then
  begin
    CS_Threads.Acquire;
    try
      while MetaData.Count > 0 do
      begin
        MetaData.Last.Free;
        MetaData.Remove(MetaData.Last);
      end;
      if Threads.Count > 0 then
        for i := 0 to Threads.Count - 1 do
          Threads[i].Terminate;
    finally
      CS_Threads.Release;
    end;
    while ItemCount > 0 do
      Sleep(100);
  end;
  MetaData.Free;
  Threads.Free;
  CS_Threads.Free;
  inherited Destroy;
end;

{ TSilentThreadMetaData }

constructor TSilentThreadMetaData.Create(const AType: TMetaDataType;
  const AWebsite, AManga, AURL, APath: String);
begin
  inherited Create;
  MetaDataType := AType;
  Website := AWebsite;
  Title := AManga;
  URL := AURL;
  SaveTo := APath;
end;

{ TSilentThread }

procedure TSilentThread.SockOnHeartBeat(Sender: TObject);
begin
  if Terminated then
  begin
    TBlockSocket(Sender).Tag := 1;
    TBlockSocket(Sender).StopFlag := True;
    TBlockSocket(Sender).AbortSocket;
  end;
end;

procedure TSilentThread.MainThreadAfterChecking;
var
  s: String;
  i, p: Integer;
begin
  if Info.mangaInfo.numChapter = 0 then
    Exit;
  try
    with MainForm do
    begin
      // add a new download task
      p := DLManager.AddTask;
      DLManager.containers.Items[p].mangaSiteID := GetMangaSiteID(website);

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
        DLManager.containers.Items[p].chapterName.Add(s);
        DLManager.containers.Items[p].chapterLinks.Add(
          Info.mangaInfo.chapterLinks.Strings[i]);
      end;

      if cbAddAsStopped.Checked then
      begin
        DLManager.containers.Items[p].Status := STATUS_STOP;
        DLManager.containers.Items[p].downloadInfo.Status := stStop;
      end
      else
      begin
        DLManager.containers.Items[p].downloadInfo.Status := stWait;
        DLManager.containers.Items[p].Status := STATUS_WAIT;
      end;

      DLManager.containers.Items[p].currentDownloadChapterPtr := 0;
      DLManager.containers.Items[p].downloadInfo.Website := website;
      DLManager.containers.Items[p].downloadInfo.Link := URL;
      DLManager.containers.Items[p].downloadInfo.Title := Info.mangaInfo.title;
      DLManager.containers.Items[p].downloadInfo.DateTime := Now;

      if FSavePath = '' then
      begin
        if Trim(edSaveTo.Text) = '' then
          edSaveTo.Text := options.ReadString('saveto', 'SaveTo', DEFAULT_PATH);
        if Trim(edSaveTo.Text) = '' then
          edSaveTo.Text := DEFAULT_PATH;
        edSaveTo.Text := CorrectPathSys(edSaveTo.Text);
        FSavePath := edSaveTo.Text;
        // save to
        if cbOptionGenerateMangaFolderName.Checked then
        begin
          if not cbOptionPathConvert.Checked then
            FSavePath := FSavePath + RemoveSymbols(Info.mangaInfo.title)
          else
            FSavePath := FSavePath + RemoveSymbols(UnicodeRemove(Info.mangaInfo.title));
        end;
        FSavePath := CorrectPathSys(FSavePath);
      end;
      DLManager.containers.Items[p].downloadInfo.SaveTo := FSavePath;

      UpdateVtDownload;
      DLManager.Backup;
      DLManager.CheckAndActiveTask(False, Self);

      // save downloaded chapters
      if Info.mangaInfo.chapterLinks.Count > 0 then
        DLManager.AddToDownloadedChaptersList(Info.mangaInfo.website + URL, Info.mangaInfo.chapterLinks);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentThread.MainThreadUpdateStatus;
begin
  if Manager.ItemCount > 0 then
    MainForm.sbMain.Panels[1].Text :=
      Format(RS_SilentThreadLoadStatus, [Manager.Threads.Count, Manager.ItemCount])
  else
    MainForm.sbMain.Panels[1].Text := '';
end;

procedure TSilentThread.Execute;
begin
  try
    Synchronize(MainThreadUpdateStatus);
    Info.mangaInfo.title := title;
    if Info.GetInfoFromURL(website, URL, Freconnect) = NO_ERROR then
      if not Terminated then
        Synchronize(MainThreadAfterChecking);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  Manager.CS_Threads.Acquire;
  try
    Manager.Threads.Remove(Self);
  finally
    Manager.CS_Threads.Release;
  end;
  Synchronize(MainThreadUpdateStatus);
end;

constructor TSilentThread.Create;
begin
  inherited Create(True);
  Freconnect := 3;
  SavePath := '';
  Info := TMangaInformation.Create;
  Info.FHTTP.Sock.OnHeartbeat := SockOnHeartBeat;
  Info.FHTTP.Sock.HeartbeatRate := SOCKHEARTBEATRATE;
end;

destructor TSilentThread.Destroy;
begin
  Info.Free;
  inherited Destroy;
end;

{ TSilentAddToFavThread }

procedure TSilentAddToFavThread.MainThreadAfterChecking;
var
  s, s2: String;
  i: Integer;
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

      FavoriteManager.Add(title,
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

end.
