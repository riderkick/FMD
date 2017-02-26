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
  SysUtils, fgl, uBaseUnit, uData, uDownloadsManager,
  WebsiteModules, FMDOptions, httpsendthread, LazFileUtils;

type

  TMetaDataType = (MD_DownloadAll, MD_AddToFavorites);

  TSilentThreadMetaData = class
  private
    ModuleId: Integer;
  public
    MetaDataType: TMetaDataType;
    Website, Title, URL, SaveTo: String;
    constructor Create(const AType: TMetaDataType;
      const AWebsite, AManga, AURL, APath: String);
  end;

  TSilentThreadManager = class;

  { TSilentThread }

  TSilentThread = class(THTTPThread)
  protected
    FSavePath: String;
    Info: TMangaInformation;
    Freconnect: Cardinal;
    // manga information from main thread
    title, website, URL: String;
    ModuleId: Integer;
    procedure MainThreadAfterChecking; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    Manager: TSilentThreadManager;
    constructor Create;
    destructor Destroy; override;
    property SavePath: String read FSavePath write FSavePath;
  end;

  { TSilentAddToFavThread }

  TSilentAddToFavThread = class(TSilentThread)
  protected
    procedure MainThreadAfterChecking; override;
  end;

  { TSilentThreadManagerThread }

  TSilentThreadManagerThread = class(THTTPThread)
  protected
    procedure Checkout;
    procedure Execute; override;
  public
    Manager: TSilentThreadManager;
    destructor Destroy; override;
  end;

  TSilentThreadMetaDatas = TFPGList<TSilentThreadMetaData>;
  TSilentThreads = TFPGList<TSilentThread>;

  { TSilentThreadManager }

  TSilentThreadManager = class
  private
    FLockAdd: Boolean;
    FManagerThread: TSilentThreadManagerThread;
    function GetItemCount: Integer;
    procedure StartManagerThread;
    procedure Checkout(Index: Integer);
  public
    MetaDatas: TSilentThreadMetaDatas;
    Threads: TSilentThreads;
    procedure Add(AType: TMetaDataType; AWebsite, AManga, AURL: String;
      ASavePath: String = '');
    procedure StopAll(WaitFor: Boolean = True);
    procedure UpdateLoadStatus;
    procedure BeginAdd;
    procedure EndAdd;
    property ItemCount: Integer read GetItemCount;
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_SilentThreadLoadStatus = 'Loading: %d/%d';

implementation

uses
  frmMain;

{ TSilentThreadManagerThread }

procedure TSilentThreadManagerThread.Checkout;
var
  i: Integer;
begin
  if Terminated then Exit;
  if Manager.MetaDatas.Count = 0 then Exit;
  with Manager do
  begin
    i := 0;
    while i < MetaDatas.Count do
    begin
      if Terminated then Break;
      with MetaDatas[i] do
        if (Threads.Count < OptionMaxThreads) and
          Modules.CanCreateConnection(ModuleId) then
        begin
          LockCreateConnection;
          try
            if Modules.CanCreateConnection(ModuleId) then
              Manager.Checkout(i);
          finally
            UnlockCreateConnection;
          end;
        end
        else
          Inc(i);
    end;
  end;
end;

procedure TSilentThreadManagerThread.Execute;
begin
  if Manager = nil then Exit;
  Self.Checkout;
  with manager do
    while (not Terminated) and (MetaDatas.Count > 0) do
    begin
      Sleep(SOCKHEARTBEATRATE);
      while (not Terminated) and (Threads.Count >= OptionMaxThreads) do
        Sleep(SOCKHEARTBEATRATE);
      Self.Checkout;
    end;
end;

destructor TSilentThreadManagerThread.Destroy;
begin
  Manager.FManagerThread := nil;
  inherited Destroy;
end;

{ TSilentThreadManager }

function TSilentThreadManager.GetItemCount: Integer;
begin
  Result := MetaDatas.Count + Threads.Count;
end;

procedure TSilentThreadManager.StartManagerThread;
begin
  if FManagerThread = nil then
  begin
    FManagerThread := TSilentThreadManagerThread.Create;
    FManagerThread.Manager := Self;
    FManagerThread.Start;
  end;
end;

procedure TSilentThreadManager.Add(AType: TMetaDataType;
  AWebsite, AManga, AURL: String; ASavePath: String = '');
begin
  if not ((AType = MD_AddToFavorites) and
    (MainForm.FavoriteManager.IsMangaExist(AManga, AWebsite))) then
  begin
    MetaDatas.Add(TSilentThreadMetaData.Create(
      AType, AWebsite, AManga, AURL, ASavePath));
    if not FLockAdd then
    begin
      StartManagerThread;
      UpdateLoadStatus;
    end;
  end;
end;

procedure TSilentThreadManager.Checkout(Index: Integer);
begin
  if (Index < 0) or (Index >= MetaDatas.Count) then Exit;
  Modules.IncActiveConnectionCount(MetaDatas[Index].ModuleId);
  case MetaDatas[Index].MetaDataType of
    MD_DownloadAll: Threads.Add(TSilentThread.Create);
    MD_AddToFavorites: Threads.Add(TSilentAddToFavThread.Create);
  end;
  with Threads.Last do
  begin
    Manager := Self;
    website := MetaDatas[Index].Website;
    title := MetaDatas[Index].Title;
    URL := MetaDatas[Index].URL;
    SavePath := MetaDatas[Index].SaveTo;
    ModuleId := MetaDatas[Index].ModuleId;
    Start;
    MetaDatas[Index].Free;
    MetaDatas.Delete(Index);
  end;
end;

procedure TSilentThreadManager.StopAll(WaitFor: Boolean);
var
  i: Integer;
begin
  if MetaDatas.Count or Threads.Count > 0 then
  begin
    while MetaDatas.Count > 0 do
    begin
      MetaDatas.Last.Free;
      MetaDatas.Remove(MetaDatas.Last);
    end;
    if Assigned(FManagerThread) then
    begin
      FManagerThread.Terminate;
      if WaitFor then
        FManagerThread.WaitFor;
    end;
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        Threads[i].Terminate;
    if WaitFor then
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

procedure TSilentThreadManager.BeginAdd;
begin
  FLockAdd := True;
end;

procedure TSilentThreadManager.EndAdd;
begin
  FLockAdd := False;
  if MetaDatas.Count > 0 then
  begin
    StartManagerThread;
    UpdateLoadStatus;
  end;
end;

constructor TSilentThreadManager.Create;
begin
  inherited Create;
  MetaDatas := TSilentThreadMetaDatas.Create;
  Threads := TSilentThreads.Create;
  FLockAdd := False;
end;

destructor TSilentThreadManager.Destroy;
var
  i: Integer;
begin
  if ItemCount > 0 then
  begin
    while MetaDatas.Count > 0 do
    begin
      MetaDatas.Last.Free;
      MetaDatas.Remove(MetaDatas.Last);
    end;
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        Threads[i].Terminate;
    while ItemCount > 0 do
      Sleep(100);
  end;
  MetaDatas.Free;
  Threads.Free;
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
  ModuleId := Modules.LocateModule(Website);
end;

{ TSilentThread }

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
      DLManager.Items[p].Website := website;

      if Trim(title) = '' then
        title := Info.mangaInfo.title;
      for i := 0 to Info.mangaInfo.numChapter - 1 do
      begin
        // generate folder name
        s := CustomRename(OptionChapterCustomRename,
                          website,
                          title,
                          info.mangaInfo.authors,
                          Info.mangaInfo.artists,
                          Info.mangaInfo.chapterName.Strings[i],
                          Format('%.4d', [i + 1]),
                          OptionChangeUnicodeCharacter,
                          OptionChangeUnicodeCharacterStr);
        DLManager.Items[p].chapterName.Add(s);
        DLManager.Items[p].chapterLinks.Add(
          Info.mangaInfo.chapterLinks.Strings[i]);
      end;

      if cbAddAsStopped.Checked then
      begin
        DLManager.Items[p].Status := STATUS_STOP;
        DLManager.Items[p].downloadInfo.Status := Format('[%d/%d] %s',[0,DLManager[p].ChapterLinks.Count,RS_Stopped]);
      end
      else
      begin
        DLManager.Items[p].downloadInfo.Status := Format('[%d/%d] %s',[0,DLManager[p].ChapterLinks.Count,RS_Waiting]);
        DLManager.Items[p].Status := STATUS_WAIT;
      end;

      DLManager.Items[p].currentDownloadChapterPtr := 0;
      DLManager.Items[p].downloadInfo.Website := website;
      DLManager.Items[p].downloadInfo.Link := URL;
      DLManager.Items[p].downloadInfo.Title := title;
      DLManager.Items[p].downloadInfo.DateTime := Now;

      if FSavePath = '' then
      begin
        FilledSaveTo;
        FSavePath := edSaveTo.Text;
        // save to
        if OptionGenerateMangaFolder then
          FSavePath := AppendPathDelim(FSavePath) + CustomRename(
            OptionMangaCustomRename,
            website,
            title,
            info.mangaInfo.authors,
            info.mangaInfo.artists,
            '',
            '',
            OptionChangeUnicodeCharacter,
            OptionChangeUnicodeCharacterStr);
      end;
      DLManager.Items[p].downloadInfo.SaveTo := FSavePath;

      UpdateVtDownload;
      DLManager.CheckAndActiveTask(False);

      // save downloaded chapters
      if Info.mangaInfo.chapterLinks.Count > 0 then
      begin
        DLManager.DownloadedChapters.Chapters[Info.mangaInfo.website + URL]:=
          Info.mangaInfo.chapterLinks.Text;
        FavoriteManager.AddToDownloadedChaptersList(Info.mangaInfo.website,
          URL, Info.mangaInfo.chapterLinks);
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentThread.DoTerminate;
begin
  LockCreateConnection;
  try
    Modules.DecActiveConnectionCount(ModuleId);
    Manager.Threads.Remove(Self);
  finally
    UnlockCreateConnection;
  end;
  Synchronize(Manager.UpdateLoadStatus);
  inherited DoTerminate;
end;

procedure TSilentThread.Execute;
begin
  Synchronize(Manager.UpdateLoadStatus);
  try
    Info.ModuleId := Self.ModuleId;
    Info.mangaInfo.title := title;
    if Info.GetInfoFromURL(website, URL, DefaultRetryCount) = NO_ERROR then
      if not Terminated then
        Synchronize(MainThreadAfterChecking);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

constructor TSilentThread.Create;
begin
  inherited Create(True);
  Freconnect := 3;
  SavePath := '';
  Info := TMangaInformation.Create(Self);
  ModuleId := -1;
end;

destructor TSilentThread.Destroy;
begin
  Info.Free;
  inherited Destroy;
end;

{ TSilentAddToFavThread }

procedure TSilentAddToFavThread.MainThreadAfterChecking;
var
  s: String;
begin
  try
    with MainForm do
    begin
      if Trim(title) = '' then
        title := Info.mangaInfo.title;
      if FSavePath = '' then
      begin
        FilledSaveTo;
        s := edSaveTo.Text;
      end
      else
        s := FSavePath;
      if OptionGenerateMangaFolder then
        s := AppendPathDelim(s) + CustomRename(
          OptionMangaCustomRename,
          website,
          title,
          info.mangaInfo.authors,
          info.mangaInfo.artists,
          '',
          '',
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);

      if Trim(title) = '' then
        title := Info.mangaInfo.title;
      FavoriteManager.Add(title,
        IntToStr(Info.mangaInfo.numChapter),
        info.mangaInfo.chapterLinks.Text,
        website,
        CorrectPathSys(s),
        URL);
      UpdateVtFavorites;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
