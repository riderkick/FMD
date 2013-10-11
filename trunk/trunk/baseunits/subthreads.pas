{
        File: subthreads.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit subthreads;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, baseunit, data, fgl, downloads,
  Graphics, Process, lclintf;

type
  // some tasks will be done by SubThread
  TSubThread = class(TThread)
  protected
    FIsLoaded: Integer;
    FURL     : String;
    procedure   Execute; override;
    procedure   DoGetInfos;
    // for Batoto only - since it use IE, this must be called in main thread
    procedure   CallMainFormGetBatotoInfo;
    procedure   CallMainFormCannotGetInfo;
    procedure   CallMainFormShowLog;
    procedure   CallMainFormGetInfos;
    procedure   CallMainFormUpdate;
    procedure   CallMainFormUpdateRequire;
    procedure   CallMainFormImportant;
    procedure   CallMainFormLatestVer;
    procedure   CallMainFormSetButton;
    procedure   CallMainFormPopSilentThreadQueue;
  public
    updateCounter: Cardinal;
    isCheckForLatestVer: Boolean;
    mangaListPos: Integer;
    cover       : TPicture;
    isHasCover,
    isCanStop,
    isTerminated,
    isSuspended : Boolean;
    OnShowInformation: procedure of object;

    fNote, fNoteForThisRevision,
    fImportant,
    title,
    website, link: String;
    isGetInfos   : Boolean;
    Info         : TMangaInformation;
    boolResult   : Boolean;

    constructor Create;
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit, logform, fileutil, silentthreads;

var
  LRequireRevision: Cardinal = 1;
  LRevision: Cardinal;
  LVersion : String;

// ----- TSubThread -----

constructor TSubThread.Create;
begin
  fImportant     := '';
  updateCounter  := 1;
  isCheckForLatestVer:= FALSE;
  isCanStop      := FALSE;
  isSuspended    := TRUE;
  isTerminated   := FALSE;
  FreeOnTerminate:= TRUE;
  isGetInfos     := FALSE;
  Cover          := TPicture.Create;

 // ;
  inherited Create(FALSE);
end;

destructor  TSubThread.Destroy;
begin
  Cover.Free;
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TSubThread.CallMainFormGetBatotoInfo;
begin
  FIsLoaded:= Info.GetInfoFromURL(website, FURL, 0);
end;

procedure   TSubThread.DoGetInfos;
{var
  root: String;}

  function  GetMangaInfo(const URL, website: String): Boolean;
  var
    selectedWebsite: String;
    filterPos      : Cardinal;
    times          : Cardinal;
  begin
    Result:= FALSE;

    if mangaListPos > -1 then
    begin
      filterPos:= MainForm.dataProcess.GetPos(mangaListPos);
      Info.mangaInfo.title:= MainForm.dataProcess.Title.Strings[filterPos];
    end;

    Info.isGenerateFolderChapterName:= MainForm.cbOptionGenerateChapterName.Checked;
    Info.isRemoveUnicode:= MainForm.cbOptionPathConvert.Checked;

    if website = BATOTO_NAME then
      times:= 0
    else
    if website = GEHENTAI_NAME then
      times:= 4
    else
      times:= 3;

    {$IFDEF WINDOWS}
    if ((website <> BATOTO_NAME) OR (NOT OptionBatotoUseIEChecked)) then
    begin
      if Info.GetInfoFromURL(website, URL, times)<>NO_ERROR then
      begin
        Info.Free;
        exit;
      end;
    end
    else
    begin
      FURL:= URL;
      Synchronize(CallMainFormGetBatotoInfo);
      if FIsLoaded<>NO_ERROR then
      begin
        Info.Free;
        exit;
      end;
    end;
    {$ELSE}
    if Info.GetInfoFromURL(website, URL, times)<>NO_ERROR then
    begin
      Info.Free;
      exit;
    end;
    {$ENDIF}

    // fixed
    if mangaListPos > -1 then
    begin
      if website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] then
      begin
        if (website = MANGASTREAM_NAME) OR
           (website = MANGAVADISI_NAME) OR
           (website = SENMANGA_NAME) OR
           (website = MANGAFRAME_NAME) OR
           (website = S2SCAN_NAME) OR
           (website = EGSCANS_NAME) OR
           (website = TURKCRAFT_NAME) OR
           (website = HUGEMANGA_NAME) OR
           (website = KOMIKID_NAME) then
        begin
          Info.mangaInfo.authors:= MainForm.DataProcess.Param[filterPos, DATA_PARAM_AUTHORS];
          Info.mangaInfo.artists:= MainForm.DataProcess.Param[filterPos, DATA_PARAM_ARTISTS];
          Info.mangaInfo.genres := MainForm.DataProcess.Param[filterPos, DATA_PARAM_GENRES];
          Info.mangaInfo.summary:= MainForm.DataProcess.Param[filterPos, DATA_PARAM_SUMMARY];
        end;
        Info.SyncInfoToData(MainForm.DataProcess, filterPos);
      end;
    end;
    Result:= TRUE;
  end;

begin
  if MainForm.cbSelectManga.ItemIndex < 0 then exit;
 // if NOT MainForm.vtMangaList.Focused then exit;

  // ---------------------------------------------------

  if NOT GetMangaInfo(link, website) then
  begin
    Synchronize(CallMainFormCannotGetInfo);
    exit;
  end;

  cover.Clear;
  if (OptionEnableLoadCover) AND (Pos('http://', Info.mangaInfo.coverLink) > 0) then
    boolResult:= GetPage(TObject(cover), Info.mangaInfo.coverLink, 1, TRUE)
  else
    boolResult:= FALSE;
  Synchronize(CallMainFormGetInfos);
  isGetInfos:= FALSE;
end;

procedure   TSubThread.CallMainFormCannotGetInfo;
begin
  MessageDlg('', stDlgCannotGetMangaInfo,
               mtInformation, [mbYes], 0);
  MainForm.rmInformation.Clear;
  MainForm.itAnimate.Enabled:= FALSE;
  MainForm.pbWait.Visible:= FALSE;
  isGetInfos:= FALSE;
end;

procedure   TSubThread.CallMainFormShowLog;
begin
  Log:= TLog.Create(MainForm);
  Log.ShowModal;
  Log.Free;
end;

procedure   TSubThread.CallMainFormGetInfos;
begin
  TransferMangaInfo(MainForm.mangaInfo, Info.mangaInfo);
  MainForm.ShowInformation;
  if boolResult then
  begin
    try
      MainForm.imCover.Picture.Assign(cover);
    except
      cover.Clear;
    end;
  end;
  Info.Free;
end;

procedure   TSubThread.CallMainFormUpdate;
var
  Process: TProcess;
begin
  if MessageDlg('', Format(stDlgNewVersion + #10#13#10#13 + fNote, [LVersion, LRevision]),
                    mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    CopyFile(oldDir + 'updater.exe', oldDir + 'old_updater.exe');
    CopyFile(oldDir + CONFIG_FOLDER + CONFIG_FILE,
             oldDir + CONFIG_FOLDER + CONFIG_FILE + '.tmp');
    Process:= TProcess.Create(nil);
    Process.CommandLine:= oldDir + 'old_updater.exe 1';
    MainForm.CloseNow;
    Process.Execute;
    Process.Free;
    Halt;
  end
  else
    MainForm.btCheckVersion.Caption:= stUpdaterCheck;
end;

procedure   TSubThread.CallMainFormUpdateRequire;
begin
  if MessageDlg('', Format(stDlgUpdaterVersionRequire, [LRequireRevision]), mtInformation, [mbYes, mbNo], 0)=mrYes then
  begin
    OpenURL('http://akarink.wordpress.com/');
  end;
end;

procedure   TSubThread.CallMainFormImportant;
var
  Process: TProcess;
begin
  MessageDlg('', fImportant, mtInformation, [mbYes], 0);
 // MainForm.CloseNow;
 // Halt;
end;

procedure   TSubThread.CallMainFormLatestVer;
begin
  MessageDlg('', stDlgLatestVersion, mtInformation, [mbYes], 0);
end;

procedure   TSubThread.CallMainFormSetButton;
begin
  MainForm.btCheckVersion.Caption:= stUpdaterCheck;
end;

procedure   TSubThread.CallMainFormPopSilentThreadQueue;
var
  meta: TSilentThreadMeta;
begin
  if silentthreads.SilentThreadQueue.Count = 0 then
    exit;
  meta:= TSilentThreadMeta(silentthreads.SilentThreadQueue.Pop);
  if meta <> nil then
  begin
    meta.Run;
    meta.Free;
  end;
end;

procedure   TSubThread.Execute;
var
  l: TStringList;
  i: Cardinal;
  s: String;
begin
  LRevision:= 0;
  while isSuspended do Sleep(32);
  Sleep(2000);
  if FileExists(WORK_FOLDER + LOG_FILE) then
    Synchronize(CallMainFormShowLog);
  if FileExists(oldDir + 'old_updater.exe') then
    DeleteFile(oldDir + 'old_updater.exe');
  Sleep(2000);
  if OptionAutoCheckFavStartup then
  begin
    MainForm.favorites.isAuto:= TRUE;
    MainForm.favorites.isShowDialog:= MainForm.cbOptionShowFavoriteDialog.Checked;
    MainForm.favorites.Run;
  end;
  while NOT Terminated do
  begin
    if ((MainForm.silentThreadCount > 0)) AND
       (MainForm.currentActiveSilentThreadCount < 2) then
    begin
      Synchronize(CallMainFormPopSilentThreadQueue);
    end;

    if isCheckForLatestVer then
    begin
      Sleep(2000);
      l:= TStringList.Create;

      l.NameValueSeparator:= '=';
      case Random(2) of
        0:
          s:= UPDATE_URL + 'updates.i';
        1:
          s:= SourceForgeURL('http://sourceforge.net/projects/fmd/files/FMD/updates/updates.i/download');
      end;
      if (GetPage(TObject(l), s, 0)) AND (l.Count > 0) then
      begin
        fNote:= '';
        fNoteForThisRevision:= '';
        for i:= 0 to l.Count-1 do
        begin
          if l.Names[i] = IntToStr(Revision) then
            fNoteForThisRevision:= l.ValueFromIndex[i];
          if l.Names[i] = 'Note' then
            fNote:= l.ValueFromIndex[i];
          if l.Names[i] = 'Revision' then
            LRevision:= StrToInt(l.ValueFromIndex[i]);
          if l.Names[i] = 'RequireRevision' then
            LRequireRevision:= StrToInt(l.ValueFromIndex[i]);
          if l.Names[i] = 'Version' then
            LVersion:= l.ValueFromIndex[i];
          if l.Names[i] = 'Important' then
          begin
            fImportant:= l.ValueFromIndex[i];
            Synchronize(CallMainFormImportant);
          end;
        end;

        if LRequireRevision > Revision then
          Synchronize(CallMainFormUpdateRequire)
        else
        if LRevision > Revision then
        begin
          Synchronize(CallMainFormUpdate);
        end
        else
        begin
          if updateCounter > 0 then
          begin
            Synchronize(CallMainFormLatestVer);
          end;
          Synchronize(CallMainFormSetButton);
        end;
      end;
      Inc(updateCounter);

      isCheckForLatestVer:= FALSE;
      l.Free;
    end;

    isCanStop:= FALSE;
    if isGetInfos then
    begin
      Info:= TMangaInformation.Create;
      DoGetInfos;
    end;
    isCanStop:= TRUE;
    Sleep(64);
  end;
end;

end.

