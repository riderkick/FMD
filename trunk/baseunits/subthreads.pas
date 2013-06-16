{
        File: subthreads.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
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
    website, link: String;
    isGetInfos   : Boolean;
    Info         : TMangaInformation;
    boolResult   : Boolean;

    constructor Create;
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit, logform, fileutil;

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

    if mangaListPos <> -1 then
    begin
      filterPos:= MainForm.dataProcess.filterPos.Items[mangaListPos];
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
    if mangaListPos <> -1 then
    begin
      if website = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] then
        Info.SyncInfoToData(MainForm.DataProcess, filterPos);
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
  boolResult:= GetPage(TObject(cover), Info.mangaInfo.coverLink, 1);
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
  Log.ShowLog;
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
    Process:= TProcess.Create(nil);
    Process.CommandLine:= oldDir + 'old_updater.exe 1';
    Process.Execute;
    MainForm.CloseNow;
    Halt;
  end
  else
    MainForm.btCheckVersion.Caption:= stUpdaterCheck;
end;

procedure   TSubThread.CallMainFormUpdateRequire;
begin
  if MessageDlg('', Format(stDlgUpdaterVersionRequire, [LRequireRevision]), mtInformation, [mbYes, mbNo], 0)=mrYes then
  begin
    OpenURL('https://sourceforge.net/projects/fmd/');
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

procedure   TSubThread.Execute;
var
  l: TStringList;
  i: Cardinal;

begin
  LRevision:= 0;
  while isSuspended do Sleep(32);
  Sleep(2000);
  if FileExists(WORK_FOLDER + LOG_FILE) then
    Synchronize(CallMainFormShowLog);
  if FileExists(oldDir + 'old_updater.exe') then
    DeleteFile(oldDir + 'old_updater.exe');
  Sleep(1000);
  if OptionAutoCheckFavStartup then
    MainForm.favorites.Run;
  while NOT Terminated do
  begin
    if isCheckForLatestVer then
    begin
      Sleep(2000);
      l:= TStringList.Create;

      l.NameValueSeparator:= '=';
      if (GetPage(TObject(l), UPDATE_URL + 'version.txt', 0)) AND (l.Count > 0) then
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

