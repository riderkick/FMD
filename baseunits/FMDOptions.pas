unit FMDOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fileinfo, FileUtil, Forms, Graphics, LazFileUtils, LazUTF8;

type

  { TIniFileRun }

  TIniFileRun = class(IniFiles.TMemIniFile)
  private
    FCSLock: TRTLCriticalSection;
    FFileAge: LongInt;
    FRealFileName: String;
  public
    constructor Create(const AFileName: String; AEscapeLineFeeds: Boolean = False); overload; override;
    destructor Destroy; override;
    procedure UpdateFile; override;
  end;

  TFMDDo = (DO_NOTHING, DO_EXIT, DO_POWEROFF, DO_HIBERNATE, DO_UPDATE);

const
  FMD_INSTANCE = '_FreeMangaDownloaderInstance_';
  FMD_TARGETOS  = {$i %FPCTARGETOS%};
  FMD_TARGETCPU = {$i %FPCTARGETCPU%};

  EXPARAM_PATH = '%PATH%';
  EXPARAM_CHAPTER = '%CHAPTER%';
  DEFAULT_EXPARAM = '"' + EXPARAM_PATH + EXPARAM_CHAPTER + '"';

  DEFAULT_MANGA_CUSTOMRENAME = '%MANGA%';
  DEFAULT_CHAPTER_CUSTOMRENAME = '%CHAPTER%';
  DEFAULT_FILENAME_CUSTOMRENAME = '%FILENAME%';

  DATA_EXT = '.dat';
  DBDATA_EXT = '.db';
  DBDATA_SERVER_EXT = '.7z';
  UPDATER_EXE = 'updater.exe';
  OLD_UPDATER_EXE = 'old_' + UPDATER_EXE;
  ZIP_EXE = '7za.exe';
  RUN_EXE = '.run';

  SOCKHEARTBEATRATE = 500;
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  MAX_TASKLIMIT = 16;
  MAX_CONNECTIONPERHOSTLIMIT = 64;
  {$ENDIF}
  {$IFDEF WIN64}
  MAX_TASKLIMIT = 64;
  MAX_CONNECTIONPERHOSTLIMIT = 256;
  {$ENDIF}
  {$ELSE}
  MAX_TASKLIMIT = 8;
  MAX_CONNECTIONPERHOSTLIMIT = 32;
  {$ENDIF}

  BACKUP_FILE_PREFIX = 'fmdbackup_';
  BACKUP_FILE_EXT = '7z';

{$i revision.inc}

var
  FMD_VERSION_NUMBER: TProgramVersion;
  FMD_VERSION_STRING,
  FMD_DIRECTORY,
  FMD_EXENAME,
  CURRENT_UPDATER_EXE,
  OLD_CURRENT_UPDATER_EXE,
  CURRENT_ZIP_EXE,
  APPDATA_DIRECTORY,
  DEFAULT_PATH,
  WORK_FOLDER,
  WORK_FILE,
  WORK_FILEDB,
  DOWNLOADEDCHAPTERS_FILE,
  DOWNLOADEDCHAPTERSDB_FILE,
  FAVORITES_FILE,
  FAVORITESDB_FILE,
  CONFIG_FOLDER,
  CONFIG_FILE,
  REVISION_FILE,
  UPDATE_FILE,
  BASE_FILE,
  ACCOUNTS_FILE,
  MODULES_FILE,
  DATA_FOLDER,
  IMAGE_FOLDER,
  LANGUAGE_FILE,
  CHANGELOG_FILE,
  DEFAULT_LOG_FILE,
  README_FILE,
  EXTRAS_FOLDER,
  MANGAFOXTEMPLATE_FOLDER,
  LUA_WEBSITEMODULE_FOLDER,
  LUA_REPO_FOLDER,
  LUA_REPO_FILE,
  LUA_REPO_WORK_FILE,
  BACKUP_FOLDER: String;

  // ini files
  revisionfile,
  updatesfile: TIniFile;
  configfile: TIniFileRun;

  // base url, should be in base.ini
  DEFAULT_SELECTED_WEBSITES: String = 'MangaDex,MangaHere,MangaInn,MangaReader';
  DB_URL: String = 'https://sourceforge.net/projects/newfmd/files/data/<website>.7z/download';
  UPDATE_URL: String = 'https://raw.githubusercontent.com/fmd-project-team/FMD/master/update';
  CHANGELOG_URL: String = 'https://raw.githubusercontent.com/fmd-project-team/FMD/master/changelog.txt';
  UPDATE_PACKAGE_NAME: String = 'updatepackage.7z';
  MODULES_URL: String = 'https://api.github.com/repos/fmd-project-team/FMD/contents/lua/modules';
  MODULES_URL2: String = 'https://github.com/fmd-project-team/FMD/file-list/master/lua/modules';

  currentWebsite: String;

  // available website
  AvailableWebsites: TStringList;

  // general
  OptionLetFMDDo: TFMDDo = DO_NOTHING;
  OptionDeleteCompletedTasksOnClose: Boolean = False;
  OptionSortDownloadsWhenAddingNewDownloadTasks: Boolean = False;

  // saveto
  OptionChangeUnicodeCharacter: Boolean = False;
  OptionChangeUnicodeCharacterStr: String = '_';
  OptionGenerateMangaFolder: Boolean = False;
  OptionMangaCustomRename: String;
  OptionGenerateChapterFolder: Boolean = True;
  OptionChapterCustomRename: String;
  OptionFilenameCustomRename: String;

  OptionConvertDigitVolume: Boolean;
  OptionConvertDigitChapter: Boolean;
  OptionConvertDigitVolumeLength: Integer;
  OptionConvertDigitChapterLength: Integer;

  OptionPDFQuality: Cardinal = 95;

  OptionPNGSaveAsJPEG: Boolean = False;
  OptionWebPSaveAs: Integer = 1;
  OptionPNGCompressionLevel: Integer = 1;
  OptionJPEGQuality: Integer = 80;

  // connections
  OptionMaxParallel: Integer = 1;
  OptionMaxThreads: Integer = 1;
  OptionMaxRetry: Integer = 5;
  OptionConnectionTimeout: Integer = 30;
  OptionRetryFailedTask: Integer = 1;
  OptionAlwaysStartTaskFromFailedChapters: Boolean = True;
  OptionEnableCloudflareBypass: Boolean = True;
  OptionAutomaticallyDisableCloudflareBypass: Boolean = False;

  // view
  OptionEnableLoadCover: Boolean = False;
  OptionShowBalloonHint: Boolean = True;
  OptionShowFavoritesTabOnNewManga: Boolean = False;
  OptionShowDownloadsTabOnNewTasks: Boolean = True;
  
  // favorites (context menu settings)
  OptionDefaultAction: Integer = 0;

  // updates
  OptionAutoCheckLatestVersion: Boolean = True;
  OptionAutoCheckFavStartup: Boolean = True;
  OptionAutoCheckFavInterval: Boolean = True;
  OptionAutoCheckFavIntervalMinutes: Cardinal = 60;
  OptionNewMangaTime: Integer = 1;
  OptionJDNNewMangaTime: Integer = MaxInt;
  OptionAutoCheckFavDownload: Boolean = False;
  OptionAutoCheckFavRemoveCompletedManga: Boolean = False;
  OptionUpdateListNoMangaInfo: Boolean = False;
  OptionUpdateListRemoveDuplicateLocalData: Boolean = False;

  // modules
  OptionModulesUpdaterShowUpdateWarning: Boolean = True;
  OptionModulesUpdaterAutoRestart: Boolean = False;

  OptionHTTPUseGzip: Boolean = True;

  OptionRemoveMangaNameFromChapter: Boolean = False;

  OptionRestartFMD: Boolean = False;

  //custom color
  //basiclist
  CL_BSNormalText: TColor = clWindowText;
  CL_BSFocusedSelectionText: TColor = clHighlightText;
  CL_BSUnfocesedSelectionText: TColor = clWindowText;
  CL_BSOdd: TColor = clBtnFace;
  CL_BSEven: TColor = clWindow;
  CL_BSSortedColumn: TColor = $F8E6D6;
  CL_BSEnabledWebsiteSettings: TColor = clYellow;

  //mangalist color
  CL_MNNewManga: TColor = $FDC594;
  CL_MNCompletedManga: TColor = $B8FFB8;

  //favoritelist color
  CL_FVBrokenFavorite: TColor = $8080FF;
  CL_FVChecking: TColor = $80EBFE;
  CL_FVNewChapterFound: TColor = $FDC594;
  CL_FVCompletedManga: TColor = $B8FFB8;
  CL_FVEmptyChapters: TColor = $CCDDFF;

  //chapterlist color
  CL_CHDownloaded: TColor = $B8FFB8;

// set base directory
procedure SetFMDdirectory(const ADir: String);
procedure SetAppDataDirectory(const ADir: String);

procedure RestartFMD;
procedure DoRestartFMD;

implementation

uses FMDVars, process, UTF8Process;

{ TIniFileRun }

constructor TIniFileRun.Create(const AFileName: String; AEscapeLineFeeds: Boolean);
begin
  FRealFileName := AFileName;
  if FileExistsUTF8(AFileName + RUN_EXE) then
    DeleteFileUTF8(RUN_EXE);
  if FileExistsUTF8(AFileName) then
    CopyFile(AFileName, AFileName + RUN_EXE);
  InitCriticalSection(FCSLock);
  if FileExistsUTF8(AFileName) then
    FFileAge := FileAgeUTF8(AFileName)
  else
    FFileAge := 0;
  inherited Create(AFileName + RUN_EXE, AEscapeLineFeeds);
end;

destructor TIniFileRun.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(FCSLock);
  if FileExistsUTF8(FileName) then
    DeleteFileUTF8(FileName);
end;

procedure TIniFileRun.UpdateFile;
begin
  if CacheUpdates and (Dirty = False) then Exit;
  EnterCriticalSection(FCSLock);
  try
    inherited UpdateFile;
    CopyFile(FileName, FRealFileName, [cffOverwriteFile, cffPreserveTime, cffCreateDestDirectory]);
  finally
    LeaveCriticalSection(FCSLock);
  end;
end;

procedure FreeNil(var Obj);
begin
  if Pointer(Obj) <> nil then
    TObject(Obj).Free;
  Pointer(Obj) := nil;
end;

procedure FreeIniFiles;
begin
  FreeNil(configfile);
end;

procedure SetIniFiles;
begin
  FreeIniFiles;
  configfile := TIniFileRun.Create(CONFIG_FILE);
end;

procedure ReadBaseFile;
begin
  if not FileExistsUTF8(BASE_FILE) then Exit;
  with TIniFile.Create(BASE_FILE) do
    try
      DEFAULT_SELECTED_WEBSITES:=ReadString('base','DEFAULT_SELECTED_WEBSITES',DEFAULT_SELECTED_WEBSITES);
      DB_URL:=ReadString('base','DB_URL',DB_URL);
      UPDATE_URL:=ReadString('base','UPDATE_URL',UPDATE_URL);
      CHANGELOG_URL:=ReadString('base','CHANGELOG_URL',CHANGELOG_URL);
      UPDATE_PACKAGE_NAME:=ReadString('base','UPDATE_PACKAGE_NAME',UPDATE_PACKAGE_NAME);
      MODULES_URL:=ReadString('base','MODULES_URL',MODULES_URL);
      MODULES_URL2:=ReadString('base','MODULES_URL2',MODULES_URL2);
    finally
      Free;
    end;
end;

procedure SetFMDdirectory(const ADir: String);
begin
  FMD_DIRECTORY := CleanAndExpandDirectory(ADir);
  FMD_EXENAME := ExtractFileNameOnly(Application.ExeName);

  CONFIG_FOLDER := FMD_DIRECTORY + 'config' + PathDelim;
  REVISION_FILE := CONFIG_FOLDER + 'revision.ini';
  UPDATE_FILE := CONFIG_FOLDER + 'updates.ini';
  BASE_FILE := CONFIG_FOLDER + 'base.ini';

  IMAGE_FOLDER := FMD_DIRECTORY + 'images' + PathDelim;
  LANGUAGE_FILE := FMD_DIRECTORY + 'languages.ini';
  CHANGELOG_FILE := FMD_DIRECTORY + 'changelog.txt';
  README_FILE := FMD_DIRECTORY + 'readme.rtf';
  EXTRAS_FOLDER := FMD_DIRECTORY + 'extras' + PathDelim;
  MANGAFOXTEMPLATE_FOLDER := EXTRAS_FOLDER + 'mangafoxtemplate' + PathDelim;
  DEFAULT_LOG_FILE := FMD_EXENAME + '.log';
  CURRENT_UPDATER_EXE := FMD_DIRECTORY + UPDATER_EXE;
  OLD_CURRENT_UPDATER_EXE := FMD_DIRECTORY + OLD_UPDATER_EXE;
  CURRENT_ZIP_EXE := FMD_DIRECTORY + ZIP_EXE;

  BACKUP_FOLDER := FMD_DIRECTORY + 'backup' + PathDelim;

  ReadBaseFile;
end;

procedure SetAppDataDirectory(const ADir: String);
begin
  APPDATA_DIRECTORY := CleanAndExpandDirectory(ADir);

  DEFAULT_PATH := 'downloads' + PathDelim;

  CONFIG_FOLDER := APPDATA_DIRECTORY + 'config' + PathDelim;
  CONFIG_FILE := CONFIG_FOLDER + 'config.ini';
  ACCOUNTS_FILE := CONFIG_FOLDER + 'accounts.db';
  MODULES_FILE := CONFIG_FOLDER + 'modules.json';
  LUA_REPO_FILE := CONFIG_FOLDER + 'lua.json';
  LUA_REPO_WORK_FILE := CONFIG_FOLDER + 'lua_repo.json';

  DATA_FOLDER := APPDATA_DIRECTORY + 'data' + PathDelim;

  WORK_FOLDER := APPDATA_DIRECTORY + 'works' + PathDelim;
  WORK_FILE := WORK_FOLDER + 'works.ini';
  WORK_FILEDB := WORK_FOLDER + 'downloads.db';
  DOWNLOADEDCHAPTERS_FILE := WORK_FOLDER + 'downloadedchapters.ini';
  DOWNLOADEDCHAPTERSDB_FILE := WORK_FOLDER + 'downloadedchapters.db';
  FAVORITES_FILE := WORK_FOLDER + 'favorites.ini';
  FAVORITESDB_FILE := WORK_FOLDER + 'favorites.db';

  LUA_WEBSITEMODULE_FOLDER := FMD_DIRECTORY + 'lua' + PathDelim + 'modules' + PathDelim;
  LUA_REPO_FOLDER := FMD_DIRECTORY + 'lua' + PathDelim;

  SetIniFiles;
end;

procedure RestartFMD;
begin
  OptionRestartFMD := True;
  FormMain.Close;
end;

procedure DoRestartFMD;
var
  p: TProcessUTF8;
begin
  p := TProcessUTF8.Create(nil);
  try
    p.InheritHandles := False;
    p.CurrentDirectory := FMD_DIRECTORY;
    p.Executable := Application.ExeName;
    p.Options := [];
    p.InheritHandles := False;
    p.Parameters.AddStrings(AppParams);
    {$ifdef windows}
    p.Parameters.Add('--dorestart-handle=' + IntToStr(Integer(Application.Handle)));
    {$ifend}
    p.Execute;
  finally
    p.Free;
  end;
end;

procedure doInitialization;
begin
  GetProgramVersion(FMD_VERSION_NUMBER);
  FMD_VERSION_STRING := ProgramversionToStr(FMD_VERSION_NUMBER);
  AvailableWebsites := TStringList.Create;
  AvailableWebsites.Sorted := False;
  SetFMDdirectory(ExtractFilePath(Application.ExeName));
  SetAppDataDirectory(FMD_DIRECTORY);
end;

procedure doFinalization;
begin
  FreeIniFiles;
  AvailableWebsites.Free;
end;

initialization
  doInitialization;

finalization
  doFinalization;

end.
