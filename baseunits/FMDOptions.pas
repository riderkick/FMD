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
  FMD_REVISION = '$WCREV$';
  FMD_INSTANCE = '_FreeMangaDownloaderInstance_';
  FMD_TARGETOS  = {$i %FPCTARGETOS%};
  FMD_TARGETCPU = {$i %FPCTARGETCPU%};

  EXPARAM_PATH = '%PATH%';
  EXPARAM_CHAPTER = '%CHAPTER%';
  DEFAULT_EXPARAM = '"' + EXPARAM_PATH + EXPARAM_CHAPTER + '"';

  DEFAULT_LIST = 'AnimeA,MangaFox,MangaHere,MangaInn,MangaReader';
  DEFAULT_MANGA_CUSTOMRENAME = '%MANGA%';
  DEFAULT_CHAPTER_CUSTOMRENAME = '%CHAPTER%';
  DEFAULT_FILENAME_CUSTOMRENAME = '%FILENAME%';

  DATA_EXT = '.dat';
  DBDATA_EXT = '.db';
  UPDATER_EXE = 'updater.exe';
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

var
  FMD_VERSION_NUMBER,
  FMD_DIRECTORY,
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
  CONFIG_ADVANCED,
  REVISION_FILE,
  UPDATE_FILE,
  MANGALIST_FILE,
  ACCOUNTS_FILE,
  WEBSITE_CONFIG_FILE,
  DATA_FOLDER,
  IMAGE_FOLDER,
  LANGUAGE_FILE,
  CHANGELOG_FILE,
  README_FILE,
  EXTRAS_FOLDER,
  MANGAFOXTEMPLATE_FOLDER: String;

  // ini files
  revisionfile,
  updatesfile,
  mangalistfile: TIniFile;
  configfile,
  advancedfile: TIniFileRun;

  // db data download url
  DBDownloadURL: String;

  currentWebsite: String;

  // available website
  AvailableWebsite: TStringList;

  OptionLetFMDDo: TFMDDo = DO_NOTHING;

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

  // connections
  OptionMaxParallel: Integer = 1;
  OptionMaxThreads: Integer = 1;
  OptionMaxRetry: Integer = 5;
  OptionConnectionTimeout: Integer = 30;
  OptionRetryFailedTask: Integer = 1;
  OptionAlwaysStartTaskFromFailedChapters: Boolean = True;

  // view
  OptionEnableLoadCover: Boolean = False;
  OptionShowBalloonHint: Boolean = True;

  // updates
  OptionAutoCheckLatestVersion: Boolean = True;
  OptionAutoCheckFavStartup: Boolean = True;
  OptionAutoCheckFavInterval: Boolean = True;
  OptionAutoCheckFavIntervalMinutes: Cardinal = 60;
  OptionNewMangaTime: Cardinal = 1;
  OptionAutoCheckFavDownload: Boolean = False;
  OptionAutoCheckFavRemoveCompletedManga: Boolean = False;
  OptionUpdateListNoMangaInfo: Boolean = False;
  OptionUpdateListRemoveDuplicateLocalData: Boolean = False;

  OptionHTTPUseGzip: Boolean = True;

  OptionRemoveMangaNameFromChapter: Boolean = False;

  //custom color
  //basiclist
  CL_BSNormalText: TColor = clWindowText;
  CL_BSFocusedSelectionText: TColor = clHighlightText;
  CL_BSUnfocesedSelectionText: TColor = clWindowText;
  CL_BSOdd: TColor = clBtnFace;
  CL_BSEven: TColor = clWindow;
  CL_BSSortedColumn: TColor = $F8E6D6;

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

implementation

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
  inherited UpdateFile;
  try
    CopyFile(FileName, FRealFileName, [cffOverwriteFile, cffPreserveTime, cffCreateDestDirectory]);
  except
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
  FreeNil(mangalistfile);
  FreeNil(configfile);
  FreeNil(advancedfile);
end;

procedure GetAvailableWebsite;
var
  l, w: TStringList;
  i, j: Integer;
begin
  AvailableWebsite.Clear;
  AvailableWebsite.BeginUpdate;
  try
    l := TStringList.Create;
    try
      mangalistfile.ReadSection('available', l);
      if l.Count > 0 then
      begin
        w := TStringList.Create;
        try
          for i := 0 to l.Count - 1 do
          begin
            w.Clear;
            w.CommaText := mangalistfile.ReadString('available', l[i], '');
            if w.Count > 0 then
              for j := 0 to w.Count - 1 do
                AvailableWebsite.Values[w[j]] := l[i];
          end;
        finally
          w.Free;
        end;
      end;
    finally
      l.Free;
    end;
  finally
    AvailableWebsite.EndUpdate;
  end;
end;

procedure SetIniFiles;
begin
  FreeIniFiles;
  mangalistfile := TIniFile.Create(MANGALIST_FILE);
  GetAvailableWebsite;
  configfile := TIniFileRun.Create(CONFIG_FILE);
  advancedfile := TIniFileRun.Create(CONFIG_ADVANCED);
end;

procedure SetFMDdirectory(const ADir: String);
begin
  FMD_DIRECTORY := CleanAndExpandDirectory(ADir);

  CONFIG_FOLDER := FMD_DIRECTORY + 'config' + PathDelim;
  REVISION_FILE := CONFIG_FOLDER + 'revision.ini';
  UPDATE_FILE := CONFIG_FOLDER + 'updates.ini';
  MANGALIST_FILE := CONFIG_FOLDER + 'mangalist.ini';

  IMAGE_FOLDER := FMD_DIRECTORY + 'images' + PathDelim;
  LANGUAGE_FILE := FMD_DIRECTORY + 'languages.ini';
  CHANGELOG_FILE := FMD_DIRECTORY + 'changelog.txt';
  README_FILE := FMD_DIRECTORY + 'readme.rtf';
  EXTRAS_FOLDER := FMD_DIRECTORY + 'extras' + PathDelim;
  MANGAFOXTEMPLATE_FOLDER := EXTRAS_FOLDER + 'mangafoxtemplate' + PathDelim;
end;

procedure SetAppDataDirectory(const ADir: String);
begin
  APPDATA_DIRECTORY := CleanAndExpandDirectory(ADir);

  DEFAULT_PATH := 'downloads' + PathDelim;

  CONFIG_FOLDER := APPDATA_DIRECTORY + 'config' + PathDelim;
  CONFIG_FILE := CONFIG_FOLDER + 'config.ini';
  CONFIG_ADVANCED := CONFIG_FOLDER + 'advanced.ini';
  ACCOUNTS_FILE := CONFIG_FOLDER + 'accounts.db';
  WEBSITE_CONFIG_FILE := CONFIG_FOLDER + 'websiteconfig.ini';

  DATA_FOLDER := APPDATA_DIRECTORY + 'data' + PathDelim;

  WORK_FOLDER := APPDATA_DIRECTORY + 'works' + PathDelim;
  WORK_FILE := WORK_FOLDER + 'works.ini';
  WORK_FILEDB := WORK_FOLDER + 'downloads.db';
  DOWNLOADEDCHAPTERS_FILE := WORK_FOLDER + 'downloadedchapters.ini';
  DOWNLOADEDCHAPTERSDB_FILE := WORK_FOLDER + 'downloadedchapters.db';
  FAVORITES_FILE := WORK_FOLDER + 'favorites.ini';
  FAVORITESDB_FILE := WORK_FOLDER + 'favorites.db';

  SetIniFiles;
end;

function GetCurrentBinVersion: String;
var
  AppVerInfo: TStringList;
  i: Integer;
begin
  Result := '';
  AppVerInfo := TStringList.Create;
  with TFileVersionInfo.Create(nil) do
    try
      try
        FileName := ParamStrUTF8(0);
        if FileName = '' then
          FileName := Application.ExeName;
        {$IF FPC_FULLVERSION >= 20701}
        ReadFileInfo;
        {$ENDIF}
        if VersionStrings.Count > 0 then
        begin
        {$IF FPC_FULLVERSION >= 20701}
          AppVerInfo.Assign(VersionStrings);
        {$ELSE}
          for i := 0 to VersionStrings.Count - 1 do
            AppVerInfo.Add(VersionCategories.Strings[i] + '=' +
              VersionStrings.Strings[i]);
        {$ENDIF}
          for i := 0 to AppVerInfo.Count - 1 do
            AppVerInfo.Strings[i] := LowerCase(AppVerInfo.Names[i]) + '=' + AppVerInfo.ValueFromIndex[i];
          Result := AppVerInfo.Values['fileversion'];
        end;
      except
      end;
    finally
      Free;
      AppVerInfo.Free;
    end;
end;

procedure doInitialization;
begin
  FMD_VERSION_NUMBER := GetCurrentBinVersion;
  AvailableWebsite := TStringList.Create;
  AvailableWebsite.Sorted := True;
  SetFMDdirectory(GetCurrentDirUTF8);
  SetAppDataDirectory(GetCurrentDirUTF8);
end;

procedure doFinalization;
begin
  FreeIniFiles;
  AvailableWebsite.Free;
end;

initialization
  doInitialization;

finalization
  doFinalization;

end.
