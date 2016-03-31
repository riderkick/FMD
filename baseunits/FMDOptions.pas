unit FMDOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fileinfo, LazFileUtils, LazUTF8, FileUtil, Forms, LCLProc;

type

  { TIniFileRun }

  TIniFileRun = class(IniFiles.TMemIniFile)
  private
    FCSLock: TRTLCriticalSection;
    FFileAge: Longint;
    FRealFileName: String;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds: Boolean = False); overload; override;
    destructor Destroy; override;
    procedure UpdateFile; override;
    procedure Reload;
  end;

const
  FMD_REVISION = '$WCREV$';
  FMD_INSTANCE = '_FreeMangaDownloaderInstance_';
  FMD_TARGETOS  = {$i %FPCTARGETOS%};
  FMD_TARGETCPU = {$i %FPCTARGETCPU%};

  EXPARAM_PATH = '%PATH%';
  EXPARAM_CHAPTER = '%CHAPTER%';
  DEFAULT_EXPARAM = '"' + EXPARAM_PATH + EXPARAM_CHAPTER + '"';

  SOCKHEARTBEATRATE = 400;

  DEFAULT_LIST = 'AnimeA,MangaFox,MangaHere,MangaInn,MangaReader';
  DEFAULT_MANGA_CUSTOMRENAME = '%MANGA%';
  DEFAULT_CHAPTER_CUSTOMRENAME = '%CHAPTER%';

  DATA_EXT = '.dat';
  DBDATA_EXT = '.db';
  UPDATER_EXE = 'updater.exe';
  ZIP_EXE = '7za.exe';
  RUN_EXE = '.run';

var
  FMD_VERSION_NUMBER,
  FMD_DIRECTORY,
  APPDATA_DIRECTORY,
  DEFAULT_PATH,
  WORK_FOLDER,
  WORK_FILE,
  DOWNLOADEDCHAPTERS_FILE,
  DOWNLOADEDCHAPTERSDB_FILE,
  FAVORITES_FILE,
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

// set base directory
procedure SetFMDdirectory(const ADir: String);
procedure SetAppDataDirectory(const ADir: String);

implementation

{ TIniFileRun }

constructor TIniFileRun.Create(const AFileName: string; AEscapeLineFeeds: Boolean);
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
  inherited UpdateFile;
  CopyFile(FileName, FRealFileName, [cffOverwriteFile, cffPreserveTime, cffCreateDestDirectory]);
end;

procedure TIniFileRun.Reload;
var
  s: TStringList;
begin
  if TryEnterCriticalsection(FCSLock) > 0 then
    try
      if FileExistsUTF8(FileName) then
        if FileAgeUTF8(FileName) <> FFileAge then
        begin
          s := TStringList.Create;
          try
            FFileAge := FileAge(FileName);
            s.LoadFromFile(FileName);
            SetStrings(s);
          finally
            s.Free;
          end;
        end;
    finally
      LeaveCriticalsection(FCSLock);
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

procedure SetIniFiles;
begin
  FreeIniFiles;
  mangalistfile := TIniFile.Create(MANGALIST_FILE);
  configfile := TIniFileRun.Create(CONFIG_FILE);
  configfile.Options := configfile.Options - [ifoStripQuotes];
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

  DEFAULT_PATH := APPDATA_DIRECTORY + 'downloads' + PathDelim;

  CONFIG_FOLDER := APPDATA_DIRECTORY + 'config' + PathDelim;
  CONFIG_FILE := CONFIG_FOLDER + 'config.ini';
  CONFIG_ADVANCED := CONFIG_FOLDER + 'advanced.ini';
  ACCOUNTS_FILE := CONFIG_FOLDER + 'accounts.db';
  WEBSITE_CONFIG_FILE := CONFIG_FOLDER + 'websiteconfig.ini';

  DATA_FOLDER := APPDATA_DIRECTORY + 'data' + PathDelim;

  WORK_FOLDER := APPDATA_DIRECTORY + 'works' + PathDelim;
  WORK_FILE := WORK_FOLDER + 'works.ini';
  DOWNLOADEDCHAPTERS_FILE := WORK_FOLDER + 'downloadedchapters.ini';
  DOWNLOADEDCHAPTERSDB_FILE := WORK_FOLDER + 'downloadedchapters.db';
  FAVORITES_FILE := WORK_FOLDER + 'favorites.ini';

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
  SetFMDdirectory(GetCurrentDirUTF8);
  SetAppDataDirectory(GetCurrentDirUTF8);
end;

procedure doFinalization;
begin
  FreeIniFiles;
end;

initialization
  doInitialization;

finalization
  doFinalization;

end.
