unit FMDVars;

{$mode objfpc}{$H+}

interface

uses
  frmMain, uDownloadsManager, uFavoritesManager, uUpdateThread, DBDataProcess,
  uUpdateDBThread, uSilentThread, uBaseUnit, uGetMangaInfosThread, CheckUpdate,
  FMDOptions, FileChannel, simpleipc;

var
  FormMain: TMainForm;

  isStartup,
  isRunDownloadFilter,
  isUpdating,
  isPendingExitCounter,
  isNormalExit: Boolean;

  //Instance
  FMDInstance: TSimpleIPCServer;

  // update fmd through main thread
  DoAfterFMD: TFMDDo;
  IsDlgCounter: Boolean = False;
  UpdateURL: String;

  // file logger
  FileLogger: TFileChannel;

  // status in status bar update
  ulTotalPtr,
  ulWorkPtr: Integer;

  // download manager
  DLManager: TDownloadManager;

  // favorite manager
  FavoriteManager: TFavoriteManager;

  // main dataprocess for manga list
  dataProcess: TDBDataProcess;

  // update manga list thread manager
  updateList: TUpdateListManagerThread;

  // updateDB thread
  updateDB: TUpdateDBThread;

  // silent thread for download all or add to favorite
  SilentThreadManager: TSilentThreadManager;

  // get manga info
  mangaInfo: TMangaInfo;
  GetInfosThread: TGetMangaInfosThread;

  // check update thread
  CheckUpdateThread: TCheckUpdateThread;

implementation

end.

