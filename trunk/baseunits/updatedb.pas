{
        File: updatedb.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit updatedb;

{$mode delphi}

interface

uses
  Classes, SysUtils, data, baseunit, FileUtil, Process;

type
  TUpdateDBThread = class(TThread)
  protected
    procedure   CallMainFormShowGetting;
    procedure   CallMainFormShowEndGetting;
    procedure   CallMainFormCannotConnectToServer;
    procedure   CallMainFormRefreshList;
    procedure   Execute; override;
  public
    websiteName  : String;
    isTerminated,
    isSuspended  : Boolean;
    constructor Create;
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit, Dialogs, zipper;

procedure   TUpdateDBThread.CallMainFormRefreshList;
begin
  if MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] = websiteName then
  begin
   // MainForm.dataProcess.RemoveFilter;
    MainForm.dataProcess.Free;
    MainForm.dataProcess:= TDataProcess.Create;
    MainForm.dataProcess.LoadFromFile(websiteName);
    MainForm.vtMangaList.Clear;
    MainForm.vtMangaList.RootNodeCount:= MainForm.dataProcess.filterPos.Count;
    MainForm.lbMode.Caption:= Format(stModeAll, [MainForm.dataProcess.filterPos.Count]);
  end;
  CallMainFormShowEndGetting;
end;

constructor TUpdateDBThread.Create;
begin
  inherited Create(FALSE);
  isSuspended:= TRUE;
  isTerminated:= FALSE;
  FreeOnTerminate:= TRUE;
end;

destructor  TUpdateDBThread.Destroy;
begin
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TUpdateDBThread.CallMainFormShowGetting;
begin
  MainForm.sbMain.Panels[0].Text:= 'Getting list for ' + websiteName + ' ...';
end;

procedure   TUpdateDBThread.CallMainFormShowEndGetting;
begin
  MainForm.sbMain.Panels[0].Text:= '';
  MainForm.isUpdating:= FALSE;
end;

procedure   TUpdateDBThread.CallMainFormCannotConnectToServer;
begin
  MessageDlg('', stDlgUpdaterCannotConnectToServer, mtInformation, [mbYes], 0);
end;

procedure   TUpdateDBThread.Execute;
var
  UnZipper: TUnZipper;
  Process : TProcess;
begin
  while isSuspended do Sleep(32);
  Synchronize(CallMainFormShowGetting);

  Process:= TProcess.Create(nil);
  Process.CommandLine:= 'updater 1 '+GetMangaDatabaseURL(websiteName);
  Process.Options:= Process.Options + [poWaitOnExit];
  Process.Execute;
  Process.Free;


 // if SavePage(GetMangaDatabaseURL(websiteName), oldDir + DATA_FOLDER, websiteName + '.zip', 10) then
  if FileExists(oldDir + DATA_FOLDER + websiteName + '.dat') then
  begin
    {UnZipper:= TUnZipper.Create;
    try
      UnZipper.FileName  := websiteName + '.zip';
      UnZipper.OutputPath:= ExtractFilePath(oldDir + DATA_FOLDER + websiteName + '.zip');
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
    finally
      UnZipper.Free;
    end;
    Sleep(32);
    DeleteFile(oldDir + DATA_FOLDER + websiteName + '.zip');
    Sleep(32);}

    Synchronize(CallMainFormRefreshList);
  end
  else
  begin
    Synchronize(CallMainFormCannotConnectToServer);
    Synchronize(CallMainFormShowEndGetting);
  end;
end;

end.

