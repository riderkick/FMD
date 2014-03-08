{
        File: uUpdateDBThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateDBThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Process, uData, uBaseUnit;

type
  TUpdateDBThread = class(TThread)
  protected
    procedure   MainThreadShowGetting;
    procedure   MainThreadShowEndGetting;
    procedure   MainThreadCannotConnectToServer;
    procedure   MainThreadRefreshList;
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
  frmMain, Dialogs, zipper;

procedure   TUpdateDBThread.MainThreadRefreshList;
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
  MainThreadShowEndGetting;
end;

constructor TUpdateDBThread.Create;
begin
  inherited Create(FALSE);
end;

destructor  TUpdateDBThread.Destroy;
begin
  isTerminated:= TRUE;
  inherited Destroy;
end;

procedure   TUpdateDBThread.MainThreadShowGetting;
begin
  MainForm.sbMain.Panels[0].Text:= 'Getting list for ' + websiteName + ' ...';
end;

procedure   TUpdateDBThread.MainThreadShowEndGetting;
begin
  MainForm.sbMain.Panels[0].Text:= '';
  MainForm.isUpdating:= FALSE;
end;

procedure   TUpdateDBThread.MainThreadCannotConnectToServer;
begin
  MessageDlg('', stDlgUpdaterCannotConnectToServer, mtInformation, [mbYes], 0);
end;

procedure   TUpdateDBThread.Execute;
var
  UnZipper: TUnZipper;
  Process : TProcess;
begin
  while isSuspended do Sleep(32);
  Synchronize(MainThreadShowGetting);

  fmdRunAsAdmin('updater.exe', '1 '+GetMangaDatabaseURL(websiteName), TRUE);

 // if SavePage(GetMangaDatabaseURL(websiteName), fmdDirectory + DATA_FOLDER, websiteName + '.zip', 10) then
  if FileExists(fmdDirectory + DATA_FOLDER + websiteName + '.dat') then
  begin
    Synchronize(MainThreadRefreshList);
  end
  else
  begin
    Synchronize(MainThreadCannotConnectToServer);
    Synchronize(MainThreadShowEndGetting);
  end;
end;

end.

