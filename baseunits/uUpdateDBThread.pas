{
        File: uUpdateDBThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateDBThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, uBaseUnit, uMisc, SimpleTranslator, FMDOptions,
  LazFileUtils;

type

  { TUpdateDBThread }

  TUpdateDBThread = class(TThread)
  protected
    procedure MainThreadShowGetting;
    procedure MainThreadShowEndGetting;
    procedure MainThreadCannotConnectToServer;
    procedure MainThreadRefreshList;
    procedure ExtractFile;
    procedure Execute; override;
  public
    websiteName: String;
    isTerminated, isSuspended: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  frmMain, FMDVars, Dialogs, ComCtrls;

procedure TUpdateDBThread.MainThreadRefreshList;
begin
  try
    if MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] = websiteName then
    begin
      MainForm.edMangaListSearch.Clear;
      MainForm.vtMangaList.Clear;
      dataProcess.Close;
      ExtractFile;
      MainForm.OpenDataDB(websiteName);
    end
    else
    begin
      if dataProcess.WebsiteLoaded(websiteName) then
        dataProcess.RemoveFilter;
      ExtractFile;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;

  MainThreadShowEndGetting;
end;

procedure TUpdateDBThread.ExtractFile;
var
  Sza, datapath, filepath: String;
begin
  Sza := FMD_DIRECTORY + ZIP_EXE;
  if not FileExistsUTF8(Sza) then Exit;

  datapath := DATA_FOLDER;
  filepath := datapath + websiteName;
  if FileExistsUTF8(filepath + '.7z') then
     filepath += '.7z'
  else
  if FileExistsUTF8(filepath + '.zip') then
    filepath += '.zip';

  if FileExistsUTF8(filepath) then
  begin
    if FileExistsUTF8(datapath + websiteName + DBDATA_EXT) then
      DeleteFileUTF8(datapath + websiteName + DBDATA_EXT);
    if FileExistsUTF8(datapath + websiteName + DATA_EXT) then
      DeleteFileUTF8(datapath + websiteName + DATA_EXT);
    RunExternalProcess(Sza, ['x', filepath, '-o' +
      AnsiQuotedStr(datapath, '"'), '-aoa'], False, True);
    DeleteFileUTF8(filepath);
  end
end;

constructor TUpdateDBThread.Create;
begin
  inherited Create(True);
end;

destructor TUpdateDBThread.Destroy;
begin
  isTerminated := True;
  inherited Destroy;
end;

procedure TUpdateDBThread.MainThreadShowGetting;
begin
  if MainForm.sbUpdateList.Visible = False then
  begin
    MainForm.sbUpdateList.Height := 23;
    MainForm.sbMain.Hide;
    MainForm.sbUpdateList.Show;
    mainForm.sbUpdateList.Panels[0].Style := psText;
    MainForm.btAbortUpdateList.Hide;
    MainForm.sbMain.Show;
  end;
  MainForm.sbMain.SizeGrip := not MainForm.sbUpdateList.Visible;
  MainForm.sbUpdateList.Panels[0].Text := 'Getting list for ' + websiteName + ' ...';
end;

procedure TUpdateDBThread.MainThreadShowEndGetting;
begin
  MainForm.sbUpdateList.Panels[0].Text := '';
  MainForm.sbUpdateList.Hide;
  MainForm.sbMain.SizeGrip := not MainForm.sbUpdateList.Visible;
  isUpdating := False;
end;

procedure TUpdateDBThread.MainThreadCannotConnectToServer;
begin
  MessageDlg('', RS_DlgCannotConnectToServer, mtInformation, [mbYes], 0);
end;

procedure TUpdateDBThread.Execute;
begin
  try
    Synchronize(MainThreadShowGetting);
    RunExternalProcess(FMD_DIRECTORY + UPDATER_EXE, ['-r' , '3', '-d',
      GetMangaDatabaseURL(websiteName), '--lang', SimpleTranslator.LastSelected]);
    if FileExistsUTF8(DATA_FOLDER + websiteName + '.7z') or
      FileExistsUTF8(DATA_FOLDER + websiteName + '.zip') then
      Synchronize(MainThreadRefreshList)
    else
      Synchronize(MainThreadShowEndGetting);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
