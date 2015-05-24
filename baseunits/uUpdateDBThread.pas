{
        File: uUpdateDBThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateDBThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, uData, uBaseUnit, uTranslation;

type
  TUpdateDBThread = class(TThread)
  protected
    procedure MainThreadShowGetting;
    procedure MainThreadShowEndGetting;
    procedure MainThreadCannotConnectToServer;
    procedure MainThreadRefreshList;
    procedure Execute; override;
  public
    websiteName: String;
    isTerminated, isSuspended: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  frmMain, Dialogs, ComCtrls;

procedure TUpdateDBThread.MainThreadRefreshList;
begin
  try
    if MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex] = websiteName then
    begin
      MainForm.edSearch.Clear;
      MainForm.dataProcess.RemoveFilter;
      MainForm.dataProcess.Free;
      MainForm.dataProcess := TDataProcess.Create;
      MainForm.dataProcess.LoadFromFile(websiteName);
      MainForm.vtMangaList.Clear;
      MainForm.vtMangaList.RootNodeCount := MainForm.dataProcess.filterPos.Count;
      MainForm.lbMode.Caption :=
        Format(RS_ModeAll, [MainForm.dataProcess.filterPos.Count]);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;

  MainThreadShowEndGetting;
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
  MainForm.isUpdating := False;
end;

procedure TUpdateDBThread.MainThreadCannotConnectToServer;
begin
  MessageDlg('', RS_DlgCannotConnectToServer, mtInformation, [mbYes], 0);
end;

procedure TUpdateDBThread.Execute;
begin
  try
    Synchronize(MainThreadShowGetting);
    {$IFDEF USEADMIN}
    fmdRunAsAdmin(fmdDirectory + 'updater.exe', '-x -r 3 -d ' +
      GetMangaDatabaseURL(websiteName) + ' --lang ' + uTranslation.LastSelected, True);
    {$ELSE}
    RunExternalProcess(fmdDirectory + 'updater.exe', ['-x', '-r' , '3', '-d',
      GetMangaDatabaseURL(websiteName), '--lang', uTranslation.LastSelected]);
    {$ENDIF}
    if FileExists(fmdDirectory + DATA_FOLDER + websiteName + '.dat') then
    begin
      Synchronize(MainThreadRefreshList);
    end
    else
    begin
      Synchronize(MainThreadShowEndGetting);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
