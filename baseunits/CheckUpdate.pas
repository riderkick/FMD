{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit CheckUpdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, uBaseUnit, FMDOptions, httpsendthread,
  BaseThread;

type

  TCheckUpdateThread = class(TBaseThread)
  private
    FHTTP: THTTPSendThread;
    fNewVersionNumber, fUpdateURL, fChangelog: String;
    procedure MainThreadUpdate;
    procedure SyncStartUpdate;
    procedure SyncEndUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_NewVersionFound = 'New Version found!';
  RS_CurrentVersion = 'Installed Version';
  RS_LatestVersion = 'Latest Version   ';
  RS_BtnCheckUpdates = 'Check for latest version';

implementation

uses
  frmMain, frmUpdateDialog, FMDVars;

{ TCheckUpdateThread }

procedure TCheckUpdateThread.MainThreadUpdate;
begin
  if IsDlgCounter then Exit;
  with TUpdateDialogForm.Create(MainForm) do try
      Caption := Application.Title + ' - ' + RS_NewVersionFound;
      with mmLog.Lines do
      begin
        BeginUpdate;
        try
          Clear;
          Add(RS_CurrentVersion + ' : ' + FMD_VERSION_NUMBER);
          Add(RS_LatestVersion + ' : ' + fNewVersionNumber + LineEnding);
          AddText(fChangelog);
        finally
          EndUpdate;
        end;
      end;
      if ShowModal = mrYes then
      begin
        UpdateURL := fUpdateURL;
        DoAfterFMD := DO_UPDATE;
        MainForm.tmExitCommand.Enabled := True;
      end
      else
        MainForm.btCheckLatestVersion.Caption := RS_BtnCheckUpdates;
    finally
      Free;
    end;
end;

procedure TCheckUpdateThread.SyncStartUpdate;
begin
  with MainForm.btCheckLatestVersion do
  begin
    MainForm.btAbortCheckLatestVersion.Visible := True;
    Width := Width - MainForm.btAbortCheckLatestVersion.Width - 4;
    Caption := RS_Checking;
  end;
end;

procedure TCheckUpdateThread.SyncEndUpdate;
begin
  with MainForm.btCheckLatestVersion do
  begin
    MainForm.btAbortCheckLatestVersion.Visible := False;
    Width := Width + MainForm.btAbortCheckLatestVersion.Width + 4;
    Caption := RS_BtnCheckUpdates;
  end;
end;

procedure TCheckUpdateThread.Execute;
var
  l: TStringList;
  updateFound: Boolean = False;
begin
  Synchronize(@SyncStartUpdate);
  l := TStringList.Create;
  try
    fNewVersionNumber := FMD_VERSION_NUMBER;
    fUpdateURL := '';
    if not Terminated and
      GetPage(FHTTP, TObject(l), UPDATE_URL + 'update', 3) then
      if l.Count > 1 then
      begin
        l.NameValueSeparator := '=';
        if Trim(l.Values['VERSION']) <> FMD_VERSION_NUMBER then
        begin
          fNewVersionNumber := Trim(l.Values['VERSION']);
          fUpdateURL := Trim(l.Values[UpperCase(FMD_TARGETOS)]);
          if fUpdateURL <> '' then
            updateFound := True;
          FHTTP.Clear;
          l.Clear;
          if not Terminated and
            GetPage(FHTTP, TObject(l), UPDATE_URL + 'changelog.txt', 3) then
            fChangelog := l.Text;
        end;
      end;
  finally
    l.Free;
  end;
  Synchronize(@SyncEndUpdate);
  if not Terminated and updateFound and (not isDlgCounter) then
    Synchronize(@MainThreadUpdate);
end;

constructor TCheckUpdateThread.Create;
begin
  inherited Create(False);
  FHTTP := THTTPSendThread.Create(Self);
end;

destructor TCheckUpdateThread.Destroy;
begin
  FHTTP.Free;
  CheckUpdateThread := nil;
  inherited Destroy;
end;

end.
