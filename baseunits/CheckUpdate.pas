{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit CheckUpdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, uBaseUnit, FMDOptions, httpsendthread,
  BaseThread, SelfUpdater, fileinfo;

type

  TCheckUpdateThread = class(TBaseThread)
  private
    FHTTP: THTTPSendThread;
    FNewVersionNumber: TProgramVersion;
    FNewVersionString: String;
    FUpdateURL: String;
    FChangelog: String;
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
          Add(RS_CurrentVersion + ' : ' + FMD_VERSION_STRING);
          Add(RS_LatestVersion + ' : ' + FNewVersionString + LineEnding);
          AddText(FChangelog);
        finally
          EndUpdate;
        end;
      end;
      if ShowModal = mrYes then
      begin
        with TSelfUpdaterThread.Create do
        begin
          UpdateURL := FUpdateURL;
          NewVersionString := FNewVersionString;
          Start;
        end;
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
begin
  FNewVersionString := '';
  FUpdateURL := '';
  FChangelog := '';
  Synchronize(@SyncStartUpdate);
  if not Terminated and FHTTP.Get(UPDATE_URL) then
  begin
    with TStringList.Create do try
      LoadFromStream(FHTTP.Document);
      if Count <> 0 then begin
        NameValueSeparator := '=';
        FNewVersionString := Trim(Values['VERSION']);
        if not TryStrToProgramVersion(FNewVersionString, FNewVersionNumber) then
          FNewVersionNumber := StrToProgramVersion('0.0.0.0');
        if NewerVersion(FNewVersionNumber, FMD_VERSION_NUMBER) then
          FUpdateURL := Trim(Values[UpperCase(FMD_TARGETOS)]);
      end;
    finally
      Free;
    end;
  end;

  if not Terminated and (FUpdateURL <> '') and FHTTP.Get(CHANGELOG_URL) then
    FChangelog := StreamToString(FHTTP.Document);
  Synchronize(@SyncEndUpdate);
  if not Terminated and (FUpdateURL <> '') and (not isDlgCounter) then
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
