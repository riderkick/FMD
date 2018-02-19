unit frmLuaModulesUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Menus, ExtCtrls, VirtualTrees, synautil, httpsendthread, BaseThread,
  XQueryEngineHTML, fpjson, jsonparser, jsonscanner, dateutils;

type

  TLuaModuleRepoFlag = (fNone, fNew, fUpdate, fDelete, fDeleted, fDownloading,
    fDownloaded, fFailedDownload);

  PPLuaModuleRepo = ^PLuaModuleRepo;
  PLuaModuleRepo = ^TLuaModuleRepo;

  { TLuaModuleRepo }

  TLuaModuleRepo = class
    name: String;
    sha: String;
    size: Integer;
    download_url: String;
    last_modified: TDateTime;
    last_message: String;
    flag: TLuaModuleRepoFlag;
    oflag: TLuaModuleRepoFlag;
    function Clone: TLuaModuleRepo;
    function SyncTo(const t: TLuaModuleRepo): Boolean;
  end;

  { TLuaModulesRepos }

  TLuaModulesRepos = class
  private
    function GetCount: Integer; inline;
    function GetRepo(const AIndex: Integer): TLuaModuleRepo; inline;
  public
    Items: TStringList;

    constructor Create;
    destructor Destroy; override;

    procedure Clear; inline;
    function Add(const AName: String): TLuaModuleRepo; overload;
    procedure Add(const I: TLuaModuleRepo); overload;
    procedure LoadFromRemote(const AHTTP: THTTPSendThread);
    procedure LoadFromRemoteHTML(const AHTTP: THTTPSendThread);
    procedure LoadFromFile(const AFilename: String);
    procedure SaveToFile(const AFilename: String);
    procedure Sort;
    function Clone: TLuaModulesRepos;
    property Count: Integer read GetCount;
    property Repo[const AIndex: Integer]: TLuaModuleRepo read GetRepo; default;
  end;

  TCheckUpdateThread = class;

  { TLuaModulesUpdaterForm }

  TLuaModulesUpdaterForm = class(TForm)
    btCheckUpdate: TBitBtn;
    ckShowUpdateWarning: TCheckBox;
    ckAutoRestart: TCheckBox;
    imStates: TImageList;
    btCheckUpdateTerminate: TSpeedButton;
    tmRepaintList: TTimer;
    vtLuaModulesRepos: TVirtualStringTree;
    procedure btCheckUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btCheckUpdateTerminateClick(Sender: TObject);
    procedure tmRepaintListTimer(Sender: TObject);
    procedure vtLuaModulesReposCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtLuaModulesReposGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
    procedure vtLuaModulesReposGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtLuaModulesReposGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    {$if VTMajorVersion < 5}
    procedure vtLuaModulesReposHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    {$else}
    procedure vtLuaModulesReposHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    {$endif}
  private
    FListCS: TRTLCriticalSection;
    FListDirty: Boolean;
  public
    Repos: TLuaModulesRepos;
    ThreadCheck: TCheckUpdateThread;
    procedure ListDirty;
    procedure LoadLocalRepos;
    procedure ReinitList(const ASort: Boolean = True);
    procedure SortList;
  end;

  { TDownloadThread }

  TDownloadThread = class(TBaseThread)
  private
    FOwner: TCheckUpdateThread;
    FModule: TLuaModuleRepo;
    FHTTP: THTTPSendThread;
  protected
    procedure Execute; override;
  public
    constructor Create(const Owner: TCheckUpdateThread; const T: TLuaModuleRepo);
    destructor Destroy; override;
  end;

  { TCheckUpdateThread }

  TCheckUpdateThread = class(TBaseThread)
  private
    FOwner: TLuaModulesUpdaterForm;
    FHTTP: THTTPSendThread;
    FReposUp: TLuaModulesRepos;
    FRepos: TLuaModulesRepos;
    FMainRepos: TLuaModulesRepos;
    FThreads: TFPList;
    FThreadsCS: TRTLCriticalSection;
    FDownloadedCount: Integer;
    FProceed: Boolean;
    FStatusList: TStringList;
    procedure RemoveThread(const T: TDownloadThread);
    procedure AddThread(const T: TDownloadThread);
  protected
    procedure SyncStartChecking;
    procedure SyncFinishChecking;
    procedure SyncAskToProceed;
    procedure SyncStartDownload;
    procedure SyncFinishDownload;
    procedure SyncFinal;
    function SyncRepos(const ARepos, AReposUp: TLuaModulesRepos): Boolean;
    procedure Download;
    procedure Execute; override;
  public
    constructor Create(const AOwner: TLuaModulesUpdaterForm);
    destructor Destroy; override;
    procedure AddStatus(const S: String);
  end;

var
  LuaModulesUpdaterForm: TLuaModulesUpdaterForm;

resourcestring
  RS_CheckUpdate = 'Check update';
  RS_Checking = 'Checking...';
  RS_FinishChecking = 'Finish checking';
  RS_StartDownloading = 'Downloading...';
  RS_FinishDownload = 'Finish download';
  RS_NewUpdateFoundTitle = 'Modules update found!';
  RS_NewUpdateFoundLostChanges = 'Modules update found, any local changes will be lost, procced?'#13#10#13#10'%s';
  RS_ModulesUpdatedTitle = 'Modules updated!';
  RS_ModulesUpdatedRestart = 'Modules updated, restart now?'#13#10#13#10'%s';
  RS_StatusNew = '%s NEW*';
  RS_StatusUpdate = '%s UPDATE*';
  RS_StatusRedownloaded = '%s REDOWNLOAD*';
  RS_StatusFailed = '%s FAILED*';
  RS_StatusDelete = '%s DELETE*';

implementation

uses frmCustomColor, FMDOptions;

const
  // RFC 3339 - ISO 8601
  DateTimeFormatStrDecode = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  DateTimeFormatStrEncode = DateTimeFormatStrDecode + '"Z"';
  UnknownDateTime = 43101.0423726852; // 01/01/2018 01:01:01

function JSONToDateTime(const s: String): TDateTime;
begin
  if Length(s) = 20 then
    Result := ScanDateTime(DateTimeFormatStrDecode, s)
  else
    Result := UnknownDateTime;
end;

function DateTimeToJSON(const d: TDateTime): String;
begin
  Result := FormatDateTime(DateTimeFormatStrEncode, d);
end;

{ TLuaModuleRepo }

function TLuaModuleRepo.Clone: TLuaModuleRepo;
begin
  Result := TLuaModuleRepo.Create;
  Result.name := name;
  Result.sha := sha;
  Result.sha := sha;
  Result.size := size;
  Result.download_url := download_url;
  Result.last_modified := last_modified;
  Result.last_message := last_message;
  Result.flag := flag;
  Result.oflag := oflag;
end;

function TLuaModuleRepo.SyncTo(const t: TLuaModuleRepo): Boolean;
begin
  Result := name = t.name;
  if not Result then
    Exit;
  if sha <> t.sha then
  begin
    t.sha := sha;
    t.last_modified := last_modified;
    t.last_message := last_message;
    t.oflag := t.flag;
    t.flag := fUpdate;
  end;
end;

{ TLuaModulesRepos }

function TLuaModulesRepos.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TLuaModulesRepos.GetRepo(const AIndex: Integer): TLuaModuleRepo;
begin
  Result := TLuaModuleRepo(Items.Objects[AIndex]);
end;

constructor TLuaModulesRepos.Create;
begin
  Items := TStringList.Create;
  Items.OwnsObjects := True;
  Clear;
end;

destructor TLuaModulesRepos.Destroy;
begin
  Clear;
  Items.Free;
  inherited Destroy;
end;

procedure TLuaModulesRepos.Clear;
begin
  Items.Clear;
end;

function TLuaModulesRepos.Add(const AName: String): TLuaModuleRepo;
begin
  Result := TLuaModuleRepo.Create;
  Result.name := AName;
  Items.AddObject(AName, Result);
end;

procedure TLuaModulesRepos.Add(const I: TLuaModuleRepo);
begin
  Items.AddObject(I.name, I);
end;

procedure TLuaModulesRepos.LoadFromRemote(const AHTTP: THTTPSendThread);
var
  j: TJSONParser;
  a: TJSONArray;
  i: Integer;
  o: TJSONObject;
  m: TLuaModuleRepo;
begin
  Clear;

  a := nil;
  j := TJSONParser.Create(AHTTP.Document, [joUTF8]);
  try
    a := TJSONArray(j.Parse);
  finally
    j.Free;
  end;
  if a = nil then
    Exit;

  try
    for i := 0 to a.Count - 1 do
    begin
      o := TJSONObject(a.Items[i]);
      m := Add(o.Get('name', ''));
      m.sha := o.Get('sha', '');
      m.download_url := o.Get('download_url', '');
      m.size := o.Get('size', 0);
    end;
  finally
    a.Free;
  end;
end;

procedure TLuaModulesRepos.LoadFromRemoteHTML(const AHTTP: THTTPSendThread);
var
  v: IXQValue;
  i: Integer;
begin
  with TXQueryEngineHTML.Create(AHTTP.Document) do
    try
      v := XPath('//table[starts-with(@class,"files")]//tr[@class="js-navigation-item"]');
      if v.Count = Count then
        for i := 1 to v.Count do
          with Repo[i - 1] do
          begin
            last_message := XPathString('./td[@class="message"]/span/a/@title', v.get(i));
            last_modified := JSONToDateTime(
              XPathString('./td[@class="age"]/span/time-ago/@datetime', v.get(i)));
          end;
    finally
      Free;
    end;
  Sort;
end;

procedure TLuaModulesRepos.LoadFromFile(const AFilename: String);
var
  f: TFileStream;
  j: TJSONParser;
  a: TJSONArray;
  o: TJSONObject;
  i: Integer;
  m: TLuaModuleRepo;
begin
  if not FileExists(AFilename) then
    Exit;

  a := nil;
  f := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    j := TJSONParser.Create(f, [joUTF8]);
    try
      a := TJSONArray(j.Parse);
    finally
      j.Free;
    end;
  finally
    f.Free;
  end;
  if a = nil then
    Exit;
  try
    Self.Clear;
    for i := 0 to a.Count - 1 do
    begin
      o := TJSONObject(a.Items[i]);
      m := Add(o.Get('name', ''));
      m.sha := o.Get('sha', '');
      m.download_url := o.Get('download_url', '');
      m.size := o.Get('size', 0);
      m.last_modified := JSONToDateTime(o.Get('last_modified', ''));
      m.last_message := o.Get('last_message', '');
      m.flag := TLuaModuleRepoFlag(o.Get('flag', 0));
      if (m.flag <> fFailedDownload) and
        (not FileExists(LUA_WEBSITEMODULE_FOLDER + m.name)) then
        m.flag := fFailedDownload;
    end;
  finally
    a.Free;
  end;
  Sort;
end;

procedure TLuaModulesRepos.SaveToFile(const AFilename: String);
var
  a: TJSONArray;
  o: TJSONObject;
  i: Integer;
  m: TLuaModuleRepo;
  f: TFileStream;
begin
  a := TJSONArray.Create;
  try
    for i := 0 to Items.Count - 1 do
    begin
      o := TJSONObject.Create;
      m := Repo[i];
      o.Add('name', m.name);
      o.Add('sha', m.sha);
      o.Add('download_url', m.download_url);
      o.Add('size', m.size);
      o.Add('last_modified', DateTimeToJSON(m.last_modified));
      o.Add('last_message', m.last_message);
      o.Add('flag', Integer(m.flag));
      a.Add(o);
    end;

    if FileExists(AFilename) then
      DeleteFile(AFilename);
    f := TFileStream.Create(AFilename, fmCreate);
    try
      a.DumpJSON(f);
    finally
      f.Free;
    end;
  finally
    a.Free;
  end;
end;

procedure TLuaModulesRepos.Sort;
begin
  if Items.Count <> 0 then
    Items.Sort;
end;

function TLuaModulesRepos.Clone: TLuaModulesRepos;
var
  i: Integer;
begin
  Result := TLuaModulesRepos.Create;
  for i := 0 to Items.Count - 1 do
    Result.Items.AddObject(Items[i], Repo[i].Clone);
end;

{ TDownloadThread }

procedure TDownloadThread.Execute;
var
  f: String;
  c: Boolean;
begin
  FModule.oflag := FModule.flag;
  FModule.flag := fDownloading;
  FOwner.FOwner.ListDirty;
  if FHTTP.GET(FModule.download_url) then
  begin
    if ForceDirectories(LUA_WEBSITEMODULE_FOLDER) then
    begin
      f := LUA_WEBSITEMODULE_FOLDER + FModule.name;
      c := True;
      if FileExists(f) then
        c := DeleteFile(f);
      if c then
      begin
        FHTTP.Document.SaveToFile(f);
        if FileExists(f) then
        begin
          case FModule.oflag of
            fNew: FOwner.AddStatus(Format(RS_StatusNew, [FModule.name]));
            fUpdate: FOwner.AddStatus(Format(RS_StatusUpdate, [FModule.name]));
            fFailedDownload: FOwner.AddStatus(Format(RS_StatusRedownloaded, [FModule.name]));
          end;
          FModule.flag := fDownloaded;
          FOwner.FDownloadedCount := InterLockedIncrement(FOwner.FDownloadedCount);
        end;
      end;
    end;
  end
  else
  begin
    FOwner.AddStatus(Format(RS_StatusFailed, [FModule.name]));
    FModule.flag := fFailedDownload;
  end;
  FOwner.FOwner.ListDirty;
end;

constructor TDownloadThread.Create(const Owner: TCheckUpdateThread; const T: TLuaModuleRepo);
begin
  inherited Create(False);
  FHTTP := THTTPSendThread.Create(Self);
  FOwner := Owner;
  FModule := T;
  FOwner.AddThread(Self);
end;

destructor TDownloadThread.Destroy;
begin
  FOwner.RemoveThread(Self);
  FHTTP.Free;
  inherited Destroy;
end;

{ TCheckUpdateThread }

procedure TCheckUpdateThread.RemoveThread(const T: TDownloadThread);
begin
  EnterCriticalsection(FThreadsCS);
  try
    FThreads.Remove(T);
  finally
    LeaveCriticalsection(FThreadsCS);
  end;
end;

procedure TCheckUpdateThread.AddThread(const T: TDownloadThread);
begin
  EnterCriticalsection(FThreadsCS);
  try
    FThreads.Add(T);
  finally
    LeaveCriticalsection(FThreadsCS);
  end;
end;

procedure TCheckUpdateThread.SyncStartChecking;
begin
  FOwner.btCheckUpdate.Caption := RS_Checking;
  FOwner.btCheckUpdateTerminate.Show;
  FOwner.tmRepaintList.Enabled := True;
end;

procedure TCheckUpdateThread.SyncFinishChecking;
begin
  FOwner.btCheckUpdate.Caption := RS_FinishChecking;
  FOwner.vtLuaModulesRepos.BeginUpdate;
  try
    FMainRepos := FOwner.Repos;
    FOwner.Repos := FRepos;
    FOwner.ReinitList;
  finally
    FOwner.vtLuaModulesRepos.EndUpdate;
  end;
end;

procedure TCheckUpdateThread.SyncAskToProceed;
begin
  FProceed := MessageDlg(RS_NewUpdateFoundTitle,
    Format(RS_NewUpdateFoundLostChanges, [Trim(FStatusList.Text)]),
    mtWarning, mbYesNo, 0) = mrYes;
end;

procedure TCheckUpdateThread.SyncStartDownload;
begin
  FOwner.btCheckUpdate.Caption := RS_StartDownloading;
end;

procedure TCheckUpdateThread.SyncFinishDownload;
begin
  FOwner.btCheckUpdate.Caption := RS_FinishDownload;
end;

procedure TCheckUpdateThread.SyncFinal;
begin
  FOwner.btCheckUpdateTerminate.Hide;
  FOwner.btCheckUpdate.Caption := RS_CheckUpdate;
  FOwner.ThreadCheck := nil;
  FOwner.tmRepaintList.Enabled := False;
  if FMainRepos <> nil then
    try
      FOwner.vtLuaModulesRepos.BeginUpdate;
      FOwner.Repos := FMainRepos;
      FOwner.ReinitList;
      FMainRepos.SaveToFile(LUA_WEBSITEMODULE_REPOS);
    finally
      FOwner.vtLuaModulesRepos.EndUpdate;
    end;
end;

function TCheckUpdateThread.SyncRepos(const ARepos, AReposUp: TLuaModulesRepos): Boolean;
var
  i, j, imax, jmax, k, inew, iupdate: Integer;
  newfound: Boolean;
  m, u: TLuaModuleRepo;
begin
  i := 0;
  j := 0;
  inew := 0;
  iupdate := 0;
  imax := ARepos.Items.Count;
  jmax := AReposUp.Items.Count;
  while (i < imax) or (j < jmax) do
  begin
    if i < imax then
      m := ARepos[i]
    else
      m := nil;
    if j < jmax then
      u := AReposUp[j]
    else
      u := nil;

    if (m <> nil) and (u <> nil) then
    begin
      if u.SyncTo(m) then // look for new update
      begin
        Inc(i);
        Inc(j);
        if m.flag = fUpdate then
          Inc(iupdate);
      end
      else
      begin  // scan remote ARepos till end
        newfound := False;
        for k := j + 1 to jmax - 1 do
        begin
          if m.name = AReposUp[k].name then // j to k-1 is new
          begin
            newfound := True;
            Break;
          end;
        end;
        if newfound then // add new
        begin
          for k := j to k - 1 do
          begin
            m := AReposUp[k].Clone;
            m.flag := fNew;
            ARepos.Add(m);
            Inc(inew);
          end;
          j := k + 1;
        end
        else  // current is marked to delete
        begin
          m.flag := fDelete;
          Inc(iupdate);
          Inc(i);
        end;
      end;
    end
    else
    if m = nil then // new found
    begin
      m := u.Clone;
      m.flag := fNew;
      ARepos.Add(m);
      Inc(inew);
      Inc(j);
    end
    else
    begin // current is marked to delete
      m.flag := fDelete;
      Inc(iupdate);
      Inc(i);
    end;
  end;
  if inew <> 0 then
    ARepos.Items.Sort;
  Result := inew + iupdate <> 0;
end;

procedure TCheckUpdateThread.Download;
var
  i, imax: Integer;
  m: TLuaModuleRepo;
  f: String;
begin
  Synchronize(@SyncStartDownload);

  FStatusList.Clear;
  i := 0;
  imax := FRepos.Items.Count;
  while i < imax do
  begin
    m := FRepos[i];
    if m.flag = fDelete then
    begin
      f := LUA_WEBSITEMODULE_FOLDER + m.name;
      if FileExists(f) then
        DeleteFile(f);
      AddStatus(Format(RS_StatusDelete, [m.name]));
      m.flag := fDeleted;
      Inc(i);
    end
    else
    if not (m.flag in [fNew, fUpdate, fFailedDownload]) then
      Inc(i)
    else
    begin
      while FThreads.Count >= OptionMaxThreads do
        Sleep(SOCKHEARTBEATRATE);
      if Terminated then
        Break;
      TDownloadThread.Create(Self, FRepos[i]);
      Inc(i);
    end;
  end;

  while FThreads.Count <> 0 do
  begin
    if Terminated then
      Break;
    Sleep(SOCKHEARTBEATRATE);
  end;

  EnterCriticalsection(FThreadsCS);
  try
    for i := 0 to FThreads.Count - 1 do
    begin
      TDownloadThread(FThreads[i]).Terminate;
    end;
  finally
    LeaveCriticalsection(FThreadsCS);
  end;

  while FThreads.Count <> 0 do
    Sleep(SOCKHEARTBEATRATE);
  Synchronize(@SyncFinishDownload);
end;

procedure TCheckUpdateThread.Execute;
var
  foundupdate: Boolean;
  i, imax: Integer;
  m: TLuaModuleRepo;
  trepos: TLuaModulesRepos;
begin
  Synchronize(@SyncStartChecking);
  if FHTTP.GET(MODULES_URL) then
  begin
    FReposUp := TLuaModulesRepos.Create;
    FReposUp.LoadFromRemote(FHTTP);
    if FHTTP.GET(MODULES_URL2) then
      FReposUp.LoadFromRemoteHTML(FHTTP);
  end;

  if not Terminated then
  begin
    // check
    FRepos := FOwner.Repos.Clone;
    foundupdate := SyncRepos(FRepos, FReposUp);

    // look for missing local files and previously failed download
    for i := 0 to FRepos.Items.Count - 1 do
    begin
      m := FRepos[i];
      if m.flag = fFailedDownload then
         foundupdate := True
      else
      if (not (m.flag in [fNew, fUpdate])) and
        (not FileExists(LUA_WEBSITEMODULE_FOLDER + m.name)) then
      begin
        m.flag := fFailedDownload;
        foundupdate := True;
      end;
      case m.flag of
        fNew: FStatusList.Add(Format(RS_StatusNew, [m.name]));
        fUpdate: FStatusList.Add(Format(RS_StatusUpdate, [m.name]));
        fDelete: FStatusList.Add(Format(RS_StatusDelete, [m.name]));
        fFailedDownload: FStatusList.Add(Format(RS_StatusFailed, [m.name]));
      end;
    end;

    Synchronize(@SyncFinishChecking);

    if foundupdate and (not Terminated) then
    begin
      if OptionModulesUpdaterShowUpdateWarning then
        Synchronize(@SyncAskToProceed)
      else
      begin
        FProceed := True;
        Sleep(1500); // delay to show the update status
      end;
      if FProceed then
        Download;
    end;

    // cleanup
    i := 0;
    imax := FRepos.Items.Count;
    while i < imax do
    begin
      m := FRepos[i];
      if m.flag = fDeleted then
      begin
        FRepos.Items.Delete(i);
        Dec(imax);
      end
      else
      begin
        if m.flag in [fNew, fUpdate, fFailedDownload] then
          m.flag := fFailedDownload
        else
          m.flag := fNone;
        Inc(i);
      end;
    end;
    trepos := FMainRepos;
    FMainRepos := FRepos;
    FRepos := trepos;
  end;

  if not Terminated then
    Sleep(1000);
  Synchronize(@SyncFinal);

  if not Terminated then
    if (FDownloadedCount <> 0) and (OptionModulesUpdaterAutoRestart or
      (MessageDlg(RS_ModulesUpdatedTitle,
      Format(RS_ModulesUpdatedRestart, [Trim(FStatusList.Text)]),
      mtConfirmation, mbYesNo, 0) = mrYes)) then
        RestartFMD;
end;

constructor TCheckUpdateThread.Create(const AOwner: TLuaModulesUpdaterForm);
begin
  inherited Create(False);
  InitCriticalSection(FThreadsCS);
  FOwner := AOwner;
  FHTTP := THTTPSendThread.Create(Self);
  FThreads := TFPList.Create;
  FDownloadedCount := 0;
  FStatusList := TStringList.Create;
end;

destructor TCheckUpdateThread.Destroy;
begin
  if Assigned(FRepos) then
    FRepos.Free;
  if Assigned(FReposUp) then
    FReposUp.Free;
  FStatusList.Free;
  FThreads.Free;
  FHTTP.Free;
  DoneCriticalsection(FThreadsCS);
  inherited Destroy;
end;

procedure TCheckUpdateThread.AddStatus(const S: String);
begin
  EnterCriticalsection(FThreadsCS);
  try
    FStatusList.Add(S);
  finally
    LeaveCriticalsection(FThreadsCS);
  end;
end;

{$R *.lfm}

{ TLuaModulesUpdaterForm }

procedure TLuaModulesUpdaterForm.btCheckUpdateClick(Sender: TObject);
begin
  if ThreadCheck = nil then
    ThreadCheck := TCheckUpdateThread.Create(Self);
end;

procedure TLuaModulesUpdaterForm.FormCreate(Sender: TObject);
begin
  AddVT(vtLuaModulesRepos);
  InitCriticalSection(FListCS);
  Repos := TLuaModulesRepos.Create;
  btCheckUpdate.Caption := RS_CheckUpdate;
  btCheckUpdateTerminate.Hide;
  vtLuaModulesRepos.NodeDataSize := SizeOf(TLuaModuleRepo);
  LoadLocalRepos;
end;

procedure TLuaModulesUpdaterForm.FormDestroy(Sender: TObject);
begin
  if ThreadCheck <> nil then
  begin
    ThreadCheck.Terminate;
    ThreadCheck.WaitFor;
  end;
  Repos.Free;
  DoneCriticalsection(FListCS);
  RemoveVT(vtLuaModulesRepos);
end;

procedure TLuaModulesUpdaterForm.btCheckUpdateTerminateClick(Sender: TObject);
begin
  ThreadCheck.Terminate;
end;

procedure TLuaModulesUpdaterForm.tmRepaintListTimer(Sender: TObject);
begin
  if FListDirty then
  begin
    vtLuaModulesRepos.Repaint;
    EnterCriticalsection(FListCS);
    try
      FListDirty := False;
    finally
      LeaveCriticalsection(FListCS);
    end;
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  m1, m2: TLuaModuleRepo;
begin
  m1 := PLuaModuleRepo(Sender.GetNodeData(Node1))^;
  m2 := PLuaModuleRepo(Sender.GetNodeData(Node2))^;
  case Column of
    0: Result := AnsiCompareStr(m1.name, m2.name);
    1:
    begin
      if m1.last_modified > m2.last_modified then
        Result := 1
      else
        Result := -1;
    end;
    2: Result := AnsiCompareStr(m1.last_message, m2.last_message);
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
begin
  with PLuaModuleRepo(Sender.GetNodeData(Node))^ do
  begin
    case Column of
      0: HintText := name;
      1: HintText := DateTimeToStr(last_modified);
      2: HintText := last_message;
    end;
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Column <> 0 then
    Exit;
  ImageIndex := Integer(PLuaModuleRepo(Sender.GetNodeData(Node))^.flag) - 1;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  with PLuaModuleRepo(Sender.GetNodeData(Node))^ do
  begin
    case Column of
      0: CellText := name;
      1: CellText := DateTimeToStr(last_modified);
      2: CellText := last_message;
    end;
  end;
end;

{$if VTMajorVersion < 5}
procedure TLuaModulesUpdaterForm.vtLuaModulesReposHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$else}
procedure TLuaModulesUpdaterForm.vtLuaModulesReposHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  Column: TColumnIndex;
  Button: TMouseButton;
{$endif}
begin
  {$if VTMajorVersion >= 5}
  Column := HitInfo.Column;
  Button := HitInfo.Button;
  {$endif}
  if Sender.SortColumn <> Column then
    Sender.SortColumn := Column
  else
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  SortList;
end;

procedure TLuaModulesUpdaterForm.ListDirty;
begin
  if TryEnterCriticalsection(FListCS) <> 0 then
    try
      FListDirty := True;
    finally
      LeaveCriticalsection(FListCS);
    end;
end;

procedure TLuaModulesUpdaterForm.LoadLocalRepos;
begin
  Repos.LoadFromFile(LUA_WEBSITEMODULE_REPOS);
  ReinitList(False);
end;

procedure TLuaModulesUpdaterForm.ReinitList(const ASort: Boolean);
var
  i: Integer;
begin
  vtLuaModulesRepos.Clear;
  for i := 0 to Repos.Items.Count - 1 do
    vtLuaModulesRepos.AddChild(nil, Repos[i]);
  if ASort then
    SortList;
end;

procedure TLuaModulesUpdaterForm.SortList;
begin
  with vtLuaModulesRepos do
    Sort(nil, Header.SortColumn, Header.SortDirection, False);
end;

end.
