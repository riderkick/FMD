unit frmAccountManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, VirtualTrees, accountmanagerdb, WebsiteModules,
  FMDOptions, HTTPSendThread, BaseThread, frmAccountSet, SimpleException;

type

  { TAccountManagerForm }

  TAccountManagerForm = class(TForm)
    btDelete: TBitBtn;
    btRefresh: TBitBtn;
    btEdit: TBitBtn;
    btAdd: TBitBtn;
    pnBtContainer: TPanel;
    vtAccountList: TVirtualStringTree;
    procedure btAddClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vtAccountListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtAccountListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtAccountListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure vtAccountListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtAccountListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vtAccountListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtAccountListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    { private declarations }
  public
    { public declarations }
    procedure RefreshList;
    procedure SaveForm;
    procedure LoadForm;
    procedure RefreshWebsiteAvailable;
  end;

  TAccountCheck = class;

  { TAccountCheckThread }

  TAccountCheckThread = class(TBaseThread)
  private
    fwebsite: String;
    fhttp: THTTPSendThread;
    fthreadlist: TAccountCheck;
    procedure SyncStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(const AWebsite: String; ThreadList: TAccountCheck);
    destructor Destroy; override;
  end;

  TAccountCheck = class
  private
    fthreads: TFPList;
    fthreadslock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddThread(const t: TAccountCheckThread);
    procedure DeleteThread(const t: TAccountCheckThread);
    procedure StopAll;
  end;

var
  AccountManagerForm: TAccountManagerForm;

const
  CL_HLRedMarks = $008080FF;

resourcestring
  RS_Unknown = 'Unknown';
  RS_Checking = 'Checking';
  RS_Valid = 'OK';
  RS_Invalid = 'Invalid';
  RS_AccountDeleteConfirmation = 'Are you sure you want to delete this account?';

implementation

uses frmMain, frmCustomColor;

var
  Websites, WebsitesAvailable: TStringlist;
  AccountThreadList: TAccountCheck;

{$R *.lfm}

{ TAccountCheck }

constructor TAccountCheck.Create;
begin
  fthreads := TFPList.Create;
  InitCriticalSection(fthreadslock);
end;

destructor TAccountCheck.Destroy;
begin
  if fthreads.Count > 0 then
    StopAll;
  while fthreads.Count > 0 do Sleep(250);
  fthreads.Free;
  DoneCriticalsection(fthreadslock);
  inherited Destroy;
end;

procedure TAccountCheck.AddThread(const t: TAccountCheckThread);
begin
  EnterCriticalsection(fthreadslock);
  try
    fthreads.Add(t);
  finally
    LeaveCriticalsection(fthreadslock);
  end;
end;

procedure TAccountCheck.DeleteThread(const t: TAccountCheckThread);
begin
  EnterCriticalsection(fthreadslock);
  try
    fthreads.Remove(t);
  finally
    LeaveCriticalsection(fthreadslock);
  end;
end;

procedure TAccountCheck.StopAll;
var
  i: Integer;
begin
  EnterCriticalsection(fthreadslock);
  try
    if fthreads.Count > 0 then
      for i := 0 to fthreads.Count - 1 do
        TAccountCheckThread(fthreads[i]).Terminate;
  finally
    LeaveCriticalsection(fthreadslock);
  end;
end;

{ TAccountCheckThread }

procedure TAccountCheckThread.SyncStatus;
begin
  if Assigned(AccountManagerForm) then
    AccountManagerForm.vtAccountList.Repaint;
end;

procedure TAccountCheckThread.Execute;
var
  p: Integer;
begin
  if fwebsite = '' then Exit;
  try
    p := Modules.LocateModule(fwebsite);
    if p > -1 then
      Modules.Login(fhttp, p);
    Synchronize(@SyncStatus);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

constructor TAccountCheckThread.Create(const AWebsite: String;
  ThreadList: TAccountCheck);
begin
  inherited Create(False);
  if AWebsite <> '' then fwebsite := AWebsite
  else fwebsite :=  '';
  fhttp := THTTPSendThread.Create(Self);
  if Assigned(ThreadList) then begin
    fthreadlist := ThreadList;
    fthreadlist.AddThread(Self);
  end
  else fthreadlist := nil;
end;

destructor TAccountCheckThread.Destroy;
begin
  if Assigned(fthreadlist) then fthreadlist.DeleteThread(Self);
  if Assigned(fhttp) then fhttp.Free;
  inherited Destroy;
end;

{ TAccountManagerForm }

procedure TAccountManagerForm.vtAccountListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if Assigned(Node) then
    Node^.CheckType := ctCheckBox;
end;

procedure TAccountManagerForm.vtAccountListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if node = nil then Exit;
  if Node^.CheckState = csUncheckedNormal then
    TargetCanvas.Font.Color := clGrayText;
end;

procedure TAccountManagerForm.RefreshList;
begin
  vtAccountList.RootNodeCount := Account.Count;
end;

procedure TAccountManagerForm.SaveForm;
var
  i: Integer;
begin
  with configfile, vtAccountList.Header.Columns do begin
    if Count > 0 then
      for i := 0 to Count - 1 do
        WriteInteger('form', 'vtAccountList' + IntToStr(i) + 'Width', Items[i].Width);
  end;
end;

procedure TAccountManagerForm.LoadForm;
var
  i: Integer;
begin
   with configfile, vtAccountList.Header.Columns do begin
     if Count > 0 then
       for i := 0 to Count - 1 do
         Items[i].Width := ReadInteger('form', 'vtAccountList' + IntToStr(i) + 'Width', 50);
  end;
end;

procedure TAccountManagerForm.RefreshWebsiteAvailable;
var
  i, p: Integer;
begin
  WebsitesAvailable.Clear;
  if Websites.Count = 0 then Exit;
  WebsitesAvailable.Assign(Websites);
  WebsitesAvailable.Sorted := True;
  if Account.Count > 0 then
    for i := 0 to Account.Count - 1 do begin
      if WebsitesAvailable.Find(Account.ValueStr[i, 1], p) then
        WebsitesAvailable.Delete(p);
    end;
  btAdd.Enabled := WebsitesAvailable.Count > 0;
end;

procedure TAccountManagerForm.vtAccountListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Assigned(Node) then begin
    case Column of
      0: if Account.ValueBool[Node^.Index, 0] then
          Node^.CheckState := csCheckedNormal
        else
          Node^.CheckState := csUncheckedNormal;
      1: CellText := Account.ValueStr[Node^.Index, 1];
      2: CellText := Account.ValueStr[Node^.Index, 2];
      3: case Account.ValueInt[Node^.Index, 5] of
           Integer(asUnknown) : CellText := RS_Unknown;
           Integer(asChecking): CellText := RS_Checking;
           Integer(asValid)   : CellText := RS_Valid;
           Integer(asInvalid) : CellText := RS_Invalid;
         end;
    end;
  end;
end;

procedure TAccountManagerForm.FormShow(Sender: TObject);
begin
  RefreshList;
  RefreshWebsiteAvailable;
end;

procedure TAccountManagerForm.vtAccountListBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if Node = nil then Exit;
  if Account.ValueInt[Node^.Index, 5] = Integer(asInvalid) then begin
    TargetCanvas.Brush.Color := CL_HLRedMarks;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TAccountManagerForm.vtAccountListChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Sender.SelectedCount > 0 then begin
    btEdit.Enabled := True;
    btDelete.Enabled := True;
    btRefresh.Enabled := True;
  end
  else begin
    btEdit.Enabled := False;
    btDelete.Enabled := False;
    btRefresh.Enabled := False;
  end;
end;

procedure TAccountManagerForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveForm;
  AccountThreadList.StopAll;
end;

procedure TAccountManagerForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Websites := TStringList.Create;
  WebsitesAvailable := TStringList.Create;
  AccountThreadList := TAccountCheck.Create;
  if Modules.Count > 0 then begin
    for i := 0 to Modules.Count - 1 do
      if Modules.Module[i].AccountSupport then
        Websites.Add(Modules.Module[i].Website);
    Websites.Sorted := True;
  end;
  LoadForm;
  AddVT(vtAccountList);
end;

procedure TAccountManagerForm.FormDestroy(Sender: TObject);
begin
  RemoveVT(vtAccountList);
  if Assigned(Websites) then Websites.Free;
  if Assigned(WebsitesAvailable) then WebsitesAvailable.Free;
  if Assigned(AccountThreadList) then AccountThreadList.Free;
end;

procedure TAccountManagerForm.btDeleteClick(Sender: TObject);
var
  node: PVirtualNode;
begin
  if vtAccountList.SelectedCount > 0 then
    if MessageDlg(RS_AccountDeleteConfirmation, mtConfirmation, mbYesNo, 0, mbNo) = mrYes then
    begin
      node := vtAccountList.GetFirstSelected;
      if Assigned(node) then begin
        Account.DeleteAccount(node^.Index);
        RefreshList;
        RefreshWebsiteAvailable;
        vtAccountList.Repaint;
        vtAccountListChange(vtAccountList, nil);
      end;
    end;
end;

procedure TAccountManagerForm.btEditClick(Sender: TObject);
var
  node: PVirtualNode;
  u, p: String;
begin
  if vtAccountList.SelectedCount > 0 then begin
    node := vtAccountList.GetFirstSelected;
    if Assigned(node) then begin
      with TAccountSetForm.Create(Self) do
      try
        cbWebsiteName.Items.Add(Account.ValueStr[node^.Index, 1]);
        cbWebsiteName.ItemIndex := 0;
        cbWebsiteName.Enabled := False;
        u := Account.ValueStr[node^.Index, 2];
        p := Account.ValueStr[node^.Index, 3];
        edUsername.Text := u;
        edPassword.Text := p;
        if ShowModal = mrOK then
          if (edUsername.Text <> u) or (edPassword.Text <> p) then begin
            Account.Username[cbWebsiteName.Text] := edUsername.Text;
            Account.Password[cbWebsiteName.Text] := edPassword.Text;
            btRefreshClick(btRefresh);
          end;
      finally
        Free;
      end;
    end;
  end;
end;

procedure TAccountManagerForm.btRefreshClick(Sender: TObject);
var
  node: PVirtualNode;
  web: String;
begin
  if vtAccountList.SelectedCount = 0 then Exit;
  node := vtAccountList.GetFirstSelected;
  if node = nil then Exit;
  if Account.ValueInt[node^.Index, 5] = Integer(asChecking) then Exit;
  web := Account.ValueStr[node^.Index, 1];
  if web <> '' then begin
    TAccountCheckThread.Create(web, AccountThreadList);
    vtAccountList.Repaint;
  end;
end;

procedure TAccountManagerForm.btAddClick(Sender: TObject);
begin
  if Websites.Count = 0 then Exit;
  with TAccountSetForm.Create(Self) do
  try
    cbWebsiteName.Items.Assign(WebsitesAvailable);
    if cbWebsiteName.Items.Count > 0 then
      cbWebsiteName.ItemIndex := 0;
    ckShowPassword.Visible:=True;
    if ShowModal = mrOK then
      if Account.AddAccount(cbWebsiteName.Text, edUsername.Text, edPassword.Text) then
      begin
        Account.Save;
        RefreshList;
        RefreshWebsiteAvailable;
        vtAccountList.Selected[vtAccountList.GetLast] := True;
        btRefreshClick(btRefresh);
      end;
  finally
    Free;
  end;
end;

procedure TAccountManagerForm.vtAccountListChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) then
    Account.ValueBool[Node^.Index, 0] := Node^.CheckState = csCheckedNormal;
end;

procedure TAccountManagerForm.vtAccountListFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  Sender.ScrollIntoView(Node, False, False);
end;

end.

