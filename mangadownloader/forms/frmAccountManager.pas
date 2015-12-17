unit frmAccountManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, ExtCtrls, VirtualTrees, accountmanagerdb, WebsiteModules,
  frmAccountSet, USimpleLogger;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vtAccountListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtAccountListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
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

var
  AccountManagerForm: TAccountManagerForm;

resourcestring
  RS_Unknown = 'Unknown';
  RS_Checking = 'Checking';
  RS_Valid = 'OK';
  RS_Invalid = 'Invalid';
  RS_AccountDeleteConfirmation = 'Are you sure you want to delete this account?';

implementation

uses frmMain;

var
  Websites, WebsitesAvailable: TStringlist;

{$R *.lfm}

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
  if Node^.CheckState = csUncheckedNormal then TargetCanvas.Font.Color := clGrayText
  else TargetCanvas.Font.Color := clDefault;
end;

procedure TAccountManagerForm.RefreshList;
begin
  vtAccountList.RootNodeCount := Account.Count;
end;

procedure TAccountManagerForm.SaveForm;
var
  i: Integer;
begin
  with MainForm.options, vtAccountList.Header.Columns do begin
    if Count > 0 then
      for i := 0 to Count - 1 do
        WriteInteger('form', 'vtAccountList' + IntToStr(i) + 'Width', Items[i].Width);
  end;
end;

procedure TAccountManagerForm.LoadForm;
var
  i: Integer;
begin
   with MainForm.options, vtAccountList.Header.Columns do begin
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
           Integer(asUnknown): CellText := RS_Unknown;
           Integer(asValid)  : CellText := RS_Valid;
           Integer(asInvalid): CellText := RS_Invalid;
         end;
    end;
  end;
end;

procedure TAccountManagerForm.FormShow(Sender: TObject);
begin
  LoadForm;
  RefreshList;
  RefreshWebsiteAvailable;
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
end;

procedure TAccountManagerForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Websites := TStringList.Create;
  WebsitesAvailable := TStringList.Create;
  if Modules.Count > 0 then begin
    for i := 0 to Modules.Count - 1 do
      if Modules.Module[i].AccountSupport then
        Websites.Add(Modules.Module[i].Website);
    Websites.Sorted := True;
  end;
end;

procedure TAccountManagerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Websites) then Websites.Free;
  if Assigned(WebsitesAvailable) then WebsitesAvailable.Free;
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
begin
  if vtAccountList.SelectedCount > 0 then begin
    node := vtAccountList.GetFirstSelected;
    if Assigned(node) then begin
      with TAccountSetForm.Create(Self) do
      try
        cbWebsiteName.Items.Add(Account.ValueStr[node^.Index, 1]);
        cbWebsiteName.ItemIndex := 0;
        cbWebsiteName.Enabled := False;
        edUsername.Text := Account.ValueStr[node^.Index, 2];
        edPassword.Text := Account.ValueStr[node^.Index, 3];
        if ShowModal = mrOK then
        begin
          Account.Username[cbWebsiteName.Text] := edUsername.Text;
          Account.Password[cbWebsiteName.Text] := edPassword.Text;
          vtAccountList.Repaint;
        end;
      finally
        Free;
      end;
    end;
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
    if ShowModal = mrOK then
      if Account.AddAccount(cbWebsiteName.Text, edUsername.Text, edPassword.Text) then
      begin
        Account.Save;
        RefreshList;
        RefreshWebsiteAvailable;
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

end.

