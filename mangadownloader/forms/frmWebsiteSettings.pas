unit frmWebsiteSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, WebsiteModules, RTTIGrids, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, PairSplitter, EditBtn, VirtualTrees,
  contnrs, RTTICtrls;

type

  { TSettingsView }

  TSettingsView = class
  private
    FSettings: TWebsiteModuleSettings;
    FOwner: TWinControl;
    FControls: TFPObjectList;
    procedure SetData(settings: TWebsiteModuleSettings);
    function GetData: TWebsiteModuleSettings;
    procedure CreateControls;
    procedure UpdateView;
    procedure OnEditorButtonClick(Sender: TObject);
  public
    constructor Create(owner: TWinControl);
    destructor Destroy; override;
    property Data: TWebsiteModuleSettings read GetData write SetData;
  end;

  { TWebsiteSettingsForm }

  TWebsiteSettingsForm = class(TForm)
    edSearch: TEditButton;
    sbProps: TScrollBox;
    spMain: TPairSplitter;
    spList: TPairSplitterSide;
    spProps: TPairSplitterSide;
    pnTop: TPanel;
    vtWebsite: TVirtualStringTree;
    procedure edSearchButtonClick(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtWebsiteCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtWebsiteFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtWebsiteGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    settingsView: TSettingsView;
  public
    procedure LoadWebsiteSettings;
  end;

var
  WebsiteSettingsForm: TWebsiteSettingsForm;

implementation

uses frmCustomColor, typinfo, StringsPropEditDlg;

{$R *.lfm}

{ TWebsiteSettingsForm }

procedure TWebsiteSettingsForm.FormCreate(Sender: TObject);
begin
  AddVT(vtWebsite);
  settingsView := TSettingsView.Create(sbProps);
end;

procedure TWebsiteSettingsForm.edSearchChange(Sender: TObject);
var
  s: String;
  node: PVirtualNode;
begin
  s:=AnsiUpperCase(edSearch.Text);
  vtWebsite.BeginUpdate;
  node:=vtWebsite.GetFirst();
  if s<>'' then
    while node<>nil do
    begin
      vtWebsite.IsVisible[node]:=Pos(s,AnsiUpperCase(PModuleContainer(vtWebsite.GetNodeData(node))^.Website))<>0;
      node:=vtWebsite.GetNext(node);
    end
  else
    while node<>nil do
    begin
      vtWebsite.IsVisible[node]:=True;
      node:=vtWebsite.GetNext(node);
    end;
  vtWebsite.EndUpdate;
end;

procedure TWebsiteSettingsForm.edSearchButtonClick(Sender: TObject);
begin
  edSearch.Clear;
end;

procedure TWebsiteSettingsForm.FormDestroy(Sender: TObject);
begin
  RemoveVT(vtWebsite);
  settingsView.Free;
end;

procedure TWebsiteSettingsForm.vtWebsiteCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Result:=AnsiCompareStr(PModuleContainer(Sender.GetNodeData(Node1))^.Website,
    PModuleContainer(Sender.GetNodeData(Node2))^.Website);
end;

procedure TWebsiteSettingsForm.vtWebsiteFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  settingsView.Data := PModuleContainer(Sender.GetNodeData(Node))^.Settings;
end;

procedure TWebsiteSettingsForm.vtWebsiteGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText:=PModuleContainer(Sender.GetNodeData(Node))^.Website;
end;

procedure TWebsiteSettingsForm.LoadWebsiteSettings;
var
  i: Integer;
begin
  vtWebsite.NodeDataSize:=SizeOf(TModuleContainer);
  vtWebsite.BeginUpdate;
  for i:=0 to Modules.Count-1 do
    vtWebsite.AddChild(nil,Modules[i]);
  vtWebsite.Sort(nil,0,sdAscending,false);
  vtWebsite.EndUpdate;
end;

{ TSettingsView }

constructor TSettingsView.Create(owner: TWinControl);
begin
  FOwner := owner;
  FControls := TFPObjectList.Create(True);
  CreateControls;
  UpdateView;
end;

destructor TSettingsView.Destroy;
begin
  FControls.Clear;
  FControls.Free;
  inherited;
end;

procedure TSettingsView.SetData(settings: TWebsiteModuleSettings);
begin
  FSettings := settings;
  UpdateView;
end;

function TSettingsView.GetData: TWebsiteModuleSettings;
begin
  Result := FSettings;
end;

procedure TSettingsView.OnEditorButtonClick(Sender: TObject);

  function FindEditor: TTIEdit;
  var
    i: Integer;
  begin
    for i := 0 to FControls.Count - 2 do
      if Sender = FControls[i] then begin
        Result := TTIEdit(FControls[i+1]);
        Exit;
      end;
  end;

var
  editor: TTIEdit;
  form: TStringsPropEditorFrm;
begin
  if not Assigned(FSettings) then Exit;
  form := TStringsPropEditorFrm.Create(FOwner);
  editor := FindEditor;
  form.Memo.Text := editor.Link.GetAsText;
  if form.ShowModal = mrOK then
    editor.Link.SetAsText(form.Memo.Text);
  form.Free;
end;

procedure TSettingsView.CreateControls;

  procedure SetAnchor(control, lbl, prev: TControl);
  begin
    control.Anchors := [akTop, akLeft, akRight];
    if prev <> nil then begin
      control.AnchorSide[akTop].Control := prev;
      control.AnchorSide[akTop].Side := asrBottom;
      control.BorderSpacing.Top := 10;
    end
    else begin
      control.AnchorSide[akTop].Control := lbl;
      control.AnchorSide[akTop].Side := asrCenter;
    end;
    control.BorderSpacing.Right := 10;
    control.BorderSpacing.Left := 50;
  end;

  function AddLabel(text: String; prev: TControl): TLabel;
  begin
    Result := TLabel.Create(FOwner);
    Result.Parent := FOwner;
    Result.Caption := text;
    Result.AnchorSide[akLeft].Control := FOwner;
    Result.AnchorSide[akLeft].Side := asrLeft;
    Result.BorderSpacing.Left := 5;
    if prev <> nil then begin
      Result.AnchorSide[akTop].Control := prev;
      Result.AnchorSide[akTop].Side := asrBottom;
      Result.BorderSpacing.Top := 10;
    end
    else
      Result.Top := 5;
    FControls.Add(Result);
  end;

  function AddEdit(name, text: String; prev: TControl): TTIEdit;
  var
    lbl: TLabel;
    btn: TButton;
  begin
    lbl := AddLabel(text, prev);

    btn := TButton.Create(FOwner);
    btn.Parent := FOwner;
    btn.Caption := '...';
    btn.AutoSize := True;
    btn.Enabled := False;
    btn.OnClick := @OnEditorButtonClick;
    SetAnchor(btn, lbl, prev);
    btn.Anchors := [akTop, akRight];
    btn.AnchorSide[akRight].Control := FOwner;
    btn.AnchorSide[akRight].Side := asrRight;
    btn.BorderSpacing.Left := 0;
    FControls.Add(btn);

    Result := TTIEdit.Create(FOwner);
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Width := 300;
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.AnchorSide[akRight].Control := btn;
    Result.AnchorSide[akRight].Side := asrLeft;
    FControls.Add(Result);
  end;

  function AddSpinEdit(name, text: String;  min, max: Integer; prev: TControl): TTISpinEdit;
  var lbl: TLabel;
  begin
    lbl := AddLabel(text, prev);
    Result := TTISpinEdit.Create(FOwner);
    Result.MinValue := min;
    Result.MaxValue := max;
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Width := 100;
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.Anchors := [akTop, akLeft];
    FControls.Add(Result);
  end;

  function AddComboBox(name, text: String; prev: TControl): TTIComboBox;
  var lbl: TLabel;
  begin
    lbl := AddLabel(text, prev);
    Result := TTIComboBox.Create(FOwner);
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Width := 200;
    result.Style := csDropDownList;
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.Anchors := [akTop, akLeft];
    FControls.Add(Result);
  end;

  function AddCheckBox(name, text: String; prev: TControl): TTICheckBox;
  var lbl: TLabel;
  begin
    lbl := AddLabel(text, prev);
    Result := TTICheckBox.Create(FOwner);
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Caption := '';
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.Anchors := [akTop, akLeft];
    FControls.Add(Result);
  end;

var
  prev: TControl = nil;
  propList: PPropList;
  typeData: PTypeData;
  cnt, i: Integer;
  maxLabelWidth: Integer = -1;
  maxLabel: TLabel;

begin
  typeData := GetTypeData(TWebsiteModuleSettings.ClassInfo);
  GetMem(propList, typeData^.PropCount * SizeOf(Pointer));
  cnt := GetPropList(TWebsiteModuleSettings.ClassInfo, propList);
  for i := 0 to cnt-1 do
    with propList^[i]^ do
      case PropType^.Kind of
      tkInteger:
        prev := AddSpinEdit(Name, Name, 1, High(Integer), prev);
      tkAString:
        prev := AddEdit(Name, Name, prev);
      tkEnumeration:
        prev := AddComboBox(Name, Name, prev);
      tkBool:
        prev := AddCheckBox(Name, Name, prev);
      end;
  FreeMem(propList);

  for i := 0 to FControls.Count - 1 do
    if (FControls[i] is TLabel) then
      with TLabel(FControls[i]) do
        if maxLabelWidth < Canvas.TextWidth(Caption) then begin
          maxLabelWidth := Canvas.TextWidth(Caption);
          maxLabel := TLabel(FControls[i]);
        end;

  for i := 0 to FControls.Count - 1 do
    if not (FControls[i] is TLabel) and not (FControls[i] is TButton) then
      with TControl(FControls[i]) do begin
        AnchorSide[akLeft].Control := maxLabel;
        AnchorSide[akLeft].Side := asrRight;
      end;
end;

procedure TSettingsView.UpdateView;
var
  i: Integer;
begin
  for i := 0 to FControls.Count-1 do begin
    if FControls[i] is TTIEdit then
      TTIEdit(FControls[i]).Link.TIObject := FSettings
    else if FControls[i] is TTISpinEdit then
      TTISpinEdit(FControls[i]).Link.TIObject := FSettings
    else if FControls[i] is TTIComboBox then
      TTIComboBox(FControls[i]).Link.TIObject := FSettings
    else if FControls[i] is TTICheckBox then
      TTICheckBox(FControls[i]).Link.TIObject := FSettings;
    if Assigned(FSettings) then
      TControl(FControls[i]).Enabled := True;
  end;
end;

end.

