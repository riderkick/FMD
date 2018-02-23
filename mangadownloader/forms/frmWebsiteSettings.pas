unit frmWebsiteSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, WebsiteModules, WebsiteModulesSettings,
  RTTIGrids, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PairSplitter, EditBtn, VirtualTrees, contnrs, RTTICtrls;

type

  { TSettingsView }

  TSettingsView = class
  private
    FSettings: TWebsiteModuleSettings;
    FOwner: TWinControl;
    FControls: TFPObjectList;
    procedure SetData(settings: TWebsiteModuleSettings);
    procedure CreateControls;
    procedure CreateControls(ci: Pointer; owner: TWinControl; controls: TFPObjectList); overload;
    procedure UpdateView;
    procedure OnEditorButtonClick(Sender: TObject);
  public
    constructor Create(owner: TWinControl);
    destructor Destroy; override;
    property Data: TWebsiteModuleSettings read FSettings write SetData;
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

procedure TSettingsView.OnEditorButtonClick(Sender: TObject);

  function FindEditor(controls: TFPObjectList): TTIEdit;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to controls.Count - 1 do begin
      if controls[i] is TFPObjectList then begin
        Result := FindEditor(TFPObjectList(controls[i]));
        if Assigned(Result) then Exit;
      end;
      if Sender = controls[i] then begin
        Result := TTIEdit(controls[i+1]);
        Exit;
      end;
    end;
  end;

var
  editor: TTIEdit;
  form: TStringsPropEditorFrm;
begin
  if not Assigned(FSettings) then Exit;
  form := TStringsPropEditorFrm.Create(FOwner);
  editor := FindEditor(FControls);
  form.Memo.Text := editor.Link.GetAsText;
  if form.ShowModal = mrOK then
    editor.Link.SetAsText(form.Memo.Text);
  form.Free;
end;

procedure TSettingsView.CreateControls(ci: Pointer; owner: TWinControl; controls: TFPObjectList);

  procedure SetAnchor(control, lbl, prev: TControl);
  begin
    control.Anchors := [akTop, akLeft, akRight];
    if prev <> nil then begin
      control.AnchorSide[akTop].Control := prev;
      control.AnchorSide[akTop].Side := asrBottom;
      control.BorderSpacing.Top := 5;
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
    Result := TLabel.Create(owner);
    Result.Parent := owner;
    Result.Caption := text;
    Result.AnchorSide[akLeft].Control := owner;
    Result.AnchorSide[akLeft].Side := asrLeft;
    Result.BorderSpacing.Left := 5;
    if prev <> nil then begin
      Result.AnchorSide[akTop].Control := prev;
      Result.AnchorSide[akTop].Side := asrBottom;
      Result.BorderSpacing.Top := 10;
    end
    else
      Result.Top := 5;
    controls.Add(Result);
  end;

  function AddEdit(name, text: String; prev: TControl): TTIEdit;
  var
    lbl: TLabel;
    btn: TButton;
  begin
    lbl := AddLabel(text, prev);
    btn := TButton.Create(owner);
    btn.Parent := owner;
    btn.Caption := '...';
    btn.AutoSize := True;
    btn.Enabled := False;
    btn.OnClick := @OnEditorButtonClick;
    SetAnchor(btn, lbl, prev);
    btn.Anchors := [akTop, akRight];
    btn.AnchorSide[akRight].Control := owner;
    btn.AnchorSide[akRight].Side := asrRight;
    btn.BorderSpacing.Left := 0;
    controls.Add(btn);

    Result := TTIEdit.Create(owner);
    Result.Parent := owner;
    Result.Link.TIObject := nil;
    Result.Link.TIPropertyName := name;
    Result.Width := 300;
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.AnchorSide[akRight].Control := btn;
    Result.AnchorSide[akRight].Side := asrLeft;
    controls.Add(Result);
  end;

  function AddSpinEdit(name, text: String;  min, max: Integer; prev: TControl): TTISpinEdit;
  var lbl: TLabel;
  begin
    lbl := AddLabel(text, prev);
    Result := TTISpinEdit.Create(owner);
    Result.MinValue := min;
    Result.MaxValue := max;
    Result.Parent := owner;
    Result.Link.TIObject := nil;
    Result.Link.TIPropertyName := name;
    Result.Width := 100;
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.Anchors := [akTop, akLeft];
    controls.Add(Result);
  end;

  function AddComboBox(name, text: String; prev: TControl): TTIComboBox;
  var lbl: TLabel;
  begin
    lbl := AddLabel(text, prev);
    Result := TTIComboBox.Create(owner);
    Result.Parent := owner;
    Result.Link.TIObject := nil;
    Result.Link.TIPropertyName := name;
    Result.Width := 200;
    result.Style := csDropDownList;
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.Anchors := [akTop, akLeft];
    controls.Add(Result);
  end;

  function AddCheckBox(name, text: String; prev: TControl): TTICheckBox;
  var lbl: TLabel;
  begin
    lbl := AddLabel(text, prev);
    Result := TTICheckBox.Create(owner);
    Result.Parent := owner;
    Result.Link.TIObject := nil;
    Result.Link.TIPropertyName := name;
    Result.Caption := '';
    Result.Enabled := False;
    SetAnchor(Result, lbl, prev);
    Result.Anchors := [akTop, akLeft];
    controls.Add(Result);
  end;

  function AddGroupBox(name: String; prev: TControl): TGroupBox;
  begin
    Result := TGroupBox.Create(owner);
    Result.Parent := owner;
    Result.Caption := name;
    Result.AutoSize:=true;
    Result.Anchors := [akTop, akLeft, akRight];
    Result.AnchorSide[akLeft].Control := owner;
    Result.AnchorSide[akLeft].Side := asrLeft;
    Result.BorderSpacing.Left := 5;
    Result.AnchorSide[akTop].Control := prev;
    Result.AnchorSide[akTop].Side := asrBottom;
    Result.BorderSpacing.Top := 5;
    Result.AnchorSide[akRight].Control := owner;
    Result.AnchorSide[akRight].Side := asrRight;
    Result.BorderSpacing.Right := 10;
    controls.Add(Result);
  end;

var
  prev: TControl;
  propList: PPropList;
  typeData: PTypeData;
  cnt, i: Integer;
  maxLabelWidth: Integer;
  maxLabel: TLabel;
  tmp: TFPObjectList;
  subci: Pointer;
  last: TObject;

begin
  typeData := GetTypeData(ci);
  GetMem(propList, typeData^.PropCount * SizeOf(Pointer));
  cnt := GetPropList(ci, propList);
  prev := nil;
  for i := 0 to cnt-1 do
    with propList^[i]^ do
      case PropType^.Kind of
      tkInteger:
        prev := AddSpinEdit(Name, Name, 0, High(Integer), prev);
      tkAString:
        prev := AddEdit(Name, Name, prev);
      tkEnumeration:
        prev := AddComboBox(Name, Name, prev);
      tkBool:
        prev := AddCheckBox(Name, Name, prev);
      tkClass: begin
        prev := AddGroupBox(Name, prev);
        tmp := TFPObjectList.Create(True);
        subci := GetClass(propList^[i]^.PropType^.Name).ClassInfo;
        CreateControls(subci, TWinControl(prev), tmp);
        controls.Add(tmp);
        end;
      end;

  FreeMem(propList);

  maxLabelWidth := -1;
  for i := 0 to controls.Count - 1 do
    if (controls[i] is TLabel) then
      with TLabel(controls[i]) do
        if maxLabelWidth < Canvas.TextWidth(Caption) then begin
          maxLabelWidth := Canvas.TextWidth(Caption);
          maxLabel := TLabel(controls[i]);
        end;

  for i := 0 to controls.Count - 1 do
    if not (controls[i] is TLabel) and
       not (controls[i] is TButton) and
       not (controls[i] is TGroupBox) and
       not (controls[i] is TFPObjectList)
    then
      with TControl(controls[i]) do begin
        AnchorSide[akLeft].Control := maxLabel;
        AnchorSide[akLeft].Side := asrRight;
      end;

  last := controls[controls.Count - 1];
  if last is TFPObjectList then
    last := controls[controls.Count - 2];
  TControl(last).BorderSpacing.Bottom := 10;
end;

procedure TSettingsView.CreateControls;
begin
  CreateControls(TWebsiteModuleSettings.ClassInfo, FOwner, FControls);
end;

procedure TSettingsView.UpdateView;

  procedure _UpdateView(controls: TFPObjectList; settings: TPersistent);
  var
    i: Integer;
    name: String;
    val: TPersistent;
  begin
    for i := 0 to controls.Count-1 do begin
      if controls[i] is TTIEdit then
        TTIEdit(controls[i]).Link.TIObject := settings
      else if controls[i] is TTISpinEdit then
        TTISpinEdit(controls[i]).Link.TIObject := settings
      else if controls[i] is TTIComboBox then
        TTIComboBox(controls[i]).Link.TIObject := settings
      else if controls[i] is TTICheckBox then
        TTICheckBox(controls[i]).Link.TIObject := settings
      else if controls[i] is TGroupBox then
        name := TControl(controls[i]).Caption
      else if controls[i] is TFPObjectList then begin
        val := nil;
        if Assigned(settings) then
          val := TPersistent(GetObjectProp(settings, name));
        _UpdateView(TFPObjectList(controls[i]), val);
      end;

      if Assigned(settings) and not (controls[i] is TFPObjectList) then
        TControl(controls[i]).Enabled := True;
    end;
  end;

begin
  _UpdateView(FControls, FSettings);
end;

end.

