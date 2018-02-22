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

uses frmCustomColor, typinfo;

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

procedure TSettingsView.CreateControls;

const LEFT_OFFSET = 250;

  function AddLabel(text: String): TLabel;
  begin
    Result := TLabel.Create(FOwner);
    Result.Parent := FOwner;
    Result.Caption := text;
    FControls.Add(Result);
  end;

  function AddEdit(name, text: String; prev: TControl): TTIEdit;
  begin
    AddLabel(text);
    Result := TTIEdit.Create(FOwner);
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Width := 300;
    Result.Enabled := False;
    FControls.Add(Result);
  end;

  function AddSpinEdit(name, text: String;  min, max: Integer; prev: TControl): TTISpinEdit;
  begin
    AddLabel(text);
    Result := TTISpinEdit.Create(FOwner);
    Result.MinValue := min;
    Result.MaxValue := max;
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Width := 100;
    Result.Enabled := False;
    FControls.Add(Result);
  end;

  function AddComboBox(name, text: String; prev: TControl): TTIComboBox;
  begin
    AddLabel(text);
    Result := TTIComboBox.Create(FOwner);
    Result.Parent := FOwner;
    Result.Link.TIObject := FSettings;
    Result.Link.TIPropertyName := name;
    Result.Width := 200;
    result.Style := csDropDownList;
    Result.Enabled := False;
    FControls.Add(Result);
  end;

var
  prev: TControl = nil;
  propList: PPropList;
  typeData: PTypeData;
  cnt, i: Integer;

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
      end;
  FreeMem(propList);
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
      TTIComboBox(FControls[i]).Link.TIObject := FSettings;
    if Assigned(FSettings) then
      TControl(FControls[i]).Enabled := True;
  end;
end;

end.

