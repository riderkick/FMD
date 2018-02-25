unit frmWebsiteSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, VirtualPropertyGrid, frmCustomColor, Forms,
  Controls, PairSplitter, EditBtn, VirtualTrees, uBaseUnit;

type

  { TWebsiteSettingsForm }

  TWebsiteSettingsForm = class(TForm)
    edSearch: TEditButton;
    edSearchProperty: TEditButton;
    spMain: TPairSplitter;
    spList: TPairSplitterSide;
    spProps: TPairSplitterSide;
    vtWebsite: TVirtualStringTree;
    procedure edSearchButtonClick(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchPropertyButtonClick(Sender: TObject);
    procedure edSearchPropertyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtWebsiteCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtWebsiteFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtWebsiteGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
  public
    SettingsView: TVirtualPropertyGrid;
    procedure LoadWebsiteSettings;
  end;

var
  WebsiteSettingsForm: TWebsiteSettingsForm;

implementation

{$R *.lfm}

{ TWebsiteSettingsForm }

procedure TWebsiteSettingsForm.FormCreate(Sender: TObject);
begin
  AddVT(vtWebsite);
  SettingsView := TVirtualPropertyGrid.Create(Self);
  with SettingsView do
  begin
    Parent := spProps;
    Align := alClient;
    AutoFullExpand := True;
    CleanEnumName := True;
    Header.Columns[0].Width := 300;
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toThemeAware, toUseExplorerTheme];
  end;
end;

procedure TWebsiteSettingsForm.edSearchChange(Sender: TObject);
begin
  SearchOnVT(vtWebsite, edSearch.Text);
end;

procedure TWebsiteSettingsForm.edSearchPropertyButtonClick(Sender: TObject);
begin
  edSearchProperty.Clear;
end;

procedure TWebsiteSettingsForm.edSearchPropertyChange(Sender: TObject);
begin
  SearchOnVT(SettingsView, edSearchProperty.Text);
end;

procedure TWebsiteSettingsForm.edSearchButtonClick(Sender: TObject);
begin
  edSearch.Clear;
end;

procedure TWebsiteSettingsForm.FormDestroy(Sender: TObject);
begin
  RemoveVT(vtWebsite);
end;

procedure TWebsiteSettingsForm.vtWebsiteCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Result := AnsiCompareStr(PModuleContainer(Sender.GetNodeData(Node1))^.Website,
    PModuleContainer(Sender.GetNodeData(Node2))^.Website);
end;

procedure TWebsiteSettingsForm.vtWebsiteFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  SettingsView.TIObject := PModuleContainer(Sender.GetNodeData(Node))^.Settings;
end;

procedure TWebsiteSettingsForm.vtWebsiteGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := PModuleContainer(Sender.GetNodeData(Node))^.Website;
end;

procedure TWebsiteSettingsForm.LoadWebsiteSettings;
var
  i: Integer;
begin
  vtWebsite.NodeDataSize := SizeOf(TModuleContainer);
  vtWebsite.BeginUpdate;
  for i := 0 to Modules.Count - 1 do
    vtWebsite.AddChild(nil, Modules[i]);
  vtWebsite.Sort(nil, 0, sdAscending, False);
  vtWebsite.EndUpdate;
end;


end.
