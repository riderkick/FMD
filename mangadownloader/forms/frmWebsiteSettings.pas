unit frmWebsiteSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, RTTIGrids, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, PairSplitter, EditBtn, VirtualTrees;

type

  { TWebsiteSettingsForm }

  TWebsiteSettingsForm = class(TForm)
    edSearch: TEditButton;
    spMain: TPairSplitter;
    spList: TPairSplitterSide;
    spProps: TPairSplitterSide;
    pnTop: TPanel;
    prSettings: TTIPropertyGrid;
    vtWebsite: TVirtualStringTree;
    procedure edSearchButtonClick(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtWebsiteClick(Sender: TObject);
    procedure vtWebsiteCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtWebsiteFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtWebsiteGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private

  public
    procedure LoadWebsiteSettings;
  end;

var
  WebsiteSettingsForm: TWebsiteSettingsForm;

implementation

uses WebsiteModules, frmCustomColor;

type

  PWebsiteSettingsItem = ^TWebsiteSettingsItem;

  TWebsiteSettingsItem = record
    Website: String;
    Settings: TWebsiteModuleSettings;
  end;


{$R *.lfm}

{ TWebsiteSettingsForm }

procedure TWebsiteSettingsForm.FormCreate(Sender: TObject);
begin
  AddVT(vtWebsite);
  prSettings.PreferredSplitterX:=200;
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
      vtWebsite.IsVisible[node]:=Pos(s,AnsiUpperCase(PWebsiteSettingsItem(vtWebsite.GetNodeData(node))^.Website))<>0;
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
end;

procedure TWebsiteSettingsForm.vtWebsiteClick(Sender: TObject);
begin
  if vtWebsite.SelectedCount=0 then Exit;
  prSettings.TIObject:=PWebsiteSettingsItem(vtWebsite.GetNodeData(vtWebsite.GetFirstSelected()))^.Settings;
end;

procedure TWebsiteSettingsForm.vtWebsiteCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Result:=AnsiCompareStr(PWebsiteSettingsItem(Sender.GetNodeData(Node1))^.Website,
    PWebsiteSettingsItem(Sender.GetNodeData(Node2))^.Website);
end;

procedure TWebsiteSettingsForm.vtWebsiteFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Finalize(PWebsiteSettingsItem(Sender.GetNodeData(Node))^);
end;

procedure TWebsiteSettingsForm.vtWebsiteGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText:=PWebsiteSettingsItem(Sender.GetNodeData(Node))^.Website;
end;

procedure TWebsiteSettingsForm.LoadWebsiteSettings;
var
  i: Integer;
  node: PVirtualNode;
  data: PWebsiteSettingsItem;
begin
  vtWebsite.NodeDataSize:=SizeOf(TWebsiteSettingsItem);
  vtWebsite.BeginUpdate;
  for i:=0 to Modules.Count-1 do
    with Modules[i] do
    begin
      node:=vtWebsite.AddChild(nil);
      vtWebsite.ValidateNode(node,False);
      data:=vtWebsite.GetNodeData(node);
      data^.Website:=Website;
      data^.Settings:=Settings;
    end;
  vtWebsite.Sort(nil,0,sdAscending,false);
  vtWebsite.EndUpdate;
end;

end.

