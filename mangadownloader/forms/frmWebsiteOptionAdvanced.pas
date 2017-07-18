unit frmWebsiteOptionAdvanced;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  LCLProc, Grids, Menus, VirtualTrees, FMDOptions, frmWebsiteSelection;

type

  TNameValue = record
    Name,
    Value: String;
  end;
  PNameValue = ^TNameValue;

  { TWebsiteOptionAdvancedForm }

  TWebsiteOptionAdvancedForm = class(TForm)
    imglstpmCokies: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    pcUpdateList: TPageControl;
    pcDownloads: TPageControl;
    pcAdvanced: TPageControl;
    pmCookies: TPopupMenu;
    tsMaxThreadsPerTask: TTabSheet;
    tsDirectoryPageNumber: TTabSheet;
    tsNumberofThreads: TTabSheet;
    tsUpdateList: TTabSheet;
    tsDownloads: TTabSheet;
    tsCookies: TTabSheet;
    tsUserAgent: TTabSheet;
    vtCookies: TVirtualStringTree;
    vtDownloadMaxThreadsPerTask: TVirtualStringTree;
    vtUpdateListDirectoryPageNumber: TVirtualStringTree;
    vtUpdateListNumberOfThreads: TVirtualStringTree;
    vtUserAgent: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure vtCookiesColumnDblClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure vtCookiesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtCookiesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vtCookiesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtCookiesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtCookiesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtCookiesHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vtCookiesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vtCookiesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
  private
    { private declarations }
    procedure LoadFromFileToVT(const AVT: TVirtualStringTree; const ASection: String);
    procedure GetWebsite(const AVT: TVirtualStringTree; const S: TStrings);
  public
    { public declarations }
  end;

var
  WebsiteOptionAdvancedForm: TWebsiteOptionAdvancedForm;

implementation

{$R *.lfm}

{ TWebsiteOptionAdvancedForm }

procedure TWebsiteOptionAdvancedForm.FormCreate(Sender: TObject);
begin
  LoadFromFileToVT(vtCookies, 'Cookies');
  LoadFromFileToVT(vtUserAgent, 'UserAgent');
  LoadFromFileToVT(vtDownloadMaxThreadsPerTask, 'DownloadMaxThreadsPerTask');
  LoadFromFileToVT(vtUpdateListDirectoryPageNumber, 'UpdateListDirectoryPageNumber');
  LoadFromFileToVT(vtUpdateListNumberOfThreads, 'UpdateListNumberOfThreads');
end;

procedure TWebsiteOptionAdvancedForm.MenuItem1Click(Sender: TObject);
var
  Data: PNameValue;
  Node: PVirtualNode;
begin
  if Screen.ActiveControl is TVirtualStringTree then
    with TWebsiteSelectionForm.Create(Self) do
      try
        GetWebsite(TVirtualStringTree(Screen.ActiveControl), cbWebsites.Items);
        if (ShowModal = mrOk) and (cbWebsites.Text <> '') then
          with TVirtualStringTree(Screen.ActiveControl) do
          begin
            Node := AddChild(nil);
            Data := GetNodeData(Node);
            Data^.Name := cbWebsites.Text;
            advancedfile.WriteString(DefaultText, cbWebsites.Text, '');
            EditNode(Node, 1);
          end;
      finally
        Free;
      end;
end;

procedure TWebsiteOptionAdvancedForm.MenuItem2Click(Sender: TObject);
begin
  if Screen.ActiveControl is TVirtualStringTree then
    with TVirtualStringTree(Screen.ActiveControl) do
      EditNode(FocusedNode, 1);
end;

procedure TWebsiteOptionAdvancedForm.MenuItem4Click(Sender: TObject);
var
  Data: PNameValue;
begin
  if Screen.ActiveControl is TVirtualStringTree then
    with TVirtualStringTree(Screen.ActiveControl) do
    begin
      Data := GetNodeData(FocusedNode);
      advancedfile.DeleteKey(DefaultText, Data^.Name);
      DeleteNode(FocusedNode);
    end;
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  if Column <> 0 then
    Sender.EditNode(Sender.FocusedNode, Column);
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PNameValue;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: Result := CompareStr(Data1^.Name, Data2^.Name);
    1: Result := CompareStr(Data1^.Value, Data2^.Value);
  end;
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column <> 0;
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PNameValue;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNameValue);
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PNameValue;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0: CellText := Data^.Name;
    1: CellText := Data^.Value;
  end;
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Sender.SortColumn := Column;
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not (Sender is TVirtualStringTree) then Exit;
  with TVirtualStringTree(Sender) do
    if (Key = VK_RETURN) and (FocusedColumn <> 0) then
      EditNode(FocusedNode, FocusedColumn);
end;

procedure TWebsiteOptionAdvancedForm.vtCookiesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  Data: PNameValue;
begin
  if Column = 0 then Exit;
  Data := Sender.GetNodeData(Node);
  if Data^.Value <> NewText then
  begin
    Data^.Value := NewText;
    advancedfile.WriteString(TVirtualStringTree(Sender).DefaultText, Data^.Name, NewText);
  end;
end;

procedure TWebsiteOptionAdvancedForm.LoadFromFileToVT(const AVT: TVirtualStringTree;
  const ASection: String);
var
  s: TStringList;
  i: Integer;
  Node: PVirtualNode;
  Data: PNameValue;
begin
  if AVT = nil then Exit;
  if ASection = '' then Exit;
  AVT.Clear;
  AVT.DefaultText := ASection;
  s := TStringList.Create;
  try
    advancedfile.ReadSectionRaw(ASection, s);
    if s.Count > 0 then
    begin
      AVT.BeginUpdate;
      try
        for i := 0 to s.Count - 1 do
        begin
          Node := AVT.AddChild(nil);
          Data := AVT.GetNodeData(Node);
          Data^.Name := s.Names[i];
          Data^.Value := s.ValueFromIndex[i];
        end;
        AVT.Header.AutoFitColumns(False, smaUseColumnOption, 0, 0);
        AVT.Header.SortColumn := 0;
        AVT.Header.SortDirection := sdAscending;
        AVT.SortTree(AVT.Header.SortColumn, AVT.Header.SortDirection);
      finally
        AVT.EndUpdate;
      end;
    end;
  finally
    s.Free;
  end;
end;

procedure TWebsiteOptionAdvancedForm.GetWebsite(const AVT: TVirtualStringTree;
  const S: TStrings);
var
  Data: PNameValue;
  Node: PVirtualNode;
  i, p: Integer;
begin
  if AVT = nil then Exit;
  if S = nil then Exit;
  S.Clear;
  if AvailableWebsite.Count > 0 then
  begin
    S.BeginUpdate;
    try
      for i := 0 to AvailableWebsite.Count - 1 do
        S.Add(AvailableWebsite.Names[i]);
      if AVT.RootNodeCount > 0 then
      begin
        Node := AVT.GetFirst();
        while Assigned(Node) do
        begin
          Data := AVT.GetNodeData(Node);
          p := S.IndexOf(Data^.Name);
          if p > -1 then
            S.Delete(p);
          Node := AVT.GetNext(Node);
        end;
      end;
    finally
      S.EndUpdate;
    end;
  end;
end;

end.

