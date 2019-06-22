unit frmTransferFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, Menus, ExtCtrls,
  VirtualTrees, uFavoritesManager, DBDataProcess;

type

  { TTransferFavoritesForm }

  TTransferFavoritesForm = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    cbWebsites: TComboBox;
    ckClearDownloadedChapters: TCheckBox;
    imgState: TImage;
    imgsState: TImageList;
    lbTransferTo: TLabel;
    rbAll: TRadioButton;
    rbValid: TRadioButton;
    rbInvalid: TRadioButton;
    vtFavs: TVirtualStringTree;
    procedure btCancelClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure cbWebsitesEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllChange(Sender: TObject);
    procedure vtFavsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFavsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFavsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtFavsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String
      );
  private
    FAllCount,
    FValidCount,
    FInvalidCount,
    FLastFilter,
    FLastWebsiteSelect: Integer;
    procedure UpdateFilterCount;
    procedure FilterState(const AState: Integer = 0);
    procedure FindMatchTitle;
  public
    procedure AddFav(const AFav: TFavoriteContainer);
  end;

  TFavsContainer = record
    Fav: TFavoriteContainer;
    NewLink: String;
    State: Integer;
  end;

  PFavContainer = ^TFavsContainer;

  { TFindMatchDBThread }

  TFindMatchDBThread = class(TThread)
  private
    FWebsite: String;
    procedure SyncBegin;
    procedure SyncEnd;
  protected
    procedure Execute; override;
  public
    Owner: TTransferFavoritesForm;
    constructor Create(AWebsite: String);
  end;

var
  TransferFavoritesForm: TTransferFavoritesForm;

resourcestring
  RS_ALL = 'All';
  RS_Valid = 'Valid';
  RS_Invalid = 'Invalid';

implementation

uses
  FMDVars, FMDOptions, db, frmCustomColor;

{$R *.lfm}

{ TFindMatchDBThread }

procedure TFindMatchDBThread.SyncBegin;
begin
  Owner.imgState.Visible := True;
  Owner.imgsState.GetBitmap(0,Owner.imgState.Picture.Bitmap);
  Owner.cbWebsites.Enabled := False;
  Owner.vtFavs.Enabled := False;
end;

procedure TFindMatchDBThread.SyncEnd;
begin
  Owner.imgsState.GetBitmap(1,Owner.imgState.Picture.Bitmap);
  Owner.cbWebsites.Enabled := True;
  Owner.vtFavs.Enabled := True;
  Owner.cbWebsites.SetFocus;

  Owner.UpdateFilterCount;

  if Owner.FLastFilter <> 0 then
  begin
    Owner.FLastFilter := -1;
    Owner.rbAllChange(nil);
  end;
end;

procedure TFindMatchDBThread.Execute;
var
  db: TDBDataProcess;
  node: PVirtualNode;
  data: PFavContainer;

begin
  Synchronize(@SyncBegin);
  Owner.FValidCount := 0;
  Owner.FInvalidCount := 0;
  db := TDBDataProcess.Create;
  try
    if db.Connect(FWebsite) then
    begin
      db.Table.ReadOnly := True;
      node := Owner.vtFavs.GetFirst();
      while Assigned(node) do
      begin
        data := Owner.vtFavs.GetNodeData(node);
        if data^.Fav.Website = db.Website then
        begin
          data^.NewLink := '';
          data^.State := 0;
        end
        else
        begin
          try
            db.Table.SQL.Text := 'SELECT link FROM ' + AnsiQuotedStr(db.TableName, '"') +
              ' WHERE title LIKE '+AnsiQuotedStr(data^.Fav.FavoriteInfo.Title, '"') + ' COLLATE NOCASE;';
            db.Table.Open;
            if db.Table.RecNo > 0 then
            begin
              data^.NewLink := db.Table.Fields[0].AsString;
              data^.State := 1;
              Inc(Owner.FValidCount);
            end
            else
            begin
              data^.NewLink := '';
              data^.State := 2;
              Inc(Owner.FInvalidCount);
            end;
          except
          end;
          db.Table.Close;
        end;
        node := Owner.vtFavs.GetNext(node);
      end;
    end;
    db.Connection.Connected := False;
  finally
    db.Free;
  end;
  Synchronize(@SyncEnd);
end;

constructor TFindMatchDBThread.Create(AWebsite: String);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FWebsite := AWebsite;
end;

{ TTransferFavoritesForm }

procedure TTransferFavoritesForm.FormCreate(Sender: TObject);
begin
  frmCustomColor.AddVT(vtFavs);
  cbWebsites.Items.Text := FormMain.cbSelectManga.Items.Text;
  FLastFilter := 0;
  FAllCount := 0;
  FValidCount := 0;
  FInvalidCount := 0;
  FLastWebsiteSelect := -1;
end;

procedure TTransferFavoritesForm.FormDestroy(Sender: TObject);
begin
  frmCustomColor.RemoveVT(vtFavs);
end;

procedure TTransferFavoritesForm.FormShow(Sender: TObject);
begin
  UpdateFilterCount;
end;

procedure TTransferFavoritesForm.rbAllChange(Sender: TObject);
begin
  if rbAll.Checked then
    FilterState(0)
  else if rbValid.Checked then
    FilterState(1)
  else if rbInvalid.Checked then
    FilterState(2);
end;

procedure TTransferFavoritesForm.btOKClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PFavContainer;
  dc: String;
  t: TFavoriteContainer;
begin
  Node := vtFavs.GetFirst();
  while Assigned(Node) do
  begin
    Data := vtFavs.GetNodeData(Node);
    // add new item and remove the old one
    if Data^.NewLink <> '' then
    begin
      with Data^.Fav.FavoriteInfo do
      begin
        if ckClearDownloadedChapters.Checked then
          dc := ''
        else
          dc := DownloadedChapterList;
        FavoriteManager.Add(
          Title,
          CurrentChapter,
          dc,
          cbWebsites.
          Text,
          SaveTo,
          Data^.NewLink);
      end;
      t := FavoriteManager.Items.Last;
      FavoriteManager.FreeAndDelete(Data^.Fav);
      Data^.Fav := t;
      if ckClearDownloadedChapters.Checked then
        t.Tag := 100; // get new chapterlist
    end;
    Node := vtFavs.GetNext(Node);
  end;
  ModalResult := mrOK;
end;

procedure TTransferFavoritesForm.cbWebsitesEditingDone(Sender: TObject);
begin
  FindMatchTitle;
end;

procedure TTransferFavoritesForm.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTransferFavoritesForm.vtFavsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PFavContainer;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TTransferFavoritesForm.vtFavsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PFavContainer;
begin
  if Column <> 0 then Exit;
  Data := Sender.GetNodeData(Node);
  if Data^.State = 0 then
    ImageIndex := -1
  else
    ImageIndex := Data^.State;
end;

procedure TTransferFavoritesForm.vtFavsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TFavsContainer);
end;

procedure TTransferFavoritesForm.vtFavsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PFavContainer;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    1: CellText := Data^.Fav.FavoriteInfo.Title;
    2: CellText := Data^.Fav.FavoriteInfo.Website;
  end;
end;

procedure TTransferFavoritesForm.UpdateFilterCount;
begin
  rbAll.Caption := RS_ALL + '(' + IntToStr(FAllCount) + ')';
  rbValid.Caption := RS_Valid + '(' + IntToStr(FValidCount) + ')';
  rbInvalid.Caption := RS_Invalid + '(' + IntToStr(FInvalidCount) + ')';
end;

procedure TTransferFavoritesForm.FilterState(const AState: Integer);
var
  Node: PVirtualNode;
  Data: PFavContainer;
begin
  if FLastFilter = AState then Exit;
  try
    vtFavs.BeginUpdate;
    Node := vtFavs.GetFirst();
    while Assigned(Node) do
    begin
      Data := vtFavs.GetNodeData(Node);
      if AState = 0 then
        vtFavs.IsVisible[Node] := True
      else
        vtFavs.IsVisible[Node] := Data^.State = AState;
      Node := vtFavs.GetNext(Node);
    end;
  finally
    vtFavs.EndUpdate;
  end;
  FLastFilter := AState;
end;

procedure TTransferFavoritesForm.FindMatchTitle;
begin
  if FLastWebsiteSelect = cbWebsites.ItemIndex then Exit;
  FLastWebsiteSelect := cbWebsites.ItemIndex;
  if not FileExists(DATA_FOLDER + cbWebsites.Text + DBDATA_EXT) then
    Exit;
  with TFindMatchDBThread.Create(cbWebsites.Text) do
  begin
    Owner := Self;
    Start;
  end;
end;

procedure TTransferFavoritesForm.AddFav(const AFav: TFavoriteContainer);
var
  Node: PVirtualNode;
  Data: PFavContainer;
begin
  Node := vtFavs.AddChild(nil);
  Data := vtFavs.GetNodeData(Node);
  Data^.Fav := AFav;
  Data^.NewLink := '';
  Data^.State := 0;
  Inc(FAllCount);
end;

end.

