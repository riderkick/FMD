unit frmTransferFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, Menus,
  VirtualTrees, uFavoritesManager, DBDataProcess;

type

  { TTransferFavoritesForm }

  TTransferFavoritesForm = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    cbWebsites: TComboBox;
    ckClearDownloadedChapters: TCheckBox;
    lbTransferTo: TLabel;
    rbAll: TRadioButton;
    rbValid: TRadioButton;
    rbInvalid: TRadioButton;
    vtFavs: TVirtualStringTree;
    procedure btCancelClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure cbWebsitesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vtFavsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String
      );
  private
    procedure SearchDBForMatchTitle;
  public
    Favs: TFavoriteContainers;
    DB: TDBDataProcess;
  end;

var
  TransferFavoritesForm: TTransferFavoritesForm;

implementation

uses
  FMDVars, FMDOptions, frmCustomColor;

{$R *.lfm}

{ TTransferFavoritesForm }

procedure TTransferFavoritesForm.FormCreate(Sender: TObject);
begin
  Favs := TFavoriteContainers.Create;
  DB := TDBDataProcess.Create;
  AddVT(vtFavs);
  cbWebsites.Items.Text := FormMain.cbSelectManga.Items.Text;
end;

procedure TTransferFavoritesForm.btOKClick(Sender: TObject);
begin
  Close;
end;

procedure TTransferFavoritesForm.cbWebsitesChange(Sender: TObject);
begin
  if not FileExists(DATA_FOLDER + cbWebsites.Text + DBDATA_EXT) then
    Exit;
  if DB.Open(cbWebsites.Text) then
    SearchDBForMatchTitle;
end;

procedure TTransferFavoritesForm.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTransferFavoritesForm.FormDestroy(Sender: TObject);
begin
  Favs.Free;
  DB.Free;
end;

procedure TTransferFavoritesForm.FormShow(Sender: TObject);
begin
  vtFavs.RootNodeCount := Favs.Count;
end;

procedure TTransferFavoritesForm.vtFavsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    1: CellText := Favs.Items[Node^.Index].FavoriteInfo.Title;
    2: CellText := Favs.Items[Node^.Index].FavoriteInfo.Website;
  end;
end;

procedure TTransferFavoritesForm.SearchDBForMatchTitle;
var
  i: Integer;
begin
  if DB.RecordCount = 0 then Exit;
  for i := 0 to Favs.Count - 1 do
  begin
    //if DB.Table.Locate('title', Favs.Items[i].FavoriteInfo.Title);
  end;
end;

end.

