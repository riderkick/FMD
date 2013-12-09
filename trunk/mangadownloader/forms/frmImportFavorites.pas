unit frmImportFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, lazutf8classes, frmImportList, uBaseUnit;

type

  { TfrmImportFavorites }

  TfrmImportFavorites = class(TForm)
    btImport: TBitBtn;
    btBrowse: TBitBtn;
    btCancel: TBitBtn;
    cbSoftware: TComboBox;
    edPath: TEdit;
    dlgPath: TSelectDirectoryDialog;
    lbSelectSoftware: TLabel;
    procedure btBrowseClick(Sender: TObject);
    procedure btImportClick(Sender: TObject);
    procedure cbSoftwareChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure DMDHandle;
    procedure FMDHandle;

    procedure Run;
  public
    { public declarations }
  end;

implementation

uses
  frmMain, uSilentThread;

{$R *.lfm}

{ TfrmImportFavorites }

{ ----- private methods ----- }

procedure TfrmImportFavorites.DMDHandle;
var
  fstream  : TFileStreamUTF8;
  unimportedMangas,
  list,
  urlList,
  mangaList: TStringList;
  path: String;
  i, j: Cardinal;
  isUnimported: Boolean;
begin
  if NOT FileExistsUTF8(CorrectFilePath(edPath.Text) + 'Config/Bookmarks') then
    exit;

  list:= TStringList.Create;
  urlList:= TStringList.Create;
  mangaList:= TStringList.Create;
  unimportedMangas:= TStringList.Create;
  fstream:= TFileStreamUTF8.Create(CorrectFilePath(edPath.Text) + 'Config/Bookmarks', fmOpenRead);

  list.LoadFromStream(fstream);
  if list.Count > 0 then
  begin
    for i:= 0 to list.Count-1 do
    begin
      if Pos('<MangaLink>', list.Strings[i]) > 0 then
        urlList.Add(GetString(list.Strings[i], '<MangaLink>', '</MangaLink>'));
      if Pos('<MangaName>', list.Strings[i]) > 0 then
        mangaList.Add(StringFilter(GetString(list.Strings[i], '<MangaName>', '</MangaName>')));
    end;
  end;

  if urlList.Count > 0 then
  begin
    path:= CorrectFilePath(MainForm.options.ReadString('saveto', 'SaveTo', ''));
    for i:= 0 to urlList.Count-1 do
    begin
      urlList.Strings[i]:=
        StringReplace(urlList.Strings[i], 'http://mangafox.com', WebsiteRoots[MANGAFOX_ID,1], []);
      urlList.Strings[i]:=
        StringReplace(urlList.Strings[i], 'http://www.mangafox.com', WebsiteRoots[MANGAFOX_ID,1], []);
      urlList.Strings[i]:=
        StringReplace(urlList.Strings[i], 'http://www.batoto.com', WebsiteRoots[BATOTO_ID,1], []);
      isUnimported:= TRUE;
      for j:= 0 to High(WebsiteRoots) do
      begin
        if (Pos(UpCase(WebsiteRoots[j,1]), UpCase(urlList.Strings[i])) > 0) AND
           (Pos('comic/_/comics/double-marriage-r3713', urlList.Strings[i]) = 0) then
        begin
          CreateAddToFavThread(
            WebsiteRoots[j,0],
            mangaList.Strings[i],
            StringReplace(urlList.Strings[i], WebsiteRoots[j,1], '', []),
            path);
          Sleep(16);
          isUnimported:= FALSE;
          break;
        end;
      end;
      if isUnimported then
        unimportedMangas.Add(mangaList.Strings[i] + ' <' + urlList.Strings[i] + '>');
    end;
  end;

  if unimportedMangas.Count > 0 then
  begin
    ImportList.mmList.Lines.Text:= unimportedMangas.Text;
    ImportList.Show;
  end;

  fstream.Free;
  list.Free;
  urlList.Free;
  mangaList.Free;
  unimportedMangas.Free;
end;

procedure TfrmImportFavorites.FMDHandle;
begin
  MainForm.favorites.MergeWith(CorrectFilePath(edPath.Text) + 'works/favorites.ini');

  MessageDlg('', stImportCompleted,
                 mtConfirmation, [mbYes], 0)
end;

procedure TfrmImportFavorites.Run;
begin
  case cbSoftware.ItemIndex of
    0: DMDHandle;
    1: FMDHandle;
  end;

  if MainForm.silentThreadCount > 0 then
    MainForm.sbMain.Panels[1].Text:= 'Loading: '+IntToStr(MainForm.silentThreadCount)
  else
    MainForm.sbMain.Panels[1].Text:= '';
end;

{ ----- public methods ----- }

procedure TfrmImportFavorites.FormCreate(Sender: TObject);
begin
  Caption:= MainForm.btFavoritesImport.Caption;
  btImport.Caption:= stImport;
  edPath.Text:= stSoftwarePath;
  btCancel.Caption:= stCancel;
  lbSelectSoftware.Caption:= stSoftware;
end;

procedure TfrmImportFavorites.btBrowseClick(Sender: TObject);
begin
  dlgPath.InitialDir:= CorrectFilePath(edPath.Text);
  if dlgPath.Execute then
    edPath.Text:= CorrectFilePath(dlgPath.FileName);
end;

procedure TfrmImportFavorites.btImportClick(Sender: TObject);
begin
  Run;
end;

procedure TfrmImportFavorites.cbSoftwareChange(Sender: TObject);
begin
end;

end.

