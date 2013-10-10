unit ImportFavoritesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, BaseUnit, lazutf8classes, ListForm;

type

  { TImportFavorites }

  TImportFavorites = class(TForm)
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

    procedure Run;
  public
    { public declarations }
  end;

implementation

uses
  mainunit, silentthreads;

{$R *.lfm}

{ TImportFavorites }

{ ----- private methods ----- }

procedure TImportFavorites.DMDHandle;
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
           (Pos('comic/_/comics/double-marriage-r3713', urlList.Strings[i]) = 0) AND
           (Pos('comic/_/comics/kokuhaku-sakano-anri-r3870', urlList.Strings[i]) = 0) AND
           (Pos('comic/_/comics/yamada-kun-to-7-nin-no-majo-r3770', urlList.Strings[i]) = 0) then
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

procedure TImportFavorites.Run;
begin
  case cbSoftware.ItemIndex of
    0: DMDHandle;
  end;

  if MainForm.silentThreadCount > 0 then
    MainForm.sbMain.Panels[1].Text:= 'Loading: '+IntToStr(MainForm.silentThreadCount)
  else
    MainForm.sbMain.Panels[1].Text:= '';
end;

{ ----- public methods ----- }

procedure TImportFavorites.FormCreate(Sender: TObject);
begin
  Caption:= MainForm.btFavoritesImport.Caption;
  btImport.Caption:= stImport;
  edPath.Text:= stSoftwarePath;
  btCancel.Caption:= stCancel;
  lbSelectSoftware.Caption:= stSoftware;
end;

procedure TImportFavorites.btBrowseClick(Sender: TObject);
begin
  dlgPath.InitialDir:= CorrectFilePath(CorrectFile(edPath.Text));
  if dlgPath.Execute then
    edPath.Text:= CorrectFilePath(CorrectFile(dlgPath.FileName));
end;

procedure TImportFavorites.btImportClick(Sender: TObject);
begin
  Run;
end;

procedure TImportFavorites.cbSoftwareChange(Sender: TObject);
begin
end;

end.

