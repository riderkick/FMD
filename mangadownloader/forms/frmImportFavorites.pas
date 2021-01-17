{
        File: frmImportFavorites.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmImportFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, Buttons, DefaultTranslator, EditBtn,
  lazutf8classes, LazFileUtils, uBaseUnit, WebsiteModules, FMDOptions, RegExpr,
  frmNewChapter;

type

  { TImportFavorites }

  TImportFavorites = class(TForm)
    btImport: TBitBtn;
    btCancel: TBitBtn;
    cbSoftware: TComboBox;
    edPath: TDirectoryEdit;
    lbSelectSoftware: TLabel;
    procedure btImportClick(Sender: TObject);
  private
    { private declarations }
    procedure DMDHandle;
    procedure FMDHandle;

    procedure Run;
  public
    { public declarations }
  end;

resourcestring
  RS_ImportCompleted = 'Import completed.';
  RS_ListUnimportedCaption = 'List of unimported manga';

implementation

uses
  frmMain, uSilentThread, FMDVars;

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
  host,
  webs,
  path: String;
  i, m: Integer;
  regx: TRegExpr;
begin
  if NOT FileExistsUTF8(CleanAndExpandDirectory(edPath.Text) + 'Config/Bookmarks') then
    exit;

  list:= TStringList.Create;
  urlList:= TStringList.Create;
  mangaList:= TStringList.Create;
  unimportedMangas:= TStringList.Create;
  fstream:= TFileStreamUTF8.Create(CleanAndExpandDirectory(edPath.Text) + 'Config/Bookmarks', fmOpenRead);

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
    path:= CleanAndExpandDirectory(configfile.ReadString('saveto', 'SaveTo', ''));
    regx := TRegExpr.Create;
    try
      regx.Expression := REGEX_HOST;
      for i:= 0 to urlList.Count-1 do
      begin
        host := '';
        webs := '';
        host := LowerCase(regx.Replace(urlList[i], '$2', True));
        if host <> '' then
        begin
          m := Modules.LocateModuleByHost(host);
          if m > -1 then
            webs := Modules.Module[m].Website;
        end;

        if webs <> '' then
        begin
          SilentThreadManager.Add(
            MD_AddToFavorites,
            webs,
            mangaList[i],
            RemoveHostFromURL(urlList[i]),
            path);
        end
        else
          unimportedMangas.Add(mangaList.Strings[i] + ' <' + urlList.Strings[i] + '>');
      end;
    finally
      regx.Free;
    end;
  end;

  if unimportedMangas.Count > 0 then
  begin
    with TNewChapter.Create(Self) do try
      Caption := RS_ListUnimportedCaption;
      lbNotification.Caption := '';
      btCancel.Visible := False;
      btQueue.Visible := False;
      btDownload.Visible := True;
      btDownload.Caption := RS_BtnOK;
      mmMemo.Lines.Text := unimportedMangas.Text;
      ShowModal;
    finally
      Free;
    end;
  end;

  fstream.Free;
  list.Free;
  urlList.Free;
  mangaList.Free;
  unimportedMangas.Free;
end;

procedure TImportFavorites.FMDHandle;
begin
  FavoriteManager.MergeWith(CleanAndExpandDirectory(edPath.Text) + 'works/favorites.ini');

  MessageDlg('', RS_ImportCompleted,
                 mtConfirmation, [mbYes], 0)
end;

procedure TImportFavorites.Run;
begin
  case cbSoftware.ItemIndex of
    0: DMDHandle;
    1: FMDHandle;
  end;
end;

{ ----- public methods ----- }

procedure TImportFavorites.btImportClick(Sender: TObject);
begin
  Run;
end;

end.

