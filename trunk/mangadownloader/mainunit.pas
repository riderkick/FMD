{
        File: mainunit.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit mainunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Grids, ColorBox, ActnList, Buttons, CheckLst, Spin, Menus,
  customdrawncontrols, VirtualTrees, RichMemo, IniFiles, Process,
  baseunit, data, types, downloads, favorites, LConvEncoding,
  updatelist, lclproc{, ActiveX};

type

  { TMainForm }

  TMainForm = class(TForm)
    btFavoritesCheckNewChapter: TBitBtn;
    btBrowse: TBitBtn;
    btDownload: TBitBtn;
    btRemoveFilterLarge: TBitBtn;
    btSearch: TBitBtn;
    btRemoveFilter: TBitBtn;
    btFilter: TButton;
    btFilterReset: TButton;
    btOptionApply: TButton;
    btUpdateList: TBitBtn;
    cbOptionUseProxy: TCheckBox;
    cbSelectManga: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox29: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox31: TCheckBox;
    CheckBox32: TCheckBox;
    CheckBox33: TCheckBox;
    CheckBox34: TCheckBox;
    CheckBox35: TCheckBox;
    CheckBox36: TCheckBox;
    CheckBox37: TCheckBox;
    CheckBox38: TCheckBox;
    CheckBox39: TCheckBox;
    CheckBox4: TCheckBox;
    cbOnlyNew: TCheckBox;
    cbAddAsStopped: TCheckBox;
    cbAddToFavorites: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    cbFilterStatus: TComboBox;
    clbChapterList: TCheckListBox;
    cbLanguages: TComboBox;
    edFilterSummary: TEdit;
    edFilterTitle: TEdit;
    edFilterAuthors: TEdit;
    edFilterArtists: TEdit;
    edCustomGenres: TEdit;
    edOptionHost: TEdit;
    edOptionPass: TEdit;
    edOptionPort: TEdit;
    edOptionUser: TEdit;
    edSaveTo: TLabeledEdit;
    edSearch: TEdit;
    gbOptionProxy: TGroupBox;
    ImageList: TImageList;
    imCover: TImage;
    edOptionDefaultPath: TLabeledEdit;
    lbFilterCustomGenres: TLabel;
    lbFilterSummary: TLabel;
    lbFilterStatus: TLabel;
    lbFilterTitle: TLabel;
    lbFilterAuthors: TLabel;
    lbFilterArtists: TLabel;
    lbMode: TLabel;
    lbOptionHost: TLabel;
    lbOptionMaxParallel: TLabel;
    lbOptionMaxRetry: TLabel;
    lbOptionMaxThread: TLabel;
    lbOptionPass: TLabel;
    lbOptionPort: TLabel;
    lbOptionUser: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miDown: TMenuItem;
    miUp: TMenuItem;
    miOpenFolder: TMenuItem;
    miFavoritesRemove: TMenuItem;
    miMangaListAddToFavorites: TMenuItem;
    miFavoritesChangeCurrentChapter: TMenuItem;
    miFavoritesChangeSaveTo: TMenuItem;
    miDownloadRemoveFinishedTasks: TMenuItem;
    miDownloadStop: TMenuItem;
    miChapterListCheckAll: TMenuItem;
    miChapterListUncheckSelected: TMenuItem;
    miChapterListCheckSelected: TMenuItem;
    miI1: TMenuItem;
    miDownloadRemuse: TMenuItem;
    miDownloadRemove: TMenuItem;
    miChapterListUncheckAll: TMenuItem;
    pmChapterList: TPopupMenu;
    pnOptions: TPageControl;
    pnChapterList: TPanel;
    pnFilter: TPanel;
    pnGenres: TPanel;
    pcMain: TPageControl;
    pnInfomation: TPanel;
    pnMainLeft: TPanel;
    pmDownload: TPopupMenu;
    pmFavorites: TPopupMenu;
    pmMangaList: TPopupMenu;
    rbOne: TRadioButton;
    rbAll: TRadioButton;
    rgOptionCompress: TRadioGroup;
    rmInformation: TRichMemo;
    sbFilter: TScrollBox;
    sbInformation: TScrollBox;
    sbDownloadConnections: TScrollBox;
    dlgSaveTo: TSelectDirectoryDialog;
    seOptionMaxParallel: TSpinEdit;
    seOptionMaxRetry: TSpinEdit;
    seOptionMaxThread: TSpinEdit;
    spInfos: TSplitter;
    spMainSplitter: TSplitter;
    sbMain: TStatusBar;
    tmBackup: TTimer;
    TrayIcon: TTrayIcon;
    tsLanguage: TTabSheet;
    tsFavorites: TTabSheet;
    tsSaveTo: TTabSheet;
    tsConnections: TTabSheet;
    tsOption: TTabSheet;
    tsFilter: TTabSheet;
    tsInformation: TTabSheet;
    tsDownload: TTabSheet;
    vtFavorites: TVirtualStringTree;
    vtDownload: TVirtualStringTree;
    vtMangaList: TVirtualStringTree;

    procedure btUpdateListClick(Sender: TObject);
    procedure cbSelectMangaChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure btBrowseClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btFavoritesCheckNewChapterClick(Sender: TObject);
    procedure btOptionApplyClick(Sender: TObject);

    procedure btFilterClick(Sender: TObject);
    procedure btFilterResetClick(Sender: TObject);
    procedure btRemoveFilterClick(Sender: TObject);

    procedure btSearchClick(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);

    procedure cbAddAsStoppedChange(Sender: TObject);
    procedure cbOptionUseProxyChange(Sender: TObject);
    procedure clbChapterListKeyPress(Sender: TObject; var Key: char);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miDownClick(Sender: TObject);

    procedure miFavoritesRemoveClick(Sender: TObject);
    procedure miMangaListAddToFavoritesClick(Sender: TObject);
    procedure miFavoritesChangeCurrentChapterClick(Sender: TObject);
    procedure miFavoritesChangeSaveToClick(Sender: TObject);

    procedure miChapterListCheckSelectedClick(Sender: TObject);
    procedure miChapterListUncheckSelectedClick(Sender: TObject);
    procedure miChapterListCheckAllClick(Sender: TObject);
    procedure miChapterListUncheckAllClick(Sender: TObject);

    procedure miDownloadRemoveFinishedTasksClick(Sender: TObject);
    procedure miDownloadRemuseClick(Sender: TObject);
    procedure miDownloadRemoveClick(Sender: TObject);
    procedure miDownloadStopClick(Sender: TObject);
    procedure miOpenFolderClick(Sender: TObject);
    procedure miUpClick(Sender: TObject);

    procedure pcMainChange(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure vtDownloadDragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vtDownloadDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: TObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vtDownloadDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);

    procedure vtDownloadFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtDownloadGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtDownloadGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtDownloadInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtFavoritesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtFavoritesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

    procedure vtMangaListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMangaListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtMangaListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtMangaListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtMangaListDblClick(Sender: TObject);

    procedure tmBackupTimer(Sender: TObject);
  public
    // only for batoto: the directory page from the last time we check the site
    batotoLastDirectoryPage: Cardinal;

    isUpdating  : Boolean;
    options     : TIniFile;
    favorites   : TFavoriteManager;
    dataProcess : TDataProcess;
    mangaInfo   : TMangaInfo;
    DLManager   : TDownloadManager;
    updateList  : TUpdateMangaManagerThread;
    cover       : TPicture;
    ticks       : Cardinal;
    backupTicks : Cardinal;

    // en: Too lazy to add it one by one
    // vi: Lười ...
    procedure InitCheckboxes;

    procedure AddChapterNameToList;

    // en: Add text to TRichMemo
    // vi: Thêm văn bản vào TRichMemo
    procedure AddTextToInfo(title, infoText: String);

    // en: Show manga information
    // vi: Xuất thông tin về manga
    procedure ShowInformation;

    // en: Load config from config.ini
    // vi: Lấy thông tin từ config.ini
    procedure LoadOptions;

    // en: Search manga from current manga list
    // vi: Tìm kiếm manga từ manga list
    procedure SearchMangaList;

    //
    procedure UpdateVtDownload;
    procedure UpdateVtFavorites;

    // en: Load form information, like previous position, size, ...
    // vi: nạp thông tin của form
    procedure LoadFormInformation;
    procedure SaveFormInformation;

    // en: load language file
    // vi: nạp ngôn ngữ
    procedure LoadLanguage(const pos: Integer);

    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  isUpdating := FALSE;
 // ticks      := GetTickCount;
 // backupTicks:= GetTickCount;
  oldDir     := GetCurrentDir;
  oldDir     := CorrectFile(oldDir);

  dataProcess:= TDataProcess.Create;

  DLManager  := TDownloadManager.Create;

  favorites  := TFavoriteManager.Create;
  favorites.OnUpdateFavorite:= UpdateVtFavorites;
  favorites.OnUpdateDownload:= UpdateVtDownload;
  favorites.DLManager       := DLManager;

  options    := TIniFile.Create(CONFIG_FOLDER + CONFIG_FILE);
  options.CacheUpdates:= FALSE;

  LoadOptions;
  LoadFormInformation;

  ShowInformation;
  mangaInfo.chapterName := TStringList.Create;
  mangaInfo.chapterLinks:= TStringList.Create;
  cover:= TPicture.Create;

  vtMangaList.NodeDataSize := SizeOf(TMangaListItem);
  vtMangaList.RootNodeCount:= dataProcess.filterPos.Count;

  vtDownload.NodeDataSize  := SizeOf(TDownloadInfo)-4;
  vtDownload.RootNodeCount := DLManager.containers.Count;

  vtFavorites.NodeDataSize := SizeOf(TFavoriteInfo);
  vtFavorites.RootNodeCount:= favorites.Count;

  InitCheckboxes;

  Application.HintHidePause:= 10000;

  lbMode.Caption:= Format(stModeAll, [dataProcess.filterPos.Count]);

  if pcMain.PageIndex = 4 then
    pcMain.PageIndex:= 0;

  // load icons
  // btUpdateList.Glyph.LoadFromFile('images/download_18.png');
  currentWebsite:= cbSelectManga.Items.Strings[cbSelectManga.ItemIndex];
  DLManager.CheckAndActiveTaskAtStartup;
  TrayIcon.Show;
end;

procedure TMainForm.cbOptionUseProxyChange(Sender: TObject);
begin
  gbOptionProxy.Enabled:= cbOptionUseProxy.Checked;
end;

procedure TMainForm.clbChapterListKeyPress(Sender: TObject; var Key: char);
var
  i: Cardinal;
begin
  if (key = #13) OR (key = #32) then
  begin
    if clbChapterList.MultiSelect then
    begin
      for i:= 0 to clbChapterList.Count-1 do
        if clbChapterList.Selected[i] then
          clbChapterList.Checked[i]:= NOT clbChapterList.Checked[i];
    end;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  case WindowState of
    wsMinimized:
      begin
        MainForm.Hide;
        TrayIcon.Show;
      end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if MessageDlg('', stDlgQuit,
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    options.WriteInteger('general', 'batotoLastDirectoryPage', batotoLastDirectoryPage);
    SaveFormInformation;
    DLManager.StopAllDownloadTasksForExit;
    dataProcess.SaveToFile;
    dataProcess.Destroy;
    CloseAction:= caFree;
  end
  else
    CloseAction:= caNone;
end;

// -----

procedure TMainForm.btDownloadClick(Sender: TObject);
var
  day, month, year: Word;
  i, pos  : Cardinal;
  isCreate: Boolean = FALSE;
begin
  if mangaInfo.chapterName.Count = 0 then exit;
  for i:= 0 to mangaInfo.chapterName.Count - 1 do
    if clbChapterList.Checked[i] then
    begin
      if NOT isCreate then
      begin
        DLManager.AddTask;
        pos:= DLManager.containers.Count-1;
        isCreate:= TRUE;
      end;
      DLManager.containers.Items[pos].mangaSiteID:= GetMangaSiteID(mangaInfo.website);
      DLManager.containers.Items[pos].chapterName .Add(Format('%.4d - %s', [i+1, mangaInfo.chapterName.Strings[i]]));
      DLManager.containers.Items[pos].chapterLinks.Add(mangaInfo.chapterLinks.Strings[i]);
    end;
  if NOT isCreate then exit;

  if cbAddAsStopped.Checked then
  begin
    DLManager.containers.Items[pos].downloadInfo.Status:= stStop;
    DLManager.containers.Items[pos].Status:= STATUS_STOP;
  end
  else
  begin
    DLManager.containers.Items[pos].downloadInfo.Status:= stWait;
    DLManager.containers.Items[pos].Status:= STATUS_WAIT;
  end;
  DLManager.containers.Items[pos].currentDownloadChapterPtr:= 0;
  DLManager.containers.Items[pos].downloadInfo.title  := mangaInfo.title;
  DLManager.containers.Items[pos].downloadInfo.Website:= mangaInfo.website;
  DLManager.containers.Items[pos].downloadInfo.SaveTo := CorrectFile(edSaveTo.Text);
  DecodeDate(Now, year, month, day);
  DLManager.containers.Items[pos].downloadInfo.dateTime:= IntToStr(Month)+'/'+IntToStr(Day)+'/'+IntToStr(Year);

  // Add to favorites
  if cbAddToFavorites.Checked then
  begin
    favorites.Add(mangaInfo.title, IntToStr(mangaInfo.numChapter),
                  mangaInfo.website, CorrectFile(edSaveTo.Text), mangaInfo.link);
    vtFavorites.NodeDataSize := SizeOf(TFavoriteInfo);
    vtFavorites.RootNodeCount:= favorites.Count;
  end;

  UpdateVtDownload;

  DLManager.Backup;
  DLManager.CheckAndActiveTask;
 // DLManager.containers.Items[pos].thread.isSuspended:= FALSE;
  pcMain.PageIndex:= 0;
end;

// -----

procedure TMainForm.btFavoritesCheckNewChapterClick(Sender: TObject);
begin
  favorites.Run;
end;

// -----

procedure TMainForm.btBrowseClick(Sender: TObject);
begin
  dlgSaveTo.InitialDir:= CorrectFile(edSaveTo.Text);
  if dlgSaveTo.Execute then
    edSaveTo.Text:= CorrectFile(dlgSaveTo.FileName);
end;

// -----

procedure TMainForm.btUpdateListClick(Sender: TObject);
begin
  if (NOT isUpdating) OR (NOT Assigned(updateList)) OR (updateList.isTerminated) then
  begin
    isUpdating:= TRUE;
    updateList:= TUpdateMangaManagerThread.Create;
    updateList.numberOfThreads:= 4;
    updateList.websites.Add(cbSelectManga.Items[cbSelectManga.ItemIndex]);
    updateList.isSuspended:= FALSE;
  end
  else
    MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0)
end;

procedure TMainForm.cbSelectMangaChange(Sender: TObject);
begin
  if currentWebsite <> cbSelectManga.Items.Strings[cbSelectManga.ItemIndex] then
  begin
    dataProcess.RemoveFilter;
    dataProcess.SaveToFile;
    dataProcess.Free;
    dataProcess:= TDataProcess.Create;
    dataProcess.LoadFromFile(cbSelectManga.Items.Strings[cbSelectManga.ItemIndex]);
    vtMangaList.Clear;
    vtMangaList.RootNodeCount:= dataProcess.filterPos.Count;
    lbMode.Caption:= Format(stModeAll, [dataProcess.filterPos.Count]);
    currentWebsite:= cbSelectManga.Items[cbSelectManga.ItemIndex];
    dataProcess.website:= cbSelectManga.Items[cbSelectManga.ItemIndex];
  end;
end;

// -----

procedure TMainForm.btSearchClick(Sender: TObject);
begin
  SearchMangaList;
end;

// -----

procedure TMainForm.btRemoveFilterClick(Sender: TObject);
begin
  if dataProcess.isFiltered then
  begin
    dataProcess.RemoveFilter;
    vtMangaList.Clear;
    vtMangaList.RootNodeCount:= dataProcess.filterPos.Count;
    lbMode.Caption:= Format(stModeAll, [dataProcess.filterPos.Count]);
  end;
end;

// -----

procedure TMainForm.btFilterClick(Sender: TObject);
var
  checkGenres, uncheckGenres: TStringList;
  i: Cardinal;
begin
  checkGenres  := TStringList.Create;
  uncheckGenres:= TStringList.Create;
  for i:= 0 to 38 do
  begin
    if TCheckBox(pnGenres.Controls[i]).State = cbChecked then
      checkGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption)
    else
    if TCheckBox(pnGenres.Controls[i]).State = cbUnchecked then
      uncheckGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption);
  end;
  CustomGenres(checkGenres, edCustomGenres.Text);
  if dataProcess.Filter(checkGenres, uncheckGenres,
                        edFilterTitle.Text, edFilterAuthors.Text,
                        edFilterArtists.Text, IntToStr(cbFilterStatus.ItemIndex),
                        edFilterSummary.Text, rbAll.Checked) then
  begin
    lbMode.Caption:=  Format(stModeFilter, [dataProcess.filterPos.Count]);
    vtMangaList.Clear;
    vtMangaList.RootNodeCount:= dataProcess.filterPos.Count;
  end;
  uncheckGenres.Free;
  checkGenres  .Free;
end;

procedure TMainForm.btFilterResetClick(Sender: TObject);
var
  i: Cardinal;
begin
  for i:= 0 to 38 do
    TCheckBox(pnGenres.Controls[i]).State:= cbGrayed;
  edFilterTitle  .Caption:= '';
  edFilterAuthors.Caption:= '';
  edFilterArtists.Caption:= '';
  edFilterSummary.Caption:= '';
  cbFilterStatus .ItemIndex:= 2;
end;

procedure TMainForm.edSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin
    SearchMangaList;
    edSearch.SetFocus;
  end;
end;

// ----- vtMangaList popup menu -----

procedure TMainForm.miMangaListAddToFavoritesClick(Sender: TObject);
var
  pos: Cardinal;
begin
  if NOT Assigned(vtMangaList.FocusedNode) then exit;
  pos:= vtMangaList.FocusedNode.Index;
  favorites.Add(dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_NAME],
                dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_NUMCHAPTER],
                dataProcess.website,
                options.ReadString('saveto', 'SaveTo', DEFAULT_PATH),
                dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_LINK]);
  UpdateVtFavorites;
  pcMain.PageIndex:= 3;
end;

// ----- vtFavorites popup menu -----

procedure TMainForm.miFavoritesRemoveClick(Sender: TObject);
begin
  if favorites.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
                mtInformation, [mbYes, mbNo], 0);
    exit;
  end;
  if NOT Assigned(vtFavorites.FocusedNode) then exit;
 // if MessageDlg('Question', 'Are you sure you want to remove?',
 //               mtConfirmation, [mbYes, mbNo], 0) = mrNo then exit;
  favorites.Remove(vtFavorites.FocusedNode.Index);
  UpdateVtFavorites;
end;

procedure TMainForm.miFavoritesChangeCurrentChapterClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  if favorites.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
                mtInformation, [mbYes, mbNo], 0);
    exit;
  end;
  if NOT Assigned(vtFavorites.FocusedNode) then exit;
  s:= favorites.favoriteInfo[vtFavorites.FocusedNode.Index].currentChapter;
  repeat
    if InputQuery('', stDlgTypeInNewChapter, s) then
  until TryStrToInt(s, i);
  if s <> favorites.favoriteInfo[vtFavorites.FocusedNode.Index].currentChapter then
  begin
    favorites.favoriteInfo[vtFavorites.FocusedNode.Index].currentChapter:= s;
    vtFavorites.Clear;
    vtFavorites.RootNodeCount:= favorites.Count;
    favorites.Backup;
  end;
end;

procedure TMainForm.miFavoritesChangeSaveToClick(Sender: TObject);
begin
  if favorites.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
                mtInformation, [mbYes, mbNo], 0);
    exit;
  end;
  if NOT Assigned(vtFavorites.FocusedNode) then exit;
  if InputQuery('', stDlgTypeInNewSavePath, favorites.favoriteInfo[vtFavorites.FocusedNode.Index].SaveTo) then
  begin
    favorites.favoriteInfo[vtFavorites.FocusedNode.Index].SaveTo:=
      CorrectFile(favorites.favoriteInfo[vtFavorites.FocusedNode.Index].SaveTo);
    vtFavorites.Clear;
    vtFavorites.RootNodeCount:= favorites.Count;
    favorites.Backup;
  end;
end;

// ----- clbChapterList popup menu -----

procedure TMainForm.miChapterListCheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
begin
  if clbChapterList.MultiSelect then
    for i:= 0 to clbChapterList.Count-1 do
      if clbChapterList.Selected[i] then
        clbChapterList.Checked[i]:= TRUE;
end;

procedure TMainForm.miChapterListUncheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
begin
  if clbChapterList.MultiSelect then
    for i:= 0 to clbChapterList.Count-1 do
      if clbChapterList.Selected[i] then
        clbChapterList.Checked[i]:= FALSE;
end;

procedure TMainForm.miChapterListCheckAllClick(Sender: TObject);
var
  i: Cardinal;
begin
  if clbChapterList.Count > 0 then
    for i:= 0 to clbChapterList.Count-1 do
      clbChapterList.Checked[i]:= TRUE;
end;

procedure TMainForm.miChapterListUncheckAllClick(Sender: TObject);
var
  i: Cardinal;
begin
  if clbChapterList.Count > 0 then
    for i:= 0 to clbChapterList.Count-1 do
      clbChapterList.Checked[i]:= FALSE;
end;

// ----- vtDownload popup menu -----

procedure TMainForm.miUpClick(Sender: TObject);
begin
  if DLManager.MoveUp(vtDownload.FocusedNode.Index) then
  begin
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.miDownClick(Sender: TObject);
begin
  if DLManager.MoveDown(vtDownload.FocusedNode.Index) then
  begin
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.miDownloadRemoveFinishedTasksClick(Sender: TObject);
begin
  if MessageDlg('', stDlgRemoveFinishTasks,
                mtConfirmation, [mbYes, mbNo], 0) = mrNo then exit;
  DLManager.RemoveAllFinishedTasks;
  vtDownload.Clear;
  vtDownload.RootNodeCount:= DLManager.containers.Count;
  DLManager.Backup;
end;

procedure TMainForm.miDownloadRemuseClick(Sender: TObject);
var i: cardinal;
begin
  if NOT Assigned(vtDownload.FocusedNode) then exit;
  if DLManager.containers.Items[vtDownload.FocusedNode.Index].Status <> STATUS_STOP then exit;
  DLManager.containers.Items[vtDownload.FocusedNode.Index].Status:= STATUS_WAIT;
  DLManager.containers.Items[vtDownload.FocusedNode.Index].downloadInfo.Status:= stWait;
  // bad coding - print waiting string to the screen
  vtDownload.Repaint;
  //DLManager.CheckAndActiveTask;
  if NOT DLManager.CanActiveTask then exit;
  DLManager.ActiveTask(vtDownload.FocusedNode.Index);
  // bad coding - print preparing/downloading string to the screen
  vtDownload.Repaint;
  DLManager.Backup;
end;

procedure TMainForm.miDownloadRemoveClick(Sender: TObject);
begin
  if NOT Assigned(vtDownload.FocusedNode) then exit;
  if MessageDlg('', stDlgRemoveTask,
                mtConfirmation, [mbYes, mbNo], 0) = mrNo then exit;
  DLManager.RemoveTask(vtDownload.FocusedNode.Index);
  vtDownload.Clear;
  vtDownload.RootNodeCount:= DLManager.containers.Count;
  DLManager.Backup;
end;

// Download table's popup menu

procedure TMainForm.miDownloadStopClick(Sender: TObject);
begin
  if NOT Assigned(vtDownload.FocusedNode) then exit;
  DLManager.StopTask(vtDownload.FocusedNode.Index);
  vtDownload.Repaint;
end;

procedure TMainForm.miOpenFolderClick(Sender: TObject);
var
  Process: TProcess;
begin
  {$IFDEF WIN32}
  if NOT Assigned(vtDownload.FocusedNode) then exit;
  Process:= TProcess.Create(nil);
  Process.CommandLine:= 'explorer.exe /e, '+
                         StringReplace(DLManager.containers.Items[vtDownload.FocusedNode.Index].downloadInfo.SaveTo, '/', '\', [rfReplaceAll]);
  Process.Execute;
  Process.Free;
  {$ENDIF}
end;

procedure TMainForm.pcMainChange(Sender: TObject);
  procedure UpdateOptions;
  begin
    seOptionMaxParallel.Value:= options.ReadInteger('connections', 'NumberOfTasks', 1);
    seOptionMaxThread.Value:= options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
    seOptionMaxRetry.Value:= options.ReadInteger('connections', 'Retry', 0);
    cbOptionUseProxy.Checked:= options.ReadBool('connections', 'UseProxy', FALSE);
    edOptionHost.Text:= options.ReadString('connections', 'Host', '');
    edOptionPass.Text:= options.ReadString('connections', 'Pass', '');
    edOptionPort.Text:= options.ReadString('connections', 'Port', '');
    edOptionUser.Text:= options.ReadString('connections', 'User', '');

    edOptionDefaultPath.Text  := options.ReadString('saveto', 'SaveTo', DEFAULT_PATH);
    rgOptionCompress.ItemIndex:= options.ReadInteger('saveto', 'Compress', 0);
  end;

begin
  if pcMain.TabIndex = 4 then
    UpdateOptions;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  WindowState:= wsNormal;
  MainForm.Show;
 // TrayIcon.Hide;
  TrayIcon.Show;
end;

procedure TMainForm.vtDownloadDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed:= TRUE;
end;

procedure TMainForm.vtDownloadDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: TObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  pSource, pTarget: PVirtualNode;
  attMode: TVTNodeAttachMode;
begin
  pSource:= TVirtualStringTree(Source).FocusedNode;
  pTarget:= Sender.DropTargetNode;
  case Mode of
    dmNowhere        : attMode:= amNoWhere;
    dmAbove          :
      begin
        attMode:= amInsertBefore;
        DLManager.Swap(pSource^.Index, pTarget^.Index);
      end;
    dmOnNode, dmBelow:
      begin
        attMode:= amInsertAfter;
        DLManager.Swap(pSource^.Index, pTarget^.Index);
      end;
  end;
  Sender.MoveTo(pSource, pTarget, attMode, False);

end;

procedure TMainForm.vtDownloadDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept:= (Source = Sender);
end;

// Download table

procedure TMainForm.vtDownloadFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  data: PDownloadInfo;
begin
  data:= Sender.GetNodeData(Node);
  if Assigned(data) then
  {begin
    data^.title   := '';
    data^.status  := '';
    data^.progress:= '';
    data^.website := '';
    data^.saveTo  := '';
    data^.dateTime:= '';
  end; }
    Finalize(data^);
end;

procedure TMainForm.vtDownloadGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  l,
  i: Cardinal;
begin
  l:= DLManager.containers.Items[Node.Index].chapterLinks.Count;
  if l > 0 then
  begin
    HintText:= '';
    if l < 5 then
    begin
      for i:= 0 to l-1 do
      HintText:= HintText + #10#13 +
        DLManager.containers.Items[Node.Index].chapterName.Strings[i] + ' : ' +
        DLManager.containers.Items[Node.Index].chapterLinks.Strings[i];
    end
    else
    begin
      for i:= 0 to 1 do
      HintText:= HintText + #10#13 +
        DLManager.containers.Items[Node.Index].chapterName.Strings[i] + ' : ' +
        DLManager.containers.Items[Node.Index].chapterLinks.Strings[i];
      HintText:= HintText + #10#13 + '...';
      for i:= l-2 to l-1 do
      HintText:= HintText + #10#13 +
        DLManager.containers.Items[Node.Index].chapterName.Strings[i] + ' : ' +
        DLManager.containers.Items[Node.Index].chapterLinks.Strings[i];
    end;
  end;
end;

procedure TMainForm.vtDownloadGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  data: PDownloadInfo;
  pos : Cardinal;
begin
  with Sender do
  begin
    pos:= Node.Index;
    data:= Sender.GetNodeData(Node);
    if (DLManager.containers.Count <> 0) then
    if (Assigned(data)) AND ((DLManager.containers.Items[pos] <> nil) OR (NOT DLManager.containers.Items[pos].thread.isTerminated)) then
    begin
      data^.title   := DLManager.containers.Items[pos].downloadInfo.title;
      data^.status  := DLManager.containers.Items[pos].downloadInfo.Status;
      data^.progress:= DLManager.containers.Items[pos].downloadInfo.Progress;
      data^.website := DLManager.containers.Items[pos].downloadInfo.Website;
      data^.saveTo  := DLManager.containers.Items[pos].downloadInfo.SaveTo;
      data^.dateTime:= DLManager.containers.Items[pos].downloadInfo.dateTime;
      case Column of
        0: CellText:= data^.title;
        1: CellText:= data^.status;
        2: CellText:= data^.Progress;
        3: CellText:= data^.website;
        4: CellText:= data^.saveTo;
        5: CellText:= data^.dateTime;
      end;
    end;
  end;
end;

procedure TMainForm.vtDownloadInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PDownloadInfo;
  pos : Cardinal;
begin
  with Sender do
  begin
    pos:= Node.Index;
    data:= GetNodeData(Node);
    if (DLManager.containers.Count <> 0) then
      if (DLManager.containers.Items[pos] <> nil) OR (NOT DLManager.containers.Items[pos].thread.isTerminated) then
      begin
        data.title   := DLManager.containers.Items[pos].downloadInfo.title;
        data.status  := DLManager.containers.Items[pos].downloadInfo.Status;
        data.progress:= DLManager.containers.Items[pos].downloadInfo.Progress;
        data.website := DLManager.containers.Items[pos].downloadInfo.Website;
        data.saveTo  := DLManager.containers.Items[pos].downloadInfo.SaveTo;
        data.dateTime:= DLManager.containers.Items[pos].downloadInfo.dateTime;
      end;
  end;
end;

// vtFavorites

procedure TMainForm.vtFavoritesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  data: PFavoriteInfo;
  pos : Cardinal;
begin
  data:= Sender.GetNodeData(Node);
  if Assigned(data) then
    case Column of
      0: CellText:= data^.title;
      1: CellText:= data^.currentChapter;
      2: CellText:= data^.website;
      3: CellText:= data^.saveTo;
    end;
end;

procedure TMainForm.vtFavoritesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PFavoriteInfo;
  pos : Cardinal;
begin
  with Sender do
  begin
    pos := Node.Index;
    data:= GetNodeData(Node);
    data.title         := favorites.favoriteInfo[pos].title;
    data.currentChapter:= favorites.favoriteInfo[pos].currentChapter;
    data.website       := favorites.favoriteInfo[pos].website;
    data.saveTo        := favorites.favoriteInfo[pos].saveTo;
  end;
end;

// options

procedure TMainForm.btOptionApplyClick(Sender: TObject);
begin
  options.WriteInteger('connections', 'NumberOfTasks', seOptionMaxParallel.Value);
  options.WriteInteger('connections', 'NumberOfThreadsPerTask', seOptionMaxThread.Value);
  options.WriteInteger('connections', 'Retry', seOptionMaxRetry.Value);
  options.WriteBool   ('connections', 'UseProxy', cbOptionUseProxy.Checked);
  options.WriteString ('connections', 'Host', edOptionHost.Text);
  options.WriteString ('connections', 'Pass', edOptionPass.Text);
  options.WriteString ('connections', 'Port', edOptionPort.Text);
  options.WriteString ('connections', 'User', edOptionUser.Text);

  options.WriteString ('saveto', 'SaveTo', edOptionDefaultPath.Text);
  options.WriteInteger('saveto', 'Compress', rgOptionCompress.ItemIndex);
  DLManager.compress:= rgOptionCompress.ItemIndex;

  options.WriteInteger('languages', 'Select', cbLanguages.ItemIndex);
  options.UpdateFile;

  if cbOptionUseProxy.Checked then
  begin
    Host:= edOptionHost.Text;
    Pass:= edOptionPass.Text;
    Port:= edOptionPort.Text;
    User:= edOptionUser.Text;
  end;

  LoadLanguage(cbLanguages.ItemIndex);

  DLManager.maxDLTasks         := seOptionMaxParallel.Value;
  DLManager.maxDLThreadsPerTask:= seOptionMaxThread.Value;
  DLManager.retryConnect       := seOptionMaxRetry.Value;
end;

procedure TMainForm.cbAddAsStoppedChange(Sender: TObject);
begin
  options.WriteBool('general', 'AddAsStopped', cbAddAsStopped.Checked);
end;

// manga list

procedure TMainForm.vtMangaListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var data: PMangaListItem;
begin
  data:= Sender.GetNodeData(Node);
  if Assigned(data) then
    Finalize(data^);
end;

procedure TMainForm.vtMangaListGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
begin
  HintText:=
    infoGenres+': '+dataProcess.Param[dataProcess.filterPos.Items[Node.Index], DATA_PARAM_GENRES]+#10#10#13+
    infoSummary+': '+#10#13+
    PrepareSummaryForHint(dataProcess.Param[dataProcess.filterPos.Items[Node.Index], DATA_PARAM_SUMMARY]);
end;

procedure TMainForm.vtMangaListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var data: PMangaListItem;
begin
  data:= Sender.GetNodeData(Node);
  if Assigned(data) then
    CellText:= data.text;
end;

procedure TMainForm.vtMangaListInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PMangaListItem;
  pos : Cardinal;
begin
  with Sender do
  begin
    pos:= dataProcess.filterPos.Items[Node.Index];
    data:= GetNodeData(Node);
    data.text:= dataProcess.Param[pos, DATA_PARAM_NAME]+
                ' ('+
                dataProcess.Param[pos, DATA_PARAM_NUMCHAPTER]+')';
  end;
end;

procedure TMainForm.vtMangaListDblClick(Sender: TObject);
begin
  ShowInformation;
end;

procedure TMainForm.InitCheckboxes;
var
  i: Cardinal;
begin
  for i:= 0 to 38 do
  begin
    TCheckBox(pnGenres.Controls[i]).Caption:= Genre[i];
    TCheckBox(pnGenres.Controls[i]).State:= cbGrayed;
  end;
end;

procedure TMainForm.AddChapterNameToList;
var
  i: Cardinal;
begin
  clbChapterList.Clear;
  if mangaInfo.chapterName.Count <> 0 then
    for i:= 0 to mangaInfo.chapterName.Count - 1 do
      clbChapterList.Items.Add(Format('%.4d - %s', [i+1, mangaInfo.chapterName.Strings[i]]));
end;

procedure TMainForm.AddTextToInfo(title, infoText: String);
var
  fp: TFontParams;
begin
  TrimLeft(infoText);
  if infoText <> '' then
    with rmInformation do
    begin
      Lines.Add(title);
      {$IFDEF WIN32}
      GetTextAttributes(0, fp);
      fp.Style:= [fsBold, fsUnderline];
      fp.Size := fp.Size+1;
      SetTextAttributes(
        Length(UTF8ToUTF16(Lines.Text))-
        Length(UTF8ToUTF16(Lines[Lines.Count-1]))-Lines.Count-1,
        Length(UTF8ToUTF16(Lines[Lines.Count-1])),
        fp);
      {$ENDIF}
      Lines.Add(infoText);
    end;
end;

procedure TMainForm.ShowInformation;
var
  data  : PMangaListItem;
  cp    : TPoint;
  root  : String;
  header: array [0..3] of Byte;

  function  GetMangaInfo(const website, URL: String): Boolean;
  var
    Info: TMangaInformation;
  begin
    Result:= FALSE;
    Info:= TMangaInformation.Create;
      if Info.GetInfoFromURL(URL, website)<>NO_ERROR then
      begin
        Info.Free;
        exit;
      end;
      Info.SyncInfoToData(DataProcess, vtMangaList.FocusedNode.Index);
      TransferMangaInfo(mangaInfo, Info.mangaInfo);
    Info.Free;
    Result:= TRUE;
  end;

begin
  if cbSelectManga.ItemIndex < 0 then exit;
  if NOT vtMangaList.Focused then exit;

  // ---------------------------------------------------

  if cbSelectManga.Items[cbSelectManga.ItemIndex] = ANIMEA_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, ANIMEA_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= ANIMEA_ROOT + root;
  end
  else
  if cbSelectManga.Items[cbSelectManga.ItemIndex] = MANGAHERE_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, MANGAHERE_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= MANGAHERE_ROOT + root;
  end
  else
  if cbSelectManga.Items[cbSelectManga.ItemIndex] = MANGAINN_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, MANGAINN_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= MANGAINN_ROOT + root;
  end
  else
  if cbSelectManga.Items[cbSelectManga.ItemIndex] = OURMANGA_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, OURMANGA_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= OURMANGA_ROOT + root;
  end
  else
  if cbSelectManga.Items[cbSelectManga.ItemIndex] = BATOTO_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, BATOTO_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= BATOTO_ROOT + root;
  end
  else
  if cbSelectManga.Items[cbSelectManga.ItemIndex] = VNSHARING_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, VNSHARING_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= VNSHARING_ROOT + root;
  end
  else
  if cbSelectManga.Items[cbSelectManga.ItemIndex] = HENTAI2READ_NAME then
  begin
    root:= dataProcess.Param[
      dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];
    if NOT GetMangaInfo(root, HENTAI2READ_NAME) then
    begin
      MessageDlg('', stDlgCannotGetMangaInfo,
                 mtInformation, [mbYes], 0);
      exit;
    end;
    root:= HENTAI2READ_ROOT + root;
  end;

  // ---------------------------------------------------

  pcMain.PageIndex:= 1;
  edSaveTo.Text:= options.ReadString('saveto', 'SaveTo', '');

  with rmInformation do
  begin
    imCover.Picture.Assign(nil);
    cover.Clear;

    Clear;

   { if (Pos('.jpg', mangaInfo.coverLink)>0) OR
       (Pos('.jpeg', mangaInfo.coverLink)>0) then  }
      if GetPage(TObject(cover), mangaInfo.coverLink, 1) then
      begin
     { cover.(header[0], 4);
      if (header[0] = JPG_HEADER[0]) AND
         (header[1] = JPG_HEADER[1]) AND
         (header[2] = JPG_HEADER[2]) then
     }   imCover.Picture.Assign(cover);
      end;

    data:= vtMangaList.GetNodedata(vtMangaList.FocusedNode);

    mangaInfo.title:= dataProcess.Param[dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_NAME];
    mangaInfo.link := dataProcess.Param[dataProcess.filterPos.Items[vtMangaList.FocusedNode.Index], DATA_PARAM_LINK];

    AddTextToInfo(infoName, mangaInfo.title+#10#13);
    AddTextToInfo(infoAuthors, mangaInfo.authors+#10#13);
    AddTextToInfo(infoArtists, mangaInfo.artists+#10#13);
    AddTextToInfo(infoGenres , mangaInfo.genres +#10#13);
    if mangaInfo.status = '0' then
      AddTextToInfo(infoStatus , 'Completed'+#10#13)
    else
      AddTextToInfo(infoStatus , 'Ongoing'+#10#13);
    AddTextToInfo(infoLink, root+#10#13);
    AddTextToInfo(infoSummary, StringBreaks(mangaInfo.summary));
    cp.X:= 0; cp.Y:= 0; CaretPos:= cp;
  end;
  AddChapterNameToList;
end;

procedure TMainForm.LoadOptions;
var
  i: Cardinal;
  s: String;
  l: TStringList;
begin
  l:= TStringList.Create;
  s:= options.ReadString('general', 'MangaList', '');
  GetParams(l, s);

  if l.Count <> 0 then
  begin
    cbSelectManga.Items.Clear;
    for i:= 0 to l.Count-1 do
      cbSelectManga.Items.Add(l.Strings[i]);
    dataProcess.LoadFromFile(cbSelectManga.Items.Strings[0]);
    dataProcess.website:= cbSelectManga.Items[0];
    cbSelectManga.ItemIndex:= 0;
  end;

  if options.ReadBool('connections', 'UseProxy', FALSE) then
  begin
    Host:= options.ReadString('connections', 'Host', '');
    Pass:= options.ReadString('connections', 'Pass', '');
    Port:= options.ReadString('connections', 'Port', '');
    User:= options.ReadString('connections', 'User', '');
  end;
 // cbLanguages.ItemIndex := options.ReadInteger('languages', 'Select', 0);

  batotoLastDirectoryPage:= options.ReadInteger('general', 'batotoLastDirectoryPage', 244);
  cbAddAsStopped.Checked := options.ReadBool('general', 'AddAsStopped', FALSE);
  LoadLanguage(options.ReadInteger('languages', 'Select', 0));

  DLManager.maxDLTasks         := options.ReadInteger('connections', 'NumberOfTasks', 1);
  DLManager.maxDLThreadsPerTask:= options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
  DLManager.retryConnect       := options.ReadInteger('connections', 'Retry', 0);
  DLManager.compress           := options.ReadInteger('saveto', 'Compress', 0);

  l.Free;
end;

procedure TMainForm.SearchMangaList;
var
  xNode  : PVirtualNode;
  data   : PMangaListItem;
  name   : String;
  endSearch,
  current: Cardinal;
begin
  name:= LowerCase(edSearch.text);
  if NOT Assigned(vtMangaList.FocusedNode) then
  begin
    xNode:= vtMangaList.GetFirst;
    endSearch:= vtMangaList.GetLast.index;
  end
  else
  begin
    xNode:= vtMangaList.FocusedNode;
    current:= xNode.index;
    if current = 0 then
      endSearch:= vtMangaList.GetLast.index
    else
      endSearch:= current-1;
  end;
  repeat
    if xNode = vtMangaList.GetLast then
      xNode:= vtMangaList.GetFirst
    else
      xNode:= vtMangaList.GetNext(xNode);
    data:= vtMangaList.GetNodedata(xNode);
    if Pos(name, LowerCase(data^.text))>0 then
    begin
      vtMangaList.FocusedNode:= xNode;
      vtMangaList.Selected[xNode]:= TRUE;
      vtMangaList.Expanded[xNode]:= TRUE;
      vtMangaList.Refresh;
      vtMangaList.SetFocus;
      break;
    end;
    if xNode.Index = endSearch then
      MessageDlg('Info', '"'+name+'" not found!',
                 mtInformation, [mbYes], 0);
  until xNode.Index = endSearch;
end;

procedure TMainForm.UpdateVtDownload;
begin
  vtDownload.Clear;
  vtDownload.RootNodeCount:= DLManager.containers.Count;
end;

procedure TMainForm.UpdateVtFavorites;
begin
  vtFavorites.Clear;
  vtFavorites.RootNodeCount:= favorites.Count;
end;

procedure TMainForm.LoadFormInformation;
begin
  pcMain.PageIndex:= options.ReadInteger('form', 'pcMainPageIndex', 0);

  MainForm.Left  := options.ReadInteger('form', 'MainFormLeft', 0);
  MainForm.Top   := options.ReadInteger('form', 'MainFormTop', 0);
  MainForm.Width := options.ReadInteger('form', 'MainFormWidth', 640);
  MainForm.Height:= options.ReadInteger('form', 'MainFormHeight', 480);

  vtDownload.Header.Columns.Items[0].Width:= options.ReadInteger('form', 'vtDownload0Width', 50);
  vtDownload.Header.Columns.Items[1].Width:= options.ReadInteger('form', 'vtDownload1Width', 50);
  vtDownload.Header.Columns.Items[2].Width:= options.ReadInteger('form', 'vtDownload2Width', 50);
  vtDownload.Header.Columns.Items[3].Width:= options.ReadInteger('form', 'vtDownload3Width', 50);
  vtDownload.Header.Columns.Items[4].Width:= options.ReadInteger('form', 'vtDownload4Width', 50);
  vtDownload.Header.Columns.Items[5].Width:= options.ReadInteger('form', 'vtDownload5Width', 50);

  vtFavorites.Header.Columns.Items[0].Width:= options.ReadInteger('form', 'vtFavorites0Width', 50);
  vtFavorites.Header.Columns.Items[1].Width:= options.ReadInteger('form', 'vtFavorites1Width', 50);
  vtFavorites.Header.Columns.Items[2].Width:= options.ReadInteger('form', 'vtFavorites2Width', 50);
  vtFavorites.Header.Columns.Items[3].Width:= options.ReadInteger('form', 'vtFavorites3Width', 50);
end;

procedure TMainForm.SaveFormInformation;
begin
  options.WriteInteger('form', 'pcMainPageIndex', pcMain.PageIndex);

  options.WriteInteger('form', 'MainFormLeft', MainForm.Left);
  options.WriteInteger('form', 'MainFormTop', MainForm.Top);
  options.WriteInteger('form', 'MainFormWidth', MainForm.Width);
  options.WriteInteger('form', 'MainFormHeight', MainForm.Height);

  options.WriteInteger('form', 'vtDownload0Width', vtDownload.Header.Columns.Items[0].Width);
  options.WriteInteger('form', 'vtDownload1Width', vtDownload.Header.Columns.Items[1].Width);
  options.WriteInteger('form', 'vtDownload2Width', vtDownload.Header.Columns.Items[2].Width);
  options.WriteInteger('form', 'vtDownload3Width', vtDownload.Header.Columns.Items[3].Width);
  options.WriteInteger('form', 'vtDownload4Width', vtDownload.Header.Columns.Items[4].Width);
  options.WriteInteger('form', 'vtDownload5Width', vtDownload.Header.Columns.Items[5].Width);

  options.WriteInteger('form', 'vtFavorites0Width', vtFavorites.Header.Columns.Items[0].Width);
  options.WriteInteger('form', 'vtFavorites1Width', vtFavorites.Header.Columns.Items[1].Width);
  options.WriteInteger('form', 'vtFavorites2Width', vtFavorites.Header.Columns.Items[2].Width);
  options.WriteInteger('form', 'vtFavorites3Width', vtFavorites.Header.Columns.Items[3].Width);
end;

procedure TMainForm.LoadLanguage(const pos: Integer);
var
  language: TIniFile;
  lang    : String;
  i, p    : Cardinal;
begin
  if pos < 0 then exit;
  language:= TIniFile.Create(CONFIG_FOLDER + LANGUAGE_FILE);

  p:= language.ReadInteger('select', 'numberOfLanguages', 0);
  if p <> 0 then
  begin
    cbLanguages.Items.Clear;
    for i:= 0 to p-1 do
      cbLanguages.Items.Add(language.ReadString('select', IntToStr(i), 'English'));
  end;
  cbLanguages.ItemIndex:= pos;
  lang:= cbLanguages.Items.Strings[cbLanguages.ItemIndex];

  tsDownload    .Caption:= language.ReadString(lang, 'tsDownloadCaption', '');
  tsInformation .Caption:= language.ReadString(lang, 'tsInformationCaption', '');
  tsFilter      .Caption:= language.ReadString(lang, 'tsFilterCaption', '');
  tsFavorites   .Caption:= language.ReadString(lang, 'tsFavoritesCaption', '');
  tsOption      .Caption:= language.ReadString(lang, 'tsOptionCaption', '');
  edSearch      .Text   := language.ReadString(lang, 'edSearchText', '');
  stModeAll             := language.ReadString(lang, 'stModeAll', '');
  stModeFilter          := language.ReadString(lang, 'stModeFilter', '');

  stStop                := language.ReadString(lang, 'stStop', '');
  stPreparing           := language.ReadString(lang, 'stPreparing', '');
  stDownloading         := language.ReadString(lang, 'stDownloading', '');
  stFinish              := language.ReadString(lang, 'stFinish', '');
  stWait                := language.ReadString(lang, 'stWait', '');
  btUpdateList  .Hint   := language.ReadString(lang, 'btUpdateListHint', '');
  btSearch      .Hint   := language.ReadString(lang, 'btSearchHint', '');
  btRemoveFilter.Hint   := language.ReadString(lang, 'btRemoveFilterHint', '');
  btRemoveFilterLarge.Hint:= btRemoveFilter.Hint;

  edSaveTo.EditLabel.Caption:= language.ReadString(lang, 'edSaveToEditLabelCaption', '');
  btDownload    .Caption  := language.ReadString(lang, 'btDownloadCaption', '');
  cbAddAsStopped.Caption  := language.ReadString(lang, 'cbAddAsStoppedCaption', '');
  cbAddToFavorites.Caption:= language.ReadString(lang, 'cbAddToFavoritesCaption', '');

  cbFilterStatus.Items.Strings[0]:= language.ReadString(lang, 'lbFilterStatus0', '');
  cbFilterStatus.Items.Strings[1]:= language.ReadString(lang, 'lbFilterStatus1', '');
  rbAll.Caption           := language.ReadString(lang, 'rbAllCaption', '');
  rbOne.Caption           := language.ReadString(lang, 'rbOneCaption', '');
  cbOnlynew.Caption       := language.ReadString(lang, 'cbOnlyNewCaption', '');
  btFilter.Caption        := language.ReadString(lang, 'btFilterCaption', '');
  btRemoveFilterLarge.Caption:= language.ReadString(lang, 'btRemoveFilterLargeCaption', '');

  btOptionApply.Caption   := language.ReadString(lang, 'btOptionApplyCaption', '');
  tsLanguage.Caption      := language.ReadString(lang, 'tsLanguageCaption', '');
  tsConnections.Caption   := language.ReadString(lang, 'tsConnectionsCaption', '');
  tsSaveTo.Caption        := language.ReadString(lang, 'tsSaveToCaption', '');
  gbOptionProxy.Caption   := language.ReadString(lang, 'gbOptionProxyCaption', '');
  cbOptionUseProxy.Caption:= language.ReadString(lang, 'cbOptionUseProxyCaption', '');
  edOptionDefaultPath.EditLabel.Caption:= language.ReadString(lang, 'edOptionDefaultPathEditLabelCaption', '');
  rgOptionCompress.Caption:= language.ReadString(lang, 'rgOptionCompressCaption', '');         ;

  dlgSaveTo.Title         := language.ReadString(lang, 'dlgSaveToTitle', '');

  miUp.Caption            := language.ReadString(lang, 'miUp', '');
  miDown.Caption          := language.ReadString(lang, 'miDown', '');
  miDownloadStop.Caption  := language.ReadString(lang, 'miDownloadStopCaption', '');
  miDownloadRemuse.Caption:= language.ReadString(lang, 'miDownloadStopRemuse', '');
  miDownloadRemove.Caption:= language.ReadString(lang, 'miDownloadRemoveCaption', '');
  miDownloadRemoveFinishedTasks.Caption:= language.ReadString(lang, 'miDownloadRemoveFinishedTasksCaption', '');
  miOpenFolder.Caption    := language.ReadString(lang, 'miOpenFolder', '');

  miChapterListCheckSelected.Caption:= language.ReadString(lang, 'miChapterListCheckSelectedCaption', '');
  miChapterListUncheckSelected.Caption:= language.ReadString(lang, 'miChapterListUncheckSelectedCaption', '');
  miChapterListCheckAll.Caption:= language.ReadString(lang, 'miChapterListCheckAllCaption', '');
  miChapterListUncheckAll.Caption:= language.ReadString(lang, 'miChapterListUncheckAllCaption', '');

  miFavoritesRemove.Caption:= language.ReadString(lang, 'miFavoritesRemoveCaption', '');
  miFavoritesChangeCurrentChapter.Caption:= language.ReadString(lang, 'miFavoritesChangeCurrentChapterCaption', '');
  miFavoritesChangeSaveTo.Caption:= language.ReadString(lang, 'miFavoritesChangeSaveToCaption', '');
  miMangaListAddToFavorites.Caption:= language.ReadString(lang, 'miMangaListAddToFavoritesCaption', '');

  infoCustomGenres       := language.ReadString(lang, 'infoCustomGenres', '');
  infoName               := language.ReadString(lang, 'infoName', '');
  infoAuthors            := language.ReadString(lang, 'infoAuthors', '');
  infoArtists            := language.ReadString(lang, 'infoArtists', '');
  infoStatus             := language.ReadString(lang, 'infoStatus', '');
  infoGenres             := language.ReadString(lang, 'infoGenres', '');
  infoSummary            := language.ReadString(lang, 'infoSummary', '');
  infoLink               := language.ReadString(lang, 'infoLink', '');

  lbFilterCustomGenres.Caption:= infoCustomGenres;
  lbFilterTitle.Caption  := infoName;
  lbFilterAuthors.Caption:= infoAuthors;
  lbFilterArtists.Caption:= infoArtists;
  lbFilterStatus.Caption := infoStatus;
  lbFilterSummary.Caption:= infoSummary;

  stDownloadManga          := language.ReadString(lang, 'stDownloadManga', '');
  stDownloadStatus         := language.ReadString(lang, 'stDownloadStatus', '');
  stDownloadProgress       := language.ReadString(lang, 'stDownloadProgress', '');
  stDownloadWebsite        := language.ReadString(lang, 'stDownloadWebsite', '');
  stDownloadSaveto         := language.ReadString(lang, 'stDownloadSaveto', '');
  stDownloadAdded          := language.ReadString(lang, 'stDownloadAdded', '');
  stFavoritesCurrentChapter:= language.ReadString(lang, 'stFavoritesCurrentChapter', '');

  stFavoritesCheck         := language.ReadString(lang, 'stFavoritesCheck', '');
  stFavoritesChecking      := language.ReadString(lang, 'stFavoritesChecking', '');

  stDlgUpdateAlreadyRunning:= language.ReadString(lang, 'stDlgUpdateAlreadyRunning', '');
  stDlgNewManga            := language.ReadString(lang, 'stDlgNewManga', '');
  stDlgQuit                := language.ReadString(lang, 'stDlgQuit', '');
  stDlgRemoveTask          := language.ReadString(lang, 'stDlgRemoveTask', '');
  stDlgRemoveFinishTasks   := language.ReadString(lang, 'stDlgRemoveFinishTasks', '');
  stDlgTypeInNewChapter    := language.ReadString(lang, 'stDlgTypeInNewChapter', '');
  stDlgTypeInNewSavePath   := language.ReadString(lang, 'stDlgTypeInNewSavePath', '');
  stDlgCannotGetMangaInfo  := language.ReadString(lang, 'stDlgCannotGetMangaInfo', '');
  stDlgFavoritesIsRunning  := language.ReadString(lang, 'stDlgFavoritesIsRunning', '');
  stDlgNoNewChapter        := language.ReadString(lang, 'stDlgNoNewChapter', '');
  stDlgHasNewChapter       := language.ReadString(lang, 'stDlgHasNewChapter', '');

  language.Free;
  if dataProcess.isFiltered then
    lbMode.Caption:= Format(stModeFilter, [dataProcess.filterPos.Count])
  else
    lbMode.Caption:= Format(stModeAll, [dataProcess.filterPos.Count]);

  // sync download table infos
  if DLManager.containers.Count > 0 then
  begin
    for i:= 0 to DLManager.containers.Count - 1 do
    begin
     // if (DLManager.containers.Items[pos] <> nil) OR (NOT DLManager.containers.Items[pos].thread.isTerminated) then
      case DLManager.containers.Items[i].Status of
        STATUS_STOP    : DLManager.containers.Items[i].downloadInfo.Status:= stStop;
        STATUS_WAIT    : DLManager.containers.Items[i].downloadInfo.Status:= stWait;
        STATUS_DOWNLOAD: DLManager.containers.Items[i].downloadInfo.Status:= stDownloading;
        STATUS_FINISH  : DLManager.containers.Items[i].downloadInfo.Status:= stFinish;
      end;
    end;
  end;

  if favorites.isRunning then
    btFavoritesCheckNewChapter.Caption:= stFavoritesChecking
  else
    btFavoritesCheckNewChapter.Caption:= stFavoritesCheck;
  vtDownload.Header.Columns.Items[0].Text:= stDownloadManga;
  vtDownload.Header.Columns.Items[1].Text:= stDownloadStatus;
  vtDownload.Header.Columns.Items[2].Text:= stDownloadProgress;
  vtDownload.Header.Columns.Items[3].Text:= stDownloadWebsite;
  vtDownload.Header.Columns.Items[4].Text:= stDownloadSaveto;
  vtDownload.Header.Columns.Items[5].Text:= stDownloadAdded;

  vtFavorites.Header.Columns.Items[0].Text:= stDownloadManga;
  vtFavorites.Header.Columns.Items[1].Text:= stFavoritesCurrentChapter;
  vtFavorites.Header.Columns.Items[2].Text:= stDownloadWebsite;
  vtFavorites.Header.Columns.Items[3].Text:= stDownloadSaveto;

  vtDownload.Repaint;
end;

procedure TMainForm.tmBackupTimer(Sender: TObject);
begin
  //DLManager.Backup;
end;

end.

