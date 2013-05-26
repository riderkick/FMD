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
  baseunit, data, types, downloads, favorites, LConvEncoding, LCLIntf,
  updatelist, updatedb, lclproc, subthreads{, ActiveX}, AnimatedGif, MemBitmap;

type

  { TMainForm }

  TMainForm = class(TForm)
    btCheckVersion: TBitBtn;
    btURL: TBitBtn;
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
    btReadOnline: TButton;
    cbOptionAutoCheckUpdate: TCheckBox;
    cbOptionShowDeleteTaskDialog: TCheckBox;
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
    cbOptionShowQuitDialog: TCheckBox;
    cbOptionPathConvert: TCheckBox;
    cbOptionGenerateChapterName: TCheckBox;
    cbOptionGenerateMangaFolderName: TCheckBox;
    cbOptionMinimizeToTray: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    cbFilterStatus: TComboBox;
    clbOptionMangaSiteSelection: TCheckListBox;
    clbChapterList: TCheckListBox;
    cbLanguages: TComboBox;
    cbOptionLetFMDDo: TComboBox;
    edFilterSummary: TEdit;
    edFilterTitle: TEdit;
    edFilterAuthors: TEdit;
    edFilterArtists: TEdit;
    edCustomGenres: TEdit;
    edURL: TEdit;
    edOptionHost: TEdit;
    edOptionPass: TEdit;
    edOptionPort: TEdit;
    edOptionUser: TEdit;
    edSaveTo: TLabeledEdit;
    edSearch: TEdit;
    gbOptionProxy: TGroupBox;
    itAnimate: TIdleTimer;
    ImageList: TImageList;
    imCover: TImage;
    edOptionDefaultPath: TLabeledEdit;
    lbOptionLetFMDDo: TLabel;
    lbOptionNewMangaTime: TLabel;
    lbOptionLanguage: TLabel;
    lbOptionDialogs: TLabel;
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
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnUpdateList: TMenuItem;
    mnUpdateDownFromServer: TMenuItem;
    miDownloadHideCompleted: TMenuItem;
    miDownloadMerge: TMenuItem;
    miOpenFolder2: TMenuItem;
    miHighlightNewManga: TMenuItem;
    miI2: TMenuItem;
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
    pbWait: TPaintBox;
    pnMainTop: TPanel;
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
    pmUpdate: TPopupMenu;
    rbOne: TRadioButton;
    rbAll: TRadioButton;
    rgOptionCompress: TRadioGroup;
    rmAbout: TRichMemo;
    rmInformation: TRichMemo;
    sbFilter: TScrollBox;
    sbInformation: TScrollBox;
    sbDownloadConnections: TScrollBox;
    dlgSaveTo: TSelectDirectoryDialog;
    seOptionMaxParallel: TSpinEdit;
    seOptionMaxRetry: TSpinEdit;
    seOptionMaxThread: TSpinEdit;
    seOptionNewMangaTime: TSpinEdit;
    spInfos: TSplitter;
    spMainSplitter: TSplitter;
    sbMain: TStatusBar;
    tsAbout: TTabSheet;
    tsWebsites: TTabSheet;
    tsDialogs: TTabSheet;
    tmBackup: TTimer;
    TrayIcon: TTrayIcon;
    tsGeneral: TTabSheet;
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

    procedure btCheckVersionClick(Sender: TObject);
    procedure btReadOnlineClick(Sender: TObject);
    procedure btUpdateListClick(Sender: TObject);
    procedure btURLClick(Sender: TObject);
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
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure itAnimateTimer(Sender: TObject);
    procedure miDownloadHideCompletedClick(Sender: TObject);
    procedure miDownloadMergeClick(Sender: TObject);
    procedure miHighlightNewMangaClick(Sender: TObject);
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
    procedure miOpenFolder2Click(Sender: TObject);
    procedure miOpenFolderClick(Sender: TObject);
    procedure miUpClick(Sender: TObject);
    procedure mnUpdateDownFromServerClick(Sender: TObject);
    procedure mnUpdateListClick(Sender: TObject);

    procedure pcMainChange(Sender: TObject);
    procedure pmDownloadPopup(Sender: TObject);
    procedure pmFavoritesPopup(Sender: TObject);
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
    procedure vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

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
    isExiting      : Boolean;
    // for manga website that available for visible on selection list
    websiteName    : TStringList;
    websiteLanguage: TStringList;
    websiteSelect  : TList;

    isUpdating  : Boolean;
    updates,
    mangalistIni,
    options     : TIniFile;
    favorites   : TFavoriteManager;
    dataProcess : TDataProcess;
    mangaInfo   : TMangaInfo;
    DLManager   : TDownloadManager;
    updateDB    : TUpdateDBThread;
    updateList  : TUpdateMangaManagerThread;
    ticks       : Cardinal;
    backupTicks : Cardinal;
    // animation gif
    gifWaiting  : TAnimatedGif;

    // doing stuff like get manga info, compress, ...
    SubThread   : TSubThread;

    // load about information
    procedure LoadAbout;

    procedure CloseNow;

    procedure CheckForTopPanel;
    // en: Too lazy to add it one by one
    // vi: Lười ...
    procedure InitCheckboxes;

    // en: hide all completed tasks
    procedure HideCompletedTasks(const isHide: Boolean);

    procedure AddChapterNameToList;

    // en: Add text to TRichMemo
    // vi: Thêm văn bản vào TRichMemo
    procedure AddTextToInfo(title, infoText: String);

    // en: Show manga information
    // vi: Xuất thông tin về manga
    procedure ShowInformation;

    // get manga list from server
    procedure RunGetList;

    // en: Load config from config.ini
    // vi: Lấy thông tin từ config.ini
    procedure LoadOptions;

    // en: Load config from mangalist.ini
    // vi: Lấy thông tin từ mangalist.ini
    procedure LoadMangaOptions;

    function  SaveMangaOptions: String;

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

uses
  logform;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  fs: TFileStream;
begin
  try
    rmAbout.Clear;
    fs:= TFileStream.Create(README_FILE, fmOpenRead or fmShareDenyNone);
    rmAbout.LoadRichText(fs);
  finally
    fs.Free;
  end;
  isUpdating := FALSE;
  isExiting  := FALSE;
 // ticks      := GetTickCount;
 // backupTicks:= GetTickCount;
  oldDir     := GetCurrentDirUTF8;
  oldDir     := CorrectFile(oldDir);

  dataProcess:= TDataProcess.Create;

  DLManager  := TDownloadManager.Create;

  favorites  := TFavoriteManager.Create;
  favorites.OnUpdateFavorite:= UpdateVtFavorites;
  favorites.OnUpdateDownload:= UpdateVtDownload;
  favorites.DLManager       := DLManager;

  options     := TIniFile.Create(oldDir + CONFIG_FOLDER + CONFIG_FILE);
  options.CacheUpdates:= FALSE;

  updates     := TIniFile.Create(oldDir + CONFIG_FOLDER + UPDATE_FILE);
  updates.CacheUpdates:= FALSE;

  mangalistIni:= TIniFile.Create(oldDir + CONFIG_FOLDER + MANGALISTINI_FILE);
  mangalistIni.CacheUpdates:= FALSE;

  websiteName    := TStringList.Create;
  websiteLanguage:= TStringList.Create;
  websiteSelect  := TList.Create;

  LoadOptions;
  LoadMangaOptions;
  LoadFormInformation;

 // ShowInformation;
  mangaInfo.chapterName := TStringList.Create;
  mangaInfo.chapterLinks:= TStringList.Create;

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
  CheckForTopPanel;
  DLManager.CheckAndActiveTaskAtStartup;
  TrayIcon.Show;

  // load some necessary options at startup
  Revision                            := options.ReadInteger('general', 'Revision', 0);
  seOptionNewMangaTime.Value          := options.ReadInteger('general', 'NewMangaTime', 3);
  miHighLightNewManga.Checked         := options.ReadBool('general', 'HighLightNewManga', FALSE);
  cbOptionShowQuitDialog.Checked      := options.ReadBool('dialogs', 'ShowQuitDialog', TRUE);
  cbOptionShowDeleteTaskDialog.Checked:= options.ReadBool('dialogs', 'ShowDeleteDldTaskDialog', TRUE);
  currentJDN:= GetCurrentJDN;

  // read online
  btReadOnline.Enabled:= FALSE;

  // subthread
  SubThread:= TSubThread.Create;
  SubThread.OnShowInformation:= nil;
  SubThread.isSuspended:= FALSE;

  if cbOptionAutoCheckUpdate.Checked then
  begin
    SubThread.updateCounter:= 0;
    SubThread.isCheckForLatestVer:= TRUE;
  end;

  HideCompletedTasks(miDownloadHideCompleted.Checked);

  // why this doesn't work ?
  case pcMain.TabIndex of
    5: LoadAbout;
  end;
  cbOptionLetFMDDo.ItemIndex:= options.ReadInteger('general', 'LetFMDDo', 0);

  if FileExists(IMAGE_FOLDER + 'waiting.gif') then
  begin
    gifWaiting:= TAnimatedGif.Create(IMAGE_FOLDER + 'waiting.gif');
    gifWaiting.EraseColor:= self.Color;
    gifWaiting.BackgroundMode:= gbmSaveBackgroundOnce;
  end;
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

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SubThread.Terminate;
  Sleep(200);
 // SubThread.WaitFor;
 // SubThread.Free;

  websiteSelect.Free;
  websiteName.Free;
  websiteLanguage.Free;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  case WindowState of
    wsMinimized:
      begin
        if cbOptionMinimizeToTray.Checked then
          MainForm.Hide;
        TrayIcon.Show;
      end;
  end;
end;

procedure TMainForm.itAnimateTimer(Sender: TObject);
var
  r: TRect;
begin
  r.Left:= 53; r.Top:= 84; r.Right:= 101; r.Bottom:= 131;
  gifWaiting.Update(pbWait.Canvas, r);
end;

procedure TMainForm.miDownloadHideCompletedClick(Sender: TObject);
var
  i: Cardinal;
begin
  miDownloadHideCompleted.Checked:= NOT miDownloadHideCompleted.Checked;
  HideCompletedTasks(miDownloadHideCompleted.Checked);
  options.WriteBool('general', 'HideCompleted', miDownloadHideCompleted.Checked);
end;

procedure TMainForm.miDownloadMergeClick(Sender: TObject);
var
  i, j: Cardinal;
// merge all finished tasks that have same manga name, website and directory
begin
  i:= DLManager.containers.Count-1;
  while i > 0 do
  begin
    if DLManager.containers.Items[i].Status = STATUS_FINISH then
    begin
      j:= i-1;
      while j > 0 do
      begin
        if (i<>j) AND
           (DLManager.containers[j].Status = STATUS_FINISH) AND
           SameText(DLManager.containers.Items[i].downloadInfo.title, DLManager.containers.Items[j].downloadInfo.title) AND
           SameText(DLManager.containers.Items[i].downloadInfo.website, DLManager.containers.Items[j].downloadInfo.website) AND
           SameText(DLManager.containers.Items[i].downloadInfo.saveTo, DLManager.containers.Items[j].downloadInfo.saveTo) then
        begin
          DLManager.containers.Items[i].chapterLinks.Text:= DLManager.containers.Items[i].chapterLinks.Text + DLManager.containers.Items[j].chapterLinks.Text;
          DLManager.containers.Items[i].chapterName .Text:= DLManager.containers.Items[i].chapterName .Text + DLManager.containers.Items[j].chapterName .Text;
          DLManager.containers.Items[i].downloadInfo.dateTime:= DLManager.containers.Items[j].downloadInfo.dateTime;
          DLManager.RemoveTask(j);
          Dec(i);
        end;
        Dec(j);
      end;
    end;
    Dec(i);
  end;
  UpdateVtDownload;
end;

procedure TMainForm.miHighlightNewMangaClick(Sender: TObject);
begin
  miHighlightNewManga.Checked:= NOT miHighlightNewManga.Checked;
  options.WriteBool('general', 'HighLightNewManga', miHighlightNewManga.Checked);
  vtMangaList.Repaint;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if cbOptionShowQuitDialog.Checked then
  begin
    if MessageDlg('', stDlgQuit,
                  mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    begin
      CloseAction:= caNone;
      exit;
    end;
  end;
  CloseNow;
  Halt;
  CloseAction:= caFree;
end;

procedure TMainForm.CloseNow;
begin
  isExiting:= TRUE;
  mangalistIni.WriteInteger('general', 'batotoLastDirectoryPage', batotoLastDirectoryPage);
  SaveFormInformation;
  DLManager.StopAllDownloadTasksForExit;
  dataProcess.SaveToFile;
  dataProcess.Destroy;
 // Halt;
end;

procedure TMainForm.LoadAbout;
var
  fs: TFileStream;
begin
  try
    rmAbout.Clear;
    fs:= TFileStream.Create(README_FILE, fmOpenRead or fmShareDenyNone);
    rmAbout.LoadRichText(fs);
  finally
    fs.Free;
  end;
end;

// -----

procedure TMainForm.btDownloadClick(Sender: TObject);
var
  s: String;
  hh, mm, ss, ms,
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
      if (mangaInfo.website <> GEHENTAI_NAME) AND
         (mangaInfo.website <> FAKKU_NAME) then
      begin
        s:= Format('%.4d', [i+1]);
        if NOT cbOptionGenerateChapterName.Checked then
        else
        begin
          if NOT cbOptionPathConvert.Checked then
            s:= s + '- ' + mangaInfo.chapterName.Strings[i]
          else
            s:= s + '- ' + UnicodeRemove(mangaInfo.chapterName.Strings[i]);
        end;
      end
      else
      begin
        if NOT cbOptionPathConvert.Checked then
          s:= RemoveSymbols(TrimLeft(TrimRight(mangaInfo.title)))
        else
          s:= UnicodeRemove(RemoveSymbols(TrimLeft(TrimRight(mangaInfo.title))));
       { if Length(s)>64 then
        begin
        //  s:= UnicodeRemove(s);
          SetLength(s, 64);
          s:= TrimLeft(TrimRight(s));
        end; }
      end;

      DLManager.containers.Items[pos].chapterName .Add(s);
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
  s:= CorrectFile(edSaveTo.Text);
  if s[Length(s)] = '/' then
    Delete(s, Length(s), 1);

  // save to
  if cbOptionGenerateMangaFolderName.Checked then
  begin
    if NOT cbOptionPathConvert.Checked then
      s:= s + '/' + RemoveSymbols(mangaInfo.title)
    else
      s:= s + '/' + RemoveSymbols(UnicodeRemove(mangaInfo.title));
  end;
  DLManager.containers.Items[pos].downloadInfo.SaveTo:= s;

  // time
  DecodeDate(Now, year, month, day);
  DecodeTime(Time, hh, mm, ss, ms);
  DLManager.containers.Items[pos].downloadInfo.dateTime:= IntToStr(Month)+'/'+IntToStr(Day)+'/'+IntToStr(Year)+' '+IntToStr(hh)+':'+IntToStr(mm)+':'+IntToStr(ss);

  // Add to favorites
  if cbAddToFavorites.Checked then
  begin
    favorites.Add(mangaInfo.title, IntToStr(mangaInfo.numChapter),
                  mangaInfo.website, s, mangaInfo.link);
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
  if edSaveTo.Text='' then exit;
  dlgSaveTo.InitialDir:= CorrectFile(edSaveTo.Text);
  if dlgSaveTo.Execute then
    edSaveTo.Text:= CorrectFile(dlgSaveTo.FileName);
end;

// -----

procedure TMainForm.btUpdateListClick(Sender: TObject);
var
  button   : TControl;
  lowerLeft: TPoint;
begin
  if dataProcess.Title.Count = 0 then
    pmUpdate.Items[0].Enabled:= FALSE
  else
    pmUpdate.Items[0].Enabled:= TRUE;
  if Sender is TControl then
  begin
    button   := TControl(Sender);
    lowerLeft:= Point(button.Left, button.Top + button.Height*2 + (button.Height div 2));
    lowerLeft:= ClientToScreen(lowerLeft);
    pmUpdate.Popup(lowerLeft.X, lowerLeft.Y);
  end;
end;

procedure TMainForm.btURLClick(Sender: TObject);
begin
  cbAddToFavorites.Checked:= FALSE;
  cbAddToFavorites.Enabled:= FALSE;
  if (SubThread.isGetInfos) then exit;
  if Pos('http://', edURL.Text) = 0 then
    edURL.Text:= 'http://' + edURL.Text;
  if Pos(GEHENTAI_ROOT, edURL.Text) = 0 then
  begin
    MessageDlg('', stDlgURLNotSupport, mtInformation, [mbYes], 0);
    exit;
  end;

  if Pos(GEHENTAI_ROOT, edURL.Text) <> 0 then
    SubThread.link:= edURL.Text + '?nw=session';

  SubThread.mangaListPos:= -1;
  SubThread.website:= GEHENTAI_NAME;//cbSelectManga.Items[cbSelectManga.ItemIndex];
 // SubThread.link:= edURL.Text;
  SubThread.isGetInfos:= TRUE;

  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add('Loading ...');
  clbChapterList.Clear;
  if Assigned(gifWaiting) then
    itAnimate.Enabled:= TRUE;
end;

procedure TMainForm.btReadOnlineClick(Sender: TObject);
begin
  OpenURL(mangaInfo.url);
end;

procedure TMainForm.btCheckVersionClick(Sender: TObject);
begin
  if subthread.isCheckForLatestVer then
    MessageDlg('', stDlgUpdaterIsRunning, mtInformation, [mbYes], 0)
  else
  begin
    subthread.isCheckForLatestVer:= TRUE;
    btCheckVersion.Caption:= stFavoritesChecking;
  end;
end;

procedure TMainForm.cbSelectMangaChange(Sender: TObject);
begin
  if currentWebsite <> cbSelectManga.Items.Strings[cbSelectManga.ItemIndex] then
  begin
    if dataProcess.Title.Count > 0 then
    begin
      dataProcess.RemoveFilter;
      dataProcess.SaveToFile;
    end;
    dataProcess.Free;
    dataProcess:= TDataProcess.Create;
    if NOT dataProcess.LoadFromFile(cbSelectManga.Items.Strings[cbSelectManga.ItemIndex]) then
    begin
      RunGetList;
    end;
    vtMangaList.Clear;
    vtMangaList.RootNodeCount:= dataProcess.filterPos.Count;
    lbMode.Caption:= Format(stModeAll, [dataProcess.filterPos.Count]);
    currentWebsite:= cbSelectManga.Items[cbSelectManga.ItemIndex];
    dataProcess.website:= cbSelectManga.Items[cbSelectManga.ItemIndex];
    CheckForTopPanel;
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
  s: String;
begin
  checkGenres  := TStringList.Create;
  uncheckGenres:= TStringList.Create;
  CustomGenres(checkGenres, edCustomGenres.Text);
  i:= 0;
  while i<checkGenres.Count do
  begin
    s:= checkGenres.Strings[i];
    if (s[1] = '-') OR (s[1] = '!') then
    begin
      if (s[1] = '-') then
        s:= StringReplace(s, '-', '', [])
      else
        s:= StringReplace(s, '!', '', []);
      uncheckGenres.Add(s);
      checkGenres.Delete(i);
    end
    else
      Inc(i);
  end;
  for i:= 0 to 38 do
  begin
    if TCheckBox(pnGenres.Controls[i]).State = cbChecked then
      checkGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption)
    else
    if TCheckBox(pnGenres.Controls[i]).State = cbUnchecked then
      uncheckGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption);
  end;
  if dataProcess.Filter(checkGenres, uncheckGenres,
                        edFilterTitle.Text, edFilterAuthors.Text,
                        edFilterArtists.Text, IntToStr(cbFilterStatus.ItemIndex),
                        edFilterSummary.Text,
                        seOptionNewMangaTime.Value,
                        rbAll.Checked, cbOnlyNew.Checked) then
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
  edCustomGenres .Caption:= '';
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
  s  : String;
  pos: Cardinal;
begin
  if NOT Assigned(vtMangaList.FocusedNode) then exit;
  pos:= vtMangaList.FocusedNode.Index;

  s:= CorrectFile(options.ReadString('saveto', 'SaveTo', DEFAULT_PATH));
  if s[Length(s)] = '/' then
    Delete(s, Length(s), 1);

  if cbOptionGenerateMangaFolderName.Checked then
  begin
    if NOT cbOptionPathConvert.Checked then
      s:= s + '/' + dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_NAME]
    else
      s:= s + '/' + UnicodeRemove(dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_NAME]);
  end;

  favorites.Add(dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_NAME],
                dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_NUMCHAPTER],
                dataProcess.website,
                s,
                dataProcess.Param[dataProcess.filterPos.Items[pos], DATA_PARAM_LINK]);
  UpdateVtFavorites;
  pcMain.PageIndex:= 3;
end;

// ----- vtFavorites popup menu -----

procedure TMainForm.miFavoritesRemoveClick(Sender: TObject);
var
  i    : Cardinal;
  xNode: PVirtualNode;
begin
  if (cbOptionShowDeleteTaskDialog.Checked) AND (vtFavorites.SelectedCount > 0) then
    if MessageDlg('', stDlgRemoveTask,
                  mtConfirmation, [mbYes, mbNo], 0) = mrNo then exit;
  if favorites.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
                mtInformation, [mbYes, mbNo], 0);
    exit;
  end;
  if vtFavorites.SelectedCount = 1 then
  begin
    if NOT Assigned(vtFavorites.FocusedNode) then exit;
    favorites.Remove(vtFavorites.FocusedNode.Index);
  end
  else
  begin
    xNode:= vtFavorites.GetFirst;

    i:= 0;
    while i < favorites.Count do
    begin
      if vtFavorites.Selected[xNode] then
        favorites.Remove(i)
      else
        Inc(i);
      xNode:= vtFavorites.GetNext(xNode);
    end;
   { for i:= 0 to vtFavorites.RootNodeCount-1 do
    begin
      if vtFavorites.Selected[xNode] then
        favorites.Remove(xNode.Index);
      xNode:= vtFavorites.GetNext(xNode);
    end; }
  end;
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

procedure TMainForm.mnUpdateDownFromServerClick(Sender: TObject);
begin
  if (NOT isUpdating) then
  begin
    RunGetList;
  end
  else
    MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0);
end;

procedure TMainForm.mnUpdateListClick(Sender: TObject);
begin
  if (NOT isUpdating) then
  begin
   // if dataProcess.Title.Count > 1 then
    begin
      isUpdating:= TRUE;
      updateList:= TUpdateMangaManagerThread.Create;
      updateList.numberOfThreads:= 4;
      updateList.websites.Add(cbSelectManga.Items[cbSelectManga.ItemIndex]);
      updateList.isSuspended:= FALSE;
    end;
  end
  else
    MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0);
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
  if cbOptionShowDeleteTaskDialog.Checked then
    if MessageDlg('', stDlgRemoveFinishTasks,
                  mtConfirmation, [mbYes, mbNo], 0) = mrNo then exit;
  DLManager.RemoveAllFinishedTasks;
  UpdateVtDownload;
  DLManager.Backup;
end;

procedure TMainForm.miDownloadRemuseClick(Sender: TObject);
var
  i    : Cardinal;
  xNode: PVirtualNode;
begin
 // if NOT Assigned(vtDownload.FocusedNode) then exit;

  // print waiting string to the screen

  if (vtDownload.SelectedCount = 1) AND (Assigned(vtDownload.FocusedNode)) then
  begin
    if DLManager.containers.Items[vtDownload.FocusedNode.Index].Status = STATUS_STOP then
    begin
      DLManager.containers.Items[vtDownload.FocusedNode.Index].Status:= STATUS_WAIT;
      DLManager.containers.Items[vtDownload.FocusedNode.Index].downloadInfo.Status:= stWait;
      if DLManager.CanActiveTask then
        DLManager.ActiveTask(vtDownload.FocusedNode.Index);
      vtDownload.Repaint;
      DLManager.Backup;
    end;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode:= vtDownload.GetFirst;
    for i:= 0 to vtDownload.RootNodeCount-1 do
    begin
      if vtDownload.Selected[xNode] then
      begin
        if DLManager.containers.Items[i].Status = STATUS_STOP then
        begin
          DLManager.containers.Items[i].Status:= STATUS_WAIT;
          DLManager.containers.Items[i].downloadInfo.Status:= stWait;
          if DLManager.CanActiveTask then
            DLManager.ActiveTask(i);
        end;
      end;
      xNode:= vtDownload.GetNext(xNode);
    end;
    vtDownload.Repaint;
    DLManager.Backup;
  end;
  //DLManager.CheckAndActiveTask;
  //if NOT DLManager.CanActiveTask then exit;
  //DLManager.ActiveTask(vtDownload.FocusedNode.Index);
  // print preparing/downloading string to the screen
  //vtDownload.Repaint;
  //DLManager.Backup;
end;

procedure TMainForm.miDownloadRemoveClick(Sender: TObject);
var
  i    : Cardinal;
  xNode: PVirtualNode;
begin
 // if NOT Assigned(vtDownload.FocusedNode) then exit;
  if (cbOptionShowDeleteTaskDialog.Checked) AND
     (vtDownload.SelectedCount > 0) then
    if MessageDlg('', stDlgRemoveTask,
                  mtConfirmation, [mbYes, mbNo], 0) = mrNo then exit;

  if (vtDownload.SelectedCount = 1) AND (Assigned(vtDownload.FocusedNode)) then
  begin
    DLManager.RemoveTask(vtDownload.FocusedNode.Index);
    UpdateVtDownload;
    DLManager.Backup;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode:= vtDownload.GetFirst;
    i:= 0;
    while i < DLManager.containers.Count do
    begin
      if vtDownload.Selected[xNode] then
        DLManager.RemoveTask(i)
      else
        Inc(i);
      xNode:= vtDownload.GetNext(xNode);
    end;
    UpdateVtDownload;
    DLManager.Backup;
  end;
end;

// Download table's popup menu

procedure TMainForm.miDownloadStopClick(Sender: TObject);
var
  i    : Cardinal;
  xNode: PVirtualNode;
begin
  //if NOT Assigned(vtDownload.FocusedNode) then exit;
  if (vtDownload.SelectedCount = 1) AND (Assigned(vtDownload.FocusedNode)) then
  begin
    DLManager.StopTask(vtDownload.FocusedNode.Index);
    vtDownload.Repaint;
  end
  else
  begin
    xNode:= vtDownload.GetFirst;
    for i:= 0 to vtDownload.RootNodeCount-1 do
    begin
      if vtDownload.Selected[xNode] then
        DLManager.StopTask(xNode.Index);
      xNode:= vtDownload.GetNext(xNode);
    end;
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.miOpenFolder2Click(Sender: TObject);
var
  Process: TProcess;
begin
  {$IFDEF WINDOWS}
  if NOT Assigned(vtFavorites.FocusedNode) then exit;
  Process:= TProcess.Create(nil);
  Process.CommandLine:= 'explorer.exe /e, '+
                         StringReplace(Favorites.favoriteInfo[vtFavorites.FocusedNode.Index].SaveTo, '/', '\', [rfReplaceAll]);
  Process.Execute;
  Process.Free;
  {$ENDIF}
end;

procedure TMainForm.miOpenFolderClick(Sender: TObject);
var
  Process: TProcess;
begin
  {$IFDEF WINDOWS}
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
  var
    l: TStringList;
    s: String;
    i: Cardinal;
  begin
    l:= TStringList.Create;

    cbOptionMinimizeToTray.Checked:= options.ReadBool('general', 'MinimizeToTray', FALSE);
    seOptionNewMangaTime.Value:= options.ReadInteger('general', 'NewMangaTime', 3);
    cbOptionLetFMDDo.ItemIndex:= options.ReadInteger('general', 'LetFMDDo', 0);
    cbOptionLetFMDDoItemIndex:= cbOptionLetFMDDo.ItemIndex;

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

    cbOptionShowQuitDialog.Checked      := options.ReadBool('dialogs', 'ShowQuitDialog', TRUE);
    cbOptionShowDeleteTaskDialog.Checked:= options.ReadBool('dialogs', 'ShowDeleteDldTaskDialog', TRUE);

    cbOptionPathConvert.Checked  := options.ReadBool   ('saveto', 'PathConv', FALSE);
    cbOptionGenerateChapterName.Checked:= options.ReadBool('saveto', 'GenChapName', FALSE);
    cbOptionGenerateMangaFolderName.Checked:= options.ReadBool('saveto', 'GenMangaName', TRUE);

    cbOptionAutoCheckUpdate.Checked:= options.ReadBool('update', 'AutoCheckUpdateAtStartup', TRUE);

    for i:= 0 to clbOptionMangaSiteSelection.Items.Count-1 do
      clbOptionMangaSiteSelection.Checked[i]:= FALSE;
    s:= mangalistIni.ReadString('general', 'MangaListSelect', '0'+SEPERATOR);
    GetParams(l, s);
    for i:= 0 to l.Count-1 do
      clbOptionMangaSiteSelection.Checked[StrToInt(l.Strings[i])]:= TRUE;

    l.Free;
  end;

begin
  case pcMain.TabIndex of
    4:
      UpdateOptions;
    5:
      begin
        UpdateOptions;
      // load rtf file
        LoadAbout;
      end;
    else
      UpdateOptions;
  end;
end;

procedure TMainForm.pmDownloadPopup(Sender: TObject);
begin
  if vtDownload.SelectedCount = 0 then
  begin
    pmDownload.Items[0].Enabled:= FALSE;
    pmDownload.Items[1].Enabled:= FALSE;
    pmDownload.Items[3].Enabled:= FALSE;
    pmDownload.Items[4].Enabled:= FALSE;
    pmDownload.Items[5].Enabled:= FALSE;
    pmDownload.Items[11].Enabled:= FALSE;
  end
  else
  if vtDownload.SelectedCount = 1 then
  begin
    pmDownload.Items[0].Enabled:= TRUE;
    pmDownload.Items[1].Enabled:= TRUE;
    pmDownload.Items[3].Enabled:= TRUE;
    pmDownload.Items[4].Enabled:= TRUE;
    pmDownload.Items[5].Enabled:= TRUE;
    pmDownload.Items[11].Enabled:= TRUE;
  end
  else
  begin
    pmDownload.Items[0].Enabled:= FALSE;
    pmDownload.Items[1].Enabled:= FALSE;
    pmDownload.Items[3].Enabled:= TRUE;
    pmDownload.Items[4].Enabled:= TRUE;
    pmDownload.Items[5].Enabled:= TRUE;
    pmDownload.Items[11].Enabled:= FALSE;
  end;
end;

procedure TMainForm.pmFavoritesPopup(Sender: TObject);
begin
  if favorites.isRunning then
  begin
    pmFavorites.Items[0].Enabled:= FALSE;
    pmFavorites.Items[1].Enabled:= FALSE;
    pmFavorites.Items[2].Enabled:= FALSE;
    exit;
  end;
  if vtFavorites.SelectedCount = 0 then
  begin
    pmFavorites.Items[0].Enabled:= FALSE;
    pmFavorites.Items[1].Enabled:= FALSE;
    pmFavorites.Items[2].Enabled:= FALSE;
    pmFavorites.Items[4].Enabled:= FALSE;
  end
  else
  if vtFavorites.SelectedCount = 1 then
  begin
    pmFavorites.Items[0].Enabled:= TRUE;
    pmFavorites.Items[1].Enabled:= TRUE;
    pmFavorites.Items[2].Enabled:= TRUE;
    {$IFDEF WINDOWS}
    pmFavorites.Items[4].Enabled:= TRUE;
    {$ELSE}
    pmFavorites.Items[4].Enabled:= FALSE;
    {$ENDIF}
  end
  else
  begin
    pmFavorites.Items[0].Enabled:= TRUE;
    pmFavorites.Items[1].Enabled:= FALSE;
    pmFavorites.Items[2].Enabled:= FALSE;
    pmFavorites.Items[4].Enabled:= FALSE;
  end;
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
var
  i: Cardinal;
  s: String;
  isStillHaveCurrentWebsite: Boolean = FALSE;
begin
  s:= SaveMangaOptions;
  if s = '' then
  begin
    MessageDlg('', stDldMangaListSelect,
               mtConfirmation, [mbYes], 0);
    exit;
  end;
  mangalistIni.WriteString('general', 'MangaListSelect', s);
  mangalistIni.UpdateFile;

  cbSelectManga.Clear;
  for i:= 0 to clbOptionMangaSiteSelection.Items.Count-1 do
  begin
    if clbOptionMangaSiteSelection.Checked[i] then
    begin
      cbSelectManga.Items.Add(websiteName.Strings[i]);
    end;
  end;

  for i:= 0 to cbSelectManga.Items.Count-1 do
  begin
    if cbSelectManga.Items[i] = currentWebsite then
    begin
      cbSelectManga.ItemIndex:= i;
      isStillHaveCurrentWebsite:= TRUE;
      break;
    end;
  end;

  // optimize required
  if NOT isStillHaveCurrentWebsite then
  begin
    cbSelectManga.ItemIndex:= 0;
    dataProcess.RemoveFilter;
    dataProcess.SaveToFile;
    dataProcess.Free;
    dataProcess:= TDataProcess.Create;
    dataProcess.LoadFromFile(cbSelectManga.Items.Strings[0]);
    vtMangaList.Clear;
    vtMangaList.RootNodeCount:= dataProcess.filterPos.Count;
    lbMode.Caption:= Format(stModeAll, [dataProcess.filterPos.Count]);
    currentWebsite:= cbSelectManga.Items[0];
  end;

  options.WriteBool   ('general', 'MinimizeToTray', cbOptionMinimizeToTray.Checked);
  options.WriteInteger('general', 'NewMangaTime', seOptionNewMangaTime.Value);
  options.WriteInteger('general', 'LetFMDDo', cbOptionLetFMDDo.ItemIndex);
  cbOptionLetFMDDoItemIndex:= cbOptionLetFMDDo.ItemIndex;

  options.WriteInteger('connections', 'NumberOfTasks', seOptionMaxParallel.Value);
  options.WriteInteger('connections', 'NumberOfThreadsPerTask', seOptionMaxThread.Value);
  options.WriteInteger('connections', 'Retry', seOptionMaxRetry.Value);
  options.WriteBool   ('connections', 'UseProxy', cbOptionUseProxy.Checked);
  options.WriteString ('connections', 'Host', edOptionHost.Text);
  options.WriteString ('connections', 'Pass', edOptionPass.Text);
  options.WriteString ('connections', 'Port', edOptionPort.Text);
  options.WriteString ('connections', 'User', edOptionUser.Text);

  options.WriteString ('saveto', 'SaveTo', edOptionDefaultPath.Text);
  options.WriteBool   ('saveto', 'PathConv', cbOptionPathConvert.Checked);
  options.WriteBool   ('saveto', 'GenChapName', cbOptionGenerateChapterName.Checked);
  options.WriteBool   ('saveto', 'GenMangaName', cbOptionGenerateMangaFolderName.Checked);
  options.WriteInteger('saveto', 'Compress', rgOptionCompress.ItemIndex);

  options.WriteBool   ('update', 'AutoCheckUpdateAtStartup', cbOptionAutoCheckUpdate.Checked);

  DLManager.compress:= rgOptionCompress.ItemIndex;

  options.WriteInteger('languages', 'Select', cbLanguages.ItemIndex);

  options.WriteBool   ('dialogs', 'ShowQuitDialog', cbOptionShowQuitDialog.Checked);
  options.WriteBool   ('dialogs', 'ShowDeleteDldTaskDialog', cbOptionShowDeleteTaskDialog.Checked);

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
  vtMangaList.Repaint;
end;

procedure TMainForm.cbAddAsStoppedChange(Sender: TObject);
begin
  options.WriteBool('general', 'AddAsStopped', cbAddAsStopped.Checked);
end;

// vtMangaList

procedure TMainForm.vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if (isExiting) OR (dataProcess.JDN.Count = 0) OR (dataProcess.filterPos.Count = 0) then exit;
  if miHighlightNewManga.Checked then
  begin
    try
      if currentJDN - Cardinal(dataProcess.JDN.Items[dataProcess.filterPos.Items[Node.Index]]) < seOptionNewMangaTime.Value then
      begin
        TargetCanvas.Brush.Color:= $ED9564;
        TargetCanvas.FillRect(CellRect);
      end;
    finally
    end;
  end;
end;

procedure TMainForm.vtMangaListChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin

end;

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
  cbAddToFavorites.Enabled:= TRUE;
  if (SubThread.isGetInfos) OR (NOT vtMangaList.Focused) then exit;

  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add('Loading ...');
  clbChapterList.Clear;

  SubThread.mangaListPos:= vtMangaList.FocusedNode.Index;
  SubThread.website:= cbSelectManga.Items[cbSelectManga.ItemIndex];
  SubThread.link:= DataProcess.Param[DataProcess.filterPos.Items[SubThread.mangaListPos], DATA_PARAM_LINK];
  SubThread.isGetInfos:= TRUE;
  //ShowInformation;

  if Assigned(gifWaiting) then
    itAnimate.Enabled:= TRUE;
end;

procedure TMainForm.CheckForTopPanel;
begin
  {if currentWebsite = GEHENTAI_NAME then
    pnMainTop.Visible:= TRUE
  else
    pnMainTop.Visible:= FALSE;}
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

procedure TMainForm.HideCompletedTasks(const isHide: Boolean);
var
  i      : Cardinal;
  xNode  : PVirtualNode;
  canExit: Boolean = FALSE;
begin
  if vtDownload.RootNodeCount = 0 then exit;
  xNode:= vtDownload.GetLast;
  for i:= vtDownload.RootNodeCount-1 downto 0 do
  begin
    vtDownload.isVisible[xNode]:= TRUE;
    if DLManager.containers.Items[i].Status = STATUS_FINISH then
      vtDownload.isVisible[xNode]:= NOT isHide;
    if canExit then
      exit;

    if xNode = vtDownload.GetFirst then
      canExit:= TRUE;
    xNode:= vtDownload.GetPrevious(xNode);
    if xNode = vtDownload.GetFirst then
      canExit:= TRUE;
  end;
end;

procedure TMainForm.AddChapterNameToList;
var
  i: Cardinal;
  s: String;
begin
  clbChapterList.Clear;
  if mangaInfo.chapterName.Count <> 0 then
  begin
    s:= mangaInfo.website;
    if (mangaInfo.website <> GEHENTAI_NAME) AND
       (mangaInfo.website <> FAKKU_NAME) then
      for i:= 0 to mangaInfo.chapterName.Count - 1 do
        clbChapterList.Items.Add(Format('%.4d - %s', [i+1, mangaInfo.chapterName.Strings[i]]))
    else
      for i:= 0 to mangaInfo.chapterName.Count - 1 do
        clbChapterList.Items.Add(mangaInfo.chapterName.Strings[i]);
  end;
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
        UTF8Length(Lines.Text)-
        UTF8Length(Lines[Lines.Count-1])-Lines.Count-1,
        UTF8Length(Lines[Lines.Count-1]),
        fp);
      {$ENDIF}
      Lines.Add(infoText);
    end;
end;

procedure TMainForm.ShowInformation;
var
  cp: TPoint;
begin
  itAnimate.Enabled:= FALSE;
  // ---------------------------------------------------
  pcMain.PageIndex:= 1;
  if edSaveTo.Text='' then
    edSaveTo.Text:= options.ReadString('saveto', 'SaveTo', '');

  with rmInformation do
  begin
    imCover.Picture.Assign(nil);
   // cover.Clear;

    Clear;

    if SubThread.mangaListPos <> -1 then
    begin
      mangaInfo.title:= dataProcess.Param[dataProcess.filterPos.Items[SubThread.mangaListPos], DATA_PARAM_NAME];
      mangaInfo.link := dataProcess.Param[dataProcess.filterPos.Items[SubThread.mangaListPos], DATA_PARAM_LINK];
    end
    else
      mangaInfo.link:= edURL.Text;

    AddTextToInfo(infoName, mangaInfo.title+#10#13);
    AddTextToInfo(infoAuthors, mangaInfo.authors+#10#13);
    AddTextToInfo(infoArtists, mangaInfo.artists+#10#13);
    AddTextToInfo(infoGenres , mangaInfo.genres +#10#13);
    if mangaInfo.status = '0' then
      AddTextToInfo(infoStatus, cbFilterStatus.Items.Strings[0]+#10#13)
    else
      AddTextToInfo(infoStatus, cbFilterStatus.Items.Strings[1]+#10#13);
    AddTextToInfo(infoLink, mangaInfo.url+#10#13);
    AddTextToInfo(infoSummary, StringBreaks(mangaInfo.summary));
    cp.X:= 0; cp.Y:= 0; CaretPos:= cp;
  end;
  AddChapterNameToList;
  if mangaInfo.link <> '' then
    btReadOnline.Enabled:= TRUE;
end;

procedure TMainForm.RunGetList;
begin
  if (MessageDlg('', stDlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0)=mrYes) AND
     (NOT isUpdating) then
  begin
    isUpdating:= TRUE;
    updateDB:= TUpdateDBThread.Create;
    updateDB.websiteName:= cbSelectManga.Items[cbSelectManga.ItemIndex];
    updateDB.isSuspended:= FALSE;
  end;
end;

procedure TMainForm.LoadOptions;
begin
  if options.ReadBool('connections', 'UseProxy', FALSE) then
  begin
    Host:= options.ReadString('connections', 'Host', '');
    Pass:= options.ReadString('connections', 'Pass', '');
    Port:= options.ReadString('connections', 'Port', '');
    User:= options.ReadString('connections', 'User', '');
  end;
 // cbLanguages.ItemIndex := options.ReadInteger('languages', 'Select', 0);

  cbOptionMinimizeToTray.Checked := options.ReadBool('general', 'MinimizeToTray', FALSE);
  miDownloadHideCompleted.Checked:= options.ReadBool('general', 'HideCompleted', FALSE);
  batotoLastDirectoryPage:= mangalistIni.ReadInteger('general', 'batotoLastDirectoryPage', 244);
  cbOptionLetFMDDo.ItemIndex:= options.ReadInteger('general', 'LetFMDDo', 0);
  cbOptionLetFMDDoItemIndex := cbOptionLetFMDDo.ItemIndex;

  cbAddAsStopped.Checked := options.ReadBool('general', 'AddAsStopped', FALSE);
  LoadLanguage(options.ReadInteger('languages', 'Select', 0));

  DLManager.maxDLTasks         := options.ReadInteger('connections', 'NumberOfTasks', 1);
  DLManager.maxDLThreadsPerTask:= options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
  DLManager.retryConnect       := options.ReadInteger('connections', 'Retry', 0);
  DLManager.compress           := options.ReadInteger('saveto', 'Compress', 0);

  cbOptionPathConvert.Checked  := options.ReadBool   ('saveto', 'PathConv', FALSE);
  cbOptionPathConvert.Checked  := options.ReadBool   ('saveto', 'PathConv', FALSE);
  cbOptionGenerateChapterName.Checked:= options.ReadBool('saveto', 'GenChapName', FALSE);
  cbOptionGenerateMangaFolderName.Checked:= options.ReadBool('saveto', 'GenMangaName', TRUE);
  cbOptionAutoCheckUpdate.Checked:= options.ReadBool('update', 'AutoCheckUpdateAtStartup', TRUE);
end;

procedure TMainForm.LoadMangaOptions;
var
  i, j: Cardinal;
  languages,
  l: TStringList;
  s: String;
begin
  l:= TStringList.Create;
  websiteLanguage.Clear;
  websiteName.Clear;
  websiteSelect.Clear;

  s:= mangalistIni.ReadString('general', 'MangaListAvail', '');
  GetParams(websiteName, s);

  s:= mangalistIni.ReadString('general', 'Languages', '');
  GetParams(websiteLanguage, s);

  cbSelectManga.Items.Clear;
  s:= mangalistIni.ReadString('general', 'MangaListSelect', '');
  GetParams(l, s);

  clbOptionMangaSiteSelection.Clear;
  for i:= 0 to websiteName.Count-1 do
    clbOptionMangaSiteSelection.Items.Add('[ '+websiteLanguage.Strings[i]+' ]  '+websiteName.Strings[i]);

  for i:= 0 to l.Count-1 do
  begin
    j:= StrToInt(l.Strings[i]);
    cbSelectManga.Items.Add(websiteName.Strings[j]);
    clbOptionMangaSiteSelection.Checked[j]:= TRUE;
  end;
  cbSelectManga.ItemIndex:= 0;
  dataProcess.LoadFromFile(cbSelectManga.Items.Strings[0]);
  dataProcess.website:= cbSelectManga.Items[0];

  l.Free;
end;

function  TMainForm.SaveMangaOptions: String;
var
  i: Cardinal;
begin
  Result:= '';
  for i:= 0 to clbOptionMangaSiteSelection.Items.Count-1 do
  begin
    if clbOptionMangaSiteSelection.Checked[i] then
      Result:= Result+IntToStr(i)+SEPERATOR;
  end;
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
  if vtMangaList.RootNodeCount = 0 then
  begin
    MessageDlg('Info', '"'+name+'" not found!',
               mtInformation, [mbYes], 0);
    exit;
  end;
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
  HideCompletedTasks(miDownloadHideCompleted.Checked);
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
  s,
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

  mnUpdateList.Caption  := language.ReadString(lang, 'mnUpdateListCaption', '');
  mnUpdateDownFromServer.Caption:= language.ReadString(lang, 'mnUpdateDownFromServerCaption', '');

  btSearch      .Hint   := language.ReadString(lang, 'btSearchHint', '');
  btRemoveFilter.Hint   := language.ReadString(lang, 'btRemoveFilterHint', '');
  btRemoveFilterLarge.Hint:= btRemoveFilter.Hint;

  edSaveTo.EditLabel.Caption:= language.ReadString(lang, 'edSaveToEditLabelCaption', '');
  btDownload    .Caption  := language.ReadString(lang, 'btDownloadCaption', '');
  btReadOnline  .Caption  := language.ReadString(lang, 'btReadOnlineCaption', '');
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
  tsGeneral.Caption       := language.ReadString(lang, 'tsGeneralCaption', '');
  tsConnections.Caption   := language.ReadString(lang, 'tsConnectionsCaption', '');
  tsSaveTo.Caption        := language.ReadString(lang, 'tsSaveToCaption', '');
  tsDialogs.Caption       := language.ReadString(lang, 'tsDialogsCaption', '');
  tsWebsites.Caption      := language.ReadString(lang, 'tsWebsitesCaption', '');
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
  miDownloadHideCompleted.Caption:= language.ReadString(lang, 'miDownloadHideCompletedTasksCaption', '');
  miDownloadMerge.Caption := language.ReadString(lang, 'miDownloadMergeTasksCaption', '');
  miOpenFolder.Caption    := language.ReadString(lang, 'miOpenFolder', '');

  miChapterListCheckSelected.Caption:= language.ReadString(lang, 'miChapterListCheckSelectedCaption', '');
  miChapterListUncheckSelected.Caption:= language.ReadString(lang, 'miChapterListUncheckSelectedCaption', '');
  miChapterListCheckAll.Caption:= language.ReadString(lang, 'miChapterListCheckAllCaption', '');
  miChapterListUncheckAll.Caption:= language.ReadString(lang, 'miChapterListUncheckAllCaption', '');

  miFavoritesRemove.Caption:= language.ReadString(lang, 'miFavoritesRemoveCaption', '');
  miFavoritesChangeCurrentChapter.Caption:= language.ReadString(lang, 'miFavoritesChangeCurrentChapterCaption', '');
  miFavoritesChangeSaveTo.Caption:= language.ReadString(lang, 'miFavoritesChangeSaveToCaption', '');
  miMangaListAddToFavorites.Caption:= language.ReadString(lang, 'miMangaListAddToFavoritesCaption', '');
  miHighlightNewManga.Caption:= language.ReadString(lang, 'miHighlightNewMangaCaption', '');
  miOpenFolder2.Caption  := miOpenFolder.Caption;

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

  lbOptionLanguage.Caption    := language.ReadString(lang, 'lbOptionLanguageCaption', '');
  lbOptionNewMangaTime.Caption:= language.ReadString(lang, 'lbOptionNewMangaTimeCaption', '');
  lbOptionMaxParallel.Caption := Format(language.ReadString(lang, 'lbOptionMaxParallelCaption', ''), [seOptionMaxParallel.MaxValue]);
  lbOptionMaxThread.Caption   := Format(language.ReadString(lang, 'lbOptionMaxThreadCaption', ''), [seOptionMaxThread.MaxValue]);
  lbOptionMaxRetry.Caption    := language.ReadString(lang, 'lbOptionMaxRetryCaption', '');
  lbOptionDialogs.Caption     := language.ReadString(lang, 'lbOptionDialogsCaption', '');

  cbOptionMinimizeToTray.Caption:= language.ReadString(lang, 'cbOptionMinimizeToTrayCaption', '');
  cbOptionAutoCheckUpdate.Caption:= language.ReadString(lang, 'cbOptionAutoCheckUpdateCaption', '');
  cbOptionPathConvert.Caption := language.ReadString(lang, 'cbOptionPathConvertCaption', '');
  cbOptionGenerateChapterName.Caption    := language.ReadString(lang, 'cbOptionGenerateChapterNameCaption', '');
  cbOptionGenerateMangaFolderName.Caption:= language.ReadString(lang, 'cbOptionGenerateMangaFolderNameCaption', '');
  cbOptionShowQuitDialog.Caption      := language.ReadString(lang, 'cbOptionShowQuitDialogCaption', '');
  cbOptionShowDeleteTaskDialog.Caption:= language.ReadString(lang, 'cbOptionShowDeleteTaskDialogCaption', '');

  stDownloadManga          := language.ReadString(lang, 'stDownloadManga', '');
  stDownloadStatus         := language.ReadString(lang, 'stDownloadStatus', '');
  stDownloadProgress       := language.ReadString(lang, 'stDownloadProgress', '');
  stDownloadWebsite        := language.ReadString(lang, 'stDownloadWebsite', '');
  stDownloadSaveto         := language.ReadString(lang, 'stDownloadSaveto', '');
  stDownloadAdded          := language.ReadString(lang, 'stDownloadAdded', '');
  stFavoritesCurrentChapter:= language.ReadString(lang, 'stFavoritesCurrentChapter', '');

  stFavoritesCheck         := language.ReadString(lang, 'stFavoritesCheck', '');
  stFavoritesChecking      := language.ReadString(lang, 'stFavoritesChecking', '');

  stUpdaterCheck           := language.ReadString(lang, 'stUpdaterCheck', '');
  btCheckVersion.Caption   := stUpdaterCheck;

  stDlgUpdaterVersionRequire:= language.ReadString(lang, 'stDlgUpdaterVersionRequire', '');
  stDlgUpdaterIsRunning    := language.ReadString(lang, 'stDlgUpdaterIsRunning', '');
  stDlgLatestVersion       := language.ReadString(lang, 'stDlgLatestVersion', '');
  stDlgNewVersion          := language.ReadString(lang, 'stDlgNewVersion', '');
  stDlgURLNotSupport       := language.ReadString(lang, 'stDlgURLNotSupport', '');
  stDldMangaListSelect     := language.ReadString(lang, 'stDldMangaListSelect', '');
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
  stDlgRemoveCompletedManga:= language.ReadString(lang, 'stDlgRemoveCompletedManga', '');
  stDlgUpdaterWantToUpdateDB:= language.ReadString(lang, 'stDlgUpdaterWantToUpdateDB', '');
  stDlgUpdaterCannotConnectToServer:= language.ReadString(lang, 'stDlgUpdaterCannotConnectToServer', '');

  lbOptionLetFMDDo.Caption := language.ReadString(lang, 'lbOptionLetFMDDoCaption', '');
  s:= language.ReadString(lang, 'cbOptionLetFMDDo', '');

  // add information to cbOptionLetFMDDo
  cbOptionLetFMDDo.Items.Clear;
  GetParams(TStringList(cbOptionLetFMDDo.Items), s);
 // cbOptionLetFMDDo.ItemIndex:= 0;

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
  if DLManager.isRunningBackup then exit;
  DLManager.Backup;
end;

end.

