{
        File: frmMain.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmMain;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, ComCtrls, Buttons, Spin, Menus, VirtualTrees, RichMemo,
  IniFiles, simpleipc, UTF8Process, lclproc, types, strutils, LCLIntf,
  LazUTF8, AnimatedGif, uBaseUnit, uData, uDownloadsManager,
  uFavoritesManager, uUpdateThread, uUpdateDBThread, uSubThread, uSilentThread,
  uMisc, uGetMangaInfosThread, USimpleException, ActiveX;

type
  TDoFMDType = (DoFMDNothing, DoFMDUpdate, DoFMDExit, DoFMDShutdown, DoFMDHibernate);

  { TMainForm }

  TMainForm = class(TForm)
    appPropertiesMain: TApplicationProperties;
    Bevel1: TBevel;
    btAddToFavorites: TBitBtn;
    btBrowse: TSpeedButton;
    btCancelFavoritesCheck: TSpeedButton;
    btOptionBrowse: TSpeedButton;
    btChecks: TSpeedButton;
    btDonate: TImage;
    btFavoritesImport: TBitBtn;
    btFilter: TBitBtn;
    btFilterReset: TBitBtn;
    btOptionApply: TBitBtn;
    btReadOnline: TBitBtn;
    btRemoveFilter: TSpeedButton;
    btSearchClear: TSpeedButton;
    btUpdateList: TSpeedButton;
    btURL: TSpeedButton;
    cbOptionAutoRemoveCompletedManga: TCheckBox;
    cbOptionUpdateListNoMangaInfo: TCheckBox;
    cbOptionEnableLoadCover: TCheckBox;
    cbOptionDigitVolume: TCheckBox;
    cbOptionDigitChapter: TCheckBox;
    cbOptionMangaFoxRemoveWatermarks: TCheckBox;
    cbOptionLiveSearch: TCheckBox;
    cbOptionUpdateListRemoveDuplicateLocalData : TCheckBox;
    cbUseRegExpr: TCheckBox;
    cbOptionProxyType: TComboBox;
    cbOptionOneInstanceOnly: TCheckBox;
    edOptionExternal: TEdit;
    edURL: TEdit;
    gbOptionExternal: TGroupBox;
    IconDL: TImageList;
    IconDL2: TImageList;
    IconList2: TImageList;
    IconMed: TImageList;
    IconSmall: TImageList;
    itMonitor: TTimer;
    itStartup: TIdleTimer;
    lbOptionProxyType: TLabel;
    lbOptionRenameDigits: TLabel;
    lbFilterHint: TLabel;
    lbOptionExternal: TLabel;
    lbOptionCustomRenameHint: TLabel;
    lbOptionCustomRenameHint1: TLabel;
    lbOptionExternalHint: TLabel;
    medURLCut: TMenuItem;
    medURLCopy: TMenuItem;
    medURLPaste: TMenuItem;
    medURLPasteandgo: TMenuItem;
    medtURLDelete: TMenuItem;
    MenuItem15: TMenuItem;
    medURLSelectAll: TMenuItem;
    MenuItem17: TMenuItem;
    medURLUndo: TMenuItem;
    miDLViewMangaInfo: TMenuItem;
    MenuItem9: TMenuItem;
    miDeleteTask: TMenuItem;
    miDeleteTaskData: TMenuItem;
    miOpenWith: TMenuItem;
    miOpenWith2: TMenuItem;
    pnThumbContainer: TPanel;
    pnMainTop: TPanel;
    btVisitMyBlog: TBitBtn;
    btCheckVersion: TBitBtn;
    btFavoritesCheckNewChapter: TBitBtn;
    btDownload: TBitBtn;
    btRemoveFilterLarge: TBitBtn;
    cbOptionAutoCheckUpdate: TCheckBox;
    cbOptionBatotoUseIE: TCheckBox;
    cbOptionShowDeleteTaskDialog: TCheckBox;
    cbOptionShowBatotoSG: TCheckBox;
    cbOptionShowAllLang: TCheckBox;
    cbOptionAutoDlFav: TCheckBox;
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
    CheckBox4: TCheckBox;
    cbOnlyNew: TCheckBox;
    cbAddAsStopped: TCheckBox;
    cbAddToFavorites: TCheckBox;
    cbOptionShowQuitDialog: TCheckBox;
    cbOptionPathConvert: TCheckBox;
    cbOptionGenerateChapterName: TCheckBox;
    cbOptionGenerateMangaFolderName: TCheckBox;
    cbOptionMinimizeToTray: TCheckBox;
    cbOptionShowFavoriteDialog: TCheckBox;
    cbOptionAutoNumberChapter: TCheckBox;
    cbOptionAutoCheckFavStartup: TCheckBox;
    cbSearchFromAllSites: TCheckBox;
    Checkbox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    cbFilterStatus: TComboBox;
    cbLanguages: TComboBox;
    cbOptionLetFMDDo: TComboBox;
    edFilterSummary: TEdit;
    edFilterTitle: TEdit;
    edFilterAuthors: TEdit;
    edFilterArtists: TEdit;
    edCustomGenres: TEdit;
    edOptionCustomRename: TEdit;
    edOptionHost: TEdit;
    edOptionPass: TEdit;
    edOptionPort: TEdit;
    edOptionUser: TEdit;
    edSaveTo: TLabeledEdit;
    edSearch: TEdit;
    gbDialogs: TGroupBox;
    gbOptionProxy: TGroupBox;
    gbOptionRenaming: TGroupBox;
    gbOptionFavorites: TGroupBox;
    gbMisc: TGroupBox;
    IconList: TImageList;
    itSaveDownloadedList: TIdleTimer;
    itRefreshForm: TIdleTimer;
    itCheckForChapters: TIdleTimer;
    itAnimate: TIdleTimer;
    imCover: TImage;
    edOptionDefaultPath: TLabeledEdit;
    lbOptionCustomRename: TLabel;
    lbOptionPDFQuality: TLabel;
    lbOptionAutoCheckMinutes: TLabel;
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
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miFavoritesViewInfos: TMenuItem;
    miChapterListHighlight: TMenuItem;
    mnDownload1Click: TMenuItem;
    mnUpdate1Click: TMenuItem;
    miMangaListDownloadAll: TMenuItem;
    miMangaListViewInfos: TMenuItem;
    mnUpdateList: TMenuItem;
    mnUpdateDownFromServer: TMenuItem;
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
    miDownloadResume: TMenuItem;
    miDownloadRemove: TMenuItem;
    miChapterListUncheckAll: TMenuItem;
    pcLeft: TPageControl;
    pbWait: TPaintBox;
    pmChapterList: TPopupMenu;
    pnOptions: TPageControl;
    pnChapterList: TPanel;
    pnFilter: TPanel;
    pnGenres: TPanel;
    pcMain: TPageControl;
    pnInfomation: TPanel;
    pmDownload: TPopupMenu;
    pmFavorites: TPopupMenu;
    pmMangaList: TPopupMenu;
    pmUpdate: TPopupMenu;
    pmEditURL: TPopupMenu;
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
    seOptionCheckMinutes: TSpinEdit;
    seOptionPDFQuality: TSpinEdit;
    seOptionDigitVolume: TSpinEdit;
    seOptionDigitChapter: TSpinEdit;
    btAbortUpdateList: TSpeedButton;
    spInfos: TSplitter;
    spMainSplitter: TSplitter;
    sbMain: TStatusBar;
    sbUpdateList: TStatusBar;
    tmBackup: TIdleTimer;
    tvDownloadFilter: TTreeView;
    tsDownloadFilter: TTabSheet;
    tsMangaList: TTabSheet;
    tsMisc: TTabSheet;
    tsUpdate: TTabSheet;
    tsAbout: TTabSheet;
    tsWebsites: TTabSheet;
    tsDialogs: TTabSheet;
    TrayIcon: TTrayIcon;
    tsGeneral: TTabSheet;
    tsFavorites: TTabSheet;
    lbOptionPDFQualityHint: TTabSheet;
    tsConnections: TTabSheet;
    tsOption: TTabSheet;
    tsFilter: TTabSheet;
    tsInformation: TTabSheet;
    tsDownload: TTabSheet;
    clbChapterList: TVirtualStringTree;
    vtOptionMangaSiteSelection: TVirtualStringTree;
    vtFavorites: TVirtualStringTree;
    vtDownload: TVirtualStringTree;
    vtMangaList: TVirtualStringTree;
    mangaCover: TPicture;

    procedure appPropertiesMainShowHint(var HintStr: String;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure btAddToFavoritesClick(Sender: TObject);
    procedure btAbortUpdateListClick(Sender: TObject);
    procedure btCancelFavoritesCheckClick(Sender: TObject);
    procedure btChecksClick(Sender: TObject);
    procedure btCheckVersionClick(Sender: TObject);
    procedure btDonateClick(Sender: TObject);
    procedure btFavoritesImportClick(Sender: TObject);
    procedure btReadOnlineClick(Sender: TObject);
    procedure btSearchClearClick(Sender: TObject);
    procedure btUpdateListClick(Sender: TObject);
    procedure btURLClick(Sender: TObject);
    procedure btVisitMyBlogClick(Sender: TObject);
    procedure cbOptionDigitChapterChange(Sender: TObject);
    procedure cbOptionDigitVolumeChange(Sender: TObject);
    procedure cbSelectMangaChange(Sender: TObject);
    procedure clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure clbChapterListFreeNode(Sender : TBaseVirtualTree;
      Node : PVirtualNode);
    procedure clbChapterListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure clbChapterListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure clbChapterListInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchClick(Sender: TObject);
    procedure edSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edURLKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure btBrowseClick(Sender: TObject);
    procedure btOptionBrowseClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btFavoritesCheckNewChapterClick(Sender: TObject);
    procedure btOptionApplyClick(Sender: TObject);

    procedure btFilterClick(Sender: TObject);
    procedure btFilterResetClick(Sender: TObject);
    procedure btRemoveFilterClick(Sender: TObject);

    procedure cbAddAsStoppedChange(Sender: TObject);
    procedure cbOptionUseProxyChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure itAnimateTimer(Sender: TObject);
    procedure itCheckForChaptersTimer(Sender: TObject);
    procedure itMonitorTimer(Sender: TObject);
    procedure itRefreshFormTimer(Sender: TObject);
    procedure itSaveDownloadedListTimer(Sender: TObject);
    procedure itStartupTimer(Sender: TObject);
    procedure medURLCutClick(Sender: TObject);
    procedure medURLCopyClick(Sender: TObject);
    procedure medURLPasteClick(Sender: TObject);
    procedure medURLPasteandgoClick(Sender: TObject);
    procedure medtURLDeleteClick(Sender: TObject);
    procedure medURLSelectAllClick(Sender: TObject);
    procedure medURLUndoClick(Sender: TObject);
    procedure miDLViewMangaInfoClick(Sender: TObject);
    procedure miChapterListHighlightClick(Sender: TObject);
    procedure miDeleteTaskClick(Sender: TObject);
    procedure miDeleteTaskDataClick(Sender: TObject);
    procedure miDownloadMergeClick(Sender: TObject);
    procedure miFavoritesViewInfosClick(Sender: TObject);
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
    procedure miDownloadResumeClick(Sender: TObject);
    procedure miDownloadRemoveClick(Sender: TObject);
    procedure miDownloadStopClick(Sender: TObject);
    procedure miMangaListDownloadAllClick(Sender: TObject);
    procedure miMangaListViewInfosClick(Sender: TObject);
    procedure miOpenFolder2Click(Sender: TObject);
    procedure miOpenFolderClick(Sender: TObject);
    procedure miOpenWith2Click(Sender: TObject);
    procedure miOpenWithClick(Sender: TObject);
    procedure miUpClick(Sender: TObject);
    procedure mnDownload1ClickClick(Sender: TObject);
    procedure mnUpdate1ClickClick(Sender: TObject);
    procedure mnUpdateDownFromServerClick(Sender: TObject);
    procedure mnUpdateListClick(Sender: TObject);

    procedure pcMainChange(Sender: TObject);
    procedure pmDownloadPopup(Sender: TObject);
    procedure pmEditURLPopup(Sender: TObject);
    procedure pmFavoritesPopup(Sender: TObject);
    procedure pmMangaListPopup(Sender: TObject);
    procedure sbUpdateListDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure seOptionCheckMinutesChange(Sender: TObject);
    procedure spMainSplitterMoved(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure tvDownloadFilterSelectionChanged(Sender: TObject);
    procedure UniqueInstanceFMDOtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: array of String);
    procedure vtDownloadAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure vtDownloadColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtDownloadDragAllowed(Sender : TBaseVirtualTree;
      Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
    procedure vtDownloadDragDrop(Sender : TBaseVirtualTree; Source : TObject;
      DataObject : IDataObject; Formats : TFormatArray; Shift : TShiftState;
      const Pt : TPoint; var Effect : LongWord; Mode : TDropMode);
    procedure vtDownloadDragOver(Sender : TBaseVirtualTree; Source : TObject;
      Shift : TShiftState; State : TDragState; const Pt : TPoint;
      Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
    procedure vtDownloadFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtDownloadGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtDownloadGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtDownloadGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtDownloadHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vtDownloadInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtDownloadKeyDown(Sender : TObject; var Key : Word;
      Shift : TShiftState);
    procedure vtDownloadKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vtFavoritesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtFavoritesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFavoritesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtFavoritesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFavoritesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtFavoritesHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vtFavoritesInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMangaListColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtMangaListDragAllowed(Sender : TBaseVirtualTree;
      Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
    procedure vtMangaListDragOver(Sender : TBaseVirtualTree; Source : TObject;
      Shift : TShiftState; State : TDragState; const Pt : TPoint;
      Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
    // for search feature
    procedure vtMangaListInitSearchNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

    procedure vtMangaListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMangaListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtMangaListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtMangaListInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tmBackupTimer(Sender: TObject);
    procedure vtOptionMangaSiteSelectionChange(Sender : TBaseVirtualTree;
      Node : PVirtualNode);
    procedure vtOptionMangaSiteSelectionFocusChanged(Sender : TBaseVirtualTree;
      Node : PVirtualNode; Column : TColumnIndex);
    procedure vtOptionMangaSiteSelectionFreeNode(Sender : TBaseVirtualTree;
      Node : PVirtualNode);
    procedure vtOptionMangaSiteSelectionGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtOptionMangaSiteSelectionGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vtOptionMangaSiteSelectionInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DisableAddToFavorites(webs: String);
  private
    PrevWindowState: TWindowState;
    procedure vtDownloadMoveItems(NextIndex : Cardinal; Mode : TDropMode);
  protected
    procedure FMDInstanceReceiveMsg(Sender: TObject);
  public
    ulTotalPtr, ulWorkPtr: Cardinal;
    optionMangaSiteSelectionNodes: array of PVirtualNode;
    LastSearchStr: String;
    LastSearchWeb: String;
    isStartup, isExiting: Boolean;
    // for manga website that available for visible on selection list
    //websiteName     :TStringList;
    //websiteLanguage :TStringList;

    isRunDownloadFilter: Boolean;
    isCanRefreshForm, isUpdating: Boolean;
    revisionIni, updates, mangalistIni, options: TIniFile;
    FavoriteManager: TFavoriteManager;
    dataProcess: TDataProcess;
    mangaInfo: TMangaInfo;
    ChapterList: array of TChapterStateItem;
    DLManager: TDownloadManager;
    updateDB: TUpdateDBThread;
    updateList: TUpdateMangaManagerThread;
    SilentThreadManager: TSilentThreadManager;
    ticks: Cardinal;
    backupTicks: Cardinal;
    // animation gif
    gifWaiting: TAnimatedGif;
    gifWaitingRect: TRect;

    // doing stuff like get manga info, compress, ...
    SubThread: TSubThread;
    isSubthread: Boolean;
    GetInfosThread: TGetMangaInfosThread;
    isGetMangaInfos: Boolean;

    // repaint treeview
    procedure tvDownloadFilterRepaint;

    // generate >> nodes
    procedure GenerateNodes;

    // load about information
    procedure LoadAbout;

    procedure CloseNow;

    procedure CheckForTopPanel;
    // en: Too lazy to add it one by one
    procedure InitCheckboxes;

    // download task filters
    procedure ShowAllTasks;
    procedure ShowCompletedTasks;
    procedure ShowInProgressTasks;
    procedure ShowStoppedTasks;

    procedure ShowTasksOnCertainDays(const L, H: longint);
    procedure ShowTodayTasks;
    procedure ShowYesterdayTasks;
    procedure ShowOneWeekTasks;
    procedure ShowOneMonthTasks;
    procedure vtDownloadFilters;

    procedure AddChapterNameToList;

    // Add text to TRichMemo
    procedure AddTextToInfo(title, infoText: String);

    // Show manga information
    procedure ShowInformation(const title, website, link: String);

    // get manga list from server
    procedure RunGetList;

    // Load config from config.ini
    procedure LoadOptions;

    // Load config from mangalist.ini
    procedure LoadMangaOptions;

    function SaveMangaOptions: String;

    procedure UpdateVtChapter;
    procedure UpdateVtDownload;
    procedure UpdateVtFavorites;

    // Load form information, like previous position, size, ...
    procedure LoadFormInformation;
    procedure SaveFormInformation;

    // load language file
    procedure LoadLanguage(const pos: Integer);

    // exception handle
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    { public declarations }
  end;

resourcestring
  RS_Loading = 'Loading ...';

var
  //Instance
  FMDInstance: TSimpleIPCServer;

  MainForm: TMainForm;
  INIAdvanced: TIniFileR;

  // update fmd through main thread
  DoAfterFMD: TDoFMDType;
  FUpdateURL: String;

resourcestring
  RS_HintFavoriteProblem = 'There is a problem with this data!'+ LineEnding +
                           'Removing and re-adding this data may fix the problem.';
  RS_DlgTitleExistInDLlist = 'This title are already in download list.' + LineEnding +
                             'Do you want to download it anyway?';

implementation

{$R *.lfm}

uses
  frmImportFavorites, RegExpr, Clipbrd;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  fs: TFileStream;
begin
  Randomize;
  InitSimpleExceptionHandler(ChangeFileExt(Application.ExeName, '.log'));
  AddIgnoredException('EImagingError');
  AddIgnoredException('ERegExpr');
  SaveIgnoredExeptionToFile;
  SilentThreadManager := TSilentThreadManager.Create;
  btAbortUpdateList.Parent := sbUpdateList;
  INIAdvanced := TIniFileR.Create(fmdDirectory + CONFIG_FOLDER + CONFIG_ADVANCED);
  isRunDownloadFilter := False;
  isCanRefreshForm := True;
  isUpdating := False;
  isExiting := False;
  isSubthread := False;
  isGetMangaInfos := False;
  DoAfterFMD := DoFMDNothing;
  fmdDirectory := CorrectFilePath(GetCurrentDirUTF8);
  Application.HintHidePause := 10000;
  sbUpdateList.DoubleBuffered := True;

  // TrayIcon
  TrayIcon.Icon.Assign(Application.Icon);
  PrevWindowState := wsNormal;

  // Load readme.rtf to rmAbout
  rmAbout.Clear;
  if FileExistsUTF8(README_FILE) then
  begin
    fs := TFileStream.Create(README_FILE, fmOpenRead or fmShareDenyNone);
    rmAbout.LoadRichText(fs);
    fs.Free;
  end;

  dataProcess := TDataProcess.Create;
  DLManager := TDownloadManager.Create;
  DLManager.Restore;

  FavoriteManager := TFavoriteManager.Create;
  FavoriteManager.OnUpdateFavorite := @UpdateVtFavorites;
  FavoriteManager.OnUpdateDownload := @UpdateVtDownload;
  FavoriteManager.DLManager := DLManager;

  // Load config.ini
  options := TIniFile.Create(fmdDirectory + CONFIG_FOLDER + CONFIG_FILE);
  options.CacheUpdates := True;

  // Load revision.ini
  revisionIni := TIniFile.Create(fmdDirectory + CONFIG_FOLDER + REVISION_FILE);
  options.CacheUpdates := False;

  // Load updates.ini
  updates := TIniFile.Create(fmdDirectory + CONFIG_FOLDER + UPDATE_FILE);
  updates.CacheUpdates := False;

  // Load mangalist.ini
  mangalistIni := TIniFile.Create(fmdDirectory + CONFIG_FOLDER + MANGALIST_FILE);
  mangalistIni.CacheUpdates := True;

  LoadOptions;
  isStartup := False;
  LoadMangaOptions;
  LoadFormInformation;

  // ShowInformation;
  mangaInfo := TMangaInfo.Create;
  //mangaInfo.chapterName := TStringList.Create;
  //mangaInfo.chapterLinks := TStringList.Create;

  vtDownload.NodeDataSize := SizeOf(TDownloadInfo) - 4;
  vtDownload.RootNodeCount := DLManager.containers.Count;

  vtFavorites.NodeDataSize := SizeOf(TFavoriteInfo);
  UpdateVtFavorites;

  InitCheckboxes;

  //lbMode.Caption := Format(stModeAll, [dataProcess.filterPos.Count]);

  if pcMain.PageIndex = 4 then
    pcMain.PageIndex := 0;

  CheckForTopPanel;
  DLManager.CheckAndActiveTaskAtStartup;
  TrayIcon.Show;

  // load some necessary options at startup
  Revision := revisionIni.ReadInteger('general', 'Revision', 0);
  revisionIni.Free;

  seOptionNewMangaTime.Value := options.ReadInteger('general', 'NewMangaTime', 3);
  miHighLightNewManga.Checked := options.ReadBool('general', 'HighlightNewManga', True);
  miChapterListHighlight.Checked :=
    options.ReadBool('general', 'HighlightDownloadedChapters', True);
  cbOptionShowQuitDialog.Checked := options.ReadBool('dialogs', 'ShowQuitDialog', True);
  cbOptionShowDeleteTaskDialog.Checked :=
    options.ReadBool('dialogs', 'ShowDeleteDldTaskDialog', True);
  cbOptionShowFavoriteDialog.Checked :=
    options.ReadBool('dialogs', 'ShowFavoritesDialog', True);
  currentJDN := GetCurrentJDN;

  // read online
  btDownload.Enabled := False;
  btReadOnline.Enabled := False;
  btAddToFavorites.Enabled := False;

  // subthread
  SubThread := TSubThread.Create;

  // why this doesn't work on some systems ?
  case pcMain.TabIndex of
    5: LoadAbout;
  end;
  cbOptionLetFMDDo.ItemIndex := options.ReadInteger('general', 'LetFMDDo', 0);

  // waiting gif
  if FileExists(IMAGE_FOLDER + 'waiting.gif') then
  begin
    gifWaiting := TAnimatedGif.Create(IMAGE_FOLDER + 'waiting.gif');
    gifWaiting.EraseColor := Self.Color;
    gifWaiting.BackgroundMode := gbmSaveBackgroundOnce;
    gifWaitingRect.Left := 53;
    gifWaitingRect.Top := 84;
    gifWaitingRect.Right := 101;
    gifWaitingRect.Bottom := 131;
  end;

  mangaCover := TPicture.Create;

  // generate nodes
  GenerateNodes;
  tvDownloadFilterRepaint;

  // refresh sort
  if DLManager.containers.Count > 1 then
  begin
    DLManager.SortDirection := Boolean(vtDownload.Header.SortDirection);
    //DLManager.Sort(Column);
    //DLManager.SortNatural(vtDownload.Header.SortColumn);    //Natural Sorting
    vtDownload.Repaint;
  end;
  if FavoriteManager.Favorites.Count > 0 then
  //if FavoriteManager.Favorites.Count > 0 then
  begin
    FavoriteManager.SortDirection := Boolean(vtFavorites.Header.SortDirection);
    FavoriteManager.SortNatural(vtFavorites.Header.SortColumn);
    vtFavorites.Repaint;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if cbOptionShowQuitDialog.Checked and (DoAfterFMD = DoFMDNothing) then
  begin
    if MessageDlg('', stDlgQuit, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      CloseAction := caNone;
      Exit;
    end;
  end;
  CloseAction := caFree;
  CloseNow;
end;

procedure TMainForm.CloseNow;
begin
  tmBackup.Enabled := False;
  itSaveDownloadedList.Enabled := False;
  itRefreshForm.Enabled := False;
  itCheckForChapters.Enabled := False;
  itAnimate.Enabled := False;
  itStartup.Enabled := False;
  itMonitor.Enabled := False;

  //Terminating all threads and wait for it
  if isGetMangaInfos then
  begin
    GetInfosThread.IsFlushed := True;
    GetInfosThread.Terminate;
    GetInfosThread.WaitFor;
  end;
  DLManager.StopAllDownloadTasksForExit;
  if isSubthread then
  begin
    SubThread.Terminate;
    SubThread.WaitFor;
  end;
  if isUpdating then
  begin
    updateList.Terminate;
    updateList.WaitFor;
  end;
  FavoriteManager.StopAllAndWait;
  SilentThreadManager.StopAllAndWait;

  if FMDInstance <> nil then
  begin
    FMDInstance.StopServer;
    FreeAndNil(FMDInstance);
  end;

  //Backup data
  if not dataProcess.isFilterAllSites then
    dataProcess.SaveToFile;
  DLManager.Backup;
  DLManager.BackupDownloadedChaptersList;
  isExiting := True;
  FavoriteManager.Backup;
  SaveFormInformation;
  options.UpdateFile;

  SetLength(optionMangaSiteSelectionNodes, 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SetLength(ChapterList, 0);
  FreeAndNil(mangaInfo);

  FreeAndNil(DLManager);
  FreeAndNil(SilentThreadManager);
  FreeAndNil(FavoriteManager);
  FreeAndNil(dataProcess);

  FreeAndNil(gifWaiting);
  FreeAndNil(mangaCover);

  FreeAndNil(mangalistIni);
  FreeAndNil(updates);
  FreeAndNil(options);
  FreeAndNil(INIAdvanced);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not isStartup then
    itStartup.Enabled := True;
end;

procedure TMainForm.cbOptionUseProxyChange(Sender: TObject);
begin
  gbOptionProxy.Enabled := cbOptionUseProxy.Checked;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsMinimized) then
  begin
    if cbOptionMinimizeToTray.Checked then
    begin
      ShowInTaskBar := stNever;
      Hide;
      if not TrayIcon.Visible then
        TrayIcon.Show;
    end;
  end
  else
    PrevWindowState := WindowState;
end;

procedure TMainForm.itAnimateTimer(Sender: TObject);
begin
  gifWaiting.Update(pbWait.Canvas, gifWaitingRect);
end;

procedure TMainForm.itCheckForChaptersTimer(Sender: TObject);
begin
  FavoriteManager.isAuto := True;
  FavoriteManager.isShowDialog := cbOptionShowFavoriteDialog.Checked;
  FavoriteManager.Run;
end;

procedure TMainForm.itMonitorTimer(Sender: TObject);
begin
  if DoAfterFMD = DoFMDUpdate then
  begin
    if FileExistsUTF8(fmdDirectory + 'updater.exe') then
      CopyFile(fmdDirectory + 'updater.exe', fmdDirectory + 'old_updater.exe');
    if FileExistsUTF8(fmdDirectory + 'old_updater.exe') then
    begin
      CloseNow;
      {$IFDEF USEADMIN}
      fmdRunAsAdmin(fmdDirectory + 'old_updater.exe',
        '-x -r 3 -a ' + FUpdateURL + ' -l ' + Application.ExeName, False);
      {$ELSE}
      RunExternalProcess(fmdDirectory + 'old_updater.exe',
        ['-x', '-r', '3', '-a', FUpdateURL, '-l', Application.ExeName], True, True);
      {$ENDIF}
      Self.Close;
    end;
  end
  else
  if DoAfterFMD <> DoFMDNothing then
  begin
    case DoAfterFMD of
      DoFMDShutdown: fmdPowerOff;
      DoFMDHibernate: fmdHibernate;
    end;
    Sleep(1000);
    Self.Close;
  end;
  DoAfterFMD := DoFMDNothing;
  itMonitor.Enabled := False;
end;

procedure TMainForm.itRefreshFormTimer(Sender: TObject);
begin
  if isCanRefreshForm then
  begin
    vtDownload.Repaint;
    isCanRefreshForm := False;
  end;
end;

procedure TMainForm.itSaveDownloadedListTimer(Sender: TObject);
begin
  DLManager.BackupDownloadedChaptersList;
end;

procedure TMainForm.itStartupTimer(Sender: TObject);
begin
  if not isStartup then
  begin
    Screen.Cursor := crHourGlass;
    itStartup.Enabled := False;
    isStartup := True;
    try
      //LoadMangaOptions;
      vtMangaList.NodeDataSize := SizeOf(TMangaListItem);
      vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
      lbMode.Caption := Format(stModeAll, [dataProcess.filterPos.Count]);
    finally
      Screen.Cursor := crDefault;
    end;
    if cbOptionAutoCheckUpdate.Checked then
      SubThread.CheckUpdate := True;
    SubThread.Start;
  end;
end;

procedure TMainForm.medURLCutClick(Sender: TObject);
begin
  edURL.CutToClipboard;
end;

procedure TMainForm.medURLCopyClick(Sender: TObject);
begin
  edURL.CopyToClipboard;
end;

procedure TMainForm.medURLPasteClick(Sender: TObject);
begin
  edURL.PasteFromClipboard;
end;

procedure TMainForm.medURLPasteandgoClick(Sender: TObject);
begin
  edURL.Clear;
  edURL.PasteFromClipboard;
  btURLClick(edURL);
end;

procedure TMainForm.medtURLDeleteClick(Sender: TObject);
begin
  edURL.ClearSelection;
end;

procedure TMainForm.medURLSelectAllClick(Sender: TObject);
begin
  edURL.SelectAll;
  edURL.SetFocus;
end;

procedure TMainForm.medURLUndoClick(Sender: TObject);
begin
  edURL.Undo;
end;

procedure TMainForm.miDLViewMangaInfoClick(Sender: TObject);
begin
  if vtDownload.Focused then
    with DLManager.containers[vtDownload.FocusedNode^.Index] do begin
      edURL.Text := FillMangaSiteHost(MangaSiteID, DownloadInfo.Link);
      btURLClick(btURL);
      pcMain.ActivePage := tsInformation;
    end;
end;

procedure TMainForm.miChapterListHighlightClick(Sender: TObject);
begin
  miChapterListHighlight.Checked := not miChapterListHighlight.Checked;
  options.WriteBool('general', 'HighlightDownloadedChapters',
    miChapterListHighlight.Checked);
  clbChapterList.Repaint;
end;

procedure TMainForm.miDeleteTaskClick(Sender: TObject);
var
  i: Integer;
  xNode: PVirtualNode;
begin
  // if NOT Assigned(vtDownload.FocusedNode) then exit;
  if (cbOptionShowDeleteTaskDialog.Checked) and
    (vtDownload.SelectedCount > 0) then
    if not (MessageDlg('', stDlgRemoveTask,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      Exit;

  if (vtDownload.SelectedCount = 1) and (Assigned(vtDownload.FocusedNode)) then
  begin
    DLManager.RemoveTask(vtDownload.FocusedNode^.Index);
    UpdateVtDownload;
    DLManager.Backup;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode := vtDownload.GetFirst;
    i := 0;
    while i < DLManager.containers.Count do
    begin
      if vtDownload.Selected[xNode] then
        DLManager.RemoveTask(i)
      else
        Inc(i);
      xNode := vtDownload.GetNext(xNode);
    end;
    UpdateVtDownload;
    DLManager.Backup;
  end;
end;

procedure TMainForm.miDeleteTaskDataClick(Sender: TObject);
var
  i, j: Cardinal;
  xNode: PVirtualNode;
  path: String;
begin
  // if NOT Assigned(vtDownload.FocusedNode) then exit;
  if (cbOptionShowDeleteTaskDialog.Checked) and
    (vtDownload.SelectedCount > 0) then
    if not (MessageDlg('', stDlgRemoveTask,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      Exit;

  if (vtDownload.SelectedCount = 1) and (Assigned(vtDownload.FocusedNode)) then
  begin
    i := vtDownload.FocusedNode^.Index;
    if DLManager.containers.Items[i].ChapterName.Count > 0 then
      for j := 0 to DLManager.containers.Items[i].ChapterName.Count - 1 do
      begin
        path := CorrectFilePath(DLManager.containers.Items[i].DownloadInfo.SaveTo +
          '/' + DLManager.containers.Items[i].ChapterName[j]);
        if path[Length(path)] = '/' then
          SetLength(path, Length(path) - 1);
        DeleteDirectory(path, False);
        if FileExistsUTF8(path + '.zip') then
          DeleteFileUTF8(path + '.zip');
        if FileExistsUTF8(path + '.cbz') then
          DeleteFileUTF8(path + '.cbz');
        if FileExistsUTF8(path + '.pdf') then
          DeleteFileUTF8(path + '.pdf');
      end;
    if IsDirectoryEmpty(DLManager.containers.Items[i].DownloadInfo.SaveTo) then
      DeleteDirectory(DLManager.containers.Items[i].DownloadInfo.SaveTo, False);
    DLManager.RemoveTask(i);
    UpdateVtDownload;
    DLManager.Backup;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode := vtDownload.GetFirst;
    i := 0;
    while i < DLManager.containers.Count do
    begin
      if vtDownload.Selected[xNode] then
      begin
        DeleteDirectory(DLManager.containers[i].DownloadInfo.SaveTo, False);
        DLManager.RemoveTask(i);
      end
      else
        Inc(i);
      xNode := vtDownload.GetNext(xNode);
    end;
    UpdateVtDownload;
    DLManager.Backup;
  end;
end;

procedure TMainForm.miDownloadMergeClick(Sender: TObject);
var
  i, j: Cardinal;
  // merge all finished tasks that have same manga name, website and directory
begin
  i := DLManager.containers.Count - 1;
  while i > 0 do
  begin
    if DLManager.containers.Items[i].Status = STATUS_FINISH then
    begin
      j := i - 1;
      while j > 0 do
      begin
        if (i <> j) and
          (DLManager.containers[j].Status = STATUS_FINISH) and
          SameText(DLManager.containers.Items[i].DownloadInfo.title,
          DLManager.containers.Items[j].DownloadInfo.title) and
          SameText(DLManager.containers.Items[i].DownloadInfo.website,
          DLManager.containers.Items[j].DownloadInfo.website) and
          SameText(DLManager.containers.Items[i].DownloadInfo.saveTo,
          DLManager.containers.Items[j].DownloadInfo.saveTo) then
        begin
          DLManager.containers.Items[i].ChapterLinks.Text :=
            DLManager.containers.Items[j].ChapterLinks.Text +
            DLManager.containers.Items[i].ChapterLinks.Text;
          DLManager.containers.Items[i].ChapterName.Text :=
            DLManager.containers.Items[j].ChapterName.Text +
            DLManager.containers.Items[i].ChapterName.Text;
          DLManager.containers.Items[i].DownloadInfo.dateTime :=
            DLManager.containers.Items[j].DownloadInfo.dateTime;
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

procedure TMainForm.miFavoritesViewInfosClick(Sender: TObject);
var
  title, website, link: String;
begin
  if (not vtFavorites.Focused) then
    Exit;
  btDownload.Enabled := False;
  pcMain.TabIndex := 1;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add('Loading ...');
  clbChapterList.Clear;

  website := FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.Website;
  link := FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.link;
  title := FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.Title;

  if isGetMangaInfos then
  begin
    GetInfosThread.IsFlushed := True;
    GetInfosThread.Terminate;
    //GetInfosThread.WaitFor;
  end;
  GetInfosThread := TGetMangaInfosThread.Create;
  GetInfosThread.MangaListPos := -2;
  GetInfosThread.Title := title;
  GetInfosThread.Website := website;
  GetInfosThread.Link := link;
  GetInfosThread.Start;

  if Assigned(gifWaiting) then
  begin
    itAnimate.Enabled := True;
    pbWait.Visible := True;
  end;

  if ExecRegExpr('^https?://', link) then
    edURL.Text := link
  else
    edURL.Text := WebsiteRoots[GetMangaSiteID(website), 1] + link;

  btDownload.Enabled := (clbChapterList.RootNodeCount > 0);
  btReadOnline.Enabled := (edURL.Text <> '');
end;

procedure TMainForm.miHighlightNewMangaClick(Sender: TObject);
begin
  miHighlightNewManga.Checked := not miHighlightNewManga.Checked;
  options.WriteBool('general', 'HighLightNewManga', miHighlightNewManga.Checked);
  vtMangaList.Repaint;
end;

procedure TMainForm.CheckForTopPanel;
begin

end;

procedure TMainForm.LoadAbout;
var
  fs: TFileStream;
begin
  try
    rmAbout.Clear;
    fs := TFileStream.Create(README_FILE, fmOpenRead or fmShareDenyNone);
    rmAbout.LoadRichText(fs);
    fs.Free;
  except
    on E: Exception do ;
  end;
end;

procedure TMainForm.tvDownloadFilterRepaint;
var
  i: Cardinal;
  LFinishedTasks: Cardinal = 0;
  LInProgressTasks: Cardinal = 0;
  LStoppedTasks: Cardinal = 0;
begin
  if (Assigned(DLManager)) and (DLManager.containers.Count > 0) then
    for i := 0 to DLManager.containers.Count - 1 do
    begin
      case DLManager.containers.Items[i].Status of
        STATUS_FINISH:
          Inc(LFinishedTasks);
        STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT:
          Inc(LInProgressTasks);
        STATUS_STOP:
          Inc(LStoppedTasks);
      end;
    end;

  // root
  tvDownloadFilter.Items[0].Text :=
    Format('%s (%d)', [stAllDownloads, vtDownload.RootNodeCount]);

  // childs
  tvDownloadFilter.Items[1].Text := Format('%s (%d)', [stFinish, LFinishedTasks]);
  tvDownloadFilter.Items[2].Text := Format('%s (%d)', [stInProgress, LInProgressTasks]);
  tvDownloadFilter.Items[3].Text := Format('%s (%d)', [stStop, LStoppedTasks]);

  // root
  tvDownloadFilter.Items[4].Text := stHistory;

  // childs
  tvDownloadFilter.Items[5].Text := stToday;
  tvDownloadFilter.Items[6].Text := stYesterday;
  tvDownloadFilter.Items[7].Text := stOneWeek;
  tvDownloadFilter.Items[8].Text := stOneMonth;
end;

procedure TMainForm.GenerateNodes;
begin
  tvDownloadFilter.Items.Clear;

  // root
  tvDownloadFilter.Items.Add(nil, stAllDownloads);
  tvDownloadFilter.Items[0].ImageIndex := 4;
  tvDownloadFilter.Items[0].SelectedIndex := 4;
  tvDownloadFilter.Items[0].StateIndex := 4;

  // childs
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[0], stFinish);
  tvDownloadFilter.Items[1].ImageIndex := 5;
  tvDownloadFilter.Items[1].SelectedIndex := 5;
  tvDownloadFilter.Items[1].StateIndex := 5;
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[0], stInProgress);
  tvDownloadFilter.Items[2].ImageIndex := 6;
  tvDownloadFilter.Items[2].SelectedIndex := 6;
  tvDownloadFilter.Items[2].StateIndex := 6;
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[0], stStop);
  tvDownloadFilter.Items[3].ImageIndex := 7;
  tvDownloadFilter.Items[3].SelectedIndex := 7;
  tvDownloadFilter.Items[3].StateIndex := 7;

  // root
  tvDownloadFilter.Items.Add(nil, stHistory);
  tvDownloadFilter.Items[4].ImageIndex := 4;
  tvDownloadFilter.Items[4].SelectedIndex := 4;
  tvDownloadFilter.Items[4].StateIndex := 4;

  // childs
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[4], stToday);
  tvDownloadFilter.Items[5].ImageIndex := 8;
  tvDownloadFilter.Items[5].SelectedIndex := 8;
  tvDownloadFilter.Items[5].StateIndex := 8;
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[4], stYesterday);
  tvDownloadFilter.Items[6].ImageIndex := 8;
  tvDownloadFilter.Items[6].SelectedIndex := 8;
  tvDownloadFilter.Items[6].StateIndex := 8;
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[4], stOneWeek);
  tvDownloadFilter.Items[7].ImageIndex := 8;
  tvDownloadFilter.Items[7].SelectedIndex := 8;
  tvDownloadFilter.Items[7].StateIndex := 8;
  tvDownloadFilter.Items.AddChild(tvDownloadFilter.Items[4], stOneMonth);
  tvDownloadFilter.Items[8].ImageIndex := 8;
  tvDownloadFilter.Items[8].SelectedIndex := 8;
  tvDownloadFilter.Items[8].StateIndex := 8;

  tvDownloadFilter.Items[options.ReadInteger('general', 'DownloadFilterSelect',
    0)].Selected := True;
end;

// -----

procedure TMainForm.btDownloadClick(Sender: TObject);
var
  s{, s1}: String;
  i, pos: Integer;
  isCreate: Boolean = False;
  Node: PVirtualNode;
begin
  if mangaInfo.chapterName.Count = 0 then
    Exit;
  Pos := 0;
  Node := clbChapterList.GetFirst;
  for i := 0 to mangaInfo.chapterName.Count - 1 do
  begin
    // if clbChapterList.Checked[i] then
    if Node^.CheckState = csCheckedNormal then
    begin
      if not isCreate then
      begin
        pos := DLManager.AddTask;
        isCreate := True;
      end;
      DLManager.containers.Items[pos].MangaSiteID := GetMangaSiteID(mangaInfo.website);
      // generate folder name
      s := CustomRename(OptionCustomRename,
        mangaInfo.website,
        mangaInfo.title,
        mangaInfo.authors,
        mangaInfo.artists,
        mangaInfo.chapterName.Strings[i],
        Format('%.4d', [i + 1]),
        cbOptionPathConvert.Checked);
      DLManager.containers.Items[pos].ChapterName.Add(s);
      DLManager.containers.Items[pos].ChapterLinks.Add(
        mangaInfo.chapterLinks.Strings[i]);
      ChapterList[i].Downloaded := True;
      clbChapterList.ReinitNode(Node, False);
    end;
    Node := clbChapterList.GetNext(Node);
  end;
  if not isCreate then
    Exit;
  if cbAddAsStopped.Checked then
  begin
    DLManager.containers.Items[pos].DownloadInfo.Status := stStop;
    DLManager.containers.Items[pos].Status := STATUS_STOP;
  end
  else
  begin
    DLManager.containers.Items[pos].DownloadInfo.Status := stWait;
    DLManager.containers.Items[pos].Status := STATUS_WAIT;
  end;
  DLManager.containers.Items[pos].CurrentDownloadChapterPtr := 0;
  DLManager.containers.Items[pos].DownloadInfo.Website := mangaInfo.website;
  DLManager.containers.Items[pos].DownloadInfo.Link := mangaInfo.url;
  DLManager.containers.Items[pos].DownloadInfo.Title := mangaInfo.title;
  DLManager.containers.Items[pos].DownloadInfo.DateTime := Now;

  s := CorrectPathSys(edSaveTo.Text);
  // save to
  if cbOptionGenerateMangaFolderName.Checked then
  begin
    if not cbOptionPathConvert.Checked then
      s := s + RemoveSymbols(mangaInfo.title)
    else
      s := s + RemoveSymbols(UnicodeRemove(mangaInfo.title));
  end;
  s := CorrectPathSys(s);
  DLManager.containers.Items[pos].DownloadInfo.SaveTo := s;
  UpdateVtDownload;

  // DLManager.Backup;
  DLManager.CheckAndActiveTask;
  DLManager.AddToDownloadedChaptersList(
    mangaInfo.website + mangaInfo.link, DLManager.containers.Items[pos].ChapterLinks);
  clbChapterList.Repaint;
  pcMain.PageIndex := 0;
end;

procedure TMainForm.btAddToFavoritesClick(Sender: TObject);
var
  s, s2: String;
  i: Integer;
begin
  if mangaInfo.title <> '' then
  begin
    s := CorrectPathSys(edSaveTo.Text);

    // save to
    if cbOptionGenerateMangaFolderName.Checked then
    begin
      if not cbOptionPathConvert.Checked then
        s := s + RemoveSymbols(mangaInfo.title)
      else
        s := s + RemoveSymbols(UnicodeRemove(mangaInfo.title));
    end;
    s := CorrectPathSys(s);

    s2 := '';
    if (mangaInfo.numChapter > 0) {AND (mangaInfo.website = MANGASTREAM_NAME)} then
    begin
      for i := 0 to mangaInfo.numChapter - 1 do
        s2 := s2 + mangaInfo.chapterLinks.Strings[i] + SEPERATOR;
    end;

    FavoriteManager.Add(mangaInfo.title, IntToStr(mangaInfo.numChapter), s2,
      mangaInfo.website, s, mangaInfo.link);
    vtFavorites.NodeDataSize := SizeOf(TFavoriteInfo);
    UpdateVtFavorites;
    btAddToFavorites.Enabled := False;
  end;
end;

procedure TMainForm.btAbortUpdateListClick(Sender: TObject);
begin
  if isUpdating then
    updateList.Terminate;
end;

procedure TMainForm.btCancelFavoritesCheckClick(Sender: TObject);
begin
  FavoriteManager.StopAll;
end;

procedure TMainForm.appPropertiesMainShowHint(var HintStr: String;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if HintInfo.HintControl = vtMangaList then
    HintInfo.HintMaxWidth := 500;
  if HintInfo.HintControl = sbUpdateList then
    if isUpdating then
      HintStr := Trim(updateList.websites.Text)
    else
      HintStr := '';
end;

// -----

procedure TMainForm.btFavoritesCheckNewChapterClick(Sender: TObject);
begin
  FavoriteManager.isAuto := False;
  FavoriteManager.isShowDialog := cbOptionShowFavoriteDialog.Checked;
  FavoriteManager.Run;
end;

// -----

procedure TMainForm.btBrowseClick(Sender: TObject);
begin
  //dlgSaveTo.InitialDir := CorrectFilePath(edSaveTo.Text);
  dlgSaveTo.InitialDir := edSaveTo.Text;
  if dlgSaveTo.Execute then
    edSaveTo.Text := dlgSaveTo.FileName;
  //edSaveTo.Text := CorrectFilePath(dlgSaveTo.FileName);
end;

procedure TMainForm.btOptionBrowseClick(Sender: TObject);
begin
  //dlgSaveTo.InitialDir := CorrectFilePath(edOptionDefaultPath.Text);
  dlgSaveTo.InitialDir := edOptionDefaultPath.Text;
  if dlgSaveTo.Execute then
    edOptionDefaultPath.Text := CorrectPathSys(dlgSaveTo.FileName);
  //edOptionDefaultPath.Text := CorrectFilePath(dlgSaveTo.FileName);
end;

// -----

procedure TMainForm.btUpdateListClick(Sender: TObject);
var
  button: TControl;
  lowerLeft: TPoint;
  {$IFNDEF SELFUPDATE}
  i: Cardinal;
  {$ENDIF}
begin
  {$IFDEF SELFUPDATE}
  pmUpdate.Items[0].Enabled := True;
  pmUpdate.Items[3].Enabled := True;
  {$ELSE}
  if dataProcess.Title.Count = 0 then
    pmUpdate.Items[0].Enabled := False
  else
    pmUpdate.Items[0].Enabled := True;

  pmUpdate.Items[3].Enabled := True;
  for i := 0 to cbSelectManga.Items.Count - 1 do
  begin
    if not (FileExistsUTF8(DATA_FOLDER + cbSelectManga.Items.Strings[i] + DATA_EXT)) then
    begin
      pmUpdate.Items[3].Enabled := False;
      Break;
    end;
  end;
  {$ENDIF}
  if Sender is TControl then
  begin
    button := TControl(Sender);
    lowerLeft := Point(button.Left, button.Top + button.Height * 2 +
      (button.Height div 2));
    lowerLeft := ClientToScreen(lowerLeft);
    pmUpdate.Popup(lowerLeft.X, lowerLeft.Y);
  end;
end;

procedure TMainForm.DisableAddToFavorites(webs: String);
begin
  if (webs = WebsiteRoots[EHENTAI_ID, 0]) or
    (webs = WebsiteRoots[FAKKU_ID, 0]) or
    (webs = WebsiteRoots[PURURIN_ID, 0])
  //OR (webs=WebsiteRoots[MANGATRADERS_ID, 0])
  then
    btAddToFavorites.Enabled := False
  else
    btAddToFavorites.Enabled := True;
end;

procedure TMainForm.FMDInstanceReceiveMsg(Sender: TObject);
begin
  { TODO 5 -oCholif : Need translation }
  MessageDlg('Free Manga Downloader', 'Free Manga Downloader already running!',
    mtWarning, [mbOK], 0);
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
  BringToFront;
end;

procedure TMainForm.btURLClick(Sender: TObject);
var
  i: Integer;
  webid: Cardinal;
  website,
  webs,
  link: String;
  regx: TRegExpr;
begin
  website := '';
  webs := '';
  link := '';
  regx := TRegExpr.Create;
  try
    regx.Expression := '^https?\://';
    if not (regx.Exec(edURL.Text)) then
      edURL.Text := 'http://' + edURL.Text;

    regx.Expression := '^https?\:(//[^/]*\w+\.\w+)(\:\d+)?(/|\Z)(.*)$';
    if regx.Exec(edURL.Text) then
    begin
      link := regx.Replace(edURL.Text, '$4', True);
      webs := regx.Replace(edURL.Text, '$1', True);
    end;

    if (webs <> '') and (link <> '') then
    begin
      for i := Low(WebsiteRoots) to High(WebsiteRoots) do
        if Pos(webs, WebsiteRoots[i, 1]) > 0 then
        begin
          webid := i;
          website := WebsiteRoots[i, 0];
          Break;
        end;
      if website = '' then
      begin
        webs := TrimLeftChar(webs, ['/']);
        for i := Low(WebsiteRoots) to High(WebsiteRoots) do
        begin
          if Pos(webs, WebsiteRoots[i, 1]) > 0 then
          begin
            webid := i;
            website := WebsiteRoots[i, 0];
            Break;
          end;
        end;
      end;
      if website <> '' then
      begin
        link := '/' + link;
        edURL.Text := FixURL(WebsiteRoots[webid, 1] + link);
        DisableAddToFavorites(website);
      end;
    end;
  finally
    regx.Free;
  end;
  
  if (website = '') or (link = '') then
  begin
    MessageDlg('', stDlgURLNotSupport, mtInformation, [mbYes], 0);
    Exit;
  end;

  if ((Pos(WebsiteRoots[KISSMANGA_ID, 1], edURL.Text) <> 0) or
    (Pos(WebsiteRoots[VNSHARING_ID, 1], edURL.Text) <> 0)) and
    (Pos('&confirm=yes', edURL.Text) <> 0) then
    Delete(link, Pos('&confirm=yes', link), Length('&confirm=yes'))
  else
  if (Pos(WebsiteRoots[ANIMEA_ID, 1], edURL.Text) <> 0) and
    (Pos('?skip=1', edURL.Text) <> 0) then
    Delete(link, Pos('?skip=1', link), Length('?skip=1'));

  if isGetMangaInfos then
  begin
    GetInfosThread.IsFlushed := True;
    GetInfosThread.Terminate;
    //GetInfosThread.WaitFor;
  end;
  GetInfosThread := TGetMangaInfosThread.Create;
  GetInfosThread.MangaListPos := -1;
  GetInfosThread.Title := '';
  GetInfosThread.Website := website;
  GetInfosThread.Link := link;
  GetInfosThread.Start;

  pcMain.TabIndex := 1;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add(RS_Loading);
  clbChapterList.Clear;
  if Assigned(gifWaiting) then
  begin
    itAnimate.Enabled := True;
    pbWait.Visible := True;
  end;

  btAddToFavorites.Enabled := not SitesWithoutFavorites(website);
end;

procedure TMainForm.btVisitMyBlogClick(Sender: TObject);
begin
  OpenURL('http://akarink.wordpress.com/');
end;

procedure TMainForm.cbOptionDigitChapterChange(Sender: TObject);
begin
  seOptionDigitChapter.Enabled := cbOptionDigitChapter.Checked;
end;

procedure TMainForm.cbOptionDigitVolumeChange(Sender: TObject);
begin
  seOptionDigitVolume.Enabled := cbOptionDigitVolume.Checked;
end;

procedure TMainForm.btReadOnlineClick(Sender: TObject);
begin
  OpenURL(mangaInfo.url);
end;

procedure TMainForm.btSearchClearClick(Sender: TObject);
begin
  edSearch.Clear;
end;

procedure TMainForm.btCheckVersionClick(Sender: TObject);
begin
  if subthread.CheckUpdate then
    MessageDlg('', stDlgUpdaterIsRunning, mtInformation, [mbYes], 0)
  else
  begin
    subthread.CheckUpdate := True;
    btCheckVersion.Caption := stFavoritesChecking;
  end;
end;

procedure TMainForm.btDonateClick(Sender: TObject);
begin
  OpenURL('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=akarin.km@gmail.com&item_name=Donation+to+Free+Manga+Downloader');
end;

procedure TMainForm.btFavoritesImportClick(Sender: TObject);
begin
  with TImportFavorites.Create(Self) do try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.btChecksClick(Sender: TObject);
var
  button: TControl;
  lowerLeft: TPoint;
begin
  if dataProcess.Title.Count = 0 then
    pmUpdate.Items[0].Enabled := False
  else
    pmUpdate.Items[0].Enabled := True;
  if Sender is TControl then
  begin
    button := TControl(Sender);
    lowerLeft := Point(spMainSplitter.Left + 5 + button.Left,
      button.Top + button.Height + 295);
    lowerLeft := ClientToScreen(lowerLeft);
    pmChapterList.Popup(lowerLeft.X, lowerLeft.Y);
  end;
  clbChapterList.SetFocus;
end;

procedure TMainForm.cbSelectMangaChange(Sender: TObject);
var
  isFilterAllSites: Boolean;
  K: Word;
begin
  if cbSelectManga.ItemIndex < 0 then
    Exit;

  if currentWebsite <> cbSelectManga.Items.Strings[cbSelectManga.ItemIndex] then
  begin
    Screen.Cursor := crHourGlass;
    if dataProcess.Title.Count > 0 then
    begin
      isFilterAllSites := dataProcess.isFilterAllSites;
      dataProcess.RemoveFilter;
      if not isFilterAllSites then
        dataProcess.SaveToFile;
    end;
    dataProcess.Free;
    dataProcess := TDataProcess.Create;
    if not dataProcess.LoadFromFile(
      cbSelectManga.Items.Strings[cbSelectManga.ItemIndex]) then
    begin
      RunGetList;
    end;
    vtMangaList.OnInitNode := @vtMangaListInitNode;
    vtMangaList.Clear;
    vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
    lbMode.Caption := Format(stModeAll, [dataProcess.filterPos.Count]);
    currentWebsite := cbSelectManga.Items[cbSelectManga.ItemIndex];
    dataProcess.website := cbSelectManga.Items[cbSelectManga.ItemIndex];
    CheckForTopPanel;
    LastSearchStr := '';
    K := VK_RETURN;
    edSearchKeyUp(edSearch, K, []);
    edSearchChange(edSearch);
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PChapterStateItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    if Data^.Downloaded then
    begin
      TargetCanvas.Brush.Color := $B8FFB8;
      TargetCanvas.FillRect(CellRect);
    end;
end;

procedure TMainForm.clbChapterListFreeNode(Sender : TBaseVirtualTree;
  Node : PVirtualNode);
var
  Data: PChapterStateItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TMainForm.clbChapterListGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TChapterStateItem);
end;

procedure TMainForm.clbChapterListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PChapterStateItem;
begin
  Data := clbChapterList.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data^.Title;
end;

procedure TMainForm.clbChapterListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PChapterStateItem;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    if mangaInfo.chapterName.Count = 1 then
      Data^.Title := ChapterList[Node^.Index].Title
    else
      Data^.Title := Format('%.4d - %s', [Node^.Index + 1,
        ChapterList[Node^.Index].Title]);
    Data^.Link := ChapterList[Node^.Index].Link;
    Data^.Downloaded := ChapterList[Node^.Index].Downloaded;
    Node^.CheckType := ctCheckBox;
    clbChapterList.ValidateNode(Node, False);
  end;
end;

procedure TMainForm.edSearchClick(Sender: TObject);
begin
  if edSearch.Text = stSearch then
    edSearch.Text := '';
end;

procedure TMainForm.edURLKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btURLClick(btURL);
end;

// --

// -----

procedure TMainForm.btRemoveFilterClick(Sender: TObject);
begin
  if dataProcess.isFiltered then
  begin
    Screen.Cursor := crHourGlass;
    try
      dataProcess.RemoveFilter;
      if dataProcess.isFilterAllSites then
      begin
        dataProcess.isFilterAllSites := False;
        dataProcess.Free;
        dataProcess := TDataProcess.Create;
        dataProcess.LoadFromFile(cbSelectManga.Items[cbSelectManga.ItemIndex]);
      end;
      vtMangaList.OnInitNode := @vtMangaListInitNode;
      vtMangaList.Clear;
      vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
      lbMode.Caption := Format(stModeAll, [dataProcess.filterPos.Count]);
      edSearch.Text := '';
    except
      on E: Exception do
        ExceptionHandler(Self, E);
    end;
    Screen.Cursor := crDefault;
  end;
end;

// -----

procedure TMainForm.btFilterClick(Sender: TObject);
var
  l, checkGenres, uncheckGenres: TStringList;
  i: Cardinal;
  s: String;
begin
  Screen.Cursor := crHourGlass;
  checkGenres := TStringList.Create;
  uncheckGenres := TStringList.Create;
  try
    if cbUseRegExpr.Checked and (Trim(edCustomGenres.Text) <> '') then
      checkGenres.Add(Trim(edCustomGenres.Text))
    else
    begin
      //CustomGenres(checkGenres, edCustomGenres.Text);
      ExtractStrings([',', ';'], [], PChar(edCustomGenres.Text), checkGenres);
      TrimStrings(checkGenres);
      i := 0;
      while i < checkGenres.Count do
      begin
        s := checkGenres.Strings[i];
        if (s[1] = '-') or (s[1] = '!') then
        begin
          if (s[1] = '-') then
            s := StringReplace(s, '-', '', [])
          else
            s := StringReplace(s, '!', '', []);
          uncheckGenres.Add(s);
          checkGenres.Delete(i);
        end
        else
          Inc(i);
      end;
    end;
    for i := 0 to 37 do
    begin
      if TCheckBox(pnGenres.Controls[i]).State = cbChecked then
        checkGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption)
      else
      if TCheckBox(pnGenres.Controls[i]).State = cbUnchecked then
        uncheckGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption);
    end;

    // we will reload the list if search from all websites is enabled
    if (cbSearchFromAllSites.Checked) and (not dataProcess.isFilterAllSites) and
      (not dataProcess.isFiltered) then
    begin
      if not dataProcess.CanFilter(checkGenres, uncheckGenres,
        edFilterTitle.Text, edFilterAuthors.Text,
        edFilterArtists.Text, IntToStr(cbFilterStatus.ItemIndex),
        edFilterSummary.Text,
        seOptionNewMangaTime.Value,
        rbAll.Checked, cbOnlyNew.Checked) then
      begin
        uncheckGenres.Free;
        checkGenres.Free;
        Exit;
      end;
      l := TStringList.Create;
      for i := 0 to cbSelectManga.Items.Count - 1 do
        l.Add(cbSelectManga.Items[i]);
      dataProcess.Free;
      dataProcess := TDataProcess.Create;
      dataProcess.LoadFromAllFiles(l);
      dataProcess.isFilterAllSites := True;
      l.Free;
    end;

    if dataProcess.Filter(checkGenres, uncheckGenres,
      edFilterTitle.Text, edFilterAuthors.Text,
      edFilterArtists.Text, IntToStr(cbFilterStatus.ItemIndex),
      edFilterSummary.Text,
      seOptionNewMangaTime.Value,
      rbAll.Checked, cbOnlyNew.Checked, cbUseRegExpr.Checked) then
    begin
      lbMode.Caption := Format(stModeFilter, [dataProcess.filterPos.Count]);
      vtMangaList.OnInitNode := @vtMangaListInitNode;
      vtMangaList.Clear;
      vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
  uncheckGenres.Free;
  checkGenres.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.btFilterResetClick(Sender: TObject);
var
  i: Cardinal;
begin
  for i := 0 to 37 do
    TCheckBox(pnGenres.Controls[i]).State := cbGrayed;
  edFilterTitle.Caption := '';
  edFilterAuthors.Caption := '';
  edFilterArtists.Caption := '';
  edFilterSummary.Caption := '';
  cbFilterStatus.ItemIndex := 2;
  edCustomGenres.Caption := '';
end;

// ----- vtMangaList popup menu -----

procedure TMainForm.miMangaListAddToFavoritesClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if vtMangaList.SelectedCount = 0 then
    Exit;
  xNode := vtMangaList.GetFirst;
  for i := 0 to vtMangaList.RootNodeCount - 1 do
  begin
    if vtMangaList.Selected[xNode] then
    begin
      SilentThreadManager.Add(MD_AddToFavorites,
        GetMangaSiteName(DataProcess.site.Items[DataProcess.GetPos(i)]),
        DataProcess.Param[DataProcess.GetPos(i), DATA_PARAM_NAME],
        DataProcess.Param[DataProcess.GetPos(i), DATA_PARAM_LINK]);
    end;
    xNode := vtMangaList.GetNext(xNode);
  end;
end;

// ----- vtFavorites popup menu -----

procedure TMainForm.miFavoritesRemoveClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
  delList: array of Cardinal;
begin
  if (cbOptionShowDeleteTaskDialog.Checked) and (vtFavorites.SelectedCount > 0) then
    if MessageDlg('', stDlgRemoveTask,
      mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
      mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  if vtFavorites.SelectedCount = 1 then
  begin
    if not Assigned(vtFavorites.FocusedNode) then
      Exit;
    FavoriteManager.Remove(vtFavorites.FocusedNode^.Index);
  end
  else
  begin
    xNode := vtFavorites.GetFirst;
    SetLength(delList, 0);
    i := 0;
    while i < FavoriteManager.Count do
    begin
      if vtFavorites.Selected[xNode] then
      begin
        SetLength(delList, Length(delList) + 1);
        delList[Length(delList) - 1] := i;
      end;
      Inc(i);
      xNode := vtFavorites.GetNext(xNode);
    end;

    if Length(delList) > 0 then
      for i := Length(delList) - 1 downto 0 do
        FavoriteManager.Remove(delList[i], False);

    FavoriteManager.Backup;
  end;
  UpdateVtFavorites;
  SetLength(delList, 0);
end;

procedure TMainForm.miFavoritesChangeCurrentChapterClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
      mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  s := FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.currentChapter;
  repeat
    if InputQuery('', stDlgTypeInNewChapter, s) then
  until TryStrToInt(s, i);
  if s <> FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.currentChapter then
  begin
    FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.currentChapter := s;
    UpdateVtFavorites;
    FavoriteManager.Backup;
  end;
end;

procedure TMainForm.miFavoritesChangeSaveToClick(Sender: TObject);
begin
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', stDlgFavoritesIsRunning,
      mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  if InputQuery('', stDlgTypeInNewSavePath,
    FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo) then
  begin
    FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo :=
      CorrectFilePath(FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo);
    UpdateVtFavorites;
    FavoriteManager.Backup;
  end;
end;

// ----- clbChapterList popup menu -----

procedure TMainForm.miChapterListCheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
  Node: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    Node := clbChapterList.GetFirst;
    for i := 0 to clbChapterList.RootNodeCount - 1 do
    begin
      if clbChapterList.Selected[Node] then
        Node^.CheckState := csCheckedNormal;
      clbChapterList.InvalidateNode(Node);
      Node := clbChapterList.GetNext(Node);
    end;
  end;
end;

procedure TMainForm.miChapterListUncheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
  Node: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    Node := clbChapterList.GetFirst;
    for i := 0 to clbChapterList.RootNodeCount - 1 do
    begin
      if clbChapterList.Selected[Node] then
        Node^.CheckState := csUncheckedNormal;
      clbChapterList.InvalidateNode(Node);
      Node := clbChapterList.GetNext(Node);
    end;
  end;
end;

procedure TMainForm.miChapterListCheckAllClick(Sender: TObject);
var
  i: Cardinal;
  Node: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    Node := clbChapterList.GetFirst;
    for i := 0 to clbChapterList.RootNodeCount - 1 do
    begin
      Node^.CheckState := csCheckedNormal;
      clbChapterList.InvalidateNode(Node);
      Node := clbChapterList.GetNext(Node);
    end;
  end;
end;

procedure TMainForm.miChapterListUncheckAllClick(Sender: TObject);
var
  i: Cardinal;
  Node: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    Node := clbChapterList.GetFirst;
    for i := 0 to clbChapterList.RootNodeCount - 1 do
    begin
      Node^.CheckState := csUncheckedNormal;
      clbChapterList.InvalidateNode(Node);
      Node := clbChapterList.GetNext(Node);
    end;
  end;
end;

// ----- vtDownload popup menu -----

procedure TMainForm.miUpClick(Sender: TObject);
begin
  if DLManager.MoveUp(vtDownload.FocusedNode^.Index) then
  begin
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.mnDownload1ClickClick(Sender: TObject);
var
  i: Integer;
begin
  if not isUpdating then
  begin
    if (MessageDlg('', stDlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0) =
      mrYes) then
    begin
      // if dataProcess.Title.Count > 1 then
      //begin
      isUpdating := True;
      updateList := TUpdateMangaManagerThread.Create;
      for i := 0 to cbSelectManga.Items.Count - 1 do
        updateList.websites.Add(cbSelectManga.Items[i]);
      updateList.isDownloadFromServer := True;
      updateList.Start;
      //end;
    end;
  end
  else
    MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0);
end;

procedure TMainForm.mnUpdate1ClickClick(Sender: TObject);
var
  i, j, e: Cardinal;
  f: Boolean;
  st: TStringList;
begin
  if (not isUpdating) then
  begin
    {$IFNDEF SELFUPDATE}
    if dataProcess.Title.Count > 0 then
    {$ENDIF}
    begin
      isUpdating := True;
      updateList := TUpdateMangaManagerThread.Create;
      for i := 0 to cbSelectManga.Items.Count - 1 do
        updateList.websites.Add(cbSelectManga.Items[i]);
      updateList.isDownloadFromServer := False;
      updateList.Start;
    end;
  end
  else
  begin
    e := 0;
    if (updateList.websites.Count > 0) and (cbSelectManga.Items.Count > 0) then
    begin
      st := TStringList.Create;
      st.Assign(updateList.websites);
      for i := 0 to st.Count - 1 do
      begin
        st[i] := Trim(StringReplace(st[i], UTF8Encode(WideString(#$2714)), '', []));
      end;
      for i := 0 to cbSelectManga.Items.Count - 1 do
      begin
        f := False;
        for j := 0 to st.Count - 1 do
          if cbSelectManga.Items[i] = st[j] then
          begin
            Inc(e);
            f := True;
            Break;
          end;
        if not f then
          updateList.websites.Add(cbSelectManga.Items[i]);
      end;
      st.Free;
    end;
    if (e > 0) and (e = cbSelectManga.Items.Count) then
      MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0);
  end;
end;

procedure TMainForm.mnUpdateDownFromServerClick(Sender: TObject);
begin
  if (not isUpdating) then
    RunGetList
  else
    MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0);
end;

procedure TMainForm.mnUpdateListClick(Sender: TObject);
var
  i: Cardinal;
  e: Boolean;
  st: TStringList;
begin
  if (not isUpdating) then
  begin
    {$IFNDEF SELFUPDATE}
    if dataProcess.Title.Count > 0 then
    {$ENDIF}
    begin
      isUpdating := True;
      updateList := TUpdateMangaManagerThread.Create;
      updateList.numberOfThreads := 4;
      updateList.websites.Add(cbSelectManga.Items[cbSelectManga.ItemIndex]);
      updateList.isDownloadFromServer := False;
      updateList.Start;
    end;
  end
  else
  begin
    e := False;
    st := TStringList.Create;
    st.Assign(updateList.websites);
    if st.Count > 0 then
    begin
      for i := 0 to st.Count - 1 do
      begin
        st[i] := Trim(StringReplace(st[i], UTF8Encode(WideString(#$2714)), '', []));
        if st[i] = cbSelectManga.Items[cbSelectManga.ItemIndex] then
          e := True;
      end;
    end;
    st.Free;
    if not e then
      updateList.websites.Add(cbSelectManga.Items[cbSelectManga.ItemIndex])
    else
      MessageDlg('', stDlgUpdateAlreadyRunning, mtInformation, [mbYes], 0);
  end;
end;

procedure TMainForm.miDownClick(Sender: TObject);
begin
  if DLManager.MoveDown(vtDownload.FocusedNode^.Index) then
  begin
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.miDownloadRemoveFinishedTasksClick(Sender: TObject);
begin
  if cbOptionShowDeleteTaskDialog.Checked then
    if not (MessageDlg('', stDlgRemoveFinishTasks,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      Exit;
  DLManager.RemoveAllFinishedTasks;
  UpdateVtDownload;
  DLManager.Backup;
  // the reason we put it in here instead of in DLManager because of the size of
  // download list will change during this method
end;

procedure TMainForm.miDownloadResumeClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  // if NOT Assigned(vtDownload.FocusedNode) then exit;

  // print waiting string to the screen

  if (vtDownload.SelectedCount = 1) and (Assigned(vtDownload.FocusedNode)) then
  begin
    if DLManager.containers.Items[vtDownload.FocusedNode^.Index].Status in
      [STATUS_STOP, STATUS_PROBLEM, STATUS_FAILED] then
    begin
      DLManager.containers.Items[vtDownload.FocusedNode^.Index].Status := STATUS_WAIT;
      DLManager.containers.Items[vtDownload.FocusedNode^.Index].DownloadInfo.Status :=
        stWait;
      if DLManager.CanActiveTask(vtDownload.FocusedNode^.Index) then
        DLManager.ActiveTask(vtDownload.FocusedNode^.Index);
      vtDownload.Repaint;
      DLManager.Backup;
    end;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode := vtDownload.GetFirst;
    for i := 0 to vtDownload.RootNodeCount - 1 do
    begin
      if vtDownload.Selected[xNode] then
      begin
        if DLManager.containers.Items[i].Status in
          [STATUS_STOP, STATUS_PROBLEM, STATUS_FAILED] then
        begin
          DLManager.containers.Items[i].Status := STATUS_WAIT;
          DLManager.containers.Items[i].DownloadInfo.Status := stWait;
          if DLManager.CanActiveTask(i) then
            DLManager.ActiveTask(i);
        end;
      end;
      xNode := vtDownload.GetNext(xNode);
    end;
    vtDownload.Repaint;
    DLManager.Backup;
  end;
end;

procedure TMainForm.miDownloadRemoveClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  // if NOT Assigned(vtDownload.FocusedNode) then exit;
  if (cbOptionShowDeleteTaskDialog.Checked) and
    (vtDownload.SelectedCount > 0) then
    if MessageDlg('', stDlgRemoveTask,
      mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;

  if (vtDownload.SelectedCount = 1) and (Assigned(vtDownload.FocusedNode)) then
  begin
    DLManager.RemoveTask(vtDownload.FocusedNode^.Index);
    UpdateVtDownload;
    DLManager.Backup;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode := vtDownload.GetFirst;
    i := 0;
    while i < DLManager.containers.Count do
    begin
      if vtDownload.Selected[xNode] then
        DLManager.RemoveTask(i)
      else
        Inc(i);
      xNode := vtDownload.GetNext(xNode);
    end;
    UpdateVtDownload;
    DLManager.Backup;
  end;
end;

// Download table's popup menu
procedure TMainForm.miDownloadStopClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  //if NOT Assigned(vtDownload.FocusedNode) then exit;
  if (vtDownload.SelectedCount = 1) and (Assigned(vtDownload.FocusedNode)) then
  begin
    DLManager.StopTask(vtDownload.FocusedNode^.Index);
    vtDownload.Repaint;
  end
  else
  begin
    xNode := vtDownload.GetFirst;
    for i := 0 to vtDownload.RootNodeCount - 1 do
    begin
      if vtDownload.Selected[xNode] then
        DLManager.StopTask(xNode^.Index, False);
      xNode := vtDownload.GetNext(xNode);
    end;
    DLManager.Backup;
    DLManager.CheckAndActiveTask;
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.miMangaListDownloadAllClick(Sender: TObject);
var
  xNode: PVirtualNode;
  AllowedToCreate, YesAll, NoAll : Boolean;
  i, j: Integer;
  mResult: TModalResult;
  mBtns: TMsgDlgButtons;
begin
  if vtMangaList.SelectedCount = 0 then
    Exit;
  try
    YesAll := False;
    NoAll := False;
    if vtMangaList.SelectedCount = 1 then
      mBtns := [mbYes, mbNo]
    else
      mBtns := [mbYes, mbNo, mbYesToAll, mbNoToAll];

    xNode := vtMangaList.GetFirst;
    for i := 0 to vtMangaList.RootNodeCount - 1 do
    begin
      if vtMangaList.Selected[xNode] then
      begin
        AllowedToCreate := True;
        if DLManager.containers.Count > 0 then
        begin
          for j := 0 to DLManager.containers.Count - 1 do
          begin
            if dataProcess.Param[dataProcess.GetPos(i), DATA_PARAM_NAME] =
              DLManager.containers.Items[j].DownloadInfo.title then
            begin
              if YesAll then
                AllowedToCreate := True
              else if NoAll then
                AllowedToCreate := False
              else
              begin
                pcMain.ActivePage := tsDownload;
                mResult := MessageDlg('', DLManager.containers.Items[j].DownloadInfo.title +
                  LineEnding + LineEnding + RS_DlgTitleExistInDLlist, mtConfirmation,
                    mBtns, 0);
                case mResult of
                  mrYes : AllowedToCreate := True;
                  mrNo  : AllowedToCreate := False;
                  mrYesToAll :
                    begin
                      YesAll := True;
                      NoAll := False;
                      AllowedToCreate := True;
                    end;
                  mrNoToAll :
                    begin
                      YesAll := False;
                      NoAll := True;
                      AllowedToCreate := False;
                    end;
                end
              end;;
              Break;
            end;
          end;
        end;

        if AllowedToCreate then
          SilentThreadManager.Add(MD_DownloadAll,
            GetMangaSiteName(DataProcess.site.Items[DataProcess.GetPos(i)]),
            dataProcess.Param[DataProcess.GetPos(i), DATA_PARAM_NAME],
            dataProcess.Param[DataProcess.GetPos(i), DATA_PARAM_LINK]);
      end;
      xNode := vtMangaList.GetNext(xNode);
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
end;

procedure TMainForm.miMangaListViewInfosClick(Sender: TObject);
var
  title, website, link: String;
  i: Integer;
begin
  if (not vtMangaList.Focused) or (vtMangaList.SelectedCount = 0) then
    Exit;
  btDownload.Enabled := False;
  pcMain.TabIndex := 1;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add('Loading ...');
  clbChapterList.Clear;

  if isGetMangaInfos then
  begin
    GetInfosThread.IsFlushed := True;
    GetInfosThread.Terminate;
    //GetInfosThread.WaitFor;
  end;
  GetInfosThread := TGetMangaInfosThread.Create;
  GetInfosThread.MangaListPos := vtMangaList.FocusedNode^.Index;
  if DataProcess.searchPos.Count = 0 then
  begin
    website := GetMangaSiteName(
      DataProcess.site.Items[DataProcess.GetPos(GetInfosThread.mangaListPos)]);
    //cbSelectManga.Items[cbSelectManga.ItemIndex];
    title := DataProcess.Param[DataProcess.GetPos(GetInfosThread.mangaListPos),
      DATA_PARAM_NAME];
    link := DataProcess.Param[DataProcess.GetPos(GetInfosThread.mangaListPos),
      DATA_PARAM_LINK];
  end
  else
  begin
    website := GetMangaSiteName(
      DataProcess.site.Items[DataProcess.searchPos.Items[GetInfosThread.mangaListPos]]);
    //cbSelectManga.Items[cbSelectManga.ItemIndex];
    title := DataProcess.Param[DataProcess.searchPos.Items[GetInfosThread.mangaListPos],
      DATA_PARAM_NAME];
    link := DataProcess.Param[DataProcess.searchPos.Items[GetInfosThread.mangaListPos],
      DATA_PARAM_LINK];
  end;
  GetInfosThread.Title := title;
  GetInfosThread.Website := website;
  GetInfosThread.Link := link;
  GetInfosThread.Start;

  //ShowInformation;
  for i := 0 to High(WebsiteRoots) do
    if Pos(website, WebsiteRoots[i, 0]) > 0 then
    begin
      link := StringReplace(link, WebsiteRoots[i, 1], '', []);
      edURL.Text := FixURL(FillMangaSiteHost(i, link));
      Break;
    end;

  if Assigned(gifWaiting) then
  begin
    itAnimate.Enabled := True;
    pbWait.Visible := True;
  end;

  btReadOnline.Enabled := (link <> '');
  btAddToFavorites.Enabled := not SitesWithoutFavorites(website);

  //check if manga already in FavoriteManager list
  if btAddToFavorites.Enabled and (Sender = vtMangaList) and
    (FavoriteManager.Favorites.Count > 0) then
    btAddToFavorites.Enabled := not FavoriteManager.IsMangaExist(title, website);
end;

procedure TMainForm.miOpenFolder2Click(Sender: TObject);
var
  Process: TProcessUTF8;
begin
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  Process := TProcessUTF8.Create(nil);
  {$IFDEF WINDOWS}
  Process.CommandLine := 'explorer.exe "' +
    StringReplace(FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo,
    '/', '\', [rfReplaceAll]) + '"';
  {$ENDIF}
  {$IFDEF UNIX}
  Process.CommandLine := 'xdg-open "' +
    StringReplace(Favorites.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo,
    '/', '\', [rfReplaceAll]) + '"';
  {$ENDIF}
  Process.Execute;
  Process.Free;
end;

procedure TMainForm.miOpenFolderClick(Sender: TObject);
var
  Process: TProcessUTF8;
begin
  if (vtDownload.SelectedCount = 0) or (Assigned(vtDownload.FocusedNode) = False) then
    Exit;
  Process := TProcessUTF8.Create(nil);
  try
    {$IFDEF WINDOWS}
    Process.CommandLine := 'explorer.exe "' +
      StringReplace(DLManager.containers.Items[
      vtDownload.FocusedNode^.Index].DownloadInfo.SaveTo, '/', '\', [rfReplaceAll]) + '"';
    {$ENDIF}
    {$IFDEF UNIX}
    Process.CommandLine := 'xdg-open "' +
      StringReplace(DLManager.containers.Items[
      vtDownload.FocusedNode^.Index].downloadInfo.SaveTo, '/', '\', [rfReplaceAll]) + '"';
    {$ENDIF}
    Process.Execute;
  finally
    Process.Free;
  end;
end;

procedure TMainForm.miOpenWith2Click(Sender: TObject);
var
  Process: TProcessUTF8;
  f, s: String;
  Info: TSearchRec;
  l: TStringList;
begin
  if (not Assigned(vtDownload.FocusedNode)) or (edOptionExternal.Text = '') then
    Exit;
  l := TStringList.Create;
  Process := TProcessUTF8.Create(nil);
  try
    s := StringReplace(FavoriteManager.Favorites[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo,
      '/', '\', [rfReplaceAll]);
    if s[Length(s)] <> DirectorySeparator then
      s := s + DirectorySeparator;

    if FindFirstUTF8(s + '*', faAnyFile and faDirectory, Info) = 0 then
      repeat
        l.Add(Info.Name);
      until FindNextUTF8(Info) <> 0;
    if l.Count >= 3 then
      f := l.Strings[2]
    else
      f := '';
    FindClose(Info);

    s := StringReplace(edOptionExternal.Text, '%PATH%', s, [rfReplaceAll]);
    s := StringReplace(s, '%FCHAPTER%', f, [rfReplaceAll]);
    Process.CommandLine := s;
    Process.Execute;
  except
  end;
  l.Free;
  Process.Free;
end;

procedure TMainForm.miOpenWithClick(Sender: TObject);
var
  Process: TProcessUTF8;
  f, ff, s: String;
  Info: TSearchRec;
  l: TStringList;
begin
  if (not Assigned(vtDownload.FocusedNode)) or (edOptionExternal.Text = '') then
    Exit;
  l := TStringList.Create;
  Process := TProcessUTF8.Create(nil);
  try
    s := StringReplace(DLManager.containers.Items[
      vtDownload.FocusedNode^.Index].DownloadInfo.SaveTo, '/', '\', [rfReplaceAll]);
    if s[Length(s)] <> DirectorySeparator then
      s := s + DirectorySeparator;

    if DLManager.containers.Items[vtDownload.FocusedNode^.Index].ChapterName.Count > 0 then
    begin
      ff := DLManager.containers.Items[vtDownload.FocusedNode^.Index].
        ChapterName[0];
      if FileExistsUTF8(s + ff + '.zip') then
        f := ff + '.zip'
      else if FileExistsUTF8(s + ff + '.cbz') then
        f := ff + '.cbz'
      else if FileExistsUTF8(s + ff + '.pdf') then
        f := ff + '.pdf'
      else if DirectoryExistsUTF8(s + ff) then
        f := ff
      else
        f := '';
    end;

    if f = '' then
    begin
      if FindFirstUTF8(s + '*', faAnyFile and faDirectory, Info) = 0 then
        repeat
          l.Add(Info.Name);
        until FindNextUTF8(Info) <> 0;
      if l.Count >= 3 then
        f := l.Strings[2]
      else
        f := '';
      FindClose(Info);
    end;

    s := StringReplace(edOptionExternal.Text, '%PATH%', s, [rfReplaceAll]);
    s := StringReplace(s, '%FCHAPTER%', f, [rfReplaceAll]);
    Process.CommandLine := s;
    Process.Execute;
  except
  end;
  l.Free;
  Process.Free;
end;

procedure TMainForm.pcMainChange(Sender: TObject);

  procedure UpdateOptions;
  var
    l: TStringList;
    s: String;
    i, j: Cardinal;
    Data: PMangaListItem;
  begin
    l := TStringList.Create;

    cbOptionMinimizeToTray.Checked :=
      options.ReadBool('general', 'MinimizeToTray', False);
    seOptionNewMangaTime.Value := options.ReadInteger('general', 'NewMangaTime', 3);
    cbOptionLetFMDDo.ItemIndex := options.ReadInteger('general', 'LetFMDDo', 0);
    cbOptionEnableLoadCover.Checked :=
      options.ReadBool('general', 'LoadMangaCover', True);
    // cbOptionLetFMDDoItemIndex:= cbOptionLetFMDDo.ItemIndex;
    cbOptionLetFMDDoItemIndex := cbOptionLetFMDDo.ItemIndex;
    cbOptionBatotoUseIE.Checked := options.ReadBool('general', 'BatotoUseIE', True);
    edOptionExternal.Text := options.ReadString('general', 'ExternalProgram', '');
    cbOptionBatotoUseIE.Checked := False;
    OptionBatotoUseIEChecked := cbOptionBatotoUseIE.Checked;

    seOptionMaxParallel.Value := options.ReadInteger('connections', 'NumberOfTasks', 1);
    seOptionMaxThread.Value :=
      options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
    seOptionMaxRetry.Value := options.ReadInteger('connections', 'Retry', 3);
    cbOptionUseProxy.Checked := options.ReadBool('connections', 'UseProxy', False);
    cbOptionProxyType.Text := options.ReadString('connections', 'ProxyType', 'HTTP');
    edOptionHost.Text := options.ReadString('connections', 'Host', '');
    edOptionPass.Text := options.ReadString('connections', 'Pass', '');
    edOptionPort.Text := options.ReadString('connections', 'Port', '');
    edOptionUser.Text := options.ReadString('connections', 'User', '');
    edOptionDefaultPath.Text := options.ReadString('saveto', 'SaveTo', DEFAULT_PATH);
    if Trim(edOptionDefaultPath.Text) = '' then
      edOptionDefaultPath.Text := DEFAULT_PATH;
    edOptionDefaultPath.Text := CorrectPathSys(edOptionDefaultPath.Text);
    rgOptionCompress.ItemIndex := options.ReadInteger('saveto', 'Compress', 0);

    edOptionCustomRename.Text :=
      options.ReadString('saveto', 'CustomRename', DEFAULT_CUSTOM_RENAME);

    cbOptionShowQuitDialog.Checked :=
      options.ReadBool('dialogs', 'ShowQuitDialog', True);
    cbOptionShowDeleteTaskDialog.Checked :=
      options.ReadBool('dialogs', 'ShowDeleteDldTaskDialog', True);
    cbOptionShowFavoriteDialog.Checked :=
      options.ReadBool('dialogs', 'ShowFavoritesDialog', True);

    cbOptionPathConvert.Checked := options.ReadBool('saveto', 'PathConv', False);
    cbOptionGenerateChapterName.Checked :=
      options.ReadBool('saveto', 'GenChapName', False);
    cbOptionGenerateMangaFolderName.Checked :=
      options.ReadBool('saveto', 'GenMangaName', True);
    cbOptionAutoNumberChapter.Checked :=
      options.ReadBool('saveto', 'AutoNumberChapter', True);
    OptionAutoNumberChapterChecked := cbOptionAutoNumberChapter.Checked;
    seOptionPDFQuality.Value := options.ReadInteger('saveto', 'PDFQuality', 95);

    cbOptionAutoRemoveCompletedManga.Checked :=
      options.ReadBool('update', 'AutoRemoveCompletedManga', True);
    cbOptionAutoCheckFavStartup.Checked :=
      options.ReadBool('update', 'AutoCheckFavStartup', False);
    seOptionCheckMinutes.Value := options.ReadInteger('update', 'AutoCheckMinutes', 0);

    cbOptionShowBatotoSG.Checked := OptionShowBatotoSG;
    cbOptionShowAllLang.Checked := OptionShowAllLang;
    cbOptionAutoDlFav.Checked := OptionAutoDlFav;

    if Length(optionMangaSiteSelectionNodes) > 0 then
      for i := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
        optionMangaSiteSelectionNodes[i]^.CheckState := csUncheckedNormal;

    s := options.ReadString('general', 'MangaListSelect',
      mangalistIni.ReadString('general', 'DefaultSelect', DEFAULT_LIST));
    if Pos(SEPERATOR, S) > 0 then
      GetParams(l, s)    //for old config
    else
      ExtractStrings([','], [], PChar(s), l);

    if l.Count > 0 then
      for i := 0 to l.Count - 1 do
      begin
        if Length(optionMangaSiteSelectionNodes) > 0 then
          for j := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
          begin
            Data := vtOptionMangaSiteSelection.GetNodeData(
              optionMangaSiteSelectionNodes[j]);
            if Data^.Text = l.Strings[i] then
            begin
              optionMangaSiteSelectionNodes[j]^.CheckState := csCheckedNormal;
              Break;
            end;
          end;
      end;

    l.Free;
  end;

begin
  case pcMain.TabIndex of
    5:// load rtf file
      LoadAbout;
  end;
  UpdateOptions;
end;

procedure TMainForm.pmDownloadPopup(Sender: TObject);
begin
  if vtDownload.SelectedCount = 0 then
  begin
    pmDownload.Items[0].Enabled := False;
    pmDownload.Items[1].Enabled := False;
    pmDownload.Items[3].Enabled := False;
    pmDownload.Items[4].Enabled := False;
    pmDownload.Items[5].Enabled := False;
    pmDownload.Items[10].Enabled := False;
    pmDownload.Items[11].Enabled := False;
    miDLViewMangaInfo.Enabled := False;

    pmDownload.Items[5].Items[0].Enabled := False;
    pmDownload.Items[5].Items[1].Enabled := False;
  end
  else
  if vtDownload.SelectedCount = 1 then
  begin
    pmDownload.Items[0].Enabled := True;
    pmDownload.Items[1].Enabled := True;
    pmDownload.Items[3].Enabled := True;
    pmDownload.Items[4].Enabled := True;
    pmDownload.Items[5].Enabled := True;
    pmDownload.Items[10].Enabled := True;
    pmDownload.Items[11].Enabled := True;
    if vtDownload.Focused then
      miDLViewMangaInfo.Enabled := DLManager.containers[vtDownload.FocusedNode^.Index].DownloadInfo.Link <> '';

    pmDownload.Items[5].Items[0].Enabled := True;
    pmDownload.Items[5].Items[1].Enabled := True;
  end
  else
  begin
    pmDownload.Items[0].Enabled := False;
    pmDownload.Items[1].Enabled := False;
    pmDownload.Items[3].Enabled := True;
    pmDownload.Items[4].Enabled := True;
    pmDownload.Items[5].Enabled := True;
    pmDownload.Items[10].Enabled := False;
    pmDownload.Items[11].Enabled := False;
    miDLViewMangaInfo.Enabled := False;

    pmDownload.Items[5].Items[0].Enabled := True;
    pmDownload.Items[5].Items[1].Enabled := True;
  end;
end;

procedure TMainForm.pmEditURLPopup(Sender: TObject);
begin
  medURLUndo.Enabled := edURL.CanUndo;
  medURLCut.Enabled := (Length(edURL.SelText) <> 0);
  medURLCopy.Enabled := (Length(edURL.SelText) <> 0);
  medURLPaste.Enabled := (Length(Clipboard.AsText) <> 0);
  medURLPasteandgo.Enabled := (Length(Clipboard.AsText) <> 0);
  medtURLDelete.Enabled := (Length(edURL.SelText) <> 0);
  medURLSelectAll.Enabled := (Length(edURL.Text) <> 0) and
    (edURL.SelLength <> Length(edURL.Text));
end;

procedure TMainForm.pmFavoritesPopup(Sender: TObject);
begin
  if FavoriteManager.isRunning then
  begin
    pmFavorites.Items[2].Enabled := True;
    pmFavorites.Items[2].Enabled := False;
    pmFavorites.Items[3].Enabled := False;
    pmFavorites.Items[4].Enabled := False;
    Exit;
  end;
  if vtFavorites.SelectedCount = 0 then
  begin
    pmFavorites.Items[0].Enabled := False;
    pmFavorites.Items[2].Enabled := False;
    pmFavorites.Items[3].Enabled := False;
    pmFavorites.Items[4].Enabled := False;
    pmFavorites.Items[6].Enabled := False;
    pmFavorites.Items[7].Enabled := False;
  end
  else
  if vtFavorites.SelectedCount = 1 then
  begin
    pmFavorites.Items[0].Enabled := True;
    pmFavorites.Items[2].Enabled := True;
    pmFavorites.Items[3].Enabled := True;
    pmFavorites.Items[4].Enabled := True;
    {$IFDEF WINDOWS}
    pmFavorites.Items[6].Enabled := True;
    pmFavorites.Items[7].Enabled := True;
    {$ELSE}
    pmFavorites.Items[4].Enabled := False;
    {$ENDIF}
  end
  else
  begin
    pmFavorites.Items[0].Enabled := False;
    pmFavorites.Items[2].Enabled := True;
    pmFavorites.Items[3].Enabled := False;
    pmFavorites.Items[4].Enabled := False;
    pmFavorites.Items[6].Enabled := False;
    pmFavorites.Items[7].Enabled := False;
  end;
end;

procedure TMainForm.pmMangaListPopup(Sender: TObject);
begin
  if vtMangaList.SelectedCount = 1 then
  begin
    pmMangaList.Items[0].Enabled := True;
    pmMangaList.Items[1].Enabled := True;
    pmMangaList.Items[2].Enabled := True;
  end
  else
  if vtMangaList.SelectedCount > 1 then
  begin
    pmMangaList.Items[0].Enabled := False;
    pmMangaList.Items[1].Enabled := True;
    pmMangaList.Items[2].Enabled := True;
  end;
  pmMangaList.Items[2].Enabled := not SitesWithoutFavorites(cbSelectManga.Text);
end;

procedure TMainForm.sbUpdateListDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  ClRect, TxtRect, BarRect, ProgressBarRect: TRect;
  Percents: double;
  tStyle: TTextStyle;
begin
  if Panel.Index = 0 then
  begin
    //Button
    with btAbortUpdateList do
    begin
      Left := Rect.Right - Width - 3;
      Top := Rect.Top + 2;
    end;

    //Information
    if ulTotalPtr = 0 then
      ulTotalPtr := 100;
    if ulWorkPtr > ulTotalPtr then
      ulWorkPtr := 0;
    Percents := ulWorkPtr / ulTotalPtr;
    with StatusBar.Canvas do
    begin
      ClRect := Rect;
      ClRect.Left := Rect.Left + 3;
      ClRect.Right := Rect.Right - btAbortUpdateList.Width - 6;
      ClRect.Bottom := Rect.Bottom - 3;

      TxtRect := ClRect;
      TxtRect.Bottom := TxtRect.Top + TextHeight('A');
      //progress-bar box
      BarRect := ClRect;
      BarRect.Top := TxtRect.Bottom + 2;

      Pen.Style := psSolid;
      Brush.Style := bsSolid;
      Pen.Color := RGB(188, 188, 188);
      Brush.Color := RGB(230, 230, 230);
      Rectangle(BarRect);

      ProgressBarRect := BarRect;
      ProgressBarRect.Right := round((ProgressBarRect.Right - ProgressBarRect.Left) *
        Percents) + ProgressBarRect.Left;

      if (ProgressBarRect.Right - ProgressBarRect.Left) > 0 then
      begin
        //green
        Pen.Color := RGB(6, 176, 37);
        Brush.Color := RGB(50, 217, 66);
        //orange
        //Pen.Color := RGB(153, 79, 0);
        //Brush.Color := RGB(233, 112, 24);
        Rectangle(ProgressBarRect);
      end;
      //TTextStyle get messed up if all record not assigned?
      with tStyle do
      begin
        Alignment := taLeftJustify;
        Layout := tlCenter;
        SingleLine := True;
        Clipping := False;
        ExpandTabs := False;
        ShowPrefix := False;
        Wordbreak := False;
        Opaque := True;
        SystemFont := False;
        RightToLeft := False;
        EndEllipsis := True;
      end;
      Brush.Style := bsClear;
      TextRect(txtRect, 5, 0, Panel.Text, tStyle);
    end;
  end;
end;

procedure TMainForm.seOptionCheckMinutesChange(Sender: TObject);
begin
  lbOptionAutoCheckMinutes.Caption :=
    Format(OptionAutoCheckMinutes, [seOptionCheckMinutes.Value]);
end;

procedure TMainForm.spMainSplitterMoved(Sender: TObject);
begin
  sbMain.Panels[0].Width := spMainSplitter.Left;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  if (WindowState = wsMinimized) or (Visible = False) then
  begin
    WindowState := PrevWindowState;
    ShowInTaskBar := stDefault;
    Show;
  end
  else
    Application.BringToFront;
end;

procedure TMainForm.tvDownloadFilterSelectionChanged(Sender: TObject);
begin
  vtDownloadFilters;
  pcMain.PageIndex := 0;
  options.WriteInteger('general', 'DownloadFilterSelect',
    tvDownloadFilter.Selected.AbsoluteIndex);
end;

procedure TMainForm.UniqueInstanceFMDOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
  BringToFront;
end;

procedure TMainForm.vtDownloadAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  Data: PDownloadInfo;
  BarRect, ProgressBarRect: TRect;
  Percents: double;
  ww, hh: Integer;
begin
  if Node = nil then
    Exit;
  if Column = 2 then
  begin
    Data := vtDownload.GetNodeData(Node);
    //if Data^.Status = stFinish then
    if DLManager.containers.Items[Node^.Index].Status in
      [STATUS_FINISH, STATUS_COMPRESS, STATUS_FAILED] then
      Percents := 1
    else
    if StrToIntDef(Trim(ExtractWord(2, Data^.Progress, ['/'])), 100) = 0 then
      Percents := 0
    else
      Percents := StrToIntDef(Trim(ExtractWord(1, Data^.Progress, ['/'])), 0) /
        StrToIntDef(Trim(ExtractWord(2, Data^.Progress, ['/'])), 100);
    //progress-bar box
    BarRect.Left := CellRect.Left + 2;
    BarRect.Top := CellRect.Top + 2;
    BarRect.Right := CellRect.Right - 2;
    BarRect.Bottom := CellRect.Bottom - 2;
    TargetCanvas.Pen.Style := psSolid;
    TargetCanvas.Brush.Style := bsSolid;

    TargetCanvas.Pen.Color := RGB(188, 188, 188);
    TargetCanvas.Brush.Color := RGB(230, 230, 230);

    TargetCanvas.Rectangle(BarRect);
    //TargetCanvas.RoundRect(BarRect, 6,6);

    // a progress-bar
    ProgressBarRect := BarRect;
    //Inc(ProgressBarRect.Left);
    //Inc(ProgressBarRect.Top);
    //Dec(ProgressBarRect.Right);
    //Dec(ProgressBarRect.Bottom);
    ProgressBarRect.Right := round((ProgressBarRect.Right - ProgressBarRect.Left) *
      Percents) + ProgressBarRect.Left;
    if (ProgressBarRect.Right - ProgressBarRect.Left) > 0 then
    begin
      //TargetCanvas.Pen.Style:= psClear;

      case DLManager.containers.Items[Node^.Index].Status of
        //(STATUS_STOP, STATUS_WAIT, STATUS_PREPARE,
        //STATUS_DOWNLOAD, STATUS_FINISH, STATUS_COMPRESS, STATUS_PROBLEM, STATUS_FAILED);
        STATUS_STOP, STATUS_FAILED:
        begin
          //Red
          TargetCanvas.Pen.Color := RGB(177, 26, 26);
          TargetCanvas.Brush.Color := RGB(240, 74, 74);
        end;
        STATUS_WAIT:
        begin
          //gray
          TargetCanvas.Pen.Color := RGB(188, 188, 188);
          TargetCanvas.Brush.Color := RGB(230, 230, 230);
        end;
        STATUS_DOWNLOAD:
        begin
          //blue
          TargetCanvas.Pen.Color := RGB(29, 107, 179);
          TargetCanvas.Brush.Color := RGB(79, 178, 250);
          //blue light
          //TargetCanvas.Pen.Color:= RGB(124,178,234);
          //TargetCanvas.Brush.Color:= RGB(163,210,254);
        end;
        STATUS_PROBLEM:
        begin
          //yellow
          //TargetCanvas.Pen.Color := RGB(195, 145, 79);
          TargetCanvas.Pen.Color := RGB(240, 74, 74);
          TargetCanvas.Brush.Color := RGB(254, 235, 128);
        end;
        STATUS_FINISH:
        begin
          //green
          TargetCanvas.Pen.Color := RGB(6, 176, 37);
          TargetCanvas.Brush.Color := RGB(50, 217, 66);
        end;
        else
        begin
          //browngold
          TargetCanvas.Pen.Color := RGB(200, 162, 94);
          TargetCanvas.Brush.Color := RGB(240, 213, 141);
        end;
      end;
      //TargetCanvas.RoundRect(ProgressBarRect, 5, 5);
      TargetCanvas.Rectangle(ProgressBarRect);
      //TargetCanvas.FillRect(ProgressBarRect);
    end;
    //text
    TargetCanvas.Font.Color := clBlack;
    TargetCanvas.Brush.Style := bsClear;
    TargetCanvas.GetTextSize(Data^.Progress, ww, hh);
    TargetCanvas.TextOut(CellRect.Left + ((CellRect.Right - CellRect.Left - ww) div 2),
      CellRect.Top + ((CellRect.Bottom - CellRect.Top - hh) div 2), Data^.Progress);
  end;
end;

procedure TMainForm.vtDownloadColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  miOpenFolderClick(Sender);
end;

procedure TMainForm.vtDownloadDragAllowed(Sender : TBaseVirtualTree;
  Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
begin
  Allowed := True;
end;

procedure TMainForm.vtDownloadMoveItems(NextIndex: Cardinal; Mode: TDropMode);
var
  i, nIndex: Integer;
  cNode: PVirtualNode;
  ConTemp: TTaskThreadContainerList;
begin
  { TODO -oCholif -cm : Dirty method for drag and drop to rearrange list, need better approach.
    Check memory and cpu consumption }
  vtDownload.BeginUpdate;
  ConTemp := TTaskThreadContainerList.Create;
  try
    nIndex := NextIndex;

    if vtDownload.SelectedCount > 0 then
    begin
      cNode := vtDownload.GetFirst;
      i := 0;
      while i < vtDownload.RootNodeCount do
               //DLManager.containers.Count do
      begin
        if vtDownload.Selected[cNode] then
        begin
          vtDownload.Selected[cNode] := False;
          ConTemp.Add(DLManager.containers[i]);
          DLManager.containers.Delete(i);
          if (i < nIndex) and (nIndex > 0) then
            Dec(nIndex);
        end
        else
          Inc(i);
        cNode := vtDownload.GetNext(cNode);
      end;
      vtDownload.FocusedNode := nil;

      for i := 0 to ConTemp.Count - 1 do
      begin
        if (i = 0) and (Mode in [dmBelow, dmNowhere]) then
          Inc(nIndex)
        else
        if (i > 0) then
        begin
          if (nIndex < DLManager.containers.Count) then
            Inc(nIndex);
        end;
        if nIndex > DLManager.containers.Count then
          Dec(nIndex);
        DLManager.containers.Insert(nIndex, ConTemp[i]);
      end;

      cNode := vtDownload.GetFirst;
      while Assigned(cNode) and (cNode^.Index < nIndex) do
        cNode := vtDownload.GetNext(cNode);

      for i := 0 to ConTemp.Count - 1 do
      begin
        if Assigned(cNode) then
        begin
          vtDownload.Selected[cNode] := True;
          vtDownload.FocusedNode := cNode;
          cNode := vtDownload.GetPrevious(cNode);
        end;
      end;
    end;
  finally
    ConTemp.Free;
    cNode := nil;
    vtDownload.EndUpdate;
    //vtDownload.Repaint;
    //Some node isn't repaint correctly if not explicitly triggering repaint?
  end;
  vtDownloadFilters;
end;

procedure TMainForm.vtDownloadDragDrop(Sender : TBaseVirtualTree;
  Source : TObject; DataObject : IDataObject; Formats : TFormatArray;
  Shift : TShiftState; const Pt : TPoint; var Effect : LongWord;
  Mode : TDropMode);
begin
  if (Source <> vtDownload) or (Source <> Sender) or
    (DLManager.containers.Count < 2) then
    Exit;
  if Mode = dmNowhere then
    vtDownloadMoveItems(vtDownload.GetLast^.Index, Mode)
  else
    vtDownloadMoveItems(vtDownload.DropTargetNode^.Index, Mode);
end;

procedure TMainForm.vtDownloadDragOver(Sender : TBaseVirtualTree;
  Source : TObject; Shift : TShiftState; State : TDragState; const Pt : TPoint;
  Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
begin
  Accept := (Sender = Source);
end;

// Download table

procedure TMainForm.vtDownloadFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PDownloadInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TMainForm.vtDownloadGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  l, i: Cardinal;
  p: PDownloadInfo;
begin
  if Column = 0 then
  begin
    l := DLManager.containers.Items[Node^.Index].ChapterLinks.Count;
    if l > 0 then
    begin
      HintText := '';
      if l < 5 then
      begin
        for i := 0 to l - 1 do
          if HintText = '' then
            HintText :=
              DLManager.containers.Items[Node^.Index].ChapterName.Strings[i]{ + ' : ' +
        DLManager.containers.Items[Node^.Index].ChapterLinks.Strings[i]}
          else
            HintText := HintText + #10#13 +
              DLManager.containers.Items[Node^.Index].ChapterName.Strings[i]{ + ' : ' +
        DLManager.containers.Items[Node^.Index].ChapterLinks.Strings[i]};
      end
      else
      begin
        for i := 0 to 1 do
          if HintText = '' then
            HintText :=
              DLManager.containers.Items[Node^.Index].ChapterName.Strings[i]{ + ' : ' +
        DLManager.containers.Items[Node^.Index].ChapterLinks.Strings[i]}
          else
            HintText := HintText + #10#13 +
              DLManager.containers.Items[Node^.Index].ChapterName.Strings[i]{ + ' : ' +
        DLManager.containers.Items[Node^.Index].ChapterLinks.Strings[i]};
        HintText := HintText + #10#13 + '...';
        for i := l - 2 to l - 1 do
          HintText := HintText + #10#13 +
            DLManager.containers.Items[Node^.Index].ChapterName.Strings[i]{ + ' : ' +
        DLManager.containers.Items[Node^.Index].ChapterLinks.Strings[i]};
      end;
    end;
  end
  else
  begin
    p := Sender.GetNodeData(Node);
    case Column of
      1: HintText := p^.Status;
      2: HintText := p^.Progress;
      3: HintText := p^.Website;
      4: HintText := p^.SaveTo;
      5: HintText := DateTimeToStr(p^.dateTime);
    end;
  end;
end;

procedure TMainForm.vtDownloadGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if vtDownload.Header.Columns[Column].Position = 0 then
  begin
    ImageIndex := integer(DLManager.containers.Items[Node^.Index].Status);
  end;
end;

procedure TMainForm.vtDownloadGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PDownloadInfo;
  pos: Cardinal;
begin
  with Sender do
  begin
    pos := Node^.Index;
    Data := Sender.GetNodeData(Node);
    if (DLManager.containers.Count <> 0) then
      if (Assigned(Data)) and ((DLManager.containers.Items[pos] <> nil) or
        (not DLManager.containers.Items[pos].Thread.isTerminated)) then
      begin
        Data^.title := DLManager.containers.Items[pos].DownloadInfo.title;
        Data^.status := DLManager.containers.Items[pos].DownloadInfo.Status;
        Data^.progress := DLManager.containers.Items[pos].DownloadInfo.Progress;
        Data^.website := DLManager.containers.Items[pos].DownloadInfo.Website;
        Data^.saveTo := DLManager.containers.Items[pos].DownloadInfo.SaveTo;
        Data^.dateTime := DLManager.containers.Items[pos].DownloadInfo.dateTime;
        case Column of
          0: CellText := Data^.title;
          1: CellText := Data^.status;
          2: CellText := Data^.Progress;
          3: CellText := Data^.website;
          4: CellText := Data^.saveTo;
          5: CellText := DateTimeToStr(Data^.dateTime);
        end;
      end;
  end;
end;

procedure TMainForm.vtDownloadHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  if (not (Column = 2)) and (DLManager.containers.Count > 1) then
  begin
    DLManager.SortColumn := Column;
    DLManager.SortDirection := not DLManager.SortDirection;
    vtDownload.Header.SortDirection := TSortDirection(DLManager.SortDirection);
    vtDownload.Header.SortColumn := Column;
    //DLManager.Sort(Column);
    DLManager.SortNatural(Column);    //Natural Sorting
    options.WriteInteger('misc', 'SortDownloadColumn', vtDownload.Header.SortColumn);
    options.WriteBool('misc', 'SortDownloadDirection', DLManager.SortDirection);
    vtDownload.Repaint;
  end;
end;

procedure TMainForm.vtDownloadInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PDownloadInfo;
  pos: Cardinal;
begin
  with Sender do
  begin
    pos := Node^.Index;
    Data := GetNodeData(Node);
    if (DLManager.containers.Count <> 0) then
      if (DLManager.containers.Items[pos] <> nil) or
        (not DLManager.containers.Items[pos].Thread.isTerminated) then
      begin
        Data^.title := DLManager.containers.Items[pos].DownloadInfo.title;
        Data^.status := DLManager.containers.Items[pos].DownloadInfo.Status;
        Data^.progress := DLManager.containers.Items[pos].DownloadInfo.Progress;
        Data^.website := DLManager.containers.Items[pos].DownloadInfo.Website;
        Data^.saveTo := DLManager.containers.Items[pos].DownloadInfo.SaveTo;
        Data^.dateTime := DLManager.containers.Items[pos].DownloadInfo.dateTime;
      end;
  end;
  vtDownload.ValidateNode(Node, False);
end;

procedure TMainForm.vtDownloadKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN]) and (ssCtrl in Shift) then
  begin
    if Key = VK_DOWN then
      vtDownloadMoveItems(vtDownload.GetFirstSelected^.Index, dmBelow)
    else
      if vtDownload.GetFirstSelected^.Index > 0 then
        vtDownloadMoveItems(vtDownload.GetFirstSelected^.Index - 1, dmAbove);
  end;
end;

procedure TMainForm.vtDownloadKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    miDeleteTaskClick(miDeleteTask);
end;

procedure TMainForm.vtFavoritesBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PFavoriteInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if Trim(Data^.Link) = '' then
    begin
      TargetCanvas.Brush.Color := $008080FF;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TMainForm.vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  miOpenFolder2Click(Sender);
end;

procedure TMainForm.vtFavoritesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PFavoriteInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TMainForm.vtFavoritesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  Data: PFavoriteInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    case Column of
      1: if Trim(Data^.Link) = '' then
           HintText := RS_HintFavoriteProblem
         else
           HintText := Data^.Title;
      2: HintText := Data^.currentChapter;
      3: HintText := Data^.website;
      4: HintText := Data^.saveTo;
    end;
end;

procedure TMainForm.vtFavoritesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PFavoriteInfo;
begin
  if vtFavorites.Header.Columns[Column].Position = 1 then
  begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then
      if Trim(Data^.Link) = '' then
        ImageIndex := 7
      else
        ImageIndex := -1;
  end;
end;

// vtFavorites

procedure TMainForm.vtFavoritesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PFavoriteInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    case Column of
      0: CellText := Data^.numbering;
      1: CellText := Data^.Title;
      2: CellText := Data^.currentChapter;
      3: CellText := Data^.website;
      4: CellText := Data^.saveTo;
    end;
end;

procedure TMainForm.vtFavoritesHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  if (not (Column = 0)) and (not FavoriteManager.isRunning) and (FavoriteManager.Count > 1) then
  begin
    FavoriteManager.isRunning := True;
    try
      FavoriteManager.SortColumn := Column;
      FavoriteManager.sortDirection := not FavoriteManager.sortDirection;
      vtFavorites.Header.SortColumn := Column;
      vtFavorites.Header.SortDirection := TSortDirection(FavoriteManager.sortDirection);
      FavoriteManager.SortNatural(Column);
      options.WriteInteger('misc', 'SortFavoritesColumn', vtFavorites.Header.SortColumn);
      options.WriteBool('misc', 'SortFavoritesDirection', FavoriteManager.sortDirection);
    finally
      UpdateVtFavorites;
      FavoriteManager.isRunning := False;
    end;
  end;
end;

procedure TMainForm.vtFavoritesInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PFavoriteInfo;
  pos: Cardinal;
begin
  with Sender do
  begin
    pos := Node^.Index;
    Data := GetNodeData(Node);
    Data^.numbering := IntToStr(pos + 1);
    Data^.Title := FavoriteManager.Favorites[pos].FavoriteInfo.Title;
    Data^.currentChapter := FavoriteManager.Favorites[pos].FavoriteInfo.currentChapter;
    Data^.website := FavoriteManager.Favorites[pos].FavoriteInfo.website;
    Data^.saveTo := FavoriteManager.Favorites[pos].FavoriteInfo.saveTo;
    Data^.Link := FavoriteManager.Favorites[pos].FavoriteInfo.Link;
  end;
  vtFavorites.ValidateNode(Node, False);
end;

procedure TMainForm.vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  //if (NOT isUpdating) then
  //begin
  if vtMangaList.SelectedCount > 0 then
    sbMain.Panels[0].Text := Format(stSelected, [vtMangaList.SelectedCount])
  else
    sbMain.Panels[0].Text := '';
  //end;
end;

procedure TMainForm.vtMangaListColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  miMangaListViewInfosClick(vtMangaList);
end;

procedure TMainForm.vtMangaListDragAllowed(Sender : TBaseVirtualTree;
  Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
begin
  Allowed := False;
end;

procedure TMainForm.vtMangaListDragOver(Sender : TBaseVirtualTree;
  Source : TObject; Shift : TShiftState; State : TDragState; const Pt : TPoint;
  Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
begin
  Accept := False;
end;

// options

procedure TMainForm.btOptionApplyClick(Sender: TObject);
var
  i: Cardinal;
  s: String;
  isStillHaveCurrentWebsite: Boolean = False;
  Data: PMangaListItem;
begin
  try
    s := SaveMangaOptions;
    if s = '' then
    begin
      MessageDlg('', stDldMangaListSelect,
        mtConfirmation, [mbYes], 0);
      Exit;
    end;
    options.WriteString('general', 'MangaListSelect', s);
    mangalistIni.UpdateFile;

    cbSelectManga.Clear;
    for i := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
    begin
      Data := vtOptionMangaSiteSelection.GetNodeData(optionMangaSiteSelectionNodes[i]);
      if (optionMangaSiteSelectionNodes[i]^.CheckState = csCheckedNormal) and
        (Data^.Text <> '') then
      begin
        cbSelectManga.Items.Add(Data^.Text);
      end;
    end;

    for i := 0 to cbSelectManga.Items.Count - 1 do
    begin
      if cbSelectManga.Items[i] = currentWebsite then
      begin
        cbSelectManga.ItemIndex := i;
        isStillHaveCurrentWebsite := True;
        Break;
      end;
    end;

    // TODO: optimize required
    if not isStillHaveCurrentWebsite then
    begin
      cbSelectManga.ItemIndex := 0;
      dataProcess.RemoveFilter;
      if not dataProcess.isFilterAllSites then
        dataProcess.SaveToFile;
      dataProcess.Free;
      dataProcess := TDataProcess.Create;
      dataProcess.LoadFromFile(cbSelectManga.Items.Strings[0]);
      vtMangaList.OnInitNode := @vtMangaListInitNode;
      vtMangaList.Clear;
      vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
      lbMode.Caption := Format(stModeAll, [dataProcess.filterPos.Count]);
      currentWebsite := cbSelectManga.Items[0];
    end;
    options.WriteBool('general', 'LiveSearch', cbOptionLiveSearch.Checked);
    options.WriteBool('general', 'OneInstanceOnly', cbOptionOneInstanceOnly.Checked);
    //FMDInstace
    if cbOptionOneInstanceOnly.Checked then
    begin
      if FMDInstance = nil then
      begin
        FMDInstance := TSimpleIPCServer.Create(Self);
        FMDInstance.ServerID := FMD_INSTANCE;
        FMDInstance.Global := True;
        FMDInstance.OnMessage := @FMDInstanceReceiveMsg;
        FMDInstance.StartServer;
      end;
    end
    else
    begin
      if FMDInstance <> nil then
      begin
        FMDInstance.StopServer;
        FreeAndNil(FMDInstance);
      end;
    end;

    options.WriteBool('general', 'MinimizeToTray', cbOptionMinimizeToTray.Checked);
    options.WriteInteger('general', 'NewMangaTime', seOptionNewMangaTime.Value);
    options.WriteInteger('general', 'LetFMDDo', cbOptionLetFMDDo.ItemIndex);
    cbOptionLetFMDDoItemIndex := cbOptionLetFMDDo.ItemIndex;
    cbOptionBatotoUseIE.Checked := False;
    options.WriteBool('general', 'BatotoUseIE', cbOptionBatotoUseIE.Checked);
    OptionBatotoUseIEChecked := cbOptionBatotoUseIE.Checked;
    options.WriteBool('general', 'LoadMangaCover', cbOptionEnableLoadCover.Checked);
    OptionEnableLoadCover := cbOptionEnableLoadCover.Checked;
    options.WriteString('general', 'ExternalProgram', edOptionExternal.Text);

    options.WriteInteger('connections', 'NumberOfTasks', seOptionMaxParallel.Value);
    options.WriteInteger('connections', 'NumberOfThreadsPerTask',
      seOptionMaxThread.Value);
    options.WriteInteger('connections', 'Retry', seOptionMaxRetry.Value);
    DLManager.retryConnect := seOptionMaxRetry.Value;
    options.WriteBool('connections', 'UseProxy', cbOptionUseProxy.Checked);
    options.WriteString('connections', 'ProxyType', cbOptionProxyType.Text);
    options.WriteString('connections', 'Host', edOptionHost.Text);
    options.WriteString('connections', 'Pass', edOptionPass.Text);
    options.WriteString('connections', 'Port', edOptionPort.Text);
    options.WriteString('connections', 'User', edOptionUser.Text);

    if Trim(edOptionDefaultPath.Text) = '' then
      edOptionDefaultPath.Text := DEFAULT_PATH;
    edOptionDefaultPath.Text := CorrectPathSys(edOptionDefaultPath.Text);
    options.WriteString('saveto', 'SaveTo', edOptionDefaultPath.Text);
    options.WriteBool('saveto', 'PathConv', cbOptionPathConvert.Checked);
    options.WriteBool('saveto', 'GenChapName', cbOptionGenerateChapterName.Checked);
    options.WriteBool('saveto', 'GenMangaName', cbOptionGenerateMangaFolderName.Checked);
    options.WriteInteger('saveto', 'Compress', rgOptionCompress.ItemIndex);
    options.WriteBool('saveto', 'AutoNumberChapter', cbOptionAutoNumberChapter.Checked);
    OptionAutoNumberChapterChecked := cbOptionAutoNumberChapter.Checked;
    options.WriteInteger('saveto', 'PDFQuality', seOptionPDFQuality.Value);
    OptionPDFQuality := seOptionPDFQuality.Value;
    options.WriteString('saveto', 'CustomRename', edOptionCustomRename.Text);
    OptionCustomRename := edOptionCustomRename.Text;

    options.WriteBool('update', 'AutoRemoveCompletedManga',
      cbOptionAutoRemoveCompletedManga.Checked);
    OptionAutoRemoveCompletedManga := cbOptionAutoRemoveCompletedManga.Checked;
    options.WriteBool('update', 'AutoCheckUpdateAtStartup',
      cbOptionAutoCheckUpdate.Checked);
    options.WriteBool('update', 'AutoCheckFavStartup',
      cbOptionAutoCheckFavStartup.Checked);
    OptionAutoCheckFavStartup := cbOptionAutoCheckFavStartup.Checked;
    options.WriteInteger('update', 'AutoCheckMinutes', seOptionCheckMinutes.Value);
    OptionCheckMinutes := seOptionCheckMinutes.Value;
    options.WriteBool('update', 'UpdateListNoMangaInfo',
      cbOptionUpdateListNoMangaInfo.Checked);
    OptionUpdateListNoMangaInfo := cbOptionUpdateListNoMangaInfo.Checked;
    options.WriteBool('update', 'UpdateListRemoveDuplicateLocalData',
      cbOptionUpdateListRemoveDuplicateLocalData.Checked);
    OptionUpdateListRemoveDuplicateLocalData := cbOptionUpdateListRemoveDuplicateLocalData.Checked;

    DLManager.compress := rgOptionCompress.ItemIndex;

    options.WriteInteger('languages', 'Select', cbLanguages.ItemIndex);
    options.WriteString('languages', 'Language',
      cbLanguages.Items.Strings[cbLanguages.ItemIndex]);

    options.WriteBool('dialogs', 'ShowQuitDialog', cbOptionShowQuitDialog.Checked);
    options.WriteBool('dialogs', 'ShowDeleteDldTaskDialog',
      cbOptionShowDeleteTaskDialog.Checked);
    options.WriteBool('dialogs', 'ShowFavoritesDialog',
      cbOptionShowFavoriteDialog.Checked);

    options.WriteBool('misc', 'ShowBatotoSG', cbOptionShowBatotoSG.Checked);
    options.WriteBool('misc', 'ShowAllLang', cbOptionShowAllLang.Checked);
    options.WriteBool('misc', 'AutoDlFav', cbOptionAutoDlFav.Checked);
    OptionShowBatotoSG := cbOptionShowBatotoSG.Checked;
    OptionShowAllLang := cbOptionShowAllLang.Checked;
    OptionAutoDlFav := cbOptionAutoDlFav.Checked;

    options.WriteBool('saveto', 'ConvertDigitVolume', cbOptionDigitVolume.Checked);
    options.WriteBool('saveto', 'ConvertDigitChapter', cbOptionDigitChapter.Checked);
    options.WriteInteger('saveto', 'DigitVolumeLength', seOptionDigitVolume.Value);
    options.WriteInteger('saveto', 'DigitChapterLength', seOptionDigitChapter.Value);

    options.WriteBool('misc', 'MangafoxRemoveWatermarks',
      cbOptionMangaFoxRemoveWatermarks.Checked);

    options.UpdateFile;

    if OptionCheckMinutes = 0 then
      itCheckForChapters.Enabled := False
    else
    begin
      itCheckForChapters.Interval := OptionCheckMinutes * 60000;
      itCheckForChapters.Enabled := True;
    end;

    if cbOptionUseProxy.Checked then
    begin
      ProxyType := cbOptionProxyType.Text;
      Host := edOptionHost.Text;
      Pass := edOptionPass.Text;
      Port := edOptionPort.Text;
      User := edOptionUser.Text;
    end
    else
    begin
      ProxyType := '';
      Host := '';
      Pass := '';
      Port := '';
      User := '';
    end;

    LoadLanguage(cbLanguages.ItemIndex);

    cbOptionLetFMDDo.ItemIndex := cbOptionLetFMDDoItemIndex;

    DLManager.maxDLTasks := seOptionMaxParallel.Value;
    DLManager.maxDLThreadsPerTask := seOptionMaxThread.Value;
    DLManager.retryConnect := seOptionMaxRetry.Value;
  finally
    //Recheck download thread
    DLManager.CheckAndActiveTask;
    vtMangaList.Repaint;
    tvDownloadFilterRepaint;
  end;
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
  if (isExiting) or (dataProcess.JDN.Count = 0) or (dataProcess.filterPos.Count = 0) then
    Exit;
  if miHighlightNewManga.Checked then
  begin
    try
      if currentJDN - cardinal(dataProcess.JDN.Items[dataProcess.GetPos(Node^.Index)]) <
        seOptionNewMangaTime.Value then
      begin
        TargetCanvas.Brush.Color := $FDC594;
        TargetCanvas.FillRect(CellRect);
      end;
    except
      on E: Exception do ;
    end;
  end;
end;

procedure TMainForm.vtMangaListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PMangaListItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TMainForm.vtMangaListGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  LPos: Integer;
  s: String;
begin
  s := '';
  LPos := dataProcess.GetPos(Node^.Index);
  if dataProcess.isFilterAllSites then
    s := s + stDownloadWebsite + ':' + #13#10 +
      GetMangaSiteName(dataProcess.site.Items[LPos]) + #13#10#13#10;
  if Trim(dataProcess.Param[LPos, DATA_PARAM_NAME]) <> '' then
    s := s + infoName + ':' + #13#10 + dataProcess.Param[LPos, DATA_PARAM_NAME];
  if Trim(dataProcess.Param[LPos, DATA_PARAM_AUTHORS]) <> '' then
    s := s + #13#10#13#10 + infoAuthors + ':' + #13#10 +
      dataProcess.Param[LPos, DATA_PARAM_AUTHORS];
  if Trim(dataProcess.Param[LPos, DATA_PARAM_ARTISTS]) <> '' then
    s := s + #13#10#13#10 + infoArtists + ':' + #13#10 +
      dataProcess.Param[LPos, DATA_PARAM_ARTISTS];
  if Trim(dataProcess.Param[LPos, DATA_PARAM_GENRES]) <> '' then
    s := s + #13#10#13#10 + infoGenres + ':' + #13#10 +
      dataProcess.Param[LPos, DATA_PARAM_GENRES];
  if Trim(dataProcess.Param[LPos, DATA_PARAM_STATUS]) <> '' then
  begin
    s := s + #13#10#13#10 + infoStatus + ':' + #13#10;
    if dataProcess.Param[LPos, DATA_PARAM_STATUS] = '0' then
      s := s + cbFilterStatus.Items.Strings[0]
    else
      s := s + cbFilterStatus.Items.Strings[1];
  end;
  if Trim(dataProcess.Param[LPos, DATA_PARAM_SUMMARY]) <> '' then
    //s := s + #13#10#13#10 + infoSummary + ':' + #13#10 + PrepareSummaryForHint(dataProcess.Param[LPos, DATA_PARAM_SUMMARY], 80);
    s := s + #13#10#13#10 + infoSummary + ':' + #13#10 +
      StringBreaks(dataProcess.Param[LPos, DATA_PARAM_SUMMARY]);
  HintText := s;
end;

procedure TMainForm.vtMangaListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PMangaListItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data^.Text;
end;

procedure TMainForm.vtMangaListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PMangaListItem;
  pos: Cardinal;
begin
  with Sender do
  begin
    pos := dataProcess.filterPos.Items[Node^.Index];
    Data := GetNodeData(Node);
    Data^.Text := dataProcess.Param[pos, DATA_PARAM_NAME] +
      ' (' +
      dataProcess.Param[pos, DATA_PARAM_NUMCHAPTER] + ')';
  end;
  vtMangaList.ValidateNode(Node, False);
end;

procedure TMainForm.vtMangaListInitSearchNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PMangaListItem;
  pos: Cardinal;
begin
  with Sender do
  begin
    pos := dataProcess.searchPos.Items[Node^.Index];
    Data := GetNodeData(Node);
    Data^.Text := dataProcess.Param[pos, DATA_PARAM_NAME] +
      ' (' +
      dataProcess.Param[pos, DATA_PARAM_NUMCHAPTER] + ')';
  end;
end;

procedure TMainForm.InitCheckboxes;
var
  i: Cardinal;
begin
  for i := 0 to 37 do
    TCheckBox(pnGenres.Controls[i]).State := cbGrayed;
end;

procedure TMainForm.ShowAllTasks;
var
  i: Cardinal;
  xNode: PVirtualNode;
  canExit: Boolean = False;
begin
  if vtDownload.RootNodeCount = 0 then
    Exit;
  xNode := vtDownload.GetLast;
  for i := vtDownload.RootNodeCount - 1 downto 0 do
  begin
    vtDownload.isVisible[xNode] := True;

    if canExit then
      Exit;
    if xNode = vtDownload.GetFirst then
      canExit := True;
    xNode := vtDownload.GetPrevious(xNode);
    if xNode = vtDownload.GetFirst then
      canExit := True;
  end;
end;

procedure TMainForm.ShowCompletedTasks;
var
  i: Cardinal;
  xNode: PVirtualNode;
  canExit: Boolean = False;
begin
  if vtDownload.RootNodeCount = 0 then
    Exit;
  xNode := vtDownload.GetLast;
  for i := vtDownload.RootNodeCount - 1 downto 0 do
  begin
    if DLManager.containers.Items[i].Status = STATUS_FINISH then
      vtDownload.isVisible[xNode] := True
    else
      vtDownload.isVisible[xNode] := False;

    if canExit then
      Exit;
    if xNode = vtDownload.GetFirst then
      canExit := True;
    xNode := vtDownload.GetPrevious(xNode);
    if xNode = vtDownload.GetFirst then
      canExit := True;
  end;
end;

procedure TMainForm.ShowInProgressTasks;
var
  i: Cardinal;
  xNode: PVirtualNode;
  canExit: Boolean = False;
begin
  if vtDownload.RootNodeCount = 0 then
    Exit;
  xNode := vtDownload.GetLast;
  for i := vtDownload.RootNodeCount - 1 downto 0 do
  begin
    if (DLManager.containers.Items[i].Status in
      [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT]) then
      vtDownload.isVisible[xNode] := True
    else
      vtDownload.isVisible[xNode] := False;

    if canExit then
      Exit;
    if xNode = vtDownload.GetFirst then
      canExit := True;
    xNode := vtDownload.GetPrevious(xNode);
    if xNode = vtDownload.GetFirst then
      canExit := True;
  end;
end;

procedure TMainForm.ShowStoppedTasks;
var
  i: Cardinal;
  xNode: PVirtualNode;
  canExit: Boolean = False;
begin
  if vtDownload.RootNodeCount = 0 then
    Exit;
  xNode := vtDownload.GetLast;
  for i := DLManager.containers.Count - 1 downto 0 do
  begin
    if DLManager.containers.Items[i].Status = STATUS_STOP then
      vtDownload.isVisible[xNode] := True
    else
      vtDownload.isVisible[xNode] := False;

    if canExit then
      Exit;
    if xNode = vtDownload.GetFirst then
      canExit := True;
    xNode := vtDownload.GetPrevious(xNode);
    if xNode = vtDownload.GetFirst then
      canExit := True;
  end;
end;

procedure TMainForm.ShowTasksOnCertainDays(const L, H: longint);
var
  i: Cardinal;
  jdn: longint;
  xNode: PVirtualNode;
  canExit: Boolean = False;
  dt: TDateTime;
  day, month, year: Word;
begin
  if vtDownload.RootNodeCount = 0 then
    Exit;
  xNode := vtDownload.GetLast;
  for i := vtDownload.RootNodeCount - 1 downto 0 do
  begin
    dt := DLManager.containers.Items[i].DownloadInfo.dateTime;
    DecodeDate(dt, year, month, day);
    jdn := DateToJDN(year, month, day);

    if (jdn >= L) and (jdn <= H) then
      vtDownload.isVisible[xNode] := True
    else
      vtDownload.isVisible[xNode] := False;

    if canExit then
      Exit;
    if xNode = vtDownload.GetFirst then
      canExit := True;
    xNode := vtDownload.GetPrevious(xNode);
    if xNode = vtDownload.GetFirst then
      canExit := True;
  end;
end;

procedure TMainForm.ShowTodayTasks;
begin
  ShowTasksOnCertainDays(GetCurrentJDN, GetCurrentJDN);
end;

procedure TMainForm.ShowYesterdayTasks;
begin
  ShowTasksOnCertainDays(GetCurrentJDN - 1, GetCurrentJDN - 1);
end;

procedure TMainForm.ShowOneWeekTasks;
begin
  ShowTasksOnCertainDays(GetCurrentJDN - 7, GetCurrentJDN);
end;

procedure TMainForm.ShowOneMonthTasks;
begin
  ShowTasksOnCertainDays(GetCurrentJDN - 30, GetCurrentJDN);
end;

procedure TMainForm.vtDownloadFilters;
begin
  if (isRunDownloadFilter) or
    (not Assigned(tvDownloadFilter.Selected)) then
    Exit;
  isRunDownloadFilter := True;
  case tvDownloadFilter.Selected.AbsoluteIndex of
    0, 4:
      ShowAllTasks;
    1:
      ShowCompletedTasks;
    2:
      ShowInProgressTasks;
    3:
      ShowStoppedTasks;
    5:
      ShowTodayTasks;
    6:
      ShowYesterdayTasks;
    7:
      ShowOneWeekTasks;
    8:
      ShowOneMonthTasks;
  end;
  tvDownloadFilterRepaint;
  isRunDownloadFilter := False;
end;

procedure TMainForm.AddChapterNameToList;
begin
  UpdateVtChapter;
end;

procedure TMainForm.AddTextToInfo(title, infoText: String);
var
  fp: TFontParams;
  cp, np: Integer;
  fn: String;
begin
  infoText := Trim(infoText);
  if infoText <> '' then
    with rmInformation do
    begin
      if Trim(Lines.Text) <> '' then
        Lines.Add('');
      SelStart := UTF8Length(Lines.Text);
      cp := SelStart;
      GetTextAttributes(cp, fp);
      fn := rmInformation.Font.Name;
      fp.Style := [fsBold, fsUnderline];
      fp.Name := fn;
      Inc(fp.Size);
      Lines.Add(title);
      SelStart := UTF8Length(Lines.Text);
      np := SelStart;
      SetTextAttributes(cp, np - cp, fp);
      if title = infoSummary then
        infoText := Trim(StringBreaks(infoText));
      Lines.Add(infoText);
      fp.Style := [];
      fp.Name := fn;
      Dec(fp.Size);
      SetTextAttributes(np, UTF8Length(Lines.Text) - np, fp);
    end;
end;

procedure TMainForm.ShowInformation(const title, website, link: String);
var
  i: Integer;
begin
  pcMain.PageIndex := 1;
  if Trim(edSaveTo.Text) = '' then
    edSaveTo.Text := options.ReadString('saveto', 'SaveTo', DEFAULT_PATH);
  if Trim(edSaveTo.Text) = '' then
    edSaveTo.Text := DEFAULT_PATH;
  edSaveTo.Text := CorrectPathSys(edSaveTo.Text);

  with rmInformation do
  begin
    imCover.Picture.Assign(nil);
    Clear;

    if (GetInfosThread <> nil) and
      ((GetInfosThread.MangaListPos > -1) or (GetInfosThread.MangaListPos = -2)) then
    begin
      mangaInfo.title := title;
      mangaInfo.link := link;
    end
    else
      edURL.Text := mangaInfo.url;

    AddTextToInfo(infoName, mangaInfo.title + #10#13);
    AddTextToInfo(infoAuthors, mangaInfo.authors + #10#13);
    AddTextToInfo(infoArtists, mangaInfo.artists + #10#13);
    AddTextToInfo(infoGenres, mangaInfo.genres + #10#13);
    if mangaInfo.status = '0' then
      AddTextToInfo(infoStatus, cbFilterStatus.Items.Strings[0] + #10#13)
    else
      AddTextToInfo(infoStatus, cbFilterStatus.Items.Strings[1] + #10#13);
    //edURL.Text:= mangaInfo.url;
    AddTextToInfo(infoSummary, mangaInfo.summary);
    CaretPos := Point(0, 0);
  end;
  SetLength(ChapterList, mangaInfo.chapterName.Count);
  for i := 0 to mangaInfo.chapterName.Count - 1 do
  begin
    ChapterList[i].Title := mangaInfo.chapterName[i];
    ChapterList[i].Link := mangaInfo.chapterLinks[i];
    ChapterList[i].Downloaded := False;
  end;
  DLManager.GetDownloadedChaptersState(mangaInfo.website + mangaInfo.link, ChapterList);
  UpdateVtChapter;

  btDownload.Enabled := (clbChapterList.RootNodeCount > 0);
  btReadOnline.Enabled := (mangaInfo.link <> '');
  btAddToFavorites.Enabled := not SitesWithoutFavorites(website);

  //check if manga already in FavoriteManager list
  if btAddToFavorites.Enabled and (FavoriteManager.Favorites.Count > 0) then
    btAddToFavorites.Enabled := not FavoriteManager.IsMangaExist(mangaInfo.title, website);
end;

procedure TMainForm.RunGetList;
begin
  if (MessageDlg('', stDlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0) =
    mrYes) and
    (not isUpdating) then
  begin
    isUpdating := True;
    updateDB := TUpdateDBThread.Create;
    updateDB.websiteName := cbSelectManga.Items[cbSelectManga.ItemIndex];
    updateDB.Start;
  end;
end;

procedure TMainForm.LoadOptions;
begin
  if options.ReadBool('connections', 'UseProxy', False) then
  begin
    ProxyType := options.ReadString('connections', 'ProxyType', 'HTTP');
    Host := options.ReadString('connections', 'Host', '');
    Pass := options.ReadString('connections', 'Pass', '');
    Port := options.ReadString('connections', 'Port', '');
    User := options.ReadString('connections', 'User', '');
  end;

  cbOptionLiveSearch.Checked := options.ReadBool('general', 'LiveSearch', True);
  cbOptionOneInstanceOnly.Checked :=
    options.ReadBool('general', 'OneInstanceOnly', True);

  //FMDInstance
  if cbOptionOneInstanceOnly.Checked then
  begin
    if FMDInstance = nil then
    begin
      FMDInstance := TSimpleIPCServer.Create(Self);
      FMDInstance.ServerID := FMD_INSTANCE;
      FMDInstance.Global := True;
      FMDInstance.OnMessage := @FMDInstanceReceiveMsg;
      FMDInstance.StartServer;
    end;
  end
  else
  begin
    if FMDInstance <> nil then
    begin
      FMDInstance.StopServer;
      FreeAndNil(FMDInstance);
    end;
  end;

  cbOptionMinimizeToTray.Checked := options.ReadBool('general', 'MinimizeToTray', False);
  OptionEnableLoadCover := options.ReadBool('general', 'LoadMangaCover', True);
  cbOptionEnableLoadCover.Checked := OptionEnableLoadCover;
  cbOptionLetFMDDo.ItemIndex := options.ReadInteger('general', 'LetFMDDo', 0);
  cbOptionLetFMDDoItemIndex := cbOptionLetFMDDo.ItemIndex;

  cbOptionBatotoUseIE.Checked := False;
  //options.ReadBool('general', 'BatotoUseIE', TRUE);
  cbOptionAutoNumberChapter.Checked :=
    options.ReadBool('general', 'AutoNumberChapter', True);
  edOptionExternal.Text := options.ReadString('general', 'ExternalProgram', '');

  OptionBatotoUseIEChecked := cbOptionBatotoUseIE.Checked;
  OptionAutoNumberChapterChecked := cbOptionAutoNumberChapter.Checked;

  cbAddAsStopped.Checked := options.ReadBool('general', 'AddAsStopped', False);
  LoadLanguage(options.ReadInteger('languages', 'Select', 0));

  DLManager.maxDLTasks := options.ReadInteger('connections', 'NumberOfTasks', 1);
  DLManager.maxDLThreadsPerTask :=
    options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
  DLManager.retryConnect := options.ReadInteger('connections', 'Retry', 0);

  DLManager.compress := options.ReadInteger('saveto', 'Compress', 0);

  cbOptionPathConvert.Checked := options.ReadBool('saveto', 'PathConv', False);
  cbOptionGenerateChapterName.Checked :=
    options.ReadBool('saveto', 'GenChapName', False);
  cbOptionGenerateMangaFolderName.Checked :=
    options.ReadBool('saveto', 'GenMangaName', True);
  cbOptionAutoNumberChapter.Checked :=
    options.ReadBool('saveto', 'AutoNumberChapter', True);
  seOptionPDFQuality.Value := options.ReadInteger('saveto', 'PDFQuality', 95);
  OptionPDFQuality := seOptionPDFQuality.Value;
  edOptionCustomRename.Text :=
    options.ReadString('saveto', 'CustomRename', DEFAULT_CUSTOM_RENAME);
  OptionCustomRename := edOptionCustomRename.Text;

  cbOptionAutoCheckUpdate.Checked :=
    options.ReadBool('update', 'AutoCheckUpdateAtStartup', True);

  cbOptionAutoRemoveCompletedManga.Checked :=
    options.ReadBool('update', 'AutoRemoveCompletedManga', True);
  OptionAutoRemoveCompletedManga := cbOptionAutoRemoveCompletedManga.Checked;
  cbOptionAutoCheckFavStartup.Checked :=
    options.ReadBool('update', 'AutoCheckFavStartup', False);
  OptionAutoCheckFavStartup := cbOptionAutoCheckFavStartup.Checked;
  seOptionCheckMinutes.Value := options.ReadInteger('update', 'AutoCheckMinutes', 0);
  OptionCheckMinutes := seOptionCheckMinutes.Value;
  cbOptionUpdateListNoMangaInfo.Checked :=
    options.ReadBool('update', 'UpdateListNoMangaInfo', False);
  OptionUpdateListNoMangaInfo := cbOptionUpdateListNoMangaInfo.Checked;
  cbOptionUpdateListRemoveDuplicateLocalData.Checked :=
    options.ReadBool('update', 'UpdateListRemoveDuplicateLocalData', False);
  OptionUpdateListRemoveDuplicateLocalData := cbOptionUpdateListRemoveDuplicateLocalData.Checked;

  // misc
  cbOptionShowBatotoSG.Checked := options.ReadBool('misc', 'ShowBatotoSG', True);
  OptionShowBatotoSG := cbOptionShowBatotoSG.Checked;
  cbOptionShowAllLang.Checked := options.ReadBool('misc', 'ShowAllLang', False);
  OptionShowAllLang := cbOptionShowAllLang.Checked;
  cbOptionAutoDlFav.Checked := options.ReadBool('misc', 'AutoDlFav', False);
  OptionAutoDlFav := cbOptionAutoDlFav.Checked;

  vtFavorites.Header.SortColumn := options.ReadInteger('misc', 'SortFavoritesColumn', 1);
  FavoriteManager.sortDirection := options.ReadBool('misc', 'SortFavoritesDirection', False);

  vtDownload.Header.SortColumn := options.ReadInteger('misc', 'SortDownloadColumn', 0);
  DLManager.SortDirection := options.ReadBool('misc', 'SortDownloadDirection', False);
  vtDownload.Header.SortDirection := TSortDirection(DLManager.SortDirection);

  vtFavorites.Header.SortDirection := TSortDirection(FavoriteManager.sortDirection);

  if OptionCheckMinutes = 0 then
    itCheckForChapters.Enabled := False
  else
  begin
    itCheckForChapters.Interval := OptionCheckMinutes * 60000;
    itCheckForChapters.Enabled := True;
  end;

  cbOptionDigitVolume.Checked := options.ReadBool('saveto', 'ConvertDigitVolume', True);
  seOptionDigitVolume.Value := options.ReadInteger('saveto', 'DigitVolumeLength', 2);
  seOptionDigitVolume.Enabled := cbOptionDigitVolume.Checked;
  cbOptionDigitChapter.Checked :=
    options.ReadBool('saveto', 'ConvertDigitChapter', True);
  seOptionDigitChapter.Value := options.ReadInteger('saveto', 'DigitChapterLength', 3);
  seOptionDigitChapter.Enabled := cbOptionDigitChapter.Checked;

  cbOptionMangaFoxRemoveWatermarks.Checked :=
    options.ReadBool('misc', 'MangafoxRemoveWatermarks', False);
end;

procedure TMainForm.LoadMangaOptions;
var
  isDeleteUnusedManga: Boolean;
  i, j, sel: Integer;
  lang: TStringList;
  s, currentLanguage: String;
  ANode, currentRootNode: PVirtualNode;
  Data: PMangaListItem;
  wName, wLang: TStringList;
begin
  wName := TStringList.Create;
  wLang := TStringList.Create;
  lang := TStringList.Create;
  try
    mangalistIni.ReadSection('available', lang);
    if lang.Count > 0 then
      for i := 0 to lang.Count - 1 do
      begin
        s := mangalistIni.ReadString('available', lang[i], '');
        ExtractParam(wName, s, ',', False);
        while wlang.Count < wName.Count do
          wLang.Add(lang[i]);
      end;

    // load to option list
    if wName.Count > 0 then
    begin
      SetLength(optionMangaSiteSelectionNodes, wName.Count);
      currentLanguage := '';
      for i := 0 to wName.Count - 1 do
        with vtOptionMangaSiteSelection do
        begin
          if currentLanguage <> wLang[i] then
          begin
            currentLanguage := wLang[i];
            currentRootNode := AddChild(nil);
            Data := GetNodeData(currentRootNode);
            Data^.Text := currentLanguage;
            ValidateNode(currentRootNode, False);
          end;
          ANode := AddChild(currentRootNode);
          ANode^.CheckState := csUncheckedNormal;
          Data := GetNodeData(ANode);
          Data^.Text := wName[i];
          ValidateNode(ANode, False);
          optionMangaSiteSelectionNodes[i] := ANode;
        end;
    end;

    // load selected manga list
    lang.Clear;
    s := options.ReadString('general', 'MangaListSelect', DEFAULT_LIST);
    if Pos(SEPERATOR, s) <> 0 then
      ExtractParam(lang, s, SEPERATOR, False)
    else
      ExtractParam(lang, s, ',', False);
    cbSelectManga.Items.Assign(lang);

    // remove unused manga name
    i := 0;
    if (lang.Count > 0) and (Length(optionMangaSiteSelectionNodes) > 0) then
      while i < lang.Count do
      begin
        isDeleteUnusedManga := True;
        for j := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
        begin
          Data := vtOptionMangaSiteSelection.GetNodeData(
            optionMangaSiteSelectionNodes[j]);
          if lang[i] = Data^.Text then
          begin
            isDeleteUnusedManga := False;
            Break;
          end;
        end;
        if isDeleteUnusedManga then
          lang.Delete(i)
        else
          Inc(i);
      end;
    {
     cbSelectManga.Items.Clear;
     s := options.ReadString('general', 'MangaListSelect',
       mangalistIni.ReadString('general', 'DefaultSelect', DEFAULT_LIST));
     if Pos(SEPERATOR, s) > 0 then
       GetParams(lang, s)  //for old config
     else
       lang.DelimitedText := s;

     if (wName.Count > 0) and (wName.Count = wLang.Count) then
     begin
       SetLength(optionMangaSiteSelectionNodes, wName.Count);
       currentLanguage := '';
       for i := 0 to wName.Count - 1 do
       begin
         with vtOptionMangaSiteSelection do
         begin
           if currentLanguage <> wLang[i] then
           begin
             currentLanguage := wLang[i];
             currentRootNode := AddChild(nil);
             Data := GetNodeData(currentRootNode);
             Data^.Text := currentLanguage;
           end;
           ANode := AddChild(currentRootNode);
           ANode^.CheckState := csUncheckedNormal;
           Data := GetNodeData(ANode);
           Data^.Text := wName[i];

           optionMangaSiteSelectionNodes[i] := ANode;
         end;
       end;
     end;

     // remove deleted manga name
     i := 0;
     if (lang.Count > 0) and (Length(optionMangaSiteSelectionNodes) > 0) then
       while i < lang.Count do
       begin
         isDeleteUnusedManga := True;
         for j := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
         begin
           Data := vtOptionMangaSiteSelection.GetNodeData(
             optionMangaSiteSelectionNodes[j]);
           if lang[i] = Data^.Text then
           begin
             isDeleteUnusedManga := False;
             Break;
           end;
         end;
         if isDeleteUnusedManga then
           lang.Delete(i)
         else
           Inc(i);
       end;

     cbSelectManga.Items.Assign(lang);
     //set selected manga sites on option manga sites list
     if (lang.Count > 0) and (Length(optionMangaSiteSelectionNodes) > 0) then
       for i := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
       begin
         if FindStrLinear(lang, wName[i]) then
           if FindStrLinear(lang, Data^.Text) then
             optionMangaSiteSelectionNodes[i]^.CheckState := csCheckedNormal;
       end;
     }

    // load last selected manga
    if cbSelectManga.Items.Count > 0 then
    begin
      sel := options.ReadInteger('form', 'SelectManga', 0);
      if sel < 0 then
        sel := 0;
      if sel > cbSelectManga.Items.Count - 1 then
        sel := cbSelectManga.Items.Count - 1;
      cbSelectManga.ItemIndex := sel;
      currentWebsite := cbSelectManga.Items.Strings[cbSelectManga.ItemIndex];
      dataProcess.LoadFromFile(cbSelectManga.Items.Strings[cbSelectManga.ItemIndex]);
      dataProcess.website := cbSelectManga.Items.Strings[cbSelectManga.ItemIndex];
    end;
  finally
    lang.Free;
    wName.Free;
    wLang.Free;
  end;
end;

function TMainForm.SaveMangaOptions: String;
var
  i: Cardinal;
  Data: PMangaListItem;
begin
  Result := '';
  if Length(optionMangaSiteSelectionNodes) > 0 then
    for i := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
    begin
      if optionMangaSiteSelectionNodes[i]^.CheckState = csCheckedNormal then
      begin
        Data := vtOptionMangaSiteSelection.GetNodeData(optionMangaSiteSelectionNodes[i]);
        if Result = '' then
          Result := Data^.Text
        else
          Result := Result + ',' + Data^.Text;
      end;
    end;
end;

procedure TMainForm.edSearchChange(Sender: TObject);
begin
  if (upcase(edSearch.Text) = LastSearchStr) and (currentWebsite = LastSearchWeb) then
    Exit;
  if edSearch.Text = '' then
  begin
    LastSearchStr := '';
    //Screen.Cursor := crHourGlass;
    DataProcess.searchPos.Clear;
    vtMangaList.OnInitNode := @vtMangaListInitNode;
    vtMangaList.Clear;
    vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
    //Screen.Cursor := crDefault;
    Exit;
  end
  else
  if cbOptionLiveSearch.Checked then
  begin
    LastSearchWeb := currentWebsite;
    LastSearchStr := upcase(edSearch.Text);
    DataProcess.Search(edSearch.Text);
    vtMangaList.Clear;
    vtMangaList.OnInitNode := @vtMangaListInitSearchNode;
    vtMangaList.RootNodeCount := dataProcess.searchPos.Count;
  end;
end;

procedure TMainForm.edSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ((upcase(edSearch.Text) = LastSearchStr) and (currentWebsite = LastSearchWeb)) or
    cbOptionLiveSearch.Checked then
    Exit;

  if Key = VK_RETURN then
  begin
    LastSearchStr := upcase(edSearch.Text);
    if edSearch.Text = '' then
    begin
      Screen.Cursor := crHourGlass;
      DataProcess.searchPos.Clear;
      vtMangaList.OnInitNode := @vtMangaListInitNode;
      vtMangaList.Clear;
      vtMangaList.RootNodeCount := dataProcess.filterPos.Count;
      Screen.Cursor := crDefault;
    end
    else
    begin
      Screen.Cursor := crHourGlass;
      LastSearchWeb := currentWebsite;
      DataProcess.Search(edSearch.Text);
      vtMangaList.OnInitNode := @vtMangaListInitSearchNode;
      vtMangaList.Clear;
      vtMangaList.RootNodeCount := dataProcess.searchPos.Count;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.UpdateVtChapter;
begin
  clbChapterList.Clear;
  clbChapterList.RootNodeCount := Length(ChapterList);
end;

procedure TMainForm.UpdateVtDownload;
begin
  //vtDownload.Clear;
  vtDownload.RootNodeCount := DLManager.containers.Count;
  // the reason we put vtDownloadFilters in here instead of in DLManager because
  // the size of download list can change while this method is running
  vtDownloadFilters;
end;

procedure TMainForm.UpdateVtFavorites;
begin
  vtFavorites.Clear;
  vtFavorites.RootNodeCount := FavoriteManager.Favorites.Count;
end;

procedure TMainForm.LoadFormInformation;
begin
  pcLeft.Width := options.ReadInteger('form', 'MainSplitter', 195);
  sbMain.Panels[0].Width := pcLeft.Width + 4;

  pcMain.PageIndex := options.ReadInteger('form', 'pcMainPageIndex', 0);

  Left := options.ReadInteger('form', 'MainFormLeft', MainForm.Left);
  Top := options.ReadInteger('form', 'MainFormTop', MainForm.Top);
  Width := options.ReadInteger('form', 'MainFormWidth', 640);
  Height := options.ReadInteger('form', 'MainFormHeight', 480);

  if options.ReadBool('form', 'MainFormMaximized', False) then
    PrevWindowState := wsMaximized
  else
    PrevWindowState := wsNormal;
  WindowState := PrevWindowState;

  vtDownload.Header.Columns.Items[0].Width :=
    options.ReadInteger('form', 'vtDownload0Width', 50);
  vtDownload.Header.Columns.Items[1].Width :=
    options.ReadInteger('form', 'vtDownload1Width', 50);
  vtDownload.Header.Columns.Items[2].Width :=
    options.ReadInteger('form', 'vtDownload2Width', 50);
  vtDownload.Header.Columns.Items[3].Width :=
    options.ReadInteger('form', 'vtDownload3Width', 50);
  vtDownload.Header.Columns.Items[4].Width :=
    options.ReadInteger('form', 'vtDownload4Width', 50);
  vtDownload.Header.Columns.Items[5].Width :=
    options.ReadInteger('form', 'vtDownload5Width', 50);

  vtFavorites.Header.Columns.Items[0].Width :=
    options.ReadInteger('form', 'vtFavorites0Width', 50);
  vtFavorites.Header.Columns.Items[1].Width :=
    options.ReadInteger('form', 'vtFavorites1Width', 50);
  vtFavorites.Header.Columns.Items[2].Width :=
    options.ReadInteger('form', 'vtFavorites2Width', 50);
  vtFavorites.Header.Columns.Items[3].Width :=
    options.ReadInteger('form', 'vtFavorites3Width', 50);
  vtFavorites.Header.Columns.Items[4].Width :=
    options.ReadInteger('form', 'vtFavorites4Width', 50);
end;

procedure TMainForm.SaveFormInformation;
begin
  options.WriteInteger('form', 'MainSplitter', pcLeft.Width);
  options.WriteInteger('form', 'pcMainPageIndex', pcMain.PageIndex);

  options.WriteInteger('form', 'vtDownload0Width',
    vtDownload.Header.Columns.Items[0].Width);
  options.WriteInteger('form', 'vtDownload1Width',
    vtDownload.Header.Columns.Items[1].Width);
  options.WriteInteger('form', 'vtDownload2Width',
    vtDownload.Header.Columns.Items[2].Width);
  options.WriteInteger('form', 'vtDownload3Width',
    vtDownload.Header.Columns.Items[3].Width);
  options.WriteInteger('form', 'vtDownload4Width',
    vtDownload.Header.Columns.Items[4].Width);
  options.WriteInteger('form', 'vtDownload5Width',
    vtDownload.Header.Columns.Items[5].Width);

  options.WriteInteger('form', 'vtFavorites0Width',
    vtFavorites.Header.Columns.Items[0].Width);
  options.WriteInteger('form', 'vtFavorites1Width',
    vtFavorites.Header.Columns.Items[1].Width);
  options.WriteInteger('form', 'vtFavorites2Width',
    vtFavorites.Header.Columns.Items[2].Width);
  options.WriteInteger('form', 'vtFavorites3Width',
    vtFavorites.Header.Columns.Items[3].Width);
  options.WriteInteger('form', 'vtFavorites4Width',
    vtFavorites.Header.Columns.Items[4].Width);
  options.WriteInteger('form', 'SelectManga', cbSelectManga.ItemIndex);

  options.WriteBool('form', 'MainFormMaximized', (WindowState = wsMaximized));
  if WindowState = wsMaximized then
    WindowState := wsNormal;
  options.WriteInteger('form', 'MainFormLeft', Left);
  options.WriteInteger('form', 'MainFormTop', Top);
  options.WriteInteger('form', 'MainFormWidth', Width);
  options.WriteInteger('form', 'MainFormHeight', Height);
end;

procedure TMainForm.LoadLanguage(const pos: Integer);
var
  language: TIniFile;
  s, langL, lang: String;
  i, p: Cardinal;
begin
  if pos < 0 then
    Exit;

  language := TIniFile.Create(CONFIG_FOLDER + LANGUAGE_FILE);
  try
    p := language.ReadInteger('select', 'numberOfLanguages', 0);
    if p <> 0 then
    begin
      cbLanguages.Items.Clear;
      for i := 0 to p - 1 do
        cbLanguages.Items.Add(language.ReadString('select', IntToStr(i), 'English'));
    end;
    cbLanguages.ItemIndex := pos;

    langL := options.ReadString('languages', 'Language', 'English');
    lang := cbLanguages.Items.Strings[cbLanguages.ItemIndex];
    if langL <> lang then
    begin
      lang := 'English';
      cbLanguages.ItemIndex := cbLanguages.Items.IndexOf(lang);
    end;

    { TODO -oCholif -cT : There is a lot of translation missing since I added more features. Need to add all of them. Check UI }
    tsMangaList.Caption := language.ReadString(lang, 'tsMangaListCaption', '');
    tsDownload.Caption := language.ReadString(lang, 'tsDownloadCaption', '');
    tsInformation.Caption := language.ReadString(lang, 'tsInformationCaption', '');
    tsFilter.Caption := language.ReadString(lang, 'tsFilterCaption', '');
    stFilters := tsFilter.Caption;
    tsFavorites.Caption := language.ReadString(lang, 'tsFavoritesCaption', '');
    gbOptionFavorites.Caption := tsFavorites.Caption;
    tsOption.Caption := language.ReadString(lang, 'tsOptionCaption', '');
    tsAbout.Caption := language.ReadString(lang, 'tsAboutCaption', '');
    // edSearch.Text   := language.ReadString(lang, 'edSearchText', '');
    stSearch := language.ReadString(lang, 'edSearchText', '');
    stModeAll := language.ReadString(lang, 'stModeAll', '');
    stModeFilter := language.ReadString(lang, 'stModeFilter', '');

    stHistory := language.ReadString(lang, 'stHistory', '');
    stToday := language.ReadString(lang, 'stToday', '');
    stYesterday := language.ReadString(lang, 'stYesterday', '');
    stOneWeek := language.ReadString(lang, 'stOneWeek', '');
    stOneMonth := language.ReadString(lang, 'stOneMonth', '');

    stAllDownloads := language.ReadString(lang, 'stAllDownloads', '');
    stInProgress := language.ReadString(lang, 'stInProgress', '');
    stStop := language.ReadString(lang, 'stStop', '');
    stStop := language.ReadString(lang, 'stStop', '');
    stPreparing := language.ReadString(lang, 'stPreparing', '');
    stDownloading := language.ReadString(lang, 'stDownloading', '');
    stFinish := language.ReadString(lang, 'stFinish', '');
    stFailed := language.ReadString(lang, 'stFailed', '');
    stWait := language.ReadString(lang, 'stWait', '');
    btUpdateList.Hint := language.ReadString(lang, 'btUpdateListHint', '');

    stAddToQueue := language.ReadString(lang, 'stAddToQueue', '');
    stCancel := language.ReadString(lang, 'stCancel', '');
    stNewChapterNotification :=
      language.ReadString(lang, 'stNewChapterNotification', '');

    mnUpdateList.Caption := language.ReadString(lang, 'mnUpdateListCaption', '');
    mnUpdateDownFromServer.Caption :=
      language.ReadString(lang, 'mnUpdateDownFromServerCaption', '');
    mnUpdate1Click.Caption := language.ReadString(lang, 'mnUpdate1ClickCaption', '');
    mnDownload1Click.Caption := language.ReadString(lang, 'mnDownload1ClickCaption', '');

    btSearchClear.Hint := language.ReadString(lang, 'btSearchHint', '');
    btRemoveFilter.Hint := language.ReadString(lang, 'btRemoveFilterHint', '');
    btRemoveFilterLarge.Hint := btRemoveFilter.Hint;
    cbSelectManga.Hint := language.ReadString(lang, 'cbSelectMangaHint', '');

    edSaveTo.EditLabel.Caption :=
      language.ReadString(lang, 'edSaveToEditLabelCaption', '');
    btDownload.Caption := language.ReadString(lang, 'btDownloadCaption', '');
    stDownload := btDownload.Caption;
    btReadOnline.Caption := language.ReadString(lang, 'btReadOnlineCaption', '');
    cbAddAsStopped.Caption := language.ReadString(lang, 'cbAddAsStoppedCaption', '');
    cbAddToFavorites.Caption := language.ReadString(lang, 'cbAddToFavoritesCaption', '');
    cbSearchFromAllSites.Caption :=
      language.ReadString(lang, 'cbSearchFromAllSitesCaption', '');

    cbFilterStatus.Items.Strings[0] := language.ReadString(lang, 'lbFilterStatus0', '');
    cbFilterStatus.Items.Strings[1] := language.ReadString(lang, 'lbFilterStatus1', '');
    rbAll.Caption := language.ReadString(lang, 'rbAllCaption', '');
    rbOne.Caption := language.ReadString(lang, 'rbOneCaption', '');
    cbOnlynew.Caption := language.ReadString(lang, 'cbOnlyNewCaption', '');
    btFilter.Caption := language.ReadString(lang, 'btFilterCaption', '');
    btRemoveFilterLarge.Caption :=
      language.ReadString(lang, 'btRemoveFilterLargeCaption', '');

    btOptionApply.Caption := language.ReadString(lang, 'btOptionApplyCaption', '');
    tsGeneral.Caption := language.ReadString(lang, 'tsGeneralCaption', '');
    tsConnections.Caption := language.ReadString(lang, 'tsConnectionsCaption', '');
    tsUpdate.Caption := language.ReadString(lang, 'tsUpdateCaption', '');
    lbOptionPDFQualityHint.Caption := language.ReadString(lang, 'tsSaveToCaption', '');
    tsDialogs.Caption := language.ReadString(lang, 'tsDialogsCaption', '');
    tsWebsites.Caption := language.ReadString(lang, 'tsWebsitesCaption', '');
    tsMisc.Caption := language.ReadString(lang, 'tsMiscCaption', '');
    gbOptionProxy.Caption := language.ReadString(lang, 'gbOptionProxyCaption', '');
    gbOptionFavorites.Caption :=
      language.ReadString(lang, 'gbOptionFavoritesCaption', '');
    cbOptionUseProxy.Caption := language.ReadString(lang, 'cbOptionUseProxyCaption', '');
    cbOptionAutoCheckFavStartup.Caption :=
      language.ReadString(lang, 'cbOptionAutoCheckFavStartupCaption', '');
    edOptionDefaultPath.EditLabel.Caption :=
      language.ReadString(lang, 'edOptionDefaultPathEditLabelCaption', '');
    rgOptionCompress.Caption := language.ReadString(lang, 'rgOptionCompressCaption', '');
    gbOptionRenaming.Caption := language.ReadString(lang, 'gbOptionRenamingCaption', '');
    dlgSaveTo.Title := language.ReadString(lang, 'dlgSaveToTitle', '');

    miUp.Caption := language.ReadString(lang, 'miUp', '');
    miDown.Caption := language.ReadString(lang, 'miDown', '');
    miDownloadStop.Caption := language.ReadString(lang, 'miDownloadStopCaption', '');
    miDownloadResume.Caption := language.ReadString(lang, 'miDownloadStopResume', '');
    miDownloadRemove.Caption := language.ReadString(lang, 'miDownloadRemoveCaption', '');
    miDownloadRemoveFinishedTasks.Caption :=
      language.ReadString(lang, 'miDownloadRemoveFinishedTasksCaption', '');
    miDownloadMerge.Caption := language.ReadString(lang,
      'miDownloadMergeTasksCaption', '');
    miOpenFolder.Caption := language.ReadString(lang, 'miOpenFolder', '');
    miOpenWith.Caption := language.ReadString(lang, 'miOpenWith', '');

    miChapterListCheckSelected.Caption :=
      language.ReadString(lang, 'miChapterListCheckSelectedCaption', '');
    miChapterListUncheckSelected.Caption :=
      language.ReadString(lang, 'miChapterListUncheckSelectedCaption', '');
    miChapterListCheckAll.Caption :=
      language.ReadString(lang, 'miChapterListCheckAllCaption', '');
    miChapterListUncheckAll.Caption :=
      language.ReadString(lang, 'miChapterListUncheckAllCaption', '');

    miFavoritesRemove.Caption :=
      language.ReadString(lang, 'miFavoritesRemoveCaption', '');
    miFavoritesChangeCurrentChapter.Caption :=
      language.ReadString(lang, 'miFavoritesChangeCurrentChapterCaption', '');
    miFavoritesChangeSaveTo.Caption :=
      language.ReadString(lang, 'miFavoritesChangeSaveToCaption', '');
    miMangaListViewInfos.Caption :=
      language.ReadString(lang, 'miMangaListViewInfosCaption', '');
    miFavoritesViewInfos.Caption :=
      language.ReadString(lang, 'miMangaListViewInfosCaption', '');
    miMangaListDownloadAll.Caption :=
      language.ReadString(lang, 'miMangaListDownloadAllCaption', '');
    miMangaListAddToFavorites.Caption :=
      language.ReadString(lang, 'miMangaListAddToFavoritesCaption', '');
    miHighlightNewManga.Caption :=
      language.ReadString(lang, 'miHighlightNewMangaCaption', '');
    miChapterListHighlight.Caption :=
      language.ReadString(lang, 'miChapterListHighlightCaption', '');

    miDeleteTask.Caption := language.ReadString(lang, 'miDeleteTaskCaption', '');
    miDeleteTaskData.Caption := language.ReadString(lang, 'miDeleteTaskDataCaption', '');

    miOpenFolder2.Caption := miOpenFolder.Caption;
    miOpenWith2.Caption := miOpenWith.Caption;

    infoCustomGenres := language.ReadString(lang, 'infoCustomGenres', '');
    infoName := language.ReadString(lang, 'infoName', '');
    infoAuthors := language.ReadString(lang, 'infoAuthors', '');
    infoArtists := language.ReadString(lang, 'infoArtists', '');
    infoStatus := language.ReadString(lang, 'infoStatus', '');
    infoGenres := language.ReadString(lang, 'infoGenres', '');
    infoSummary := language.ReadString(lang, 'infoSummary', '');
    infoLink := language.ReadString(lang, 'infoLink', '');

    lbFilterCustomGenres.Caption := infoCustomGenres;
    lbFilterTitle.Caption := infoName;
    lbFilterAuthors.Caption := infoAuthors;
    lbFilterArtists.Caption := infoArtists;
    lbFilterStatus.Caption := infoStatus;
    lbFilterSummary.Caption := infoSummary;

    lbOptionAutoCheckMinutes.Caption :=
      Format(language.ReadString(lang, 'lbOptionAutoCheckMinutesCaption', ''),
      [seOptionCheckMinutes.Value]);
    OptionAutoCheckMinutes := lbOptionAutoCheckMinutes.Caption;
    lbOptionLanguage.Caption := language.ReadString(lang, 'lbOptionLanguageCaption', '');
    lbOptionNewMangaTime.Caption :=
      language.ReadString(lang, 'lbOptionNewMangaTimeCaption', '');
    lbOptionMaxParallel.Caption :=
      Format(language.ReadString(lang, 'lbOptionMaxParallelCaption', ''),
      [seOptionMaxParallel.MaxValue]);
    lbOptionMaxThread.Caption :=
      Format(language.ReadString(lang, 'lbOptionMaxThreadCaption', ''),
      [seOptionMaxThread.MaxValue]);
    lbOptionMaxRetry.Caption := language.ReadString(lang, 'lbOptionMaxRetryCaption', '');
    lbOptionDialogs.Caption := language.ReadString(lang, 'lbOptionDialogsCaption', '');
    lbOptionPDFQuality.Caption :=
      language.ReadString(lang, 'lbOptionPDFQualityCaption', '');
    lbOptionPDFQualityHint.Hint :=
      language.ReadString(lang, 'lbOptionPDFQualityHint', '');
    // seOptionPDFQuality.Hint     := lbOptionPDFQuality.Hint;
    lbOptionCustomRenameHint.Hint :=
      language.ReadString(lang, 'edOptionCustomRenameHint', '');
    lbOptionCustomRenameHint.Hint :=
      StringReplace(lbOptionCustomRenameHint.Hint, '\n', #10, [rfReplaceAll]);
    lbOptionCustomRenameHint.Hint :=
      StringReplace(lbOptionCustomRenameHint.Hint, '\r', #13, [rfReplaceAll]);
    lbOptionCustomRename.Hint :=
      language.ReadString(lang, 'lbOptionCustomRenameHint', '');

    cbOptionMinimizeToTray.Caption :=
      language.ReadString(lang, 'cbOptionMinimizeToTrayCaption', '');
    cbOptionAutoCheckUpdate.Caption :=
      language.ReadString(lang, 'cbOptionAutoCheckUpdateCaption', '');
    cbOptionPathConvert.Caption :=
      language.ReadString(lang, 'cbOptionPathConvertCaption', '');
    cbOptionGenerateChapterName.Caption :=
      language.ReadString(lang, 'cbOptionGenerateChapterNameCaption', '');
    cbOptionGenerateMangaFolderName.Caption :=
      language.ReadString(lang, 'cbOptionGenerateMangaFolderNameCaption', '');
    cbOptionShowQuitDialog.Caption :=
      language.ReadString(lang, 'cbOptionShowQuitDialogCaption', '');
    cbOptionShowDeleteTaskDialog.Caption :=
      language.ReadString(lang, 'cbOptionShowDeleteTaskDialogCaption', '');
    cbOptionShowFavoriteDialog.Caption :=
      language.ReadString(lang, 'cbOptionShowFavoriteDialogCaption', '');
    cbOptionBatotoUseIE.Caption :=
      language.ReadString(lang, 'cbOptionBatotoUseIECaption', '');
    cbOptionAutoNumberChapter.Caption :=
      language.ReadString(lang, 'cbOptionAutoNumberChapterCaption', '');
    cbOptionAutoCheckFavStartup.Caption :=
      language.ReadString(lang, 'cbOptionAutoCheckFavStartupCaption', '');
    cbOptionEnableLoadCover.Caption :=
      language.ReadString(lang, 'cbOptionEnableLoadCoverCaption', '');
    cbOptionAutoCheckFavStartup.Caption :=
      language.ReadString(lang, 'cbOptionAutoCheckFavStartupCaption', '');
    cbOptionShowBatotoSG.Caption :=
      language.ReadString(lang, 'cbOptionShowBatotoSGCaption', '');
    cbOptionShowAllLang.Caption :=
      language.ReadString(lang, 'cbOptionShowAllLangCaption', '');
    cbOptionAutoDlFav.Caption :=
      language.ReadString(lang, 'cbOptionAutoDlFavCaption', '');
    cbOptionAutoRemoveCompletedManga.Caption :=
      language.ReadString(lang, 'cbOptionAutoRemoveCompletedMangaCaption', '');
    cbSelectManga.Hint := language.ReadString(lang, 'cbSelectMangaHint', '');

    gbOptionExternal.Caption := language.ReadString(lang, 'gbOptionExternalCaption', '');
    lbOptionExternal.Caption := language.ReadString(lang, 'lbOptionExternalCaption', '');
    lbOptionExternalHint.Hint :=
      StringFilter(language.ReadString(lang, 'lbOptionExternalHint', ''));
    lbOptionExternalHint.Hint :=
      StringReplace(lbOptionExternalHint.Hint, '\n', #10, [rfReplaceAll]);
    lbOptionExternalHint.Hint :=
      StringReplace(lbOptionExternalHint.Hint, '\r', #13, [rfReplaceAll]);

    lbFilterHint.Hint := language.ReadString(lang, 'lbFilterHint', '');
    lbFilterHint.Hint := StringReplace(lbFilterHint.Hint, '\n', #10, [rfReplaceAll]);
    lbFilterHint.Hint := StringReplace(lbFilterHint.Hint, '\r', #13, [rfReplaceAll]);

    stDownloadManga := language.ReadString(lang, 'stDownloadManga', '');
    stDownloadStatus := language.ReadString(lang, 'stDownloadStatus', '');
    stDownloadProgress := language.ReadString(lang, 'stDownloadProgress', '');
    stDownloadWebsite := language.ReadString(lang, 'stDownloadWebsite', '');
    stDownloadSaveto := language.ReadString(lang, 'stDownloadSaveto', '');
    stDownloadAdded := language.ReadString(lang, 'stDownloadAdded', '');
    stFavoritesCurrentChapter :=
      language.ReadString(lang, 'stFavoritesCurrentChapter', '');
    stFavoritesHasNewChapter :=
      language.ReadString(lang, 'stFavoritesHasNewChapter', '');

    stFavoritesCheck := language.ReadString(lang, 'stFavoritesCheck', '');
    stFavoritesChecking := language.ReadString(lang, 'stFavoritesChecking', '');

    stImport := language.ReadString(lang, 'stImport', '');
    stImportList := language.ReadString(lang, 'stImportList', '');
    stImportCompleted := language.ReadString(lang, 'stImportCompleted', '');
    stSoftware := language.ReadString(lang, 'stSoftware', '');
    stSoftwarePath := language.ReadString(lang, 'stSoftwarePath', '');

    stUpdaterCheck := language.ReadString(lang, 'stUpdaterCheck', '');
    stSelected := language.ReadString(lang, 'stSelected', '');
    btCheckVersion.Caption := stUpdaterCheck;

    btFavoritesImport.Caption :=
      language.ReadString(lang, 'btFavoritesImportCaption', '');

    stOptionAutoCheckMinutesCaption :=
      language.ReadString(lang, 'OptionAutoCheckMinutesCaption', '');
    stIsCompressing := language.ReadString(lang, 'stIsCompressing', '');
    stDlgUpdaterVersionRequire :=
      language.ReadString(lang, 'stDlgUpdaterVersionRequire', '');
    stDlgUpdaterIsRunning := language.ReadString(lang, 'stDlgUpdaterIsRunning', '');
    stDlgLatestVersion := language.ReadString(lang, 'stDlgLatestVersion', '');
    stDlgNewVersion := language.ReadString(lang, 'stDlgNewVersion', '');
    stDlgURLNotSupport := language.ReadString(lang, 'stDlgURLNotSupport', '');
    stDldMangaListSelect := language.ReadString(lang, 'stDldMangaListSelect', '');
    stDlgUpdateAlreadyRunning :=
      language.ReadString(lang, 'stDlgUpdateAlreadyRunning', '');
    stDlgNewManga := language.ReadString(lang, 'stDlgNewManga', '');
    stDlgQuit := language.ReadString(lang, 'stDlgQuit', '');
    stDlgRemoveTask := language.ReadString(lang, 'stDlgRemoveTask', '');
    stDlgRemoveFinishTasks := language.ReadString(lang, 'stDlgRemoveFinishTasks', '');
    stDlgTypeInNewChapter := language.ReadString(lang, 'stDlgTypeInNewChapter', '');
    stDlgTypeInNewSavePath := language.ReadString(lang, 'stDlgTypeInNewSavePath', '');
    stDlgCannotGetMangaInfo := language.ReadString(lang, 'stDlgCannotGetMangaInfo', '');
    stDlgFavoritesIsRunning := language.ReadString(lang, 'stDlgFavoritesIsRunning', '');
    stDlgNoNewChapter := language.ReadString(lang, 'stDlgNoNewChapter', '');
    stDlgHasNewChapter := language.ReadString(lang, 'stDlgHasNewChapter', '');
    stDlgRemoveCompletedManga :=
      language.ReadString(lang, 'stDlgRemoveCompletedManga', '');
    stDlgUpdaterWantToUpdateDB :=
      language.ReadString(lang, 'stDlgUpdaterWantToUpdateDB', '');
    stDlgUpdaterCannotConnectToServer :=
      language.ReadString(lang, 'stDlgUpdaterCannotConnectToServer', '');

    lbOptionLetFMDDo.Caption := language.ReadString(lang, 'lbOptionLetFMDDoCaption', '');
    s := language.ReadString(lang, 'cbOptionLetFMDDo', '');

    // apply genres & descriptions
    for i := 0 to 37 do
    begin
      Genre[i] := defaultGenres[i];
      //language.ReadString(lang, StringReplace(StringReplace(defaultGenres[i], ' ', '', [rfReplaceAll]), '-', '', [rfReplaceAll])+'G', '')
      TCheckBox(pnGenres.Controls[i]).Caption := Genre[i];
      TCheckBox(pnGenres.Controls[i]).Hint :=
        language.ReadString(lang, StringReplace(StringReplace(defaultGenres[i],
        ' ', '', [rfReplaceAll]), '-', '', [rfReplaceAll]) + 'M', '');
      TCheckBox(pnGenres.Controls[i]).ShowHint := True;
    end;
  finally
    language.Free;
  end;

  // add information to cbOptionLetFMDDo
  cbOptionLetFMDDo.Items.Clear;
  GetParams(cbOptionLetFMDDo.Items, s);
  // cbOptionLetFMDDo.ItemIndex:= 0;


  if dataProcess.isFiltered then
    lbMode.Caption := Format(stModeFilter, [dataProcess.filterPos.Count])
  else
    lbMode.Caption := Format(stModeAll, [dataProcess.filterPos.Count]);

  // sync download table infos
  if DLManager.containers.Count > 0 then
  begin
    for i := 0 to DLManager.containers.Count - 1 do
    begin
      // if (DLManager.containers.Items[pos] <> nil) OR (NOT DLManager.containers.Items[pos].Thread.isTerminated) then
      case DLManager.containers.Items[i].Status of
        STATUS_STOP: DLManager.containers.Items[i].DownloadInfo.Status := stStop;
        STATUS_WAIT: DLManager.containers.Items[i].DownloadInfo.Status := stWait;
        STATUS_PREPARE: DLManager.containers.Items[i].DownloadInfo.Status := stPreparing;
        STATUS_DOWNLOAD: DLManager.containers.Items[i].DownloadInfo.Status :=
            stDownloading;
        STATUS_FINISH: DLManager.containers.Items[i].DownloadInfo.Status := stFinish;
      end;
    end;
  end;

  if FavoriteManager.isRunning then
    btFavoritesCheckNewChapter.Caption := stFavoritesChecking
  else
    btFavoritesCheckNewChapter.Caption := stFavoritesCheck;
  vtDownload.Header.Columns.Items[0].Text := stDownloadManga;
  vtDownload.Header.Columns.Items[1].Text := stDownloadStatus;
  vtDownload.Header.Columns.Items[2].Text := stDownloadProgress;
  vtDownload.Header.Columns.Items[3].Text := stDownloadWebsite;
  vtDownload.Header.Columns.Items[4].Text := stDownloadSaveto;
  vtDownload.Header.Columns.Items[5].Text := stDownloadAdded;

  vtFavorites.Header.Columns.Items[1].Text := stDownloadManga;
  vtFavorites.Header.Columns.Items[2].Text := stFavoritesCurrentChapter;
  vtFavorites.Header.Columns.Items[3].Text := stDownloadWebsite;
  vtFavorites.Header.Columns.Items[4].Text := stDownloadSaveto;

  vtDownload.Repaint;
end;

procedure TMainForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
  USimpleException.ExceptionHandle(Sender, E);
end;

procedure TMainForm.tmBackupTimer(Sender: TObject);
begin
  if not DLManager.isRunningBackup then
    DLManager.Backup;
end;

procedure TMainForm.vtOptionMangaSiteSelectionChange(Sender : TBaseVirtualTree;
  Node : PVirtualNode);
begin
  vtOptionMangaSiteSelection.Refresh;
end;

procedure TMainForm.vtOptionMangaSiteSelectionFocusChanged(
  Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex);
begin
  vtOptionMangaSiteSelection.Refresh;
end;

procedure TMainForm.vtOptionMangaSiteSelectionFreeNode(
  Sender : TBaseVirtualTree; Node : PVirtualNode);
var
  Data: PMangaListItem;
begin
  Data := vtOptionMangaSiteSelection.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TMainForm.vtOptionMangaSiteSelectionGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TMangaListItem);
end;

procedure TMainForm.vtOptionMangaSiteSelectionGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PMangaListItem;
begin
  Data := vtOptionMangaSiteSelection.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data^.Text;
end;

procedure TMainForm.vtOptionMangaSiteSelectionInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Level: Integer;
begin
  Level := vtOptionMangaSiteSelection.GetNodeLevel(Node);
  if Level = 1 then
    Node^.CheckType := ctCheckBox;
  vtOptionMangaSiteSelection.ValidateNode(Node, False);
end;

end.
