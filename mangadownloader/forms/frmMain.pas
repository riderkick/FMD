{
        File: frmMain.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  ActiveX, windows,
  {$else}
  FakeActiveX,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  ExtCtrls, ComCtrls, Buttons, Spin, Menus, VirtualTrees, RichMemo, IniFiles,
  simpleipc, lclproc, types, strutils, LCLIntf, DefaultTranslator, EditBtn,
  FileUtil, TAGraph, TASources, TASeries, AnimatedGif,
  uBaseUnit, uData, uDownloadsManager, uFavoritesManager, uUpdateThread,
  uUpdateDBThread, uSubThread, uSilentThread, uMisc, uGetMangaInfosThread,
  uTranslation, frmDropTarget, USimpleException, USimpleLogger;

type

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
    btWebsitesSearchClear: TSpeedButton;
    btUpdateList: TSpeedButton;
    btURL: TSpeedButton;
    cbOptionAutoDlFav: TCheckBox;
    cbOptionAutoRemoveCompletedManga: TCheckBox;
    cbOptionEnableLoadCover: TCheckBox;
    cbOptionShowDownloadToolbar: TCheckBox;
    cbOptionUpdateListNoMangaInfo: TCheckBox;
    cbOptionDigitVolume: TCheckBox;
    cbOptionDigitChapter: TCheckBox;
    cbOptionMangaFoxRemoveWatermarks: TCheckBox;
    cbOptionLiveSearch: TCheckBox;
    cbOptionUpdateListRemoveDuplicateLocalData : TCheckBox;
    cbUseRegExpr: TCheckBox;
    cbOptionProxyType: TComboBox;
    cbOptionOneInstanceOnly: TCheckBox;
    miFavoritesStopCheckNewChapter: TMenuItem;
    miFavoritesCheckNewChapter: TMenuItem;
    pnDownloadToolbarLeft: TPanel;
    pnDownloadToolbar: TPanel;
    TransferRateGraphArea: TAreaSeries;
    TransferRateGraph: TChart;
    ckDropTarget: TCheckBox;
    edOptionDefaultPath: TEdit;
    edOptionExternalParams: TEdit;
    edOptionExternalPath: TFileNameEdit;
    edSaveTo: TEdit;
    edWebsitesSearch: TEdit;
    edURL: TEdit;
    gbDropTarget: TGroupBox;
    gbOptionExternal: TGroupBox;
    IconDL: TImageList;
    IconMed: TImageList;
    IconSmall: TImageList;
    itMonitor: TTimer;
    itStartup: TIdleTimer;
    lbDefaultDownloadPath: TLabel;
    lbDropTargetOpacity: TLabel;
    lbOptionExternalParams: TLabel;
    lbOptionConnectionTimeout: TLabel;
    lbSaveTo: TLabel;
    lbOptionProxyType: TLabel;
    lbOptionRenameDigits: TLabel;
    lbFilterHint: TLabel;
    lbOptionExternal: TLabel;
    lbOptionCustomRenameHint: TLabel;
    lbOptionCustomRenameHint1: TLabel;
    lbOptionExternalParamsHint: TLabel;
    TransferRateGraphList: TListChartSource;
    medURLCut: TMenuItem;
    medURLCopy: TMenuItem;
    medURLPaste: TMenuItem;
    medURLPasteandgo: TMenuItem;
    medtURLDelete: TMenuItem;
    MenuItem15: TMenuItem;
    medURLSelectAll: TMenuItem;
    MenuItem17: TMenuItem;
    medURLUndo: TMenuItem;
    miFavoritesDownloadAll: TMenuItem;
    miDownloadViewMangaInfo: TMenuItem;
    MenuItem9: TMenuItem;
    miDownloadDeleteTask: TMenuItem;
    miDownloadDeleteTaskData: TMenuItem;
    miDownloadOpenWith: TMenuItem;
    miFavoritesOpenWith: TMenuItem;
    pnlWebsitesToolRight: TPanel;
    pnlWebsitesTool: TPanel;
    pnCustomGenre: TPanel;
    pnThumbContainer: TPanel;
    pnMainTop: TPanel;
    btVisitMyBlog: TBitBtn;
    btCheckVersion: TBitBtn;
    btFavoritesCheckNewChapter: TBitBtn;
    btDownload: TBitBtn;
    btRemoveFilterLarge: TBitBtn;
    cbOptionAutoCheckUpdate: TCheckBox;
    cbOptionShowDeleteTaskDialog: TCheckBox;
    cbOptionShowBatotoSG: TCheckBox;
    cbOptionShowAllLang: TCheckBox;
    cbOptionUseProxy: TCheckBox;
    cbSelectManga: TComboBox;
    ckFilterAction: TCheckBox;
    ckFilterHarem: TCheckBox;
    ckFilterHentai: TCheckBox;
    ckFilterHistorical: TCheckBox;
    ckFilterHorror: TCheckBox;
    ckFilterJosei: TCheckBox;
    ckFilterLolicon: TCheckBox;
    ckFilterMartialArts: TCheckBox;
    ckFilterMature: TCheckBox;
    ckFilterMecha: TCheckBox;
    ckFilterMusical: TCheckBox;
    ckFilterAdult: TCheckBox;
    ckFilterMystery: TCheckBox;
    ckFilterPsychological: TCheckBox;
    ckFilterRomance: TCheckBox;
    ckFilterSchoolLife: TCheckBox;
    ckFilterSciFi: TCheckBox;
    ckFilterSeinen: TCheckBox;
    ckFilterShotacon: TCheckBox;
    ckFilterShoujo: TCheckBox;
    ckFilterShoujoAi: TCheckBox;
    ckFilterShounen: TCheckBox;
    ckFilterAdventure: TCheckBox;
    ckFilterShounenAi: TCheckBox;
    ckFilterSliceofLife: TCheckBox;
    ckFilterSmut: TCheckBox;
    ckFilterSports: TCheckBox;
    ckFilterSupernatural: TCheckBox;
    ckFilterTragedy: TCheckBox;
    ckFilterYaoi: TCheckBox;
    ckFilterYuri: TCheckBox;
    ckFilterWeebtons: TCheckBox;
    ckFilterComedy: TCheckBox;
    cbOnlyNew: TCheckBox;
    cbAddAsStopped: TCheckBox;
    cbOptionShowQuitDialog: TCheckBox;
    cbOptionPathConvert: TCheckBox;
    cbOptionGenerateChapterName: TCheckBox;
    cbOptionGenerateMangaFolderName: TCheckBox;
    cbOptionMinimizeToTray: TCheckBox;
    cbOptionAutoNumberChapter: TCheckBox;
    cbOptionAutoCheckFavStartup: TCheckBox;
    cbSearchFromAllSites: TCheckBox;
    ckFilterDoujinshi: TCheckBox;
    ckFilterDrama: TCheckBox;
    ckFilterEchi: TCheckBox;
    ckFilterFantasy: TCheckBox;
    ckFilterGenderBender: TCheckBox;
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
    edSearch: TEdit;
    gbDialogs: TGroupBox;
    gbOptionProxy: TGroupBox;
    gbOptionRenaming: TGroupBox;
    gbOptionFavorites: TGroupBox;
    gbMisc: TGroupBox;
    IconList: TImageList;
    itSaveDownloadedList: TIdleTimer;
    itRefreshDLInfo: TIdleTimer;
    itCheckForChapters: TIdleTimer;
    itAnimate: TIdleTimer;
    imCover: TImage;
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
    miDownloadMergeCompleted: TMenuItem;
    miFavoritesOpenFolder: TMenuItem;
    miHighlightNewManga: TMenuItem;
    miI2: TMenuItem;
    miDownloadOpenFolder: TMenuItem;
    miFavoritesDelete: TMenuItem;
    miMangaListAddToFavorites: TMenuItem;
    miFavoritesChangeCurrentChapter: TMenuItem;
    miFavoritesChangeSaveTo: TMenuItem;
    miDownloadDeleteCompleted: TMenuItem;
    miDownloadStop: TMenuItem;
    miChapterListCheckAll: TMenuItem;
    miChapterListUncheckSelected: TMenuItem;
    miChapterListCheckSelected: TMenuItem;
    miI1: TMenuItem;
    miDownloadResume: TMenuItem;
    miDownloadDelete: TMenuItem;
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
    rgDropTargetMode: TRadioGroup;
    rgOptionCompress: TRadioGroup;
    rmAbout: TRichMemo;
    rmInformation: TRichMemo;
    sbFilter: TScrollBox;
    sbInformation: TScrollBox;
    sbDownloadConnections: TScrollBox;
    dlgSaveTo: TSelectDirectoryDialog;
    seOptionMaxParallel: TSpinEdit;
    seOptionMaxRetry: TSpinEdit;
    seOptionConnectionTimeout: TSpinEdit;
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
    tbDropTargetOpacity: TTrackBar;
    tbWebsitesCollapseAll: TToolButton;
    tbWebsitesExpandAll: TToolButton;
    ToolBarWebsites: TToolBar;
    tsView: TTabSheet;
    tmBackup: TIdleTimer;
    ToolBarDownload: TToolBar;
    tbDownloadResumeAll: TToolButton;
    tbDownloadStopAll: TToolButton;
    ToolButton1: TToolButton;
    tbDownloadDeleteCompleted: TToolButton;
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
    tsSaveTo: TTabSheet;
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
    procedure btWebsitesSearchClearClick(Sender: TObject);
    procedure cbOptionDigitChapterChange(Sender: TObject);
    procedure cbOptionDigitVolumeChange(Sender: TObject);
    procedure cbSelectMangaChange(Sender: TObject);
    procedure ckDropTargetChange(Sender: TObject);
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
    procedure edURLKeyPress(Sender: TObject; var Key: Char);
    procedure edWebsitesSearchChange(Sender: TObject);
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
    procedure itRefreshDLInfoStartTimer(Sender: TObject);
    procedure itRefreshDLInfoStopTimer(Sender: TObject);
    procedure itRefreshDLInfoTimer(Sender: TObject);
    procedure itSaveDownloadedListTimer(Sender: TObject);
    procedure itStartupTimer(Sender: TObject);
    procedure medURLCutClick(Sender: TObject);
    procedure medURLCopyClick(Sender: TObject);
    procedure medURLPasteClick(Sender: TObject);
    procedure medURLPasteandgoClick(Sender: TObject);
    procedure medtURLDeleteClick(Sender: TObject);
    procedure medURLSelectAllClick(Sender: TObject);
    procedure medURLUndoClick(Sender: TObject);
    procedure miDownloadViewMangaInfoClick(Sender: TObject);
    procedure miChapterListHighlightClick(Sender: TObject);
    procedure miDownloadDeleteTaskClick(Sender: TObject);
    procedure miDownloadMergeCompletedClick(Sender: TObject);
    procedure miFavoritesCheckNewChapterClick(Sender: TObject);
    procedure miFavoritesDownloadAllClick(Sender: TObject);
    procedure miFavoritesStopCheckNewChapterClick(Sender: TObject);
    procedure miFavoritesViewInfosClick(Sender: TObject);
    procedure miHighlightNewMangaClick(Sender: TObject);

    procedure miFavoritesDeleteClick(Sender: TObject);
    procedure miMangaListAddToFavoritesClick(Sender: TObject);
    procedure miFavoritesChangeCurrentChapterClick(Sender: TObject);
    procedure miFavoritesChangeSaveToClick(Sender: TObject);

    procedure miChapterListCheckSelectedClick(Sender: TObject);
    procedure miChapterListUncheckSelectedClick(Sender: TObject);
    procedure miChapterListCheckAllClick(Sender: TObject);
    procedure miChapterListUncheckAllClick(Sender: TObject);

    procedure miDownloadDeleteCompletedClick(Sender: TObject);
    procedure miDownloadResumeClick(Sender: TObject);
    procedure miDownloadStopClick(Sender: TObject);
    procedure miMangaListDownloadAllClick(Sender: TObject);
    procedure miMangaListViewInfosClick(Sender: TObject);
    procedure miFavoritesOpenFolderClick(Sender: TObject);
    procedure miDownloadOpenFolderClick(Sender: TObject);
    procedure miFavoritesOpenWithClick(Sender: TObject);
    procedure miDownloadOpenWithClick(Sender: TObject);
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
    procedure tbDownloadDeleteCompletedClick(Sender: TObject);
    procedure tbDownloadResumeAllClick(Sender: TObject);
    procedure tbDownloadStopAllClick(Sender: TObject);
    procedure tbDropTargetOpacityChange(Sender: TObject);
    procedure tbWebsitesCollapseAllClick(Sender: TObject);
    procedure tbWebsitesExpandAllClick(Sender: TObject);
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
    procedure vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtMangaListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtMangaListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
    procedure ClearChapterListState;
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
    isUpdating: Boolean;
    revisionIni, updates, mangalistIni, options: TIniFile;
    FavoriteManager: TFavoriteManager;
    dataProcess: TDBDataProcess;
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

    procedure CloseNow(WaitFor: Boolean = True);

    procedure CheckForTopPanel;
    // en: Too lazy to add it one by one
    procedure InitCheckboxes;

    // download task filters
    procedure ShowTasks(Status: TDownloadStatusTypes = []);

    procedure ShowTasksOnCertainDays(const L, H: longint);
    procedure ShowTodayTasks;
    procedure ShowYesterdayTasks;
    procedure ShowOneWeekTasks;
    procedure ShowOneMonthTasks;
    procedure vtDownloadFilters;

    procedure AddChapterNameToList;

    // Create silent thread
    procedure AddSilentThread(URL: string);

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
    procedure SaveDropTargetFormInformation;

    // load language file
    procedure LoadLanguage;

    // openwith
    procedure OpenWithExternalProgram(const dirPath, Filename: String);

    //transfer rate graph
    procedure TransferRateGraphInit(xCount: Integer = 10);
    procedure TransferRateGraphAddItem(TransferRate: Integer);

    // exception handle
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    { public declarations }
  end;

var
  //Instance
  FMDInstance: TSimpleIPCServer;

  MainForm: TMainForm;
  INIAdvanced: TIniFileR;

  // update fmd through main thread
  DoAfterFMD: TFMDDo;
  FUpdateURL: String;

const
  CL_HLBlueMarks = $FDC594;
  CL_HLGreenMarks = $B8FFB8;
  CL_HLRedMarks = $008080FF;

resourcestring
  RS_FilterStatusItems = 'Completed'#13#10'Ongoing'#13#10'<none>';
  RS_OptionFMDDoItems = 'Do nothing'#13#10'Exit FMD'#13#10'Shutdown'#13#10'Hibernate';
  RS_DropTargetModeItems = 'Download all'#13#10'Add to favorites';

  RS_HintFavoriteProblem = 'There is a problem with this data!'#13#10
                         + 'Removing and re-adding this data may fix the problem.';
  RS_DlgTitleExistInDLlist = 'This title are already in download list.'#13#10
                           + 'Do you want to download it anyway?';
  RS_DlgQuit = 'Are you sure you want to exit?';
  RS_DlgRemoveTask = 'Are you sure you want to delete the task(s)?';
  RS_DlgRemoveFavorite = 'Are you sure you want to delete the favorite(s)?';
  RS_DlgURLNotSupport = 'URL not supported!';
  RS_DlgUpdaterIsRunning = 'Updater is running!';
  RS_DlgTypeInNewChapter = 'Type in new chapter:';
  RS_DlgTypeInNewSavePath = 'Type in new save path:';
  RS_DlgUpdaterWantToUpdateDB = 'Do you want to download manga list from the server?';
  RS_DlgRemoveFinishTasks = 'Are you sure you want to delete all finished tasks?';
  RS_DlgMangaListSelect = 'You must choose at least 1 manga website!';
  RS_DlgCannotGetMangaInfo = 'Cannot get manga info. Please check your internet connection and try it again.';
  RS_DlgCannotConnectToServer = 'Cannot connect to the server.';
  RS_LblOptionExternalParamsHint = '%s : Path to the manga'#13#10+
                                   '%s : Chapter filename'#13#10+
                                   #13#10+
                                   'Example : "%s%s"';
  RS_LblAutoCheckNewChapterMinute = 'Auto check for new chapter every %d minutes';
  RS_BtnOK = '&OK';
  RS_Loading = 'Loading ...';
  RS_Checking = 'Checking...';
  RS_AllDownloads = 'All downloads';
  RS_InProgress = 'In progress';
  RS_History = 'History';
  RS_Today = 'Today';
  RS_Yesterday = 'Yesterday';
  RS_OneWeek = 'One week';
  RS_OneMonth = 'One month';
  RS_Import = 'Import';
  RS_Software = 'Software';
  RS_SoftwarePath = 'Path to the software (e.g. C:\MangaDownloader)';
  RS_Cancel = 'Cancel';
  RS_ModeAll = 'Mode: Show all (%d)';
  RS_ModeFiltered = 'Mode: Filtered (%d)';
  RS_Selected = 'Selected: %d';
  RS_InfoWebsite = 'Website:';
  RS_InfoTitle = 'Title:';
  RS_InfoAuthors = 'Author(s):';
  RS_InfoArtists = 'Artist(s):';
  RS_InfoGenres = 'Genre(s):';
  RS_InfoStatus = 'Status:';
  RS_InfoSummary = 'Summary:';

implementation

{$R *.lfm}

uses
  frmImportFavorites, RegExpr, Clipbrd, LazFileUtils, LazUTF8;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  fs: TFileStream;
begin
  Randomize;
  fmdDirectory := CleanAndExpandDirectory(GetCurrentDirUTF8);
  SetLogFile(Format('%s\%s_LOG_%s.txt', ['log', ExtractFileNameOnly(ParamStrUTF8(0)),
    FormatDateTime('dd-mm-yyyy', Now)]));
  Writelog_I('Starting ' + AnsiQuotedStr(Application.Title, '"'));
  InitSimpleExceptionHandler;
  AddIgnoredException('EImagingError');
  AddIgnoredException('ERegExpr');
  SilentThreadManager := TSilentThreadManager.Create;
  btAbortUpdateList.Parent := sbUpdateList;
  INIAdvanced := TIniFileR.Create(fmdDirectory + CONFIG_FOLDER + CONFIG_ADVANCED);
  isRunDownloadFilter := False;
  isUpdating := False;
  isExiting := False;
  isSubthread := False;
  isGetMangaInfos := False;
  DoAfterFMD := DO_NOTHING;
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
    try
      rmAbout.LoadRichText(fs);
    finally
      fs.free;
    end;
  end;

  dataProcess := TDBDataProcess.Create;
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
  options.StripQuotes := False;

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
  if cbFilterStatus.Items.Count > 2 then
    cbFilterStatus.ItemIndex := 2;

  // ShowInformation;
  mangaInfo := TMangaInfo.Create;

  vtDownload.NodeDataSize := SizeOf(TDownloadInfo) - 4;
  vtDownload.RootNodeCount := DLManager.Count;

  vtFavorites.NodeDataSize := SizeOf(TFavoriteInfo);
  UpdateVtFavorites;

  InitCheckboxes;

  pcMain.ActivePage := tsDownload;

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
  currentJDN := GetCurrentJDN;

  // read online
  btDownload.Enabled := False;
  btReadOnline.Enabled := False;
  btAddToFavorites.Enabled := False;

  // subthread
  SubThread := TSubThread.Create;

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
  if DLManager.Count > 1 then
  begin
    DLManager.SortDirection := Boolean(vtDownload.Header.SortDirection);
    vtDownload.Repaint;
  end;
  if FavoriteManager.Count > 0 then
  begin
    FavoriteManager.SortDirection := Boolean(vtFavorites.Header.SortDirection);
    FavoriteManager.Sort(vtFavorites.Header.SortColumn);
    vtFavorites.Repaint;
  end;
  uTranslation.LangDir := GetCurrentDirUTF8 + PathDelim + 'languages';
  uTranslation.LangAppName := 'fmd';
  LoadLanguage;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if cbOptionShowQuitDialog.Checked and (DoAfterFMD = DO_NOTHING) then
  begin
    if MessageDlg('', RS_DlgQuit, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      CloseAction := caNone;
      Exit;
    end;
  end;
  CloseNow;
  CloseAction := caFree;
end;

procedure TMainForm.CloseNow(WaitFor: Boolean);
begin
  if Assigned(FormDropTarget) then
    FormDropTarget.Close;
  tmBackup.Enabled := False;
  itSaveDownloadedList.Enabled := False;
  itRefreshDLInfo.Enabled := False;
  itCheckForChapters.Enabled := False;
  itAnimate.Enabled := False;
  itStartup.Enabled := False;
  itMonitor.Enabled := False;

  //Terminating all threads and wait for it
  if isGetMangaInfos then
  begin
    GetInfosThread.IsFlushed := True;
    GetInfosThread.Terminate;
    if WaitFor then
      GetInfosThread.WaitFor;
  end;
  if isSubthread then
  begin
    SubThread.Terminate;
    if WaitFor then
      SubThread.WaitFor;
  end;
  if isUpdating then
  begin
    updateList.Terminate;
    if WaitFor then
      updateList.WaitFor;
  end;
  FavoriteManager.StopChekForNewChapter(WaitFor);
  SilentThreadManager.StopAll(WaitFor);
  DLManager.StopAllDownloadTasksForExit;

  if FMDInstance <> nil then
  begin
    FMDInstance.StopServer;
    FreeAndNil(FMDInstance);
  end;

  //Backup data
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
  Writelog_I(AnsiQuotedStr(Application.Title, '"') + ' exit normally');
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
  if DLManager.isDlgCounter then Exit;
  if options.ReadBool('update', 'AutoCheckUpdate', True) then
    SubThread.CheckUpdate := True;
  FavoriteManager.isAuto := True;
  FavoriteManager.CheckForNewChapter;
end;

procedure TMainForm.itMonitorTimer(Sender: TObject);
begin
  if DoAfterFMD <> DO_NOTHING then
  begin
    itMonitor.Enabled := False;
    Self.CloseNow(False);
    case DoAfterFMD of
      DO_POWEROFF: fmdPowerOff;
      DO_HIBERNATE: fmdHibernate;
      DO_UPDATE:
      begin
        if FileExistsUTF8(fmdDirectory + 'updater.exe') then
          CopyFile(fmdDirectory + 'updater.exe', fmdDirectory + 'old_updater.exe');
        if FileExistsUTF8(fmdDirectory + 'old_updater.exe') then
        begin
          RunExternalProcess(fmdDirectory + 'old_updater.exe',
            ['-x', '-r', '3', '-a', FUpdateURL, '-l', Application.ExeName,
             '--lang', uTranslation.LastSelected], True, False);
          Self.Close;
        end;
      end;
    end;
    Self.Close;
  end;
end;

procedure TMainForm.itRefreshDLInfoStartTimer(Sender: TObject);
begin
  if Assigned(DLManager) then
  begin
    TransferRateGraphInit(round(TransferRateGraph.Width/4));
    TransferRateGraph.Visible := True;
  end;
end;

procedure TMainForm.itRefreshDLInfoStopTimer(Sender: TObject);
begin
  if Assigned(DLManager) then
  begin
    DLManager.ClearTransferRate;
    TransferRateGraph.Visible := False;
  end;
  vtDownload.Repaint;
end;

procedure TMainForm.itRefreshDLInfoTimer(Sender: TObject);
begin
  if Assigned(DLManager) then
    TransferRateGraphAddItem(DLManager.TransferRate);
  vtDownload.Repaint;
end;

procedure TMainForm.itSaveDownloadedListTimer(Sender: TObject);
begin
  DLManager.BackupDownloadedChaptersList;
end;

procedure TMainForm.itStartupTimer(Sender: TObject);
begin
  itStartup.Enabled := False;
  if not isStartup then
  begin
    Screen.Cursor := crHourGlass;
    isStartup := True;
    try
      if cbSelectManga.ItemIndex > -1 then
      begin
        vtMangaList.Clear;
        dataProcess.Open(cbSelectManga.Items[cbSelectManga.ItemIndex]);
      end;
      vtMangaList.RootNodeCount := dataProcess.RecordCount;
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
      dataProcess.Refresh;
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
  edURL.Text := Clipboard.AsText;
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

procedure TMainForm.miDownloadViewMangaInfoClick(Sender: TObject);
begin
  if vtDownload.Focused then
    with DLManager.TaskItem(vtDownload.FocusedNode^.Index) do begin
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
  if Length(ChapterList) > 0 then
  begin
    if miChapterListHighlight.Checked then
      DLManager.GetDownloadedChaptersState(mangaInfo.website + mangaInfo.link,
        ChapterList)
    else
      ClearChapterListState;
    clbChapterList.Repaint;
  end;
end;

procedure TMainForm.miDownloadDeleteTaskClick(Sender: TObject);
var
  i, j: Integer;
  xNode: PVirtualNode;
  f: String;
  finfo: TSearchRec;
  fs: TStringList;
begin
  if vtDownload.SelectedCount = 0 then Exit;
  if DLManager.Count = 0 then Exit;
  if (cbOptionShowDeleteTaskDialog.Checked) then
    if MessageDlg('', RS_DlgRemoveTask,
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  DLManager.CS_DownloadManager_Task.Acquire;
  try
    i:=0;
    xNode := vtDownload.GetFirst;
    while i < DLManager.Count do
    begin
      if vtDownload.Selected[xNode] then
      begin
        if Sender = miDownloadDeleteTaskData then
        begin
          DLManager.StopTask(i, True, False);
          if DLManager.TaskItem(i).ChapterName.Count > 0 then
          begin
            for j := 0 to DLManager.TaskItem(i).ChapterName.Count-1 do
            begin
              f := CleanAndExpandDirectory(DLManager.TaskItem(i).DownloadInfo.SaveTo) +
                DLManager.TaskItem(i).ChapterName[j];
              if FileExistsUTF8(f + '.zip') then
                DeleteFileUTF8(f + '.zip')
              else if FileExistsUTF8(f + '.cbz') then
                DeleteFileUTF8(f + '.cbz')
              else if FileExistsUTF8(f + '.pdf') then
                DeleteFileUTF8(f + '.pdf')
              else if DirectoryExistsUTF8(f) then
                DeleteDirectory(f, False);
            end;
          end;
          f := CleanAndExpandDirectory(DLManager.TaskItem(i).DownloadInfo.SaveTo);
          fs := TStringList.Create;
          try
            if FindFirstUTF8(f + '*', faAnyFile and faDirectory, finfo) = 0 then
            repeat
              fs.Add(finfo.Name);
            until FindNextUTF8(finfo) <> 0;
            FindCloseUTF8(finfo);
            if fs.Count = 2 then
              DeleteDirectory(f, False);
          finally
            fs.Free;
          end;
        end;
        DLManager.RemoveTask(i);
      end
      else
        Inc(i);
      xNode := vtDownload.GetNext(xNode);
    end;
  finally
    DLManager.CS_DownloadManager_Task.Release;
  end;
  vtDownload.ClearSelection;
  DLManager.CheckAndActiveTask;
  UpdateVtDownload;
  DLManager.Backup;
end;

procedure TMainForm.miDownloadMergeCompletedClick(Sender: TObject);
var
  i, j: Cardinal;
  // merge all finished tasks that have same manga name, website and directory
begin
  i := DLManager.Count - 1;
  while i > 0 do
  begin
    if DLManager.TaskItem(i).Status = STATUS_FINISH then
    begin
      j := i - 1;
      while j > 0 do
      begin
        if (i <> j) and
          (DLManager.TaskItem(j).Status = STATUS_FINISH) and
          SameText(DLManager.TaskItem(i).DownloadInfo.title,
          DLManager.TaskItem(j).DownloadInfo.title) and
          SameText(DLManager.TaskItem(i).DownloadInfo.website,
          DLManager.TaskItem(j).DownloadInfo.website) and
          SameText(DLManager.TaskItem(i).DownloadInfo.saveTo,
          DLManager.TaskItem(j).DownloadInfo.saveTo) then
        begin
          DLManager.TaskItem(i).ChapterLinks.Text :=
            DLManager.TaskItem(j).ChapterLinks.Text +
            DLManager.TaskItem(i).ChapterLinks.Text;
          DLManager.TaskItem(i).ChapterName.Text :=
            DLManager.TaskItem(j).ChapterName.Text +
            DLManager.TaskItem(i).ChapterName.Text;
          DLManager.TaskItem(i).DownloadInfo.dateTime :=
            DLManager.TaskItem(j).DownloadInfo.dateTime;
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

procedure TMainForm.miFavoritesCheckNewChapterClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount > 0 then
  begin
    xNode := vtFavorites.GetFirstSelected;
    repeat
      if Assigned(xNode) then
      begin
        FavoriteManager.CheckForNewChapter(xNode^.Index);
        xNode := vtFavorites.GetNextSelected(xNode);
      end;
    until xNode = nil;
    vtFavorites.Repaint;
  end;
end;

procedure TMainForm.miFavoritesDownloadAllClick(Sender: TObject);
var
  i: Integer;
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then
    Exit;
  try
    xNode := vtFavorites.GetFirstSelected;
    for i := 0 to vtFavorites.SelectedCount - 1 do
    begin
      if vtFavorites.Selected[xNode] then
        with FavoriteManager.FavoriteItem(xNode^.Index).FavoriteInfo do
          SilentThreadManager.Add(MD_DownloadAll, Website, Title, Link, SaveTo);
      xNode := vtFavorites.GetNextSelected(xNode);
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
end;

procedure TMainForm.miFavoritesStopCheckNewChapterClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount > 0 then
  begin
    xNode := vtFavorites.GetFirstSelected;
    repeat
      if Assigned(xNode) then
      begin
        FavoriteManager.StopChekForNewChapter(False, xNode^.Index);
        xNode := vtFavorites.GetNextSelected(xNode);
      end;
    until xNode = nil;
    vtFavorites.Repaint;
  end;
end;

procedure TMainForm.miFavoritesViewInfosClick(Sender: TObject);
var
  title, website, link: String;
begin
  if (not vtFavorites.Focused) then
    Exit;
  btDownload.Enabled := False;
  pcMain.ActivePage := tsInformation;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add('Loading ...');
  clbChapterList.Clear;

  website := FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.Website;
  link := FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.link;
  title := FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.Title;

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
  LFailedTask: Cardinal = 0;
begin
  if (Assigned(DLManager)) and (DLManager.Count > 0) then
    for i := 0 to DLManager.Count - 1 do
    begin
      case DLManager.TaskItem(i).Status of
        STATUS_FINISH: Inc(LFinishedTasks);
        STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT: Inc(LInProgressTasks);
        STATUS_STOP: Inc(LStoppedTasks);
        STATUS_PROBLEM, STATUS_FAILED: Inc(LFailedTask);
      end;
    end;

  // root
  tvDownloadFilter.Items[0].Text :=
    Format('%s (%d)', [RS_AllDownloads, vtDownload.RootNodeCount]);

  // childs
  tvDownloadFilter.Items[1].Text := Format('%s (%d)', [RS_Finish, LFinishedTasks]);
  tvDownloadFilter.Items[2].Text := Format('%s (%d)', [RS_InProgress, LInProgressTasks]);
  tvDownloadFilter.Items[3].Text := Format('%s (%d)', [RS_Stopped, LStoppedTasks]);
  tvDownloadFilter.Items[4].Text := Format('%s (%d)', [RS_Failed, LFailedTask]);

  // root
  tvDownloadFilter.Items[5].Text := RS_History;

  // childs
  tvDownloadFilter.Items[6].Text := RS_Today;
  tvDownloadFilter.Items[7].Text := RS_Yesterday;
  tvDownloadFilter.Items[8].Text := RS_OneWeek;
  tvDownloadFilter.Items[9].Text := RS_OneMonth;
end;

procedure TMainForm.GenerateNodes;
begin
  with tvDownloadFilter do begin
    Items.Clear;

    // root
    Items.Add(nil, RS_AllDownloads);
    Items[0].ImageIndex := 4;
    Items[0].SelectedIndex := 4;
    Items[0].StateIndex := 4;

    // childs
    Items.AddChild(tvDownloadFilter.Items[0], RS_Finish);
    Items[1].ImageIndex := 5;
    Items[1].SelectedIndex := 5;
    Items[1].StateIndex := 5;
    Items.AddChild(tvDownloadFilter.Items[0], RS_InProgress);
    Items[2].ImageIndex := 6;
    Items[2].SelectedIndex := 6;
    Items[2].StateIndex := 6;
    Items.AddChild(tvDownloadFilter.Items[0], RS_Stopped);
    Items[3].ImageIndex := 7;
    Items[3].SelectedIndex := 7;
    Items[3].StateIndex := 7;
    Items.AddChild(tvDownloadFilter.Items[0], RS_Failed);
    Items[4].ImageIndex := 16;
    Items[4].SelectedIndex := 16;
    Items[4].StateIndex := 16;

    // root
    Items.Add(nil, RS_History);
    Items[5].ImageIndex := 4;
    Items[5].SelectedIndex := 4;
    Items[5].StateIndex := 4;

    // childs
    Items.AddChild(tvDownloadFilter.Items[5], RS_Today);
    Items[6].ImageIndex := 8;
    Items[6].SelectedIndex := 8;
    Items[6].StateIndex := 8;
    Items.AddChild(tvDownloadFilter.Items[5], RS_Yesterday);
    Items[7].ImageIndex := 8;
    Items[7].SelectedIndex := 8;
    Items[7].StateIndex := 8;
    Items.AddChild(tvDownloadFilter.Items[5], RS_OneWeek);
    Items[8].ImageIndex := 8;
    Items[8].SelectedIndex := 8;
    Items[8].StateIndex := 8;
    Items.AddChild(tvDownloadFilter.Items[5], RS_OneMonth);
    Items[9].ImageIndex := 8;
    Items[9].SelectedIndex := 8;
    Items[9].StateIndex := 8;

    Items[Self.options.ReadInteger('general', 'DownloadFilterSelect',0)].Selected := True;
  end;
end;

procedure TMainForm.btDownloadClick(Sender: TObject);
var
  s: String;
  i, pos: Integer;
  isCreate: Boolean = False;
  xNode: PVirtualNode;
begin
  if clbChapterList.CheckedCount = 0 then
    Exit;
  Pos := -1;
  xNode := clbChapterList.GetFirstChecked;
  for i := 0 to clbChapterList.CheckedCount - 1 do
  begin
    if xNode^.CheckState = csCheckedNormal then
    begin
      if not isCreate then
      begin
        pos := DLManager.AddTask;
        isCreate := True;
      end;
      DLManager.TaskItem(pos).MangaSiteID := GetMangaSiteID(mangaInfo.website);
      // generate folder name
      s := CustomRename(OptionCustomRename,
        mangaInfo.website,
        mangaInfo.title,
        mangaInfo.authors,
        mangaInfo.artists,
        mangaInfo.chapterName.Strings[xNode^.Index],
        Format('%.4d', [xNode^.Index + 1]),
        cbOptionPathConvert.Checked);
      DLManager.TaskItem(pos).ChapterName.Add(s);
      DLManager.TaskItem(pos).ChapterLinks.Add(
        mangaInfo.chapterLinks.Strings[xNode^.Index]);
      ChapterList[xNode^.Index].Downloaded := True;
      clbChapterList.ReinitNode(xNode, False);
    end;
    xNode := clbChapterList.GetNextChecked(xNode);
  end;
  if not isCreate then
    Exit;
  if cbAddAsStopped.Checked then
  begin
    DLManager.TaskItem(pos).DownloadInfo.Status := RS_Stopped;
    DLManager.TaskItem(pos).Status := STATUS_STOP;
  end
  else
  begin
    DLManager.TaskItem(pos).DownloadInfo.Status := RS_Waiting;
    DLManager.TaskItem(pos).Status := STATUS_WAIT;
  end;
  DLManager.TaskItem(pos).CurrentDownloadChapterPtr := 0;
  DLManager.TaskItem(pos).DownloadInfo.Website := mangaInfo.website;
  DLManager.TaskItem(pos).DownloadInfo.Link := mangaInfo.url;
  DLManager.TaskItem(pos).DownloadInfo.Title := mangaInfo.title;
  DLManager.TaskItem(pos).DownloadInfo.DateTime := Now;

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
  DLManager.TaskItem(pos).DownloadInfo.SaveTo := s;
  UpdateVtDownload;

  DLManager.Backup;
  DLManager.CheckAndActiveTask;
  DLManager.AddToDownloadedChaptersList(
    mangaInfo.website + mangaInfo.link, DLManager.TaskItem(pos).ChapterLinks);
  FavoriteManager.AddToDownloadedChaptersList(
    mangaInfo.website, mangaInfo.link, DLManager.TaskItem(pos).ChapterLinks);
  clbChapterList.Repaint;
  pcMain.ActivePage := tsDownload;
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
  FavoriteManager.StopChekForNewChapter(False);
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
  FavoriteManager.CheckForNewChapter;
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
  btAddToFavorites.Enabled := not SitesWithoutFavorites(webs);
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

procedure TMainForm.ClearChapterListState;
var
  i: Integer;
begin
  if Length(ChapterList) > 0 then
    for i := Low(ChapterList) to High(ChapterList) do
      ChapterList[i].Downloaded := False;
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
    MessageDlg('', RS_DlgURLNotSupport, mtInformation, [mbYes], 0);
    Exit;
  end;

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

  pcMain.ActivePage := tsInformation;
  imCover.Picture.Assign(nil);
  clbChapterList.Clear;
  if Assigned(gifWaiting) then
  begin
    itAnimate.Enabled := True;
    pbWait.Visible := True;
  end;
  btAddToFavorites.Enabled := not SitesWithoutFavorites(website);
  rmInformation.Clear;
  rmInformation.Lines.Add(RS_Loading);
end;

procedure TMainForm.btVisitMyBlogClick(Sender: TObject);
begin
  OpenURL('http://akarink.wordpress.com/');
end;

procedure TMainForm.btWebsitesSearchClearClick(Sender: TObject);
begin
  edWebsitesSearch.Clear;
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
  if SubThread.CheckUpdate then
    MessageDlg('', RS_DlgUpdaterIsRunning, mtInformation, [mbYes], 0)
  else
    SubThread.CheckUpdate := True;
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
begin
  if Sender is TControl then
    with TControl(Sender) do begin
      pmChapterList.Alignment := Menus.paRight;
      pmChapterList.PopUp(ControlOrigin.x, ControlOrigin.y);
      pmChapterList.Alignment := Menus.paLeft;
    end;
  clbChapterList.SetFocus;
end;

procedure TMainForm.cbSelectMangaChange(Sender: TObject);
begin
  if cbSelectManga.ItemIndex < 0 then
    Exit;

  if currentWebsite <> cbSelectManga.Items[cbSelectManga.ItemIndex] then
  begin
    Screen.Cursor := crHourGlass;
    try
      if dataProcess = nil then
        dataProcess := TDBDataProcess.Create;
      vtMangaList.Clear;
      if not dataProcess.Open(
        cbSelectManga.Items.Strings[cbSelectManga.ItemIndex]) then
        RunGetList;
      vtMangaList.RootNodeCount := dataProcess.RecordCount;
      dataProcess.Refresh;
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
      currentWebsite := cbSelectManga.Items[cbSelectManga.ItemIndex];
      dataProcess.website := cbSelectManga.Items[cbSelectManga.ItemIndex];
      CheckForTopPanel;
      LastSearchStr := '';
      edSearchChange(cbSelectManga);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.ckDropTargetChange(Sender: TObject);
begin
  if ckDropTarget.Checked then
  begin
    if FormDropTarget = nil then
      Application.CreateForm(TFormDropTarget, FormDropTarget);
    frmDropTarget.OnDropChekout := @AddSilentThread;
    frmDropTarget.FAlphaBlendValue := tbDropTargetOpacity.Position;
    FormDropTarget.Show;
  end
  else
  begin
    if Assigned(FormDropTarget) then
      FormDropTarget.Close;
  end;
end;

procedure TMainForm.clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if Assigned(Node) then
    if ChapterList[Node^.Index].Downloaded then
    begin
      TargetCanvas.Brush.Color := CL_HLGreenMarks;
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

procedure TMainForm.edURLKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btURLClick(btURL);
end;

procedure TMainForm.edWebsitesSearchChange(Sender: TObject);
var
  s: String;
  lcount: Integer;
  data: PMangaListItem;
  xNode, lNode: PVirtualNode;
begin
  if Length(optionMangaSiteSelectionNodes) < 1 then Exit;
  s := Trim(LowerCase(edWebsitesSearch.Text));
  vtOptionMangaSiteSelection.BeginUpdate;
  try
    lNode := nil;
    lcount := 0;
    vtOptionMangaSiteSelection.RootNode^.TotalHeight := vtOptionMangaSiteSelection.DefaultNodeHeight;
    if s = '' then
    begin
      xNode := vtOptionMangaSiteSelection.GetFirst;
      while Assigned(xNode) do
      begin
        Include(xNode^.States, vsVisible);
        if xNode^.ChildCount > 0 then
        begin
          lNode := xNode;
          Inc(vtOptionMangaSiteSelection.RootNode^.TotalHeight, xNode^.NodeHeight);
        end
        else
          if Assigned(lNode) then
            if vsExpanded in lNode^.States then
              Inc(vtOptionMangaSiteSelection.RootNode^.TotalHeight, xNode^.NodeHeight);
        xNode := vtOptionMangaSiteSelection.GetNext(xNode);
      end;
    end
    else
    begin
      xNode := vtOptionMangaSiteSelection.GetFirst;
      while Assigned(xNode) do
      begin
        Include(xNode^.States, vsVisible);
        if xNode^.ChildCount > 0 then
        begin
          if Assigned(lNode) then
          begin
            if lcount > 0 then
              Inc(vtOptionMangaSiteSelection.RootNode^.TotalHeight, lNode^.NodeHeight)
            else
              Exclude(lNode^.States, vsVisible);
          end;
          lNode := xNode;
          lcount := 0;
        end
        else
        begin
          data := vtOptionMangaSiteSelection.GetNodeData(xNode);
          if Assigned(data) then
          begin
            if Pos(s, LowerCase(data^.Text)) <> 0 then
            begin
              Inc(lcount);
              if Assigned(lNode) then
              begin
                if vsExpanded in lNode^.States then
                  Inc(vtOptionMangaSiteSelection.RootNode^.TotalHeight, xNode^.NodeHeight);
              end;
            end
            else
              Exclude(xNode^.States, vsVisible);
          end;
        end;
        xNode := vtOptionMangaSiteSelection.GetNext(xNode);
      end;
      if Assigned(lNode) then
      begin
        if lcount > 0 then
          Inc(vtOptionMangaSiteSelection.RootNode^.TotalHeight, lNode^.NodeHeight)
        else
          Exclude(lNode^.States, vsVisible);
      end;
    end;
  finally
    vtOptionMangaSiteSelection.EndUpdate;
  end;
end;

procedure TMainForm.btRemoveFilterClick(Sender: TObject);
begin
  if dataProcess.Filtered then
  begin
    vtMangaList.Clear;
    Screen.Cursor := crHourGlass;
    try
      dataProcess.RemoveFilter;
      if dataProcess.FilterAllSites then
      begin
        dataProcess.FilterAllSites := False;
        dataProcess.Free;
        dataProcess := TDBDataProcess.Create;
        dataProcess.Open(cbSelectManga.Items[cbSelectManga.ItemIndex]);
      end;
      vtMangaList.RootNodeCount := dataProcess.RecordCount;
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
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
  checkGenres, uncheckGenres: TStringList;
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
    if cbSearchFromAllSites.Checked then
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
      dataProcess.SitesList.Assign(cbSelectManga.Items);
      dataProcess.FilterAllSites := True;
    end;

    vtMangaList.Clear;
    dataProcess.Filter(checkGenres, uncheckGenres,
      edFilterTitle.Text, edFilterAuthors.Text,
      edFilterArtists.Text, IntToStr(cbFilterStatus.ItemIndex),
      edFilterSummary.Text,
      seOptionNewMangaTime.Value,
      rbAll.Checked, cbOnlyNew.Checked, cbUseRegExpr.Checked);
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
  uncheckGenres.Free;
  checkGenres.Free;
  Screen.Cursor := crDefault;

  vtMangaList.RootNodeCount := dataProcess.RecordCount;
  if dataProcess.Filtered then
    lbMode.Caption := Format(RS_ModeFiltered, [vtMangaList.RootNodeCount])
  else
    lbMode.Caption := Format(RS_ModeAll, [vtMangaList.RootNodeCount])
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
  xNode := vtMangaList.GetFirstSelected;
  for i := 0 to vtMangaList.SelectedCount - 1 do
  begin
    if vtMangaList.Selected[xNode] then
    begin
      SilentThreadManager.Add(MD_AddToFavorites,
        dataProcess.WebsiteName[xNode^.Index],
        DataProcess.Value[xNode^.Index, DATA_PARAM_TITLE],
        DataProcess.Value[xNode^.Index, DATA_PARAM_LINK]);
    end;
    xNode := vtMangaList.GetNextSelected(xNode);
  end;
end;

// ----- vtFavorites popup menu -----

procedure TMainForm.miFavoritesDeleteClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
  delList: array of Cardinal;
begin
  if (cbOptionShowDeleteTaskDialog.Checked) and (vtFavorites.SelectedCount > 0) then
    if MessageDlg('', RS_DlgRemoveFavorite,
      mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', RS_DlgFavoritesCheckIsRunning,
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
    MessageDlg('', RS_DlgFavoritesCheckIsRunning,
      mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  s := FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.currentChapter;
  repeat
    if InputQuery('', RS_DlgTypeInNewChapter, s) then
  until TryStrToInt(s, i);
  if s <> FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.currentChapter then
  begin
    FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.currentChapter := s;
    UpdateVtFavorites;
    FavoriteManager.Backup;
  end;
end;

procedure TMainForm.miFavoritesChangeSaveToClick(Sender: TObject);
begin
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', RS_DlgFavoritesCheckIsRunning,
      mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  if InputQuery('', RS_DlgTypeInNewSavePath,
    FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.SaveTo) then
  begin
    FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.SaveTo :=
      CorrectFilePath(FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.SaveTo);
    UpdateVtFavorites;
    FavoriteManager.Backup;
  end;
end;

// ----- clbChapterList popup menu -----

procedure TMainForm.miChapterListCheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    xNode := clbChapterList.GetFirstSelected;
    for i := 0 to clbChapterList.SelectedCount - 1 do
    begin
      if clbChapterList.Selected[xNode] then
        xNode^.CheckState := csCheckedNormal;
      clbChapterList.InvalidateNode(xNode);
      xNode := clbChapterList.GetNextSelected(xNode);
    end;
  end;
end;

procedure TMainForm.miChapterListUncheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if clbChapterList.SelectedCount > 0 then
  begin
    xNode := clbChapterList.GetFirstSelected;
    for i := 0 to clbChapterList.SelectedCount - 1 do
    begin
      if clbChapterList.Selected[xNode] then
        xNode^.CheckState := csUncheckedNormal;
      clbChapterList.InvalidateNode(xNode);
      xNode := clbChapterList.GetNextSelected(xNode);
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

procedure TMainForm.mnDownload1ClickClick(Sender: TObject);
var
  i: Integer;
begin
  if not isUpdating then
  begin
    if (MessageDlg('', RS_DlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0) =
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
    MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes], 0);
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
      MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes], 0);
  end;
end;

procedure TMainForm.mnUpdateDownFromServerClick(Sender: TObject);
begin
  if (not isUpdating) then
    RunGetList
  else
    MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes], 0);
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
      MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes], 0);
  end;
end;

procedure TMainForm.miDownloadDeleteCompletedClick(Sender: TObject);
begin
  if cbOptionShowDeleteTaskDialog.Checked then
    if not (MessageDlg('', RS_DlgRemoveFinishTasks,
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
  if (vtDownload.SelectedCount = 1) and (Assigned(vtDownload.FocusedNode)) then
  begin
    if DLManager.TaskItem(vtDownload.FocusedNode^.Index).Status in
      [STATUS_STOP, STATUS_PROBLEM, STATUS_FAILED] then
    begin
      DLManager.TaskItem(vtDownload.FocusedNode^.Index).Status := STATUS_WAIT;
      DLManager.TaskItem(vtDownload.FocusedNode^.Index).DownloadInfo.Status :=
        RS_Waiting;
      if DLManager.CanActiveTask(vtDownload.FocusedNode^.Index) then
        DLManager.ActiveTask(vtDownload.FocusedNode^.Index);
      vtDownload.Repaint;
      DLManager.Backup;
    end;
  end
  else
  if (vtDownload.SelectedCount > 1) then
  begin
    xNode := vtDownload.GetFirstSelected;
    for i := 0 to vtDownload.SelectedCount - 1 do
    begin
      if vtDownload.Selected[xNode] and
        (DLManager.TaskItem(xNode^.Index).Status in
          [STATUS_STOP, STATUS_PROBLEM, STATUS_FAILED]) then
      begin
        DLManager.TaskItem(xNode^.Index).Status := STATUS_WAIT;
        DLManager.TaskItem(xNode^.Index).DownloadInfo.Status := RS_Waiting;
        if DLManager.CanActiveTask(xNode^.Index) then
          DLManager.ActiveTask(xNode^.Index);
      end;
      xNode := vtDownload.GetNextSelected(xNode);
    end;
    vtDownload.Repaint;
    DLManager.Backup;
  end;
end;

procedure TMainForm.miDownloadStopClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if not Assigned(vtDownload.FocusedNode) then exit;
  xNode := vtDownload.GetFirstSelected;
  for i := 0 to vtDownload.SelectedCount - 1 do
  begin
    if vtDownload.Selected[xNode] then
      DLManager.StopTask(xNode^.Index, False);
    xNode := vtDownload.GetNextSelected(xNode);
  end;
  DLManager.Backup;
  DLManager.CheckAndActiveTask;
  vtDownload.Repaint;
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

    xNode := vtMangaList.GetFirstSelected;
    for i := 0 to vtMangaList.SelectedCount - 1 do
    begin
      if vtMangaList.Selected[xNode] then
      begin
        AllowedToCreate := True;
        if DLManager.Count > 0 then
          for j := 0 to DLManager.Count - 1 do
            if dataProcess.Value[xNode^.Index, DATA_PARAM_TITLE] =
              DLManager.TaskItem(j).DownloadInfo.title then
            begin
              if YesAll then
                AllowedToCreate := True
              else if NoAll then
                AllowedToCreate := False
              else
              begin
                pcMain.ActivePage := tsDownload;
                mResult := MessageDlg('', DLManager.TaskItem(j).DownloadInfo.title +
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
                end;
              end;
              Break;
            end;

        if AllowedToCreate then
          SilentThreadManager.Add(MD_DownloadAll,
            dataProcess.WebsiteName[xNode^.Index],
            dataProcess.Value[xNode^.Index, DATA_PARAM_TITLE],
            dataProcess.Value[xNode^.Index, DATA_PARAM_LINK]);
      end;
      xNode := vtMangaList.GetNextSelected(xNode);
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
  btAddToFavorites.Enabled := False;
  pcMain.ActivePage := tsInformation;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add(RS_Loading);
  clbChapterList.Clear;

  if isGetMangaInfos then
  begin
    GetInfosThread.IsFlushed := True;
    GetInfosThread.Terminate;
    //GetInfosThread.WaitFor;
  end;
  GetInfosThread := TGetMangaInfosThread.Create;
  GetInfosThread.MangaListPos := vtMangaList.FocusedNode^.Index;

  website := dataProcess.WebsiteName[GetInfosThread.MangaListPos];
  title := DataProcess.Value[GetInfosThread.mangaListPos, DATA_PARAM_TITLE];
  link := DataProcess.Value[GetInfosThread.mangaListPos, DATA_PARAM_LINK];

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
end;

procedure TMainForm.miFavoritesOpenFolderClick(Sender: TObject);
begin
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  OpenDocument(TrimRightChar(
    FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.SaveTo,
    [PathDelim]));
end;

procedure TMainForm.miDownloadOpenFolderClick(Sender: TObject);
begin
  if (vtDownload.SelectedCount = 0) or (Assigned(vtDownload.FocusedNode) = False) then
    Exit;
  OpenDocument(TrimRightChar(
    DLManager.TaskItem(vtDownload.FocusedNode^.Index).DownloadInfo.SaveTo,
    [PathDelim]));
end;

procedure TMainForm.miFavoritesOpenWithClick(Sender: TObject);
var
  f, fd: String;
  Info: TSearchRec;
  l: TStringList;
begin
  if (not Assigned(vtFavorites.FocusedNode)) then
    Exit;
  l := TStringList.Create;
  try
    fd := StringReplace(FavoriteManager.FavoriteItem(
      vtFavorites.FocusedNode^.Index).FavoriteInfo.SaveTo, '/', '\', [rfReplaceAll]);
    if fd[Length(fd)] <> PathDelim then
      fd := fd + PathDelim;

    if FindFirstUTF8(fd + '*', faAnyFile and faDirectory, Info) = 0 then
      repeat
        l.Add(Info.Name);
      until FindNextUTF8(Info) <> 0;
    if l.Count >= 3 then
      f := l.Strings[2]
    else
      f := '';
    FindCloseUTF8(Info);

    OpenWithExternalProgram(fd, f);
  except
  end;
  l.Free;
end;

procedure TMainForm.miDownloadOpenWithClick(Sender: TObject);
var
  f, fd, ff: String;
  Info: TSearchRec;
  l: TStringList;
begin
  if (not Assigned(vtDownload.FocusedNode)) then
    Exit;
  l := TStringList.Create;
  try
    fd := StringReplace(DLManager.TaskItem(
      vtDownload.FocusedNode^.Index).DownloadInfo.SaveTo, '/', '\', [rfReplaceAll]);
    if fd[Length(fd)] <> PathDelim then
      fd := fd + PathDelim;

    if DLManager.TaskItem(vtDownload.FocusedNode^.Index).ChapterName.Count > 0 then
    begin
      ff := DLManager.TaskItem(vtDownload.FocusedNode^.Index).
        ChapterName[0];
      if FileExistsUTF8(fd + ff + '.zip') then
        f := ff + '.zip'
      else if FileExistsUTF8(fd + ff + '.cbz') then
        f := ff + '.cbz'
      else if FileExistsUTF8(fd + ff + '.pdf') then
        f := ff + '.pdf'
      else if DirectoryExistsUTF8(fd + ff) then
        f := ff
      else
        f := '';
    end;

    if f = '' then
    begin
      if FindFirstUTF8(fd + '*', faAnyFile and faDirectory, Info) = 0 then
        repeat
          l.Add(Info.Name);
        until FindNextUTF8(Info) <> 0;
      if l.Count >= 3 then
        f := l.Strings[2]
      else
        f := '';
      FindCloseUTF8(Info);
    end;

    OpenWithExternalProgram(fd, f);
  except
  end;
  l.Free;
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
    OptionLetFMDDo := TFMDDo(cbOptionLetFMDDo.ItemIndex);
    edOptionExternalPath.FileName := options.ReadString('general', 'ExternalProgramPath', '');
    edOptionExternalParams.Text := options.ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM);

    cbOptionShowDownloadToolbar.Checked := options.ReadBool('view', 'ShowDownloadsToolbar', True);

    seOptionMaxParallel.Value := options.ReadInteger('connections', 'NumberOfTasks', 1);
    seOptionMaxThread.Value :=
      options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
    seOptionMaxRetry.Value := options.ReadInteger('connections', 'Retry', 3);
    seOptionConnectionTimeout.Value := options.ReadInteger('connections', 'ConnectionTimeout', 15);
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
    if Trim(edOptionCustomRename.Text) = '' then
      edOptionCustomRename.Text := DEFAULT_CUSTOM_RENAME;

    cbOptionShowQuitDialog.Checked :=
      options.ReadBool('dialogs', 'ShowQuitDialog', True);
    cbOptionShowDeleteTaskDialog.Checked :=
      options.ReadBool('dialogs', 'ShowDeleteDldTaskDialog', True);

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
    lbOptionAutoCheckMinutes.Caption := Format(RS_LblAutoCheckNewChapterMinute,
      [seOptionCheckMinutes.Value]);

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
  if pcMain.ActivePage = tsAbout then
    LoadAbout
  else
  if pcMain.ActivePage = tsFavorites then
    vtFavorites.Repaint;
  UpdateOptions;
end;

procedure TMainForm.pmDownloadPopup(Sender: TObject);

  function FinishedTaskPresent: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    with DLManager do begin
      CS_DownloadManager_Task.Acquire;
      try
        for i := 0 to Count - 1 do
          if TaskItem(i).Status = STATUS_FINISH then
          begin
            Result := True;
            Break;
          end;
      finally
        CS_DownloadManager_Task.Release;
      end;
    end;
  end;

  function SelectedTaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
  var
    xNode: PVirtualNode;
  begin
    Result := False;
    if vtDownload.SelectedCount > 0 then
    begin
      with DLManager do
      begin
        CS_DownloadManager_Task.Acquire;
        try
          xNode := vtDownload.GetFirstSelected;
          repeat
            if TaskItem(xNode^.Index).Status in Stats then
            begin
              Result := True;
              Break;
            end;
            xNode := vtDownload.GetNextSelected(xNode);
          until xNode = nil;
        finally
          CS_DownloadManager_Task.Release;
        end;
      end;
    end;
  end;

begin
  with DLManager do begin
    if vtDownload.SelectedCount = 0 then
    begin
      miDownloadStop.Enabled := False;
      miDownloadResume.Enabled := False;
      miDownloadDelete.Enabled := False;
      miDownloadDeleteTask.Enabled := False;
      miDownloadDeleteTaskData.Enabled := False;
      miDownloadDeleteCompleted.Enabled := FinishedTaskPresent;
      miDownloadMergeCompleted.Enabled := miDownloadDeleteCompleted.Enabled;
      miDownloadViewMangaInfo.Enabled := False;
      miDownloadOpenFolder.Enabled := False;
      miDownloadOpenWith.Enabled := False;
    end
    else
    if vtDownload.SelectedCount = 1 then
    begin
      miDownloadStop.Enabled := (TaskItem(vtDownload.FocusedNode^.Index).Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT]);
      miDownloadResume.Enabled := (TaskItem(vtDownload.FocusedNode^.Index).Status in [STATUS_STOP, STATUS_FAILED, STATUS_PROBLEM]);
      miDownloadDelete.Enabled := True;
      miDownloadDeleteTask.Enabled := True;
      miDownloadDeleteTaskData.Enabled := True;
      miDownloadDeleteCompleted.Enabled := FinishedTaskPresent;
      miDownloadMergeCompleted.Enabled := miDownloadDeleteCompleted.Enabled;
      miDownloadViewMangaInfo.Enabled := (TaskItem(vtDownload.FocusedNode^.Index).DownloadInfo.Link <> '');
      miDownloadOpenFolder.Enabled := True;
      miDownloadOpenWith.Enabled := True;
    end
    else
    begin
      miDownloadStop.Enabled := SelectedTaskStatusPresent([STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT]);
      miDownloadResume.Enabled := SelectedTaskStatusPresent([STATUS_STOP, STATUS_FAILED, STATUS_PROBLEM]);
      miDownloadDelete.Enabled := True;
      miDownloadDeleteTask.Enabled := True;
      miDownloadDeleteTaskData.Enabled := True;
      miDownloadDeleteCompleted.Enabled := FinishedTaskPresent;
      miDownloadMergeCompleted.Enabled := miDownloadDeleteCompleted.Enabled;
      miDownloadViewMangaInfo.Enabled := False;
      miDownloadOpenFolder.Enabled := False;
      miDownloadOpenWith.Enabled := False;
    end;
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

  function SelectedStatusPresent(Stats: TFavoriteStatusTypes): Boolean;
  var
    xNode: PVirtualNode;
  begin
    Result := False;
    with FavoriteManager do
    begin
      if vtFavorites.SelectedCount > 0 then
      begin
        Lock;
        try
          xNode := vtFavorites.GetFirstSelected;
          repeat
            if Assigned(xNode) then
            begin
              if FavoriteManager.FavoriteItem(xNode^.Index).Status in Stats then
              begin
                Result := True;
                Break;
              end;
              xNode := vtFavorites.GetNextSelected(xNode);
            end;
          until xNode = nil;
        finally
          LockRelease;
        end;
      end;
    end;
  end;

begin
  if vtFavorites.SelectedCount = 0 then
  begin
    miFavoritesViewInfos.Enabled := False;
    miFavoritesDownloadAll.Enabled := False;
    miFavoritesDelete.Enabled := False;
    miFavoritesChangeSaveTo.Enabled := False;
    miFavoritesOpenFolder.Enabled := False;
    miFavoritesOpenWith.Enabled := False;
  end
  else
  if vtFavorites.SelectedCount = 1 then
  begin
    miFavoritesCheckNewChapter.Visible := SelectedStatusPresent([STATUS_IDLE]);
    miFavoritesStopCheckNewChapter.Visible :=
      SelectedStatusPresent([STATUS_CHECK, STATUS_CHECKING]);
    miFavoritesViewInfos.Enabled := True;
    miFavoritesDownloadAll.Enabled := (Trim(FavoriteManager.FavoriteItem(
      vtFavorites.FocusedNode^.Index).FavoriteInfo.Link) <> '');
    miFavoritesDelete.Enabled := True;
    miFavoritesChangeSaveTo.Enabled := True;
    miFavoritesOpenFolder.Enabled :=
      DirectoryExistsUTF8(FavoriteManager.FavoriteItem(vtFavorites.FocusedNode^.Index).FavoriteInfo.SaveTo);
    miFavoritesOpenWith.Enabled := miFavoritesOpenFolder.Enabled;
  end
  else
  begin
    miFavoritesCheckNewChapter.Visible := SelectedStatusPresent([STATUS_IDLE]);
    miFavoritesStopCheckNewChapter.Visible :=
      SelectedStatusPresent([STATUS_CHECK, STATUS_CHECKING]);
    miFavoritesViewInfos.Enabled := False;
    miFavoritesDownloadAll.Enabled := True;
    miFavoritesDelete.Enabled := True;
    miFavoritesChangeSaveTo.Enabled := False;
    miFavoritesOpenFolder.Enabled := False;
    miFavoritesOpenWith.Enabled := False;
  end;
  if FavoriteManager.isRunning then
  begin
    miFavoritesDelete.Enabled := False;
    miFavoritesChangeSaveTo.Enabled := False;
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
    Format(RS_LblAutoCheckNewChapterMinute, [seOptionCheckMinutes.Value]);
end;

procedure TMainForm.spMainSplitterMoved(Sender: TObject);
begin
  sbMain.Panels[0].Width := spMainSplitter.Left;
end;

procedure TMainForm.tbDownloadDeleteCompletedClick(Sender: TObject);
begin
  if DLManager.TaskStatusPresent([STATUS_FINISH]) then
    miDownloadDeleteCompletedClick(miDownloadDeleteCompleted);
end;

procedure TMainForm.tbDownloadResumeAllClick(Sender: TObject);
begin
  DLManager.StartAllTasks;
end;

procedure TMainForm.tbDownloadStopAllClick(Sender: TObject);
begin
  DLManager.StopAllTasks;
end;

procedure TMainForm.tbDropTargetOpacityChange(Sender: TObject);
begin
  frmDropTarget.FAlphaBlendValue := tbDropTargetOpacity.Position;
  if Assigned(FormDropTarget) then
    FormDropTarget.AlphaBlendValue := frmDropTarget.FAlphaBlendValue;
end;

procedure TMainForm.tbWebsitesCollapseAllClick(Sender: TObject);
begin
  vtOptionMangaSiteSelection.FullCollapse;
end;

procedure TMainForm.tbWebsitesExpandAllClick(Sender: TObject);
begin
  vtOptionMangaSiteSelection.FullExpand;
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
  pcMain.ActivePage := tsDownload;
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
  if Node = nil then Exit;
  if Node^.Index >= DLManager.Count then Exit;
  if Column = 2 then
  begin
    Data := vtDownload.GetNodeData(Node);
    //if Data^.Status = stFinish then
    if DLManager.TaskItem(Node^.Index).Status in
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

      case DLManager.TaskItem(Node^.Index).Status of
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
  miDownloadOpenFolderClick(Sender);
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
  ConTemp: TFPList;
begin
  vtDownload.BeginUpdate;
  ConTemp := TFPList.Create;
  try
    nIndex := NextIndex;

    if vtDownload.SelectedCount > 0 then
    begin
      cNode := vtDownload.GetFirst;
      i := 0;
      while i < vtDownload.RootNodeCount do
               //DLManager.Count do
      begin
        if vtDownload.Selected[cNode] then
        begin
          vtDownload.Selected[cNode] := False;
          ConTemp.Add(DLManager.TaskItem(i));
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
          if (nIndex < DLManager.Count) then
            Inc(nIndex);
        end;
        if nIndex > DLManager.Count then
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
    (DLManager.Count < 2) then
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
  if Node^.Index >= DLManager.Count then Exit;
  if Column = 0 then
  begin
    l := DLManager.TaskItem(Node^.Index).ChapterLinks.Count;
    if l > 0 then
    begin
      HintText := '';
      if l < 5 then
      begin
        for i := 0 to l - 1 do
          if HintText = '' then
            HintText :=
              DLManager.TaskItem(Node^.Index).ChapterName.Strings[i]{ + ' : ' +
        DLManager.TaskItem(Node^.Index).ChapterLinks.Strings[i]}
          else
            HintText := HintText + LineEnding +
              DLManager.TaskItem(Node^.Index).ChapterName.Strings[i]{ + ' : ' +
        DLManager.TaskItem(Node^.Index).ChapterLinks.Strings[i]};
      end
      else
      begin
        for i := 0 to 1 do
          if HintText = '' then
            HintText :=
              DLManager.TaskItem(Node^.Index).ChapterName.Strings[i]{ + ' : ' +
        DLManager.TaskItem(Node^.Index).ChapterLinks.Strings[i]}
          else
            HintText := HintText + LineEnding +
              DLManager.TaskItem(Node^.Index).ChapterName.Strings[i]{ + ' : ' +
        DLManager.TaskItem(Node^.Index).ChapterLinks.Strings[i]};
        HintText := HintText + LineEnding + '...';
        for i := l - 2 to l - 1 do
          HintText := HintText + LineEnding +
            DLManager.TaskItem(Node^.Index).ChapterName.Strings[i]{ + ' : ' +
        DLManager.TaskItem(Node^.Index).ChapterLinks.Strings[i]};
      end;
    end;
  end
  else
  begin
    p := Sender.GetNodeData(Node);
    case Column of
      1: HintText := p^.Status;
      2: HintText := p^.Progress;
      4: HintText := p^.Website;
      5: HintText := p^.SaveTo;
      6: HintText := DateTimeToStr(p^.dateTime);
    end;
  end;
end;

procedure TMainForm.vtDownloadGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if (Node^.Index < DLManager.Count) and
    (vtDownload.Header.Columns[Column].Position = 0) then
    ImageIndex := integer(DLManager.TaskItem(Node^.Index).Status);
end;

procedure TMainForm.vtDownloadGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PDownloadInfo;
  pos: Cardinal;
begin
  if Node^.Index >= DLManager.Count then Exit;
  with Sender do
  begin
    pos := Node^.Index;
    Data := Sender.GetNodeData(Node);
    if (DLManager.Count > 0) then
      if Assigned(Data) and (DLManager.TaskItem(pos) <> nil) then
      begin
        Data^.Title := DLManager.TaskItem(pos).DownloadInfo.Title;
        Data^.Status := DLManager.TaskItem(pos).DownloadInfo.Status;
        Data^.Progress := DLManager.TaskItem(pos).DownloadInfo.Progress;
        Data^.TransferRate := DLManager.TaskItem(pos).DownloadInfo.TransferRate;
        Data^.Website := DLManager.TaskItem(pos).DownloadInfo.Website;
        Data^.SaveTo := DLManager.TaskItem(pos).DownloadInfo.SaveTo;
        Data^.DateTime := DLManager.TaskItem(pos).DownloadInfo.DateTime;
        case Column of
          0: CellText := Data^.title;
          1: CellText := Data^.status;
          2: CellText := '';
          3: CellText := Data^.TransferRate;
          4: CellText := Data^.website;
          5: CellText := Data^.saveTo;
          6: CellText := DateTimeToStr(Data^.dateTime);
        end;
      end;
  end;
end;

procedure TMainForm.vtDownloadHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  if DLManager.Count < 2 then Exit;
  if (Column = 2) or (Column = 3) then Exit;
  if DLManager.SortColumn = Column then
    DLManager.SortDirection := not DLManager.SortDirection;
  DLManager.SortColumn := Column;
  vtDownload.Header.SortDirection := TSortDirection(DLManager.SortDirection);
  vtDownload.Header.SortColumn := Column;
  DLManager.Sort(Column);
  options.WriteInteger('misc', 'SortDownloadColumn', vtDownload.Header.SortColumn);
  options.WriteBool('misc', 'SortDownloadDirection', DLManager.SortDirection);
  vtDownload.Repaint;
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
    if (DLManager.Count <> 0) then
      if (DLManager.TaskItem(pos) <> nil) or
        (not DLManager.TaskItem(pos).Thread.isTerminated) then
      begin
        Data^.title := DLManager.TaskItem(pos).DownloadInfo.title;
        Data^.status := DLManager.TaskItem(pos).DownloadInfo.Status;
        Data^.progress := DLManager.TaskItem(pos).DownloadInfo.Progress;
        Data^.TransferRate := DLManager.TaskItem(pos).DownloadInfo.TransferRate;
        Data^.website := DLManager.TaskItem(pos).DownloadInfo.Website;
        Data^.saveTo := DLManager.TaskItem(pos).DownloadInfo.SaveTo;
        Data^.dateTime := DLManager.TaskItem(pos).DownloadInfo.dateTime;
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
    miDownloadDeleteTaskClick(miDownloadDeleteTask);
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
    with FavoriteManager.FavoriteItem(Node^.Index) do
    begin
      if Trim(FavoriteInfo.Link) = '' then
      begin
        TargetCanvas.Brush.Color := CL_HLRedMarks;
        TargetCanvas.FillRect(CellRect);
      end
      else
      if Status = STATUS_CHECKING then
      begin
        TargetCanvas.Brush.Color := CL_HLGreenMarks;
        TargetCanvas.FillRect(CellRect);
      end;
    end;
  end;
end;

procedure TMainForm.vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  miFavoritesOpenFolderClick(Sender);
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
      with FavoriteManager.FavoriteItem(Node^.Index) do
      begin
        if Trim(FavoriteInfo.Link) = '' then
          ImageIndex := 16
        else
        case Status of
          STATUS_CHECK: ImageIndex := 19;
          STATUS_CHECKING: ImageIndex := 12;
          STATUS_CHECKED: ImageIndex := 20;
          else
            ImageIndex := -1;
        end;
      end;
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
  if FavoriteManager.isRunning then Exit;
  if FavoriteManager.Count < 2 then Exit;
  if Column = 0 then Exit;
  FavoriteManager.isRunning := True;
  try
    if FavoriteManager.SortColumn = Column then
      FavoriteManager.sortDirection := not FavoriteManager.sortDirection;
    FavoriteManager.SortColumn := Column;
    vtFavorites.Header.SortColumn := Column;
    vtFavorites.Header.SortDirection := TSortDirection(FavoriteManager.sortDirection);
    FavoriteManager.Sort(Column);
    options.WriteInteger('misc', 'SortFavoritesColumn', vtFavorites.Header.SortColumn);
    options.WriteBool('misc', 'SortFavoritesDirection', FavoriteManager.sortDirection);
  finally
    UpdateVtFavorites;
    FavoriteManager.isRunning := False;
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
    Data^.numbering := IntToStr(QWord(pos) + 1);
    Data^.Title := FavoriteManager.FavoriteItem(pos).FavoriteInfo.Title;
    Data^.currentChapter := FavoriteManager.FavoriteItem(pos).FavoriteInfo.currentChapter;
    Data^.website := FavoriteManager.FavoriteItem(pos).FavoriteInfo.website;
    Data^.saveTo := FavoriteManager.FavoriteItem(pos).FavoriteInfo.saveTo;
    Data^.Link := FavoriteManager.FavoriteItem(pos).FavoriteInfo.Link;
  end;
  vtFavorites.ValidateNode(Node, False);
end;

procedure TMainForm.vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  //if (NOT isUpdating) then
  //begin
  if vtMangaList.SelectedCount > 0 then
    sbMain.Panels[0].Text := Format(RS_Selected, [vtMangaList.SelectedCount])
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
      MessageDlg('', RS_DlgMangaListSelect,
        mtConfirmation, [mbYes], 0);
      Exit;
    end;

    // general
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

    if not isStillHaveCurrentWebsite then
    begin
      if cbSelectManga.Items.Count > 0 then
      begin
        cbSelectManga.ItemIndex := 0;
        cbSelectMangaChange(Sender);
      end
      else
      begin
        cbSelectManga.ItemIndex := -1;
        cbSelectManga.Text := '';
        currentWebsite := '';
        FreeAndNil(dataProcess);
        vtMangaList.Clear;
        lbMode.Caption := Format(RS_ModeAll, [0]);
      end;
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

    options.WriteString('languages', 'Selected',
      AvailableLanguages.Names[cbLanguages.ItemIndex]);
    options.WriteBool('general', 'MinimizeToTray', cbOptionMinimizeToTray.Checked);
    options.WriteInteger('general', 'NewMangaTime', seOptionNewMangaTime.Value);
    options.WriteInteger('general', 'LetFMDDo', cbOptionLetFMDDo.ItemIndex);
    OptionLetFMDDo := TFMDDo(cbOptionLetFMDDo.ItemIndex);
    options.WriteBool('general', 'LoadMangaCover', cbOptionEnableLoadCover.Checked);
    OptionEnableLoadCover := cbOptionEnableLoadCover.Checked;
    options.WriteString('general', 'ExternalProgramPath', edOptionExternalPath.FileName);
    options.WriteString('general', 'ExternalProgramParams', edOptionExternalParams.Text);

    // view
    options.WriteBool('droptarget', 'Show', ckDropTarget.Checked);
    options.WriteInteger('droptarget', 'Mode', rgDropTargetMode.ItemIndex);
    options.WriteInteger('droptarget', 'Opacity', tbDropTargetOpacity.Position);
    options.WriteInteger('droptarget', 'Width', frmDropTarget.FWidth);
    options.WriteInteger('droptarget', 'Heigth', frmDropTarget.FHeight);
    options.WriteInteger('droptarget', 'Top', frmDropTarget.FTop);
    options.WriteInteger('droptarget', 'Left', frmDropTarget.FLeft);
    options.WriteBool('view', 'ShowDownloadsToolbar', cbOptionShowDownloadToolbar.Checked);
    ToolBarDownload.Visible := cbOptionShowDownloadToolbar.Checked;

    // connections
    options.WriteInteger('connections', 'NumberOfTasks', seOptionMaxParallel.Value);
    options.WriteInteger('connections', 'NumberOfThreadsPerTask',
      seOptionMaxThread.Value);
    options.WriteInteger('connections', 'Retry', seOptionMaxRetry.Value);
    DLManager.retryConnect := seOptionMaxRetry.Value;
    options.WriteInteger('connections', 'ConnectionTimeout', seOptionConnectionTimeout.Value);
    OptionConnectionTimeout := seOptionConnectionTimeout.Value*1000;
    options.WriteBool('connections', 'UseProxy', cbOptionUseProxy.Checked);
    options.WriteString('connections', 'ProxyType', cbOptionProxyType.Text);
    options.WriteString('connections', 'Host', edOptionHost.Text);
    options.WriteString('connections', 'Pass', edOptionPass.Text);
    options.WriteString('connections', 'Port', edOptionPort.Text);
    options.WriteString('connections', 'User', edOptionUser.Text);

    // saveto
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
    if Trim(edOptionCustomRename.Text) = '' then
      edOptionCustomRename.Text := DEFAULT_CUSTOM_RENAME;
    options.WriteString('saveto', 'CustomRename', edOptionCustomRename.Text);
    OptionCustomRename := edOptionCustomRename.Text;
    options.WriteBool('saveto', 'ConvertDigitVolume', cbOptionDigitVolume.Checked);
    options.WriteBool('saveto', 'ConvertDigitChapter', cbOptionDigitChapter.Checked);
    options.WriteInteger('saveto', 'DigitVolumeLength', seOptionDigitVolume.Value);
    options.WriteInteger('saveto', 'DigitChapterLength', seOptionDigitChapter.Value);

    // update
    options.WriteBool('update', 'AutoRemoveCompletedManga',
      cbOptionAutoRemoveCompletedManga.Checked);
    OptionAutoRemoveCompletedManga := cbOptionAutoRemoveCompletedManga.Checked;
    options.WriteBool('update', 'AutoCheckUpdate',
      cbOptionAutoCheckUpdate.Checked);
    options.WriteBool('update', 'AutoCheckFavStartup',
      cbOptionAutoCheckFavStartup.Checked);
    OptionAutoCheckFavStartup := cbOptionAutoCheckFavStartup.Checked;
    options.WriteInteger('update', 'AutoCheckMinutes', seOptionCheckMinutes.Value);
    OptionCheckMinutes := seOptionCheckMinutes.Value;
    lbOptionAutoCheckMinutes.Caption := Format(RS_LblAutoCheckNewChapterMinute,
      [seOptionCheckMinutes.Value]);
    options.WriteBool('update', 'UpdateListNoMangaInfo',
      cbOptionUpdateListNoMangaInfo.Checked);
    OptionUpdateListNoMangaInfo := cbOptionUpdateListNoMangaInfo.Checked;
    options.WriteBool('update', 'UpdateListRemoveDuplicateLocalData',
      cbOptionUpdateListRemoveDuplicateLocalData.Checked);
    OptionUpdateListRemoveDuplicateLocalData := cbOptionUpdateListRemoveDuplicateLocalData.Checked;

    DLManager.compress := rgOptionCompress.ItemIndex;

    // dialogs
    options.WriteBool('dialogs', 'ShowQuitDialog', cbOptionShowQuitDialog.Checked);
    options.WriteBool('dialogs', 'ShowDeleteDldTaskDialog',
      cbOptionShowDeleteTaskDialog.Checked);

    // misc
    options.WriteBool('misc', 'ShowBatotoSG', cbOptionShowBatotoSG.Checked);
    options.WriteBool('misc', 'ShowAllLang', cbOptionShowAllLang.Checked);
    options.WriteBool('misc', 'AutoDlFav', cbOptionAutoDlFav.Checked);
    OptionShowBatotoSG := cbOptionShowBatotoSG.Checked;
    OptionShowAllLang := cbOptionShowAllLang.Checked;
    OptionAutoDlFav := cbOptionAutoDlFav.Checked;
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

    DLManager.maxDLTasks := seOptionMaxParallel.Value;
    DLManager.maxDLThreadsPerTask := seOptionMaxThread.Value;
    DLManager.retryConnect := seOptionMaxRetry.Value;

    LoadLanguage;
  finally
    //Recheck download thread
    DLManager.CheckAndActiveTask;
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
var
  data: PMangaListItem;
begin
  if (isExiting) or (dataProcess.RecordCount = 0) then
    Exit;
  if miHighlightNewManga.Checked then
  begin
    if Assigned(Node) then
      if StrToIntDef(dataProcess.Value[Node^.Index, DATA_PARAM_JDN], 0) > (currentJDN - seOptionNewMangaTime.Value) then
      begin
        TargetCanvas.Brush.Color := CL_HLBlueMarks;
        TargetCanvas.FillRect(CellRect);
      end;
  end;
end;

procedure TMainForm.vtMangaListGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  LPos: Integer;
  s: String;
begin
  s := '';
  LPos := Node^.Index;
  with dataProcess do
  begin
    if FilterAllSites then
      s := s + RS_InfoWebsite + LineEnding +
        dataProcess.WebsiteName[LPos] + LineEnding + LineEnding;
    if Trim(Value[LPos, DATA_PARAM_TITLE]) <> '' then
      s := s + RS_InfoTitle + LineEnding + Value[LPos, DATA_PARAM_TITLE];
    if Trim(Value[LPos, DATA_PARAM_AUTHORS]) <> '' then
      s := s + LineEnding + LineEnding + RS_InfoAuthors + LineEnding +
        Value[LPos, DATA_PARAM_AUTHORS];
    if Trim(Value[LPos, DATA_PARAM_ARTISTS]) <> '' then
      s := s + LineEnding + LineEnding + RS_InfoArtists + LineEnding +
        Value[LPos, DATA_PARAM_ARTISTS];
    if Trim(Value[LPos, DATA_PARAM_GENRES]) <> '' then
      s := s + LineEnding + LineEnding + RS_InfoGenres + LineEnding +
        Value[LPos, DATA_PARAM_GENRES];
    if Trim(Value[LPos, DATA_PARAM_STATUS]) <> '' then
    begin
      s := s + LineEnding + LineEnding + RS_InfoStatus + LineEnding;
      if Value[LPos, DATA_PARAM_STATUS] = '0' then
        s := s + cbFilterStatus.Items[0]
      else
        s := s + cbFilterStatus.Items[1];
    end;
    if Trim(Value[LPos, DATA_PARAM_SUMMARY]) <> '' then
      //s := s + LineEndingLineEnding + infoSummary + ':' + LineEnding + PrepareSummaryForHint(dataProcess.Value[LPos, DATA_PARAM_SUMMARY], 80);
      s := s + LineEnding + LineEnding + RS_InfoSummary + ':' + LineEnding +
        StringBreaks(dataProcess.Value[LPos, DATA_PARAM_SUMMARY]);
  end;
  HintText := s;
end;

procedure TMainForm.vtMangaListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Assigned(Node) then
    CellText :=  Format('%s (%s)', [dataProcess.Value[Node^.Index, DATA_PARAM_TITLE],
      dataProcess.Value[Node^.Index, DATA_PARAM_NUMCHAPTER]]);
end;

procedure TMainForm.InitCheckboxes;
var
  i: Cardinal;
begin
  for i := 0 to 37 do
    TCheckBox(pnGenres.Controls[i]).State := cbGrayed;
end;

procedure TMainForm.ShowTasks(Status: TDownloadStatusTypes);
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
    if Status = [] then
      vtDownload.isVisible[xNode] := True
    else
      vtDownload.IsVisible[xNode] := DLManager.TaskItem(i).Status in Status;
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
  if vtDownload.RootNodeCount <> DLManager.Count then
    vtDownload.RootNodeCount := DLManager.Count;
  xNode := vtDownload.GetLast;
  for i := DLManager.Count-1 downto 0 do
  begin
    if i < DLManager.Count then
    begin
      dt := DLManager.TaskItem(i).DownloadInfo.dateTime;
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
    0, 5: ShowTasks;
    1: ShowTasks([STATUS_FINISH]);
    2: ShowTasks([STATUS_PREPARE, STATUS_DOWNLOAD, STATUS_COMPRESS]);
    3: ShowTasks([STATUS_STOP]);
    4: ShowTasks([STATUS_PROBLEM, STATUS_FAILED]);
    6: ShowTodayTasks;
    7: ShowYesterdayTasks;
    8: ShowOneWeekTasks;
    9: ShowOneMonthTasks;
  end;
  tvDownloadFilterRepaint;
  isRunDownloadFilter := False;
end;

procedure TMainForm.AddChapterNameToList;
begin
  UpdateVtChapter;
end;

procedure TMainForm.AddSilentThread(URL: string);
var
  mt: TMetaDataType;
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
    if not (regx.Exec(URL)) then
      URL := 'http://' + URL;

    regx.Expression := '^https?\:(//[^/]*\w+\.\w+)(\:\d+)?(/|\Z)(.*)$';
    if regx.Exec(URL) then
    begin
      link := regx.Replace(URL, '$4', True);
      webs := regx.Replace(URL, '$1', True);
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
        URL := FixURL(WebsiteRoots[webid, 1] + link);
        DisableAddToFavorites(website);
      end;
    end;
  finally
    regx.Free;
  end;
  if (website = '') or (link = '') then Exit;
  if rgDropTargetMode.ItemIndex = 0 then
    mt := MD_DownloadAll
  else
    mt := MD_AddToFavorites;
  if (mt = MD_AddToFavorites) and (SitesWithoutFavorites(website)) then Exit;
  SilentThreadManager.Add(mt, website, '', link);
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
      if title = RS_InfoSummary then
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
  pcMain.ActivePage := tsInformation;
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

    AddTextToInfo(RS_InfoTitle, mangaInfo.title + LineEnding);
    AddTextToInfo(RS_InfoAuthors, mangaInfo.authors + LineEnding);
    AddTextToInfo(RS_InfoArtists, mangaInfo.artists + LineEnding);
    AddTextToInfo(RS_InfoGenres, mangaInfo.genres + LineEnding);
    i := StrToIntDef(mangaInfo.status, -1);
    if (i > -1) and (i < cbFilterStatus.Items.Count) then
      AddTextToInfo(RS_InfoStatus, cbFilterStatus.Items[i]);
    AddTextToInfo(RS_InfoSummary, mangaInfo.summary);
    CaretPos := Point(0, 0);
  end;
  SetLength(ChapterList, mangaInfo.chapterName.Count);
  for i := 0 to mangaInfo.chapterName.Count - 1 do
  begin
    ChapterList[i].Title := mangaInfo.chapterName[i];
    ChapterList[i].Link := mangaInfo.chapterLinks[i];
    ChapterList[i].Downloaded := False;
  end;
  if miChapterListHighlight.Checked then
    DLManager.GetDownloadedChaptersState(mangaInfo.website + mangaInfo.link,
      ChapterList)
  else
    ClearChapterListState;
  UpdateVtChapter;

  btDownload.Enabled := (clbChapterList.RootNodeCount > 0);
  btReadOnline.Enabled := (mangaInfo.link <> '');
  btAddToFavorites.Enabled := not SitesWithoutFavorites(website);

  //check if manga already in FavoriteManager list
  if btAddToFavorites.Enabled and (FavoriteManager.Count > 0) then
    btAddToFavorites.Enabled := not FavoriteManager.IsMangaExist(mangaInfo.title, website);
end;

procedure TMainForm.RunGetList;
begin
  if (MessageDlg('', RS_DlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0) =
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
var
  i: Integer;
begin
  // general
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
  cbOptionLiveSearch.Checked := options.ReadBool('general', 'LiveSearch', True);
  cbOptionMinimizeToTray.Checked := options.ReadBool('general', 'MinimizeToTray', False);
  OptionEnableLoadCover := options.ReadBool('general', 'LoadMangaCover', True);
  cbOptionEnableLoadCover.Checked := OptionEnableLoadCover;
  cbOptionLetFMDDo.ItemIndex := options.ReadInteger('general', 'LetFMDDo', 0);
  OptionLetFMDDo := TFMDDo(cbOptionLetFMDDo.ItemIndex);
  cbOptionAutoNumberChapter.Checked :=
    options.ReadBool('general', 'AutoNumberChapter', True);
  edOptionExternalPath.FileName := options.ReadString('general', 'ExternalProgramPath', '');
  edOptionExternalParams.Text := options.ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM);
  OptionAutoNumberChapterChecked := cbOptionAutoNumberChapter.Checked;
  cbAddAsStopped.Checked := options.ReadBool('general', 'AddAsStopped', False);

  // view
  frmDropTarget.FWidth := options.ReadInteger('droptarget', 'Width',
    frmDropTarget.FWidth);
  frmDropTarget.FHeight := options.ReadInteger('droptarget', 'Heigth',
    frmDropTarget.FHeight);
  frmDropTarget.FTop := options.ReadInteger('droptarget', 'Top',
    frmDropTarget.FTop);
  frmDropTarget.FLeft := options.ReadInteger('droptarget', 'Left',
    frmDropTarget.FLeft);
  rgDropTargetMode.ItemIndex := options.ReadInteger('droptarget', 'Mode', 0);
  tbDropTargetOpacity.Position := options.ReadInteger('droptarget', 'Opacity', 255);
  ckDropTarget.Checked := options.ReadBool('droptarget', 'Show', False);
  cbOptionShowDownloadToolbar.Checked := options.ReadBool('view', 'ShowDownloadsToolbar', True);
  ToolBarDownload.Visible := cbOptionShowDownloadToolbar.Checked;

  // connection
  seOptionMaxParallel.Value := options.ReadInteger('connections', 'NumberOfTasks', 1);
  seOptionMaxThread.Value := options.ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
  seOptionMaxRetry.Value := options.ReadInteger('connections', 'Retry', 3);;
  DLManager.maxDLTasks := seOptionMaxParallel.Value;
  DLManager.maxDLThreadsPerTask := seOptionMaxThread.Value;
  DLManager.retryConnect := seOptionMaxRetry.Value;
  seOptionConnectionTimeout.Value := options.ReadInteger('connections', 'ConnectionTimeout', 15);
  OptionConnectionTimeout := seOptionConnectionTimeout.Value*1000;

  // saveto
  DLManager.compress := options.ReadInteger('saveto', 'Compress', 0);
  cbOptionPathConvert.Checked := options.ReadBool('saveto', 'PathConv', False);
  cbOptionGenerateChapterName.Checked :=
    options.ReadBool('saveto', 'GenChapName', False);
  cbOptionGenerateMangaFolderName.Checked :=
    options.ReadBool('saveto', 'GenMangaName', True);
  cbOptionAutoNumberChapter.Checked :=
    options.ReadBool('saveto', 'AutoNumberChapter', True);
  seOptionPDFQuality.Value := options.ReadInteger('saveto', 'PDFQuality', 100);
  OptionPDFQuality := seOptionPDFQuality.Value;
  edOptionCustomRename.Text :=
    options.ReadString('saveto', 'CustomRename', DEFAULT_CUSTOM_RENAME);
  if Trim(edOptionCustomRename.Text) = '' then
    edOptionCustomRename.Text := DEFAULT_CUSTOM_RENAME;
  OptionCustomRename := edOptionCustomRename.Text;
  if options.ReadBool('connections', 'UseProxy', False) then
  begin
    ProxyType := options.ReadString('connections', 'ProxyType', 'HTTP');
    Host := options.ReadString('connections', 'Host', '');
    Pass := options.ReadString('connections', 'Pass', '');
    Port := options.ReadString('connections', 'Port', '');
    User := options.ReadString('connections', 'User', '');
  end;

  // update
  cbOptionAutoCheckUpdate.Checked :=
    options.ReadBool('update', 'AutoCheckUpdate', True);
  cbOptionAutoRemoveCompletedManga.Checked :=
    options.ReadBool('update', 'AutoRemoveCompletedManga', False);
  OptionAutoRemoveCompletedManga := cbOptionAutoRemoveCompletedManga.Checked;
  cbOptionAutoCheckFavStartup.Checked :=
    options.ReadBool('update', 'AutoCheckFavStartup', True);
  OptionAutoCheckFavStartup := cbOptionAutoCheckFavStartup.Checked;
  seOptionCheckMinutes.Value := options.ReadInteger('update', 'AutoCheckMinutes', 60);
  lbOptionAutoCheckMinutes.Caption := Format(RS_LblAutoCheckNewChapterMinute,
    [seOptionCheckMinutes.Value]);
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

  cbLanguages.Items.Clear;
  uTranslation.CollectLanguagesFiles;
  if uTranslation.AvailableLanguages.Count > 0 then
    for i := 0 to AvailableLanguages.Count - 1 do
      cbLanguages.Items.Add(uTranslation.AvailableLanguages.ValueFromIndex[i]);

  cbLanguages.ItemIndex := uTranslation.AvailableLanguages.IndexOfName(
    options.ReadString('languages', 'Selected', 'en'));
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
  if (cbOptionLiveSearch.Checked = False) and (Sender = edSearch) then Exit;
  if (upcase(edSearch.Text) = LastSearchStr) and (currentWebsite = LastSearchWeb) then
    Exit;

  LastSearchWeb := currentWebsite;
  LastSearchStr := UpCase(edSearch.Text);

  vtMangaList.Clear;
  dataProcess.Search(edSearch.Text);
  vtMangaList.RootNodeCount := dataProcess.RecordCount;
  if dataProcess.Filtered then
    lbMode.Caption := Format(RS_ModeFiltered, [vtMangaList.RootNodeCount])
  else
    lbMode.Caption := Format(RS_ModeAll, [vtMangaList.RootNodeCount]);
end;

procedure TMainForm.UpdateVtChapter;
begin
  clbChapterList.Clear;
  clbChapterList.RootNodeCount := Length(ChapterList);
end;

procedure TMainForm.UpdateVtDownload;
begin
  //vtDownload.Clear;
  vtDownload.RootNodeCount := DLManager.Count;
  // the reason we put vtDownloadFilters in here instead of in DLManager because
  // the size of download list can change while this method is running
  vtDownloadFilters;
end;

procedure TMainForm.UpdateVtFavorites;
begin
  vtFavorites.Clear;
  vtFavorites.RootNodeCount := FavoriteManager.Count;
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

procedure TMainForm.SaveDropTargetFormInformation;
begin
  with options do
  begin
    WriteInteger('droptarget', 'Opacity', frmDropTarget.FAlphaBlendValue);
    WriteInteger('droptarget', 'Width', frmDropTarget.FWidth);
    WriteInteger('droptarget', 'Heigth', frmDropTarget.FHeight);
    WriteInteger('droptarget', 'Top', frmDropTarget.FTop);
    WriteInteger('droptarget', 'Left', frmDropTarget.FLeft);
    UpdateFile;
  end;
end;

procedure TMainForm.LoadLanguage;
var
  idxLanguages,
  idxFilterStatus,
  idxOptionLetFMDDo,
  idxOptionProxyType,
  idxDropTargetMode: Integer;
begin
  if uTranslation.LastSelected <> AvailableLanguages.Names[cbLanguages.ItemIndex] then
  begin
    idxLanguages := cbLanguages.ItemIndex;
    idxFilterStatus := cbFilterStatus.ItemIndex;
    idxOptionLetFMDDo := cbOptionLetFMDDo.ItemIndex;
    idxOptionProxyType := cbOptionProxyType.ItemIndex;
    idxDropTargetMode := rgDropTargetMode.ItemIndex;
    if uTranslation.SetLangByIndex(cbLanguages.ItemIndex) then
    begin
      lbOptionExternalParamsHint.Hint := Format(RS_LblOptionExternalParamsHint,
        [EXPARAM_PATH, EXPARAM_CHAPTER, EXPARAM_PATH, EXPARAM_CHAPTER]);

      cbFilterStatus.Items.Text := RS_FilterStatusItems;
      cbOptionLetFMDDo.Items.Text := RS_OptionFMDDoItems;
      rgDropTargetMode.Items.Text := RS_DropTargetModeItems;

      cbLanguages.ItemIndex := idxLanguages;
      cbFilterStatus.ItemIndex := idxFilterStatus;
      cbOptionLetFMDDo.ItemIndex := idxOptionLetFMDDo;
      cbOptionProxyType.ItemIndex := idxOptionProxyType;
      rgDropTargetMode.ItemIndex := idxDropTargetMode;
      Self.Repaint;
      vtMangaList.Repaint;
      tvDownloadFilterRepaint;
    end;
  end;
end;

procedure TMainForm.OpenWithExternalProgram(const dirPath, Filename: String);
var
  Exe, Params,
  p, f: String;
begin
  Exe := Trim(options.ReadString('general', 'ExternalProgramPath', ''));
  Params := Trim(options.ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM));

  p := Trim(TrimRightChar(Trim(dirPath), [PathDelim]));
  f := Trim(TrimChar(Trim(Filename), [PathDelim]));

  if Exe <> '' then
  begin
    if (Pos(EXPARAM_PATH + EXPARAM_CHAPTER, Params) <> 0) then
      f := PathDelim + f;
    Params := StringReplace(Params, EXPARAM_PATH, p, [rfIgnoreCase, rfReplaceAll]);
    Params := StringReplace(Params, EXPARAM_CHAPTER, f, [rfIgnoreCase, rfReplaceAll]);
    RunExternalProcess(Exe, Params, True, False);
  end
  else
  begin
    if (p <> '') and (f <> '') then
      f := p + PathDelim + f;
    OpenDocument(f);
  end;
end;

procedure TMainForm.TransferRateGraphInit(xCount: Integer);
var
  i: Integer;
begin
  TransferRateGraphList.Clear;
  TransferRateGraphList.DataPoints.NameValueSeparator := '|';
  TransferRateGraphArea.Legend.Format := FormatByteSize(0, True);
  if xCount=0 then
    TransferRateGraphInit
  else
    for i:=1 to xCount do
      TransferRateGraphList.DataPoints.Add(IntToStr(i)+'|0|?|');
end;

procedure TMainForm.TransferRateGraphAddItem(TransferRate: Integer);
var
  i: Integer;
begin
  TransferRateGraphArea.Legend.Format := FormatByteSize(TransferRate, True);
  with TransferRateGraphList.DataPoints do
  begin
    if Count=0 then
      TransferRateGraphInit;
    for i := 0 to Count - 1 do
      if i < Count - 1 then
        Strings[i] := Format('%d|%s', [i+1, ValueFromIndex[i+1]]);
    Strings[Count-1] := Format('%d|%d|?|',[Count,TransferRate]);
  end;
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
