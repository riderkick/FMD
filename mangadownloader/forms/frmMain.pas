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
  simpleipc, lclproc, types, LCLIntf, DefaultTranslator, EditBtn, PairSplitter,
  MultiLog, FileChannel, FileUtil, LazUTF8Classes, TAGraph, TASources, TASeries,
  TATools, AnimatedGif, uBaseUnit, uDownloadsManager, uFavoritesManager,
  uUpdateThread, uUpdateDBThread, uSilentThread, uMisc, uGetMangaInfosThread,
  frmDropTarget, frmAccountManager, frmWebsiteOptionCustom,
  frmWebsiteOptionAdvanced, frmCustomColor, frmLogger, CheckUpdate,
  accountmanagerdb, DBDataProcess, MangaFoxWatermark, SimpleTranslator,
  FMDOptions, httpsendthread, SimpleException;

type

  { TMainForm }

  TMainForm = class(TForm)
    appPropertiesMain: TApplicationProperties;
    btOpenLog: TBitBtn;
    btClearLogFile: TBitBtn;
    btAddToFavorites: TBitBtn;
    btCancelFavoritesCheck: TSpeedButton;
    btAbortCheckLatestVersion: TSpeedButton;
    btChecks: TSpeedButton;
    btDonate: TImage;
    btFavoritesImport: TBitBtn;
    btFilter: TBitBtn;
    btFilterReset: TBitBtn;
    btOptionApply: TBitBtn;
    btReadOnline: TBitBtn;
    btRemoveFilter: TSpeedButton;
    btMangaListSearchClear: TSpeedButton;
    btUpdateList: TSpeedButton;
    cbOptionAutoCheckFavStartup: TCheckBox;
    cbOptionAutoCheckFavInterval: TCheckBox;
    cbOptionAutoCheckFavDownload: TCheckBox;
    cbOptionAutoCheckFavRemoveCompletedManga: TCheckBox;
    cbOptionAutoOpenFavStartup: TCheckBox;
    cbOptionEnableLoadCover: TCheckBox;
    cbOptionMinimizeOnStart: TCheckBox;
    cbOptionShowBalloonHint: TCheckBox;
    cbOptionGenerateChapterFolder: TCheckBox;
    cbOptionRemoveMangaNameFromChapter: TCheckBox;
    cbOptionShowDownloadMangalistDialog: TCheckBox;
    cbOptionShowDownloadToolbar: TCheckBox;
    cbOptionShowDownloadToolbarDeleteAll: TCheckBox;
    cbOptionUpdateListNoMangaInfo: TCheckBox;
    cbOptionDigitVolume: TCheckBox;
    cbOptionDigitChapter: TCheckBox;
    cbOptionLiveSearch: TCheckBox;
    cbOptionUpdateListRemoveDuplicateLocalData : TCheckBox;
    cbUseRegExpr: TCheckBox;
    cbOptionProxyType: TComboBox;
    cbOptionOneInstanceOnly: TCheckBox;
    ckEnableLogging: TCheckBox;
    edDownloadsSearch: TEditButton;
    edFavoritesSearch: TEditButton;
    edFilterMangaInfoChapters: TEditButton;
    edLogFileName: TEditButton;
    edOptionChangeUnicodeCharacterStr: TEdit;
    edOptionDefaultPath: TEditButton;
    edOptionExternalPath: TEditButton;
    edOptionFilenameCustomRename: TEdit;
    edOptionMangaCustomRename: TEdit;
    edSaveTo: TEditButton;
    edURL: TEditButton;
    edWebsitesSearch: TEditButton;
    lbLogFileName: TLabel;
    lbOptionFilenameCustomRenameHint: TLabel;
    lbOptionFilenameCustomRename: TLabel;
    lbOptionMangaCustomRenameHint: TLabel;
    lbOptionMangaCustomRename: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    miFavoritesEnable: TMenuItem;
    miFavoritesDisable: TMenuItem;
    miChapterListDescending: TMenuItem;
    miChapterListAscending: TMenuItem;
    miMangaListDelete: TMenuItem;
    miDownloadDeleteTaskDataFavorite: TMenuItem;
    miTrayExit: TMenuItem;
    miTrayRestore: TMenuItem;
    miTrayShowDropBox: TMenuItem;
    MenuItem8: TMenuItem;
    miTrayFinishNothing: TMenuItem;
    miTrayFinishExit: TMenuItem;
    miTrayFinishShutdown: TMenuItem;
    MenuItem2: TMenuItem;
    miTrayFinishHibernate: TMenuItem;
    miTrayAfterDownloadFinish: TMenuItem;
    miTrayStopAll: TMenuItem;
    miTrayResumeAll: TMenuItem;
    miDownloadEnable: TMenuItem;
    miDownloadDisable: TMenuItem;
    miChapterListFilter: TMenuItem;
    mnFilterGenreAllIndeterminate: TMenuItem;
    mnFilterGenreAllCheck: TMenuItem;
    mnFilterGenreAllUncheck: TMenuItem;
    miChapterListHideDownloaded: TMenuItem;
    miAbortSilentThread: TMenuItem;
    mmChangelog: TMemo;
    pcInfo: TPageControl;
    psInfo: TPairSplitter;
    pssInfoList: TPairSplitterSide;
    pssInfo: TPairSplitterSide;
    psDownloads: TPairSplitter;
    pssDownloadsFilter: TPairSplitterSide;
    pssDownloads: TPairSplitterSide;
    pcMisc: TPageControl;
    pcWebsiteOptions: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel8: TPanel;
    pcAbout: TPageControl;
    pmSbMain: TPopupMenu;
    pmFilterGenreAll: TPopupMenu;
    pmTray: TPopupMenu;
    sbSaveTo: TScrollBox;
    sbWebsiteOptions: TScrollBox;
    btDownloadSplit: TSpeedButton;
    tsInfoManga: TTabSheet;
    tsinfoFilterAdv: TTabSheet;
    tsCustomColor: TTabSheet;
    tsLog: TTabSheet;
    tmAnimateMangaInfo: TTimer;
    tmBackup: TTimer;
    tmCheckFavorites: TTimer;
    tmRefreshDownloadsInfo: TTimer;
    tsWebsiteAdvanced: TTabSheet;
    tsWebsiteSelection: TTabSheet;
    tsWebsiteOptions: TTabSheet;
    tsAccounts: TTabSheet;
    tsAboutText: TTabSheet;
    tsChangelogText: TTabSheet;
    TransferRateToolset: TChartToolset;
    miFavoritesStopCheckNewChapter: TMenuItem;
    miFavoritesCheckNewChapter: TMenuItem;
    pnDownloadToolbarLeft: TPanel;
    pnDownloadToolbar: TPanel;
    TransferRateGraphArea: TAreaSeries;
    TransferRateGraph: TChart;
    ckDropTarget: TCheckBox;
    edOptionExternalParams: TEdit;
    gbDropTarget: TGroupBox;
    gbOptionExternal: TGroupBox;
    IconDL: TImageList;
    IconMed: TImageList;
    IconSmall: TImageList;
    tmExitCommand: TTimer;
    lbDefaultDownloadPath: TLabel;
    lbDropTargetOpacity: TLabel;
    lbOptionExternalParams: TLabel;
    lbOptionConnectionTimeout: TLabel;
    lbSaveTo: TLabel;
    lbOptionProxyType: TLabel;
    lbOptionRenameDigits: TLabel;
    lbFilterHint: TLabel;
    lbOptionExternal: TLabel;
    lbOptionChapterCustomRenameHint: TLabel;
    lbOptionPDFQualityHint: TLabel;
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
    btCheckLatestVersion: TBitBtn;
    btFavoritesCheckNewChapter: TBitBtn;
    btDownload: TBitBtn;
    btRemoveFilterLarge: TBitBtn;
    cbOptionAutoCheckLatestVersion: TCheckBox;
    cbOptionShowDeleteTaskDialog: TCheckBox;
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
    cbOptionChangeUnicodeCharacter: TCheckBox;
    cbOptionGenerateMangaFolder: TCheckBox;
    cbOptionMinimizeToTray: TCheckBox;
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
    edOptionChapterCustomRename: TEdit;
    edOptionHost: TEdit;
    edOptionPass: TEdit;
    edOptionPort: TEdit;
    edOptionUser: TEdit;
    edMangaListSearch: TEdit;
    gbDialogs: TGroupBox;
    gbOptionProxy: TGroupBox;
    gbOptionRenaming: TGroupBox;
    gbOptionFavorites: TGroupBox;
    IconList: TImageList;
    imCover: TImage;
    lbOptionChapterCustomRename: TLabel;
    lbOptionPDFQuality: TLabel;
    lbOptionAutoCheckFavIntervalMinutes: TLabel;
    lbOptionLetFMDDo: TLabel;
    lbOptionNewMangaTime: TLabel;
    lbOptionLanguage: TLabel;
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
    pbWait: TPaintBox;
    pmChapterList: TPopupMenu;
    pcOptions: TPageControl;
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
    seOptionAutoCheckFavIntervalMinutes: TSpinEdit;
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
    ToolBarDownload: TToolBar;
    tbDownloadResumeAll: TToolButton;
    tbDownloadStopAll: TToolButton;
    tbSeparator1: TToolButton;
    tbDownloadDeleteCompleted: TToolButton;
    tvDownloadFilter: TTreeView;
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
    procedure btAbortCheckLatestVersionClick(Sender: TObject);
    procedure btCancelFavoritesCheckClick(Sender: TObject);
    procedure btChecksClick(Sender: TObject);
    procedure btCheckLatestVersionClick(Sender: TObject);
    procedure btClearLogFileClick(Sender: TObject);
    procedure btDonateClick(Sender: TObject);
    procedure btDownloadSplitClick(Sender: TObject);
    procedure btFavoritesImportClick(Sender: TObject);
    procedure btOpenLogClick(Sender: TObject);
    procedure btReadOnlineClick(Sender: TObject);
    procedure btMangaListSearchClearClick(Sender: TObject);
    procedure btUpdateListClick(Sender: TObject);
    procedure btVisitMyBlogClick(Sender: TObject);
    procedure cbAddAsStoppedChange(Sender: TObject);
    procedure cbOptionAutoCheckFavIntervalChange(Sender: TObject);
    procedure cbOptionAutoCheckFavStartupChange(Sender: TObject);
    procedure cbOptionChangeUnicodeCharacterChange(Sender: TObject);
    procedure cbOptionDigitChapterChange(Sender: TObject);
    procedure cbOptionDigitVolumeChange(Sender: TObject);
    procedure cbOptionGenerateMangaFolderChange(Sender: TObject);
    procedure cbSelectMangaChange(Sender: TObject);
    procedure clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure clbChapterListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure clbChapterListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure edDownloadsSearchButtonClick(Sender: TObject);
    procedure edDownloadsSearchChange(Sender: TObject);
    procedure edFavoritesSearchButtonClick(Sender: TObject);
    procedure edFavoritesSearchChange(Sender: TObject);
    procedure edFilterMangaInfoChaptersButtonClick(Sender: TObject);
    procedure edFilterMangaInfoChaptersChange(Sender: TObject);
    procedure edLogFileNameButtonClick(Sender: TObject);
    procedure edMangaListSearchChange(Sender: TObject);
    procedure edMangaListSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure edOptionDefaultPathButtonClick(Sender: TObject);
    procedure edOptionExternalPathButtonClick(Sender: TObject);
    procedure edSaveToButtonClick(Sender: TObject);
    procedure edURLButtonClick(Sender: TObject);
    procedure edURLKeyPress(Sender: TObject; var Key: Char);
    procedure edWebsitesSearchButtonClick(Sender: TObject);
    procedure edWebsitesSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure btDownloadClick(Sender: TObject);
    procedure btFavoritesCheckNewChapterClick(Sender: TObject);
    procedure btOptionApplyClick(Sender: TObject);

    procedure btFilterClick(Sender: TObject);
    procedure btFilterResetClick(Sender: TObject);
    procedure btRemoveFilterClick(Sender: TObject);

    procedure cbOptionUseProxyChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miChapterListAscendingClick(Sender: TObject);
    procedure miFavoritesEnableClick(Sender: TObject);
    procedure tmAnimateMangaInfoTimer(Sender: TObject);
    procedure tmCheckFavoritesTimer(Sender: TObject);
    procedure tmExitCommandTimer(Sender: TObject);
    procedure tmRefreshDownloadsInfoStartTimer(Sender: TObject);
    procedure tmRefreshDownloadsInfoStopTimer(Sender: TObject);
    procedure tmRefreshDownloadsInfoTimer(Sender: TObject);
    procedure tmStartupTimer(Sender: TObject);
    procedure medURLCutClick(Sender: TObject);
    procedure medURLCopyClick(Sender: TObject);
    procedure medURLPasteClick(Sender: TObject);
    procedure medURLPasteandgoClick(Sender: TObject);
    procedure medtURLDeleteClick(Sender: TObject);
    procedure medURLSelectAllClick(Sender: TObject);
    procedure medURLUndoClick(Sender: TObject);
    procedure miAbortSilentThreadClick(Sender: TObject);
    procedure miChapterListFilterClick(Sender: TObject);
    procedure miChapterListHideDownloadedClick(Sender: TObject);
    procedure miDownloadEnableClick(Sender: TObject);
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
    procedure miMangaListDeleteClick(Sender: TObject);
    procedure miMangaListDownloadAllClick(Sender: TObject);
    procedure miMangaListViewInfosClick(Sender: TObject);
    procedure miFavoritesOpenFolderClick(Sender: TObject);
    procedure miDownloadOpenFolderClick(Sender: TObject);
    procedure miFavoritesOpenWithClick(Sender: TObject);
    procedure miDownloadOpenWithClick(Sender: TObject);
    procedure miTrayExitClick(Sender: TObject);
    procedure miTrayFinishNothingClick(Sender: TObject);
    procedure miTrayShowDropBoxClick(Sender: TObject);
    procedure mnDownload1ClickClick(Sender: TObject);
    procedure mnFilterGenreAllCheckClick(Sender: TObject);
    procedure mnFilterGenreAllIndeterminateClick(Sender: TObject);
    procedure mnFilterGenreAllUncheckClick(Sender: TObject);
    procedure mnUpdate1ClickClick(Sender: TObject);
    procedure mnUpdateDownFromServerClick(Sender: TObject);
    procedure mnUpdateListClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pmDownloadPopup(Sender: TObject);
    procedure pmEditURLPopup(Sender: TObject);
    procedure pmFavoritesPopup(Sender: TObject);
    procedure pmMangaListPopup(Sender: TObject);
    procedure pmSbMainPopup(Sender: TObject);
    procedure pmTrayPopup(Sender: TObject);
    procedure rgOptionCompressSelectionChanged(Sender: TObject);
    procedure sbUpdateListDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure seOptionAutoCheckFavIntervalMinutesChange(Sender: TObject);
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
    procedure vtDownloadDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtDownloadFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
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
    procedure vtDownloadKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure vtDownloadKeyDown(Sender : TObject; var Key : Word;
      Shift : TShiftState);
    procedure vtDownloadKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vtDownloadPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtFavoritesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtFavoritesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure vtFavoritesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
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
    procedure vtFavoritesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMangaListColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtMangaListGetCursor(Sender: TBaseVirtualTree;
      var ACursor: TCursor);
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
    optionMangaSiteSelectionNodes: array of PVirtualNode;
    LastSearchStr, LastSearchWeb: String;

    // state of chapterlist in mangainfo
    ChapterList: array of TChapterStateItem;

    // animation gif
    gifWaiting: TAnimatedGif;
    gifWaitingRect: TRect;

    // generate >> nodes
    procedure GeneratetvDownloadFilterNodes;

    // load about information
    procedure LoadAbout;

    procedure CloseNow;

    // en: Too lazy to add it one by one
    procedure InitCheckboxes;

    // download task filters
    procedure tvDownloadFilterRefresh(const ResourceChanged: Boolean = False);
    procedure vtDownloadUpdateFilters(const RefreshTree: Boolean = True);

    procedure AddChapterNameToList;

    // Create silent thread
    procedure AddSilentThread(URL: string; MetaDataType: TMetaDataType); overload;
    procedure AddSilentThread(URL: string); overload;

    // Add text to TRichMemo
    procedure AddTextToInfo(const ATitle, AValue: String);

    // fill edSaveTo with default path
    procedure FilledSaveTo;

    // View manga information
    procedure ViewMangaInfo(const AURL, AWebsite, ATitle: String;
      const AMangaListPos: Integer = -1);

    // Show manga information
    procedure ShowInformation(const title, website, link: String);

    // get manga list from server
    procedure RunGetList;

    // Load config from config.ini
    procedure LoadOptions;
    procedure SaveOptions;
    procedure ApplyOptions;

    // Load config from mangalist.ini
    procedure LoadMangaOptions;

    function SaveMangaOptions: String;

    procedure UpdateVtChapter;
    procedure UpdateVtDownload; inline;
    procedure UpdateVtFavorites;
    procedure UpdateVtMangaListFilterStatus;

    // load form information, like previous position, size, ...
    procedure LoadFormInformation;
    procedure SaveFormInformation;

    // drop target
    procedure ShowDropTarget(const AShow: Boolean);
    procedure SaveDropTargetFormInformation;

    // load language from file
    procedure CollectLanguagesFromFiles;
    procedure ApplyLanguage;

    // openwith
    procedure OpenWithExternalProgramChapters(const Dir: String;
      const Chapters: TStrings = nil);
    procedure OpenWithExternalProgram(const Dir, Filename: String);

    // transfer rate graph
    procedure TransferRateGraphInit(xCount: Integer = 10);
    procedure TransferRateGraphAddItem(TransferRate: Integer);

    // exit counter
    procedure DoExitWaitCounter;
    function ShowExitCounter: Boolean;

    // open db with thread
    procedure OpenDataDB(const AWebsite: String);

    // search db with thread
    procedure SearchDataDB(const ATitle: String);

    // change all filter genre checkbox state
    procedure FilterGenreChangeAllState(const AState: TCheckBoxState);

    // filter chapter list
    procedure FilterChapterList(const SearchStr: String; const HideDownloaded: Boolean);

    // exception handle
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    { public declarations }
  end;

  { TOpenDBThread }

  TOpenDBThread = class(TThread)
  private
    FWebsite: String;
  protected
    procedure SetControlEnabled(const Value: Boolean);
    procedure SyncOpenStart;
    procedure SyncOpenFinish;
    procedure Execute; override;
  public
    constructor Create(const AWebsite: String);
    destructor Destroy; override;
  end;

  { TSearchDBThread }

  TSearchDBThread = class(TThread)
  private
    FSearchStr: String;
    FNewSearch: Boolean;
  protected
    procedure SyncBeforeSearch;
    procedure SyncAfterSearch;
    procedure Execute; override;
  public
    constructor Create(const ASearchStr: String);
    destructor Destroy; override;
    procedure NewSearch(const ASearchStr: String);
  end;

  procedure AdvanceLoadHTTPConfig(const HTTP: THTTPSendThread; Website: String);

var
  MainForm: TMainForm;

const
  CL_HLBlueMarks        = $FDC594;
  CL_HLGreenMarks       = $B8FFB8;
  CL_HLRedMarks         = $8080FF;
  CL_HLYellowMarks      = $80EBFE;

  CL_BarGrayLine        = $bcbcbc;
  CL_BarGray            = $e6e6e6;

  CL_BarGreenLine       = $25b006;
  CL_BarGreen           = $42d932;

  CL_BarOrangeLine       = $00b399;
  CL_BarOrange          = $1870e9;

  CL_BarRedLine         = $1a1ab1;
  CL_BarRed             = $4b4af0;

  CL_BarBlueLine        = $b36b1d;
  CL_BarBlue            = $fab24f;

  CL_BarBlueLightLine   = $eab27c;
  CL_BarBlueLight       = $fed2a3;

  CL_BarYellowLine      = $4a4af0;
  CL_BarYellow          = $80ebfe;

  CL_BarBrownGoldLine   = $5ea2c8;
  CL_BarBrownGold       = $8dd5f0;

  CL_YellowLight        = $eaffff;
  CL_BlueLight          = $f8f8f3;

resourcestring
  RS_FilterStatusItems = 'Completed'#13#10'Ongoing'#13#10'<none>';
  RS_OptionFMDDoItems = 'Nothing'#13#10'Exit'#13#10'Shutdown'#13#10'Hibernate';
  RS_DropTargetModeItems = 'Download all'#13#10'Add to favorites';

  RS_HintFavoriteProblem = 'There is a problem with this data!'#13#10
                         + 'Removing and re-adding this data may fix the problem.';
  RS_DlgTitleExistInDLlist = 'This title are already in download list.'#13#10
                           + 'Do you want to download it anyway?';
  RS_DlgQuit = 'Are you sure you want to exit?';
  RS_DlgRemoveItem = 'Are you sure you want to delete this item(s)?';
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
  RS_DlgSplitDownload = 'Split download';
  RS_DlgDownloadCount = 'Download count:';
  RS_WrongInput = 'Invalid input!';
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
  RS_FMDAlreadyRunning = 'Free Manga Downloader already running!';
  RS_ModeSearching = 'Mode: Searching...';

implementation

{$R *.lfm}

uses
  frmImportFavorites, frmShutdownCounter, WebsiteModules, FMDVars, RegExpr,
  Clipbrd, LazFileUtils, LazUTF8;

var
  // thread for open db
  OpenDBThread: TOpenDBThread;

  // thread for search db
  SearchDBThread: TSearchDBThread;

  // ...
  UpdateStatusTextStyle: TTextStyle;

{$ifdef windows}
  PrevWndProc: windows.WNDPROC;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  if uMsg = WM_DISPLAYCHANGE then
  begin
    Screen.UpdateMonitors;
    Screen.UpdateScreen;
    if Screen.MonitorCount < MainForm.Monitor.MonitorNum then
      MainForm.DefaultMonitor := dmMainForm;
    if (MainForm.Left > Screen.Width) or (MainForm.Top > Screen.Height) then
      MainForm.MoveToDefaultPosition;
  end;
  Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;
{$endif}

procedure ChangeAllCursor(const ParentControl: TWinControl; const Cur: TCursor);
var
  i: Integer;
begin
  if ParentControl = nil then Exit;
  ParentControl.Cursor := Cur;
  if ParentControl.ControlCount > 0 then
    for i := 0 to ParentControl.ControlCount - 1 do
      ParentControl.Controls[i].Cursor := Cur;
end;

procedure AdvanceLoadHTTPConfig(const HTTP: THTTPSendThread; Website: String);
var
  s: String;
begin
  if HTTP = nil then Exit;
  if Website = '' then Exit;
  s:=Trim(advancedfile.ReadString('UserAgent',Website,''));
  if s<>'' then HTTP.UserAgent:=s;
  s:=Trim(advancedfile.ReadString('Cookies',Website,''));
  if s<>'' then HTTP.Cookies.Text:=s;
end;

{ TSearchDBThread }

procedure TSearchDBThread.SyncBeforeSearch;
begin
  with MainForm do
  begin
    vtMangaList.Cursor := crHourGlass;
    lbMode.Caption := RS_ModeSearching;
    vtMangaList.BeginUpdate;
  end;
end;

procedure TSearchDBThread.SyncAfterSearch;
begin
  with MainForm do
  begin
    vtMangaList.RootNodeCount := dataProcess.RecordCount;
    vtMangaList.EndUpdate;
    UpdateVtMangaListFilterStatus;
    LastSearchWeb := dataProcess.Website;
    LastSearchStr := UpCase(FSearchStr);
    vtMangaList.Cursor := crDefault;
  end;
end;

procedure TSearchDBThread.Execute;
begin
  if dataProcess <> nil then
  begin
    Synchronize(@SyncBeforeSearch);
    while FNewSearch do
    begin
      FNewSearch := False;
      dataProcess.Search(FSearchStr);
    end;
    if not Terminated then
      Synchronize(@SyncAfterSearch);
  end;
end;

constructor TSearchDBThread.Create(const ASearchStr: String);
begin
  FreeOnTerminate := True;
  FSearchStr := ASearchStr;
  FNewSearch := True;
  inherited Create(False);
end;

destructor TSearchDBThread.Destroy;
begin
  SearchDBThread := nil;
  inherited Destroy;
end;

procedure TSearchDBThread.NewSearch(const ASearchStr: String);
begin
  if ASearchStr <> FSearchStr then
  begin
    FSearchStr := ASearchStr;
    FNewSearch := True;
  end;
end;

{ TOpenDBThread }

procedure TOpenDBThread.SetControlEnabled(const Value: Boolean);
begin
  with MainForm do
  begin
    cbSelectManga.Enabled := Value;
    btUpdateList.Enabled := Value;
    edMangaListSearch.Enabled := Value;
    btMangaListSearchClear.Enabled := Value;
    btRemoveFilter.Enabled := Value;
  end;
end;

procedure TOpenDBThread.SyncOpenStart;
begin
  with MainForm do
  begin
    ChangeAllCursor(pssInfoList, crHourGlass);
    SetControlEnabled(False);
    lbMode.Caption := RS_Loading;
    vtMangaList.Clear;
  end;
end;

procedure TOpenDBThread.SyncOpenFinish;
begin
  with MainForm do
  begin
    LastSearchStr := upcase(edMangaListSearch.Text);
    LastSearchWeb := currentWebsite;
    if dataProcess.Filtered then
      lbMode.Caption := Format(RS_ModeFiltered, [dataProcess.RecordCount])
    else
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
    SetControlEnabled(True);
    vtMangaList.BeginUpdate;
    vtMangaList.RootNodeCount := dataProcess.RecordCount;
    vtMangaList.EndUpdate;
    ChangeAllCursor(pssInfoList, crDefault);
  end;
end;

procedure TOpenDBThread.Execute;
begin
  if (FWebsite <> '') and (dataProcess <> nil) then
  begin
    Synchronize(@SyncOpenStart);
    if dataProcess <> nil then
    begin
      dataProcess.Open(FWebsite);
      if FormMain.edMangaListSearch.Text <> '' then
        dataProcess.Search(MainForm.edMangaListSearch.Text);
    end;
    if not Terminated then
    Synchronize(@SyncOpenFinish);
  end;
end;

constructor TOpenDBThread.Create(const AWebsite: String);
begin
  FreeOnTerminate := True;
  FWebsite := AWebsite;
  inherited Create(False);
end;

destructor TOpenDBThread.Destroy;
begin
  OpenDBThread := nil;
  inherited Destroy;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  FormMain := Self;
  {$ifdef windows}
  PrevWndProc := windows.WNDPROC(windows.GetWindowLongPtr(Self.Handle, GWL_WNDPROC));
  windows.SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback));
  {$endif}
  btAbortUpdateList.Parent := sbUpdateList;
  isRunDownloadFilter := False;
  isUpdating := False;
  isPendingExitCounter:=False;
  isNormalExit:=False;
  DoAfterFMD := DO_NOTHING;
  Application.HintHidePause := 10000;
  sbUpdateList.DoubleBuffered := True;

  ForceDirectoriesUTF8(CONFIG_FOLDER);

  // load about
  LoadAbout;

  // remove old updater
  if FileExistsUTF8(FMD_DIRECTORY + 'old_' + UPDATER_EXE) then
  begin
    if FileExistsUTF8(FMD_DIRECTORY + UPDATER_EXE) then
      DeleteFileUTF8(FMD_DIRECTORY + 'old_' + UPDATER_EXE)
    else
      RenameFileUTF8(FMD_DIRECTORY + 'old_' + UPDATER_EXE, FMD_DIRECTORY + UPDATER_EXE);
  end;

  // TrayIcon
  TrayIcon.Icon.Assign(Application.Icon);
  PrevWindowState := wsNormal;

  // account
  accountmanagerdb.InitAccountManager(ACCOUNTS_FILE);

  // main dataprocess
  dataProcess := TDBDataProcess.Create;

  // downloadmanager
  DLManager := TDownloadManager.Create;
  DLManager.Restore;

  // favorites
  FavoriteManager := TFavoriteManager.Create;
  FavoriteManager.DLManager := DLManager;
  FavoriteManager.OnUpdateFavorite := @UpdateVtFavorites;
  FavoriteManager.OnUpdateDownload := @UpdateVtDownload;

  // download all / add to favorites
  SilentThreadManager := TSilentThreadManager.Create;

  // ShowInformation
  mangaInfo := TMangaInfo.Create;

  // generate tvDownloadFilter nodes
  GeneratetvDownloadFilterNodes;

  // set connection limit
  seOptionMaxParallel.MaxValue := MAX_TASKLIMIT;
  seOptionMaxThread.MaxValue := MAX_CONNECTIONPERHOSTLIMIT;

  if cbFilterStatus.Items.Count > 2 then
    cbFilterStatus.ItemIndex := 2;

  // show download list
  UpdateVtDownload;

  // show favorite list
  UpdateVtFavorites;

  InitCheckboxes;

  pcMain.ActivePage := tsDownload;

  TrayIcon.Show;

  currentJDN := GetCurrentJDN;

  // read online
  btDownload.Enabled := False;
  btDownloadSplit.Enabled := btDownload.Enabled;
  btReadOnline.Enabled := False;
  btAddToFavorites.Enabled := False;

  // waiting gif
  if FileExistsUTF8(IMAGE_FOLDER + 'waiting.gif') then
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

  //textstyle for updatestatusbar
  with UpdateStatusTextStyle do
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

  // embed form
  AccountManagerForm := TAccountManagerForm.Create(Self);
  with AccountManagerForm do begin
    Parent := tsAccounts;
    Align := alClient;
    ChildSizing.LeftRightSpacing := 0;
    ChildSizing.TopBottomSpacing := 0;
    Show;
  end;

  WebsiteOptionCustomForm := TCustomOptionForm.Create(Self);
  with WebsiteOptionCustomForm do
  begin
    Parent := sbWebsiteOptions;
    BorderStyle := bsNone;
    Align := alClient;
    Show;
  end;

  WebsiteOptionAdvancedForm := TWebsiteOptionAdvancedForm.Create(Self);
  with WebsiteOptionAdvancedForm do
  begin
    Parent := tsWebsiteAdvanced;
    BorderStyle := bsNone;
    Align := alClient;
    Show;
  end;

  CustomColorForm := TCustomColorForm.Create(Self);
  with CustomColorForm do
  begin
    Parent := tsCustomColor;
    BorderStyle := bsNone;
    Align := alClient;
    Show;
    AddVT(Self.vtMangaList);
    AddVT(Self.clbChapterList);
    AddVT(Self.vtDownload);
    AddVT(Self.vtFavorites);
    AddVT(Self.vtOptionMangaSiteSelection);
    AddVT(AccountManagerForm.vtAccountList);
    AddVT(WebsiteOptionAdvancedForm.vtCookies);
    AddVT(WebsiteOptionAdvancedForm.vtUserAgent);
    AddVT(WebsiteOptionAdvancedForm.vtDownloadMaxThreadsPerTask);
    AddVT(WebsiteOptionAdvancedForm.vtUpdateListDirectoryPageNumber);
    AddVT(WebsiteOptionAdvancedForm.vtUpdateListNumberOfThreads);
  end;

  // logger
  FormLogger := TFormLogger.Create(Self);

  // load mangafox template
  MangaFoxWatermark.SetTemplateDirectory(MANGAFOXTEMPLATE_FOLDER);

  // load configfile
  isStartup := False;
  CollectLanguagesFromFiles;
  LoadMangaOptions;
  LoadOptions;
  ApplyOptions;

  // minimize on start
  if cbOptionMinimizeOnStart.Checked then
    Application.ShowMainForm := False;

  // hint
  ShowHint := True;
  Application.HintPause := 500;
  Application.HintHidePause := 3000;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Logger.Send(Self.ClassName+'.FormClose');
  if cbOptionShowQuitDialog.Checked and (DoAfterFMD = DO_NOTHING) then
  begin
    if MessageDlg('', RS_DlgQuit, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      Logger.Send(Self.ClassName+'.FormClose aborted!');
      CloseAction := caNone;
      Exit;
    end;
  end;
  CloseNow;
  CloseAction := caFree;
end;

procedure TMainForm.CloseNow;
begin
  isExiting := True;
  {$ifdef windows}
  if Assigned(PrevWndProc) then
    windows.SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(PrevWndProc));
  {$endif}
  if FavoriteManager.isRunning then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating check favorites threads');
    FavoriteManager.StopChekForNewChapter(True);
    Logger.Send(Self.ClassName+'.CloseNow, check favorites threads terminated');
  end;
  if SilentThreadManager.Count > 0 then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating silentthreads');
    SilentThreadManager.StopAll(True);
    Logger.Send(Self.ClassName+'.CloseNow, silentthreads terminated');
  end;
  if DLManager.ItemsActiveTask.Count > 0 then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating downloads threads');
    DLManager.StopAllDownloadTasksForExit;
    Logger.Send(Self.ClassName+'.CloseNow, downlads threads terminated');
  end;
  //Terminating all threads and wait for it
  if Assigned(CheckUpdateThread) then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating CheckUpdateThread');
    CheckUpdateThread.Terminate;
    CheckUpdateThread.WaitFor;
    Logger.Send(Self.ClassName+'.CloseNow, CheckUpdateThread terminated');
  end;
  if Assigned(SearchDBThread) then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating SearchDBThread');
    SearchDBThread.Terminate;
    SearchDBThread.WaitFor;
    Logger.Send(Self.ClassName+'.CloseNow, SearchDBThread terminated');
  end;
  if Assigned(OpenDBThread) then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating OpenDBThread');
    OpenDBThread.Terminate;
    OpenDBThread.WaitFor;
    Logger.Send(Self.ClassName+'.CloseNow, OpenDBThread terminated');
  end;
  if Assigned(GetInfosThread) then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating GetInfosThread');
    try
      GetInfosThread.Terminate;
      GetInfosThread.WaitFor;
    except
    end;
    Logger.Send(Self.ClassName+'.CloseNow, GetInfosThread terminated');
  end;
  if isUpdating then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, terminating UpdateListThread');
    updateList.Terminate;
    updateList.WaitFor;
    Logger.Send(Self.ClassName+'.CloseNow, UpdateListThread terminated');
  end;

  Logger.Send(Self.ClassName+'.CloseNow, disabling all timer');
  tmBackup.Enabled := False;
  tmRefreshDownloadsInfo.Enabled := False;
  tmCheckFavorites.Enabled := False;
  tmAnimateMangaInfo.Enabled := False;
  tmExitCommand.Enabled := False;

  //Backup data
  Logger.Send(Self.ClassName+'.CloseNow, backup downloads');
  DLManager.Backup;
  Logger.Send(Self.ClassName+'.CloseNow, backup favorites');
  FavoriteManager.Backup;
  Logger.Send(Self.ClassName+'.CloseNow, backup all data to file');
  SaveOptions;
  SaveFormInformation;

  Logger.Send(Self.ClassName+'.CloseNow, closing other forms');
  //embed form
  if Assigned(AccountManagerForm) then
    AccountManagerForm.Close;

  if Assigned(FormDropTarget) then
    FormDropTarget.Close;

  if FMDInstance <> nil then
  begin
    Logger.Send(Self.ClassName+'.CloseNow, stop ipc server');
    FMDInstance.StopServer;
    FreeAndNil(FMDInstance);
  end;
  isNormalExit:=True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Logger.Send(Self.ClassName+'.FormDestroy, freeing all objects');
  SetLength(optionMangaSiteSelectionNodes, 0);
  SetLength(ChapterList, 0);
  FreeAndNil(mangaInfo);

  FreeAndNil(DLManager);
  FreeAndNil(SilentThreadManager);
  FreeAndNil(FavoriteManager);
  FreeAndNil(dataProcess);

  FreeAndNil(gifWaiting);
  FreeAndNil(mangaCover);

  if isNormalExit then
    Logger.Send(QuotedStrd(Application.Title)+' exit normally [PID:'+IntToStr(GetProcessID)+'] [HANDLE:'+IntToStr(GetCurrentProcess)+']')
  else
    Logger.SendWarning(QuotedStrd(Application.Title)+' doesn''t exit normally [PID:'+IntToStr(GetProcessID)+'] [HANDLE:'+IntToStr(GetCurrentProcess)+']');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not isStartup then
  begin
    LoadFormInformation;
    with TTimer.Create(nil) do
    begin
      OnTimer := @tmStartupTimer;
      Interval := 1000;
      Enabled := True;
    end;
  end;
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

procedure TMainForm.miChapterListAscendingClick(Sender: TObject);
var
  i, j, f: Integer;
  t: TChapterStateItem;
  Node, FNode: PVirtualNode;
  c: array of TCheckState;
begin
  if not (Sender is TMenuItem) then Exit;
  if TMenuItem(Sender).Checked then Exit;
  TMenuItem(Sender).Checked := True;
  configfile.WriteBool('general', 'SortChapterListAscending', miChapterListAscending.Checked);
  if Length(ChapterList) <> 0 then
  begin
    // invert chapterlist
    for i := Low(ChapterList) to (High(ChapterList) div 2) do
    begin
      j := High(ChapterList) - i;
      t := ChapterList[i];
      ChapterList[i] := ChapterList[j];
      ChapterList[j] := t;
    end;
    // rearrange checked state and focused
    if (clbChapterList.CheckedCount <> 0 ) or (clbChapterList.SelectedCount <> 0) then
    begin
      FNode := nil;
      if Assigned(clbChapterList.FocusedNode) then
        f := clbChapterList.FocusedNode^.Index
      else
        f := -1;
      SetLength(c, clbChapterList.RootNodeCount);
      Node := clbChapterList.GetFirst();
      while Assigned(Node) do
      begin
        c[Node^.Index] := Node^.CheckState;
        Node := clbChapterList.GetNext(Node);
      end;
      i := Low(c);
      Node := clbChapterList.GetLast();
      while Assigned(Node) do
      begin
        if i = f then
          FNode := Node;
        Node^.CheckState := c[i];
        Inc(i);
        Node := clbChapterList.GetPrevious(Node);
      end;
      SetLength(c, 0);
      if Assigned(FNode) then
        clbChapterList.FocusedNode := FNode
    end;
    clbChapterList.ClearSelection;
    clbChapterList.Repaint;
  end;
end;

procedure TMainForm.miFavoritesEnableClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  Node := vtFavorites.GetFirstSelected();
  while Assigned(Node) do
  begin
    if Sender = miFavoritesDisable then
      FavoriteManager.StopChekForNewChapter(False, Node^.Index);
    FavoriteManager[Node^.Index].Enabled := (Sender = miFavoritesEnable);
    Node := vtFavorites.GetNextSelected(Node);
  end;
  UpdateVtFavorites;
end;

procedure TMainForm.tmAnimateMangaInfoTimer(Sender: TObject);
begin
  gifWaiting.Update(pbWait.Canvas, gifWaitingRect);
end;

procedure TMainForm.tmCheckFavoritesTimer(Sender: TObject);
begin
  if IsDlgCounter then Exit;
  if OptionAutoCheckLatestVersion then
    btCheckLatestVersionClick(btCheckLatestVersion);
  FavoriteManager.isAuto := True;
  FavoriteManager.CheckForNewChapter;
end;

function TMainForm.ShowExitCounter: Boolean;
begin
  IsDlgCounter := True;
  with TShutdownCounterForm.Create(nil) do try
    case DoAfterFMD of
      DO_POWEROFF:
        begin
          WaitTimeout := 60;
          LabelMessage := RS_LblMessageShutdown;
        end;
      DO_HIBERNATE:
        begin
          WaitTimeout := 30;
          LabelMessage := RS_LblMessageHibernate;
        end;
      DO_EXIT:
        begin
          WaitTimeout := 5;
          LabelMessage := RS_LblMessageExit;
        end;
    end;
    Result := (ShowModal = mrOK);
  finally
    Free;
  end;
  isPendingExitCounter:=False;
  IsDlgCounter := False;
end;

procedure TMainForm.OpenDataDB(const AWebsite: String);
begin
  if OpenDBThread = nil then
    OpenDBThread := TOpenDBThread.Create(AWebsite);
end;

procedure TMainForm.SearchDataDB(const ATitle: String);
begin
  if SearchDBThread = nil then
    SearchDBThread := TSearchDBThread.Create(ATitle)
  else
  begin
    SearchDBThread.NewSearch(ATitle);
  end;
end;

procedure TMainForm.FilterGenreChangeAllState(const AState: TCheckBoxState);
var
  i: Integer;
begin
  for i := 0 to pnGenres.ControlCount - 1 do
    if pnGenres.Controls[i] is TCheckBox then
      TCheckBox(pnGenres.Controls[i]).State := AState;
end;

procedure TMainForm.FilterChapterList(const SearchStr: String;
  const HideDownloaded: Boolean);
var
  Node: PVirtualNode;
  S: String;
  isShow: Boolean;
begin
  if clbChapterList.RootNodeCount = 0 then Exit;
  with clbChapterList do
    try
      BeginUpdate;
      S := AnsiUpperCase(SearchStr);
      Node := GetFirst();
      while Assigned(Node) do
      begin
        isShow := True;
        if HideDownloaded then
          isShow := not ChapterList[Node^.Index].Downloaded;
        if isShow and (S <> '') then
          isShow := Pos(S, AnsiUpperCase(ChapterList[Node^.Index].Title)) <> 0;
        IsVisible[Node] := isShow;
        Node := GetNext(Node);
      end;
    finally
      EndUpdate;
    end;
end;

procedure TMainForm.tmExitCommandTimer(Sender: TObject);
begin
  tmExitCommand.Enabled := False;
  if DoAfterFMD <> DO_NOTHING then
  begin
    if DoAfterFMD in [DO_POWEROFF, DO_HIBERNATE, DO_EXIT] then
    begin
      if ShowExitCounter then
      begin
        Self.CloseNow;
        if DoAfterFMD = DO_POWEROFF then
          fmdPowerOff
        else
        if DoAfterFMD = DO_HIBERNATE then
          fmdHibernate;
        Self.Close;
      end;
    end
    else
    if DoAfterFMD = DO_UPDATE then
    begin
      if FileExistsUTF8(FMD_DIRECTORY + UPDATER_EXE) then
        CopyFile(FMD_DIRECTORY + UPDATER_EXE, FMD_DIRECTORY + 'old_' + UPDATER_EXE);
      if FileExistsUTF8(FMD_DIRECTORY + 'old_' + UPDATER_EXE) then
      begin
        Self.CloseNow;
        RunExternalProcess(FMD_DIRECTORY + 'old_' + UPDATER_EXE,
          ['-x', '-r', '3', '-a', UpdateURL, '-l', Application.ExeName,
           '--lang', SimpleTranslator.LastSelected], True, False);
        Self.Close;
      end;
    end;
    DoAfterFMD := DO_NOTHING;
  end;
end;

procedure TMainForm.tmRefreshDownloadsInfoStartTimer(Sender: TObject);
begin
  if Assigned(DLManager) then
  begin
    TransferRateGraphInit(round(TransferRateGraph.Width/4));
    TransferRateGraph.Visible := True;
  end;
end;

procedure TMainForm.tmRefreshDownloadsInfoStopTimer(Sender: TObject);
begin
  TransferRateGraph.Visible := False;
  vtDownload.Repaint;
end;

procedure TMainForm.tmRefreshDownloadsInfoTimer(Sender: TObject);
begin
  if Assigned(DLManager) then
    TransferRateGraphAddItem(DLManager.TransferRate);
  vtDownload.Repaint;
end;

procedure TMainForm.tmStartupTimer(Sender: TObject);
begin
  if not isStartup then
  begin
    isStartup := True;
    if cbSelectManga.ItemIndex > -1 then
      OpenDataDB(cbSelectManga.Items[cbSelectManga.ItemIndex]);
    if OptionAutoCheckLatestVersion then
      btCheckLatestVersionClick(btCheckLatestVersion);
    if OptionAutoCheckFavStartup then
    begin
      FavoriteManager.isAuto := True;
      FavoriteManager.CheckForNewChapter;
    end;
    DLManager.CheckAndActiveTaskAtStartup;
  end;
  if Sender is TTimer then
    TTimer(Sender).Free;
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
  edURLButtonClick(edURL);
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

procedure TMainForm.miAbortSilentThreadClick(Sender: TObject);
begin
  if Assigned(SilentThreadManager) then
    SilentThreadManager.StopAll(False);
end;

procedure TMainForm.miChapterListFilterClick(Sender: TObject);
begin
  edFilterMangaInfoChapters.Visible := miChapterListFilter.Checked;
  if edFilterMangaInfoChapters.Visible then
  begin
    clbChapterList.AnchorSide[akTop].Control := edFilterMangaInfoChapters;
    clbChapterList.AnchorSide[akTop].Side := asrBottom;
    edFilterMangaInfoChapters.SetFocus;
  end
  else
  begin
    edFilterMangaInfoChapters.Clear;
    clbChapterList.AnchorSide[akTop].Control := nil;
    clbChapterList.AnchorSide[akTop].Side := asrTop;
    clbChapterList.Top := 0;
  end;
end;

procedure TMainForm.miChapterListHideDownloadedClick(Sender: TObject);
begin
  if Sender = miChapterListHideDownloaded then
  begin
    miChapterListHideDownloaded.Checked := not miChapterListHideDownloaded.Checked;
    configfile.WriteBool('general', 'ChapterListHideDownloaded', miChapterListHideDownloaded.Checked);
  end;

  FilterChapterList(edFilterMangaInfoChapters.Text, miChapterListHideDownloaded.Checked);
end;

procedure TMainForm.miDownloadEnableClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vtDownload.SelectedCount = 0 then Exit;
  Node := vtDownload.GetFirstSelected();
  while Assigned(Node) do
  begin
    if Sender = miDownloadEnable then
      DLManager.EnableTask(Node^.Index)
    else
      DLManager.DisableTask(Node^.Index);
    Node := vtDownload.GetNextSelected(Node);
  end;
  UpdateVtDownload;
end;

procedure TMainForm.miDownloadViewMangaInfoClick(Sender: TObject);
begin
  if Assigned(vtDownload.FocusedNode) then
    with DLManager.Items[vtDownload.FocusedNode^.Index].DownloadInfo do
      ViewMangaInfo(Link, Website, Title);
end;

procedure TMainForm.miChapterListHighlightClick(Sender: TObject);
begin
  if Sender = miChapterListHighlight then
  begin
    miChapterListHighlight.Checked := not miChapterListHighlight.Checked;
    configfile.WriteBool('general', 'HighlightDownloadedChapters', miChapterListHighlight.Checked);
  end;
  if Length(ChapterList) = 0 then Exit;
  if miChapterListHighlight.Checked then
    DLManager.GetDownloadedChaptersState(mangaInfo.website + mangaInfo.link,
      ChapterList)
  else
    ClearChapterListState;
  clbChapterList.Repaint;
end;

procedure TMainForm.miDownloadDeleteTaskClick(Sender: TObject);
var
  xNode: PVirtualNode;
  i: Integer;
  f: String;
begin
  if vtDownload.SelectedCount = 0 then Exit;
  if DLManager.Count = 0 then Exit;
  if (cbOptionShowDeleteTaskDialog.Checked) then
    if MessageDlg('', RS_DlgRemoveTask,
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  vtDownload.BeginUpdate;
  try
    EnterCriticalSection(DLManager.CS_Task);
    // stop selected nodes
    xNode := vtDownload.GetPreviousSelected(nil);
    while Assigned(xNode) do
    begin
      with DLManager.Items[xNode^.Index] do
        if ThreadState then
        begin
          Task.IsForDelete := True;
          Task.Terminate;
        end;
      xNode := vtDownload.GetPreviousSelected(xNode);
    end;
    // cleaning the data
    xNode := vtDownload.GetPreviousSelected(nil);
    while Assigned(xNode) do
    begin
      Exclude(xNode^.States, vsSelected);
      with DLManager.Items[xNode^.Index] do
      begin
        if ThreadState then
          Task.WaitFor;
        if (Sender = miDownloadDeleteTaskData) or (Sender = miDownloadDeleteTaskDataFavorite)
          and (ChapterName.Count > 0) then begin
          for i := 0 to ChapterName.Count - 1 do begin
            f := CorrectPathSys(DownloadInfo.SaveTo + ChapterName[i]);
            if FileExistsUTF8(f + '.zip') then
              DeleteFileUTF8(f + '.zip')
            else if FileExistsUTF8(f + '.cbz') then
              DeleteFileUTF8(f + '.cbz')
            else if FileExistsUTF8(f + '.pdf') then
              DeleteFileUTF8(f + '.pdf')
            else if DirectoryExistsUTF8(f) then
              DeleteDirectory(f, False);
          end;
          RemoveDirUTF8(DownloadInfo.SaveTo);
        end;
        if (Sender = miDownloadDeleteTaskDataFavorite) and
          (FavoriteManager.Items.Count <> 0) and
          (FavoriteManager.isRunning = False) then
          try
            FavoriteManager.Lock;
            for i := 0 to FavoriteManager.Count - 1 do
            begin
              if SameText(DLManager[xNode^.Index].DownloadInfo.Link, FavoriteManager[i].FavoriteInfo.Link)
                and SameText(DLManager[xNode^.Index].DownloadInfo.Website, FavoriteManager[i].FavoriteInfo.Website) then
                begin
                  FavoriteManager.FreeAndDelete(i);
                  Break;
                end;
            end;
          finally
            FavoriteManager.LockRelease;
          end;
        DLManager.FreeAndDelete(xNode^.Index);
      end;
      xNode := vtDownload.GetPreviousSelected(xNode);
    end;
  finally
    LeaveCriticalSection(DLManager.CS_Task);
  end;
  vtDownload.RootNodeCount := DLManager.Items.Count;
  vtDownload.EndUpdate;
  UpdateVtFavorites;
  UpdateVtDownload;
  DLManager.CheckAndActiveTask();
  Exit;
end;

procedure TMainForm.miDownloadMergeCompletedClick(Sender: TObject);
var
  i, j: Cardinal;
  ic, jc: TTaskContainer;
  // merge all finished tasks that have same manga name, website and directory
begin
  i:=DLManager.Count-1;
  while i>0 do begin
    ic:=DLManager.Items[i];
    if ic.Status=STATUS_FINISH then
    begin
      j:=i-1;
      while j>0 do begin
        jc:=DLManager.Items[j];
        if (i<>j) and
          (jc.Status = STATUS_FINISH) and
          SameText(ic.DownloadInfo.title,jc.DownloadInfo.title) and
          SameText(ic.DownloadInfo.website,jc.DownloadInfo.website) and
          SameText(ic.DownloadInfo.saveTo,jc.DownloadInfo.saveTo) then
        begin
          ic.ChapterLinks.Text:=jc.ChapterLinks.Text+ic.ChapterLinks.Text;
          ic.ChapterName.Text:=jc.ChapterName.Text+ic.ChapterName.Text;
          ic.DownloadInfo.dateTime:=jc.DownloadInfo.dateTime;
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
  if vtFavorites.SelectedCount = 0 then Exit;
  SilentThreadManager.BeginAdd;
  try
    xNode := vtFavorites.GetFirstSelected;
    for i := 0 to vtFavorites.SelectedCount - 1 do
    begin
      if vtFavorites.Selected[xNode] then
        with FavoriteManager.Items[xNode^.Index].FavoriteInfo do
          SilentThreadManager.Add(MD_DownloadAll, Website, Title, Link, SaveTo);
      xNode := vtFavorites.GetNextSelected(xNode);
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
  SilentThreadManager.EndAdd;
end;

procedure TMainForm.miFavoritesStopCheckNewChapterClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  xNode := vtFavorites.GetFirstSelected;
  while Assigned(xNode) do
  begin
    FavoriteManager.StopChekForNewChapter(False, xNode^.Index);
    xNode := vtFavorites.GetNextSelected(xNode);
  end;
  UpdateVtFavorites;
end;

procedure TMainForm.miFavoritesViewInfosClick(Sender: TObject);
begin
  if Assigned(vtFavorites.FocusedNode) then
    with FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo do
      ViewMangaInfo(Link, Website, Title);
end;

procedure TMainForm.miHighlightNewMangaClick(Sender: TObject);
begin
  miHighlightNewManga.Checked := not miHighlightNewManga.Checked;
  configfile.WriteBool('general', 'HighLightNewManga', miHighlightNewManga.Checked);
  vtMangaList.Repaint;
end;

procedure TMainForm.LoadAbout;
var
  i: Integer;
  fs: TFileStreamUTF8;
  st: TStringList;
  regx: TRegExpr;
begin
  // load readme.rtf
  if FileExistsUTF8(README_FILE) then begin
    regx := TRegExpr.Create;
    st := TStringList.Create;
    try
      regx.ModifierI := True;
      regx.Expression := '(version.*)((\d+\.){3}\d+)';
      st.LoadFromFile(README_FILE);
      if st.Count > 0 then
        for i := 0 to st.Count - 1 do
          if regx.Exec(st[i]) then
          begin
            if regx.Match[2] <> FMD_VERSION_NUMBER then begin
              st[i] := regx.Replace(st[i], '$1\' + FMD_VERSION_NUMBER, True);
              if DeleteFileUTF8(README_FILE) then
                st.SaveToFile(README_FILE);
            end;
            Break;
          end;
    finally
      st.Free;
      regx.Free;
    end;
    fs := TFileStreamUTF8.Create(README_FILE, fmOpenRead or fmShareDenyNone);
    try
      rmAbout.LoadRichText(fs);
    finally
      fs.free;
    end;
  end;
  // load changelog.txt
  if FileExistsUTF8(CHANGELOG_FILE) then  mmChangelog.Lines.LoadFromFile(CHANGELOG_FILE);
end;

procedure TMainForm.GeneratetvDownloadFilterNodes;

  function Add(const ParentNode: TTreeNode; const S: String;
    const ImgIdx: Integer = -1): TTreeNode;
  begin
    if Assigned(ParentNode) then
      Result := tvDownloadFilter.Items.AddChild(ParentNode, S)
    else
      Result := tvDownloadFilter.Items.Add(nil, S);
    with Result do
    begin
      ImageIndex := ImgIdx;
      SelectedIndex := ImgIdx;
      StateIndex := ImgIdx;
    end;
  end;

var
  Node: TTreeNode;

begin
  with tvDownloadFilter do begin
    Items.Clear;

    // download
    Node := Add(nil, RS_AllDownloads, 4);
    Add(Node, RS_Finish, 5);
    Add(Node, RS_InProgress, 6);
    Add(Node, RS_Stopped, 7);
    Add(Node, RS_Failed, 16);
    Add(Node, RS_Disabled, 22);

    // history
    Node := Add(nil, RS_History, 4);
    Add(Node, RS_Today, 8);
    Add(Node, RS_Yesterday, 8);
    Add(Node, RS_OneWeek, 8);
    Add(Node, RS_OneMonth, 8);

    Items[configfile.ReadInteger('general', 'DownloadFilterSelect', 0)].Selected := True;
  end;
end;

procedure TMainForm.btDownloadClick(Sender: TObject);
var
  links,names:TStrings;
  node:PVirtualNode;
  s:String;
  c,p,r,i,j,k,l, newdl:Integer;
begin
  if clbChapterList.CheckedCount = 0 then
    Exit;
  links:=TStringList.Create;
  names:=TStringList.Create;
  try
    node:=clbChapterList.GetFirstChecked();
    while Assigned(node) do
    begin
      if (vsVisible in node^.States) then
      begin
        links.Add(ChapterList[node^.Index].Link);
        s:=CustomRename(OptionChapterCustomRename,
          mangaInfo.website,
          mangaInfo.title,
          mangaInfo.authors,
          mangaInfo.artists,
          ChapterList[node^.Index].Title,
          Format('%.4d',[ChapterList[node^.Index].Index]),
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);
        names.Add(s);
        ChapterList[node^.Index].Downloaded:=True;
        clbChapterList.ReinitNode(node,False);
      end;
      node:=clbChapterList.GetNextChecked(node);
    end;
    clbChapterList.Repaint;
    if links.Count<>0 then
    begin
      s:=edSaveTo.Text;
      if OptionGenerateMangaFolder then
        s:=AppendPathDelim(s)+CustomRename(
          OptionMangaCustomRename,
          mangaInfo.website,
          mangaInfo.title,
          mangaInfo.authors,
          mangaInfo.artists,
          '',
          '',
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);
      c:=1;
      p:=links.Count;
      r:=0;
      if btDownload.Tag>=links.Count then
      begin
        c:=links.Count;
        p:=1;
      end
      else
      if btDownload.Tag>1 then
      begin
        c:=btDownload.Tag;
        p:=links.Count div c;
        r:=links.Count mod c;
      end;
      btDownload.Tag:=0;
      k:=0;
      for i:=1 to c do
      begin
        if i<=r then
          l:=p+1
        else
        if i=c then
          l:=links.Count-k
        else
          l:=p;
        newdl := DLManager.AddTask;
        with DLManager[newdl] do
        begin
          for j:=1 to l do
          begin
            ChapterLinks.Add(links[k]);
            ChapterName.Add(names[k]);
            Inc(k);
          end;
          if cbAddAsStopped.Checked then
          begin
            DownloadInfo.Status:=Format('[%d/%d] %s',[0,ChapterLinks.Count,RS_Stopped]);
            Status:=STATUS_STOP;
          end
          else
          begin
            DownloadInfo.Status:=Format('[%d/%d] %s',[0,ChapterLinks.Count,RS_Waiting]);
            Status:=STATUS_WAIT;
          end;
          Website:=mangaInfo.website;
          DownloadInfo.Website:=mangaInfo.website;
          DownloadInfo.Link:=mangaInfo.link;
          DownloadInfo.Title:=mangaInfo.title;
          DownloadInfo.DateTime:=Now;
          DownloadInfo.SaveTo:=s;
          CurrentDownloadChapterPtr:=0;
          SaveToDB(newdl);
        end;
      end;
      DLManager.DownloadedChapters.Chapters[mangaInfo.website+mangaInfo.link]:=links.Text;
      FavoriteManager.AddToDownloadedChaptersList(mangaInfo.website,mangaInfo.link,links);
      DLManager.CheckAndActiveTask;
      pcMain.ActivePage:=tsDownload;
      UpdateVtDownload;
    end;
  finally
    links.Free;
    names.Free;
  end;
end;

procedure TMainForm.btAddToFavoritesClick(Sender: TObject);
var
  s: String;
begin
  if mangaInfo.title <> '' then
  begin
    // save to
    s := edSaveTo.Text;
    if OptionGenerateMangaFolder then
      s := AppendPathDelim(s) + CustomRename(
          OptionMangaCustomRename,
          mangaInfo.website,
          mangaInfo.title,
          mangaInfo.authors,
          mangaInfo.artists,
          '',
          '',
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);

    FavoriteManager.Add(mangaInfo.title, IntToStr(mangaInfo.numChapter), mangaInfo.chapterLinks.Text,
      mangaInfo.website, CleanAndExpandDirectory(s), mangaInfo.link);
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

procedure TMainForm.btAbortCheckLatestVersionClick(Sender: TObject);
begin
  if Assigned(CheckUpdateThread) then
    CheckUpdateThread.Terminate;
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
  MessageDlg(Application.Title, RS_FMDAlreadyRunning, mtWarning, [mbOK], 0);
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

procedure TMainForm.btVisitMyBlogClick(Sender: TObject);
begin
  OpenURL('http://akarink.wordpress.com/');
end;

procedure TMainForm.cbAddAsStoppedChange(Sender: TObject);
begin
  configfile.WriteBool('general', 'AddAsStopped', cbAddAsStopped.Checked);
end;

procedure TMainForm.cbOptionAutoCheckFavIntervalChange(Sender: TObject);
begin
  seOptionAutoCheckFavIntervalMinutes.Enabled := cbOptionAutoCheckFavInterval.Checked;
  lbOptionAutoCheckFavIntervalMinutes.Enabled := cbOptionAutoCheckFavInterval.Checked;
end;

procedure TMainForm.cbOptionAutoCheckFavStartupChange(Sender: TObject);
begin
  cbOptionAutoOpenFavStartup.Enabled := cbOptionAutoCheckFavStartup.Checked;
end;

procedure TMainForm.cbOptionChangeUnicodeCharacterChange(Sender: TObject);
begin
  edOptionChangeUnicodeCharacterStr.Enabled := cbOptionChangeUnicodeCharacter.Checked;
end;

procedure TMainForm.cbOptionDigitChapterChange(Sender: TObject);
begin
  seOptionDigitChapter.Enabled := cbOptionDigitChapter.Checked;
end;

procedure TMainForm.cbOptionDigitVolumeChange(Sender: TObject);
begin
  seOptionDigitVolume.Enabled := cbOptionDigitVolume.Checked;
end;

procedure TMainForm.cbOptionGenerateMangaFolderChange(Sender: TObject);
begin
  edOptionMangaCustomRename.Enabled := cbOptionGenerateMangaFolder.Checked;
  lbOptionMangaCustomRename.Enabled := edOptionMangaCustomRename.Enabled;
  lbOptionMangaCustomRenameHint.Enabled := edOptionMangaCustomRename.Enabled;
end;

procedure TMainForm.btReadOnlineClick(Sender: TObject);
begin
  OpenURL(mangaInfo.url);
end;

procedure TMainForm.btMangaListSearchClearClick(Sender: TObject);
begin
  edMangaListSearch.Tag := 1;
  edMangaListSearch.Clear;
end;

procedure TMainForm.btCheckLatestVersionClick(Sender: TObject);
begin
  if Assigned(CheckUpdateThread) then
    MessageDlg('', RS_DlgUpdaterIsRunning, mtInformation, [mbYes], 0)
  else
    CheckUpdateThread := TCheckUpdateThread.Create;
end;

procedure TMainForm.btClearLogFileClick(Sender: TObject);
var
  F: TextFile;
begin
  if FileExistsUTF8(edLogFileName.Text) then
  begin
    system.Assign(F, edLogFileName.Text);
    Rewrite(F);
    CloseFile(F);
  end;
end;

procedure TMainForm.btDonateClick(Sender: TObject);
begin
  OpenURL('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=akarin.km@gmail.com&item_name=Donation+to+Free+Manga+Downloader');
end;

procedure TMainForm.btDownloadSplitClick(Sender: TObject);
var
  s:String='';
  c:Integer=-1;
begin
  if InputQuery(RS_DlgSplitDownload,RS_DlgDownloadCount,s) and (s<>'') then
  begin
    c:=StrToIntDef(s,-1);
    if c<=0 then
      MessageDlg(RS_WrongInput,mtError,[mbOK],0)
    else
    begin
      btDownload.Tag:=c;
      btDownloadClick(btDownload);
    end;
  end;
end;

procedure TMainForm.btFavoritesImportClick(Sender: TObject);
begin
  with TImportFavorites.Create(Self) do try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.btOpenLogClick(Sender: TObject);
begin
  FormLogger.Show;
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
  configfile.WriteInteger('form', 'SelectManga', cbSelectManga.ItemIndex);
  if currentWebsite <> cbSelectManga.Items[cbSelectManga.ItemIndex] then
  begin
    currentWebsite := cbSelectManga.Items[cbSelectManga.ItemIndex];
    vtMangaList.Clear;
    if dataProcess = nil then
      dataProcess := TDBDataProcess.Create
    else
    if dataProcess.Connected then
      dataProcess.Close;
    lbMode.Caption := Format(RS_ModeAll, [0]);
    if DataFileExist(cbSelectManga.Items[cbSelectManga.ItemIndex]) then
    begin
      OpenDataDB(cbSelectManga.Items[cbSelectManga.ItemIndex]);
    end
    else
    if cbOptionShowDownloadMangalistDialog.Checked then
      RunGetList;
  end;
end;

procedure TMainForm.clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if CellPaintMode <> cpmPaint then Exit;
  if Node^.Index>=Length(ChapterList) then Exit;
  if ChapterList[Node^.Index].Downloaded then
  begin
    TargetCanvas.Brush.Color:=CL_CHDownloaded;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TMainForm.clbChapterListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Node^.Index>=Length(ChapterList) then Exit;
  if Length(ChapterList)=1 then
    CellText:=ChapterList[Node^.Index].Title
  else
    CellText:=Format('%.4d - %s',[ChapterList[Node^.Index].Index, ChapterList[Node^.Index].Title]);
end;

procedure TMainForm.clbChapterListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if Assigned(Node) then Node^.CheckType:=ctCheckBox;
end;

procedure TMainForm.edDownloadsSearchButtonClick(Sender: TObject);
begin
  edDownloadsSearch.Clear;
end;

procedure TMainForm.edDownloadsSearchChange(Sender: TObject);
var
  Node: PVirtualNode;
  S: String;
begin
  if vtDownload.RootNodeCount = 0 then Exit;
  with vtDownload do
    try
      BeginUpdate;
      if (edDownloadsSearch.Text = '') and (VisibleCount <> RootNodeCount) then
      begin
        Node := GetFirst();
        while Assigned(Node) do
        begin
          IsVisible[Node] := DLManager[Node^.Index].Visible;
          Node := GetNext(Node);
        end;
      end
      else
      begin
        S := AnsiUpperCase(edDownloadsSearch.Text);
        Node := GetFirst();
        while Assigned(Node) do
        begin
          if DLManager[Node^.Index].Visible then
            IsVisible[Node] := Pos(S, AnsiUpperCase(DLManager[Node^.Index].DownloadInfo.Title)) > 0;
          Node := GetNext(Node);
        end;
      end;
    finally
      EndUpdate;
    end;
end;

procedure TMainForm.edFavoritesSearchButtonClick(Sender: TObject);
begin
  edFavoritesSearch.Clear;
end;

procedure TMainForm.edFavoritesSearchChange(Sender: TObject);
var
  Node: PVirtualNode;
  S: String;
begin
  if vtFavorites.RootNodeCount = 0 then Exit;
  with vtFavorites do
    try
      BeginUpdate;
      if (edFavoritesSearch.Text = '') and (VisibleCount <> RootNodeCount) then
      begin
        Node := GetFirst();
        while Assigned(Node) do
        begin
          IsVisible[Node] := True;
          Node := GetNext(Node);
        end;
      end
      else
      begin
        S := AnsiUpperCase(edFavoritesSearch.Text);
        Node := GetFirst();
        while Assigned(Node) do
        begin
          IsVisible[Node] := Pos(S, AnsiUpperCase(FavoriteManager[Node^.Index].FavoriteInfo.Title)) > 0;
          Node := GetNext(Node);
        end;
      end;
    finally
      EndUpdate;
    end;
end;

procedure TMainForm.edFilterMangaInfoChaptersButtonClick(Sender: TObject);
begin
  edFilterMangaInfoChapters.Clear;
end;

procedure TMainForm.edFilterMangaInfoChaptersChange(Sender: TObject);
begin
  FilterChapterList(edFilterMangaInfoChapters.Text, miChapterListHideDownloaded.Checked);
end;

procedure TMainForm.edLogFileNameButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      InitialDir := ExtractFileDir(edLogFileName.Text);
      if Execute then
        edLogFileName.Text := FileName;
    finally
      Free;
    end;
end;

procedure TMainForm.edURLKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    edURLButtonClick(edURL);
end;

procedure TMainForm.edWebsitesSearchButtonClick(Sender: TObject);
begin
  edWebsitesSearch.Clear;
end;

procedure TMainForm.edWebsitesSearchChange(Sender: TObject);
var
  s: String;
  lcount: Integer;
  data: PSingleItem;
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
      vtMangaList.RootNodeCount := dataProcess.RecordCount;
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
      edMangaListSearch.Tag := -1;
      edMangaListSearch.Clear;
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
  checkGenres,
  uncheckGenres: TStringList;
  i: Integer;
  s: String;
begin
  Screen.Cursor := crHourGlass;
  checkGenres := TStringList.Create;
  uncheckGenres := TStringList.Create;
  try
    edCustomGenres.Text := Trim(edCustomGenres.Text);
    if cbUseRegExpr.Checked and (edCustomGenres.Text <> '') then
      checkGenres.Add(edCustomGenres.Text)
    else
    begin
      ExtractStrings([','], [], PChar(edCustomGenres.Text), checkGenres);
      TrimStrings(checkGenres);
      i := 0;
      while i < checkGenres.Count do begin
        s := Trim(checkGenres.Strings[i]);
        if (s <> '') and (s[1] = '-') or (s[1] = '!') then begin
          Delete(s, 1, 1);
          uncheckGenres.Add(s);
          checkGenres.Delete(i);
        end
        else Inc(i);
      end;
    end;

    if pnGenres.ControlCount > 0 then
      for i := 0 to pnGenres.ControlCount - 1 do
        if pnGenres.Controls[i] is TCheckBox then begin
          if TCheckBox(pnGenres.Controls[i]).State = cbChecked then
            checkGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption)
          else
          if TCheckBox(pnGenres.Controls[i]).State = cbUnchecked then
            uncheckGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption);
        end;

    if dataProcess.CanFilter(
      checkGenres,
      uncheckGenres,
      edFilterTitle.Text,
      edFilterAuthors.Text,
      edFilterArtists.Text,
      IntToStr(cbFilterStatus.ItemIndex),
      edFilterSummary.Text,
      OptionNewMangaTime,
      rbAll.Checked,
      cbOnlyNew.Checked) then
    begin
      dataProcess.FilterAllSites := cbSearchFromAllSites.Checked;
      if cbSearchFromAllSites.Checked then
        dataProcess.SitesList.Assign(cbSelectManga.Items);

      edMangaListSearch.Tag := -1;
      edMangaListSearch.Clear;
      vtMangaList.Clear;

      dataProcess.Filter(
        checkGenres,
        uncheckGenres,
        edFilterTitle.Text,
        edFilterAuthors.Text,
        edFilterArtists.Text,
        IntToStr(cbFilterStatus.ItemIndex),
        edFilterSummary.Text,
        OptionNewMangaTime,
        rbAll.Checked,
        cbOnlyNew.Checked,
        cbUseRegExpr.Checked);
    end;
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
  if vtMangaList.SelectedCount = 0 then Exit;
  SilentThreadManager.BeginAdd;
  try
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
  finally
    SilentThreadManager.EndAdd;
  end;
end;

// ----- vtFavorites popup menu -----

procedure TMainForm.miFavoritesDeleteClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  if FavoriteManager.isRunning then begin
    MessageDlg('', RS_DlgFavoritesCheckIsRunning,
      mtInformation, [mbOK], 0);
    Exit;
  end;
  if cbOptionShowDeleteTaskDialog.Checked then
    if MessageDlg('', RS_DlgRemoveFavorite, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;

  xNode := vtFavorites.GetLast();
  while Assigned(xNode) do begin
    if vtFavorites.Selected[xNode] then
      FavoriteManager.Remove(xNode^.Index, False);
    xNode := vtFavorites.GetPreviousSelected(xNode);
  end;
  FavoriteManager.Backup;
  UpdateVtFavorites;
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
  s := FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.currentChapter;
  repeat
    if InputQuery('', RS_DlgTypeInNewChapter, s) then
  until TryStrToInt(s, i);
  if s <> FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.currentChapter then
  begin
    FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.currentChapter := s;
    UpdateVtFavorites;
    FavoriteManager.Backup;
  end;
end;

procedure TMainForm.miFavoritesChangeSaveToClick(Sender: TObject);
var
  s: String;
begin
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', RS_DlgFavoritesCheckIsRunning,
      mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  if not Assigned(vtFavorites.FocusedNode) then
    Exit;
  s := '';
  with TSelectDirectoryDialog.Create(Self) do
    try
      InitialDir := FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo;
      if Execute then
        s := FileName;
    finally
      Free;
    end;

  if s <> '' then
  begin
    FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo := s;
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
  if clbChapterList.SelectedCount > 0 then
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
      updateList := TUpdateListManagerThread.Create;
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

procedure TMainForm.mnFilterGenreAllCheckClick(Sender: TObject);
begin
  FilterGenreChangeAllState(cbChecked);
end;

procedure TMainForm.mnFilterGenreAllIndeterminateClick(Sender: TObject);
begin
  FilterGenreChangeAllState(cbGrayed);
end;

procedure TMainForm.mnFilterGenreAllUncheckClick(Sender: TObject);
begin
  FilterGenreChangeAllState(cbUnchecked);
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
      updateList := TUpdateListManagerThread.Create;
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
      updateList := TUpdateListManagerThread.Create;
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
  // the reason we put it in here instead of in DLManager because of the size of
  // download list will change during this method
end;

procedure TMainForm.miDownloadResumeClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtDownload.SelectedCount > 0 then begin
    xNode := vtDownload.GetFirstSelected();
    while Assigned(xNode) do begin
      DLManager.SetTaskActive(xNode^.Index);
      xNode := vtDownload.GetNextSelected(xNode);
    end;
    DLManager.CheckAndActiveTask();
    UpdateVtDownload;
  end;
end;

procedure TMainForm.miDownloadStopClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtDownload.SelectedCount > 0 then begin
    xNode := vtDownload.GetFirstSelected();
    while Assigned(xNode) do begin
      DLManager.StopTask(xNode^.Index, False);
      xNode := vtDownload.GetNextSelected(xNode);
    end;
    DLManager.CheckAndActiveTask();
    UpdateVtDownload;
  end;
end;

procedure TMainForm.miMangaListDeleteClick(Sender: TObject);
var
  Node: PVirtualNode;
  DeleteCount: Integer;
begin
  if vtMangaList.SelectedCount = 0 then Exit;
  if dataProcess.Table.Active = False then Exit;
  if MessageDlg('', RS_DlgRemoveItem, mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
  try
    vtMangaList.BeginUpdate;
    DeleteCount := 0;
    Node := vtMangaList.GetPreviousSelected(nil);
    while Assigned(Node) do
    begin
      if dataProcess.DeleteData(Node^.Index) then
        Inc(DeleteCount);
      Node := vtMangaList.GetPreviousSelected(Node);
    end;
    dataProcess.Table.ApplyUpdates;
    dataProcess.Table.SQLTransaction.CommitRetaining;
    if DeleteCount <> 0 then
    begin
      vtMangaList.ClearSelection;
      vtMangaList.RootNodeCount := dataProcess.RecordCount;
      UpdateVtMangaListFilterStatus;
    end;
  finally
    vtMangaList.EndUpdate;
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
  if vtMangaList.SelectedCount = 0 then Exit;

  SilentThreadManager.BeginAdd;
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
              DLManager.Items[j].DownloadInfo.title then
            begin
              if YesAll then
                AllowedToCreate := True
              else if NoAll then
                AllowedToCreate := False
              else
              begin
                pcMain.ActivePage := tsDownload;
                mResult := MessageDlg('', DLManager.Items[j].DownloadInfo.title +
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
  SilentThreadManager.EndAdd;
end;

procedure TMainForm.miMangaListViewInfosClick(Sender: TObject);
begin
  if Assigned(vtMangaList.FocusedNode) then
    ViewMangaInfo(DataProcess.Value[vtMangaList.FocusedNode^.Index, DATA_PARAM_LINK],
      DataProcess.WebsiteName[vtMangaList.FocusedNode^.Index],
      DataProcess.Value[vtMangaList.FocusedNode^.Index, DATA_PARAM_TITLE],
      vtMangaList.FocusedNode^.Index);
end;

procedure TMainForm.miFavoritesOpenFolderClick(Sender: TObject);
begin
  if Assigned(vtFavorites.FocusedNode) then
    OpenDocument(ChompPathDelim(
      FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo));
end;

procedure TMainForm.miDownloadOpenFolderClick(Sender: TObject);
begin
  if Assigned(vtDownload.FocusedNode) then
    OpenDocument(ChompPathDelim(
      DLManager.Items[vtDownload.FocusedNode^.Index].DownloadInfo.SaveTo));
end;

procedure TMainForm.miFavoritesOpenWithClick(Sender: TObject);
begin
  if Assigned(vtFavorites.FocusedNode) then
     OpenWithExternalProgramChapters(
       FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo);
end;

procedure TMainForm.miDownloadOpenWithClick(Sender: TObject);
begin
  if Assigned(vtDownload.FocusedNode) then
    with DLManager.Items[vtDownload.FocusedNode^.Index] do
      OpenWithExternalProgramChapters(DownloadInfo.SaveTo, ChapterName);
end;

procedure TMainForm.miTrayExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TMainForm.miTrayFinishNothingClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    OptionLetFMDDo := TFMDDo(TMenuItem(Sender).Tag);
    configfile.WriteInteger('general', 'LetFMDDo', Integer(OptionLetFMDDo));
  end;
end;

procedure TMainForm.miTrayShowDropBoxClick(Sender: TObject);
begin
  ShowDropTarget(TMenuItem(Sender).Checked);
end;

procedure TMainForm.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsFavorites then
    vtFavorites.Repaint
  else if pcMain.ActivePage = tsOption then
    LoadOptions;
end;

procedure TMainForm.pmDownloadPopup(Sender: TObject);
var
  iStop,
  iResume,
  iEnable,
  iDisable: Boolean;

  procedure ScanTasks;
  var
    Node: PVirtualNode;
  begin
    iStop := False;
    iResume := False;
    iEnable := False;
    iDisable := False;
    Node := vtDownload.GetFirstSelected();
    while Assigned(Node) do
    begin
      if DLManager[Node^.Index].Enabled then
      begin
        if not iDisable then
          iDisable := True;
        case DLManager[Node^.Index].Status of
          STATUS_DOWNLOAD,
          STATUS_PREPARE,
          STATUS_WAIT     : if not iStop then iStop := True;
          STATUS_STOP,
          STATUS_FAILED,
          STATUS_PROBLEM  : if not iResume then iResume := True;
        end;
      end
      else if not iEnable then
        iEnable := True;
      if iStop and iResume and iStop and iEnable and iDisable then
        Break;
      Node := vtDownload.GetNextSelected(Node);
    end;
  end;

begin
  miDownloadDeleteCompleted.Enabled := True;
  miDownloadMergeCompleted.Enabled := True;
  with DLManager do begin
    if vtDownload.SelectedCount = 0 then
    begin
      miDownloadStop.Enabled := False;
      miDownloadResume.Enabled := False;
      miDownloadDelete.Enabled := False;
      miDownloadDeleteTask.Enabled := False;
      miDownloadDeleteTaskData.Enabled := False;
      miDownloadViewMangaInfo.Enabled := False;
      miDownloadOpenFolder.Enabled := False;
      miDownloadOpenWith.Enabled := False;
      miDownloadEnable.Enabled := False;
      miDownloadDisable.Enabled := False;
    end
    else
    begin
      ScanTasks;
      miDownloadStop.Enabled := iStop;
      miDownloadResume.Enabled := iResume;
      miDownloadDelete.Enabled := True;
      miDownloadDeleteTask.Enabled := True;
      miDownloadDeleteTaskData.Enabled := True;
      miDownloadOpenWith.Enabled := vtDownload.SelectedCount = 1;
      miDownloadOpenFolder.Enabled := miDownloadOpenWith.Enabled;
      miDownloadViewMangaInfo.Enabled := miDownloadOpenFolder.Enabled and
        (DLManager[vtDownload.FocusedNode^.Index].DownloadInfo.Link <> '');
      miDownloadEnable.Enabled := iEnable;
      miDownloadDisable.Enabled := iDisable;
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
var
  iCheck,
  iStop,
  iEnable,
  iDisable: Boolean;

  procedure ScanFavs;
  var
    Node: PVirtualNode;
  begin
    iCheck := False;
    iStop := False;
    iEnable := False;
    iDisable := False;
    Node := vtFavorites.GetFirstSelected();
    while Assigned(Node) do
    begin
      if FavoriteManager[Node^.Index].Enabled then
      begin
        if not iDisable then
          iDisable := True;
        case FavoriteManager[Node^.Index].Status of
          STATUS_IDLE      : if not iCheck then iCheck := True;
          STATUS_CHECK,
          STATUS_CHECKING,
          STATUS_CHECKED   : if not iStop then iStop := True;
        end;
      end
      else if not iEnable then
        iEnable := True;
      if iEnable and iDisable and iCheck and iStop then
        Break;
      Node := vtFavorites.GetNextSelected(Node);
    end;
  end;

begin
  if vtFavorites.SelectedCount = 0 then
  begin
    miFavoritesViewInfos.Enabled := False;
    miFavoritesDownloadAll.Enabled := False;
    miFavoritesEnable.Enabled := False;
    miFavoritesDisable.Enabled := False;
    miFavoritesDelete.Enabled := False;
    miFavoritesChangeSaveTo.Enabled := False;
    miFavoritesOpenFolder.Enabled := False;
    miFavoritesOpenWith.Enabled := False;
  end
  else
  begin
    ScanFavs;
    miFavoritesCheckNewChapter.Enabled := iCheck;
    miFavoritesStopCheckNewChapter.Enabled := iStop;
    miFavoritesEnable.Enabled := iEnable;
    miFavoritesDisable.Enabled := iDisable;
    if vtFavorites.SelectedCount = 1 then
    begin
      miFavoritesViewInfos.Enabled := True;
      miFavoritesDownloadAll.Enabled := (Trim(FavoriteManager[vtFavorites.FocusedNode^.Index].FavoriteInfo.Link) <> '');
      miFavoritesDelete.Enabled := True;
      miFavoritesChangeSaveTo.Enabled := True;
      miFavoritesOpenFolder.Enabled := DirectoryExistsUTF8(FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo);
      miFavoritesOpenWith.Enabled := miFavoritesOpenFolder.Enabled;
    end
    else
    begin
      miFavoritesViewInfos.Enabled := False;
      miFavoritesDownloadAll.Enabled := True;
      miFavoritesDelete.Enabled := True;
      miFavoritesChangeSaveTo.Enabled := False;
      miFavoritesOpenFolder.Enabled := False;
      miFavoritesOpenWith.Enabled := False;
    end;
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

procedure TMainForm.pmSbMainPopup(Sender: TObject);
begin
  if Assigned(SilentThreadManager) then
  begin
    if SilentThreadManager.Count = 0 then
      Abort;
  end
  else
    Abort;
end;

procedure TMainForm.pmTrayPopup(Sender: TObject);
var
  i: Integer;
begin
  with miTrayAfterDownloadFinish do
    for i := 0 to Count - 1 do
      if Items[i].Tag = Integer(OptionLetFMDDo) then
      begin
        Items[i].Checked := True;
        Break;
      end;
  miTrayShowDropBox.Checked := Assigned(FormDropTarget);
end;

procedure TMainForm.rgOptionCompressSelectionChanged(Sender: TObject);
begin
  seOptionPDFQuality.Enabled:=rgOptionCompress.ItemIndex=3;
  lbOptionPDFQuality.Enabled:=seOptionPDFQuality.Enabled;
  lbOptionPDFQualityHint.Enabled:=seOptionPDFQuality.Enabled;
end;

procedure TMainForm.sbUpdateListDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  ClRect, TxtRect, BarRect, ProgressBarRect: TRect;
  Percents: double;
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
      ulWorkPtr := ulTotalPtr;
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
      Pen.Color:=CL_BarGrayLine;
      Brush.Color:=CL_BarGray;
      Rectangle(BarRect);

      ProgressBarRect := BarRect;
      ProgressBarRect.Right := round((ProgressBarRect.Right - ProgressBarRect.Left) *
        Percents) + ProgressBarRect.Left;

      if (ProgressBarRect.Right - ProgressBarRect.Left) > 0 then
      begin
        Pen.Color:=CL_BarGreenLine;
        Brush.Color:=CL_BarGreen;
        Rectangle(ProgressBarRect);
      end;
      Brush.Style := bsClear;
      TextRect(txtRect, 5, 0, Panel.Text, UpdateStatusTextStyle);
    end;
  end;
end;

procedure TMainForm.seOptionAutoCheckFavIntervalMinutesChange(Sender: TObject);
begin
  lbOptionAutoCheckFavIntervalMinutes.Caption :=
    Format(RS_LblAutoCheckNewChapterMinute, [seOptionAutoCheckFavIntervalMinutes.Value]);
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
  UpdateVtDownload;
end;

procedure TMainForm.tbDownloadStopAllClick(Sender: TObject);
begin
  DLManager.StopAllTasks;
  UpdateVtDownload;
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
  vtDownloadUpdateFilters(False);
  pcMain.ActivePage := tsDownload;
  configfile.WriteInteger('general', 'DownloadFilterSelect',
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

procedure TMainForm.vtDownloadColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  if Column = 5 then
    miDownloadOpenFolderClick(Sender)
  else
    miDownloadOpenWithClick(Sender);
end;

procedure TMainForm.vtDownloadDragAllowed(Sender : TBaseVirtualTree;
  Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
begin
  Allowed := True;
end;

procedure TMainForm.vtDownloadMoveItems(NextIndex: Cardinal; Mode: TDropMode);
var
  i, nIndex: Cardinal;
  cNode: PVirtualNode;
  ConTemp: TTaskContainers;
begin
  if vtDownload.SelectedCount=0 then Exit;
  nIndex:=NextIndex;
  vtDownload.BeginUpdate;
  ConTemp:=TTaskContainers.Create;
  EnterCriticalSection(DLManager.CS_Task);
  try
    i:=0;
    cNode:=vtDownload.GetFirstSelected();
    while cNode<>nil do
    begin
      vtDownload.Selected[cNode]:=False;
      ConTemp.Add(DLManager.Items[cNode^.Index-i]);
      DLManager.Items.Delete(cNode^.Index-i);
      if (nIndex>0) and (cNode^.Index<nIndex) then
        Dec(nIndex);
      Inc(i);
      cNode:=vtDownload.GetNextSelected(cNode);
    end;

    for i:=0 to ConTemp.Count-1 do
    begin
      if (i=0) and (Mode in [dmBelow,dmNowhere]) then
        Inc(nIndex)
      else if (i>0) and (nIndex<DLManager.Count) then
        Inc(nIndex);
      if nIndex>DLManager.Count then
        nIndex:=DLManager.Count;
      DLManager.Items.Insert(nIndex, ConTemp[i]);
    end;

    cNode:=vtDownload.GetFirst;
    while cNode^.Index<nIndex do
      cNode:=vtDownload.GetNext(cNode);

    if Mode in [dmBelow,dmNowhere] then
      vtDownload.FocusedNode:=cNode;

    for i:=0 to ConTemp.Count-1 do
    begin
      vtDownload.Selected[cNode]:=True;
      if i<ConTemp.Count-1 then
        cNode:=vtDownload.GetPrevious(cNode);
    end;

    if Mode=dmAbove then
      vtDownload.FocusedNode:=cNode;
  finally
    LeaveCriticalSection(DLManager.CS_Task);
  end;
  ConTemp.Free;
  vtDownload.EndUpdate;
  vtDownloadUpdateFilters;
end;

procedure TMainForm.vtDownloadDragDrop(Sender : TBaseVirtualTree;
  Source : TObject; DataObject : IDataObject; Formats : TFormatArray;
  Shift : TShiftState; const Pt : TPoint; var Effect : LongWord;
  Mode : TDropMode);
begin
  if (Source=vtDownload) and (vtDownload.RootNodeCount>1) then
  begin
    if Mode = dmNowhere then
      vtDownloadMoveItems(vtDownload.GetLast^.Index, Mode)
    else
      vtDownloadMoveItems(vtDownload.DropTargetNode^.Index, Mode);
  end
  else
    AddSilentThread(frmDropTarget.GetDropURLs(DataObject), MD_DownloadAll);
end;

procedure TMainForm.vtDownloadDragOver(Sender : TBaseVirtualTree;
  Source : TObject; Shift : TShiftState; State : TDragState; const Pt : TPoint;
  Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
begin
  Accept:=True;
end;

procedure TMainForm.vtDownloadDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  BarRect: TRect;
  Percents: double;
  ww, hh: Integer;
begin
  if Column <> 2 then Exit;
  DefaultDraw := False;
  with DLManager.Items[Node^.Index], TargetCanvas do
  begin
    if Status in [STATUS_FINISH, STATUS_COMPRESS, STATUS_FAILED] then
      Percents := 1
    else
    if (DLManager.Items[Node^.Index].DownCounter = 0) or
      (DLManager.Items[Node^.Index].PageNumber = 0) then
      Percents := 0
    else
      Percents := DLManager.Items[Node^.Index].DownCounter / DLManager.Items[Node^.Index].PageNumber;

    // base bar
    BarRect.Left := CellRect.Left + 2;
    BarRect.Top := CellRect.Top + 2;
    BarRect.Right := CellRect.Right - 2;
    BarRect.Bottom := CellRect.Bottom - 2;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Pen.Color := CL_BarGrayLine;
    Brush.Color := CL_BarGray;
    Rectangle(BarRect);

    // progress bar
    if Percents > 0 then
    begin
      BarRect.Right := round((BarRect.Right - BarRect.Left) * Percents) + BarRect.Left;
      case DLManager.Items[Node^.Index].Status of
        STATUS_STOP,
        STATUS_FAILED  : begin
                           Pen.Color   := CL_BarRedLine;
                           Brush.Color := CL_BarRed;
                         end;
        STATUS_WAIT    : begin
                           Pen.Color   := CL_BarGrayLine;
                           Brush.Color := CL_BarGray;
                         end;
        STATUS_DOWNLOAD: begin
                           Pen.Color   := CL_BarBlueLine;
                           Brush.Color := CL_BarBlue;
                         end;
        STATUS_PROBLEM : begin
                           Pen.Color   := CL_BarYellowLine;
                           Brush.Color := CL_BarYellow;
                         end;
        STATUS_FINISH  : begin
                           Pen.Color   := CL_BarGreenLine;
                           Brush.Color := CL_BarGreen;
                         end;
        else
          begin
            Pen.Color   := CL_BarBrownGoldLine;
            Brush.Color := CL_BarBrownGold;
          end;
      end;
      Rectangle(BarRect);
    end;
    // text
    if DownloadInfo.Progress <> '' then
    begin
      Font.Color := clBlack;
      Brush.Style := bsClear;
      GetTextSize(DownloadInfo.Progress, ww, hh);
      TextOut(CellRect.Left + ((CellRect.Right - CellRect.Left - ww) div 2),
        CellRect.Top + ((CellRect.Bottom - CellRect.Top - hh) div 2), DownloadInfo.Progress);
    end;
  end;
end;

procedure TMainForm.vtDownloadFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Sender.ScrollIntoView(Node, False, False);
end;

// Download table

procedure TMainForm.vtDownloadGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  l, i: Cardinal;
begin
  with DLManager.Items[Node^.Index],DLManager.Items[Node^.Index].DownloadInfo do
    case Column of
      0: begin
           l := ChapterLinks.Count;
           if l>0 then
           begin
             HintText:='';
             if l<5 then
               for i:=0 to l-1 do begin
                 if HintText<>'' then HintText+=LineEnding;
                 HintText+=ChapterName.Strings[i]
               end
             else
             begin
               for i:=0 to 1 do begin
                 if HintText<>'' then HintText+=LineEnding;
                 HintText+=ChapterName.Strings[i]
               end;
               HintText+=LineEnding+'...';
               for i:=l-2 to l-1 do begin
                 if HintText<>'' then HintText+=LineEnding;
                 HintText+=ChapterName.Strings[i]
               end;
             end;
           end;
         end;
      1: HintText:=Status;
      2: HintText:=Progress;
      4: HintText:=Website;
      5: HintText:=SaveTo;
      6: HintText:=DateTimeToStr(DateTime);
    end;
end;

procedure TMainForm.vtDownloadGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if vtDownload.Header.Columns[Column].Position = 0 then
    if not DLManager[Node^.Index].Enabled then
      ImageIndex := 8
    else
      ImageIndex := Integer(DLManager[Node^.Index].Status);
end;

procedure TMainForm.vtDownloadGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  with DLManager[Node^.Index].DownloadInfo do
    case Column of
      0: CellText:=Title;
      1: CellText:=Status;
      2: begin
           if Progress='' then CellText:='Empty'
           else CellText:=Progress;
         end;
      3: CellText:=TransferRate;
      4: CellText:=Website;
      5: CellText:=SaveTo;
      6: CellText:=DateTimeToStr(DateTime);
    end;
end;

procedure TMainForm.vtDownloadHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  if Button <> mbLeft then Exit;
  if DLManager.Count < 2 then Exit;
  if (Column = 2) or (Column = 3) then Exit;
  if DLManager.SortColumn = Column then
    DLManager.SortDirection := not DLManager.SortDirection;
  DLManager.SortColumn := Column;
  vtDownload.Header.SortDirection := TSortDirection(DLManager.SortDirection);
  vtDownload.Header.SortColumn := Column;
  DLManager.Sort(Column);
  UpdateVtDownload;
end;

procedure TMainForm.vtDownloadKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  if (ssCtrl in Shift) then begin
    if (Sender.SelectedCount>0) and
      (CharCode in [VK_UP,VK_DOWN,VK_HOME,VK_END]) then
      DoDefault:=False;
  end;
end;

procedure TMainForm.vtDownloadKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
var
  p: Cardinal;
begin
  if not (ssCtrl in Shift) then Exit;
  if vtDownload.SelectedCount=0 then Exit;
  p:=vtDownload.GetFirstSelected()^.Index;
  case Key of
    VK_UP   : if p>0 then vtDownloadMoveItems(p-1,dmAbove);
    VK_DOWN : vtDownloadMoveItems(p,dmBelow);
    VK_HOME : vtDownloadMoveItems(0,dmAbove);
    VK_END  : vtDownloadMoveItems(vtDownload.RootNodeCount-1,dmBelow);
  end;
end;

procedure TMainForm.vtDownloadKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    miDownloadDeleteTaskClick(miDownloadDeleteTask);
end;

procedure TMainForm.vtDownloadPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if not DLManager[Node^.Index].Enabled then
    TargetCanvas.Font.Color := TVirtualStringTree(Sender).Colors.DisabledColor;
end;

procedure TMainForm.vtFavoritesBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

var
  C: TColor;
begin
  if CellPaintMode <> cpmPaint then Exit;
  with TargetCanvas, FavoriteManager.Items[Node^.Index] do
  begin
    C := Brush.Color;
    Brush.Color := clNone;
    if Trim(FavoriteInfo.Link) = '' then
      Brush.Color := CL_FVBrokenFavorite
    else
    begin
      if FavoriteInfo.CurrentChapter = '0' then
        Brush.Color := CL_FVEmptyChapters;
      if Status = STATUS_CHECKING then
        Brush.Color := CL_FVChecking
      else
      if (Status = STATUS_CHECKED) and
        Assigned(NewMangaInfo) then
      begin
        if NewMangaInfoChaptersPos.Count > 0 then
          Brush.Color := CL_FVNewChapterFound
        else
        if NewMangaInfo.status = MangaInfo_StatusCompleted then
          Brush.Color := CL_FVCompletedManga;
      end;
    end;
    if Brush.Color <> clNone then
      FillRect(CellRect)
    else
      Brush.Color := C;
  end;
end;

procedure TMainForm.vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  if Column = 4 then
    miFavoritesOpenFolderClick(Sender)
  else
    miFavoritesOpenWithClick(Sender);
end;

procedure TMainForm.vtFavoritesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
begin
  AddSilentThread(frmDropTarget.GetDropURLs(DataObject), MD_AddToFavorites);
end;

procedure TMainForm.vtFavoritesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept:=True;
  Effect:=DROPEFFECT_LINK;
end;

procedure TMainForm.vtFavoritesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
begin
  if Node^.Index>=FavoriteManager.Count then Exit;
  with FavoriteManager.Items[Node^.Index].FavoriteInfo do
    case Column of
      1: if Trim(Link)='' then HintText:=RS_HintFavoriteProblem
         else HintText:=Title;
      2: HintText:=currentChapter;
      3: HintText:=website;
      4: HintText:=saveTo;
    end;
end;

procedure TMainForm.vtFavoritesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if vtFavorites.Header.Columns[Column].Position<>1 then Exit;
  with FavoriteManager.Items[Node^.Index] do
  begin
    if Trim(FavoriteInfo.Link)='' then
      ImageIndex:=16
    else
      case FavoriteManager.Items[Node^.Index].Status of
        STATUS_CHECK    : ImageIndex:=19;
        STATUS_CHECKING : ImageIndex:=12;
        STATUS_CHECKED  :
          begin
            ImageIndex:=20;
            if Assigned(NewMangaInfo) then
            begin
              if NewMangaInfoChaptersPos.Count>0 then
                ImageIndex:=21
              else
              if NewMangaInfo.status=MangaInfo_StatusCompleted then
                ImageIndex:=5
            end;
          end;
        else
          ImageIndex:=-1;
      end;
  end;
end;

// vtFavorites

procedure TMainForm.vtFavoritesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Node^.Index>=FavoriteManager.Count then Exit;
  with FavoriteManager.Items[Node^.Index].FavoriteInfo do
    case Column of
      0: CellText:=IntToStr(Node^.Index+1);
      1: CellText:=Title;
      2: CellText:=currentChapter;
      3: CellText:=website;
      4: CellText:=saveTo;
    end;
end;

procedure TMainForm.vtFavoritesHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  if Button <> mbLeft then Exit;
  if FavoriteManager.isRunning then Exit;
  if FavoriteManager.Count < 2 then Exit;
  if Column = 0 then Exit;
  FavoriteManager.isRunning := True;
  try
    if FavoriteManager.SortColumn = Column then
      FavoriteManager.sortDirection := not FavoriteManager.sortDirection
    else
      FavoriteManager.SortColumn := Column;
    vtFavorites.Header.SortColumn := Column;
    vtFavorites.Header.SortDirection := TSortDirection(FavoriteManager.sortDirection);
    FavoriteManager.Sort(Column);
  finally
    UpdateVtFavorites;
    FavoriteManager.isRunning := False;
  end;
end;

procedure TMainForm.vtFavoritesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if not FavoriteManager[Node^.Index].Enabled then
    TargetCanvas.Font.Color := TVirtualStringTree(Sender).Colors.DisabledColor;
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

// options

procedure TMainForm.btOptionApplyClick(Sender: TObject);
begin
  SaveOptions;
  ApplyOptions;
  if not Self.Focused then Self.SetFocus;
end;

// vtMangaList

procedure TMainForm.vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if CellPaintMode <> cpmPaint then Exit;
  if Node^.Index>=dataProcess.RecordCount then Exit;
  with TargetCanvas do
  begin
    Brush.Color := clNone;
    if dataProcess.Value[Node^.Index, DATA_PARAM_STATUS] = MangaInfo_StatusCompleted then
      Brush.Color := CL_MNCompletedManga;
    if miHighlightNewManga.Checked and
      (dataProcess.ValueInt[Node^.Index, DATA_PARAM_JDN] > (currentJDN - OptionNewMangaTime)) then
        Brush.Color := CL_MNNewManga;

    if Brush.Color <> clNone then
      FillRect(CellRect);
  end;
end;

procedure TMainForm.vtMangaListGetCursor(Sender: TBaseVirtualTree;
  var ACursor: TCursor);
begin
  ACursor := Sender.Cursor;
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
      s := s + LineEnding + LineEnding + RS_InfoSummary + LineEnding +
        StringBreaks(dataProcess.Value[LPos, DATA_PARAM_SUMMARY]);
  end;
  HintText := s;
end;

procedure TMainForm.vtMangaListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Assigned(Node) then
    CellText :=  Format('%s (%d)', [dataProcess.Value[Node^.Index, DATA_PARAM_TITLE],
      dataProcess.ValueInt[Node^.Index, DATA_PARAM_NUMCHAPTER]]);
end;

procedure TMainForm.InitCheckboxes;
var
  i: Cardinal;
begin
  for i := 0 to 37 do
    TCheckBox(pnGenres.Controls[i]).State := cbGrayed;
end;

procedure TMainForm.tvDownloadFilterRefresh(const ResourceChanged: Boolean);
begin
  // update download filter treeview
  tvDownloadFilter.BeginUpdate;
  with DLManager, tvDownloadFilter do
    try
      // root
      Items[0].Text := Format('%s (%d)', [RS_AllDownloads, vtDownload.RootNodeCount]);

      // childs
      Items[1].Text := Format('%s (%d)', [RS_Finish, StatusCount[STATUS_FINISH]]);
      Items[2].Text := Format('%s (%d)', [RS_InProgress,
        StatusCount[STATUS_DOWNLOAD] +
        StatusCount[STATUS_PREPARE] +
        StatusCount[STATUS_WAIT]]);
      Items[3].Text := Format('%s (%d)', [RS_Stopped, StatusCount[STATUS_STOP]]);
      Items[4].Text := Format('%s (%d)', [RS_Failed,
        StatusCount[STATUS_PROBLEM] +
        StatusCount[STATUS_FAILED]]);
      Items[5].Text := Format('%s (%d)', [RS_Disabled, DisabledCount]);

      if ResourceChanged then
      begin
        // root
        Items[6].Text := RS_History;

        // childs
        Items[7].Text := RS_Today;
        Items[8].Text := RS_Yesterday;
        Items[9].Text := RS_OneWeek;
        Items[10].Text := RS_OneMonth;
      end;
    finally
      tvDownloadFilter.EndUpdate;
    end;
end;

procedure TMainForm.vtDownloadUpdateFilters(const RefreshTree: Boolean);
var
  ACurrentJDN: Integer;

  procedure ShowTasks(S: TDownloadStatusTypes = []);
  var
    xNode: PVirtualNode;
  begin
    if (S = []) and (vtDownload.VisibleCount = vtDownload.RootNodeCount) then
      Exit;
    xNode := vtDownload.GetFirst();
    while Assigned(xNode) do
    begin
      with DLManager[xNode^.Index] do
      begin
        if S = [] then
          Visible := True
        else
          Visible := Status in S;
        vtDownload.IsVisible[xNode] := Visible;
      end;
      xNode := vtDownload.GetNext(xNode);
    end;
  end;

  procedure ShowTasksOnCertainDays(const L, H: Integer);
  var
    jdn: Integer;
    xNode: PVirtualNode;
  begin
    xNode := vtDownload.GetFirst();
    while Assigned(xNode) do
    begin
      with DLManager.Items[xNode^.Index] do
      begin
        jdn := DateToJDN(DownloadInfo.DateTime);
        Visible := (jdn >= L) and (jdn <= H);;
        vtDownload.IsVisible[xNode] := Visible;
      end;
      xNode := vtDownload.GetNext(xNode);
    end;
  end;

  procedure ShowDisabled;
  var
    xNode: PVirtualNode;
  begin
    xNode := vtDownload.GetFirst();
    while Assigned(xNode) do
    begin
      with DLManager.Items[xNode^.Index] do
      begin
        Visible := not Enabled;
        vtDownload.IsVisible[xNode] := Visible;
      end;
      xNode := vtDownload.GetNext(xNode);
    end;
  end;

begin
  if tvDownloadFilter.Selected = nil then Exit;

  vtDownload.BeginUpdate;
  try
    if vtDownload.RootNodeCount <> DLManager.Count then
      vtDownload.RootNodeCount := DLManager.Count;

    // filter download list
    if tvDownloadFilter.Selected.AbsoluteIndex > 5 then
      ACurrentJDN := DateToJDN(Now);
    case tvDownloadFilter.Selected.AbsoluteIndex of
      0, 6: ShowTasks;
      1: ShowTasks([STATUS_FINISH]);
      2: ShowTasks([STATUS_WAIT, STATUS_PREPARE, STATUS_DOWNLOAD, STATUS_COMPRESS]);
      3: ShowTasks([STATUS_STOP]);
      4: ShowTasks([STATUS_PROBLEM, STATUS_FAILED]);
      5: ShowDisabled;

      7: ShowTasksOnCertainDays(ACurrentJDN, ACurrentJDN);
      8: ShowTasksOnCertainDays(ACurrentJDN - 1, ACurrentJDN - 1);
      9: ShowTasksOnCertainDays(ACurrentJDN - 7, ACurrentJDN);
      10: ShowTasksOnCertainDays(ACurrentJDN - 30, ACurrentJDN);
    end;
  finally
    vtDownload.EndUpdate;
  end;

  if RefreshTree then
    tvDownloadFilterRefresh;
  if edDownloadsSearch.Text <> '' then
    edDownloadsSearchChange(edDownloadsSearch);
end;

procedure TMainForm.AddChapterNameToList;
begin
  UpdateVtChapter;
end;

procedure TMainForm.AddSilentThread(URL: string; MetaDataType: TMetaDataType);
var
  i, j, m: Integer;
  host, link, webs: String;
  URls: TStringList;
begin
  if Trim(URL) = '' then Exit;
  URLs := TStringList.Create;
  try
    URls.Text := URL;
    if URls.Count > 0 then
    begin
      GoogleResultURLs(URls);
      SilentThreadManager.BeginAdd;
      with TRegExpr.Create do
      try
        Expression := REGEX_HOST;
        for i := 0 to URls.Count - 1 do
        begin
          host := '';
          link := '';
          webs := '';
          host := LowerCase(Replace(URls[i], '$2', True));
          link := Replace(URls[i], '$4', True);
          if (host <> '') and (link <> '') then
          begin
            m := Modules.LocateModuleByHost(host);
            if m > -1 then
              webs := Modules.Module[m].Website;
            if webs = '' then
            begin
              for j := Low(WebsiteRoots) to High(WebsiteRoots) do
                if Pos(host, LowerCase(WebsiteRoots[j, 1])) <> 0 then
                  webs := WebsiteRoots[j, 0];
            end;
            if webs <> '' then
            begin
              if not ((MetaDataType = MD_AddToFavorites) and SitesWithoutFavorites(webs)) then
                SilentThreadManager.Add(MetaDataType, webs, '', link);
            end;
          end;
        end;
      finally
        Free;
      end;
      SilentThreadManager.EndAdd;
    end;
  finally
    URls.Free;
  end;
end;

procedure TMainForm.AddSilentThread(URL: string);
var
  mt: TMetaDataType;
begin
  if Trim(URL)='' then Exit;
  if rgDropTargetMode.ItemIndex=0 then
    mt:=MD_DownloadAll
  else
    mt:=MD_AddToFavorites;
  AddSilentThread(URL,mt);
end;

procedure TMainForm.AddTextToInfo(const ATitle, AValue: String);
var
  p: Integer;
  fp: TFontParams;
  s: string;
begin
  s := Trim(FixWhiteSpace(AValue));
  if s = '' then Exit;
  if ATitle = RS_InfoSummary then
    s := Trim(StringBreaks(s));
  with rmInformation do
  begin
    if Lines.Count > 0 then
      Lines.Add('');
    p := SelStart;
    GetTextAttributes(p, fp);
    fp.Style += [fsBold, fsUnderline];
    Inc(fp.Size);
    SetTextAttributes(p, 0, fp);
    Lines.Add(ATitle);
    p := SelStart;
    fp.Style -= [fsBold, fsUnderline];
    Dec(fp.Size);
    SetTextAttributes(p, 0, fp);
    Lines.Add(s);
  end;
end;

procedure TMainForm.FilledSaveTo;
begin
  if Trim(edSaveTo.Text) = '' then
  begin
    edSaveTo.Text := Trim(configfile.ReadString('saveto', 'SaveTo', DEFAULT_PATH));
    if edSaveTo.Text = '' then
      edSaveTo.Text := DEFAULT_PATH;
  end
  else
    edSaveTo.Text := CleanAndExpandDirectory(edSaveTo.Text);
end;

procedure TMainForm.ViewMangaInfo(const AURL, AWebsite, ATitle: String;
  const AMangaListPos: Integer);
var
  i: Integer;
begin
  if (AURL = '') or (AWebsite = '') then Exit;

  // terminate exisiting getmangainfo thread
  if Assigned(GetInfosThread) then
    try
      { TODO -oriderkick : Access violation on terminate
        if terminating thread while starting download cover
      }
      GetInfosThread.Terminate;
      GetInfosThread.WaitFor;
    except
    end;

  // set the UI
  i := Modules.LocateModule(AWebsite);
  if i <> -1 then
    edURL.Text := FillHost(Modules.Module[i].RootURL, AURL)
  else
    edURL.Text := FillMangaSiteHost(AWebsite, AURL);
  pcMain.ActivePage := tsInformation;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add(RS_Loading);
  clbChapterList.Clear;
  if Assigned(gifWaiting) then
  begin
    tmAnimateMangaInfo.Enabled := True;
    pbWait.Visible := True;
  end;
  btDownload.Enabled := False;
  btDownloadSplit.Enabled := btDownload.Enabled;
  btReadOnline.Enabled := True;
  DisableAddToFavorites(AWebsite);
  //check if manga already in FavoriteManager list
  if btAddToFavorites.Enabled and
    (ATitle <> '') and
    (FavoriteManager.Count > 0) then
    btAddToFavorites.Enabled := not FavoriteManager.IsMangaExist(ATitle, AWebsite);

  // start the thread
  GetInfosThread := TGetMangaInfosThread.Create;
  GetInfosThread.MangaListPos := AMangaListPos;
  GetInfosThread.Title := ATitle;
  GetInfosThread.Website := AWebsite;
  GetInfosThread.Link := AURL;
  GetInfosThread.Start;
end;

procedure TMainForm.ShowInformation(const title, website, link: String);
var
  i, j: Integer;
begin
  pcMain.ActivePage := tsInformation;
  FilledSaveTo;

  imCover.Picture.Assign(nil);

  with rmInformation do
    try
      Lines.BeginUpdate;
      Lines.Clear;
      edURL.Text := mangaInfo.url;
      AddTextToInfo(RS_InfoTitle, mangaInfo.title);
      AddTextToInfo(RS_InfoAuthors, mangaInfo.authors);
      AddTextToInfo(RS_InfoArtists, mangaInfo.artists);
      AddTextToInfo(RS_InfoGenres, mangaInfo.genres);
      i := StrToIntDef(mangaInfo.status, -1);
      if (i > -1) and (i < cbFilterStatus.Items.Count) then
        AddTextToInfo(RS_InfoStatus, cbFilterStatus.Items[i]);
      AddTextToInfo(RS_InfoSummary, mangaInfo.summary);
      CaretPos := Point(0, 0);
    finally
      Lines.EndUpdate;
    end;

  SetLength(ChapterList, mangaInfo.chapterName.Count);
  if Length(ChapterList) <> 0 then
  begin
    if miChapterListAscending.Checked then
      j := 0
    else
      j := High(ChapterList);
    for i := low(ChapterList) to High(ChapterList) do
    begin
      ChapterList[i].Index := j + 1;
      ChapterList[i].Title := mangaInfo.chapterName[j];
      ChapterList[i].Link := mangaInfo.chapterLinks[j];
      ChapterList[i].Downloaded := False;
      if miChapterListAscending.Checked then
        Inc(j)
      else
        Dec(j);
    end;
  end;

  miChapterListHighlightClick(nil);
  UpdateVtChapter;
  miChapterListHideDownloadedClick(nil);
  edFilterMangaInfoChaptersChange(nil);
  if (clbChapterList.RootNodeCount <> 0) and miChapterListAscending.Checked then
    clbChapterList.FocusedNode := clbChapterList.GetLast();

  btDownload.Enabled := (clbChapterList.RootNodeCount > 0);
  btDownloadSplit.Enabled := btDownload.Enabled;
  btReadOnline.Enabled := (mangaInfo.link <> '');
  DisableAddToFavorites(website);

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
  l: TStringList;
  i, j: Integer;
  s: String;
  data: PSingleItem;
begin
  with configfile do begin
    // general
    cbOptionOneInstanceOnly.Checked := ReadBool('general', 'OneInstanceOnly', True);
    cbOptionLiveSearch.Checked := ReadBool('general', 'LiveSearch', True);
    cbOptionMinimizeOnStart.Checked := ReadBool('general', 'MinimizeOnStart', False);
    cbOptionMinimizeToTray.Checked := ReadBool('general', 'MinimizeToTray', False);
    cbOptionLetFMDDo.ItemIndex := ReadInteger('general', 'LetFMDDo', 0);
    edOptionExternalPath.Text := ReadString('general', 'ExternalProgramPath', '');
    edOptionExternalParams.Text := ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM);
    miChapterListHideDownloaded.Checked := ReadBool('general', 'ChapterListHideDownloaded', False);
    cbAddAsStopped.Checked := ReadBool('general', 'AddAsStopped', False);
    miHighLightNewManga.Checked := ReadBool('general', 'HighlightNewManga', True);
    miChapterListHighlight.Checked := ReadBool('general', 'HighlightDownloadedChapters', True);
    miChapterListAscending.Checked := ReadBool('general', 'SortChapterListAscending', True);
    miChapterListDescending.Checked := not miChapterListAscending.Checked;

    // view
    cbOptionShowDownloadToolbar.Checked := ReadBool('view', 'ShowDownloadsToolbar', True);
    cbOptionShowDownloadToolbarDeleteAll.Checked := ReadBool('view', 'ShowDownloadsToolbarDeleteAll', False);
    cbOptionEnableLoadCover.Checked := ReadBool('view', 'LoadMangaCover', True);
    cbOptionShowBalloonHint.Checked := ReadBool('view', 'ShowBalloonHint', OptionShowBalloonHint);
    ckDropTarget.Checked := ReadBool('droptarget', 'Show', False);
    frmDropTarget.FWidth := ReadInteger('droptarget', 'Width', frmDropTarget.FWidth);
    frmDropTarget.FHeight := ReadInteger('droptarget', 'Heigth', frmDropTarget.FHeight);
    frmDropTarget.FTop := ReadInteger('droptarget', 'Top', frmDropTarget.FTop);
    frmDropTarget.FLeft := ReadInteger('droptarget', 'Left', frmDropTarget.FLeft);
    rgDropTargetMode.ItemIndex := ReadInteger('droptarget', 'Mode', 0);
    tbDropTargetOpacity.Position := ReadInteger('droptarget', 'Opacity', 255);

    // connection
    seOptionMaxParallel.Value := ReadInteger('connections', 'NumberOfTasks', 1);
    seOptionMaxThread.Value := ReadInteger('connections', 'NumberOfThreadsPerTask', 1);
    seOptionMaxRetry.Value := ReadInteger('connections', 'Retry', 5);;
    seOptionConnectionTimeout.Value := ReadInteger('connections', 'ConnectionTimeout', 30);
    cbOptionUseProxy.Checked := ReadBool('connections', 'UseProxy', False);
    cbOptionProxyType.Text := ReadString('connections', 'ProxyType', 'HTTP');
    edOptionHost.Text := ReadString('connections', 'Host', '');
    edOptionPass.Text := ReadString('connections', 'Pass', '');
    edOptionPort.Text := ReadString('connections', 'Port', '');
    edOptionUser.Text := ReadString('connections', 'User', '');

    // saveto
    edOptionDefaultPath.Text := ReadString('saveto', 'SaveTo', DEFAULT_PATH);
    if Trim(edOptionDefaultPath.Text) = '' then
      edOptionDefaultPath.Text := DEFAULT_PATH;
    seOptionPDFQuality.Value := ReadInteger('saveto', 'PDFQuality', 100);
    rgOptionCompress.ItemIndex := ReadInteger('saveto', 'Compress', 0);
    cbOptionChangeUnicodeCharacter.Checked := ReadBool('saveto', 'ChangeUnicodeCharacter', False);
    edOptionChangeUnicodeCharacterStr.Text := ReadString('saveto', 'ChangeUnicodeCharacterStr', OptionChangeUnicodeCharacterStr);
    cbOptionRemoveMangaNameFromChapter.Checked := ReadBool('saveto', 'RemoveMangaNameFromChapter', False);
    cbOptionGenerateMangaFolder.Checked := ReadBool('saveto', 'GenerateMangaFolder', True);
    edOptionMangaCustomRename.Text := ReadString('saveto', 'MangaCustomRename', DEFAULT_MANGA_CUSTOMRENAME);
    if Trim(edOptionMangaCustomRename.Text) = '' then
      edOptionMangaCustomRename.Text := DEFAULT_MANGA_CUSTOMRENAME;
    cbOptionGenerateChapterFolder.Checked := ReadBool('saveto', 'GenerateChapterFolder', True);
    edOptionChapterCustomRename.Text := ReadString('saveto', 'ChapterCustomRename', DEFAULT_CHAPTER_CUSTOMRENAME);
    if Trim(edOptionChapterCustomRename.Text) = '' then
      edOptionChapterCustomRename.Text := DEFAULT_CHAPTER_CUSTOMRENAME;
    cbOptionDigitVolume.Checked := ReadBool('saveto', 'ConvertDigitVolume', True);
    seOptionDigitVolume.Value := ReadInteger('saveto', 'DigitVolumeLength', 2);
    seOptionDigitVolume.Enabled := cbOptionDigitVolume.Checked;
    cbOptionDigitChapter.Checked := ReadBool('saveto', 'ConvertDigitChapter', True);
    seOptionDigitChapter.Value := ReadInteger('saveto', 'DigitChapterLength', 3);
    seOptionDigitChapter.Enabled := cbOptionDigitChapter.Checked;
    edOptionFilenameCustomRename.Text := ReadString('saveto', 'FilenameCustomRename', DEFAULT_FILENAME_CUSTOMRENAME);
    if Trim(edOptionFilenameCustomRename.Text) = '' then
      edOptionFilenameCustomRename.Text := DEFAULT_FILENAME_CUSTOMRENAME;

    // update
    cbOptionAutoCheckLatestVersion.Checked := ReadBool('update', 'AutoCheckLatestVersion', True);
    cbOptionAutoCheckFavStartup.Checked := ReadBool('update', 'AutoCheckFavStartup', True);
    cbOptionAutoCheckFavStartupChange(cbOptionAutoCheckFavStartup);
    cbOptionAutoOpenFavStartup.Checked := ReadBool('update', 'AutoOpenFavStartup', False);
    cbOptionAutoCheckFavInterval.Checked := ReadBool('update', 'AutoCheckFavInterval', True);
    seOptionAutoCheckFavIntervalMinutes.Value := ReadInteger('update', 'AutoCheckFavIntervalMinutes', 60);
    lbOptionAutoCheckFavIntervalMinutes.Caption := Format(RS_LblAutoCheckNewChapterMinute, [seOptionAutoCheckFavIntervalMinutes.Value]);
    cbOptionAutoCheckFavIntervalChange(cbOptionAutoCheckFavInterval);
    seOptionNewMangaTime.Value := ReadInteger('update', 'NewMangaTime', 1);
    cbOptionAutoCheckFavDownload.Checked := ReadBool('update', 'AutoCheckFavAutoDownload', False);
    cbOptionAutoCheckFavRemoveCompletedManga.Checked := ReadBool('update', 'AutoCheckFavAutoRemoveCompletedManga', False);
    cbOptionUpdateListNoMangaInfo.Checked := ReadBool('update', 'UpdateListNoMangaInfo', False);
    cbOptionUpdateListRemoveDuplicateLocalData.Checked := ReadBool('update', 'UpdateListRemoveDuplicateLocalData', False);

    // dialogs
    cbOptionShowQuitDialog.Checked := ReadBool('dialogs', 'ShowQuitDialog', True);
    cbOptionShowDeleteTaskDialog.Checked := ReadBool('dialogs', 'ShowDeleteDldTaskDialog', True);
    cbOptionShowDownloadMangalistDialog.Checked := ReadBool('dialogs', 'ShowDownloadMangalistDialog', True);

    // misc
    frmCustomColor.LoadFromIniFile(configfile);
    ckEnableLogging.Checked := ReadBool('logger', 'Enabled', False);
    edLogFileName.Text := ReadString('logger', 'LogFileName', '');
    if edLogFileName.Text = '' then
      edLogFileName.Text := ChangeFileExt(Application.ExeName, '.log');

    // websites
    if Length(optionMangaSiteSelectionNodes) > 0 then
      for i := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
        optionMangaSiteSelectionNodes[i]^.CheckState := csUncheckedNormal;
    l := TStringList.Create;
    try
      s := ReadString('general', 'MangaListSelect',
        mangalistfile.ReadString('general', 'DefaultSelect', DEFAULT_LIST));
      if Pos(SEPERATOR, s) > 0 then
        GetParams(l, s)    //for old config
      else
        ExtractStrings([','], [], PChar(s), l);
      if (l.Count > 0) and (Length(optionMangaSiteSelectionNodes) > 0) then
        for i := 0 to l.Count - 1 do
          for j := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
          begin
            data := vtOptionMangaSiteSelection.GetNodeData(
              optionMangaSiteSelectionNodes[j]);
            if SameText(l[i], data^.Text) then
            begin
              optionMangaSiteSelectionNodes[j]^.CheckState := csCheckedNormal;
              Break;
            end;
          end;
    finally
      l.Free;
    end;
  end;
end;

procedure TMainForm.SaveOptions;
var
  s: String;
begin
  if Length(optionMangaSiteSelectionNodes) > 0 then
  begin
    s := SaveMangaOptions;
    if s = '' then
    begin
      MessageDlg('', RS_DlgMangaListSelect,
        mtConfirmation, [mbYes], 0);
      Exit;
    end;
  end;

  with configfile do
    try
      // general
      WriteString('general', 'MangaListSelect', s);
      WriteBool('general', 'LiveSearch', cbOptionLiveSearch.Checked);
      WriteBool('general', 'OneInstanceOnly', cbOptionOneInstanceOnly.Checked);
      if cbLanguages.ItemIndex > -1 then
        WriteString('languages', 'Selected', AvailableLanguages.Names[cbLanguages.ItemIndex]);
      WriteBool('general', 'MinimizeOnStart', cbOptionMinimizeOnStart.Checked);
      WriteBool('general', 'MinimizeToTray', cbOptionMinimizeToTray.Checked);
      WriteInteger('general', 'LetFMDDo', cbOptionLetFMDDo.ItemIndex);
      WriteString('general', 'ExternalProgramPath', edOptionExternalPath.Text);
      WriteString('general', 'ExternalProgramParams', edOptionExternalParams.Text);
      WriteBool('general', 'ChapterListHideDownloaded', miChapterListHideDownloaded.Checked);
      WriteBool('general', 'AddAsStopped', cbAddAsStopped.Checked);
      WriteBool('general', 'HighlightNewManga', miHighlightNewManga.Checked);
      WriteBool('general', 'HighlightDownloadedChapters', miChapterListHighlight.Checked);

      // view
      WriteBool('view', 'ShowDownloadsToolbar', cbOptionShowDownloadToolbar.Checked);
      WriteBool('view', 'ShowDownloadsToolbarDeleteAll', cbOptionShowDownloadToolbarDeleteAll.Checked);
      WriteBool('view', 'LoadMangaCover', cbOptionEnableLoadCover.Checked);
      WriteBool('view', 'ShowBalloonHint', cbOptionShowBalloonHint.Checked);
      if not (isExiting and Assigned(FormDropTarget)) then
        SaveDropTargetFormInformation;

      // connections
      WriteInteger('connections', 'NumberOfTasks', seOptionMaxParallel.Value);
      WriteInteger('connections', 'NumberOfThreadsPerTask', seOptionMaxThread.Value);
      WriteInteger('connections', 'Retry', seOptionMaxRetry.Value);
      WriteInteger('connections', 'ConnectionTimeout', seOptionConnectionTimeout.Value);
      WriteBool('connections', 'UseProxy', cbOptionUseProxy.Checked);
      WriteString('connections', 'ProxyType', cbOptionProxyType.Text);
      WriteString('connections', 'Host', edOptionHost.Text);
      WriteString('connections', 'Pass', edOptionPass.Text);
      WriteString('connections', 'Port', edOptionPort.Text);
      WriteString('connections', 'User', edOptionUser.Text);

      // saveto
      if Trim(edOptionDefaultPath.Text) = '' then
        edOptionDefaultPath.Text := DEFAULT_PATH;
      edOptionDefaultPath.Text := CleanAndExpandDirectory(edOptionDefaultPath.Text);
      WriteString('saveto', 'SaveTo', edOptionDefaultPath.Text);
      WriteBool('saveto', 'ChangeUnicodeCharacter', cbOptionChangeUnicodeCharacter.Checked);
      WriteString('saveto', 'ChangeUnicodeCharacterStr', edOptionChangeUnicodeCharacterStr.Text);
      WriteBool('saveto', 'GenerateMangaFolder', cbOptionGenerateMangaFolder.Checked);
      if Trim(edOptionMangaCustomRename.Text) = '' then
        edOptionMangaCustomRename.Text := DEFAULT_MANGA_CUSTOMRENAME;
      WriteString('saveto', 'MangaCustomRename', edOptionMangaCustomRename.Text);
      WriteInteger('saveto', 'Compress', rgOptionCompress.ItemIndex);
      WriteInteger('saveto', 'PDFQuality', seOptionPDFQuality.Value);
      WriteBool('saveto', 'RemoveMangaNameFromChapter', cbOptionRemoveMangaNameFromChapter.Checked);
      WriteBool('saveto', 'GenerateChapterFolder', cbOptionGenerateChapterFolder.Checked);
      if Trim(edOptionChapterCustomRename.Text) = '' then
        edOptionChapterCustomRename.Text := DEFAULT_CHAPTER_CUSTOMRENAME;
      WriteString('saveto', 'ChapterCustomRename', edOptionChapterCustomRename.Text);
      WriteBool('saveto', 'ConvertDigitVolume', cbOptionDigitVolume.Checked);
      WriteBool('saveto', 'ConvertDigitChapter', cbOptionDigitChapter.Checked);
      WriteInteger('saveto', 'DigitVolumeLength', seOptionDigitVolume.Value);
      WriteInteger('saveto', 'DigitChapterLength', seOptionDigitChapter.Value);
      if Trim(edOptionFilenameCustomRename.Text) = '' then
        edOptionFilenameCustomRename.Text := DEFAULT_FILENAME_CUSTOMRENAME;
      WriteString('saveto', 'FilenameCustomRename', edOptionFilenameCustomRename.Text);

      // update
      WriteBool('update', 'AutoCheckLatestVersion', cbOptionAutoCheckLatestVersion.Checked);
      WriteBool('update', 'AutoCheckFavStartup', cbOptionAutoCheckFavStartup.Checked);
      WriteBool('update', 'AutoOpenFavStartup', cbOptionAutoOpenFavStartup.Checked);
      WriteBool('update', 'AutoCheckFavInterval', cbOptionAutoCheckFavInterval.Checked);
      WriteInteger('update', 'AutoCheckFavIntervalMinutes', seOptionAutoCheckFavIntervalMinutes.Value);
      WriteInteger('update', 'NewMangaTime', seOptionNewMangaTime.Value);
      WriteBool('update', 'AutoCheckFavAutoDownload', cbOptionAutoCheckFavDownload.Checked);
      WriteBool('update', 'AutoCheckFavAutoRemoveCompletedManga', cbOptionAutoCheckFavRemoveCompletedManga.Checked);
      WriteBool('update', 'UpdateListNoMangaInfo', cbOptionUpdateListNoMangaInfo.Checked);
      WriteBool('update', 'UpdateListRemoveDuplicateLocalData', cbOptionUpdateListRemoveDuplicateLocalData.Checked);

      // dialogs
      WriteBool('dialogs', 'ShowQuitDialog', cbOptionShowQuitDialog.Checked);
      WriteBool('dialogs', 'ShowDeleteDldTaskDialog', cbOptionShowDeleteTaskDialog.Checked);
      WriteBool('dialogs', 'ShowDownloadMangalistDialog', cbOptionShowDownloadMangalistDialog.Checked);

      // misc
      frmCustomColor.SaveToIniFile(configfile);
      WriteBool('logger', 'Enabled', ckEnableLogging.Checked);
      if edLogFileName.Text = '' then
        edLogFileName.Text := ChangeFileExt(Application.ExeName, '.log');
      WriteString('logger', 'LogFileName', edLogFileName.Text);
    finally
      UpdateFile;
    end;
  advancedfile.UpdateFile;
  Modules.SaveWebsiteOption;
end;

procedure TMainForm.ApplyOptions;
var
  i: Integer;
  isStillHaveCurrentWebsite: Boolean = False;
  data: PSingleItem;
  St: TStringList;
begin
  try
    // general
    cbSelectManga.Clear;
    for i := 0 to Length(optionMangaSiteSelectionNodes) - 1 do
    begin
      data := vtOptionMangaSiteSelection.GetNodeData(optionMangaSiteSelectionNodes[i]);
      if (optionMangaSiteSelectionNodes[i]^.CheckState = csCheckedNormal) and
        (Data^.Text <> '') then
        cbSelectManga.Items.Add(data^.Text);
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
        cbSelectMangaChange(cbSelectManga);
      end
      else
      begin
        cbSelectManga.ItemIndex := -1;
        cbSelectManga.Text := '';
        currentWebsite := '';
        vtMangaList.Clear;
        lbMode.Caption := Format(RS_ModeAll, [0]);
      end;
    end;
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
    OptionLetFMDDo := TFMDDo(cbOptionLetFMDDo.ItemIndex);
    OptionEnableLoadCover := cbOptionEnableLoadCover.Checked;

    //view
    ToolBarDownload.Visible := cbOptionShowDownloadToolbar.Checked;
    tbDownloadDeleteCompleted.Visible := cbOptionShowDownloadToolbarDeleteAll.Checked;
    tbSeparator1.Visible := tbDownloadDeleteCompleted.Visible;
    ShowDropTarget(ckDropTarget.Checked);
    OptionShowBalloonHint := cbOptionShowBalloonHint.Checked;

    //connection
    DLManager.maxDLTasks := seOptionMaxParallel.Value;
    DLManager.maxDLThreadsPerTask := seOptionMaxThread.Value;
    DLManager.retryConnect := seOptionMaxRetry.Value;
    OptionMaxThreads := seOptionMaxThread.Value;
    SetDefaultTimeoutAndApply(seOptionConnectionTimeout.Value * 1000);
    SetDefaultRetryCountAndApply(seOptionMaxRetry.Value);
    if cbOptionUseProxy.Checked then
      SetDefaultProxyAndApply(cbOptionProxyType.Text, edOptionHost.Text,
        edOptionPort.Text, edOptionUser.Text, edOptionPass.Text)
    else
      SetDefaultProxyAndApply('', '', '' ,'', '');

    //saveto
    OptionPDFQuality := seOptionPDFQuality.Value;
    DLManager.compress := rgOptionCompress.ItemIndex;
    OptionChangeUnicodeCharacter := cbOptionChangeUnicodeCharacter.Checked;
    OptionChangeUnicodeCharacterStr := edOptionChangeUnicodeCharacterStr.Text;
    OptionRemoveMangaNameFromChapter := cbOptionRemoveMangaNameFromChapter.Checked;
    OptionGenerateMangaFolder := cbOptionGenerateMangaFolder.Checked;
    OptionMangaCustomRename := edOptionMangaCustomRename.Text;
    OptionGenerateChapterFolder := cbOptionGenerateChapterFolder.Checked;
    OptionChapterCustomRename := edOptionChapterCustomRename.Text;
    OptionFilenameCustomRename := edOptionFilenameCustomRename.Text;
    OptionConvertDigitVolume := cbOptionDigitVolume.Checked;
    OptionConvertDigitVolumeLength := seOptionDigitVolume.Value;
    OptionConvertDigitChapter := cbOptionDigitChapter.Checked;
    OptionConvertDigitChapterLength := seOptionDigitChapter.Value;

    //update
    OptionAutoCheckLatestVersion := cbOptionAutoCheckLatestVersion.Checked;
    OptionAutoCheckFavStartup := cbOptionAutoCheckFavStartup.Checked;
    OptionAutoCheckFavInterval := cbOptionAutoCheckFavInterval.Checked;
    OptionAutoCheckFavIntervalMinutes := seOptionAutoCheckFavIntervalMinutes.Value;
    OptionNewMangaTime := seOptionNewMangaTime.Value;
    OptionAutoCheckFavDownload := cbOptionAutoCheckFavDownload.Checked;
    OptionAutoCheckFavRemoveCompletedManga := cbOptionAutoCheckFavRemoveCompletedManga.Checked;
    OptionUpdateListNoMangaInfo := cbOptionUpdateListNoMangaInfo.Checked;
    OptionUpdateListRemoveDuplicateLocalData := cbOptionUpdateListRemoveDuplicateLocalData.Checked;
    tmCheckFavorites.Interval := OptionAutoCheckFavIntervalMinutes * 60000;
    tmCheckFavorites.Enabled := OptionAutoCheckFavInterval;

    //misc
    frmCustomColor.Apply;
    SimpleException.SetLogFileName(edLogFileName.Text);

    if ckEnableLogging.Checked and (not Logger.Enabled) then
    begin
      Logger.Enabled := True;
      FileLogger := TFileChannel.Create(edLogFileName.Text, [fcoShowHeader, fcoShowPrefix, fcoShowTime]);
      Logger.Channels.Add(FileLogger);
      Logger.Send(QuotedStrd(Application.Title)+' started with [PID:'+IntToStr(GetProcessID)+'] [HANDLE:'+IntToStr(GetCurrentProcess)+']');
      St := TStringList.Create;
      try
        St.AddText(SimpleException.GetApplicationInfo);
        Logger.Send('Application info', St);
      finally
        St.Free;
      end;
    end
    else
    if (not ckEnableLogging.Checked) and (Logger.Enabled) then
    begin
      Logger.Enabled := False;
      Logger.Channels.Remove(FileLogger);
      FreeAndNil(FileLogger);
    end;

    //languages
    ApplyLanguage;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
  if isStartup then
    DLManager.CheckAndActiveTask;
end;

procedure TMainForm.LoadMangaOptions;
var
  isDeleteUnusedManga: Boolean;
  i, j, sel: Integer;
  lang: TStringList;
  s, currentLanguage: String;
  ANode, currentRootNode: PVirtualNode;
  data: PSingleItem;
  wName, wLang: TStringList;
begin
  DBDownloadURL:=mangalistfile.ReadString('general','DBDownloadURL','');
  wName := TStringList.Create;
  wLang := TStringList.Create;
  lang := TStringList.Create;
  try
    mangalistfile.ReadSection('available', lang);
    TrimStrings(lang);
    if lang.Count > 0 then
      for i := 0 to lang.Count - 1 do
      begin
        s := Trim(mangalistfile.ReadString('available', lang[i], ''));
        if s <> '' then
          ExtractStrings([','], [], PChar(s), wName);
        TrimStrings(wName);
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
            data := GetNodeData(currentRootNode);
            data^.Text := currentLanguage;
            ValidateNode(currentRootNode, False);
          end;
          ANode := AddChild(currentRootNode);
          ANode^.CheckState := csUncheckedNormal;
          data := GetNodeData(ANode);
          data^.Text := wName[i];
          ValidateNode(ANode, False);
          optionMangaSiteSelectionNodes[i] := ANode;
        end;
    end;

    // load selected manga list
    lang.Clear;
    s := configfile.ReadString('general', 'MangaListSelect', DEFAULT_LIST);
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
      sel := configfile.ReadInteger('form', 'SelectManga', 0);
      if sel < 0 then
        sel := 0;
      if sel > cbSelectManga.Items.Count - 1 then
        sel := cbSelectManga.Items.Count - 1;
      cbSelectManga.ItemIndex := sel;
      currentWebsite := cbSelectManga.Items[cbSelectManga.ItemIndex];
    end;
  finally
    lang.Free;
    wName.Free;
    wLang.Free;
  end;
end;

function TMainForm.SaveMangaOptions: String;
var
  i: Integer;
  data: PSingleItem;
begin
  Result := '';
  if Length(optionMangaSiteSelectionNodes) > 0 then
    for i := Low(optionMangaSiteSelectionNodes) to High(optionMangaSiteSelectionNodes) do
    begin
      data := vtOptionMangaSiteSelection.GetNodeData(optionMangaSiteSelectionNodes[i]);
      if optionMangaSiteSelectionNodes[i]^.CheckState = csCheckedNormal then
      begin
        if Result = '' then
          Result := data^.Text
        else
          Result := Result + ',' + data^.Text;
      end;
    end;
end;

procedure TMainForm.edMangaListSearchChange(Sender: TObject);
begin
  if edMangaListSearch.Tag = -1 then
  begin
    edMangaListSearch.Tag := 0;
    LastSearchWeb := currentWebsite;
    LastSearchStr := UpCase(edMangaListSearch.Text);
    Exit;
  end;
  if (not cbOptionLiveSearch.Checked) and (edMangaListSearch.Tag = 0) then Exit;
  if edMangaListSearch.Tag <> 0 then
    edMangaListSearch.Tag := 0;
  if (upcase(edMangaListSearch.Text) = LastSearchStr) and (currentWebsite = LastSearchWeb) then
    Exit;

  SearchDataDB(edMangaListSearch.Text);

  //vtMangaList.Clear;
  //dataProcess.Search(edMangaListSearch.Text);
  //vtMangaList.RootNodeCount := dataProcess.RecordCount;
  //if dataProcess.Filtered then
  //  lbMode.Caption := Format(RS_ModeFiltered, [vtMangaList.RootNodeCount])
  //else
  //  lbMode.Caption := Format(RS_ModeAll, [vtMangaList.RootNodeCount]);
end;

procedure TMainForm.edMangaListSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    edMangaListSearch.Tag := 1;
    edMangaListSearchChange(edMangaListSearch);
  end
  else
  if edMangaListSearch.Tag <> 0 then
    edMangaListSearch.Tag := 0;
end;

procedure TMainForm.edOptionDefaultPathButtonClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
    try
      InitialDir := edOptionDefaultPath.Text;
      if Execute then
        edOptionDefaultPath.Text := CleanAndExpandDirectory(FileName);
    finally
      Free;
    end;
end;

procedure TMainForm.edOptionExternalPathButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      InitialDir := ExtractFileDir(edOptionExternalPath.Text);
      if Execute then
        edOptionExternalPath.Text := FileName;
    finally
      Free;
    end;
end;

procedure TMainForm.edSaveToButtonClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
    try
      InitialDir := edSaveTo.Text;
      if Execute then
        edSaveTo.Text := CleanAndExpandDirectory(FileName);
    finally
      Free;
    end;
end;

procedure TMainForm.edURLButtonClick(Sender: TObject);
var
  i: Integer;
  website,
  host,
  link: String;
begin
  btDownload.Enabled := False;
  btDownloadSplit.Enabled := btDownload.Enabled;
  btAddToFavorites.Enabled := False;
  btReadOnline.Enabled := False;

  website := '';
  SplitURL(edURL.Text, @host, @link);

  if (host <> '') and (link <> '') then
  begin
    host := LowerCase(host);
    i := Modules.LocateModuleByHost(host);
    if i <> -1 then
      website := Modules.Module[i].Website
    else
    begin
      i := LocateMangaSiteID(host);
      if i <> -1 then
        website := WebsiteRoots[i, 0];
    end;
  end;

  if (website = '') or (link = '') then
  begin
    tmAnimateMangaInfo.Enabled := False;
    pbWait.Visible := False;
    MessageDlg('', RS_DlgURLNotSupport, mtInformation, [mbYes], 0);
    Exit;
  end;

  ViewMangaInfo(link, website, '');
end;

procedure TMainForm.UpdateVtChapter;
begin
  if clbChapterList.RootNodeCount = Length(ChapterList) then
    clbChapterList.Repaint
  else
  begin
    clbChapterList.BeginUpdate;
    clbChapterList.RootNodeCount := Length(ChapterList);
    clbChapterList.EndUpdate;
  end;
end;

procedure TMainForm.UpdateVtDownload;
begin
  vtDownloadUpdateFilters;
end;

procedure TMainForm.UpdateVtFavorites;
begin
  if vtFavorites.RootNodeCount = FavoriteManager.Items.Count then
    vtFavorites.Repaint
  else
  begin
    vtFavorites.BeginUpdate;
    vtFavorites.RootNodeCount := FavoriteManager.Count;
    vtFavorites.EndUpdate;
  end;
end;

procedure TMainForm.UpdateVtMangaListFilterStatus;
begin
  if dataProcess.Filtered then
    lbMode.Caption := Format(RS_ModeFiltered, [dataProcess.RecordCount])
  else
    lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
end;

procedure TMainForm.LoadFormInformation;
var
  i: Integer;
begin
  with configfile do
  begin
    psDownloads.Position := ReadInteger('form', 'DownloadsSplitter', psDownloads.Position);
    psInfo.Position := ReadInteger('form', 'MangaInfoSplitter', psInfo.Position);

    if cbOptionAutoCheckFavStartup.Checked and cbOptionAutoOpenFavStartup.Checked then
      pcMain.ActivePage := tsFavorites
    else
      pcMain.PageIndex := ReadInteger('form', 'pcMainPageIndex', 0);

    Left := ReadInteger('form', 'MainFormLeft', MainForm.Left);
    Top := ReadInteger('form', 'MainFormTop', MainForm.Top);
    Width := ReadInteger('form', 'MainFormWidth', 640);
    Height := ReadInteger('form', 'MainFormHeight', 480);

    if ReadBool('form', 'MainFormMaximized', False) then
      PrevWindowState := wsMaximized
    else
      PrevWindowState := wsNormal;
    WindowState := PrevWindowState;

    if vtDownload.Header.Columns.Count > 0 then
     with vtDownload.Header.Columns do
      for i := 0 to Count - 1 do
        Items[i].Width := ReadInteger('form', 'vtDownload' + IntToStr(i) + 'Width', 50);

    if vtFavorites.Header.Columns.Count > 0 then
     with vtFavorites.Header.Columns do
      for i := 0 to Count - 1 do
        Items[i].Width := ReadInteger('form', 'vtFavorites' + IntToStr(i) + 'Width', 50);

    FavoriteManager.SortColumn := ReadInteger('misc', 'SortFavoritesColumn', 1);
    FavoriteManager.sortDirection := ReadBool('misc', 'SortFavoritesDirection', False);
    vtFavorites.Header.SortColumn := FavoriteManager.SortColumn;
    vtFavorites.Header.SortDirection := TSortDirection(FavoriteManager.sortDirection);

    DLManager.SortColumn := ReadInteger('misc', 'SortDownloadColumn', 0);
    DLManager.SortDirection := ReadBool('misc', 'SortDownloadDirection', False);
    vtDownload.Header.SortColumn := DLManager.SortColumn;
    vtDownload.Header.SortDirection := TSortDirection(DLManager.SortDirection);
  end;
end;

procedure TMainForm.SaveFormInformation;
var
  i: Integer;
begin
  with configfile do
  begin
    WriteInteger('form', 'DownloadsSplitter', psDownloads.Position);
    WriteInteger('form', 'MangaInfoSplitter', psInfo.Position);
    WriteInteger('form', 'pcMainPageIndex', pcMain.PageIndex);
    WriteInteger('form', 'SelectManga', cbSelectManga.ItemIndex);
    WriteBool('form', 'MainFormMaximized', (WindowState = wsMaximized));
    if WindowState = wsMaximized then
      WindowState := wsNormal;
    WriteInteger('form', 'MainFormLeft', Left);
    WriteInteger('form', 'MainFormTop', Top);
    WriteInteger('form', 'MainFormWidth', Width);
    WriteInteger('form', 'MainFormHeight', Height);

    if vtDownload.Header.Columns.Count > 0 then
     with vtDownload.Header.Columns do
      for i := 0 to Count - 1 do
        WriteInteger('form', 'vtDownload' + IntToStr(i) + 'Width', Items[i].Width);

    if vtFavorites.Header.Columns.Count > 0 then
     with vtFavorites.Header.Columns do
      for i := 0 to Count - 1 do
        WriteInteger('form', 'vtFavorites' + IntToStr(i) + 'Width', Items[i].Width);

    WriteInteger('misc', 'SortDownloadColumn', vtDownload.Header.SortColumn);
    WriteBool('misc', 'SortDownloadDirection', DLManager.SortDirection);

    WriteInteger('misc', 'SortFavoritesColumn', vtFavorites.Header.SortColumn);
    WriteBool('misc', 'SortFavoritesDirection', FavoriteManager.sortDirection);
  end;
end;

procedure TMainForm.ShowDropTarget(const AShow: Boolean);
begin
  ckDropTarget.Checked := AShow;
  configfile.WriteBool('droptarget', 'Show', AShow);
  if AShow then
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

procedure TMainForm.SaveDropTargetFormInformation;
begin
  with configfile do
  begin
    WriteBool('droptarget', 'Show', ckDropTarget.Checked);
    WriteInteger('droptarget', 'Mode', rgDropTargetMode.ItemIndex);
    WriteInteger('droptarget', 'Opacity', frmDropTarget.FAlphaBlendValue);
    WriteInteger('droptarget', 'Width', frmDropTarget.FWidth);
    WriteInteger('droptarget', 'Heigth', frmDropTarget.FHeight);
    WriteInteger('droptarget', 'Top', frmDropTarget.FTop);
    WriteInteger('droptarget', 'Left', frmDropTarget.FLeft);
  end;
end;

procedure TMainForm.CollectLanguagesFromFiles;
var
  i: Integer;
begin
  cbLanguages.Items.Clear;
  SimpleTranslator.LangDir := CleanAndExpandDirectory(GetCurrentDirUTF8) + 'languages';
  SimpleTranslator.LangAppName := 'fmd';
  SimpleTranslator.CollectLanguagesFiles;
  if SimpleTranslator.AvailableLanguages.Count > 0 then
  begin
    for i := 0 to AvailableLanguages.Count - 1 do
      cbLanguages.Items.Add(SimpleTranslator.AvailableLanguages.ValueFromIndex[i]);
    cbLanguages.ItemIndex := SimpleTranslator.AvailableLanguages.IndexOfName(
    configfile.ReadString('languages', 'Selected', 'en'));
  end;
end;

procedure TMainForm.ApplyLanguage;
var
  idxSelectManga,
  idxLanguages,
  idxFilterStatus,
  idxOptionLetFMDDo,
  idxOptionProxyType,
  idxDropTargetMode: Integer;
begin
  if AvailableLanguages.Count = 0 then Exit;
  if cbLanguages.ItemIndex < 0 then Exit;
  if cbLanguages.ItemIndex >= AvailableLanguages.Count then Exit;
  if SimpleTranslator.LastSelected <> AvailableLanguages.Names[cbLanguages.ItemIndex] then
  begin
    // TCombobox.Items will be cleared upon changing language,
    // and ItemIndex will fall to -1
    // save TComboBox.ItemIndex
    idxSelectManga:=cbSelectManga.ItemIndex;
    idxLanguages := cbLanguages.ItemIndex;
    idxFilterStatus := cbFilterStatus.ItemIndex;
    idxOptionLetFMDDo := cbOptionLetFMDDo.ItemIndex;
    idxOptionProxyType := cbOptionProxyType.ItemIndex;
    idxDropTargetMode := rgDropTargetMode.ItemIndex;
    if SimpleTranslator.SetLangByIndex(cbLanguages.ItemIndex) then
    begin
      // assign new value
      lbOptionExternalParamsHint.Hint := Format(RS_LblOptionExternalParamsHint,
        [EXPARAM_PATH, EXPARAM_CHAPTER, EXPARAM_PATH, EXPARAM_CHAPTER]);
      lbOptionPDFQualityHint.Hint:=lbOptionPDFQuality.Hint;

      cbFilterStatus.Items.Text := RS_FilterStatusItems;
      cbOptionLetFMDDo.Items.Text := RS_OptionFMDDoItems;
      rgDropTargetMode.Items.Text := RS_DropTargetModeItems;

      // restore ItemIndex
      cbSelectManga.ItemIndex:=idxSelectManga;
      cbLanguages.ItemIndex := idxLanguages;
      cbFilterStatus.ItemIndex := idxFilterStatus;
      cbOptionLetFMDDo.ItemIndex := idxOptionLetFMDDo;
      cbOptionProxyType.ItemIndex := idxOptionProxyType;
      rgDropTargetMode.ItemIndex := idxDropTargetMode;
      Self.Repaint;
      vtMangaList.Repaint;
      tvDownloadFilterRefresh(True);

      // refresh custom option
      WebsiteOptionCustomForm.CreateWebsiteOption;
    end;
  end;
end;

procedure TMainForm.OpenWithExternalProgramChapters(const Dir: String;
  const Chapters: TStrings);

  function FindSupportedOutputExt(const Dir, Filename: String): String;
  var
    i: Integer;
    ADir, SDir: String;
  begin
    Result := '';
    if Filename = '' then Exit;
    ADir := CorrectPathSys(Dir);
    if not DirectoryExistsUTF8(ADir) then Exit;
    for i := Low(FMDSupportedOutputExt) to High(FMDSupportedOutputExt) do
    begin
      SDir := ChompPathDelim(CorrectPathSys(ADir + Filename));
      if FileExistsUTF8(SDir + FMDSupportedOutputExt[i]) then
      begin
        Result := GetLastDir(SDir) + FMDSupportedOutputExt[i];
        Break;
      end;
    end;
    if Result = '' then
    begin
      ADir := CorrectPathSys(ADir + Filename);
      if DirectoryExistsUTF8(ADir) then
      Result := GetLastDir(ADir);
    end;
  end;

var
  ADir, AFilename: String;
  i: Integer;
  FindList: TStringList;
  SearchRec: TSearchRec;
begin
  if Dir = '' then Exit;
  ADir := CorrectPathSys(Dir);
  if Assigned(Chapters) then
    if Chapters.Count > 0 then
      for i := 0 to Chapters.Count - 1 do
      begin
        AFilename := FindSupportedOutputExt(ADir, Chapters[i]);
        if AFilename <> '' then
          Break;
      end;

  if AFilename = '' then
    try
      FindList := TStringList.Create;
      if FindFirstUTF8(ADir + '*', faAnyFile and faDirectory, SearchRec) = 0 then
        repeat
          FindList.Add(SearchRec.Name);
        until FindNextUTF8(SearchRec) <> 0;
      if FindList.Count >= 3 then
        AFilename := FindList.Strings[2]
      else
        AFilename := '';
      FindCloseUTF8(SearchRec);
    finally
      FindList.Free;
    end;
  OpenWithExternalProgram(ADir, AFilename);
end;

procedure TMainForm.OpenWithExternalProgram(const Dir, Filename: String);
var
  ADir, AParam, Exe, Params: String;
begin
  Exe := Trim(configfile.ReadString('general', 'ExternalProgramPath', ''));
  Params := Trim(configfile.ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM));

  ADir := Trim(ChompPathDelim(CorrectPathSys(Dir)));
  AParam := Trim(ChompPathDelim(Filename));

  if Exe <> '' then
  begin
    if (Pos(EXPARAM_PATH + EXPARAM_CHAPTER, Params) <> 0) then
      AParam := PathDelim + AParam;
    Params := StringReplace(Params, EXPARAM_PATH, ADir, [rfIgnoreCase, rfReplaceAll]);
    Params := StringReplace(Params, EXPARAM_CHAPTER, AParam, [rfIgnoreCase, rfReplaceAll]);
    RunExternalProcess(Exe, Params, True, False);
  end
  else
  begin
    if (ADir <> '') and (AParam <> '') then
      AParam := ADir + PathDelim + AParam;
    OpenDocument(AParam);
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

procedure TMainForm.DoExitWaitCounter;
begin
  Logger.Send(Self.ClassName+', Execute exit counter');
  if isUpdating then begin
    Logger.Send(Self.ClassName+', Update thread still exist, pending exit counter');
    isPendingExitCounter:=True
  end
  else tmExitCommand.Enabled:=True;
end;

procedure TMainForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
  SimpleException.ExceptionHandle(Sender, E);
end;

procedure TMainForm.tmBackupTimer(Sender: TObject);
begin
  if not DLManager.isRunningBackup then
    DLManager.Backup;
  if not FavoriteManager.isRunning then
    FavoriteManager.Backup;
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
  Data: PSingleItem;
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
  Data: PSingleItem;
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
