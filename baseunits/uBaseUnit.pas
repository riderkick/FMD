{
        File: uBaseUnit.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uBaseUnit;

{$mode delphi}
{$MACRO ON}
{$DEFINE DOWNLOADER}

interface

uses
  {$ifdef windows}
  Windows,
  {$else}
  UTF8Process,
  {$endif}
  SysUtils, Classes, Graphics, Forms, lazutf8classes, LazUTF8, LazFileUtils,
  LConvEncoding, strutils, fileinfo, base64, fpjson, jsonparser, jsonscanner,
  FastHTMLParser, fgl, RegExpr, synautil, httpsend, blcksock, ssl_openssl,
  synacode, GZIPUtils, uFMDThread, uMisc, httpsendthread, simplehtmltreeparser,
  xquery, xquery_json, Imaging, ImagingExtras, SimpleException, SimpleLogger;

type
  TFMDDo = (DO_NOTHING, DO_EXIT, DO_POWEROFF, DO_HIBERNATE, DO_UPDATE);

const
  FMD_REVISION = '$WCREV$';
  FMD_INSTANCE = '_FreeMangaDownloaderInstance_';

  FMD_TARGETOS  = {$i %FPCTARGETOS%};
  FMD_TARGETCPU = {$i %FPCTARGETCPU%};

  JPG_HEADER: array[0..2] of Byte = ($FF, $D8, $FF);
  GIF_HEADER: array[0..2] of Byte = ($47, $49, $46);
  PNG_HEADER: array[0..2] of Byte = ($89, $50, $4E);

  UTF8BOM = #$EF#$BB#$BF;

  EXPARAM_PATH    = '%PATH%';
  EXPARAM_CHAPTER = '%CHAPTER%';
  DEFAULT_EXPARAM = '"' + EXPARAM_PATH + EXPARAM_CHAPTER + '"';

  DATA_PARAM_TITLE      = 0;
  DATA_PARAM_LINK       = 1;
  DATA_PARAM_AUTHORS    = 2;
  DATA_PARAM_ARTISTS    = 3;
  DATA_PARAM_GENRES     = 4;
  DATA_PARAM_STATUS     = 5;
  DATA_PARAM_SUMMARY    = 6;
  DATA_PARAM_NUMCHAPTER = 7;
  DATA_PARAM_JDN        = 8;

  FILTER_HIDE = 0;
  FILTER_SHOW = 1;

  defaultGenres :array [0..37] of string =
    ('Action',       'Adult',        'Adventure',     'Comedy',
    'Doujinshi',     'Drama',        'Ecchi',         'Fantasy',
    'Gender Bender', 'Harem',        'Hentai',        'Historical',
    'Horror',        'Josei',        'Lolicon',       'Martial Arts',
    'Mature',        'Mecha',        'Musical',       'Mystery',
    'Psychological', 'Romance',      'School Life',   'Sci-fi',
    'Seinen',        'Shotacon',     'Shoujo',        'Shoujo Ai',
    'Shounen',       'Shounen Ai',   'Slice of Life', 'Smut',
    'Sports',        'Supernatural', 'Tragedy',       'Yaoi',
    'Yuri',          'Webtoons');

  Symbols: array [0..10] of Char =
    ('\', '/', ':', '*', '?', '"', '<', '>', '|', #9, ';');

  StringFilterChar: array [0..35] of array [0..1] of String = (
    (#10, '\n'),
    (#13, '\r'),
    ('&#x27;', ''''),
    ('&#33;', '!'),
    ('&#36;', '$'),
    ('&#37;', '%'),
    ('&#38;', '&'),
    ('&#39;', ''''),
    ('&#033;', '!'),
    ('&#036;', '$'),
    ('&#037;', '%'),
    ('&#038;', '&'),
    ('&#039;', ''''),
    ('&#8211;', '-'),
    ('&gt;', '>'),
    ('&lt;', '<'),
    ('&amp;', '&'),
    ('&ldquo;', '"'),
    ('&rdquo;', '"'),
    ('&quot;', '"'),
    ('&lsquo;', ''''),
    ('&rsquo;', ''''),
    ('&nbsp;', ' '),
    ('&cent;', '¢'),
    ('&pound;', '£'),
    ('&yen;', '¥'),
    ('&euro;', '©'),
    ('&copy;', '€'),
    ('&reg;', '®'),
    ('［', '['),
    ('］', ']'),
    ('（', '('),
    ('）', ')'),
    ('&frac12;', '½'),
    ('&deg;', '°'),
    ('&sup2;', '²')
    );

  HTMLEntitiesChar: array [0..82] of array [0..1] of String = (
    ('&#171;', '«'),
    ('&#176;', '°'),
    ('&Agrave;', 'À'),
    ('&#192;', 'À'),
    ('&Aacute;', 'Á'),
    ('&#193;', 'Á'),
    ('&Acirc;', 'Â'),
    ('&#194;', 'Â'),
    ('&Atilde;', 'Ã'),
    ('&ccedil;', 'ç'),
    ('&Egrave;', 'È'),
    ('&Eacute;', 'É'),
    ('&Ecirc;', 'Ê'),
    ('&#202;', 'Ê'),
    ('&Etilde;', 'Ẽ'),
    ('&Igrave;', 'Ì'),
    ('&Iacute;', 'Í'),
    ('&Itilde;', 'Ĩ'),
    ('&ETH;', 'Đ'),
    ('&Ograve;', 'Ò'),
    ('&Oacute;', 'Ó'),
    ('&Ocirc;', 'Ô'),
    ('&#212;', 'Ô'),
    ('&Otilde;', 'Õ'),
    ('&Ugrave;', 'Ù'),
    ('&Uacute;', 'Ú'),
    ('&Yacute;', 'Ý'),
    ('&#221;', 'Ý'),
    ('&agrave;', 'à'),
    ('&#224;', 'à'),
    ('&aacute;', 'á'),
    ('&#225;', 'á'),
    ('&acirc;', 'â'),
    ('&#226;', 'â'),
    ('&atilde;', 'ã'),
    ('&#227;', 'ã'),
    ('&#231;', 'ç'),
    ('&egrave;', 'è'),
    ('&#232;', 'è'),
    ('&eacute;', 'é'),
    ('&#233;', 'é'),
    ('&etilde;', 'ẽ'),
    ('&ecirc;', 'ê'),
    ('&#234;', 'ê'),
    ('&igrave;', 'ì'),
    ('&#236;', 'ì'),
    ('&iacute;', 'í'),
    ('&#237;', 'í'),
    ('&itilde;', 'ĩ'),
    ('&#238;', 'î'),
    ('&eth;', 'đ'),
    ('&ograve;', 'ò'),
    ('&#242;', 'ò'),
    ('&oacute;', 'ó'),
    ('&#243;', 'ó'),
    ('&ocirc;', 'ô'),
    ('&#244;', 'ô'),
    ('&otilde;', 'õ'),
    ('&#245;', 'õ'),
    ('&ugrave;', 'ù'),
    ('&#249;', 'ù'),
    ('&uacute;', 'ú'),
    ('&#250;', 'ú'),
    ('&yacute;', 'ý'),
    ('&#253;', 'ý'),
    ('&#8217;', ''''),
    ('&#8220;', '"'),
    ('&#8221;', '"'),
    ('&#8230;', '...'),
    ('&Auml;', 'Ä'),
    ('&auml;', 'ä'),
    ('&Ouml;', 'Ö'),
    ('&ouml;', 'ö'),
    ('&Uuml;', 'Ü'),
    ('&uuml;', 'ü'),
    ('&szlig;', 'ß'),
    ('&mu;', 'μ'),
    ('&#956;', 'μ'),
    ('&raquo;', '»'),
    ('&laquo;', '«'),
    ('&#8216;', '‘'),
    ('&ndash;', '-'),
    ('&gamma;', 'γ')
    );

  README_FILE             = 'readme.rtf';
  WORK_FOLDER             = 'works' + PathDelim;
  WORK_FILE               = 'works.ini';
  DOWNLOADEDCHAPTERS_FILE = 'downloadedchapters.ini';
  FAVORITES_FILE          = 'favorites.ini';
  IMAGE_FOLDER            = 'images' + PathDelim;
  DATA_EXT                = '.dat';
  DBDATA_EXT              = '.db';
  CONFIG_FOLDER           = 'config' + PathDelim;
  CONFIG_FILE             = 'config.ini';
  CONFIG_ADVANCED         = 'advanced.ini';
  REVISION_FILE           = 'revision.ini';
  UPDATE_FILE             = 'updates.ini';
  MANGALIST_FILE          = 'mangalist.ini';
  LANGUAGE_FILE           = 'languages.ini';
  CHANGELOG_FILE          = 'changelog.txt';
  ACCOUNTS_FILE            = 'accounts.db';

  UPDATE_URL = 'https://raw.githubusercontent.com/riderkick/FMD/master/';

  OPTION_MANGALIST = 0;
  OPTION_RECONNECT = 1;

  UNKNOWN_ERROR         = -1;
  NO_ERROR              = 0;
  NET_PROBLEM           = 1;
  INFORMATION_NOT_FOUND = 2;

  SOCKHEARTBEATRATE = 400;

  DEFAULT_LIST = 'AnimeA,MangaFox,MangaHere,MangaInn,MangaReader';
  DEFAULT_MANGA_CUSTOMRENAME = '%MANGA%';
  DEFAULT_CHAPTER_CUSTOMRENAME = '%CHAPTER%';

  FMDFormatSettings :TFormatSettings = (
    CurrencyFormat            :1;
    NegCurrFormat             :5;
    ThousandSeparator         :',';
    DecimalSeparator          :'.';
    CurrencyDecimals          :2;
    DateSeparator             :'/';
    TimeSeparator             :':';
    ListSeparator             :',';
    CurrencyString            :'$';
    ShortDateFormat           :'m/d/y';
    LongDateFormat            :'dd" "mmmm" "yyyy';
    TimeAMString              :'AM';
    TimePMString              :'PM';
    ShortTimeFormat           :'hh:nn';
    LongTimeFormat            :'hh:nn:ss';
    ShortMonthNames           :('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames            :('January', 'February', 'March', 'April', 'May',
                                'June', 'July', 'August', 'September', 'October',
                                'November', 'December');
    ShortDayNames             :('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames              :('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                'Thursday', 'Friday', 'Saturday');
    TwoDigitYearCenturyWindow :50;
    );

  // EN: Param seperator
  SEPERATOR  = '!%~';
  SEPERATOR2 = '~%!';

  // common regex to split host/url
  REGEX_HOST = '(?ig)^(\w+://)?([^/]*\.\w+)?(\:\d+)?(/?.*)$';

  ANIMEA_ID              = 0;
  MANGAINN_ID            = 1;
  OURMANGA_ID            = 2;
  MANGA24H_ID            = 3;
  VNSHARING_ID           = 4;
  FAKKU_ID               = 5;
  TRUYEN18_ID            = 6;
  MANGAPARK_ID           = 7;
  MANGATRADERS_ID        = 8;
  MANGAEDEN_ID           = 9;
  PERVEDEN_ID            = 10;
  TRUYENTRANHTUAN_ID     = 11;
  TURKCRAFT_ID           = 12;
  MANGAVADISI_ID         = 13;
  MANGAFRAME_ID          = 14;
  EATMANGA_ID            = 15;
  STARKANA_ID            = 16;
  BLOGTRUYEN_ID          = 17;
  KOMIKID_ID             = 18;
  ESMANGAHERE_ID         = 19;
  ANIMEEXTREMIST_ID      = 20;
  HUGEMANGA_ID           = 21;
  S2SCAN_ID              = 22;
  IMANHUA_ID             = 23;
  MABUNS_ID              = 24;
  MANGAESTA_ID           = 25;
  CENTRALDEMANGAS_ID     = 26;
  EGSCANS_ID             = 27;
  MANGAAR_ID             = 28;
  MANGAAE_ID             = 29;
  ANIMESTORY_ID          = 30;
  LECTUREENLIGNE_ID      = 31;
  SCANMANGA_ID           = 32;
  MANGAGO_ID             = 33;
  DM5_ID                 = 34;
  MANGACOW_ID            = 35;
  KIVMANGA_ID            = 36;
  MEINMANGA_ID           = 37;
  MANGASPROJECT_ID       = 38;
  MANGAREADER_POR_ID     = 39;
  NINEMANGA_ID           = 40;
  NINEMANGA_ES_ID        = 41;
  NINEMANGA_CN_ID        = 42;
  NINEMANGA_RU_ID        = 43;
  NINEMANGA_DE_ID        = 44;
  NINEMANGA_IT_ID        = 45;
  NINEMANGA_BR_ID        = 46;
  JAPANSHIN_ID           = 47;
  JAPSCAN_ID             = 48;
  CENTRUMMANGI_PL_ID     = 49;
  MANGALIB_PL_ID         = 50;
  ONEMANGA_ID            = 51;
  MANGATOWN_ID           = 52;
  MANGAOKU_ID            = 53;
  MYREADINGMANGAINFO_ID  = 54;
  IKOMIK_ID              = 55;
  NHENTAI_ID             = 56;
  MANGAMINT_ID           = 57;
  UNIXMANGA_ID           = 58;
  EXTREMEMANGAS_ID       = 59;
  MANGAHOST_ID           = 60;
  PORNCOMIX_ID           = 61;
  PORNCOMIXRE_ID         = 62;
  PORNCOMIXIC_ID         = 63;
  XXCOMICS_ID            = 64;
  XXCOMICSMT_ID          = 65;
  XXCOMICS3D_ID          = 66;
  PORNXXXCOMICS_ID       = 67;
  MANGASEE_ID            = 68;
  MANGAKU_ID             = 69;
  MANGAAT_ID             = 70;
  READMANGATODAY_ID      = 71;
  DYNASTYSCANS_ID        = 72;

  WebsiteRoots: array [0..72] of array [0..1] of String = (
    ('AnimeA', 'http://manga.animea.net'),
    ('MangaInn', 'http://www.mangainn.me'),
    ('OurManga', 'http://www.ourmanga.com'),
    ('Manga24h', 'http://manga24h.com'),
    ('VnSharing', 'http://truyen.vnsharing.net'),
    ('Fakku', 'https://www.fakku.net'),
    ('Truyen18', 'http://www.truyen18.org'),
    ('MangaPark', 'http://mangapark.me'),
    ('MangaTraders', 'http://mangatraders.org'),
    ('MangaEden', 'http://www.mangaeden.com'),
    ('PervEden', 'http://www.perveden.com'),
    ('TruyenTranhTuan', 'http://truyentranhtuan.com'),
    ('Turkcraft', 'http://turkcraft.com'),
    ('MangaVadisi', 'http://www.mangavadisi.net'),
    ('MangaFrame', 'http://www.mangaframe.com'),
    ('EatManga', 'http://eatmanga.com'),
    ('Starkana', 'http://starkana.jp'),
    ('BlogTruyen', 'http://blogtruyen.com'),
    ('Komikid', 'http://www.komikid.com'),
    ('ESMangaHere', 'http://es.mangahere.co'),
    ('AnimExtremist', 'http://www.animextremist.com'),
    ('HugeManga', 'http://hugemanga.com'),
    ('S2Scans', 'http://reader.s2smanga.com'),
    ('imanhua', 'http://www.imanhua.com'),
    ('Mabuns', 'http://www.mabuns.web.id'),
    ('MangaEsta', 'http://www.mangaesta.net'),
    ('CentralDeMangas', 'http://centraldemangas.com.br'),
    ('EGScans', 'http://read.egscans.com'),
    ('MangaAr', 'http://manga-ar.net'),
    ('MangaAe', 'http://www.manga.ae'),
    ('AnimeStory', 'http://www.anime-story.com'),
    ('Lecture-En-Ligne', 'http://www.lecture-en-ligne.com'),
    ('ScanManga', 'http://www.scan-manga.com'),
    ('MangaGo', 'http://www.mangago.me'),
    ('DM5', 'http://www.dm5.com'),
    ('Mangacow', 'http://mangacow.co'),
    ('KivManga', 'http://www.kivmanga.com'),
    ('MeinManga', 'http://www.meinmanga.com/'),
    ('MangasPROJECT', 'http://mangaproject.xpg.uol.com.br'),
    ('MangaREADER_POR', 'http://www.mangareader.com.br'),
    ('NineManga', 'http://www.ninemanga.com'),
    ('NineManga_ES', 'http://es.ninemanga.com'),
    ('NineManga_CN', 'http://cn.ninemanga.com'),
    ('NineManga_RU', 'http://ru.ninemanga.com'),
    ('NineManga_DE', 'http://de.ninemanga.com'),
    ('NineManga_IT', 'http://it.ninemanga.com'),
    ('NineManga_BR', 'http://br.ninemanga.com'),
    ('Japan-Shin', 'http://www.japan-shin.com'),
    ('Japscan', 'http://www.japscan.com'),
    ('Centrum-Mangi_PL', 'http://centrum-mangi.pl'),
    ('Manga-Lib_PL', 'http://www.manga-lib.pl/index.php'),
    ('OneManga', 'http://www.onemanga2.com'),
    ('MangaTown', 'http://www.mangatown.com'),
    ('MangaOku', 'http://www.mangaoku.net'),
    ('MyReadingMangaInfo', 'http://myreadingmanga.info'),
    ('I-Komik', 'http://www.i-komik.com'),
    ('NHentai', 'http://nhentai.net'),
    ('MangaMint', 'http://www.mangamint.com'),
    ('UnixManga', 'http://unixmanga.co'),
    ('ExtremeMangas', 'http://www.extrememangas.com'),
    ('MangaHost', 'http://br.mangahost.com'),
    ('PornComix', 'http://porncomix.wf'),
    ('PornComixRE', 'http://porncomix.re'),
    ('PornComixIC', 'http://incest.porncomix.re'),
    ('XXComics', 'http://gallery.xxcomics.net'),
    ('XXComicsMT', 'http://milftoon.xxcomics.net'),
    ('XXComics3D', 'http://3dincest.xxcomics.net'),
    ('PornXXXComics', 'http://pornxxxcomics.com'),
    ('MangaSee', 'http://mangasee.co'),
    ('MangaKu', 'http://mangaku.web.id'),
    ('MangaAt', 'http://www.mangaat.com'),
    ('ReadMangaToday', 'http://www.readmanga.today'),
    ('Dynasty-Scans', 'http://dynasty-scans.com')
    );

  ALPHA_LIST = '#abcdefghijklmnopqrstuvwxyz';

  ANIMEA_BROWSER = '/browse.html?page=';
  ANIMEA_SKIP = '?skip=1';

  MANGAINN_BROWSER = '/mangalist/';

  OURMANGA_BROWSER = '/directory/';

  MANGA24H_BROWSER = '/manga/update/page/';

  VNSHARING_BROWSER = '/DanhSach';

  FAKKU_BROWSER_1 = '/manga/newest';
  FAKKU_BROWSER_2 = '/doujinshi/newest';

  TRUYEN18_ROOT = 'http://www.truyen18.org';
  TRUYEN18_BROWSER = '/moi-dang/danhsach';

  MANGATRADERS_BROWSER = '/directory/';

  MANGAEDEN_BROWSER_1 = '/en-directory/';
  MANGAEDEN_BROWSER_2 = '/it-directory/';

  PERVEDEN_BROWSER_1 = '/en-directory/';
  PERVEDEN_BROWSER_2 = '/it-directory/';

  TRUYENTRANHTUAN_BROWSER = '/danh-sach-truyen';

  TURKCRAFT_BROWSER = '/';

  MANGAVADISI_BROWSER = '/hemenoku/';

  MANGAFRAME_BROWSER = '/okuyucu/directory/';

  EATMANGA_BROWSER = '/Manga-Scan/';
  EATMANGA_maxDLTask: Cardinal = 1;

  STARKANA_BROWSER = '/manga/list';

  BLOGTRUYEN_BROWSER = '/danhsach/tatca';
  BLOGTRUYEN_JS_BROWSER = '/ListStory/GetListStory/';
  BLOGTRUYEN_POST_FORM = 'Url=tatca&OrderBy=1&PageIndex=';

  KOMIKID_BROWSER = '/daftar.php';

  ESMANGAHERE_BROWSER = '/mangalist/';

  ANIMEEXTREMIST_BROWSER = '/mangas.htm?ord=todos';

  HUGEMANGA_BROWSER = '/';

  IMANHUA_BROWSER = '/all.html';

  MABUNS_BROWSER = '/p/mabuns-manga-list.html';

  MANGAESTA_BROWSER = '/p/manga-list.html';

  CENTRALDEMANGAS_BROWSER = '/mangas/list/*';

  EGSCANS_BROWSER = '/';

  MANGAAR_BROWSER = '/manga/';

  MANGAAE_BROWSER = '/manga/';

  ANIMESTORY_BROWSER = '/mangas/';

  LECTUREENLIGNE_BROWSER = '/index.php?page=liste&ordre=titre';

  SCANMANGA_BROWSER = '/scanlation/liste_des_mangas.html';

  MANGAGO_BROWSER = '/list/directory/all/';

  DM5_BROWSER = '/manhua-new';

  //MANGACOW_BROWSER :string = '/manga-list/all/any/name-az/';
  MANGACOW_BROWSER = '/manga-list/all/any/last-added/';

  KIVMANGA_BROWSER = '/';

  MEINMANGA_BROWSER = '/directory/all/';

  MANGASPROJECT_BROWSER = '/AJAX/listaMangas/all';

  MANGAREADER_POR_BROWSER = '/AJAX/listaMangas/all';

  NINEMANGA_BROWSER =
    '/search/?name_sel=contain&wd=&author_sel=contain&author=&artist_sel=contain&artist=&category_id=&out_category_id=&completed_series=either';

  JAPANSHIN_BROWSER = '/lectureenligne/reader/list/';
  JAPSCAN_BROWSER = '/mangas/';

  CENTRUMMANGI_PL_BROWSER = '/spis/';

  MANGALIB_PL_BROWSER = '/manga/directory';

  ONEMANGA_BROWSER = '/manga-list/all/any/last-added/';

  MANGATOWN_BROWSER = '/directory/';

  IKOMIK_BROWSER = '/manga-directory/';

  UNIONMANGAS_BROWSER = '/mangas';

  UNIXMANGA_BROWSER = '/onlinereading/manga-lists.html';

  EXTREMEMANGAS_BROWSER = '/2013/04/lista-de-mangas.html';

  MANGAHOST_BROWSER = '/mangas';

  DYNASTYSCANS_BROWSER: array [0..3] of String = (
    '/anthologies',
    '/doujins',
    '/issues',
    '/series'
    );

var
  FMD_VERSION_NUMBER: String = '';

  DATA_FOLDER: String = 'data' + PathDelim;

  {$IFDEF WINDOWS}
  DEFAULT_PATH: String = '\downloads';
  {$ELSE}
  DEFAULT_PATH: String = '/downloads';
  {$ENDIF}

  // Sites var
  BROWSER_INVERT: Boolean = False;

  FAKKU_BROWSER: String = '/manga/newest';

  MANGAEDEN_BROWSER: String = '/en-directory/';

  PERVEDEN_BROWSER: String = '/en-directory/';

  MANGALIB_PL_COOKIES: String;

  DBDownloadURL: String;
  //------------------------------------------

  Genre: array [0..37] of String;

  Revision: Cardinal;
  currentJDN: Integer;
  isChangeDirectory: Boolean = False;

  currentWebsite: String;

  OptionProxyType: String = '';
  OptionProxyHost: String = '';
  OptionProxyPort: String = '';
  OptionProxyUser: String = '';
  OptionProxyPass: String = '';

  fmdDirectory: String;

  OptionLetFMDDo: TFMDDo = DO_NOTHING;

  OptionChangeUnicodeCharacter: Boolean = False;
  OptionGenerateMangaFolderName: Boolean = False;
  OptionMangaCustomRename: String;
  OptionChapterCustomRename: String;

  OptionPDFQuality: Cardinal = 95;
  OptionConnectionMaxRetry: Integer = 5;
  OptionConnectionTimeout: Integer = 30000;
  OptionUpdateListNoMangaInfo: Boolean = False;
  OptionUpdateListRemoveDuplicateLocalData: Boolean = False;

  OptionMaxThreads: Integer = 1;

  OptionEnableLoadCover: Boolean = False;

  OptionAutoCheckLatestVersion: Boolean = True;
  OptionAutoCheckFavStartup: Boolean = True;
  OptionAutoCheckFavInterval: Boolean = True;
  OptionAutoCheckFavIntervalMinutes: Cardinal = 60;
  OptionNewMangaTime: Cardinal = 1;
  OptionAutoCheckFavDownload: Boolean = False;
  OptionAutoCheckFavRemoveCompletedManga: Boolean = False;

  OptionBatotoShowScanGroup: Boolean = False;
  OptionBatotoShowAllLang: Boolean = False;
  OptionMangaFoxTemplateFolder: String = 'extras' + PathDelim + 'mangafoxtemplate';
  OptionMangaFoxRemoveWatermark: Boolean = True;
  OptionMangaFoxSaveAsPNG: Boolean = False;
  OptionEHentaiDownloadOriginalImage: Boolean = False;

  OptionHTTPUseGzip: Boolean = True;

  OptionRemoveMangaNameFromChapter: Boolean = False;
type
  TArrayOfString = array of String;

  TCheckStyleType = (CS_DIRECTORY_COUNT, CS_DIRECTORY_PAGE,
    CS_DIRECTORY_PAGE_2, CS_INFO);
  TFlagType = (CS_GETPAGENUMBER, CS_GETPAGELINK, CS_DOWNLOAD);
  TDownloadStatusType = (STATUS_STOP, STATUS_WAIT, STATUS_PREPARE,
    STATUS_DOWNLOAD, STATUS_FINISH, STATUS_COMPRESS,
    STATUS_PROBLEM, STATUS_FAILED);
  TDownloadStatusTypes = set of TDownloadStatusType;

  TFavoriteStatusType = (STATUS_IDLE, STATUS_CHECK, STATUS_CHECKING, STATUS_CHECKED);
  TFavoriteStatusTypes = set of TFavoriteStatusType;

  TMemory = Pointer;

  PMangaListItem = ^TMangaListItem;

  TMangaListItem = record
    Text: String;
    JDN: Longint;
  end;

  PSingleItem = ^TSingleItem;

  TSingleItem = record
    Text: String;
  end;

  PChapterStateItem = ^TChapterStateItem;

  TChapterStateItem = record
    Title,
    Link: String;
    Downloaded: Boolean;
  end;

  PMangaInfo = ^TMangaInfo;

  { TMangaInfo }

  TMangaInfo = class
  public
    url,
    title,
    link,
    website,
    coverLink,
    authors,
    artists,
    genres,
    status,
    summary: String;
    numChapter: Cardinal;
    chapterName,
    chapterLinks: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  PDownloadInfo = ^TDownloadInfo;

  TDownloadInfo = record
    Website,
    Link,
    Title,
    SaveTo,
    Status,
    Progress,
    TransferRate: String;
    DateTime: TDateTime;
    iProgress: Integer;
  end;

  PFavoriteInfo = ^TFavoriteInfo;

  TFavoriteInfo = record
    Website,
    Title,
    Link,
    SaveTo,
    Numbering,
    DownloadedChapterList,
    CurrentChapter: String;
  end;

  TCardinalList = TFPGList<Cardinal>;
  TByteList = TFPGList<Byte>;

  TDownloadPageThread = class(TThread)
  protected
    procedure Execute; override;
  public
    isSuccess, isDone: Boolean;
    Retry: Cardinal;
    URL, Path: String;
    constructor Create(CreateSuspended: Boolean);
  end;

  { TParseHTML }

  TParseHTML = class
  private
    FRaw: String;
    procedure FoundTag(NoCaseTag, ActualTag: String);
    procedure FoundText(Text: String);
  public
    Output: TStrings;
    constructor Create(const Raw: String = '');
    function Exec(const Raw: String = ''): String;
    property Raw: String read FRaw write FRaw;
  end;

  { THTTPSendThread }

  THTTPSendThread = class(httpsendthread.THTTPSendThread)
  public
    constructor Create(AOwner: TFMDThread = nil);
  end;

  { THTMLForm }

  THTMLForm = class
  private
    fdata: TStringList;
    fvalueseparator: String;
    fdelimiter: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Put(const AName, AValue: String);
    procedure Remove(const AName: String);
    function GetData: String;
    property ValueSeparator: String read fvalueseparator write fvalueseparator;
    property Delimiter: String read fdelimiter write fdelimiter;
    property Data: TStringList read fdata;
  end;

// Get current binary version
function GetCurrentBinVersion: String;
// Remove Unicode
function UnicodeRemove(const S: String): String;
// Check a directory to see if it's empty (return TRUE) or not
function IsDirectoryEmpty(const ADir: String): Boolean;
function CheckRedirect(const HTTP: THTTPSend): String;
function CorrectFilePath(const APath: String): String;
function CorrectURL(const URL: String): String;
procedure CheckPath(const S: String);

function GetMangaSiteID(const Name: String): Integer;
function GetMangaSiteName(const ID: Cardinal): String;
function GetMangaSiteRoot(const Website: String): String; overload;
function GetMangaSiteRoot(const MangaID: Cardinal): String; overload;
function GetMangaDatabaseURL(const AWebsite: String): String;

function SitesMemberOf(const website: String; MangaSiteIDs: array of Cardinal): Boolean;
function SitesWithSortedList(const website: String): Boolean;
function SitesWithoutFavorites(const website: String): Boolean;
// Return true if the website doesn't contain manga information
function SitesWithoutInformation(const website: String): Boolean;
function SitesWithoutReferer(const website: String): Boolean;
function SitesWithSingleChapter(const website: String): Boolean;

// url
function FillURLProtocol(const AProtocol, AURL: String): String;

// Fill in website host if it's not present
function FillMangaSiteHost(const MangaID: Cardinal; URL: String): String; overload;
function FillMangaSiteHost(const Website, URL: String): String; overload;
function FillHost(const Host, URL: String): String;
function MaybeFillHost(const Host, URL: String): String;

// modify url
function GetHostURL(URL: String): String;
function RemoveHostFromURL(URL: String): String;
procedure RemoveHostFromURLs(const URLs: TStringList);
procedure RemoveHostFromURLsPair(const URLs, Names: TStringList);

//JSON
procedure ParseJSONArray(const S, Path: String; var OutArray: TStringList);

//HTML
procedure ParseHTML(const aRaw: String; aOutput: TStrings);

// XPath / CSS Selector
procedure ParseHTMLTree(var tp: TTreeParser; const S: String);
function SelectXPathString(Expression: String; TP: TTreeParser): String;
function SelectXPathIX(Expression: String; TP: TTreeParser): IXQValue;
function SelectCSSString(Expression: String; TP: TTreeParser): String;
function SelectCSSIX(Expression: String; TP: TTreeParser): IXQValue;

//convert charset to utf8
function ConvertCharsetToUTF8(S: String): String; overload;
procedure ConvertCharsetToUTF8(S: TStrings); overload;

// encode/decode
function Base64Encode(const s: String): String;
function Base64Decode(const s: String): String;

// StringUtils
function PadZero(const S: String; ATotalWidth: Integer = 3;
  PadAll: Boolean = False; StripZero: Boolean = False): String;
procedure PadZeros(S: TStrings; ATotalWidth: Integer = 3;
  PadAll: Boolean = False; StripZeros: Boolean = False);

function StringReplaceBrackets(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
function StreamToString(const Stream: TStream): String; inline;
function GetRightValue(const Name, s: String): String;
function QuotedStrd(const S: String): String; overload; inline;
function QuotedStrd(const S: Integer): String; overload; inline;
function BracketStr(const S: String): String; inline;
function RandomString(SLength: Integer; ONumber: Boolean = False;
  OSymbol: Boolean = False; OSpace: Boolean = False): String;
function GetValuesFromString(Str: String; Sepr: Char): String;
procedure InvertStrings(const St: TStringList); overload;
procedure InvertStrings(const Sts: array of TStringList); overload;
procedure TrimStrings(TheStrings: TStrings);
procedure RemoveDuplicateStrings(Strs: array of TStringList; RemIndex: Cardinal = 0);

procedure CleanHTMLComments(const Str: TStringList);
function FixHTMLTagQuote(const s: String): String;
function FixCommonBrokenHTML(const s: String): String;
function URLDecode(const s: String): String;
function HTMLDecode(const AStr: String): String;

function RemoveSymbols(const input: String): String;
function CorrectPathSys(const Path: String): String;

function CleanString(const S: String): String;
function CleanMultilinedString(const S: String; MaxLineEnding: Integer = 1): String;
function CleanAndExpandURL(const URL: String): String;
function CleanURL(const URL: String): String;
function AppendURLDelim(const URL: String): String;
function AppendURLDelimLeft(const URL: String): String;
function RemoveURLDelim(const URL: String): String; inline;
function RemoveURLDelimLeft(const URL: String): String; inline;
function FixURL(const URL: String): String;
function FixPath(const path: String): String;
function GetLastDir(const path: String): String;
function StringFilter(const Source: String): String;
function HTMLEntitiesFilter(const Source: String): String;
function CommonStringFilter(const Source: String): String;
function StringBreaks(const Source: String): String;
function BreaksString(const Source: String): String;
function RemoveBreaks(const Source: String): String;
function RemoveStringBreaks(const Source: String): String;
function RemoveDoubleSpace(const Source: String): String;
function TrimChar(const Source: String; const Chars: TSysCharSet): String;
function TrimLeftChar(const Source: String; const Chars: TSysCharSet): String;
function TrimRightChar(const Source: String; const Chars: TSysCharSet): String;

function PrepareSummaryForHint(const Source: String; MaxLength: Cardinal = 80): String;
procedure AddCommaString(var Dest: String; S: String);

function StringOfString(c: String; l: Integer): String;
function IncStr(const S: String; N: Integer = 1): String; overload;
function IncStr(const I: Integer; N: Integer = 1): String; overload; inline;

//get heaader value from THTTPSend.Headers
function GetHeaderValue(const AHeaders: TStrings; HName: String): String;

// custom rename feature
function CustomRename(const AString, AWebsite, AMangaName, AAuthor, AArtist,
  AChapter, ANumbering: String; const AIsUnicodeRemove: Boolean): String;

// Get substring from source
function GetString(const Source, sStart, sEnd: String): String;

function Find(const S: String; var List: TStringList; out index: Integer): Boolean;
function FindStrQuick(const s: String; var AStrings: TStringList): Boolean;

// Get param from input
procedure GetParams(const output: TStrings; input: String); overload;
procedure GetParams(var output: TCardinalList; input: String); overload;
procedure GetParams(var output: TList; input: String); overload;
function ExtractParam(const output: TStrings; input, sep: String;
  WhiteSp: Boolean = True): Integer;

function RemoveDuplicateNumbersInString(const AString: String): String;
// Set param from input
function SetParams(input: TObject): String; overload;
function SetParams(const input: array of String): String; overload;

procedure CustomGenres(var output: TStringList; input: String);

// deal with sourceforge URL.
function SourceForgeURL(URL: String): String;
// Get HTML source code from a URL.
function GetPageAndParse(const AHTTP: THTTPSend; Output: TStrings; URL: String;
  const Reconnect: Integer = 0): Integer;
function GetPage(const AHTTP: THTTPSend; var output: TObject; URL: String;
  const Reconnect: Integer = 0; Method: String = 'GET'): Boolean; overload;
function GetPage(var output: TObject; URL: String; const Reconnect: Integer = 0): Boolean;
  overload; inline;
// Get url from a bitly url.
function GetURLFromBitly(const URL: String): String;

// try to save tmemorystream to file, return the saved filename if success, otherwise return empty string
function SaveImageStreamToFile(Stream: TMemoryStream; Path, FileName: String): String;

// Download an image from url and save it to a specific location.
function SaveImage(const AHTTP: THTTPSend; const mangaSiteID: Integer; URL: String;
  const Path, Name: String; var SavedFilename: String; const Reconnect: Integer = 0): Boolean; overload;
function SaveImage(const AHTTP: THTTPSend; const mangaSiteID: Integer; URL: String;
  const Path, Name: String; const Reconnect: Integer = 0): Boolean; overload;
function SaveImage(const AHTTP: THTTPSend; URL: String;
  const Path, Name: String; const Reconnect: Integer = 0): Boolean; overload;
function SaveImage(const mangaSiteID: Integer; URL: String;
  const Path, Name: String; var SavedFilename: String; const Reconnect: Integer = 0): Boolean;
  overload; inline;

procedure QuickSortChapters(var chapterList, linkList: TStringList);
procedure QuickSortData(var merge: TStringList);
// This method uses to sort the data. Use when we load all the lists.
procedure QuickSortDataWithWebID(var merge: TStringList; const webIDList: TByteList);


function GetCurrentJDN: Longint;
function DateToJDN(const year, month, day: Word): Longint; overload;
function DateToJDN(const date: TDate): Longint; overload;
function JDNToDate(const JDN: Longint): TDate;

{function  ConvertInt32ToStr(const aValue: Cardinal)  : String;
function  ConvertStrToInt32(const aStr  : String): Cardinal;}
procedure TransferMangaInfo(var dest: TMangaInfo; const Source: TMangaInfo);

// cross platform funcs

function fmdGetTempPath: String;
function fmdGetTickCount: Cardinal;
procedure fmdPowerOff;
procedure fmdHibernate;

implementation

uses
  {$IFDEF DOWNLOADER}frmMain, WebsiteModules;{$ENDIF}

{$IFDEF WINDOWS}
// thanks Leledumbo for the code
const
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege';
  SE_TCB_NAME = 'SeTcbPrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME = 'SeBackupPrivilege';
  SE_RESTORE_NAME = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_AUDIT_NAME = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege';

function SetSuspendState(hibernate, forcecritical, disablewakeevent: Boolean): Boolean;
  stdcall; external 'powrprof.dll' Name 'SetSuspendState';
function IsHibernateAllowed: Boolean;
  stdcall; external 'powrprof.dll' Name 'IsPwrHibernateAllowed';
function IsPwrSuspendAllowed: Boolean;
  stdcall; external 'powrprof.dll' Name 'IsPwrSuspendAllowed';
function IsPwrShutdownAllowed: Boolean;
  stdcall; external 'powrprof.dll' Name 'IsPwrShutdownAllowed';
function LockWorkStation: Boolean; stdcall; external 'user32.dll' Name 'LockWorkStation';

function NTSetPrivilege(sPrivilege: String; bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin
  Result := True;
  // Only for Windows NT/2000/XP and later.
  if not (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
  Result := False;

  // obtain the processes token
  if OpenProcessToken(GetCurrentProcess(),
    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  begin
    try
      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValue(nil, PChar(sPrivilege),
        TokenPriv.Privileges[0].Luid) then
      begin
        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege

        AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
          PrevTokenPriv, ReturnLength);
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
  if not Result then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;

{$ENDIF}

function GetCurrentBinVersion: String;
var
  AppVerInfo: TStringList;
  i: Integer;
begin
  Result := '';
  AppVerInfo := TStringList.Create;
  with TFileVersionInfo.Create(nil) do
    try
      try
        FileName := ParamStrUTF8(0);
        if FileName = '' then
          FileName := Application.ExeName;
        {$IF FPC_FULLVERSION >= 20701}
        ReadFileInfo;
        {$ENDIF}
        if VersionStrings.Count > 0 then
        begin
        {$IF FPC_FULLVERSION >= 20701}
          AppVerInfo.Assign(VersionStrings);
        {$ELSE}
          for i := 0 to VersionStrings.Count - 1 do
            AppVerInfo.Add(VersionCategories.Strings[i] + '=' +
              VersionStrings.Strings[i]);
        {$ENDIF}
          for i := 0 to AppVerInfo.Count - 1 do
            AppVerInfo.Strings[i] := LowerCase(AppVerInfo.Names[i]) + '=' + AppVerInfo.ValueFromIndex[i];
          Result := AppVerInfo.Values['fileversion'];
        end;
      except
      end;
    finally
      Free;
      AppVerInfo.Free;
    end;
end;

function UnicodeRemove(const S: String): String;
var
  i: Cardinal;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    if (Byte(Result[i]) < 31) or (Byte(Result[i]) > 127) then
    begin
      Delete(Result, i, 1);
      Insert('_', Result, i);
    end;
  end;
end;

function IsDirectoryEmpty(const ADir: String): Boolean;
var
  searchRec: TSearchRec;
begin
  try
    Result := (FindFirstUTF8(CorrectFilePath(ADir) + '*.*', faAnyFile
{$ifdef unix} or faSymLink
{$endif unix}
      , searchRec) = 0) and
      (FindNextUTF8(searchRec) = 0) and
      (FindNextUTF8(searchRec) <> 0);
  finally
    FindCloseUTF8(searchRec);
  end;
end;

function CorrectURL(const URL: String): String;
begin
  Result := StringReplace(URL, ' ', '%20', [rfReplaceAll]);
end;

function CorrectFilePath(const APath: String): String;
var
  i: Integer;
begin
  Result := APath;
  if APath = '' then
    Exit('');
  for i := 1 to Length(Result) do
    if Result[i] = '\' then
      Result[i] := '/';
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
  while system.Pos('//', Result) > 0 do
    Result := StringReplace(Result, '//', '/', []);
end;

// took from an old project - maybe bad code
procedure CheckPath(const S: String);
var
  wS, lcS, lcS2: String;
  i, j: Word;
begin
  wS := s;
  lcS2 := '';
  if wS[2] <> ':' then
  begin
    {$IFDEF WINDOWS}
    lcS2 := CorrectFilePath(fmdDirectory);
    {$ELSE}
    lcS2 := '';
    {$ENDIF}
    Insert('/', wS, 1);
  end
  else
  begin
    if Length(wS) = 2 then
      wS := wS + '/';
  end;
  for i := 1 to Length(wS) do
  begin
    lcS2 := lcS2 + wS[i];
    if (wS[i] = '/') and ((wS[i + 1] <> '/') or (wS[i + 1] <> ' ')) and
      (i < Length(wS)) then
    begin
      j := i + 1;
      lcS := '';
      repeat
        lcS := lcS + wS[j];
        if j = Length(wS) then
          Break;
        Inc(j);
      until wS[j] = '/';
      if not DirectoryExistsUTF8(lcS2 + lcS) then
      begin
        CreateDirUTF8(lcS2 + lcS);
      end;
    end;
  end;
end;

function GetMangaSiteID(const Name: String): Integer;
var
  i: Integer;
begin
  Result := High(WebsiteRoots) + 1;
  for i := Low(WebsiteRoots) to High(WebsiteRoots) do
    if SameText(Name, WebsiteRoots[i, 0]) then
      Exit(i);
end;

function GetMangaSiteName(const ID: Cardinal): String;
begin
  if ID > High(WebsiteRoots) then Exit('');
  Result := WebsiteRoots[ID, 0];
end;

function GetMangaSiteRoot(const Website: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(WebsiteRoots) to High(WebsiteRoots) do
    if Website = WebsiteRoots[i, 0] then
      Exit(WebsiteRoots[i, 1]);
end;

function GetMangaSiteRoot(const MangaID: Cardinal): String;
begin
  Result := WebsiteRoots[MangaID, 1];
end;

function GetMangaDatabaseURL(const AWebsite: String): String;
begin
  if DBDownloadURL = '' then
    DBDownloadURL := 'https://bintray.com/artifact/download/riderkick/FMD/db/<website>.7z';
  Result := DBDownloadURL;
  if Pos('<website>', LowerCase(Result)) > 0 then
    Result := StringReplace(Result, '<website>', AWebsite, [rfIgnoreCase, rfReplaceAll])
  else
    Result := Result + AWebsite;
end;

function SitesMemberOf(const website: String; MangaSiteIDs: array of Cardinal): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(MangaSiteIDs) to High(MangaSiteIDs) do
    if website = WebsiteRoots[MangaSiteIDs[i], 0] then
    begin
      Result := True;
      Break;
    end;
end;

function SitesWithSortedList(const website: String): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Modules.ModuleAvailable(website, i) then
  begin
    Result := Modules.Module[i].SortedList;
    Exit;
  end;
  Result := SitesMemberOf(website, [
    FAKKU_ID,
    NINEMANGA_ID,
    NINEMANGA_ES_ID,
    NINEMANGA_CN_ID,
    NINEMANGA_RU_ID,
    NINEMANGA_DE_ID,
    NINEMANGA_IT_ID,
    NINEMANGA_BR_ID,
    MANGACOW_ID,
    ONEMANGA_ID,
    MYREADINGMANGAINFO_ID,
    NHENTAI_ID,
    PORNCOMIX_ID,
    XXCOMICS_ID,
    XXCOMICSMT_ID,
    XXCOMICS3D_ID,
    PORNCOMIXRE_ID,
    PORNCOMIXIC_ID,
    PORNXXXCOMICS_ID,
    MANGAPARK_ID
    ]);
end;

function SitesWithoutFavorites(const website: String): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Modules.ModuleAvailable(website, i) then
  begin
    Result := not Modules.Module[i].FavoriteAvailable;
    Exit;
  end;
  Result := SitesMemberOf(website, [
    FAKKU_ID,
    MYREADINGMANGAINFO_ID,
    NHENTAI_ID,
    PORNCOMIX_ID,
    XXCOMICS_ID,
    XXCOMICSMT_ID,
    XXCOMICS3D_ID,
    PORNCOMIXRE_ID,
    PORNCOMIXIC_ID,
    PORNXXXCOMICS_ID
    ]);
end;

function SitesWithoutInformation(const website: String): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Modules.ModuleAvailable(website, i) then
  begin
    Result := not Modules.Module[i].InformationAvailable;
    Exit;
  end;
  Result := SitesMemberOf(website, [
    MANGASPROJECT_ID,
    MANGAVADISI_ID,
    TURKCRAFT_ID,
    HUGEMANGA_ID,
    KIVMANGA_ID,
    MANGAOKU_ID,
    UNIXMANGA_ID
    ]);
end;

function SitesWithoutReferer(const website: String): Boolean;
begin
  Result := False;
  Result := SitesMemberOf(website, [
    MEINMANGA_ID,
    IKOMIK_ID,
    PORNCOMIX_ID,
    XXCOMICS_ID,
    XXCOMICSMT_ID,
    XXCOMICS3D_ID,
    PORNCOMIXRE_ID,
    PORNCOMIXIC_ID,
    PORNXXXCOMICS_ID
    ]);
end;

function SitesWithSingleChapter(const website: String): Boolean;
begin
  Result := False;
  Result := SitesMemberOf(website, [
    FAKKU_ID,
    MYREADINGMANGAINFO_ID,
    NHENTAI_ID,
    PORNCOMIX_ID,
    XXCOMICS_ID,
    XXCOMICSMT_ID,
    XXCOMICS3D_ID,
    PORNCOMIXRE_ID
    ]);
end;

function FillURLProtocol(const AProtocol, AURL: String): String;
begin
  Result := AURL;
  if AURL <> '' then begin
    Result := ReplaceRegExpr('^\w*:?//', AURL, '', False);
    if AProtocol <> '' then
      Result := AProtocol + Result;
  end;
end;

function FillMangaSiteHost(const MangaID: Cardinal; URL: String): String;
begin
  Result := URL;
  if MangaID <= High(WebsiteRoots) then
    Result := FillHost(WebsiteRoots[MangaID, 1], URL);
end;

function FillMangaSiteHost(const Website, URL: String): String;
begin
  Result := URL;
  if Website = '' then Exit(URL);
  Result := FillMangaSiteHost(GetMangaSiteID(Website), URL);
end;

function FillHost(const Host, URL: String): String;
var
  tu: String;
begin
  Result := CleanURL(URL);
  if Host = '' then Exit;
  with TRegExpr.Create do
    try
      Expression := REGEX_HOST;
      tu := Replace(Result, '$4', True);
      if tu <> '' then
        Result := RemoveURLDelim(Host) + AppendURLDelimLeft(tu);
    finally
      Free;
    end;
end;

function MaybeFillHost(const Host, URL: String): String;
var
  tu: String;
begin
  Result := CleanURL(URL);
  if Host = '' then Exit;
  if URL = '' then Exit;
  with TRegExpr.Create do
    try
      Expression := REGEX_HOST;
      if Replace(Result, '$2', True) = '' then
      begin
        tu := Replace(Result, '$4', True);
        if tu <> '' then
          Result := RemoveURLDelim(Host) + AppendURLDelimLeft(tu);
      end;
    finally
      Free;
    end;
end;

function GetHostURL(URL: String): String;
begin
  Result := URL;
  if URL = '' then Exit;
  Result := ReplaceRegExpr(REGEX_HOST, Result, '$1$2', True);
end;

function RemoveHostFromURL(URL: String): String;
begin
  Result := ReplaceRegExpr(REGEX_HOST, URL, '$4', True);
  if Result = '' then
    Result := URL;
  if (Result <> '') and (Result[1] <> '/') then
    Result := '/' + Result;
end;

procedure RemoveHostFromURLs(const URLs: TStringList);
var
  i: Integer;
  s: String;
begin
  if URLs = nil then Exit;
  if URLs.Count > 0 then
    with TRegExpr.Create do
      try
        Expression := REGEX_HOST;
        for i := 0 to URLs.Count - 1 do
        begin
          URLs[i] := Trim(URLs[i]);
          s := Replace(URLs[i], '$4', True);
          if s = '' then
            s := URLs[i];
          if (s <> '') and (s[1] <> '/') then
            s := '/' + s;
          URLs[i] := s;
        end;
      finally
        Free;
      end;
end;

procedure RemoveHostFromURLsPair(const URLs, Names: TStringList);
var
  i: Integer;
  s: String;
begin
  if (URLs = nil) or (Names = nil) then Exit;
  if (URLs.Count <> Names.Count) then Exit;
  if URLs.Count > 0 then
    with TRegExpr.Create do
      try
        Expression := REGEX_HOST;
        i := 0;
        while i < URLs.Count do
        begin
          URLs[i] := Trim(URLs[i]);
          s := Replace(URLs[i], '$4', True);
          if s = '' then
            s := URLs[i];
          if (s <> '') and (s[1] <> '/') then
            s := '/' + s;
          URLs[i] := s;
          if (URLs[i] = '') or (URLs[i] = '/') then
          begin
            URLs.Delete(i);
            Names.Delete(i);
          end
          else
            Inc(i);
        end;
      finally
        Free;
      end;
end;

procedure ParseJSONArray(const S, Path: String; var OutArray: TStringList);
var
  P: TJSONParser;
  D: TJSONData;
  O: TJSONObject;
  i: Integer;
begin
  OutArray.BeginUpdate;
  P := TJSONParser.Create(Trim(S), jsonscanner.DefaultOptions);
  try
    D := P.Parse;
    try
      if Assigned(D) then
        if (D.JSONType = jtArray) and (D.Count > 0) then
          for i := 0 to D.Count - 1 do
          begin
            O := TJSONObject(D.Items[i]);
            OutArray.Add(O.Strings[Path]);
          end;
    except
    end;
    D.Free;
  finally
    P.Free;
  end;
  OutArray.EndUpdate;
end;

procedure ParseHTML(const aRaw: String; aOutput: TStrings);
begin
  if not Assigned(aOutput) then Exit;
  with TParseHTML.Create(aRaw) do try
      Output := aOutput;
      Exec;
    finally
      Free;
    end;
end;

procedure ParseHTMLTree(var tp: TTreeParser; const S: String);
begin
  if tp = nil then tp := TTreeParser.Create;
  with tp do begin
    parsingModel := pmHTML;
    repairMissingStartTags := True;
    repairMissingEndTags := True;
    trimText := False;
    readComments := False;
    readProcessingInstructions := False;
    autoDetectHTMLEncoding := False;
    if S <> '' then parseTree(S);
  end;
end;

function SelectXPathString(Expression: String; TP: TTreeParser): String;
begin
  Result := '';
  if TP = nil then Exit;
  if TP.getLastTree = nil then Exit;
  try
    Result := TXQueryEngine.evaluateStaticXPath3(Expression, TP.getLastTree).toString;
  except
  end;
end;

function SelectXPathIX(Expression: String; TP: TTreeParser): IXQValue;
begin
  Result := xqvalue();
  if TP = nil then Exit;
  if TP.getLastTree = nil then Exit;
  try
    Result := TXQueryEngine.evaluateStaticXPath3(Expression, TP.getLastTree);
  except
  end;
end;

function SelectCSSString(Expression: String; TP: TTreeParser): String;
begin
  Result := '';
  if TP = nil then Exit;
  if TP.getLastTree = nil then Exit;
  try
    Result := TXQueryEngine.evaluateStaticCSS3(Expression, TP.getLastTree).toString;
  except
  end;
end;

function SelectCSSIX(Expression: String; TP: TTreeParser): IXQValue;
begin
  Result := xqvalue();
  if TP = nil then Exit;
  if TP.getLastTree = nil then Exit;
  try
    Result := TXQueryEngine.evaluateStaticCSS3(Expression, TP.getLastTree);
  except
  end;
end;

function ConvertCharsetToUTF8(S: String): String;
var
  cs: String;
begin
  Result := S;
  if Trim(S) = '' then Exit;
  with TRegExpr.Create do
    try
      Expression := '(?ig)^.*<meta\s.*charset=([^''";\s]+).*$';
      if Exec(S) then begin
        cs := LowerCase(Replace(S, '$1', True));
        if cs = 'gb2312' then cs := EncodingCP936
        else if (cs = 'big5') or (cs = 'big5-hkscs') then cs := EncodingCP950;
      end
      else cs := GuessEncoding(S);
    finally
      Free;
    end;
  if cs <> '' then Result := ConvertEncoding(S, cs, 'utf8');
end;

procedure ConvertCharsetToUTF8(S: TStrings);
var
  cs: String;
  i: Integer;
begin
  if Trim(S.Text) = '' then Exit;
  cs := '';
  if S.Count > 1 then
  begin
    with TRegExpr.Create do
      try
        Expression := '(?ig)^.*<meta\s.*charset=([^''";\s]+).*$';
        for i := 0 to S.Count - 1 do
          if Pos('/head', S[i]) > 0 then Break
          else if Pos('<meta', S[i]) > 0 then
            if Exec(S[i]) then
            begin
              cs := LowerCase(Replace(S[i], '$1', True));
              if cs = 'gb2312' then cs := EncodingCP936
              else if (cs = 'big5') or (cs = 'big5-hkscs') then cs := EncodingCP950;
              Break;
            end;
      finally
        Free;
      end;
  end;
  if cs = '' then cs := GuessEncoding(S.Text);
  if cs <> '' then S.Text := ConvertEncoding(S.Text, cs, 'utf8');
end;

function StreamToString(const Stream: TStream): String;
var
  p, x: Int64;
begin
  //SetString(Result, PChar(Stream.Memory), Stream.Size div SizeOf(Char));
  p := Stream.Position;
  Stream.Position := 0;
  Setlength(Result, Stream.Size);
  x := Stream.Read(PChar(Result)^, Stream.Size);
  SetLength(Result, x);
  Stream.Position := p;
end;

function GetRightValue(const Name, s: String): String;
var
  i: Integer;
begin
  if s = '' then Exit('');
  if Name = '' then Exit(s);
  i := Pos(Name, s);
  if i > 0 then
    Result := Trim(Copy(s, i + Length(Name), Length(s)));
end;

function QuotedStrd(const S: String): String;
begin
  Result := AnsiQuotedStr(S, '"');
end;

function QuotedStrd(const S: Integer): String;
begin
  Result := QuotedStrd(IntToStr(S));
end;

function BracketStr(const S: String): String;
begin
  Result := '(' + S + ')';
end;

procedure ParseCommandLine(const cmd: String; var Output: TStrings;
  AStripQuotes: Boolean = False);
var
  s, cl: String;
  cq: Integer;
  acl, lq: Boolean;

  procedure Addcl;
  begin
    if cl <> '' then
    begin
      if AStripQuotes and (Length(cl) > 1) then
      begin
        if cl[1] = '"' then
          Delete(cl, 1, 1);
        if cl[Length(cl)] = '"' then
          Delete(cl, Length(cl), 1);
      end;
      Output.Add(cl);
      cl := '';
      acl := False;
    end;
  end;

begin
  if not Assigned(Output) then Exit;
  Output.Clear;
  Output.BeginUpdate;
  try
    s := cmd;
    cl := '';
    cq := 0;
    lq := False;
    while s <> '' do
    begin
      acl := True;
      if s[1] = '"' then
      begin
        Inc(cq);
        lq := True;
      end
      else
      begin
        if s[1] = ' ' then
        begin
          if cq > 0 then
          begin
            if (not odd(cq)) and lq then
            begin
              cq := 0;
              Addcl;
            end;
          end
          else
            Addcl;
        end;
        lq := False;
      end;
      if acl then
        cl := cl + s[1];
      Delete(s, 1, 1);
    end;
    Addcl;
  finally
    Output.EndUpdate;
  end;
end;

function RandomString(SLength: Integer; ONumber: Boolean; OSymbol: Boolean;
  OSpace: Boolean): String;
var
  sgen: String;
const
  alp = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  num = '0123456789';
  sym = '!@#$%^&*()_+[{]}\|;:''",<.>/?';
begin
  Result := '';
  if SLength = 0 then Exit;
  Randomize;
  sgen := alp;
  if ONumber then
    sgen := sgen + num;
  if OSymbol then
    sgen := sgen + sym;
  if OSpace then
    sgen := sgen + #32;
  repeat
    Result := Result + sgen[Random(Length(sgen)) + 1];
  until (Length(Result) = SLength);
end;

function GetValuesFromString(Str: String; Sepr: Char): String;
var
  p: Integer;
  s: String;
begin
  Result := '';
  if Str = '' then Exit;
  p := Pos(Sepr, Str);
  if p > 0 then
  begin
    p := p + Length(Sepr);
    s := Trim(Copy(Str, p, Length(Str)));
    if s <> '' then s := TrimChar(s, ['''', '"', ';', ' ']);
    Result := s;
  end;
end;

procedure InvertStrings(const St: TStringList);
var
  i: Integer;
begin
  if St.Count > 1 then
    for i := 0 to ((St.Count - 1) div 2) do
      St.Exchange(i, St.Count - 1 - i);
end;

function FixHTMLTagQuote(const s: String): String;
begin
  Result := s;
  if Length(Result) > 2 then
  begin
    Result := StringReplace(Result, '=''', '="', [rfReplaceAll]);
    Result := StringReplace(Result, ''' ', '" ', [rfReplaceAll]);
    Result := StringReplace(Result, '''>', '">', [rfReplaceAll]);
    Result := StringReplace(Result, '''/>', '"/>', [rfReplaceAll]);
  end;
end;

function FixCommonBrokenHTML(const s: String): String;
begin
  Result := s;
  Result := StringReplace(Result, '="width="', '="width:', [rfReplaceAll]);
  Result := StringReplace(Result, '"target="', 'target="', [rfReplaceAll]);
  Result := StringReplace(Result, 'rel=''''', '', [rfReplaceAll]);
end;

function URLDecode(const s: String): String;
var
  sAnsi: String;
  sUtf8: String;
  sWide: WideString;

  i, len: Cardinal;
  ESC: String[2];
  CharCode: Integer;
  c: Char;
begin
  sAnsi := PChar(s);
  SetLength(sUtf8, Length(sAnsi));
  i := 1;
  len := 1;
  while (i <= Cardinal(Length(sAnsi))) do begin
    if (sAnsi[i] <> '%') then begin
      if (sAnsi[i] = '+') then begin
        c := ' ';
      end else begin
        c := sAnsi[i];
      end;
      sUtf8[len] := c;
      Inc(len);
    end else begin
      Inc(i);
      ESC := Copy(sAnsi, i, 2);
      Inc(i, 1);
      try
        CharCode := StrToInt('$' + ESC);
        c := Char(CharCode);
        sUtf8[len] := c;
        Inc(len);
      except end;
    end;
    Inc(i);
  end;
  Dec(len);
  SetLength(sUtf8, len);

  sWide := UTF8Decode(sUtf8);
  len := Length(sWide);

  Result := {%H-}sWide;
end;

function HTMLDecode(const AStr: String): String;
var
  Sp, Rp, Cp, Tp: PChar;
  S: String;
  I, Code: Integer;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  Cp := Sp;
  try
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '&': begin
          Cp := Sp;
          Inc(Sp);
          case Sp^ of
            'a': if AnsiStrPos(Sp, 'amp;') = Sp then  { do not localize }
              begin
                Inc(Sp, 3);
                Rp^ := '&';
              end;
            'l',
            'g': if (AnsiStrPos(Sp, 'lt;') = Sp) or (AnsiStrPos(Sp, 'gt;') = Sp) then
                { do not localize }
              begin
                Cp := Sp;
                Inc(Sp, 2);
                while (Sp^ <> ';') and (Sp^ <> #0) do
                  Inc(Sp);
                if Cp^ = 'l' then
                  Rp^ := '<'
                else
                  Rp^ := '>';
              end;
            'n': if AnsiStrPos(Sp, 'nbsp;') = Sp then  { do not localize }
              begin
                Inc(Sp, 4);
                Rp^ := ' ';
              end;
            'q': if AnsiStrPos(Sp, 'quot;') = Sp then  { do not localize }
              begin
                Inc(Sp, 4);
                Rp^ := '"';
              end;
            '#': begin
              Tp := Sp;
              Inc(Tp);
              while (Sp^ <> ';') and (Sp^ <> #0) do
                Inc(Sp);
              SetString(S, Tp, Sp - Tp);
              Val(S, I, Code);
              Rp^ := Chr((I));
            end;
            else
              Exit;
          end;
        end
        else
          Rp^ := Sp^;
      end;
      Inc(Rp);
      Inc(Sp);
    end;
  except
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function RemoveSymbols(const input: String): String;
var
  i: Integer;
begin
  Result := input;
  for i := Low(Symbols) to High(Symbols) do
  begin
    if Pos(Symbols[i], Result) > 0 then
      Result := StringReplace(Result, Symbols[i], '', [rfReplaceAll]);
  end;

  if (Length(Result) > 0) and
    (Result[Length(Result)] = '.') then
  begin
    Result[Length(Result)] := '-';
  end;
end;

procedure InvertStrings(const Sts: array of TStringList);
var
  i: Integer;
begin
  for i := Low(Sts) to High(Sts) do
    InvertStrings(Sts[i]);
end;

procedure TrimStrings(TheStrings: TStrings);
var
  i: Integer;
begin
  if TheStrings = nil then Exit;
  if TheStrings.Count > 0 then
  begin
    i := 0;
    while i < TheStrings.Count do begin
      TheStrings[i] := Trim(TheStrings[i]);
      if TheStrings[i] = '' then TheStrings.Delete(i)
      else Inc(i);
    end;
  end;
end;

procedure RemoveDuplicateStrings(Strs: array of TStringList;
  RemIndex: Cardinal);
var
  i, j, k: Integer;
begin
  if Length(Strs) = 0 then
    Exit;
  if RemIndex > High(Strs) then
    Exit;
  i := 0;
  while i < Strs[RemIndex].Count do
  begin
    j := i + 1;
    while j < Strs[RemIndex].Count do
    begin
      if Strs[RemIndex].Strings[i] = Strs[RemIndex].Strings[j] then
      begin
        for k := 0 to High(Strs) do
          Strs[k].Delete(j);
      end
      else
        Inc(j);
    end;
    Inc(i);
  end;
end;

procedure CleanHTMLComments(const Str: TStringList);
var
  i: Integer;
begin
  if Str.Count > 0 then
  begin
    Str.BeginUpdate;
    for i := 0 to Str.Count - 1 do
    begin
      Str[i] := TrimLeft(Str[i]);
      if (Pos('<!', Str[i]) = 1) or (Pos('-->', Str[i]) = 1) then
        Str[i] := '';
    end;
    Str.EndUpdate;
  end;
end;

function CorrectPathSys(const Path: String): String;
begin
  Result := Trim(Path);
  if Length(Result) > 0 then
  begin
    {$IFDEF WINDOWS}
    //max length = 260
    Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
    if Length(Result) > 0 then
    begin
      if Length(Result) > MAX_PATH - 13 then
        SetLength(Result, MAX_PATH - 13);
      Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
    end;
    if Length(Result) > 0 then
    begin
      if Result[Length(Result)] <> '\' then
        Result := Result + '\';
    end;
    {$ENDIF}
    {$IFDEF UNIX}
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
    if Length(Result) > 0 then
    begin
      if Result[Length(Result)] <> '/' then
        Result := Result + '/';
    end;
    {$ENDIF}
  end;
end;

function StringOfString(c: String; l: Integer): String;
var
  i: Integer;
begin
  Result := '';
  if c = '' then Exit;
  if l < 1 then Exit;
  for i := 1 to l do
    Result += c;
end;

function IncStr(const S: String; N: Integer): String;
var
  i: Integer;
begin
  Result := S;
  i := StrToIntDef(S, -1);
  if i > -1 then
  begin
    Inc(i, N);
    Result := IntToStr(i);
  end;
end;

function IncStr(const I: Integer; N: Integer): String;
begin
  Result := IntToStr(I + N);
end;

function GetHeaderValue(const AHeaders: TStrings; HName: String): String;
var
  i, p: Integer;
begin
  Result := '';
  if (AHeaders.Count > 0) and (HName <> '') then
  begin
    for i := 0 to AHeaders.Count - 1 do
    begin
      if (Pos(lowercase(HName), lowercase(AHeaders.Strings[i])) > 0) then
      begin
        p := Pos(':', AHeaders.Strings[i]);
        if p > 0 then
          Result := Copy(AHeaders.Strings[i], p + 2,
            Length(AHeaders.Strings[i]) - p - 1);
      end;
    end;
  end;
end;

function Base64Encode(const s: String): String;
begin
  if s = '' then Exit(s);
  Result := EncodeStringBase64(s);
end;

function Base64Decode(const s: String): String;
begin
  if s = '' then Exit(s);
  Result := DecodeStringBase64(s);
end;

function PadZero(const S: String; ATotalWidth: Integer; PadAll: Boolean; StripZero: Boolean): String;
var
  isnumber: Boolean;
  n: String;
  i: Integer;

  procedure padn(var R: String);
  begin
    if isnumber then
    begin
      if StripZero then
        while (Length(n) > 0) and (n[1] = '0') do
          Delete(n, 1, 1);
      R := R + StringOfChar('0', ATotalWidth - Length(n)) + n;
      n := '';
      isnumber := False;
    end;
  end;

begin
  if S = '' then Exit(S);
  Result := '';
  isnumber := False;
  n := '';
  for i := 1 to Length(S) do
  begin
    if S[i] in ['0'..'9'] then
    begin
      n := n + S[i];
      isnumber := True;
    end
    else
    begin
      if isnumber then
      begin
        padn(Result);
        if not PadAll then
        begin
          Result := Result + S[i];
          Break;
        end;
      end;
      Result := Result + S[i];
    end;
  end;
  padn(Result);
  Inc(i);
  if i < Length(S) then
    Result := Result + Copy(S, i, Length(S) - i + 1);
end;

procedure PadZeros(S: TStrings; ATotalWidth: Integer; PadAll: Boolean; StripZeros: Boolean);
var
  i: Integer;
begin
  if S = nil then Exit;
  if S.Count = 0 then Exit;
  for i := 0 to S.Count - 1 do
  begin
    S[i] := PadZero(S[i], ATotalWidth, PadAll, StripZeros);
  end;
end;

function StringReplaceBrackets(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
var
  b1, b2: Char;
  p, r: String;
  i: Integer;
begin
  Result := Trim(S);
  if OldPattern = '' then Exit;
  p := Trim(OldPattern);
  r := Trim(NewPattern);
  b1 := #0;
  b2 := #0;
  i := Pos(p, Result);
  if i > 0 then begin
    if i > 1 then b1 := Result[i - 1];
    if i + Length(p) <= Length(Result) then b2 := Result[i + Length(p)];
    if b1 in ['(', '[', '{'] then p := b1 + p else b1 := #0;
    if b2 in [')', ']', '}'] then p := p + b2 else b2 := #0;
    if r <> '' then begin
      if b1 <> #0 then r := b1 + r;
      if b2 <> #0 then r := r + b2;
    end;
    Result := StringReplace(Result, p, r, Flags);
  end;
end;

function CustomRename(const AString, AWebsite, AMangaName, AAuthor, AArtist,
  AChapter, ANumbering: String; const AIsUnicodeRemove: Boolean): String;
var
  chap: String;
begin
  Result := Trim(AString);

  // for rename chapter only
  if AChapter <> '' then begin
    // numbering/index
    if (Pos('%NUMBERING%', Result) = 0) and (Pos('%CHAPTER%', Result) = 0) then
      Result := ANumbering + Result
    else
      Result := Result;
    if AWebsite = WebsiteRoots[FAKKU_ID, 0] then
    begin
      if Pos('%NUMBERING% - ', Result) > 0 then
        Result := StringReplaceBrackets(Result, '%NUMBERING% - ', '', [rfReplaceAll])
      else
        Result := StringReplaceBrackets(Result, '%NUMBERING%', '', [rfReplaceAll]);
    end
    else
      Result := StringReplaceBrackets(Result, '%NUMBERING%', ANumbering, [rfReplaceAll]);
    // pad number
    chap := Trim(AChapter);
    with MainForm.options do begin
      if ReadBool('saveto', 'ConvertDigitVolume', False) then begin
        if ReadBool('saveto', 'ConvertDigitChapter', False) then
          VolumeChapterPadZero(chap, ReadInteger('saveto', 'DigitVolumeLength', 2),
            ReadInteger('saveto', 'DigitChapterLength', 3))
        else
          VolumeChapterPadZero(chap, ReadInteger('saveto', 'DigitVolumeLength', 2), 0);
      end
      else if ReadBool('saveto', 'ConvertDigitChapter', False) then
        VolumeChapterPadZero(chap, 0, ReadInteger('saveto', 'DigitChapterLength', 3));
    end;
    Result := StringReplaceBrackets(Result, '%CHAPTER%', chap, [rfReplaceAll]);

    if Result = '' then begin
      if AWebsite = WebsiteRoots[FAKKU_ID, 0] then
        Result := chap
      else
        Result := ANumbering;
    end;
  end;

  Result := StringReplaceBrackets(Result, '%WEBSITE%', AWebsite, [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, '%MANGA%', AMangaName, [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, '%AUTHOR%', AAuthor, [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, '%ARTIST%', AArtist, [rfReplaceAll]);

  if Result = '' then Result := AMangaName;

  if Result = '' then Exit;

  // strip unicode character
  if AIsUnicodeRemove then
    Result := UnicodeRemove(Result);

  // replace htmlentities
  Result := HTMLEntitiesFilter(StringFilter(Result));

  // remove unaccepted character (Windows)
  Result := RemoveSymbols(Result);

  // remove pathdelim
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '', [rfReplaceAll]);

  Result := Trim(Result);
end;

function GetString(const Source, sStart, sEnd: String): String;
var
  l: Integer;
  s: String;
begin
  Result := '';
  if Length(Source) > 0 then
  begin
    l := Pos(sStart, Source);
    if (l <> 0) and (Source[l + Length(sStart)] <> sEnd[1]) then
    begin
      s := RightStr(Source, Length(Source) - l - Length(sStart) + 1);
      l := Pos(sEnd, s);
      if (l <> 0) then
        Result := LeftStr(s, l - 1);
    end;
  end;
end;

function Find(const S: String; var List: TStringList; out index: Integer): Boolean;
var
  i: Cardinal;
begin
  Result := False;
  index := -1;
  if List.Count = 0 then
    Exit;
  for i := 0 to List.Count - 1 do
  begin
    if CompareText(S, List.Strings[i]) = 0 then
    begin
      index := i;
      Result := True;
      Break;
    end;
  end;
end;

function FindStrQuick(const s: String; var AStrings: TStringList): Boolean;
var
  p: Integer;
begin
  if AStrings.Count > 0 then
  begin
    if not AStrings.Sorted then
      AStrings.Sorted := True;
    Result := AStrings.Find(s, p);
  end
  else
    Result := False;
end;

procedure GetParams(const output: TStrings; input: String);
var
  l: Word;
begin
  repeat
    l := Pos(SEPERATOR, input);
    if l <> 0 then
    begin
      output.Add(LeftStr(input, l - 1));
      input := RightStr(input, Length(input) - l - Length(SEPERATOR) + 1);
    end;
  until l = 0;
end;

procedure GetParams(var output: TCardinalList; input: String);
var
  l: Word;
begin
  repeat
    l := Pos(SEPERATOR, input);
    if l <> 0 then
    begin
      output.Add(StrToInt(LeftStr(input, l - 1)));
      input := RightStr(input, Length(input) - l - Length(SEPERATOR) + 1);
    end;
  until l = 0;
end;

procedure GetParams(var output: TList; input: String);
var
  l: Word;
begin
  repeat
    l := Pos(SEPERATOR, input);
    if l <> 0 then
    begin
      output.Add(Pointer(StrToInt(LeftStr(input, l - 1))));
      input := RightStr(input, Length(input) - l - Length(SEPERATOR) + 1);
    end;
  until l = 0;
end;

function ExtractParam(const output: TStrings; input, sep: String;
  WhiteSp: Boolean): Integer;
var
  l, lse: QWord;
  s: String;
begin
  Result := 0;
  if sep = '' then
    sep := ',';
  lse := Length(sep);
  repeat
    l := Pos(sep, input);
    if l <> 0 then
    begin
      s := LeftStr(input, l - 1);
      if (Length(s) > 0) or WhiteSp then
      begin
        Inc(Result);
        output.Add(s);
      end;
      input := RightStr(input, Length(input) - l - lse + 1);
    end;
  until l = 0;
  if Length(input) > 0 then
    output.Add(input);
end;

function RemoveDuplicateNumbersInString(const AString: String): String;
var
  i, j: Integer;
  list: TList;
begin
  Result := AString;
  if AString = '' then
    Exit;
  list := TList.Create;
  GetParams(list, AString);
  i := 0;
  while i < list.Count do
  begin
    j := i;
    while j < list.Count do
    begin
      if (i <> j) and (list.Items[i] = list.Items[j]) then
        list.Delete(j)
      else
        Inc(j);
    end;
    Inc(i);
  end;
  Result := '';
  for i := 0 to list.Count - 1 do
    Result := Result + IntToStr(Integer(list.Items[i])) + SEPERATOR;
  list.Free;
end;

function SetParams(input: TObject): String;
var
  i: Cardinal;
begin
  Result := '';
  if input is TStringList then
  begin
    if TStringList(input).Count = 0 then
      Exit;
    for i := 0 to TStringList(input).Count - 1 do
      Result := Result + TStringList(input).Strings[i] + SEPERATOR;
  end
  else
  if input is TCardinalList then
  begin
    if TCardinalList(input).Count = 0 then
      Exit;
    for i := 0 to TCardinalList(input).Count - 1 do
      Result := Result + IntToStr(TCardinalList(input).Items[i]) + SEPERATOR;
  end
  else
  if input is TByteList then
  begin
    if TByteList(input).Count = 0 then
      Exit;
    for i := 0 to TByteList(input).Count - 1 do
      Result := Result + IntToStr(TByteList(input).Items[i]) + SEPERATOR;
  end;
end;

function SetParams(const input: array of String): String;
var
  i: Cardinal;
begin
  Result := '';
  if Length(input) = 0 then
    Exit;
  for i := 0 to Length(input) - 1 do
    Result := Result + input[i] + SEPERATOR;
end;

function CleanString(const S: String): String;
begin
  Result := Trim(S);
  if Result = '' then Exit;
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  Result := Trim(Result);
end;

function CleanMultilinedString(const S: String; MaxLineEnding: Integer): String;
var
  rn, rnp, n, np: String;
begin
  Result := Trim(s);
  if Result = '' then Exit;
  if MaxLineEnding < 1 then MaxLineEnding := 1;

  rn := StringOfString(#13#10, MaxLineEnding);
  rnp := rn + #13#10;
  while Pos(rnp, Result) > 0 do
    Result := StringReplace(Result, rnp, rn, [rfReplaceAll]);

  n := StringOfChar(#10, MaxLineEnding);
  np := n + #10;
  while Pos(np, Result) > 0 do
    Result := StringReplace(Result, np, n, [rfReplaceAll]);
end;

function CleanAndExpandURL(const URL: String): String;
begin
  Result := AppendURLDelim(CleanURL(URL));
end;

function CleanURL(const URL: String): String;
var
  x: Integer;
  p: String;
begin
  Result := Trim(URL);
  if Result = '' then Exit;
  if Pos(':', Result) = 1 then
    Delete(Result, 1, 1);
  if Pos('//', Result) = 1 then
    Delete(Result, 1, 2);
  p := '';
  x := Pos('://', Result);
  if x > 0 then
  begin
    x := x + 2;
    p := Copy(Result, 1, x);
    Delete(Result, 1, x);
    while Pos('/', Result) = 1 do
      Delete(Result, 1, 1);
  end;
  while Pos('//', Result) > 0 do
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
  Result := p + Result;
end;

function AppendURLDelim(const URL: String): String;
begin
  Result := URL;
  if (URL <> '') and (URL[Length(URL)] <> '/') then
    Result := URL + '/';
end;

function AppendURLDelimLeft(const URL: String): String;
begin
  Result := URL;
  if (URL <> '') and (URL[1] <> '/') then
    Result := '/' + URL;
end;

function RemoveURLDelim(const URL: String): String;
begin
  Result := TrimRightChar(URL, ['/']);
end;

function RemoveURLDelimLeft(const URL: String): String;
begin
  Result := TrimLeftChar(URL, ['/']);
end;

function FixURL(const URL: String): String;
begin
  Result := URL;
  if Pos(':', Result) or Pos('/', Result) > 0 then
    Result := TrimLeftChar(Result, [':', '/']);
end;

function FixPath(const path: String): String;
var
  i: Cardinal;
begin
  Result := path;
  if Length(path) = 0 then
    Exit;
  for i := 1 to Length(path) do
  begin
    if Byte(path[i]) >= 128 then
      Result := Result + '_'
    else
      Result := Result + path[i];
  end;
end;

function GetLastDir(const path: String): String;
var
  i: Cardinal;
  s: String;
begin
  Result := '';
  if Length(path) = 0 then
    Exit;
  s := path;
  while s[Length(s)] in ['/', '\'] do
    SetLength(s, Length(s) - 1);
  i := Length(s);
  for i := 1 to Length(s) do
  begin
    Result := Result + s[i];
    if (s[i] in ['/', '\']) and (i < Length(s)) then
      Result := '';
  end;
end;

function StringFilter(const Source: String): String;
var
  i: Integer;
begin
  Result := Source;
  if Length(Source) = 0 then
    Exit;

  for i := Low(StringFilterChar) to High(StringFilterChar) do
  begin
    if Pos(StringFilterChar[i, 0], LowerCase(Result)) > 0 then
      Result := StringReplace(Result, StringFilterChar[i, 0], StringFilterChar[i, 1],
        [rfIgnoreCase, rfReplaceAll]);
  end;

  // broken entities
  for i := Low(StringFilterChar) to High(StringFilterChar) do
  begin
    if Length(StringFilterChar[i, 0]) > 1 then
    begin
      if StringFilterChar[i, 0][Length(StringFilterChar[i, 0])] = ';' then
      begin
        if Pos(LeftStr(StringFilterChar[i, 0], Length(StringFilterChar[i, 0]) - 1),
          LowerCase(Result)) > 0 then
          Result := StringReplace(Result, LeftStr(StringFilterChar[i, 0],
            Length(StringFilterChar[i, 0]) - 1), StringFilterChar[i, 1],
            [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
  end;
end;

function HTMLEntitiesFilter(const Source: String): String;
var
  i: Integer;
begin
  Result := Source;
  if Length(Source) = 0 then
    Exit;

  for i := Low(HTMLEntitiesChar) to High(HTMLEntitiesChar) do
  begin
    if Pos(HTMLEntitiesChar[i, 0], Result) > 0 then
      Result := StringReplace(Result, HTMLEntitiesChar[i, 0], HTMLEntitiesChar[i, 1],
        [rfIgnoreCase, rfReplaceAll]);
  end;

  // broken entities
  for i := Low(HTMLEntitiesChar) to High(HTMLEntitiesChar) do
  begin
    if Length(HTMLEntitiesChar[i, 0]) > 1 then
    begin
      if HTMLEntitiesChar[i, 0][Length(HTMLEntitiesChar[i, 0])] = ';' then
      begin
        if Pos(LeftStr(HTMLEntitiesChar[i, 0], Length(HTMLEntitiesChar[i, 0]) - 1), Result) > 0 then
          Result := StringReplace(Result, LeftStr(HTMLEntitiesChar[i, 0],
            Length(HTMLEntitiesChar[i, 0]) - 1), HTMLEntitiesChar[i, 1],
            [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
  end;
end;

procedure CustomGenres(var output: TStringList; input: String);
var
  s: String = '';
  i: Word;
begin
  if Length(input) = 0 then
    Exit;
  for i := 1 to Length(input) do
  begin
    if (input[i] = ',') or (input[i] = ';') then
    begin
      TrimLeft(TrimRight(s));
      if Length(s) <> 0 then
      begin
        output.Add(s);
        s := '';
      end;
    end
    else
      s := s + input[i];
  end;
  TrimLeft(TrimRight(s));
  if Length(s) <> 0 then
    output.Add(s);
end;

function CommonStringFilter(const Source: String): String;
begin
  Result := Source;
  if Source = '' then Exit;
  Result := Trim(HTMLEntitiesFilter(StringFilter(Trim(Source))));
end;

function StringBreaks(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
end;

function BreaksString(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
end;

function RemoveBreaks(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
end;

function RemoveStringBreaks(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, '\n', '', [rfReplaceAll]);
  Result := StringReplace(Result, '\r', '', [rfReplaceAll]);
end;

function RemoveDoubleSpace(const Source: String): String;
begin
  Result := Source;
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

function TrimChar(const Source: String; const Chars: TSysCharSet): String;
begin
  Result := Source;
  if Length(Result) > 0 then
    while (Length(Result) > 0) and (Result[1] in Chars) do
      Delete(Result, 1, 1);
  if Length(Result) > 0 then
    while (Length(Result) > 0) and (Result[Length(Result)] in Chars) do
      Delete(Result, Length(Result), 1);
end;

function TrimLeftChar(const Source: String; const Chars: TSysCharSet): String;
var
  i, j: Longint;
begin
  Result := Source;
  i := Length(Result);
  if i > 0 then
  begin
    j := 1;
    while (j <= i) and (Result[j] in Chars) do
      Inc(j);
    if j > 1 then
      Delete(Result, 1, j - 1);
  end;
end;

function TrimRightChar(const Source: String; const Chars: TSysCharSet): String;
var
  i, j: Longint;
begin
  Result := Source;
  i := Length(Result);
  if i > 0 then
  begin
    j := i;
    while (j > 0) and (Result[j] in Chars) do
      Dec(j);
    if j <> i then
      SetLength(Result, j);
  end;
end;

function PrepareSummaryForHint(const Source: String; MaxLength: Cardinal = 80): String;
var
  i: Cardinal = 1;
  j: Cardinal = 1;
begin
  Result := Source;
  repeat
    if (j > MaxLength) and (Result[i] = ' ') then
    begin
      Insert(#10#13, Result, i);
      Inc(i, 2);
      j := 1;
    end;
    Inc(j);
    Inc(i);
  until i >= Length(Result);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll, rfIgnoreCase]);
  Result := TrimLeft(TrimRight(Result));
end;

procedure AddCommaString(var Dest: String; S: String);
begin
  S := Trim(TrimChar(Trim(S), [',', ' ']));
  if (S = '') or (S = ',') then Exit;
  if Dest = '' then
    Dest := S
  else
    Dest := Dest + ', ' + S;
end;

function CheckRedirect(const HTTP: THTTPSend): String;
var
  lineHeader: String;
  i: Byte;
begin
  Result := '';
  i := 0;
  while (Result = '') and (i < HTTP.Headers.Count) do
  begin
    lineHeader := HTTP.Headers[I];
    if Pos('Location: ', lineHeader) = 1 then
      Result := Copy(lineHeader, 11, Length(lineHeader));
    Inc(i);
  end;
end;

function SFDirectLinkURL(URL: String; Document: TMemoryStream): String;
{
Transform this part of the body:
<noscript>
<meta http-equiv="refresh" content="5; url=http://downloads.sourceforge.net/project/base64decoder/base64decoder/version%202.0/b64util.zip?r=&amp;ts=1329648745&amp;use_mirror=kent">
</noscript>
into a valid URL:
http://downloads.sourceforge.net/project/base64decoder/base64decoder/version%202.0/b64util.zip?r=&amp;ts=1329648745&amp;use_mirror=kent
}
const
  Refresh = '<meta http-equiv="refresh"';
  URLMarker = 'url=';
var
  Counter: Integer;
  HTMLBody: TStringList;
  RefreshStart: Integer;
  URLStart: Integer;
begin
  HTMLBody := TStringList.Create;
  try
    HTMLBody.LoadFromStream(Document);
    for Counter := 0 to HTMLBody.Count - 1 do
    begin
      // This line should be between noscript tags and give the direct download locations:
      RefreshStart := Ansipos(Refresh, HTMLBody[Counter]);
      if RefreshStart > 0 then
      begin
        URLStart := AnsiPos(URLMarker, HTMLBody[Counter]) + Length(URLMarker);
        if URLStart > RefreshStart then
        begin
          // Look for closing "
          URL := Copy(HTMLBody[Counter],
            URLStart,
            PosEx('"', HTMLBody[Counter], URLStart + 1) - URLStart);
          Break;
        end;
      end;
    end;
  finally
    HTMLBody.Free;
  end;
  Result := URL;
end;

function SourceForgeURL(URL: String): String;
  // Detects sourceforge download and tries to deal with
  // redirection, and extracting direct download link.
  // Thanks to
  // Ocye: http://lazarus.freepascal.org/index.php/topic,13425.msg70575.html#msg70575
const
  SFProjectPart = '//sourceforge.net/projects/';
  SFFilesPart = '/files/';
  SFDownloadPart = '/download';
var
  HTTPSender: THTTPSend;
  i, j: Integer;
  FoundCorrectURL: Boolean;
  SFDirectory: String; //Sourceforge directory
  SFDirectoryBegin: Integer;
  SFFileBegin: Integer;
  SFFilename: String; //Sourceforge name of file
  SFProject: String;
  SFProjectBegin: Integer;
label
  loop;
begin
  // Detect SourceForge download; e.g. from URL
  //          1         2         3         4         5         6         7         8         9
  // 1234557890123456789012345578901234567890123455789012345678901234557890123456789012345578901234567890
  // http://sourceforge.net/projects/base64decoder/files/base64decoder/version%202.0/b64util.zip/download
  //                                 ^^^project^^^       ^^^directory............^^^ ^^^file^^^
  FoundCorrectURL := True; //Assume not a SF download
  i := Pos(SFProjectPart, URL);
  if i > 0 then
  begin
    // Possibly found project; now extract project, directory and filename parts.
    SFProjectBegin := i + Length(SFProjectPart);
    j := PosEx(SFFilesPart, URL, SFProjectBegin);
    if (j > 0) then
    begin
      SFProject := Copy(URL, SFProjectBegin, j - SFProjectBegin);
      SFDirectoryBegin := PosEx(SFFilesPart, URL, SFProjectBegin) + Length(SFFilesPart);
      if SFDirectoryBegin > 0 then
      begin
        // Find file
        // URL might have trailing arguments... so: search for first
        // /download coming up from the right, but it should be after
        // /files/
        i := RPos(SFDownloadPart, URL);
        // Now look for previous / so we can make out the file
        // This might perhaps be the trailing / in /files/
        SFFileBegin := RPosEx('/', URL, i - 1) + 1;

        if SFFileBegin > 0 then
        begin
          SFFilename := Copy(URL, SFFileBegin, i - SFFileBegin);
          //Include trailing /
          SFDirectory := Copy(URL, SFDirectoryBegin, SFFileBegin - SFDirectoryBegin);
          FoundCorrectURL := False;
        end;
      end;
    end;
  end;

  if not FoundCorrectURL then
  begin
    try
      // Rewrite URL if needed for Sourceforge download redirection
      // Detect direct link in HTML body and get URL from that
      loop:
        HTTPSender := THTTPSend.Create;
      //Who knows, this might help:
      HTTPSender.UserAgent := UA_CURL;
      while not FoundCorrectURL do
      begin
        HTTPSender.HTTPMethod('GET', URL);
        case HTTPSender.Resultcode of
          301, 302, 307:
          begin
            for i := 0 to HTTPSender.Headers.Count - 1 do
              if (Pos('Location: ', HTTPSender.Headers.Strings[i]) > 0) or
                (Pos('location: ', HTTPSender.Headers.Strings[i]) > 0) then
              begin
                j := Pos('use_mirror=', HTTPSender.Headers.Strings[i]);
                if j > 0 then
                  URL :=
                    'http://' + RightStr(HTTPSender.Headers.Strings[i],
                    length(HTTPSender.Headers.Strings[i]) - j - 10) +
                    '.dl.sourceforge.net/project/' +
                    SFProject + '/' + SFDirectory + SFFilename
                else
                  URL := StringReplace(
                    HTTPSender.Headers.Strings[i], 'Location: ', '', []);
                HTTPSender.Clear;//httpsend
                FoundCorrectURL := True;
                Break; //out of rewriting loop
              end;
          end;
          100..200:
          begin
            //Assume a sourceforge timer/direct link page
            URL := SFDirectLinkURL(URL, HTTPSender.Document); //Find out
            FoundCorrectURL := True; //We're done by now
          end;
          else
          begin
            HTTPSender.Free;
            goto loop;
          end;
        end;//case
      end;  //while
    finally
      HTTPSender.Free;
    end;
  end;
  Result := URL;
end;

function GetPageAndParse(const AHTTP: THTTPSend; Output: TStrings; URL: String;
  const Reconnect: Integer): Integer;
begin
  if Output = nil then Exit(INFORMATION_NOT_FOUND);
  if GetPage(AHTTP, TObject(Output), URL, Reconnect) then
  begin
    if Output.Count > 0 then
    begin
      Result := NO_ERROR;
      ParseHTML(Output.Text, Output);
    end
    else
      Result := INFORMATION_NOT_FOUND;
  end
  else
    Result := NET_PROBLEM;
end;

function GetPage(const AHTTP: THTTPSend; var output: TObject; URL: String;
  const Reconnect: Integer; Method: String): Boolean;
  // If AHTTP <> nil, we will use it as http sender. Otherwise we create a new
  // instance.
var
  HTTP: THTTPSend;
  HTTPHeader: TStringList;
  counter: Integer;
  s: String;
  meth: String;
  mstream: TMemoryStream;

  procedure HTTPClear;
  begin
    if Assigned(HTTP) then
      with HTTP do
      begin
        RangeStart := 0;
        RangeEnd := 0;
        Headers.Clear;
      end;
  end;

  procedure preTerminate;
  begin
    HTTPHeader.Free;
    if AHTTP = nil then
      HTTP.Free;
  end;

  function checkTerminate: Boolean;
  begin
    Result := HTTP.Sock.Tag = 1; //terminate via THTTPSendThread;
    if Result then
    begin
      HTTP.Sock.Tag := 0;
      preTerminate;
    end;
  end;

label
  globReturn;

begin
  Result := False;
  if Trim(URL) = '' then Exit;

  URL := FixURL(URL);
  URL := EncodeURL(DecodeURL(URL));

  HTTPHeader := TStringList.Create;
  HTTPHeader.NameValueSeparator := ':';
  if AHTTP <> nil then
  begin
    if LeftStr(AHTTP.Headers.Text, 5) <> 'HTTP/' then
      HTTPHeader.Text := AHTTP.Headers.Text;
    HTTP := AHTTP;
    HTTPClear;
  end
  else
  begin
    HTTP := THTTPSend.Create;
    HTTP.Timeout := OptionConnectionTimeout;
    HTTP.Sock.ConnectionTimeout := OptionConnectionTimeout;
    HTTP.Sock.SetTimeout(OptionConnectionTimeout);
  end;
  HTTP.Headers.NameValueSeparator := ':';

  globReturn:

    if OptionProxyType = 'HTTP' then
    begin
      HTTP.ProxyHost := OptionProxyHost;
      HTTP.ProxyPort := OptionProxyPort;
      HTTP.ProxyUser := OptionProxyUser;
      HTTP.ProxyPass := OptionProxyPass;
    end
    else
    if (OptionProxyType = 'SOCKS4') or (OptionProxyType = 'SOCKS5') then
    begin
      if OptionProxyType = 'SOCKS4' then
        HTTP.Sock.SocksType := ST_Socks4
      else
      if OptionProxyType = 'SOCKS5' then
        HTTP.Sock.SocksType := ST_Socks5;
      HTTP.Sock.SocksIP := OptionProxyHost;
      HTTP.Sock.SocksPort := OptionProxyPort;
      HTTP.Sock.SocksUsername := OptionProxyUser;
      http.Sock.SocksPassword := OptionProxyPass;
    end
    else
    begin
      HTTP.Sock.SocksIP := OptionProxyHost;
      HTTP.Sock.SocksPort := OptionProxyPort;
      HTTP.Sock.SocksUsername := OptionProxyUser;
      http.Sock.SocksPassword := OptionProxyPass;
      HTTP.ProxyHost := OptionProxyHost;
      HTTP.ProxyPort := OptionProxyPort;
      HTTP.ProxyUser := OptionProxyUser;
      HTTP.ProxyPass := OptionProxyPass;
    end;

  HTTPHeader.Values['DNT'] := ' 1';
  HTTPHeader.Values['Accept'] := ' text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  HTTPHeader.Values['Accept-Charset'] := ' UTF-8';
  HTTPHeader.Values['Accept-Language'] := ' en-US,en;q=0.8';
  HTTP.Protocol := '1.1';
  HTTP.KeepAlive := False;
  if (HTTP.UserAgent = '') or (HTTP.UserAgent = UA_SYNAPSE) then
    HTTP.UserAgent := DEFAULT_UA;
  if OptionHTTPUseGzip then
    HTTPHeader.Values['Accept-Encoding'] := ' gzip, deflate';

  //Method
  if Method <> '' then meth := Method
  else meth := 'GET';
  if HTTP.Sock.Tag = 100 then //POST form
    meth := 'POST';
  if meth = 'POST' then
    HTTP.MimeType := 'application/x-www-form-urlencoded; charset=UTF-8'
  else
  begin
    HTTP.Document.Clear;
    HTTP.RangeStart := 0;
    HTTP.RangeEnd := 0;
  end;

  //User-Agent
  if Trim(HTTPHeader.Values['User-Agent']) <> '' then
  begin
    HTTP.UserAgent := Trim(HTTPHeader.Values['User-Agent']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('User-Agent'));
  end;
  //MimeType
  if Trim(HTTPHeader.Values['Content-Type']) <> '' then
  begin
    HTTP.MimeType := Trim(HTTPHeader.Values['Content-Type']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('Content-Type'));
  end;

  if Pos(WebsiteRoots[MEINMANGA_ID, 1], URL) > 0 then
    HTTPHeader.Values['Accept-Charset'] := ' utf8'
  else
  if Pos(WebsiteRoots[MANGALIB_PL_ID, 1], URL) > 0 then
  begin
    if MANGALIB_PL_COOKIES <> '' then
      HTTP.Cookies.Text := MANGALIB_PL_COOKIES;
    if (Pos('/page/confirm_', URL) > 0) then
    begin
      s := ReplaceRegExpr('^.*/confirm_(.+)\?backlink.*$', URL, '$1', True) + '=1';
      meth := 'POST';
      HTTP.Document.Clear;
      HTTP.Document.Position := 0;
      HTTP.Document.Write(PChar(s)^, Length(s));
      HTTP.Protocol := '1.1';
      HTTP.MimeType := 'application/x-www-form-urlencoded';
      HTTPHeader.Values['Referer'] := ' ' + URL;
      HTTPHeader.Values['Accept'] := ' text/html';
    end;
  end
  else
  if (Pos('imgmega.com/', URL) > 0) then
  begin
    s := ReplaceRegExpr('^.*\w+\.\w+/(\w+)/.*$', URL, '$1', True);
    s := 'op=view&id=' + s + '&pre=1&next=Continue+to+image...';
    meth := 'POST';
    HTTP.Document.Clear;
    HTTP.Document.Position := 0;
    HTTP.Document.Write(PChar(s)^, Length(s));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
  end;

  if checkTerminate then Exit;
  HTTP.Headers.Text := HTTPHeader.Text;
  counter := 0;
  while (not HTTP.HTTPMethod(meth, URL)) or (HTTP.ResultCode > 500) do
  begin
    if checkTerminate then Exit;
    if (Reconnect > -1) and (Reconnect <= counter) then
    begin
      preTerminate;
      Exit;
    end;
    Inc(Counter);
    HTTPClear;
    HTTP.Headers.Text := HTTPHeader.Text;
  end;

  while (HTTP.ResultCode > 300) and (HTTP.ResultCode < 400) do
  begin
    if checkTerminate then Exit;
    HTTPHeader.Values['Referer'] := ' ' + URL;
    s := Trim(HTTP.Headers.Values['Location']);
    if s <> '' then
    begin
      with TRegExpr.Create do
        try
          Expression := REGEX_HOST;
          if Replace(s, '$1', True) = '' then
          begin
            if s[1] <> '/' then
              s := '/' + s;
            URL := Replace(URL, '$1$2$3', True) + s;
          end
          else
            URL := s;
        finally
          Free;
        end;
    end;

    HTTP.Clear;
    HTTP.Headers.Text := HTTPHeader.Text;
    counter := 0;
    while (not HTTP.HTTPMethod('GET', URL)) or (HTTP.ResultCode > 500) do
    begin
      if checkTerminate then Exit;
      if (Reconnect > -1) and (Reconnect <= counter) then
      begin
        preTerminate;
        Exit;
      end;
      Inc(counter);
      HTTP.Clear;
      HTTP.Headers.Text := HTTPHeader.Text;
    end;
  end;

  if HTTP.ResultCode <> 404 then
  begin
    // Decompress the html file
    s := LowerCase(HTTP.Headers.Values['Content-Encoding']);
    if (Pos('gzip', s) <> 0) or (Pos('deflate', s) <> 0) then
    begin
      mstream := TMemoryStream.Create;
      try
        ZUncompressStream(HTTP.Document, mstream);
        HTTP.Document.Clear;
        HTTP.Document.LoadFromStream(mstream);
      except
      end;
      mstream.Free;
    end;
    try
      if output is TStringList then
        TStringList(output).LoadFromStream(HTTP.Document)
      else
      if output is TPicture then
        TPicture(output).LoadFromStream(HTTP.Document)
      else
      if output is TStream then
        HTTP.Document.SaveToStream(TStream(output));
    except
      on E: Exception do
        WriteLog_E('GetPage.WriteOutput.Error!', E);
    end;
    Result := True;
  end
  else
    Result := False;

  preTerminate;
end;

function GetPage(var output: TObject; URL: String; const Reconnect: Integer
  ): Boolean;
begin
  Result := GetPage(nil, output, URL, Reconnect);
end;

function GetURLFromBitly(const URL: String): String;
var
  i: Cardinal;
  httpSource: TStringList;
begin
  Result := '';
  httpSource := TStringList.Create;
  GetPage(TObject(httpSource), URL, 4);
  if httpSource.Count > 0 then
    for i := 0 to httpSource.Count do
      if Pos(';url=', httpSource.Strings[i]) > 0 then
      begin
        Result := GetString(httpSource.Strings[i], ';url=', '&amp;');
        Break;
      end;
  httpSource.Free;
end;

function SaveImageStreamToFile(Stream: TMemoryStream; Path, FileName: String
  ): String;
var
  p, f: String;
  fs: TFileStreamUTF8;
begin
  Result := '';
  if Stream = nil then Exit;
  if Stream.Size = 0 then Exit;
  p := CleanAndExpandDirectory(Path);
  if not DirectoryExistsUTF8(p) then ForceDirectoriesUTF8(p);
  if DirectoryExistsUTF8(p) then begin
    f := DetermineStreamFormat(Stream);
    if f = '' then Exit;
    f := p + FileName + '.' + f;
    if FileExistsUTF8(f) then DeleteFileUTF8(f);
    try
      fs := TFileStreamUTF8.Create(f, fmCreate);
      try
        Stream.SaveToStream(fs);
      finally
        fs.Free;
      end;
    except
      on E: Exception do
        WriteLog_E('SaveImageStreamToFile.Failed!', E);
    end;
    if FileExistsUTF8(f) then Result := f;
  end;
end;

function SaveImage(const AHTTP: THTTPSend; const mangaSiteID: Integer;
  URL: String; const Path, Name: String; var SavedFilename: String;
  const Reconnect: Integer): Boolean;
var
  HTTPHeader: TStringList;
  HTTP: THTTPSend;
  counter: Integer;
  s: String;

  procedure preTerminate;
  begin
    HTTPHeader.Free;
    if AHTTP = nil then
      HTTP.Free;
  end;

  function checkTerminate: Boolean;
  begin
    Result := HTTP.Sock.Tag = 1; //terminate via THTTPSendThread
    if Result then
    begin
      HTTP.Sock.Tag := 0;
      preTerminate;
    end;
  end;

begin
  Result := False;
  if Trim(URL) = '' then Exit;

  // Check to see if a file with similar name was already exist. If so then we
  // skip the download process.
  if Trim(URL) = 'D' then Exit(True);
  s := CleanAndExpandDirectory(Path) + Name;
  if (FileExistsUTF8(s + '.jpg')) or
    (FileExistsUTF8(s + '.png')) or
    (FileExistsUTF8(s + '.gif')) then
    Exit(True);

  URL := FixURL(URL);
  URL := EncodeURL(DecodeURL(URL));

  HTTPHeader := TStringList.Create;
  HTTPHeader.NameValueSeparator := ':';
  if AHTTP <> nil then
  begin
    if LeftStr(AHTTP.Headers.Text, 5) <> 'HTTP/' then
      HTTPHeader.Text := AHTTP.Headers.Text;
    HTTP := AHTTP;
    HTTP.Clear;
  end
  else
  begin
    HTTP := THTTPSend.Create;
    HTTP.Timeout := OptionConnectionTimeout;
    HTTP.Sock.ConnectionTimeout := OptionConnectionTimeout;
    HTTP.Sock.SetTimeout(OptionConnectionTimeout);
  end;
  HTTP.Headers.NameValueSeparator := ':';

  if OptionProxyType = 'HTTP' then
  begin
    HTTP.ProxyHost := OptionProxyHost;
    HTTP.ProxyPort := OptionProxyPort;
    HTTP.ProxyUser := OptionProxyUser;
    HTTP.ProxyPass := OptionProxyPass;
  end
  else
  if (OptionProxyType = 'SOCKS4') or (OptionProxyType = 'SOCKS5') then
  begin
    if OptionProxyType = 'SOCKS4' then
      HTTP.Sock.SocksType := ST_Socks4
    else
    if OptionProxyType = 'SOCKS5' then
      HTTP.Sock.SocksType := ST_Socks5;
    HTTP.Sock.SocksIP := OptionProxyHost;
    HTTP.Sock.SocksPort := OptionProxyPort;
    HTTP.Sock.SocksUsername := OptionProxyUser;
    http.Sock.SocksPassword := OptionProxyPass;
  end
  else
  begin
    HTTP.Sock.SocksIP := OptionProxyHost;
    HTTP.Sock.SocksPort := OptionProxyPort;
    HTTP.Sock.SocksUsername := OptionProxyUser;
    http.Sock.SocksPassword := OptionProxyPass;
    HTTP.ProxyHost := OptionProxyHost;
    HTTP.ProxyPort := OptionProxyPort;
    HTTP.ProxyUser := OptionProxyUser;
    HTTP.ProxyPass := OptionProxyPass;
  end;

  HTTPHeader.Values['DNT'] := ' 1';
  HTTPHeader.Values['Accept'] := ' text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  HTTPHeader.Values['Accept-Charset'] := ' UTF-8';
  HTTPHeader.Values['Accept-Language'] := ' en-US,en;q=0.8';
  HTTP.Protocol := '1.1';
  HTTP.KeepAlive := False;
  if (HTTP.UserAgent = '') or (HTTP.UserAgent = UA_SYNAPSE) then
    HTTP.UserAgent := DEFAULT_UA;
  HTTP.MimeType := 'text/html';

  //User-Agent
  if Trim(HTTPHeader.Values['User-Agent']) <> '' then
  begin
    HTTP.UserAgent := Trim(HTTPHeader.Values['User-Agent']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('User-Agent'));
  end;
  //MimeType
  if Trim(HTTPHeader.Values['Content-Type']) <> '' then
  begin
    HTTP.MimeType := Trim(HTTPHeader.Values['Content-Type']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('Content-Type'));
  end;

  if Pos('.imgur.com/', LowerCase(URL)) = 0 then
    if ((mangaSiteID >= 0) and (mangaSiteID <= High(WebsiteRoots))) then
    begin
      if HTTPHeader.Values['Referer'] = '' then
        if not (SitesWithoutReferer(WebsiteRoots[mangaSiteID, 0])) then
          HTTPHeader.Values['Referer'] := ' ' + WebsiteRoots[mangaSiteID, 1];
    end;

  HTTP.Document.Clear;
  HTTP.RangeStart := 0;
  HTTP.RangeEnd := 0;

  if checkTerminate then Exit;
  HTTP.Headers.Text := HTTPHeader.Text;
  counter := 0;
  while (not HTTP.HTTPMethod('GET', URL)) or (HTTP.ResultCode > 500) do
  begin
    if checkTerminate then Exit;
    if (Reconnect > -1) and (Reconnect <= counter) then
    begin
      preTerminate;
      Exit;
    end;
    Inc(counter);
    HTTP.Clear;
    HTTP.Headers.Text := HTTPHeader.Text;
  end;

  while (HTTP.ResultCode > 300) and (HTTP.ResultCode < 400) do
  begin
    if checkTerminate then Exit;
    HTTPHeader.Values['Referer'] := ' ' + URL;
    s := Trim(HTTP.Headers.Values['Location']);
    if s <> '' then
    begin
      with TRegExpr.Create do
        try
          Expression := REGEX_HOST;
          if Replace(s, '$1', True) = '' then
          begin
            if s[1] <> '/' then
              s := '/' + s;
            URL := Replace(URL, '$1$2$3', True) + s;
          end
          else
            URL := s;
        finally
          Free;
        end;
    end;

    HTTP.Clear;
    HTTP.Headers.Text := HTTPHeader.Text;
    counter := 0;
    while (not HTTP.HTTPMethod('GET', URL)) or (HTTP.ResultCode > 500) do
    begin
      if checkTerminate then Exit;
      if (Reconnect > -1) and (Reconnect <= counter) then
      begin
        preTerminate;
        Exit;
      end;
      Inc(counter);
      HTTP.Clear;
      HTTP.Headers.Text := HTTPHeader.Text;
    end;
  end;
  if checkTerminate then Exit;
  SavedFilename := SaveImageStreamToFile(HTTP.Document, Path, Name);
  preTerminate;
  Result := SavedFilename <> '';
end;

function SaveImage(const AHTTP: THTTPSend; const mangaSiteID: Integer;
  URL: String; const Path, Name: String; const Reconnect: Integer): Boolean;
var
  f: String;
begin
  Result := SaveImage(AHTTP, mangaSiteID, URL, Path, Name, f, Reconnect);
end;

function SaveImage(const AHTTP: THTTPSend; URL: String; const Path, Name: String; const Reconnect: Integer
  ): Boolean;
var
  f: String;
begin
  Result := SaveImage(AHTTP, -1, URL, Path, Name, f, Reconnect);
end;

function SaveImage(const mangaSiteID: Integer; URL: String; const Path,
  Name: String; var SavedFilename: String; const Reconnect: Integer): Boolean;
begin
  Result := SaveImage(nil, mangaSiteID, URL, Path, Name, SavedFilename, Reconnect);
end;

procedure QuickSortChapters(var chapterList, linkList: TStringList);

  procedure QSort(L, R: Cardinal);
  var
    i, j: Cardinal;
    X: String;
  begin
    X := chapterList.Strings[(L + R) div 2];
    i := L;
    j := R;
    while i <= j do
    begin
      while StrComp(PChar(chapterList.Strings[i]), PChar(X)) < 0 do
        Inc(i);
      while StrComp(PChar(chapterList.Strings[j]), PChar(X)) > 0 do
        Dec(j);
      if i <= j then
      begin
        chapterList.Exchange(i, j);
        linkList.Exchange(i, j);
        Inc(i);
        if j > 0 then
          Dec(j);
      end;
    end;
    if L < j then
      QSort(L, j);
    if i < R then
      QSort(i, R);
  end;

begin
  if chapterList.Count <= 2 then
    Exit;
  QSort(0, chapterList.Count - 1);
end;

procedure QuickSortData(var merge: TStringList);
var
  names, output: TStringList;

  procedure QSort(L, R: Cardinal);
  var
    i, j: Cardinal;
    X: String;
  begin
    X := names.Strings[(L + R) div 2];
    i := L;
    j := R;
    while i <= j do
    begin
      while StrComp(PChar(names.Strings[i]), PChar(X)) < 0 do
        Inc(i);
      while StrComp(PChar(names.Strings[j]), PChar(X)) > 0 do
        Dec(j);
      if i <= j then
      begin
        names.Exchange(i, j);
        merge.Exchange(i, j);
        Inc(i);
        if j > 0 then
          Dec(j);
      end;
    end;
    if L < j then
      QSort(L, j);
    if i < R then
      QSort(i, R);
  end;

var
  i: Cardinal;

begin
  names := TStringList.Create;
  output := TStringList.Create;
  for i := 0 to merge.Count - 1 do
  begin
    output.Clear;
    GetParams(output, merge.Strings[i]);
    names.Add(output.Strings[DATA_PARAM_TITLE]);
  end;
  QSort(0, names.Count - 1);
  output.Free;
  names.Free;
end;

// this procedure is similar to QuickSortData except it sort the siteID as well
procedure QuickSortDataWithWebID(var merge: TStringList; const webIDList: TByteList);
var
  names, output: TStringList;

  procedure QSort(L, R: Cardinal);
  var
    i, j: Cardinal;
    X: String;
  begin
    X := names.Strings[(L + R) div 2];
    i := L;
    j := R;
    while i <= j do
    begin
      while StrComp(PChar(names.Strings[i]), PChar(X)) < 0 do
        Inc(i);
      while StrComp(PChar(names.Strings[j]), PChar(X)) > 0 do
        Dec(j);
      if i <= j then
      begin
        names.Exchange(i, j);
        merge.Exchange(i, j);
        webIDList.Exchange(i, j);
        Inc(i);
        if j > 0 then
          Dec(j);
      end;
    end;
    if L < j then
      QSort(L, j);
    if i < R then
      QSort(i, R);
  end;

var
  i: Cardinal;

begin
  names := TStringList.Create;
  output := TStringList.Create;
  for i := 0 to merge.Count - 1 do
  begin
    output.Clear;
    GetParams(output, merge.Strings[i]);
    names.Add(output.Strings[DATA_PARAM_TITLE]);
  end;
  QSort(0, names.Count - 1);
  output.Free;
  names.Free;
end;

function DateToJDN(const year, month, day: Word): Longint;
var
  a, y, m: Longint;
begin
  a := (14 - month) div 12;
  y := year + 4800 - a;
  m := month + (12 * a) - 3;
  Result := Round((day + ((153 * m + 2) div 5) + (365 * y) + (y div 4) - (y div 100) +
    (y div 400) - 32045) - 0.5);
end;

function DateToJDN(const date: TDate): Longint;
var
  day, month, year: Word;
begin
  DecodeDate(date, year, month, day);
  Result := DateToJDN(year, month, day);
end;

function JDNToDate(const JDN: Longint): TDate;
var
  a, b, c, d, e, m: Longint;
  day, month, year: Word;
begin
  a := trunc(JDN + 32044.5);
  b := (4 * a + 3) div 146097;
  c := a - (146097 * b div 4);
  d := (4 * c + 3) div 1461;
  e := c - (1461 * d div 4);
  m := (5 * e + 2) div 153;
  day := e - ((153 * m + 2) div 5) + 1;
  month := m + 3 - 12 * (m div 10);
  year := (100 * b) + d - 4800 + (m div 10);
  Result := EncodeDate(year, month, day);
end;

function GetCurrentJDN: Longint;
var
  day, month, year: Word;
begin
  DecodeDate(Now, year, month, day);
  Result := DateToJDN(year, month, day);
end;

procedure TransferMangaInfo(var dest: TMangaInfo; const Source: TMangaInfo);
begin
  dest.url := Source.url;
  dest.title := Source.title;
  dest.link := Source.link;
  dest.website := Source.website;
  dest.coverLink := Source.coverLink;
  dest.authors := Source.authors;
  dest.artists := Source.artists;
  dest.genres := Source.genres;
  dest.status := Source.status;
  dest.summary := Source.summary;
  dest.numChapter := Source.numChapter;
  dest.chapterName.Assign(Source.chapterName);
  dest.chapterLinks.Assign(Source.chapterLinks);
end;

{ THTMLForm }

constructor THTMLForm.Create;
begin
  fdata := TStringList.Create;
  fdata.NameValueSeparator := '=';
  fdata.Delimiter := '&';
  fvalueseparator := '=';
  fdelimiter := '&';
end;

destructor THTMLForm.Destroy;
begin
  fdata.Free;
  inherited Destroy;
end;

procedure THTMLForm.Put(const AName, AValue: String);
begin
  fdata.Values[AName] := AValue;
end;

procedure THTMLForm.Remove(const AName: String);
var
  i: Integer;
begin
  i := fdata.IndexOfName(AName);
  if i > -1 then fdata.Delete(i);
end;

function THTMLForm.GetData: String;
var
  i: Integer;
begin
  Result := '';
  if fdata.Count > 0 then
    for i := 0 to fdata.Count - 1 do begin
      if Result <> '' then Result := Result + fdelimiter;
      Result := Result + fdata.Names[i] + fvalueseparator + EncodeURLElement(fdata.ValueFromIndex[i]);
    end;
end;

{ THTTPSendThread }

constructor THTTPSendThread.Create(AOwner: TFMDThread);
begin
  inherited Create(AOwner);
  RetryCount := OptionConnectionMaxRetry;
  Timeout := OptionConnectionTimeout;
  SetProxy(OptionProxyType, OptionProxyHost, OptionProxyPort, OptionProxyUser, OptionProxyPass);
end;

{ TParseHTML }

procedure TParseHTML.FoundTag(NoCaseTag, ActualTag: String);
begin
  Output.Add(ActualTag);
end;

procedure TParseHTML.FoundText(Text: String);
begin
  Output.Add(Text);
end;

constructor TParseHTML.Create(const Raw: String);
begin
  inherited Create;
  if Raw <> '' then
    FRaw := Raw
  else
    FRaw := '';
end;

function TParseHTML.Exec(const Raw: String): String;
var
  parser: THTMLParser;
begin
  if not Assigned(Output) then Exit;
  if Raw <> '' then
    FRaw := Raw;
  if FRaw = '' then
    Exit('');
  Output.Clear;
  Output.BeginUpdate;
  parser := THTMLParser.Create(PChar(FRaw));
  try
    parser.OnFoundTag := FoundTag;
    parser.OnFoundText := FoundText;
    parser.Exec;
  finally
    parser.Free;
  end;
  Output.EndUpdate;
end;

{ TMangaInfo }

constructor TMangaInfo.Create;
begin
  inherited Create;
  chapterName := TStringList.Create;
  chapterLinks := TStringList.Create;
end;

destructor TMangaInfo.Destroy;
begin
  chapterName.Free;
  chapterLinks.Free;
  inherited Destroy;
end;

constructor TDownloadPageThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  isDone := False;
  FreeOnTerminate := True;
end;

procedure TDownloadPageThread.Execute;
begin
  isDone := True;
  SuspendThread(Self.Handle);
end;

// OS dependent
function fmdGetTempPath: String;
var
  l: Cardinal;
begin
{$IFDEF WINDOWS}
  SetLength(Result, 4096);
  l := GetTempPath(4096, PChar(Result));
  SetLength(Result, l + 1);
{$ENDIF}
{$IFDEF UNIX}
  Result := GetTempDir(False);
{$ENDIF}
end;

function fmdGetTickCount: Cardinal;
begin
 {$IFDEF WINDOWS}
  Result := GetTickCount64;
 {$ENDIF}
end;

procedure fmdPowerOff;
begin
{$IFDEF WINDOWS}
  if IsPwrShutdownAllowed then
  begin
    NTSetPrivilege(SE_SHUTDOWN_NAME, True);
    ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, 0);
  end;
{$ENDIF}
{$IFDEF UNIX}
  // This process require admin rights in order to execute
  with TProcessUTF8.Create(nil) do try
      CommandLine := 'poweroff';
      Execute;
    finally
      Free;
    end;
{$ENDIF}
end;

procedure fmdHibernate;
begin
  {$IFDEF WINDOWS}
  SetSuspendState(True, False, False);
  {$ENDIF}
end;

function HeaderByName(const AHeaders: TStrings; const AHeaderName: String): String;
var
  i, p: Integer;
  hn: String;
begin
  Result := '';
  if AHeaders.Count < 1 then
    Exit;
  hn := AHeaderName;
  //if hn[Length(hn)] <> ':' then
  //  hn := hn + ':';
  for i := 0 to AHeaders.Count - 1 do
  begin
    p := Pos(LowerCase(hn), LowerCase(AHeaders.Strings[i]));
    if p > 0 then
    begin
      p := Pos(':', AHeaders.Strings[i]);
      if p > 0 then
      begin
        Result := Copy(AHeaders.Strings[i], p + 2, Length(AHeaders.Strings[i]) - p + 1);
        Break;
      end;
    end;
  end;
end;

initialization
  FMD_VERSION_NUMBER := GetCurrentBinVersion;
  {$IFDEF WINDOWS}
  DEFAULT_PATH := GetCurrentDir + DirectorySeparator + 'downloads';
  {$ELSE}
  DEFAULT_PATH := '/downloads';
  {$ENDIF}

end.
