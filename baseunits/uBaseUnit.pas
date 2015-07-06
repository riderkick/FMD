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
  ShellApi, windows,
  {$else}
  UTF8Process,
  {$endif}
  SysUtils, Classes, Graphics, Forms, lazutf8classes, LazUTF8, strutils,
  fileinfo, fpjson, jsonparser, FastHTMLParser, fgl, FileUtil, RegExpr,
  synautil, httpsend, blcksock, ssl_openssl, GZIPUtils, uFMDThread, uMisc,
  USimpleException, USimpleLogger;

Type
  TFMDDo = (DO_NOTHING, DO_EXIT, DO_POWEROFF, DO_HIBERNATE, DO_UPDATE);

const
  FMD_REVISION = '$WCREV$';
  FMD_INSTANCE = '_FreeMangaDownloaderInstance_';

  FMD_TARGETOS = {$i %FPCTARGETOS%};
  FMD_TARGETCPU = {$i %FPCTARGETCPU%};

  JPG_HEADER: array[0..2] of Byte = ($FF, $D8, $FF);
  GIF_HEADER: array[0..2] of Byte = ($47, $49, $46);
  PNG_HEADER: array[0..2] of Byte = ($89, $50, $4E);

  UTF8BOM = #$EF#$BB#$BF;

  EXPARAM_PATH    = '%PATH%';
  EXPARAM_CHAPTER = '%CHAPTER%';
  DEFAULT_EXPARAM = '"' + EXPARAM_PATH + EXPARAM_CHAPTER + '"';

  DATA_PARAM_NAME       = 0;
  DATA_PARAM_LINK       = 1;
  DATA_PARAM_AUTHORS    = 2;
  DATA_PARAM_ARTISTS    = 3;
  DATA_PARAM_GENRES     = 4;
  DATA_PARAM_STATUS     = 5;
  DATA_PARAM_SUMMARY    = 6;
  DATA_PARAM_NUMCHAPTER = 7;
  DATA_PARAM_JDN        = 8;
  DATA_PARAM_READ       = 9;

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

  StringFilterChar: array [0..35] of array [0..1] of string = (
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

  HTMLEntitiesChar: array [0..82] of array [0..1] of string = (
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
  WORK_FOLDER             = 'works/';
  WORK_FILE               = 'works.ini';
  DOWNLOADEDCHAPTERS_FILE = 'downloadedchapters.ini';
  FAVORITES_FILE          = 'favorites.ini';
  IMAGE_FOLDER            = 'images/';
  DATA_FOLDER             = 'data/';
  DATA_EXT                = '.dat';
  DBDATA_EXT              = '.db';
  CONFIG_FOLDER           = 'config/';
  CONFIG_FILE             = 'config.ini';
  CONFIG_ADVANCED         = 'advanced.ini';
  REVISION_FILE           = 'revision.ini';
  UPDATE_FILE             = 'updates.ini';
  MANGALIST_FILE          = 'mangalist.ini';
  LANGUAGE_FILE           = 'languages.ini';
  LOG_FILE                = 'changelog.txt';

  UPDATE_URL = 'https://raw.githubusercontent.com/riderkick/FMD/master/';

  OPTION_MANGALIST = 0;
  OPTION_RECONNECT = 1;

  NO_ERROR              = 0;
  NET_PROBLEM           = 1;
  INFORMATION_NOT_FOUND = 2;

  SOCKHEARTBEATRATE = 300;

  DEFAULT_LIST = 'AnimeA,MangaFox,MangaHere,MangaInn,MangaReader';
  DEFAULT_CUSTOM_RENAME = '%NUMBERING% - %CHAPTER%';

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

  ANIMEA_ID              = 0;
  MANGAHERE_ID           = 1;
  MANGAINN_ID            = 2;
  OURMANGA_ID            = 3;
  KISSMANGA_ID           = 4;
  BATOTO_ID              = 5;
  MANGA24H_ID            = 6;
  VNSHARING_ID           = 7;
  HENTAI2READ_ID         = 8;
  FAKKU_ID               = 9;
  TRUYEN18_ID            = 10;
  MANGAREADER_ID         = 11;
  MANGAPARK_ID           = 12;
  EHENTAI_ID             = 13;
  MANGAFOX_ID            = 14;
  MANGATRADERS_ID        = 15;
  MANGASTREAM_ID         = 16;
  MANGAEDEN_ID           = 17;
  PERVEDEN_ID            = 18;
  TRUYENTRANHTUAN_ID     = 19;
  TURKCRAFT_ID           = 20;
  MANGAVADISI_ID         = 21;
  MANGAFRAME_ID          = 22;
  EATMANGA_ID            = 23;
  STARKANA_ID            = 24;
  MANGAPANDA_ID          = 25;
  REDHAWKSCANS_ID        = 26;
  BLOGTRUYEN_ID          = 27;
  KOMIKID_ID             = 28;
  SUBMANGA_ID            = 29;
  ESMANGAHERE_ID         = 30;
  ANIMEEXTREMIST_ID      = 31;
  PECINTAKOMIK_ID        = 32;
  HUGEMANGA_ID           = 33;
  S2SCAN_ID              = 34;
  SENMANGA_ID            = 35;
  IMANHUA_ID             = 36;
  MABUNS_ID              = 37;
  MANGAESTA_ID           = 38;
  CENTRALDEMANGAS_ID     = 39;
  EGSCANS_ID             = 40;
  MANGAAR_ID             = 41;
  MANGAAE_ID             = 42;
  ANIMESTORY_ID          = 43;
  LECTUREENLIGNE_ID      = 44;
  SCANMANGA_ID           = 45;
  MANGAGO_ID             = 46;
  DM5_ID                 = 47;
  PURURIN_ID             = 48;
  MANGACOW_ID            = 49;
  KIVMANGA_ID            = 50;
  MANGACAN_ID            = 51;
  MEINMANGA_ID           = 52;
  MANGASPROJECT_ID       = 53;
  MANGAREADER_POR_ID     = 54;
  MANGA2U_ID             = 55;
  MANGASTREAMTO_ID       = 56;
  NINEMANGA_ID           = 57;
  NINEMANGA_ES_ID        = 58;
  NINEMANGA_CN_ID        = 59;
  NINEMANGA_RU_ID        = 60;
  NINEMANGA_DE_ID        = 61;
  NINEMANGA_IT_ID        = 62;
  NINEMANGA_BR_ID        = 63;
  JAPANSHIN_ID           = 64;
  JAPSCAN_ID             = 65;
  CENTRUMMANGI_PL_ID     = 66;
  MANGALIB_PL_ID         = 67;
  ONEMANGA_ID            = 68;
  MANGATOWN_ID           = 69;
  READHENTAIMANGA_ID     = 70;
  MANGAOKU_ID            = 71;
  MYREADINGMANGAINFO_ID  = 72;
  IKOMIK_ID              = 73;
  NHENTAI_ID             = 74;
  UNIONMANGAS_ID         = 75;
  MANGAMINT_ID           = 76;
  UNIXMANGA_ID           = 77;
  HAKIHOME_ID            = 78;
  EXTREMEMANGAS_ID       = 79;
  MANGAHOST_ID           = 80;
  PORNCOMIX_ID           = 81;
  PORNCOMIXRE_ID         = 82;
  PORNCOMIXIC_ID         = 83;
  XXCOMICS_ID            = 84;
  XXCOMICSMT_ID          = 85;
  XXCOMICS3D_ID          = 86;
  PORNXXXCOMICS_ID       = 87;
  MANGASEE_ID            = 88;
  MANGAKU_ID             = 89;
  ACADEMYVN_ID           = 90;
  MANGAAT_ID             = 91;
  SENMANGARAW_ID         = 92;
  READMANGATODAY_ID      = 93;
  LONEMANGA_ID           = 94;
  DYNASTYSCANS_ID        = 95;
  MADOKAMI_ID            = 96;
  MANGACAP_ID            = 97;
  MANGABOOM_ID           = 98;
  AUTHRONE_ID            = 99;
  EYEONMANGA_ID          = 100;

  WebsiteRoots: array [0..100] of array [0..1] of string = (
    ('AnimeA', 'http://manga.animea.net'),
    ('MangaHere', 'http://www.mangahere.co'),
    ('MangaInn', 'http://www.mangainn.me'),
    ('OurManga', 'http://www.ourmanga.com'),
    ('KissManga', 'http://kissmanga.com'),
    ('Batoto', 'http://bato.to'),
    ('Manga24h', 'http://manga24h.com'),
    ('VnSharing', 'http://truyen.vnsharing.net'),
    ('Hentai2Read', 'http://hentai2read.com'),
    ('Fakku', 'https://www.fakku.net'),
    ('Truyen18', 'http://www.truyen18.org'),
    ('MangaReader', 'http://www.mangareader.net'),
    ('MangaPark', 'http://mangapark.me'),
    ('E-Hentai', 'http://g.e-hentai.org'),
    ('MangaFox', 'http://mangafox.me'),
    ('MangaTraders', 'http://mangatraders.org'),
    ('MangaStream', 'http://mangastream.com'),
    ('MangaEden', 'http://www.mangaeden.com'),
    ('PervEden', 'http://www.perveden.com'),
    ('TruyenTranhTuan', 'http://truyentranhtuan.com'),
    ('Turkcraft', 'http://turkcraft.com'),
    ('MangaVadisi', 'http://www.mangavadisi.net'),
    ('MangaFrame', 'http://www.mangaframe.com'),
    ('EatManga', 'http://eatmanga.com'),
    ('Starkana', 'http://starkana.jp'),
    ('MangaPanda', 'http://www.mangapanda.com'),
    ('RedHawkScans', 'http://manga.redhawkscans.com'),
    ('BlogTruyen', 'http://blogtruyen.com'),
    ('Komikid', 'http://www.komikid.com'),
    ('SubManga', 'http://submanga.com'),
    ('ESMangaHere', 'http://es.mangahere.co'),
    ('AnimExtremist', 'http://www.animextremist.com'),
    ('PecintaKomik', 'http://www.pecintakomik.com'),
    ('HugeManga', 'http://hugemanga.com'),
    ('S2Scans', 'http://reader.s2smanga.com'),
    ('SenManga', 'http://www.senmanga.com'),
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
    ('Pururin', 'http://pururin.com'),
    ('Mangacow', 'http://mangacow.co'),
    ('KivManga', 'http://www.kivmanga.com'),
    ('Mangacan', 'http://mangacanblog.com'),
    ('MeinManga', 'http://www.meinmanga.com/'),
    ('MangasPROJECT', 'http://mangaproject.xpg.uol.com.br'),
    ('MangaREADER_POR', 'http://www.mangareader.com.br'),
    ('Manga2u', 'http://www.mangakaka.com'),
    ('MangaStreamTo', 'http://www.mangastream.to'),
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
    ('ReadHentaiManga', 'http://readhentaimanga.com'),
    ('MangaOku', 'http://www.mangaoku.net'),
    ('MyReadingMangaInfo', 'http://myreadingmanga.info'),
    ('I-Komik', 'http://www.i-komik.com'),
    ('NHentai', 'http://nhentai.net'),
    ('UnionMangas', 'http://unionmangas.com.br'),
    ('MangaMint', 'http://www.mangamint.com'),
    ('UnixManga', 'http://unixmanga.co'),
    ('HakiHome', 'http://hakihome.com'),
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
    ('AcademyVN', 'http://truyen.academyvn.com'),
    ('MangaAt', 'http://www.mangaat.com'),
    ('SenMangaRAW', 'http://raw.senmanga.com'),
    ('ReadMangaToday', 'http://www.readmanga.today'),
    ('LoneManga', 'http://lonemanga.com'),
    ('Dynasty-Scans', 'http://dynasty-scans.com'),
    ('Madokami', 'https://manga.madokami.com'),
    ('MangaCap', 'http://www.mangacap.com'),
    ('MangaBoom', 'http://www.mangaboom.com'),
    ('Authrone', 'http://www.authrone.com'),
    ('EyeOnManga', 'http://www.eyeonmanga.com')
    );

  ALPHA_LIST = '#abcdefghijklmnopqrstuvwxyz';

  ANIMEA_BROWSER = '/browse.html?page=';
  ANIMEA_SKIP = '?skip=1';

  MANGAHERE_BROWSER = '/mangalist/';

  MANGAINN_BROWSER = '/mangalist/';

  OURMANGA_BROWSER = '/directory/';

  KISSMANGA_BROWSER = '/MangaList';

  BATOTO_BROWSER_1 = '/comic/_/sp/';
  BATOTO_BROWSER_2 = '/comic/_/comics/';

  MANGA24H_BROWSER = '/manga/update/page/';

  VNSHARING_BROWSER = '/DanhSach';

  HENTAI2READ_ROOT = 'http://hentai2read.com';
  HENTAI2READ_MROOT = 'http://m.hentai2read.com';
  HENTAI2READ_BROWSER = '/hentai-list/all/any/name-az/';

  FAKKU_BROWSER_1 = '/manga/newest';
  FAKKU_BROWSER_2 = '/doujinshi/newest';

  TRUYEN18_ROOT = 'http://www.truyen18.org';
  TRUYEN18_BROWSER = '/moi-dang/danhsach';

  MANGAREADER_BROWSER = '/alphabetical';

  //MANGAFOX_BROWSER :string = '/directory/';
  MANGAFOX_BROWSER = '/manga/';

  MANGATRADERS_BROWSER = '/directory/';

  MANGASTREAM_ROOT = 'http://mangastream.com';
  MANGASTREAM_ROOT2 = 'http://readms.com';

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

  MANGAPANDA_ROOT = 'http://www.mangapanda.com';
  MANGAPANDA_BROWSER = '/alphabetical';

  REDHAWKSCANS_BROWSER = '/reader/list/';

  BLOGTRUYEN_BROWSER = '/danhsach/tatca';
  BLOGTRUYEN_JS_BROWSER = '/ListStory/GetListStory/';
  BLOGTRUYEN_POST_FORM = 'Url=tatca&OrderBy=1&PageIndex=';

  KOMIKID_BROWSER = '/daftar.php';

  SUBMANGA_BROWSER = '/series/n';

  ESMANGAHERE_BROWSER = '/mangalist/';

  ANIMEEXTREMIST_BROWSER = '/mangas.htm?ord=todos';

  PECINTAKOMIK_BROWSER = '/directory/';

  HUGEMANGA_BROWSER = '/';

  SENMANGA_BROWSER = '/Manga/';

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

  PURURIN_BROWSER = '/browse/';

  //MANGACOW_BROWSER :string = '/manga-list/all/any/name-az/';
  MANGACOW_BROWSER = '/manga-list/all/any/last-added/';

  KIVMANGA_BROWSER = '/';

  MANGACAN_BROWSER = '/daftar-komik-manga-bahasa-indonesia.html';

  MEINMANGA_BROWSER = '/directory/all/';

  MANGASPROJECT_BROWSER = '/AJAX/listaMangas/all';

  MANGAREADER_POR_BROWSER = '/AJAX/listaMangas/all';

  //MANGA2U_BROWSER = '/list/all/any/most-popular/';
  MANGA2U_BROWSER = '/manga_list/all/any/last-added/';

  EHENTAI_BROWSER = 'f_doujinshi=on&f_manga=on&f_western=on&f_apply=Apply+Filter';
  EHENTAI_maxDLTask: Integer = 2;

  MANGASTREAMTO_BROWSER = '/series.html';

  NINEMANGA_BROWSER =
  '/search/?name_sel=contain&wd=&author_sel=contain&author=&artist_sel=contain&artist=&category_id=&out_category_id=&completed_series=either';

  JAPANSHIN_BROWSER = '/lectureenligne/reader/list/';
  JAPSCAN_BROWSER = '/mangas/';

  CENTRUMMANGI_PL_BROWSER = '/spis/';

  MANGALIB_PL_BROWSER = '/manga/directory';

  ONEMANGA_BROWSER = '/manga-list/all/any/last-added/';

  MANGATOWN_BROWSER = '/directory/';

  READHENTAIMANGA_BROWSER = '/hentai-manga-list/all/any/last-added/';

  IKOMIK_BROWSER = '/manga-directory/';

  UNIONMANGAS_BROWSER = '/mangas';

  UNIXMANGA_BROWSER = '/onlinereading/manga-lists.html';

  HAKIHOME_BROWSER = '/ListMangaHentai.html';

  EXTREMEMANGAS_BROWSER = '/2013/04/lista-de-mangas.html';

  MANGAHOST_BROWSER = '/mangas';

  DYNASTYSCANS_BROWSER: array [0..3] of string = (
    '/anthologies',
    '/doujins',
    '/issues',
    '/series'
    );

  MADOKAMI_BROWSER: array [0..11] of string = (
    '/Manga/%23%20-%20F',
    '/Manga/G%20-%20M',
    '/Manga/N%20-%20Z',
    '/Manga/_Autouploads/AutoUploaded%20from%20Assorted%20Sources',
    '/Manga/_Autouploads/ComicWalker',
    '/Manga/Non-English/Bahasa%20Indonesia',
    '/Manga/Non-English/Brazilian%20Portuguese',
    '/Manga/Non-English/Fran%C3%A7ais',
    '/Manga/Non-English/Italian',
    '/Manga/Non-English/Spanish',
    '/Manga/_Doujinshi',
    '/Raws'
    );

var
  FMD_VERSION_NUMBER: String = '';

  {$IFDEF WINDOWS}
  DEFAULT_PATH : string = '/downloads';
  {$ELSE}
  DEFAULT_PATH: string = '/downloads';
  {$ENDIF}

  // Sites var
  BROWSER_INVERT: Boolean = False;

  BATOTO_BROWSER: string = '/search';

  FAKKU_BROWSER: string = '/manga/newest';

  MANGAEDEN_BROWSER: string = '/en-directory/';

  PERVEDEN_BROWSER: string = '/en-directory/';

  MANGALIB_PL_COOKIES: String;
  //------------------------------------------

  Genre: array [0..37] of String;

  Revision: Cardinal;
  currentJDN: Cardinal;
  isChangeDirectory: Boolean = False;

  currentWebsite: String;

  ProxyType: String = '';
  Host: String = '';
  Port: String = '';
  User: String = '';
  Pass: String = '';

  fmdDirectory: String;

  OptionLetFMDDo: TFMDDo = DO_NOTHING;

  OptionCustomRename: String;

  OptionCheckMinutes: Cardinal = 0;
  OptionPDFQuality: Cardinal = 95;
  OptionMaxRetry: Cardinal = 0;
  OptionConnectionTimeout: Integer = 15000;
  OptionUpdateListNoMangaInfo: Boolean = False;
  OptionUpdateListRemoveDuplicateLocalData: Boolean = False;

  OptionShowBatotoSG: Boolean = True;
  OptionShowAllLang: Boolean = True;
  OptionAutoDlFav: Boolean = True;
  OptionEnableLoadCover: Boolean = False;
  OptionAutoNumberChapterChecked: Boolean = True;
  OptionAutoRemoveCompletedManga: Boolean = True;
  OptionAutoCheckFavStartup: Boolean = False;

type
  TArrayOfString = array of string;

  TCheckStyleType = (CS_DIRECTORY_COUNT, CS_DIRECTORY_PAGE,
                     CS_DIRECTORY_PAGE_2, CS_INFO);
  TFlagType       = (CS_GETPAGENUMBER, CS_GETPAGELINK, CS_DOWNLOAD);
  TDownloadStatusType     = (STATUS_STOP, STATUS_WAIT, STATUS_PREPARE,
                             STATUS_DOWNLOAD, STATUS_FINISH, STATUS_COMPRESS,
                             STATUS_PROBLEM, STATUS_FAILED);
  TDownloadStatusTypes = set of TDownloadStatusType;

  TFavoriteStatusType = (STATUS_IDLE, STATUS_CHECK, STATUS_CHECKING, STATUS_CHECKED);
  TFavoriteStatusTypes = set of TFavoriteStatusType;

  TMemory = Pointer;

  PMangaListItem = ^TMangaListItem;

  TMangaListItem = record
    Text: String;
  end;

  PSingleItem = ^TSingleItem;

  TSingleItem = record
    Text: String;
  end;

  PChapterStateItem = ^TChapterStateItem;

  TChapterStateItem = record
    Title,
    Link      : String;
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
    numbering,
    downloadedChapterList,
    currentChapter: String;
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
    FRaw: string;
    procedure FoundTag(NoCaseTag, ActualTag: string);
    procedure FoundText(Text: string);
  public
    Output: TStrings;
    constructor Create(const Raw: string = '');
    function Exec(const Raw: string = ''): string;
    property Raw: string read FRaw write FRaw;
  end;

  { THTTPSendThread }

  THTTPSendThread = class(THTTPSend)
    protected
      FOwner: TFMDThread;
      procedure CloseConnection(SendTerminateTag: Boolean = True);
      procedure SockOnHeartBeat(Sender: TObject);
    public
      constructor Create(AOwner: TFMDThread);
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

function GetMangaSiteID(const Name: String): Cardinal;
function GetMangaSiteName(const ID: Cardinal): String;
function GetMangaSiteRoot(const Website: String): String; overload;
function GetMangaSiteRoot(const MangaID: Cardinal): String; overload;
function GetMangaDatabaseURL(const Name: String): String;

function SitesMemberOf(const website: String; MangaSiteIDs: array of Cardinal): Boolean;
function SitesWithSortedList(const website:String): Boolean;
function SitesWithoutFavorites(const website:String): Boolean;
// Return true if the website doesn't contain manga information
function SitesWithoutInformation(const website: String): Boolean;
function SitesWithoutPageLink(const website: String): Boolean;
function SitesWithoutReferer(const website: String): Boolean;
function SitesRefererisURL(const website: String): Boolean;
function SitesWithSingleChapter(const website: String): Boolean;
function SitesIsWPManga(const websiteid: Cardinal): Boolean; overload;
function SitesIsWPManga(const website: String): Boolean; overload;

// Fill in website host if it's not present
function FillMangaSiteHost(const MangaID: Cardinal; URL: String): String;
function RemoveHostFromURL(URL: String): String;
procedure RemoveHostFromURLs(Const URLs: TStringList);
procedure RemoveHostFromURLsPair(Const URLs, Names : TStringList);

//JSON
procedure ParseJSONArray(const S, Path: String; var OutArray: TStringList);

//HTML
procedure ParseHTML(const aRaw: string; var aOutput: TStringList);

// StringUtils
function QuotedStrd(const S: string): string; overload;
function QuotedStrd(const S: Integer): string; overload;
function BracketStr(const S: string): string;
procedure ParseCommandLine(const cmd: string; var Output: TStrings;
  AStripQuotes: Boolean = False);
function ParsedCommandLine(const cmd: String): TArrayOfString;
function StringsToArray(const S: TStrings): TArrayOfString;
function StringsToCommandLine(const S: TStrings): string; overload;
function StringsToCommandLine(const S: array of string): string; overload;
procedure DeleteArrayOfString(Var TheStrings: TArrayOfString;Index: Integer);
function RandomString(SLength: Integer; ONumber: Boolean = False;
  OSymbol: Boolean = False; OSpace: Boolean = False): string;
function GetValuesFromString(Str: String; Sepr: Char): String;
procedure InvertStrings(Const St: TStringList); overload;
procedure InvertStrings(const Sts: array of TStringList); overload;
procedure TrimStrings(Const Str: TStringList);
procedure RemoveDuplicateStrings(Strs: Array of TStringList; RemIndex: Cardinal = 0);
procedure CleanHTMLComments(Const Str: TStringList);

function FixHTMLTagQuote(const s: String): String;
function FixCommonBrokenHTML(const s:String): string;
function URLDecode(const s: String): String;
function HTMLDecode(const AStr: String): String;

function RemoveSymbols(const input: String): String;
function CorrectPathSys(const Path: String): String;

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
procedure AddCommaString(var Dest: string; S: string);

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
function GetPage(const AHTTP: THTTPSend; var output: TObject; URL: String;
  const Reconnect: Cardinal): Boolean; overload;
function GetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
  overload; inline;
// Get url from a bitly url.
function GetURLFromBitly(const URL: String): String;
// Download an image from url and save it to a specific location.
function SaveImage(const AHTTP: THTTPSend; const mangaSiteID: Integer; URL: String;
  const Path, Name, prefix: String; const Reconnect: Cardinal): Boolean; overload;
function SaveImage(const mangaSiteID: Integer; URL: String;
  const Path, Name, prefix: String; const Reconnect: Cardinal): Boolean; overload; inline;

procedure QuickSortChapters(var chapterList, linkList: TStringList);
procedure QuickSortData(var merge: TStringList);
// This method uses to sort the data. Use when we load all the lists.
procedure QuickSortDataWithWebID(var merge: TStringList; const webIDList: TByteList);


function GetCurrentJDN: longint;
function DateToJDN(const year, month, day: word): longint; overload;
function DateToJDN(const date: TDate): longint; overload;

{function  ConvertInt32ToStr(const aValue: Cardinal)  : String;
function  ConvertStrToInt32(const aStr  : String): Cardinal;}
procedure TransferMangaInfo(var dest: TMangaInfo; const Source: TMangaInfo);

// cross platform funcs

function fmdGetTempPath: String;
function fmdGetTickCount: Cardinal;
procedure fmdPowerOff;
procedure fmdHibernate;
function RunExternalProcessAsAdmin(Exe, Params: String; ShowWind: Boolean = True;
  isPersistent: Boolean = True): Boolean;
function RunExternalProcess(Exe: String; Params: array of string; ShowWind: Boolean = True;
  isPersistent: Boolean = True): Boolean; overload;
function RunExternalProcess(Exe, Params: String; ShowWind: Boolean =  True;
  isPersistent: Boolean = True): Boolean; overload;
function RunExternalProcess(CommandLine: String; ShowWind: Boolean =  True;
  isPersistent: Boolean = True): Boolean; overload;

implementation

uses
  {$IFDEF DOWNLOADER}frmMain;{$ENDIF}

{$IFDEF WINDOWS}
// thanks Leledumbo for the code
const
  SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
  SE_TCB_NAME                 = 'SeTcbPrivilege';
  SE_SECURITY_NAME            = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME              = 'SeBackupPrivilege';
  SE_RESTORE_NAME             = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
  SE_DEBUG_NAME               = 'SeDebugPrivilege';
  SE_AUDIT_NAME               = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME              = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME          = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME   = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME       = 'SeManageVolumePrivilege';

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
    if (byte(Result[i]) < 31) or (byte(Result[i]) > 127) then
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
  i, j: word;
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
  SetCurrentDirUTF8(fmdDirectory);
end;

function GetMangaSiteID(const Name: String): Cardinal;
var
  i: Integer;
begin
  for i := Low(WebsiteRoots) to High(WebsiteRoots) do
    if Name = WebsiteRoots[i, 0] then
      Exit(i);
end;

function GetMangaSiteName(const ID: Cardinal): String;
begin
  Result := WebsiteRoots[ID, 0];
end;

function GetMangaSiteRoot(const Website : String) : String;
var
  i: Integer;
begin
  for i := Low(WebsiteRoots) to High(WebsiteRoots) do
    if Website = WebsiteRoots[i, 0] then
      Exit(WebsiteRoots[i, 1]);
end;

function GetMangaSiteRoot(const MangaID : Cardinal) : String;
begin
  Result := WebsiteRoots[MangaID, 1];
end;

// bad coding.. but this is how FMD works
function GetMangaDatabaseURL(const Name: String): String;
begin
  Result := 'https://bintray.com/artifact/download/riderkick/FMD/data/' + Name + '.7z';
end;

function SitesMemberOf(const website: String; MangaSiteIDs: array of Cardinal): Boolean;
var
  i: Cardinal;
begin
  Result := False;
  for i := Low(MangaSiteIDs) to High(MangaSiteIDs) do
    if website = WebsiteRoots[MangaSiteIDs[i], 0] then
    begin
      Result := True;
      Break;
    end;
end;

function SitesWithSortedList(const website : String) : Boolean;
begin
  Result := SitesIsWPManga(website);
  if not Result then
    Result := SitesMemberOf(website, [
      FAKKU_ID,
      PURURIN_ID,
      EHENTAI_ID,
      NINEMANGA_ID,
      NINEMANGA_ES_ID,
      NINEMANGA_CN_ID,
      NINEMANGA_RU_ID,
      NINEMANGA_DE_ID,
      NINEMANGA_IT_ID,
      NINEMANGA_BR_ID,
      MANGACOW_ID,
      ONEMANGA_ID,
      READHENTAIMANGA_ID,
      MYREADINGMANGAINFO_ID,
      NHENTAI_ID,
      MANGA2U_ID,
      PORNCOMIX_ID,
      XXCOMICS_ID,
      XXCOMICSMT_ID,
      XXCOMICS3D_ID,
      PORNCOMIXRE_ID,
      PORNCOMIXIC_ID,
      PORNXXXCOMICS_ID,
      MANGAPARK_ID,
      SENMANGA_ID,
      MANGACAP_ID
      ]);
end;

function SitesWithoutFavorites(const website : String) : Boolean;
begin
  Result := False;
  Result := SitesMemberOf(website, [
    EHENTAI_ID,
    FAKKU_ID,
    PURURIN_ID,
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
begin
  Result := False;
  Result := SitesMemberOf(website, [
    MANGASPROJECT_ID,
    MANGAVADISI_ID,
    S2SCAN_ID,
    EGSCANS_ID,
    TURKCRAFT_ID,
    HUGEMANGA_ID,
    KIVMANGA_ID,
    MANGACAN_ID,
    MANGASTREAMTO_ID,
    MANGAOKU_ID,
    UNIXMANGA_ID
    ]);
end;

function SitesWithoutPageLink(const website : String) : Boolean;
begin
  Result := False;
  Result := SitesMemberOf(website, [
    EHENTAI_ID
    ]);
end;

function SitesWithoutReferer(const website : String) : Boolean;
begin
  Result := False;
  Result:= SitesMemberOf(website, [
    MEINMANGA_ID,
    PECINTAKOMIK_ID,
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

function SitesRefererisURL(const website : String) : Boolean;
begin
  Result := False;
  Result := SitesMemberOf(website, [
    SENMANGA_ID
    ]);
end;

function SitesWithSingleChapter(const website : String) : Boolean;
begin
  Result := False;
  Result := SitesMemberOf(website, [
    FAKKU_ID,
    PURURIN_ID,
    EHENTAI_ID,
    MYREADINGMANGAINFO_ID,
    NHENTAI_ID,
    PORNCOMIX_ID,
    XXCOMICS_ID,
    XXCOMICSMT_ID,
    XXCOMICS3D_ID,
    PORNCOMIXRE_ID
    ]);
end;

function SitesIsWPManga(const websiteid: Cardinal): Boolean;
begin
  Result := websiteid in [
    MANGACAP_ID,
    MANGABOOM_ID,
    AUTHRONE_ID,
    EYEONMANGA_ID
    ];
end;

function SitesIsWPManga(const website: String): Boolean;
begin
  Result := SitesIsWPManga(GetMangaSiteID(website));
end;

function FillMangaSiteHost(const MangaID : Cardinal; URL : String) : String;
var
  regx: TRegExpr;
begin
  Result := URL;
  if Length(URL) < 3 then
    Exit;
  if MangaID <= High(WebsiteRoots) then
    regx := TRegExpr.Create;
    try
      regx.ModifierI := True;
      regx.Expression := '^([a-z]+\:)?(//)?[^/]*\w+\.\w+(\:\d+)?/?';
      if Pos('//', Result) = 1 then
        Delete(Result, 1, 2);
      if not regx.Exec(URL) then
        Result := TrimRightChar(WebsiteRoots[MangaID, 1], ['/']) +
          '/' + TrimLeftChar(URL, ['/']);
      regx.Expression := '([^:])[/]{2,}(\b|\Z)';
      Result := regx.Replace(Result, '$1/', True);
    finally
      regx.Free
    end;
end;

function RemoveHostFromURL(URL : String) : String;
var
  regx: TRegExpr;
begin
  Result := URL;
  regx := TRegExpr.Create;
  try
    regx.ModifierI := True;
    regx.Expression := '^([a-z]+\:)?(//)?[^/]*\w+\.\w+(\:\d+)?/?';
    if regx.Exec(URL) then
      Result := regx.Replace(URL, '/', False);
  finally
    regx.Free
  end;
end;

procedure RemoveHostFromURLs(const URLs : TStringList);
var
 i: Integer;
 regx: TRegExpr;
begin
  if URLs.Count > 0 then
  begin
    regx := TRegExpr.Create;
    try
      regx.ModifierI := True;
      regx.Expression := '^([a-z]+\:)?(//)?[^/]*\w+\.\w+(\:\d+)?/?';
      for i := 0 to URLs.Count - 1 do
      begin
        if regx.Exec(URLs[i]) then
          URLs[i] := regx.Replace(URLs[i], '/', False);
      end;
    finally
      regx.Free
    end;
  end;
end;

procedure RemoveHostFromURLsPair(const URLs, Names : TStringList);
var
 i: Integer;
 regx: TRegExpr;
begin
  if URLs.Count > 0 then
  begin
    regx := TRegExpr.Create;
    try
      regx.ModifierI := True;
      regx.Expression := '^([a-z]+\:)?(//)?[^/]*\w+\.\w+(\:\d+)?/?';
      i := 0;
      while i < URLs.Count do
      begin
        if regx.Exec(URLs[i]) then
          URLs[i] := regx.Replace(URLs[i], '/', False);
        if URLs[i] = '/' then
        begin
          URLs.Delete(i);
          Names.Delete(i);
        end
        else
          Inc(i);
      end;
    finally
      regx.Free
    end;
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
  P := TJSONParser.Create(Trim(S));
  try
    D := P.Parse;
    try
      If Assigned(D) then
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

procedure ParseHTML(const aRaw: string; var aOutput: TStringList);
begin
  if not Assigned(aOutput) then Exit;
  with TParseHTML.Create(aRaw) do try
    Output := aOutput;
    Exec;
  finally
    Free;
  end;
end;

function QuotedStrd(const S: string): string;
begin
  Result := AnsiQuotedStr(S, '"');
end;

function QuotedStrd(const S: Integer): string;
begin
  Result := QuotedStrd(IntToStr(S));
end;

function BracketStr(const S: string): string;
begin
  Result := '(' + S + ')';
end;

procedure ParseCommandLine(const cmd: string; var Output: TStrings;
  AStripQuotes: Boolean = False);
var
  s, cl: string;
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

function ParsedCommandLine(const cmd: String): TArrayOfString;
var
  ts: TStrings;
begin
  if cmd = '' then Exit;
  ts := TStringList.Create;
  try
    ParseCommandLine(cmd, ts, True);
    Result := StringsToArray(ts);
  finally
    ts.Free;
  end;
end;

function StringsToArray(const S: TStrings): TArrayOfString;
var
  i: Integer;
begin
  if not Assigned(S) then Exit;
  if S.Count = 0 then Exit;
  SetLength(Result, S.Count);
  for i := 0 to S.Count - 1 do
    Result[i] := S[i];
end;

function StringsToCommandLine(const S: TStrings): string;
var
  i: Integer;
begin
  Result := '';
  if S.Count>0 then
  begin
    for i := 0 to S.Count-1 do
    begin
      if Pos(' ', S[i]) <> 0 then
        Result := Result + '"' + S[i] + '" '
      else
        Result := Result + S[i] + ' ';
    end;
    Result := Trim(Result);
  end;
end;

function StringsToCommandLine(const S: array of string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(S)>0 then
  begin
    for i := Low(S) to High(S) do
    begin
      if (Pos(' ', S[i]) <> 0) and
        ((LeftStr(S[i], 1) <> '"') and (RightStr(S[i], 1) <> '"')) then
        Result := Result + '"' + S[i] + '" '
      else
        Result := Result + S[i] + ' ';
    end;
    Result := Trim(Result);
  end;
end;

procedure DeleteArrayOfString(var TheStrings: TArrayOfString;Index: Integer);
 var
   i: Integer;
 begin
   if Length(TheStrings) > 0 then
   begin
     for i := Low(TheStrings) to High(TheStrings) -1 do
       TheStrings[i] := TheStrings[i + 1];
     SetLength(TheStrings, Length(TheStrings) - 1);
   end;
 end;

function RandomString(SLength: Integer; ONumber: Boolean; OSymbol: Boolean;
  OSpace: Boolean): string;
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
  until (Length(Result) = SLength)
end;

function GetValuesFromString(Str: String; Sepr: Char): String;
var
  i: Integer;
begin
  Result := '';
  if Str = '' then
    Exit;
  if Pos(Sepr, Str) > 0 then
  begin
    for i := 1 to Length(Str) do
      if (Str[i] = Sepr) and (i < Length(Str)) then
      begin
        Result := Copy(Str, i + 1, Length(Str) - i);
        Break;
      end;
    if Result <> '' then
    begin
      while (Result <> '') and (Result[Length(Result)] in [' ', '"', '''', ';']) do
        Delete(Result, Length(Result), 1);
      while (Result <> '') and (Result[1] in [' ', '"', '''']) do
        Delete(Result, 1, 1);
    end;
  end;
end;

procedure InvertStrings(const St : TStringList);
var
  i: Integer;
begin
  if St.Count > 1 then
    for i := 0 to ((St.Count - 1) div 2) do
      St.Exchange(i, St.Count - 1 - i);
end;

function FixHTMLTagQuote(const s : String) : String;
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

function FixCommonBrokenHTML(const s: String): string;
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
   ESC: string[2];
   CharCode: integer;
   c: char;
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
                 'g': if (AnsiStrPos(Sp, 'lt;') = Sp) or (AnsiStrPos(Sp, 'gt;') = Sp) then { do not localize }
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
                        Inc(Sp,4);
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

procedure TrimStrings(const Str : TStringList);
var
  i: Integer;
begin
  if Str.Count > 0 then
  begin
    i := 0;
    while i < Str.Count do
    begin
      if Trim(Str[i]) = '' then
        Str.Delete(i)
      else
      begin
        Str[i] := Trim(Str[i]);
        Inc(i);
      end;
    end;
  end;
end;

procedure RemoveDuplicateStrings(Strs : array of TStringList;
  RemIndex : Cardinal);
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

procedure CleanHTMLComments(const Str : TStringList);
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

function CustomRename(const AString, AWebsite, AMangaName, AAuthor, AArtist,
  AChapter, ANumbering : String; const AIsUnicodeRemove : Boolean) : String;
var
  chap: String;
begin
  Result := '';
  chap := Trim(AChapter);
  //Convert Digit in chapter name
  if MainForm.options.ReadBool('saveto', 'ConvertDigitVolume', True) then
    if MainForm.options.ReadBool('saveto', 'ConvertDigitChapter', True) then
      padZero(chap, MainForm.options.ReadInteger('saveto', 'DigitVolumeLength', 2),
        MainForm.options.ReadInteger('saveto', 'DigitChapterLength', 3))
    else
      padZero(chap, MainForm.options.ReadInteger('saveto', 'DigitVolumeLength', 2), 0)
  else if MainForm.options.ReadBool('saveto', 'ConvertDigitChapter', True) then
    padZero(chap, 0, MainForm.options.ReadInteger('saveto', 'DigitChapterLength', 3));

  if (Pos('%NUMBERING%', AString) = 0) and (Pos('%CHAPTER%', AString) = 0) then
    Result := ANumbering + AString
  else
    Result := AString;

  Result := TrimLeft(TrimRight(Result));
  Result := StringReplace(Result, '%WEBSITE%', AWebsite, [rfReplaceAll]);
  Result := StringReplace(Result, '%MANGA%', AMangaName, [rfReplaceAll]);
  Result := StringReplace(Result, '%AUTHOR%', AAuthor, [rfReplaceAll]);
  Result := StringReplace(Result, '%ARTIST%', AArtist, [rfReplaceAll]);
  Result := StringReplace(Result, '%CHAPTER%', chap, [rfReplaceAll]);
  if (AWebsite = WebsiteRoots[FAKKU_ID, 0]) or
    (AWebsite = WebsiteRoots[MANGASTREAM_ID, 0]) then
  begin
    if Pos('%NUMBERING% - ', Result) > 0 then
      Result := StringReplace(Result, '%NUMBERING% - ', '', [rfReplaceAll])
    else
      Result := StringReplace(Result, '%NUMBERING%', '', [rfReplaceAll])
  end
  else
    Result := StringReplace(Result, '%NUMBERING%', ANumbering, [rfReplaceAll]);
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '', [rfReplaceAll]);
  if Result = '' then
  begin
    if (AWebsite = WebsiteRoots[FAKKU_ID, 0]) or
      (AWebsite = WebsiteRoots[MANGASTREAM_ID, 0]) then
      Result := chap
    else
      Result := ANumbering;
  end;
  if AIsUnicodeRemove then
    Result := UnicodeRemove(Result);

  Result := Trim(Result);
  Result := RemoveSymbols(HTMLEntitiesFilter(StringFilter(Result)));
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
      AStrings.Sort;
    Result := AStrings.Find(s, p);
  end
  else
    Result := False;
end;

procedure GetParams(const output: TStrings; input: String);
var
  l: word;
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
  l: word;
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
  l: word;
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

function ExtractParam(const output : TStrings; input, sep : String;
  WhiteSp : Boolean) : Integer;
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
    Result := Result + IntToStr(integer(list.Items[i])) + SEPERATOR;
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

function FixURL(const URL : String) : String;
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
    if byte(path[i]) >= 128 then
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
  i: word;
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

function CommonStringFilter(const Source : String) : String;
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

function RemoveDoubleSpace(const Source : String) : String;
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
  i, j: LongInt;
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
  i, j: LongInt;
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

procedure AddCommaString(var Dest: string; S: string);
begin
  if Trim(S) = '' then Exit;
  if Trim(S) = ',' then Exit;
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

function GetPage(const AHTTP: THTTPSend; var output: TObject; URL: String;
  const Reconnect: Cardinal): Boolean;
  // If AHTTP <> nil, we will use it as http sender. Otherwise we create a new
  // instance.
var
  //i: Cardinal;
  HTTP: THTTPSend;
  HTTPHeader: TStringList;
  counter: Cardinal = 0;
  s: String;
  meth: String = 'GET';
  //zstream: TGZFileStream;
  isGZip: Boolean = True;
  mstream: TMemoryStream;

  procedure HTTPClear;
  begin
    if Assigned(HTTP) then
      with HTTP do
      begin
        RangeStart := 0;
        RangeEnd := 0;
        Headers.Clear;
        MimeType := 'text/html';
      end;
  end;

  procedure preTerminate;
  begin
    HTTPHeader.Free;
    if AHTTP = nil then
      HTTP.Free;
  end;

  function checkTerminate: boolean;
  begin
    Result := HTTP.Sock.Tag =  1; //terminated via OnHeartBeat
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
    HTTP := THTTPSend.Create;
  HTTP.Headers.NameValueSeparator := ':';

  globReturn:

  if ProxyType = 'HTTP' then
  begin
    HTTP.ProxyHost := Host;
    HTTP.ProxyPort := Port;
    HTTP.ProxyUser := User;
    HTTP.ProxyPass := Pass;
  end
  else
  if (ProxyType = 'SOCKS4') or (ProxyType = 'SOCKS5') then
  begin
    if ProxyType = 'SOCKS4' then
      HTTP.Sock.SocksType := ST_Socks4
    else
    if ProxyType = 'SOCKS5' then
      HTTP.Sock.SocksType := ST_Socks5;
    HTTP.Sock.SocksIP := Host;
    HTTP.Sock.SocksPort := Port;
    HTTP.Sock.SocksUsername := User;
    http.Sock.SocksPassword := Pass;
  end
  else
  begin
    HTTP.Sock.SocksIP := Host;
    HTTP.Sock.SocksPort := Port;
    HTTP.Sock.SocksUsername := User;
    http.Sock.SocksPassword := Pass;
    HTTP.ProxyHost := Host;
    HTTP.ProxyPort := Port;
    HTTP.ProxyUser := User;
    HTTP.ProxyPass := Pass;
  end;

  HTTPHeader.Values['DNT'] := ' 1';
  HTTP.Protocol := '1.1';
  HTTP.KeepAlive := False;
  HTTP.Timeout := OptionConnectionTimeout;
  HTTP.Sock.SetTimeout(OptionConnectionTimeout);

  //User-Agent
  if Trim(HTTPHeader.Values['User-Agent']) <> '' then
  begin
    HTTP.UserAgent := Trim(HTTPHeader.Values['User-Agent']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('User-Agent'));
  end
  else
  if Trim(HTTP.UserAgent) = '' then
    HTTP.UserAgent := UA_FIREFOX;
  //MimeType
  if Trim(HTTPHeader.Values['Content-Type']) <> '' then
  begin
    HTTP.MimeType := Trim(HTTPHeader.Values['Content-Type']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('Content-Type'));
  end
  else
  if Trim(HTTP.MimeType) = '' then
    HTTP.MimeType := '';

  if isGZip then
  begin
    //HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTPHeader.Values['Accept-Encoding'] := ' gzip, deflate';
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

  if HTTP.Sock.Tag = 100 then //POST
  begin
    meth := 'POST';
    HTTP.MimeType := 'application/x-www-form-urlencoded';
  end;

  counter := 0;
  HTTP.Headers.Text := HTTPHeader.Text;
  while (not HTTP.HTTPMethod(meth, URL)) or
    (HTTP.ResultCode >= 500) or
    (HTTP.ResultCode = 451) do
  begin
    if checkTerminate then Exit;
    if (Reconnect <> 0) and (Reconnect <= counter) then
    begin
      preTerminate;
      Exit;
    end;
    Inc(Counter);
    HTTPClear;
    HTTP.Headers.Text := HTTPHeader.Text;
  end;

  counter := 0;
  while (HTTP.ResultCode = 302) or (HTTP.ResultCode = 301) do
  begin
    if checkTerminate then Exit;
    HTTPHeader.Values['Referer'] := ' ' + URL;
    s := Trim(HTTP.Headers.Values['Location']);
    s := TrimLeftChar(s, ['/', ':']);
    if s <> '' then
    begin
      if LowerCase(Copy(s, 1, 4)) <> 'http' then
        s := 'http://' + s;
      URL := s;
    end;

    if Pos(HENTAI2READ_ROOT, URL) <> 0 then
      HTTP.Headers.Insert(0, 'Referer:' + HENTAI2READ_ROOT + '/');

    counter := 0;
    HTTP.Clear;
    HTTP.Headers.Text := HTTPHeader.Text;
    while (not HTTP.HTTPMethod('GET', URL)) or
      (HTTP.ResultCode > 500) do  //500 for abort
    begin
      if checkTerminate then Exit;
      if (Reconnect <> 0) and (Reconnect <= counter) then
      begin
        preTerminate;
        Exit;
      end;
      Inc(Counter);
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
      begin
        E.Message := 'GetPage.WriteOutput error: '#13#10 + E.Message;
        USimpleException.ExceptionHandleSaveLogOnly(nil, E);
      end;
    end;
    Result := True;
  end
  else
    Result := False;

  preTerminate;
end;

function GetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
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

function SaveImage(const AHTTP: THTTPSend; const mangaSiteID: Integer;
  URL: String; const Path, Name, prefix: String; const Reconnect: Cardinal
  ): Boolean;
  // prefix: For example: 000<our prefix>.jpg.
var
  retryToSave: Boolean = False;
  header: array [0..3] of Byte;
  ext, lpath, fpath: String;
  HTTPHeader: TStringList;
  HTTP: THTTPSend;
  counter: Cardinal;
  s: String;
  //source    : TPicture;
  fstream: TFileStreamUTF8;

  procedure preTerminate;
  begin
    HTTPHeader.Free;
    if AHTTP = nil then
      HTTP.Free;
  end;

  function checkTerminate: boolean;
  begin
    Result := HTTP.Sock.Tag = 1; //terminated via OnHeartBeat
    if Result then
    begin
      HTTP.Sock.Tag := 0;
      preTerminate;
    end;
  end;

begin
  Result := False;
  if Trim(URL) = '' then Exit;
  fpath := '';
  s := Path + '/' + Name;
  // Check to see if a file with similar name was already exist. If so then we
  // skip the download process.
  if (FileExistsUTF8(s + '.jpg')) or
    (FileExistsUTF8(s + '.png')) or
    (FileExistsUTF8(s + '.gif')) or
    (Trim(URL) = 'D') then
    Exit(True);

  URL := FixURL(URL);

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
    HTTP := THTTPSend.Create;
  HTTP.Headers.NameValueSeparator := ':';

  if ProxyType = 'HTTP' then
  begin
    HTTP.ProxyHost := Host;
    HTTP.ProxyPort := Port;
    HTTP.ProxyUser := User;
    HTTP.ProxyPass := Pass;
  end
  else
  if (ProxyType = 'SOCKS4') or (ProxyType = 'SOCKS5') then
  begin
    if ProxyType = 'SOCKS4' then
      HTTP.Sock.SocksType := ST_Socks4
    else
    if ProxyType = 'SOCKS5' then
      HTTP.Sock.SocksType := ST_Socks5;
    HTTP.Sock.SocksIP := Host;
    HTTP.Sock.SocksPort := Port;
    HTTP.Sock.SocksUsername := User;
    http.Sock.SocksPassword := Pass;
  end
  else
  begin
    HTTP.Sock.SocksIP := Host;
    HTTP.Sock.SocksPort := Port;
    HTTP.Sock.SocksUsername := User;
    http.Sock.SocksPassword := Pass;
    HTTP.ProxyHost := Host;
    HTTP.ProxyPort := Port;
    HTTP.ProxyUser := User;
    HTTP.ProxyPass := Pass;
  end;

  HTTPHeader.Values['DNT'] := ' 1';
  HTTP.Protocol := '1.1';
  HTTP.KeepAlive := False;
  HTTP.Timeout := OptionConnectionTimeout;
  HTTP.Sock.SetTimeout(OptionConnectionTimeout);

  //User-Agent
  if Trim(HTTPHeader.Values['User-Agent']) <> '' then
  begin
    HTTP.UserAgent := Trim(HTTPHeader.Values['User-Agent']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('User-Agent'));
  end
  else
  if Trim(HTTP.UserAgent) = '' then
    HTTP.UserAgent := UA_FIREFOX;
  //MimeType
  if Trim(HTTPHeader.Values['Content-Type']) <> '' then
  begin
    HTTP.MimeType := Trim(HTTPHeader.Values['Content-Type']);
    HTTPHeader.Delete(HTTPHeader.IndexOfName('Content-Type'));
  end
  else
  if Trim(HTTP.MimeType) = '' then
    HTTP.MimeType := '';

  if Pos('.imgur.com/', LowerCase(URL)) = 0 then
    if ((mangaSiteID >= 0) and (mangaSiteID <= High(WebsiteRoots))) then
    begin
      if HTTPHeader.Values['Referer'] = '' then
        if not (SitesWithoutReferer(WebsiteRoots[mangaSiteID, 0])) then
        begin
          if SitesRefererisURL(WebsiteRoots[mangaSiteID, 0]) then
            HTTPHeader.Values['Referer'] := ' ' + URL
          else
            HTTPHeader.Values['Referer'] := ' ' + WebsiteRoots[mangaSiteID, 1];
        end;
    end;

  {$IFDEF DOWNLOADER}
  if checkTerminate then Exit;
  {$ENDIF}
  counter := 0;
  HTTP.Headers.Text := HTTPHeader.Text;
  while (not HTTP.HTTPMethod('GET', URL)) or
    (HTTP.ResultCode >= 500) or   //500 for abort
    (HTTP.ResultCode = 403) do
  begin
    {$IFDEF DOWNLOADER}
    if checkTerminate then Exit;
    {$ENDIF}
    if (Reconnect <> 0) and (Reconnect <= counter) then
    begin
      preTerminate;
      Exit;
    end;
    Inc(Counter);
    HTTP.Clear;
    HTTP.Headers.Text := HTTPHeader.Text;
  end;

  counter := 0;
  while (HTTP.ResultCode = 302) or (HTTP.ResultCode = 301) do
  begin
    {$IFDEF DOWNLOADER}
    if checkTerminate then Exit;
    {$ENDIF}

    HTTPHeader.Values['Referer'] := ' ' + URL;
    s := Trim(HTTP.Headers.Values['Location']);
    s := TrimLeftChar(s, ['/', ':']);
    if s <> '' then
    begin
      if LowerCase(Copy(s, 1, 4)) <> 'http' then
        s := 'http://' + s;
      URL := s;
    end;

    HTTP.Clear;
    counter := 0;
    HTTP.Headers.Text := HTTPHeader.Text;
    while (not HTTP.HTTPMethod('GET', URL)) or
      (HTTP.ResultCode > 500) or  //500 for abort
      (HTTP.ResultCode = 403) do
    begin
      {$IFDEF DOWNLOADER}
      if checkTerminate then Exit;
      {$ENDIF}
      if (Reconnect <> 0) and (Reconnect <= counter) then
      begin
        preTerminate;
        Exit;
      end;
      Inc(Counter);
      HTTP.Clear;
      HTTP.Headers.Text := HTTPHeader.Text;
      Sleep(500);
    end;
  end;
  HTTP.Document.Seek(0, soBeginning);
  HTTP.Document.Read(header[0], 4);
  if (header[0] = JPG_HEADER[0]) and
    (header[1] = JPG_HEADER[1]) and
    (header[2] = JPG_HEADER[2]) then
    ext := '.jpg'
  else
  if (header[0] = PNG_HEADER[0]) and
    (header[1] = PNG_HEADER[1]) and
    (header[2] = PNG_HEADER[2]) then
    ext := '.png'
  else
  if (header[0] = GIF_HEADER[0]) and
    (header[1] = GIF_HEADER[1]) and
    (header[2] = GIF_HEADER[2]) then
    ext := '.gif'
  else
    ext := '';
  if ext <> '' then
  begin
    // If an error occured, verify the path and redo the job.
    // If the error still persists, break the loop.
    repeat
      try
        {$IFDEF DOWNLOADER}
        if checkTerminate then Exit;
        {$ENDIF}
        lpath := CleanAndExpandDirectory(CorrectPathSys(Path));
        if not DirectoryExistsUTF8(lpath) then
          ForceDirectoriesUTF8(lpath);
        if DirectoryExistsUTF8(lpath) then
        begin
          fpath := CleanAndExpandFilename(lpath + Name + prefix + ext);
          if FileExistsUTF8(fpath) then
            DeleteFile(fpath);
          fstream := TFileStreamUTF8.Create(fpath, fmCreate);
          try
            HTTP.Document.SaveToStream(fstream);
          finally
            fstream.Free;
          end;
          Result := FileExistsUTF8(fpath);
        end
        else
          Result := False;
        Break;
      except
        on E: Exception do
        begin
          E.Message := 'SaveImage.SavetoFile.Error'#13#10 + E.Message + #13#10 +
            (CorrectPathSys(Path) + '/' + Name + prefix + ext);
          USimpleException.ExceptionHandleSaveLogOnly(nil, E);
          {$IFDEF DOWNLOADER}
          if checkTerminate then Exit;
          {$ENDIF}
          if not retryToSave then
          begin
            CheckPath(Path);
            retryToSave := True;
          end
          else
            Break;
        end;
      end;
    until False;
  end
  else
  begin
    s := 'SaveImage.ExtEmpty'#13#10'URL: ' + URL;
    USimpleException.ExceptionHandleSaveLogOnly(nil, Exception.Create(s));
  end;
  preTerminate;
  Result := (fpath <> '') and FileExistsUTF8(fpath);
end;

function SaveImage(const mangaSiteID: Integer; URL: String;
  const Path, Name, prefix: String; const Reconnect: Cardinal): Boolean;
begin
  Result := SaveImage(nil, mangaSiteID, URL, Path, Name, prefix, Reconnect);
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
    names.Add(output.Strings[DATA_PARAM_NAME]);
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
    names.Add(output.Strings[DATA_PARAM_NAME]);
  end;
  QSort(0, names.Count - 1);
  output.Free;
  names.Free;
end;

function DateToJDN(const year, month, day: word): longint;
var
  a, y, m: longint;
begin
  a := (14 - month) div 12;
  y := year + 4800 - a;
  m := month + (12 * a) - 3;
  Result := Round((day + ((153 * m + 2) div 5) + (365 * y) + (y div 4) - (y div 100) +
    (y div 400) - 32045) - 0.5);
end;

function DateToJDN(const date: TDate): longint;
var
  day, month, year: word;
begin
  DecodeDate(date, year, month, day);
  Result := DateToJDN(year, month, day);
end;

function GetCurrentJDN: longint;
var
  day, month, year: word;
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

{ THTTPSendThread }

procedure THTTPSendThread.CloseConnection(SendTerminateTag: Boolean);
begin
  with Self.Sock do
  begin
    if SendTerminateTag then
      Tag := 1;
    StopFlag := True;
    AbortSocket;
  end;
end;

procedure THTTPSendThread.SockOnHeartBeat(Sender: TObject);
begin
  if Assigned(FOwner) then
    if FOwner.IsTerminated then
       CloseConnection;
end;

constructor THTTPSendThread.Create(AOwner: TFMDThread);
begin
  inherited Create;
  if Assigned(AOwner) then
  begin
    FOwner := TFMDThread(AOwner);
    Sock.OnHeartbeat := SockOnHeartBeat;
    Sock.HeartbeatRate := SOCKHEARTBEATRATE;
  end;
end;

{ TParseHTML }

procedure TParseHTML.FoundTag(NoCaseTag, ActualTag: string);
begin
  Output.Add(ActualTag);
end;

procedure TParseHTML.FoundText(Text: string);
begin
  Output.Add(Text);
end;

constructor TParseHTML.Create(const Raw: string);
begin
  inherited Create;
  if Raw <> '' then
    FRaw := Raw
  else
    FRaw := '';
end;

function TParseHTML.Exec(const Raw: string): string;
var
  parser: THTMLParser;
begin
  if not Assigned(Output) then Exit;
  if Raw <> '' then
    FRaw := Raw;
  if FRaw = '' then
    Exit('');
  Output.Clear;
  parser := THTMLParser.Create(PChar(FRaw));
  try
    parser.OnFoundTag := FoundTag;
    parser.OnFoundText := FoundText;
    parser.Exec;
  finally
    parser.Free;
  end;
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

function RunExternalProcessAsAdmin(Exe, Params: String; ShowWind: Boolean;
  isPersistent: Boolean): Boolean;
var
 {$IFDEF WINDOWS}
  SEInfo: TSHELLEXECUTEINFOW;
 {$ELSE}
  Process: TProcessUTF8;
  pr: TStringList;
 {$ENDIF}
begin
  {$IFDEF WINDOWS}
  Initialize(SEInfo);
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(SEInfo);
  with SEInfo do begin
    wnd := 0;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    if isPersistent then
      fMask := fMask or SEE_MASK_NOCLOSEPROCESS;
    lpVerb := 'runas';
    lpFile := PWideChar(UTF8Decode(Exe));
    lpParameters := PWideChar(UTF8Decode(Params));
    if ShowWind then
      nShow := SW_SHOWNORMAL
    else
      nShow := SW_HIDE;
  end;
  Result := ShellExecuteExW(@SEInfo);
  if isPersistent then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
  {$ELSE}
  Process := TProcessUTF8.Create(nil);
  try
    Process.Executable := Exe;
    pr := TStringList.Create;
    try
      ParseCommandLine(Params, TStrings(pr), True);
      Process.Parameters.Assign(pr);
    finally
      pr.Free;
    end;
    Process.Execute;
  finally
    Process.Free;
  end;
  {$ENDIF}
end;

{$ifdef windows}
function WinRunProcessA(Exe, Params: string; ShowWind: Boolean; isPersistent: Boolean): Boolean;
var
  SEInfo: TSHELLEXECUTEINFOA;
begin
  Initialize(SEInfo);
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TSHELLEXECUTEINFOA);
  with SEInfo do begin
    wnd := 0;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    if isPersistent then
      fMask := fMask or SEE_MASK_NOCLOSEPROCESS;
    lpFile := PChar(Utf8ToAnsi(Exe));
    lpParameters := PChar(Utf8ToAnsi(Params));
    if ShowWind then
      nShow := SW_SHOWNORMAL
    else
      nShow := SW_HIDE;
  end;
  Result := ShellExecuteExA(@SEInfo);
  if isPersistent then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
end;

function WinRunProcessW(Exe, Params: string; ShowWind: Boolean; isPersistent: Boolean): Boolean;
var
  SEInfo: TSHELLEXECUTEINFOW;
begin
  Initialize(SEInfo);
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TSHELLEXECUTEINFOW);
  with SEInfo do begin
    wnd := 0;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    if isPersistent then
      fMask := fMask or SEE_MASK_NOCLOSEPROCESS;
    lpFile := PWideChar(UTF8Decode(Exe));
    lpParameters := PWideChar(UTF8Decode(Params));
    if ShowWind then
      nShow := SW_SHOWNORMAL
    else
      nShow := SW_HIDE;
  end;
  Result := ShellApi.ShellExecuteExW(@SEInfo);
  if isPersistent then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
end;
{$endif}

function RunExternalProcess(Exe: String; Params: array of string;
  ShowWind: Boolean; isPersistent: Boolean): Boolean;
{$ifndef windows}
var
  Process: TProcessUTF8;
  I: Integer;
{$endif}
begin
  if Trim(Exe) = '' then Exit(False);
  Result := True;
  {$ifdef windows}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := WinRunProcessW(Exe, StringsToCommandLine(Params), ShowWind, isPersistent)
  else
    Result := WinRunProcessA(Exe, StringsToCommandLine(Params), ShowWind, isPersistent);
  {$else}
  Process := TProcessUTF8.Create(nil);
  try
    Process.InheritHandles := isPersistent;
    Process.Executable := Exe;
    Process.Parameters.AddStrings(Params);
    if isPersistent then
      Process.Options := Process.Options + [poWaitOnExit]
    else
      Process.Options := [];
    if ShowWind then
      Process.ShowWindow := swoShow
    else
      Process.ShowWindow := swoHIDE;
    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 0 to GetEnvironmentVariableCount - 1 do
      Process.Environment.Add(GetEnvironmentString(I));
    Process.Execute;
  except
    on E: Exception do
    begin
      WriteLog_E('RunExternalProcess.Error '#13#10+
        'Executable: '+Exe+#13#10+
        'Parameters: '+StringsToCommandLine(Params)+#13#10+
        'Message   : '+E.Message+#13#10+
        GetStackTraceInfo);
    end;
  end;
  Process.Free;
 {$endif}
end;

function RunExternalProcess(Exe, Params: String; ShowWind: Boolean;
  isPersistent: Boolean): Boolean;
begin
  if Trim(Exe) = '' then Exit(False);
  {$ifdef windows}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := WinRunProcessW(Exe, Params, ShowWind, isPersistent)
  else
    Result := WinRunProcessA(Exe, Params, ShowWind, isPersistent);
  {$else}
  Result := RunExternalProcess(Exe, ParsedCommandLine(Params), ShowWind, isPersistent);
  {$endif}
end;

function RunExternalProcess(CommandLine: String; ShowWind: Boolean;
  isPersistent: Boolean): Boolean;
var
 s: string;
 sa: TArrayOfString;
begin
  if Trim(CommandLine) = '' then Exit(False);
  try
    sa := ParsedCommandLine(CommandLine);
    s := sa[Low(sa)];
    DeleteArrayOfString(sa, Low(sa));
    {$ifdef windows}
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      Result := WinRunProcessW(s, StringsToCommandLine(sa), ShowWind, isPersistent)
    else
      Result := WinRunProcessA(s, StringsToCommandLine(sa), ShowWind, isPersistent);
    {$else}
    Result := RunExternalProcess(s, sa, ShowWind, isPersistent);
    {$endif}
  finally
    SetLength(sa, 0);
  end;
end;

function HeaderByName(const AHeaders: TStrings; const AHeaderName: String): String;
var
  i, p: Cardinal;
  hn: String;

begin
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
