{
        File: baseunit.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit baseunit;

{$MODE DELPHI}

interface

uses SysUtils, Classes, HTTPSend, graphics, genericlib, IniFiles;

const
  JPG_HEADER: array[0..2] of Byte = ($FF, $D8, $FF);
  GIF_HEADER: array[0..2] of Byte = ($47, $49, $46);
  PNG_HEADER: array[0..2] of Byte = ($89, $50, $4E);
  CS_DIRECTORY_COUNT = 0;
  CS_DIRECTORY_PAGE  = 1;
  CS_INFO            = 2;
  CS_GETPAGENUMBER   = 3;
  CS_GETPAGELINK     = 4;
  CS_DOWNLOAD        = 5;

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

  FILTER_HIDE           = 0;
  FILTER_SHOW           = 1;

  Genre: array [0..38] of String =
    ('Action'       , 'Adult'        , 'Adventure'    , 'Comedy',
     'Doujinshi'    , 'Drama'        , 'Ecchi'        , 'Fantasy',
     'Gender Bender', 'Harem'        , 'Hentai'       , 'Historical',
     'Horror'       , 'Josei'        , 'Lolicon'      , 'Martial Arts',
     'Mature'       , 'Mecha'        , 'Musical'      , 'Mystery',
     'Psychological', 'Romance'      , 'School Life'  , 'Sci-fi',
     'Seinen'       , 'Shotacon'     , 'Shoujo'       , 'Shoujo Ai',
     'Shounen'      , 'Shounen Ai'   , 'Slice of Life', 'Smut',
     'Sports'       , 'Supernatural' , 'Taboo'        , 'Traged',
     'Yaoi'         , 'Yuri'         , 'Webtoons');

  Symbols: array [0..8] of Char =
    ('\', '/', ':', '*', '?', '"', '<', '>', '|');

  {$IFDEF WIN32}
  DEFAULT_PATH  = 'c:\downloads';
  {$ELSE}
  DEFAULT_PATH  = '/downloads';
  {$ENDIF}

  README_FILE       = 'readme.rtf';

  WORK_FOLDER       = 'works/';
  WORK_FILE         = 'works.ini';

  FAVORITES_FILE    = 'favorites.ini';
  DATA_FOLDER       = 'data/';
  DATA_EXT          = '.dat';
  CONFIG_FOLDER     = 'config/';
  CONFIG_FILE       = 'config.ini';
  MANGALISTINI_FILE = 'mangalist.ini';
  LANGUAGE_FILE     = 'languages.ini';

  OPTION_MANGALIST = 0;
  OPTION_RECONNECT = 1;

  STATUS_STOP      = 0;
  STATUS_WAIT      = 1;
  STATUS_PREPARE   = 2;
  STATUS_DOWNLOAD  = 3;
  STATUS_FINISH    = 4;

  NO_ERROR              = 0;
  NET_PROBLEM           = 1;
  INFORMATION_NOT_FOUND = 2;

  ANIMEA_NAME       = 'AnimeA';       ANIMEA_ID      = 0;
  MANGAHERE_NAME    = 'MangaHere';    MANGAHERE_ID   = 1;
  MANGAINN_NAME     = 'MangaInn';     MANGAINN_ID    = 2;
  OURMANGA_NAME     = 'OurManga';     OURMANGA_ID    = 3;
  KISSMANGA_NAME    = 'KissManga';    KISSMANGA_ID   = 4;
  BATOTO_NAME       = 'Batoto';       BATOTO_ID      = 5;
  MANGA24H_NAME     = 'Manga24h';     MANGA24H_ID    = 6;
  VNSHARING_NAME    = 'VnSharing';    VNSHARING_ID   = 7;
  HENTAI2READ_NAME  = 'Hentai2Read';  HENTAI2READ_ID = 8;
  FAKKU_NAME        = 'Fakku';        FAKKU_ID       = 9;
  TRUYEN18_NAME     = 'Truyen18';     TRUYEN18_ID    = 10;
  MANGAREADER_NAME  = 'MangaReader';  MANGAREADER_ID = 11;
  MANGAPARK_NAME    = 'MangaPark';    MANGAPARK_ID   = 12;
  GEHENTAI_NAME     = 'g.e-hentai (doujinshi)'; GEHENTAI_ID = 13;

var
  currentJDN       : Cardinal;
  isChangeDirectory: Boolean = FALSE;

  currentWebsite,
  stModeAll,
  stModeFilter,

  stCompressing,
  stPreparing,
  stDownloading,
  stWait,
  stStop,
  stFinish: String;

  Host  : String = '';
  Port  : String = '';
  User  : String = '';
  Pass  : String = '';
  oldDir: String;
  // EN: Param seperator
  // VI: Ký tự dùng để chia cắt param trong dữ liệu
  SEPERATOR: String = '!%~';

  ANIMEA_ROOT   : String = 'http://manga.animea.net';
  ANIMEA_BROWSER: String = '/browse.html?page=';
  ANIMEA_SKIP   : String = '?skip=1';

  MANGAHERE_ROOT   : String = 'http://www.mangahere.com';
  MANGAHERE_BROWSER: String = '/mangalist/';

  MANGAINN_ROOT   : String = 'http://www.mangainn.com';
  MANGAINN_BROWSER: String = '/mangalist/';

  OURMANGA_ROOT   : String = 'http://www.ourmanga.com';
  OURMANGA_BROWSER: String = '/directory/';

  KISSMANGA_ROOT   : String = 'http://kissmanga.com';
  KISSMANGA_BROWSER: String = '/MangaList';

  BATOTO_ROOT      : String = 'http://www.batoto.net';
  BATOTO_BROWSER   : String = '/search';

  MANGA24H_ROOT   : String = 'http://manga24h.com';
  MANGA24H_BROWSER: String = '/manga/list';

  VNSHARING_ROOT   : String = 'http://truyen.vnsharing.net';
  VNSHARING_BROWSER: String = '/DanhSach';

  HENTAI2READ_ROOT   : String = 'http://hentai2read.com';
  HENTAI2READ_BROWSER: String = '/hentai-list/all/any/name-az/';

  FAKKU_ROOT             : String = 'http://www.fakku.net';
  FAKKU_BROWSER          : String = '/manga/newest';
  FAKKU_MANGA_BROWSER    : String = '/manga/newest';
  FAKKU_DOUJINSHI_BROWSER: String = '/doujinshi/newest';

  TRUYEN18_ROOT   : String = 'http://www.truyen18.org';
  TRUYEN18_BROWSER: String = '/moi-dang/danhsach';

  MANGAREADER_ROOT   : String = 'http://www.mangareader.net';
  MANGAREADER_BROWSER: String = '/alphabetical';

  MANGAPARK_ROOT   : String = 'http://www.mangapark.com';
  MANGAPARK_BROWSER: String = '/list/';

  GEHENTAI_ROOT   : String = 'http://g.e-hentai.org';
  GEHENTAI_BROWSER: String = '&f_doujinshi=on&advsearch=1&f_search=Search+Keywords&f_srdd=2&f_sname=on&f_stags=on&f_apply=Apply+Filter';

  // en: dialog messages
  // vi: nội dung hộp thoại
  infoCustomGenres,
  infoName,
  infoAuthors,
  infoArtists,
  infoGenres,
  infoStatus,
  infoSummary,
  infoLink ,

  stDownloadManga,
  stDownloadStatus,
  stDownloadProgress,
  stDownloadWebsite,
  stDownloadSaveto,
  stDownloadAdded,
  stFavoritesCurrentChapter,

  stFavoritesCheck,
  stFavoritesChecking,

  stDlgURLNotSupport,
  stDldMangaListSelect,
  stDlgUpdateAlreadyRunning,
  stDlgNewManga,
  stDlgQuit,
  stDlgRemoveTask,
  stDlgRemoveFinishTasks,
  stDlgTypeInNewChapter,
  stDlgTypeInNewSavePath,
  stDlgCannotGetMangaInfo,
  stDlgFavoritesIsRunning,
  stDlgNoNewChapter,
  stDlgHasNewChapter,
  stDlgRemoveCompletedManga: String;

type
  PMangaListItem = ^TMangaListItem;
  TMangaListItem = record
    Text: String;
  end;

  PMangaInfo = ^TMangaInfo;
  TMangaInfo = record
    url,
    title,
    link,
    website,
    coverLink,
    authors,
    artists,
    genres,
    status,
    summary     : String;
    numChapter  : Cardinal;
    chapterName,
    chapterLinks: TStringList;
  end;

  PDownloadInfo = ^TDownloadInfo;
  TDownloadInfo = record
    title,
    Status,
    Progress,
    Website,
    SaveTo,
    dateTime : String;
    iProgress: Integer;
  end;

  PFavoriteInfo = ^TFavoriteInfo;
  TFavoriteInfo = record
    title,
    currentChapter,
    Website,
    SaveTo,
    Link     : String;
  end;

  TCardinalList = TGenericList<Cardinal>;
  TByteList   = TGenericList<Byte>;

  TDownloadPageThread = class(TThread)
  protected
    procedure Execute; override;
  public
    isSuccess,
    isDone: Boolean;
    Retry : Cardinal;
    URL,
    Path  : String;
    constructor Create(CreateSuspended: Boolean);
  end;

function  UnicodeRemove(const S: String): String;
function  CheckRedirect(const HTTP: THTTPSend): String;
function  CorrectFile(const APath: String): String;
function  CorrectFilePath(const APath: String): String;
function  CorrectURL(const URL: String): String;
procedure CheckPath(const S: String);

function  GetMangaSiteID(const name: String): Cardinal;

function  RemoveSymbols(const input: String): String;

// EN: Get substring from source
// VI: Lấy chuỗi con từ chuỗi mẹ
function  GetString(const source, sStart, sEnd: String): String;

function  Find(const S: String; var List: TStringList; out index: Integer): Boolean;

// EN: Get param from input
// VI: Lấy param từ input
procedure GetParams(var output: TStringList; input: String); overload;
procedure GetParams(var output: TCardinalList; input: String); overload;
// EN: Set param from input
// VI: Cài param từ input
function  SetParams(input: TObject): String; overload;
function  SetParams(const input: array of String): String; overload;

procedure CustomGenres(var output: TStringList; input: String);

function  FixPath(const path: String): String;
function  FixLastDir(const path: String): String;
function  StringFilter(const source: String): String;
function  HTMLEntitiesFilter(const source: String): String;
function  StringBreaks(const source: String): String;
function  RemoveStringBreaks(const source: String): String;

function  PrepareSummaryForHint(const source: String):  String;

// EN: Get HTML source code from a URL
// VI: Lấy webcode từ 1 URL
function  GetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
function  SavePage(URL: String;  const Path, name: String; const Reconnect: Cardinal): Boolean;

procedure QuickSortData(var merge: TStringList);

function  GetCurrentJDN: LongInt;

{function  ConvertInt32ToStr(const aValue: Cardinal)  : String;
function  ConvertStrToInt32(const aStr  : String): Cardinal;}
procedure TransferMangaInfo(var dest: TMangaInfo; const source: TMangaInfo);

// cross platform funcs

function  fmdGetTempPath: String;

implementation

uses FileUtil{$IFDEF WINDOWS}, Windows{$ENDIF};

function  UnicodeRemove(const S: String): String;
var i: Cardinal;
begin
 // if doRemoveName then
    Result:= S;
 { else
  begin
    result:= '';
    exit;
  end; }
 // if NOT doRemoveUnicode then exit;
  for i:= 1 to Length(Result) do
  begin
    if (Byte(Result[i])<31) OR (Byte(Result[i])>127) then
    begin
      Delete(Result, i, 1);
      Insert('_', Result, i);
    end;
  end;
end;

function  CorrectFile(const APath: String): String;
var I: Integer;
begin
  Result:= APath;
  for I:=1 to Length(Result) do
    if Result[I]= '\' then
      Result[I]:= '/';
  if Result[Length(Result)]<>'/' then
    Result:= Result + '/';
  while system.Pos('//', Result) > 0 do
    Result:= StringReplace(Result, '//', '/', []);
end;

function  CorrectURL(const URL: String): String;
begin
  Result:= StringReplace(URL, ' ', '%20', [rfReplaceAll]);
end;

function  CorrectFilePath(const APath: String): String;
var I: Integer;
begin
  Result:= APath;
  for I:=1 to Length(Result) do
    if Result[I]= '\' then
      Result[I]:='/';
  if Length(Result)<>0 then
    if Result[Length(Result)]<>'/' then
      Result:= Result+'/';
end;

// took from an old project - maybe bad code
procedure CheckPath(const S: String);
var
    wS,
    lcS,
    lcS2: String;
    i,
    j   : Word;
begin
  wS:= s;
  lcS2:= '';
  if wS[2]<>':' then
  begin
    {$IFDEF WIN32}
    lcS2:= CorrectFile(oldDir);
    {$ELSE}
    lcS2:= '';
    {$ENDIF}
    Insert('/', wS, 1);
  end
  else
  begin
    if Length(wS)=2 then
      wS:= wS+'/';
  end;
  for i:= 1 to Length(wS) do
  begin
    lcS2:= lcS2+wS[i];
    if (wS[i]='/') AND ((wS[i+1]<>'/') OR (wS[i+1]<>' ')) AND
       (i<Length(wS)) then
    begin
      j:= i+1;
      lcS:= '';
      repeat
        lcS:= lcS+wS[j];
        Inc(j);
      until wS[j]='/';
      if NOT DirectoryExistsUTF8(lcS2+lcS) then
      begin
        CreateDirUTF8(lcS2+lcS);
      end;
    end;
  end;
  SetCurrentDirUTF8(oldDir);
  Delete(wS, 1, 1);
end;

function  GetMangaSiteID(const name: String): Cardinal;
begin
  if name = ANIMEA_NAME then Result:= ANIMEA_ID
  else
  if name = MANGAHERE_NAME then Result:= MANGAHERE_ID
  else
  if name = MANGAINN_NAME then Result:= MANGAINN_ID
  else
  if name = OURMANGA_NAME then Result:= OURMANGA_ID
  else
  if name = KISSMANGA_NAME then Result:= KISSMANGA_ID
  else
  if name = BATOTO_NAME then Result:= BATOTO_ID
  else
  if name = MANGA24H_NAME then Result:= MANGA24H_ID
  else
  if name = VNSHARING_NAME then Result:= VNSHARING_ID
  else
  if name = HENTAI2READ_NAME then Result:= HENTAI2READ_ID
  else
  if name = FAKKU_NAME then Result:= FAKKU_ID
  else
  if name = MANGAREADER_NAME then Result:= MANGAREADER_ID
  else
  if name = MANGAPARK_NAME then Result:= MANGAPARK_ID
  else
  if name = GEHENTAI_NAME then Result:= GEHENTAI_ID;
end;

function  RemoveSymbols(const input: String): String;
var
  i     : Cardinal;
  isDone: Boolean;
begin
  Result:= input;
  repeat
    isDone:= TRUE;
    for i:= 0 to 8 do
      if Pos(Symbols[i], Result)<>0 then
      begin
        isDone:= FALSE;
        Result:= StringReplace(Result, Symbols[i], '', [rfReplaceAll]);
      end;
  until isDone;
  if (Length(Result)>0) AND
     (Result[Length(Result)] = '.') then
  begin
    Result[Length(Result)]:= '-';
  end;
end;

function  GetString(const source, sStart, sEnd: String): String;
var
  l: Word;
  s: String;
begin
  Result:= '';
  l:= Pos(sStart, source);
  if (l<>0) AND (source[l+Length(sStart)]<>sEnd[1]) then
  begin
    s:= RightStr(source, Length(source)-l-Length(sStart)+1);
    l:= Pos(sEnd, s);
    if (l<>0) then
      Result:= LeftStr(s, l-1);
  end;
end;

function  Find(const S: String; var List: TStringList; out index: Integer): Boolean;
var
  i: Cardinal;
begin
  Result:= FALSE;
  index:= -1;
  if List.Count = 0 then exit;
  for i:= 0 to List.Count-1 do
  begin
    if CompareStr(S, List.Strings[i])=0 then
    begin
      index:= i;
      Result:= TRUE;
      break;
    end;
  end;
end;

procedure GetParams(var output: TStringList; input: String);
var l: Word;
begin
  repeat
    l:= Pos(SEPERATOR, input);
    if l<>0 then
    begin
      output.Add(LeftStr(input, l-1));
      input:= RightStr(input, Length(input)-l-Length(SEPERATOR)+1);
    end;
  until l = 0;
end;

procedure GetParams(var output: TCardinalList; input: String);
var l: Word;
begin
  repeat
    l:= Pos(SEPERATOR, input);
    if l<>0 then
    begin
      output.Add(StrToInt(LeftStr(input, l-1)));
      input:= RightStr(input, Length(input)-l-Length(SEPERATOR)+1);
    end;
  until l = 0;
end;

function  SetParams(input: TObject): String;
var
  i: Cardinal;
begin
  Result:= '';
  if input is TStringList then
  begin
    if TStringList(input).Count = 0 then exit;
    for i:= 0 to TStringList(input).Count-1 do
      Result:= Result + TStringList(input).Strings[i] + SEPERATOR;
  end
  else
  if input is TCardinalList then
  begin
    if TCardinalList(input).Count = 0 then exit;
    for i:= 0 to TCardinalList(input).Count-1 do
      Result:= Result + IntToStr(TCardinalList(input).Items[i]) + SEPERATOR;
  end
  else
  if input is TByteList then
  begin
    if TByteList(input).Count = 0 then exit;
    for i:= 0 to TByteList(input).Count-1 do
      Result:= Result + IntToStr(TByteList(input).Items[i]) + SEPERATOR;
  end;
end;

function  SetParams(const input: array of String): String;
var
  i: Cardinal;
begin
  Result:= '';
  if Length(input) = 0 then exit;
  for i:= 0 to Length(input)-1 do
    Result:= Result + input[i] + SEPERATOR;
end;

function  FixPath(const path: String): String;
var
  i: Cardinal;
begin
  Result:= '';
  if Length(path)=0 then exit;
  for i:= 1 to Length(path) do
  begin
    if Byte(path[i])>=128 then
      Result:= Result+'_'
    else
      Result:= Result+path[i];
  end;
end;

function  FixLastDir(const path: String): String;
var
  i, p: Cardinal;
begin
  Result:= '';
  if Length(path)=0 then exit;
  i:= Length(path);
  for i:= 1 to Length(path) do
  begin
    Result:= Result+path[i];
    if path[i] = '/' then
      p:= i;
  end;
  for i:= p to Length(Result)-1 do
    if Byte(Result[i])>=128 then
    begin
      Delete(Result, i, 1);
      Insert('_', Result, i);
    end;
end;

function  StringFilter(const source: String): String;
begin
  if Length(source) = 0 then exit;
  Result:= StringReplace(source, '&#33;', '!', [rfReplaceAll]);
  Result:= StringReplace(Result, '&#036;', '$', [rfReplaceAll]);
  Result:= StringReplace(Result, '&#039;', '''', [rfReplaceAll]);
  Result:= StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  Result:= StringReplace(Result, '&nbsp;', '', [rfReplaceAll]);
  Result:= StringReplace(Result, '&ldquo;', '"', [rfReplaceAll]);
  Result:= StringReplace(Result, '&ldquo;', '"', [rfReplaceAll]);
  Result:= StringReplace(Result, '&rdquo;', '"', [rfReplaceAll]);
  Result:= StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result:= StringReplace(Result, '&lsquo;', '''', [rfReplaceAll]);
  Result:= StringReplace(Result, '&rsquo;', '''', [rfReplaceAll]);
 // Result:= StringReplace(Result, '&nbsp;', ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, #10, '\n',  [rfReplaceAll]);
  Result:= StringReplace(Result, #13, '\r',  [rfReplaceAll]);
end;

function  HTMLEntitiesFilter(const source: String): String;
begin
  if Length(source) = 0 then exit;

  // uppercase

  Result:= StringReplace(source, '&Agrave;', 'À', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Aacute;', 'Á', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Acirc;' , 'Â', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Atilde;', 'Ã', [rfReplaceAll]);

  Result:= StringReplace(Result, '&Egrave;', 'È', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Eacute;', 'É', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Ecirc;' , 'Ê', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Etilde;', 'Ẽ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&Igrave;', 'Ì', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Iacute;', 'Í', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Itilde;', 'Ĩ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&ETH;'   , 'Đ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&Ograve;', 'Ò', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Oacute;', 'Ó', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Ocirc;' , 'Ô', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Otilde;', 'Õ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&Ugrave;', 'Ù', [rfReplaceAll]);
  Result:= StringReplace(Result, '&Uacute;', 'Ú', [rfReplaceAll]);

  Result:= StringReplace(Result, '&Yacute;', 'Ý', [rfReplaceAll]);

  // lowercase

  Result:= StringReplace(Result, '&agrave;', 'à', [rfReplaceAll]);
  Result:= StringReplace(Result, '&aacute;', 'á', [rfReplaceAll]);
  Result:= StringReplace(Result, '&acirc;' , 'â', [rfReplaceAll]);
  Result:= StringReplace(Result, '&atilde;', 'ã', [rfReplaceAll]);

  Result:= StringReplace(Result, '&egrave;', 'è', [rfReplaceAll]);
  Result:= StringReplace(Result, '&eacute;', 'é', [rfReplaceAll]);
  Result:= StringReplace(Result, '&etilde;', 'ẽ', [rfReplaceAll]);
  Result:= StringReplace(Result, '&ecirc;' , 'ê', [rfReplaceAll]);

  Result:= StringReplace(Result, '&igrave;', 'ì', [rfReplaceAll]);
  Result:= StringReplace(Result, '&iacute;', 'í', [rfReplaceAll]);
  Result:= StringReplace(Result, '&itilde;', 'ĩ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&eth;'   , 'đ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&ograve;', 'ò', [rfReplaceAll]);
  Result:= StringReplace(Result, '&oacute;', 'ó', [rfReplaceAll]);
  Result:= StringReplace(Result, '&ocirc;' , 'ô', [rfReplaceAll]);
  Result:= StringReplace(Result, '&otilde;', 'õ', [rfReplaceAll]);

  Result:= StringReplace(Result, '&ugrave;', 'ù', [rfReplaceAll]);
  Result:= StringReplace(Result, '&uacute;', 'ú', [rfReplaceAll]);

  Result:= StringReplace(Result, '&yacute;', 'ý', [rfReplaceAll]);
end;

procedure  CustomGenres(var output: TStringList; input: String);
var
  s: String = '';
  i: Word;
begin
  if Length(input) = 0 then exit;
  for i:= 1 to Length(input) do
  begin
    if (input[i] = ',') OR (input[i] = ';') then
    begin
      TrimLeft(TrimRight(s));
      if Length(s) <> 0 then
      begin
        output.Add(s);
        s:= '';
      end;
    end
    else
      s:= s+input[i];
  end;
  TrimLeft(TrimRight(s));
  if Length(s) <> 0 then
    output.Add(s);
end;

function  StringBreaks(const source: String): String;
begin
  if Length(source) = 0 then exit;
  Result:= source;
  Result:= StringReplace(Result, '\n', #10,  [rfReplaceAll]);
  Result:= StringReplace(Result, '\r', #13,  [rfReplaceAll]);
end;

function  RemoveStringBreaks(const source: String): String;
begin
  if Length(source) = 0 then exit;
  Result:= StringReplace(source, #10, '', [rfReplaceAll]);
  Result:= StringReplace(Result, #13, '', [rfReplaceAll]);
end;

function  PrepareSummaryForHint(const source: String):  String;
var
  i: Cardinal = 1;
  j: Cardinal = 1;
begin
  Result:= source;
  repeat
    if (j>80) AND (Result[i] = ' ') then
    begin
      Insert(#10#13, Result, i);
      Inc(i, 2);
      j:= 1;
    end;
    Inc(j);
    Inc(i);
  until i >= Length(Result);
  Result:= StringReplace(Result, '\n', #10,  [rfReplaceAll]);
  Result:= StringReplace(Result, '\r', #13,  [rfReplaceAll]);
end;

function  CheckRedirect(const HTTP: THTTPSend): String;
var
  lineHeader: String;
  i: Byte;
begin
  Result:= '';
  i:= 0;
  while (Result = '') AND (i < HTTP.Headers.Count) do
  begin
    lineHeader:= HTTP.Headers[I];
    if Pos('Location: ', lineHeader) = 1 then
      Result:= Copy(lineHeader, 11, Length(lineHeader));
    Inc(i);
  end;
end;

function  GetPage(var output: TObject; URL: String; const Reconnect: Cardinal): Boolean;
var
  HTTP   : THTTPSend;
  code   : Cardinal;
  counter: Cardinal = 0;
  s      : String;
label
  globReturn;
begin
  Result:= FALSE;
  HTTP:= THTTPSend.Create;
globReturn:
  HTTP.ProxyHost:= Host;
  HTTP.ProxyPort:= Port;
  HTTP.ProxyUser:= User;
  HTTP.ProxyHost:= Pass;
  if Pos(HENTAI2READ_ROOT, URL) <> 0 then
    HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/');
 // else
  //  HTTP.Headers.Insert(0, 'Referer:'+GEHENTAI_ROOT+'/')
 //   HTTP.Headers.Insert(0, '{''User-Agent'' : ''Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.11 (KHTML, like Gecko) Ubuntu/11.10 Chromium/17.0.963.79 Chrome/17.0.963.79 Safari/535.11''}');

 { else
  if Pos(TRUYEN18_ROOT, URL) <> 0 then
    HTTP.Headers.Insert(0, 'Referer:'+'http://adf.ly/2337104/http%3a%2f%2fwww.truyen18.org%2fhentai%2fkaren-chameleon%2f4220.html');
  else
  if Pos(MANGA24H_ROOT, URL) <> 0 then
  begin
    HTTP.Headers.Add('Accept: text/xml,application/xml,application/json,application/xhtml+xml,');
    HTTP.Headers.Add('text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5');
    HTTP.Headers.Add('Cache-Control: max-age=0');
    HTTP.Headers.Add('Connection: keep-alive');
    HTTP.Headers.Add('Keep-Alive: 300');
    HTTP.Headers.Add('Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7');
    HTTP.Headers.Add('Accept-Language: en-us,en;q=0.5');
    HTTP.Headers.Add('Pragma: ');

    HTTP.UserAgent:= 'Mozilla/5.0 (Windows NT 5.1; rv:5.0) Gecko/20100101 Firefox/5.0 Firefox/5.0';
    HTTP.Headers.Add('Referer:'+MANGA24H_ROOT+'/;');
    HTTP.Headers.Add('Encoding:gzip,deflate;');

    HTTP.Cookies.Text:=
   // HTTP.Headers.Insert(0, 'Cookie:expires=Sat, 26-Jan-2013 00:00:01 GMT');
  end;}
  while (NOT HTTP.HTTPMethod('GET', URL)) OR
        (HTTP.ResultCode > 500) do
  begin
    code:= HTTP.ResultCode;
    HTTP.Document.SaveToFile('error.txt');
    if Reconnect <> 0 then
    begin
      if Reconnect <= counter then
      begin
        HTTP.Free;
        exit;
      end;
      Inc(counter);
    end;
    HTTP.Clear;
    Sleep(500);
  end;
  if Pos('?nw=session', URL) > 0 then
  begin
    HTTP.Clear;
    Delete(URL, Length(URL)-10, 11);
    goto globReturn;
  end;
 // HTTP.Document.SaveToFile('error2.txt');
 // code:= HTTP.ResultCode;
 // s:= HTTP.Cookies.Text;
  while HTTP.ResultCode = 302 do
  begin
    URL:= CheckRedirect(HTTP);
    HTTP.Clear;
    HTTP.RangeStart:= 0;
    if Pos(HENTAI2READ_ROOT, URL) <> 0 then
      HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/');{
    else
    if Pos(MANGA24H_ROOT, URL) <> 0 then
      HTTP.Headers.Insert(0, 'Cookie:expires=Sat, 26-Jan-2013 00:00:01 GMT');}
    while (NOT HTTP.HTTPMethod('GET', URL)) OR
        (HTTP.ResultCode >= 500) do
    begin
      if Reconnect <> 0 then
      begin
        if Reconnect <= counter then
        begin
          HTTP.Free;
          exit;
        end;
        Inc(counter);
      end;
      HTTP.Clear;
      Sleep(500);
    end;
  end;
  if output is TStringList then
    TStringList(output).LoadFromStream(HTTP.Document)
  else
  if output is TPicture then
    TPicture(output).LoadFromStream(HTTP.Document);
  HTTP.Free;
  Result:= TRUE;
end;

function  SavePage(URL: String; const Path, name: String; const Reconnect: Cardinal): Boolean;
var
  header  : array [0..3] of Byte;
  ext     : String;
  HTTP    : THTTPSend;
  counter : Cardinal = 0;
begin
  Result:= FALSE;
  HTTP:= THTTPSend.Create;
  HTTP.ProxyHost:= Host;
  HTTP.ProxyPort:= Port;
  HTTP.ProxyUser:= User;
  HTTP.ProxyHost:= Pass;
  if Pos(HENTAI2READ_ROOT, URL) <> 0 then
    HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/');
  while (NOT HTTP.HTTPMethod('GET', URL)) OR
        (HTTP.ResultCode >= 500) do
  begin
    if Reconnect <> 0 then
    begin
      if Reconnect <= counter then
      begin
        HTTP.Free;
        exit;
      end;
      Inc(counter);
    end;
    HTTP.Clear;
    Sleep(500);
  end;

  while HTTP.ResultCode = 302 do
  begin
    URL:= CheckRedirect(HTTP);
    HTTP.Clear;
    HTTP.RangeStart:= 0;
    if Pos(HENTAI2READ_ROOT, URL) <> 0 then
      HTTP.Headers.Insert(0, 'Referer:'+HENTAI2READ_ROOT+'/');
    while (NOT HTTP.HTTPMethod('GET', URL)) OR
        (HTTP.ResultCode >= 500) do
    begin
      if Reconnect <> 0 then
      begin
        if Reconnect <= counter then
        begin
          HTTP.Free;
          exit;
        end;
        Inc(counter);
      end;
      HTTP.Clear;
      Sleep(500);
    end;
  end;
  HTTP.Document.Seek(0, soBeginning);
  HTTP.Document.Read(header[0], 4);
  if (header[0] = JPG_HEADER[0]) AND
     (header[1] = JPG_HEADER[1]) AND
     (header[2] = JPG_HEADER[2]) then
    ext:= '.jpg'
  else
  if (header[0] = PNG_HEADER[0]) AND
     (header[1] = PNG_HEADER[1]) AND
     (header[2] = PNG_HEADER[2]) then
    ext:= '.png'
  else
  if (header[0] = GIF_HEADER[0]) AND
     (header[1] = GIF_HEADER[1]) AND
     (header[2] = GIF_HEADER[2]) then
    ext:= '.gif'
  else
    ext:= '';

 // SetCurrentDirUTF8();
 // HTTP.Document.SaveToFile('/home/akarin/FreeSpace/FMD/trunk/mangadownloader/downloads/' + name+ext);
  HTTP.Document.SaveToFile(Path+name+ext);
  HTTP.Free;
  Result:= TRUE;
end;

procedure QuickSortData(var merge: TStringList);
var
  names, output: TStringList;

  procedure QSort(L, R: Cardinal);
  var i, j: Cardinal;
         X: String;
  begin
    X:= names.Strings[(L+R) div 2];
    i:= L;
    j:= R;
    repeat
      while StrComp(PChar(names.Strings[i]), PChar(X))<0 do Inc(i);
      while StrComp(PChar(names.Strings[j]), PChar(X))>0 do Dec(j);
      if i<=j then
      begin
        names.Exchange(i, j);
        merge.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    until i>j;
    if L < j then QSort(L, j);
    if i < R then QSort(i, R);
  end;

var
  i: Cardinal;

begin
  names := TStringList.Create;
  output:= TStringList.Create;
  for i:= 0 to merge.Count-1 do
  begin
    output.Clear;
    GetParams(output, merge.Strings[i]);
    names.Add(output.Strings[DATA_PARAM_NAME]);
  end;
  QSort(1, names.Count-1);
  output.Free;
  names.Free;
end;

function  GetCurrentJDN: LongInt;
var
  day, month, year: Word;
  a, y, m         : Single;
begin
  DecodeDate(Now, year, month, day);
  a:= (14 - month) / 12;
  y:= year + 4800 - a;
  m:= month + 12*a - 3;
  Result:= Round(day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - 32045);
end;

{function  ConvertInt32ToStr(const aValue: Cardinal)  : String;
begin
  Result:= '';
  Result:= Result+Char(aValue);
  Result:= Result+Char(aValue shr 8);
  Result:= Result+Char(aValue shr 16);
  Result:= Result+Char(aValue shr 24);
end;

function  ConvertStrToInt32(const aStr  : String): Cardinal;
begin
  Result:= (Byte(aStr[4]) shl 24) OR
           (Byte(aStr[3]) shl 16) OR
           (Byte(aStr[2]) shl 8) OR
            Byte(aStr[1]);
end;}

procedure TransferMangaInfo(var dest: TMangaInfo; const source: TMangaInfo);
var
  i: Cardinal;
begin
  dest.url        := source.url;
  dest.title      := source.title;
  dest.link       := source.link;
  dest.website    := source.website;
  dest.coverLink  := source.coverLink;
  dest.authors    := source.authors;
  dest.artists    := source.artists;
  dest.genres     := source.genres;
  dest.status     := source.status;
  dest.summary    := source.summary;
  dest.numChapter := source.numChapter;
  dest.chapterName .Clear;
  dest.chapterLinks.Clear;
  if source.chapterLinks.Count <> 0 then
    for i:= 0 to source.chapterLinks.Count-1 do
    begin
      dest.chapterName .Add(source.chapterName .Strings[i]);
      dest.chapterLinks.Add(source.chapterLinks.Strings[i]);
    end;
end;

constructor TDownloadPageThread.Create(CreateSuspended: Boolean);
begin
  isDone:= FALSE;
  FreeOnTerminate:= TRUE;
  inherited Create(CreateSuspended);
end;

procedure   TDownloadPageThread.Execute;
begin
 // isSuccess:= SavePage(URL, Path, Retry);
  isDone   := TRUE;
  Suspend;
end;

function    fmdGetTempPath: String;
var
  l: Cardinal;
begin
{$IFDEF WINDOWS}
  SetLength(Result, 4096);
  l:= GetTempPath(4096, PChar(Result));
  SetLength(Result, l+1);
{$ENDIF}
end;

end.
