{
        File: baseunit.pas
        License: GPLv3
        This unit is part of Free Manga Downloader
}

unit baseunit;

{$MODE DELPHI}

interface

uses SysUtils, Classes, HTTPSend, graphics, genericlib, IniFiles;

const
  JPG_HEADER: array[0..2] of Byte = ($FF, $D8, $FF);
  GIF_HEADER: array[0..2] of Byte = ($47, $49, $46);
  PNG_HEADER: array[0..3] of Byte = ($89, $50, $4E, $47);
  CS_PAGE = 0;
  CS_INFO = 1;
  CS_GETPAGENUMBER   = 2;
  CS_GETPAGELINK     = 3;
  CS_DOWNLOAD        = 4;

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

  Genre: array [0..38] of AnsiString =
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

  DEFAULT_PATH  = 'c:\downloads';

  WORK_FOLDER   = 'works/';
  WORK_FILE     = 'works.ini';

  FAVORITES_FILE= 'favorites.ini';
  DATA_FOLDER   = 'data/';
  DATA_EXT      = '.dat';
  CONFIG_FOLDER = 'config/';
  CONFIG_FILE   = 'config.ini';

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

  ANIMEA_NAME = 'AnimeA'; ANIMEA_ID   = 0;

var
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
  SEPERATOR: AnsiString = #253#254;

  ANIMEA_ROOT   : String = 'http://manga.animea.net';
  ANIMEA_BROWSER: String = '/browse.html?page=';
  ANIMEA_SKIP   : String = '?skip=1';

  // en: dialog messages
  // vi: nội dung hộp thoại
  infoName,
  infoAuthors,
  infoArtists,
  infoGenres,
  infoStatus,
  infoSummary,
  infoLink ,
  stDlgNewManga,
  stDlgQuit,
  stDlgRemoveTask,
  stDlgRemoveFinishTasks,
  stDlgTypeInNewChapter,
  stDlgTypeInNewSavePath,
  stDlgCannotGetMangaInfo,
  stDlgFavoritesIsRunning,
  stDlgNoNewChapter,
  stDlgHasNewChapter : String;


type
  PMangaListItem = ^TMangaListItem;
  TMangaListItem = record
    Text: AnsiString;
  end;

  PMangaInfo = ^TMangaInfo;
  TMangaInfo = record
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

function  CorrectFile(const APath: String): String;
function  CorrectFilePath(const APath: String): String;
procedure CheckPath(const S: AnsiString);

function  GetMangaSiteID(const name: AnsiString): Cardinal;

function  RemoveSymbols(const input: AnsiString): AnsiString;

// EN: Get substring from source
// VI: Lấy chuỗi con từ chuỗi mẹ
function  GetString(const source, sStart, sEnd: AnsiString): AnsiString;

function  Find(const S: String; var List: TStringList; out index: Integer): Boolean;

// EN: Get param from input
// VI: Lấy param từ input
procedure GetParams(var output: TStringList; input: AnsiString); overload;
procedure GetParams(var output: TCardinalList; input: AnsiString); overload;
// EN: Set param from input
// VI: Cài param từ input
function  SetParams(input: TObject): AnsiString; overload;
function  SetParams(input: array of AnsiString): AnsiString; overload;

function  StringFilter(const source: AnsiString): AnsiString;

function  PrepareSummaryForHint(const source: AnsiString):  AnsiString;

// EN: Get HTML source code from a URL
// VI: Lấy webcode từ 1 URL
function  GetPage(var output: TObject; URL: AnsiString; const Reconnect: Cardinal): Boolean;
function  SavePage(URL: AnsiString; const Path: String; const Reconnect: Cardinal): Boolean;

procedure QuickSortData(var merge: TStringList);

function  GetCurrentJDN: LongInt;

function  ConvertInt32ToStr(const aValue: Cardinal)  : AnsiString;
function  ConvertStrToInt32(const aStr  : AnsiString): Cardinal;
procedure TransferMangaInfo(var dest: TMangaInfo; const source: TMangaInfo);

implementation

function  CorrectFile(const APath: String): String;
var I: Integer;
begin
  Result:= APath;
  for I:=1 to Length(Result) do
    if Result[I]= '\' then
      Result[I]:= '/';
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
procedure CheckPath(const S: AnsiString);
var
    wS,
    lcS,
    lcS2: AnsiString;
    i,
    j   : Word;
begin
  wS:= s;
  lcS2:= '';
  if wS[2]<>':' then
  begin
    lcS2:= CorrectFile(oldDir);
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
      if DirectoryExists(lcS2+lcS)=FALSE then
        MkDir(lcS2+lcS);
    end;
  end;
  SetCurrentDir(oldDir);
  Delete(wS, 1, 1);
end;

function  GetMangaSiteID(const name: AnsiString): Cardinal;
begin
  if name = ANIMEA_NAME then Result:= 0;
end;

function  RemoveSymbols(const input: AnsiString): AnsiString;
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
        Result:= StringReplace(Result, Symbols[i], '', []);
      end;
  until isDone;
end;

function  GetString(const source, sStart, sEnd: AnsiString): AnsiString;
var
  l: Word;
  s: AnsiString;
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

procedure GetParams(var output: TStringList; input: AnsiString);
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

procedure GetParams(var output: TCardinalList; input: AnsiString);
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

function  SetParams(input: TObject): AnsiString;
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

function  SetParams(input: array of AnsiString): AnsiString;
var
  i: Cardinal;
begin
  Result:= '';
  if Length(input) = 0 then exit;
  for i:= 0 to Length(input)-1 do
    Result:= Result + input[i] + SEPERATOR;
end;

function  StringFilter(const source: AnsiString): AnsiString;
begin
  if Length(source) = 0 then exit;
  Result:= source;
  Result:= StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result:= StringReplace(Result, #10, '\n',  [rfReplaceAll]);
  Result:= StringReplace(Result, #13, '\r',  [rfReplaceAll]);
end;

function  PrepareSummaryForHint(const source: AnsiString):  AnsiString;
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
  lineHeader: AnsiString;
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

function  GetPage(var output: TObject; URL: AnsiString; const Reconnect: Cardinal): Boolean;
var
  HTTP   : THTTPSend;
  counter: Cardinal = 0;
begin
  Result:= FALSE;
  HTTP:= THTTPSend.Create;
  HTTP.ProxyHost:= Host;
  HTTP.ProxyPort:= Port;
  HTTP.ProxyUser:= User;
  HTTP.ProxyHost:= Pass;
  while (NOT HTTP.HTTPMethod('GET', URL)) AND
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
    while (NOT HTTP.HTTPMethod('GET', URL)) AND
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
  if output is TJPEGImage then
    TJPEGImage(output).LoadFromStream(HTTP.Document);
  HTTP.Free;
  Result:= TRUE;
end;

function  SavePage(URL: AnsiString; const Path: String; const Reconnect: Cardinal): Boolean;
var
  header : array [0..3] of Byte;
  ext    : String;
  HTTP   : THTTPSend;
  counter: Cardinal = 0;
begin
  Result:= FALSE;
  HTTP:= THTTPSend.Create;
  HTTP.ProxyHost:= Host;
  HTTP.ProxyPort:= Port;
  HTTP.ProxyUser:= User;
  HTTP.ProxyHost:= Pass;
  while (NOT HTTP.HTTPMethod('GET', URL)) AND
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
    while (NOT HTTP.HTTPMethod('GET', URL)) AND
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
  if (header[0] = GIF_HEADER[0]) AND
     (header[1] = GIF_HEADER[1]) AND
     (header[2] = GIF_HEADER[2]) then
    ext:= '.gif'
  else
  if (header[0] = PNG_HEADER[0]) AND
     (header[1] = PNG_HEADER[1]) AND
     (header[2] = PNG_HEADER[2]) AND
     (header[3] = PNG_HEADER[3]) then
    ext:= '.png'
  else
    ext:= '';
  HTTP.Document.SaveToFile(Path+ext);
  HTTP.Free;
  Result:= TRUE;
end;

procedure QuickSortData(var merge: TStringList);
var
  names, output: TStringList;

  procedure QSort(L, R: Cardinal);
  var i, j: Cardinal;
         X: AnsiString;
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
    names.Add(output.Strings[1]);
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

function  ConvertInt32ToStr(const aValue: Cardinal)  : AnsiString;
begin
  Result:= '';
  Result:= Result+Char(aValue);
  Result:= Result+Char(aValue shr 8);
  Result:= Result+Char(aValue shr 16);
  Result:= Result+Char(aValue shr 24);
end;

function  ConvertStrToInt32(const aStr  : AnsiString): Cardinal;
begin
  Result:= (Byte(aStr[4]) shl 24) OR
           (Byte(aStr[3]) shl 16) OR
           (Byte(aStr[2]) shl 8) OR
            Byte(aStr[1]);
end;

procedure TransferMangaInfo(var dest: TMangaInfo; const source: TMangaInfo);
var
  i: Cardinal;
begin
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
  isSuccess:= SavePage(URL, Path, Retry);
  isDone   := TRUE;
  Suspend;
end;

end.
