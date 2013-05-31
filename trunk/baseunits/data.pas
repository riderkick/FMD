{
        File: data.pas
        License: GPLv2
        This unit is part of Free Manga Downloader
}

unit data;

{$mode delphi}
{$DEFINE DOWNLOADER}

// EN: This unit contains all necessary functions for data processing
// VI: Unit chứa tất cả các hàm liên quan tới xử lý dữ liệu

interface

uses
  Classes, SysUtils, baseunit, fgl;

type
  TDataProcess = class(TObject)
  private
    function    GetInfo(const index: Cardinal): TStringList;
    function    GetParam(const index, paramNo: Cardinal): String;
  public
    website,
    Filename  : String;
    isFiltered: Boolean;
    filterMark: TByteList;
    filterPos : TCardinalList;
    Data,

    // parts
    Title,
    Link,
    Authors,
    Artists,
    Genres,
    Status,
    Summary   : TStringList;
    JDN       : TList;

    constructor Create;
    destructor  Destroy; override;
    function    FirstParam(const index: Cardinal): String;

    // en: Break data into parts... This may be considered as bad coding, but
    //     it's for faster filter
    // vi: Thử tục này sẽ break data nhằm tăng tốc cho filter
    procedure   BreakDataToParts(const i: Cardinal);

    function    LoadFromFile(const website: String): Boolean;
    procedure   SaveToFile(const website: String); overload;
    procedure   SaveToFile; overload;

    // en: Filter by genres, title, authors, ...
    // vi: Filter theo genre, tên, tác giả, ...
    function    Filter(const checkedGenres, uncheckedGenres: TStringList;
                       const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                       const minusDay: Cardinal;
                       const haveAllChecked, searchNewManga: Boolean): Boolean;

    // en: Remove filter
    // vi: Xóa bỏ filter
    procedure   RemoveFilter;
    procedure   Sort;
    property    Info [index: Cardinal]: TStringList read GetInfo;
    property    Param[index, paramNo: Cardinal]: String read GetParam;
  end;

  TMangaInformation = class(TObject)
  public
    isGetByUpdater : Boolean;
    mangaInfo      : TMangaInfo;
    parse          : TStringList;
    isGenerateFolderChapterName,
    isRemoveUnicode: Boolean;

    procedure   OnTag (tag : String);
    procedure   OnText(text: String);
    constructor Create;
    destructor  Destroy; override;
    function    GetDirectoryPage(var Page: Cardinal;
                                 const website: String): Byte;
    function    GetNameAndLink(const names, links: TStringList;
                               const website, URL: String): Byte;
    function    GetInfoFromURL(const website, URL: String; const Reconnect: Cardinal): Byte;
    procedure   SyncInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
    // Only use this function for getting manga infos for the first time
    procedure   AddInfoToDataWithoutBreak(const name, link : String;
                                          const DataProcess: TDataProcess);
    // Only use this function for update manga list
    procedure   AddInfoToData(const name, link : String;
                              const DataProcess: TDataProcess);
  end;

var
  options: TStringList;

implementation

uses
  HTMLParser, FastHTMLParser, HTMLUtil, HTTPSend, SynaCode{$IFDEF WINDOWS}{$IFDEF DOWNLOADER}, IECore{$ENDIF}{$ENDIF};

// ----- TDataProcess -----

constructor TDataProcess.Create;
begin
  inherited Create;
  isFiltered:= FALSE;
  Data      := TStringList.Create;

  Title     := TStringList.Create;
  Link      := TStringList.Create;
  Authors   := TStringList.Create;
  Artists   := TStringList.Create;
  Genres    := TStringList.Create;
  Status    := TStringList.Create;
  Summary   := TStringList.Create;
  JDN       := TList.Create;

  filterMark:= TByteList.Create;
  filterPos := TCardinalList.Create;
end;

destructor  TDataProcess.Destroy;
begin
  filterMark.Free;
  filterPos.Free;

  Title.Free;
  Link.Free;
  Authors.Free;
  Artists.Free;
  Genres.Free;
  Status.Free;
  Summary.Free;
  JDN.Free;

  Data.Free;
  inherited Destroy;
end;

function    TDataProcess.FirstParam(const index: Cardinal): String;
var
  l: Cardinal;
begin
  Result:= '';
  l:= Pos(SEPERATOR, data.Strings[index]);
  if l<>0 then
    Result:= LeftStr(data.Strings[index], l-1);
end;

function    TDataProcess.GetInfo(const index: Cardinal): TStringList;
begin
  GetParams(Result, Data.Strings[index]);
end;

function    TDataProcess.GetParam(const index, paramNo: Cardinal): String;
var
  l: TStringList;
begin
  l:= TStringList.Create;
  GetParams(l, Data.Strings[index]);
  Result:= l.Strings[paramNo];
  l.Free;
end;

// en: break data - for fast filter
procedure   TDataProcess.BreakDataToParts(const i: Cardinal);
var
  l: TStringList;
begin
  l:= TStringList.Create;

  GetParams(l, data.Strings[i]);
  Title.Strings  [i]:= l.Strings[DATA_PARAM_NAME];
  Link.Strings   [i]:= l.Strings[DATA_PARAM_LINK];
  Authors.Strings[i]:= l.Strings[DATA_PARAM_AUTHORS];
  Artists.Strings[i]:= l.Strings[DATA_PARAM_ARTISTS];
  Genres.Strings [i]:= l.Strings[DATA_PARAM_GENRES];
  Status.Strings [i]:= l.Strings[DATA_PARAM_STATUS];
  Summary.Strings[i]:= l.Strings[DATA_PARAM_SUMMARY];
  JDN.Items      [i]:= Pointer(StrToInt(l.Strings[DATA_PARAM_JDN]));

  l.Free;
end;

function   TDataProcess.LoadFromFile(const website: String): Boolean;
var
  i: Cardinal;
  l: TStringList;
  Filename: String;
begin
  Filename:= DATA_FOLDER+website;

  data.Clear;
  filterMark.Clear;
  filterPos .Clear;

  title.Clear;
  authors.Clear;
  artists.Clear;
  genres.Clear;
  status.Clear;
  summary.Clear;
  jdn.Clear;

  if NOT FileExists(Filename+DATA_EXT) then exit(FALSE);
  l:= TStringList.Create;

  self.Filename:= Filename;

  data.LoadFromFile(Filename+DATA_EXT);

  if data.Count > 0 then
  begin
    QuickSortData(data);
 { for i:= 0 to 2 do
    Data.Delete(Data.Count-1);
  SaveToFile;
  Halt;}
    for i:= 0 to data.Count-1 do
    begin

      filterMark.Add(FILTER_SHOW);
      filterPos.Add(i);

      l.Clear;
      GetParams(l, data.Strings[i]);

      title.Add  (l.Strings[DATA_PARAM_NAME]);
      link.Add   (l.Strings[DATA_PARAM_LINK]);
      authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
      artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
      genres.Add (l.Strings[DATA_PARAM_GENRES]);
      status.Add (l.Strings[DATA_PARAM_STATUS]);
      summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
      jdn.Add    (Pointer(StrToInt(l.Strings[DATA_PARAM_JDN])));
    end;
  end;
  l.Free;
  Result:= TRUE;
end;

procedure   TDataProcess.SaveToFile(const website: String);
begin
  if data.Count = 0 then exit;
  QuickSortData(data);
  data.SaveToFile(DATA_FOLDER+website+DATA_EXT);
end;

procedure   TDataProcess.SaveToFile;
begin
  if data.Count = 0 then exit;
  QuickSortData(data);
  data.SaveToFile(Filename+DATA_EXT);
end;

function    TDataProcess.Filter(const checkedGenres, uncheckedGenres: TStringList;
                                const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                                const minusDay: Cardinal;
                                const haveAllChecked, searchNewManga: Boolean): Boolean;
var
 // tmp,
  currentJDN,
  i, j, fpos, count: Cardinal;
  s, s2            : String;
begin
  Result:= FALSE;
  if (filterPos.Count = 0) OR
     (data.Count = 0) OR
     ((stTitle = '') AND
      (stAuthors = '') AND
      (stArtists = '') AND
      (stSummary = '') AND
      (stStatus = '2') AND
      (checkedGenres.Count = 0) AND
      (uncheckedGenres.Count = 0)) AND
      (NOT searchNewManga) then
    exit;

    // ugly filter code but quite fast
    if searchNewManga then
    begin
      currentJDN:= GetCurrentJDN;
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (currentJDN - Cardinal(jdn.Items[fpos]) >= minusDay) AND
           (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos]:= FILTER_HIDE;
      end;
    end;

    if stTitle <> '' then
    begin
      s:= LowerCase(stTitle);
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (Pos(s, LowerCase(Title.Strings[fpos])) = 0) AND
           (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos]:= FILTER_HIDE;
      end;
    end;

    if stAuthors <> '' then
    begin
      s:= LowerCase(stAuthors);
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (Pos(s, LowerCase(Authors.Strings[fpos])) = 0) AND
           (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos]:= FILTER_HIDE;
      end;
    end;

    if stArtists <> '' then
    begin
      s:= LowerCase(stArtists);
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (Pos(s, LowerCase(Artists.Strings[fpos])) = 0) AND
           (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos]:= FILTER_HIDE;
      end;
    end;

    if stSummary <> '' then
    begin
      s:= LowerCase(stSummary);
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (Pos(s, LowerCase(Summary.Strings[fpos])) = 0) AND
           (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos]:= FILTER_HIDE;
      end;
    end;

    if stStatus <> '2' then
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (CompareStr(stStatus, Status.Strings[fpos]) <> 0) AND
           (filterMark.Items[fpos] = FILTER_SHOW) then
          filterMark.Items[fpos]:= FILTER_HIDE;
      end;

    //tmp:= 0;
    if checkedGenres.Count <> 0 then
    begin
      for i:= 0 to checkedGenres.Count-1 do
        checkedGenres.Strings[i]:= LowerCase(checkedGenres.Strings[i]);
      // magical girl lyrical nanoha
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (filterMark.Items[fpos] = FILTER_SHOW) then
        begin
          s:= (LowerCase(Genres.Strings[fpos]));
          //s:= LowerCase(Param[fpos, DATA_PARAM_GENRES]);
          if haveAllChecked then
          begin
            count:= checkedGenres.Count;
            for j:= 0 to checkedGenres.Count-1 do
              if Pos((checkedGenres.Strings[j]+','), s) <> 0 then
                Dec(count);
            if count > 0 then
            begin
             // Inc(tmp);
              filterMark.Items[fpos]:= FILTER_HIDE;
            end;
          end
          else
          begin
            filterMark.Items[fpos]:= FILTER_HIDE;
            for j:= 0 to checkedGenres.Count-1 do
              if Pos((checkedGenres.Strings[j]+','), s) <> 0 then
              begin
                filterMark.Items[fpos]:= FILTER_SHOW;
                break;
              end;
          end;
        end;
      end;
    end;

    if uncheckedGenres.Count <> 0 then
    begin
      for i:= 0 to uncheckedGenres.Count-1 do
        uncheckedGenres.Strings[i]:= LowerCase(uncheckedGenres.Strings[i]);

      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (filterMark.Items[fpos] = FILTER_SHOW) then
        begin
          s:= LowerCase(Genres.Strings[fpos]);
          if haveAllChecked then
          begin
            count:= uncheckedGenres.Count;
            for j:= 0 to uncheckedGenres.Count-1 do
              if Pos((uncheckedGenres.Strings[j]+','), s) = 0 then
                Dec(count);
            if count > 0 then
              filterMark.Items[fpos]:= FILTER_HIDE;
          end
          else
          begin
            for j:= 0 to uncheckedGenres.Count-1 do
              if Pos((uncheckedGenres.Strings[j]+','), s) <> 0 then
              begin
                filterMark.Items[fpos]:= FILTER_HIDE;
                break;
              end;
          end;
        end;
      end;
    end;

  fpos:= filterPos.Count;
  filterPos.Clear;
  for i:= 0 to data.Count - 1 do
    if filterMark.Items[i] = FILTER_SHOW then
      filterPos.Add(i);

  if filterPos.Count <> fpos then
  begin
    isFiltered:= TRUE;
    Result:= TRUE;
  end;
end;

procedure   TDataProcess.RemoveFilter;
var
  i: Cardinal;
begin
  filterMark.Clear;
  filterPos.Clear;
  for i:= 0 to data.Count-1 do
  begin
    filterMark.Add(FILTER_SHOW);
    filterPos.Add(i);
  end;
  isFiltered:= FALSE;
end;

procedure   TDataProcess.Sort;
begin
  QuickSortData(data);
end;

// ----- TMangaInformation -----

constructor TMangaInformation.Create;
begin
  inherited Create;
  parse:= TStringList.Create;
  mangaInfo.chapterName := TStringList.Create;
  mangaInfo.chapterLinks:= TStringList.Create;
  isGetByUpdater:= FALSE;
end;

destructor  TMangaInformation.Destroy;
begin
  mangaInfo.chapterLinks.Free;
  mangaInfo.chapterName .Free;
  parse.Free;
  inherited Destroy;
end;

procedure   TMangaInformation.OnTag(tag: String);
begin
  parse.Add(tag);
end;

procedure   TMangaInformation.OnText(text: String);
begin
  parse.Add(text);
end;

function    TMangaInformation.GetDirectoryPage(var Page: Cardinal;
                                               const website: String): Byte;
var
  s     : String;
  source: TStringList;
  Parser: TjsFastHTMLParser;

  // get directory page from AnimeA
  function   GetAnimeADirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), ANIMEA_ROOT + ANIMEA_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))='http://manga.animea.net/browse.html?page=1') AND
         (Pos('Next', parse.Strings[i+1])>0) then
      begin
        Page:= StrToInt(TrimRight(TrimLeft(parse.Strings[i-4])));
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  // get directory page from KissManga
  function   GetKissMangaDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), KISSMANGA_ROOT + KISSMANGA_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if //(GetTagName(parse.Strings[i]) = 'a') AND
         //(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))='http://manga.animea.net/browse.html?page=1') AND
         (Pos('&raquo; Last', parse.Strings[i])>0) then
      begin
        Page:= StrToInt(StringReplace(TrimRight(TrimLeft(GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'page=')))), '"', '', [rfReplaceAll]));
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  // get directory page from Batoto, because the structure of the site is quiet
  // different from the others, we must do a scan to search for the page
  function   GetBatotoDirectoryPage: Byte;
  var
    myParser: THTMLParser;
    isFoundPage: Boolean = FALSE;
    i: Cardinal;
    s: String;
  begin
   // Page:= batotoLastDirectoryPage;
    while NOT isFoundPage do
    begin
      Inc(Page);
      Result:= INFORMATION_NOT_FOUND;
      if NOT GetPage(TObject(source), BATOTO_ROOT + BATOTO_BROWSER + '?&p=' + IntToStr(Page), 0) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
      isFoundPage:= TRUE;
      parse.Clear;
      myParser:= THTMLParser.Create(PChar(source.Text));
      myParser.OnFoundTag := OnTag;
      myParser.OnFoundText:= OnText;
      myParser.Exec;
      myParser.Free;
      if parse.Count=0 then
      begin
        source.Free;
        exit;
      end;
      for i:= 0 to parse.Count-1 do
      begin
        if (GetTagName(parse.Strings[i]) = 'a') AND
           (Pos('/comic/', parse.Strings[i])>0) then
        begin
          isFoundPage:= FALSE;
          break;
        end;
      end;
    end;
    Dec(Page);
    source.Free;
  end;

  function   GetManga24hDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGA24H_ROOT + MANGA24H_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('Pages (', parse.Strings[i]) > 0) then
      begin
        s:= GetString(parse.Strings[i], 'Pages (', ')');
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetVnSharingDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), VNSHARING_ROOT + VNSHARING_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('&raquo;', parse.Strings[i]) > 0) then
      begin
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'page='));
        SetLength(s, Length(s)-1);
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetHentai2ReadDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), HENTAI2READ_ROOT + HENTAI2READ_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'img') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'alt='))='Next Page') then
      begin
        s:= TrimRight(TrimLeft(parse.Strings[i-5]));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetFakkuDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), FAKKU_ROOT + FAKKU_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title='))='Last Page') then
      begin
        s:= TrimRight(TrimLeft(GetString(parse.Strings[i], '/page/', '"')));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetTruyen18DirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), TRUYEN18_ROOT + TRUYEN18_BROWSER + '/page/1.html', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('/danhsach/page/4.html', parse.Strings[i]) > 0) then
      begin
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+6], 'href='));
        s:= GetString(s, '/page/', '.html');
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetMangaParkDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAPARK_ROOT + MANGAPARK_BROWSER + '1?az', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= parse.Count-2 downto 0 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (i <= parse.Count) AND
         (Pos('...', parse.Strings[i+1])<>0) AND
         (Pos('?az', parse.Strings[i])<>0) then
      begin
        s:= GetString(parse.Strings[i], '/list/', '?az');
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetGEHentaiDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), GEHENTAI_ROOT + '/?page=0' + GEHENTAI_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if Pos('Jump to page: (1-', parse.Strings[i])<>0 then
      begin
        s:= GetString(parse.Strings[i], 'Jump to page: (1-', ')');
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetMangaFoxDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAFOX_ROOT + MANGAFOX_BROWSER + '?az', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= parse.Count-2 downto 0 do
    begin
      if (GetTagName(parse.Strings[i]) = 'span') AND
         (i <= parse.Count) AND
         (Pos('span class="next"', parse.Strings[i])<>0)  then
      begin
        s:= GetString(parse.Strings[i-6], 'href="', '.htm');
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetMangaTradersDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGATRADERS_ROOT + MANGATRADERS_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (i+6 <= parse.Count) AND
         (Pos('page/15/', parse.Strings[i])<>0)  then
      begin
        s:= TrimLeft(TrimRight(parse.Strings[i+5]));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

begin
  source:= TStringList.Create;
  if website = ANIMEA_NAME then
    Result:= GetAnimeADirectoryPage
  else
  if website = KISSMANGA_NAME then
    Result:= GetKissMangaDirectoryPage
  else
  if website = BATOTO_NAME then
    Result:= GetBatotoDirectoryPage
  else
  if website = MANGA24H_NAME then
    Result:= GetManga24hDirectoryPage
  else
  if website = VNSHARING_NAME then
    Result:= GetVnSharingDirectoryPage
  else
  if website = TRUYEN18_NAME then
    Result:= GetTruyen18DirectoryPage
  else
  if website = HENTAI2READ_NAME then
    Result:= GetHentai2ReadDirectoryPage
  else
  if website = FAKKU_NAME then
    Result:= GetFakkuDirectoryPage
  else
  if website = MANGAPARK_NAME then
    Result:= GetMangaParkDirectoryPage
  else
  if website = GEHENTAI_NAME then
    Result:= GetGEHentaiDirectoryPage
  else
  if website = MANGAFOX_NAME then
    Result:= GetMangaFoxDirectoryPage
  else
  if website = MANGATRADERS_NAME then
    Result:= GetMangaTradersDirectoryPage
  else
  begin
    Result:= NO_ERROR;
    Page:= 1;
  end;
end;

function    TMangaInformation.GetNameAndLink(const names, links: TStringList;
                                             const website, URL: String): Byte;
var
  source: TStringList;
  Parser: TjsFastHTMLParser;

  // get name and link of the manga from AnimeA
  function   AnimeAGetNameAndLink: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), ANIMEA_ROOT + ANIMEA_BROWSER + URL, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    for i:= 0 to source.Count-1 do
    begin
      if Pos('manga_img', source.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        links.Add(GetString(source.Strings[i], '"', '"'));
        names.Add(GetString(source.Strings[i], 'title="', ' Manga"'));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaHere
  function   MangaHereGetNameAndLink: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAHERE_ROOT + MANGAHERE_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if Pos('manga_info', parse.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        names.Add(StringFilter(GetString(parse.Strings[i], 'rel="', '" href')));
        links.Add(StringReplace(GetString(parse.Strings[i], 'href="', '">'), MANGAHERE_ROOT, '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaInn
  function   MangaInnGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAINN_ROOT + MANGAINN_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'li') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='mangalistItems') then
      begin
        Result:= NO_ERROR;
        s:= TrimRight(StringFilter(parse.Strings[i+2]));
      //  if s <> 'Hajimete no Aku' then
        begin
          names.Add(s);
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), MANGAINN_ROOT, '', []));
        end;
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from OurManga
  function   OurMangaGetNameAndLink: Byte;
  var
    isGetNameAndLink: Boolean = FALSE;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), OURMANGA_ROOT + OURMANGA_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='m_s_title') then
      begin
        if NOT isGetNameAndLink then
          isGetNameAndLink:= TRUE
        else
        begin
          Result:= NO_ERROR;
          s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        //  if s <> 'Hajimete no Aku' then
          begin
            names.Add(s);
            links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), OURMANGA_ROOT, '', []));
          end;
        end;
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from KissManga
  function   KissMangaGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), KISSMANGA_ROOT + KISSMANGA_BROWSER + '?page=' + IntToStr(StrToInt(URL)+1), 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('/Manga/', parse.Strings[i])>0) AND
         (Pos('title=', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
        s:= StringReplace(s, #13, '', [rfReplaceAll]);
        s:= TrimRight(TrimLeft(s));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), KISSMANGA_ROOT, '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Batoto
  function   BatotoGetNameAndLink: Byte;
  var
    myParser: THTMLParser;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), BATOTO_ROOT + BATOTO_BROWSER + '?&p=' + URL, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    myParser:= THTMLParser.Create(PChar(source.Text));
    myParser.OnFoundTag := OnTag;
    myParser.OnFoundText:= OnText;
    myParser.Exec;
    myParser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('/comic/', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        if (Pos('bloody-rose-r8162', parse.Strings[i]) = 0) AND
           (Pos('dragon-and-weed-origins-outbreak-r6901', parse.Strings[i]) = 0) AND
           (Pos('dragon-and-weed-origins-the-fallen-r8180', parse.Strings[i]) = 0) then
        begin
          names.Add(s);
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), BATOTO_ROOT, '', []));
        end;
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Manga24h
  function   Manga24hGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
    myParser: THTMLParser;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGA24H_ROOT + MANGA24H_BROWSER + IntToStr(StrToInt(URL)+1), 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    source.SaveToFile('test.txt');
    parse.Clear;
    myParser:= THTMLParser.Create(PChar(source.Text));
    myParser.OnFoundTag := OnTag;
    myParser.OnFoundText:= OnText;
    myParser.Exec;
    myParser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('<strong>', parse.Strings[i])<>0) AND
         (Pos('</strong>', parse.Strings[i+2])<>0) AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href=')) <> '') then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(s);
        links.Add('/'+StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href=')), MANGA24H_ROOT, '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from VnSharing
  function   VnSharingGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result:= INFORMATION_NOT_FOUND;
    // bad code
    if NOT GetPage(TObject(source), VNSHARING_ROOT + VNSHARING_BROWSER + '?page='+ IntToStr(StrToInt(URL)+1), 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('/Truyen/', parse.Strings[i])>0) AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'width='))<>'') then
      begin
       { if NOT isGetNameAndLink then
          isGetNameAndLink:= TRUE
        else
        begin }
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
       // if s <> '/Truyen/Tenki-Yohou-no-Koibito?id=506' then
        if s <> '/Truyen/Bakuman-Fantasy-Weirdos?id=6238' then
        begin
          links.Add(s);
          s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Hentai2Read
  function   Hentai2ReadGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), HENTAI2READ_ROOT + HENTAI2READ_BROWSER + URL, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'td') AND
         ((GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Completed') OR
          (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Ongoing')) then
      begin
        begin
          Result:= NO_ERROR;
          s:= TrimLeft(TrimRight(StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'title=')))));
        //  if s <> 'Hajimete no Aku' then
          begin
            names.Add(s);
            links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), HENTAI2READ_ROOT, '', []));
          end;
        end;
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Fakku
  function   FakkuGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    i:= StrToInt(URL);
    if i = 0 then
    begin
      if NOT GetPage(TObject(source), FAKKU_ROOT + FAKKU_BROWSER, 0) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
    end
    else
    begin
      if NOT GetPage(TObject(source), FAKKU_ROOT + FAKKU_BROWSER + '/page/' + IntToStr(i+1), 0) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title='))<>'') AND
         ((Pos('-english', parse.Strings[i])>0) OR
          (Pos('-japanese', parse.Strings[i])>0)) then
        // ((GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Completed') OR
        //  (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Ongoing')) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')))));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), FAKKU_ROOT, '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Truyen18
  function   Truyen18GetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), TRUYEN18_ROOT + TRUYEN18_BROWSER + '/page/'+ IntToStr(StrToInt(URL)+1)+'.html', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('/truyen/', parse.Strings[i])>0) AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i-2], 'class='))='odd') then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        begin
          links.Add(s);
          s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaReader
  function   MangaReaderGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAREADER_ROOT + MANGAREADER_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('<li>', parse.Strings[i])>0) AND
         (Pos('</a>', parse.Strings[i+3])>0) AND
         (Length(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href='))) > 2) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href='));
        links.Add(s);
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        names.Add(HTMLEntitiesFilter(s));
      end;
      if Pos('Network', parse.Strings[i])>0 then
        break;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaPark
  function   MangaParkGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAPARK_ROOT + MANGAPARK_BROWSER + URL + '?az', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('"title"', parse.Strings[i])>0) AND
         (Pos('"_blank"', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        links.Add(s);
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
      end;
    //  if Pos('Network', parse.Strings[i])>0 then
    //    break;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaFox
  function   MangaFoxGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAFOX_ROOT + MANGAFOX_BROWSER + IntToStr(StrToInt(URL)+1) + '.htm?az', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('"manga_text"', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href='));
        links.Add(s);
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+3])));
        names.Add(HTMLEntitiesFilter(s));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaTraders
  function   MangaTradersGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGATRADERS_ROOT + MANGATRADERS_BROWSER + 'All/page/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= parse.Count-1 downto 5 do
    begin
      if (Pos('/manga/series/', parse.Strings[i]) > 0) AND
         (TryStrToInt(GetString(parse.Strings[i], '/manga/series/', '"'), tmp)) AND
         (Pos('<img', parse.Strings[i+1]) = 0) AND
         (Pos('Anything without a category', parse.Strings[i+1]) = 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetString(parse.Strings[i], 'href="', '"');
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from g.e-hentai
  function   GEHentaiGetNameAndLink: Byte;
  var
    pad: Cardinal = 0;
    i  : Cardinal;
    s  : String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), GEHENTAI_ROOT + '/?page=' + URL + GEHENTAI_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('http://g.e-hentai.org/g/', parse.Strings[i])>0) then
      begin
        Inc(pad);
        if pad mod 2 = 0 then
        begin
          Result:= NO_ERROR;
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
          links.Add(s);
          s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
      if Pos('Network', parse.Strings[i])>0 then
        break;
    end;
    source.Free;
    Sleep(250);
  end;

begin
  source:= TStringList.Create;
  if website = ANIMEA_NAME then
    Result:= AnimeAGetNameAndLink
  else
  if website = MANGAHERE_NAME then
    Result:= MangaHereGetNameAndLink
  else
  if website = MANGAINN_NAME then
    Result:= MangaInnGetNameAndLink
  else
  if website = OURMANGA_NAME then
    Result:= OurMangaGetNameAndLink
  else
  if website = KISSMANGA_NAME then
    Result:= KissMangaGetNameAndLink
  else
  if website = BATOTO_NAME then
    Result:= BatotoGetNameAndLink
  else
  if website = MANGA24H_NAME then
    Result:= Manga24hGetNameAndLink
  else
  if website = VNSHARING_NAME then
    Result:= VnSharingGetNameAndLink
  else
  if website = HENTAI2READ_NAME then
    Result:= Hentai2ReadGetNameAndLink
  else
  if website = FAKKU_NAME then
    Result:= FakkuGetNameAndLink
  else
  if website = TRUYEN18_NAME then
    Result:= Truyen18GetNameAndLink
  else
  if website = MANGAREADER_NAME then
    Result:= MangaReaderGetNameAndLink
  else
  if website = MANGAPARK_NAME then
    Result:= MangaParkGetNameAndLink
  else
  if website = MANGAFOX_NAME then
    Result:= MangaFoxGetNameAndLink
  else
  if website = MANGATRADERS_NAME then
    Result:= MangaTradersGetNameAndLink
  else
  if website = GEHENTAI_NAME then
    Result:= GEHentaiGetNameAndLink;
end;

function    TMangaInformation.GetInfoFromURL(const website, URL: String; const Reconnect: Cardinal): Byte;
var
  source: TStringList;
  Parser: TjsFastHTMLParser;
  s     : String;

function   GetAnimeAInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractGenres: Boolean = FALSE;
begin
  mangaInfo.url:= ANIMEA_ROOT + URL + ANIMEA_SKIP;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= ANIMEA_NAME;

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'Manga - Read ', ' Manga Scans');

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='manga_img_big') then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src'));

    // get authors
    if (Pos('Author(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimRight(TrimLeft(parse.Strings[i+3]));

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimRight(TrimLeft(parse.Strings[i+2]));

    // get genres
    if (Pos('Genre(s):', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      isExtractGenres:= TRUE;
    end;
    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) <> 'a' then
        for j:= 0 to 38 do
          if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
      if Pos('</li>', parse.Strings[i]) > 0 then
        isExtractGenres:= FALSE;
    end;

      // get summary
    if (parse.Strings[i]='<p>') then
    begin
      j:= i+1;
      mangaInfo.summary:= '';
      while (j<parse.Count-1) AND (Pos('</p>', parse.Strings[j]) = 0) do
      begin
        mangaInfo.summary:= mangaInfo.summary+parse.Strings[j];
        Inc(j);
      end;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;

      // get chapter name and links
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (GetTagAttribute(parse.Strings[i], 'href=')<>'') AND
       (GetTagAttribute(parse.Strings[i], 'id=')<>'') AND
       (GetTagAttribute(parse.Strings[i], 'title=')<>'') then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href')));
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title'))+' '+parse.Strings[i+3]))));
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

function   GetMangaHereInfoFromURL: Byte;
var
  i, j: Cardinal;
begin
  mangaInfo.url:= MANGAHERE_ROOT + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGAHERE_NAME;

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'Manga - Read ', ' Online at ');

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='img') then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src'));

      // get summary
    if (Pos('id="show"', parse.Strings[i])) <> 0 then
    begin
      parse.Strings[i+1]:= StringFilter(parse.Strings[i+1]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '\n', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '\r', [rfReplaceAll]);
      mangaInfo.summary:= parse.Strings[i+1];
    end;

      // get chapter name and links
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='color_0077') AND
       (Pos('http://www.mangahere.com/manga/', GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')))<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), MANGAHERE_ROOT, '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))));
    end;

    // get authors
    if (Pos('Author(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= parse.Strings[i+3];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+3];

    // get genres
    if (Pos('Genre(s):', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 38 do
        if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i+2]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
      if Pos('Completed', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;

  // Delete 'latest' chapter because it isnt exist
  if (mangaInfo.status = '1') AND (mangainfo.ChapterName.Count > 0) then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.chapterLinks.Delete(mangainfo.chapterLinks.Count-1);
  end;
  Result:= NO_ERROR;
end;

function   GetMangaInnInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractChapters: Boolean = FALSE;
begin
  mangaInfo.url:= MANGAINN_ROOT + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;
  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGAINN_NAME;

  // using parser
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], ' - Read ', ' Online For Free');

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if Pos('/mangas/logos/', parse.Strings[i]) <> 0 then
        mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

      // get summary
    if (Pos('Summary', parse.Strings[i])) <> 0 then
    begin
      j:= i;
      while Pos('</td>', parse.Strings[j]) = 0 do
      begin
        Inc(j);
        if (GetTagName(parse.Strings[j]) = 'span') AND
           (GetTagAttribute(parse.Strings[j], 'class=')<>'') then
        begin
          parse.Strings[j+1]:= StringFilter(parse.Strings[j+1]);
          parse.Strings[j+1]:= StringReplace(parse.Strings[j+1], #10, '\n', [rfReplaceAll]);
          parse.Strings[j+1]:= StringReplace(parse.Strings[j+1], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j+1];
        end;
      end;
    end;

    // get chapter name and links
    if isExtractChapters then
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('http://www.mangainn.com/manga/chapter', parse.Strings[i])<>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(CorrectURL(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), MANGAINN_ROOT, '', [rfReplaceAll])));
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
        parse.Strings[i+2]:= TrimLeft(parse.Strings[i+2]);
        parse.Strings[i+2]:= TrimRight(parse.Strings[i+2]);
        parse.Strings[i+4]:= TrimLeft(parse.Strings[i+4]);
        parse.Strings[i+4]:= TrimRight(parse.Strings[i+4]);
        mangaInfo.chapterName.Add(StringFilter(RemoveSymbols(parse.Strings[i+2] + parse.Strings[i+4])));
      end;

    // get authors
    if (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+4]));

    // get artists
    if (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(parse.Strings[i+4]));

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 38 do
        if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i+4]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
    end;

    // get status
    if (Pos('Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+3])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
      if Pos('Completed', parse.Strings[i+3])<>0 then
        mangaInfo.status:= '0';  // completed
    end;

    if Pos('Chapter Name', parse.Strings[i]) <> 0 then
      if GetTagAttribute(parse.Strings[i-1], 'class=') <> '' then
        isExtractChapters:= TRUE;
  end;
  Result:= NO_ERROR;
end;

function   GetOurMangaInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= OURMANGA_ROOT + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= OURMANGA_NAME;
  mangaInfo.coverLink:= 'http://www.ourmanga.com/images/naruto.jpg';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'Read ', ' Manga Online ');

    // get summary
    if (Pos('Summary:', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+2;
      mangaInfo.summary:= '';
      while Pos('</p>', parse.Strings[j])=0 do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

      // get chapter name and links
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (Pos(URL, parse.Strings[i])<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), OURMANGA_ROOT, '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
     // parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))));
    end;

    // get authors
    if (Pos('Author:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+2]);

    // get artists
    if (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+2]);

    // get genres
    if (Pos('Categories:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) <> 'a' then
        for j:= 0 to 38 do
          if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
      if Pos('</h5>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Completed', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;

  // Delete 'latest' chapter because it isnt exist
  if {(mangaInfo.status = '1') AND }(mangainfo.ChapterName.Count > 0) then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.chapterLinks.Delete(mangainfo.chapterLinks.Count-1);
  end;
  Result:= NO_ERROR;
end;

function   GetKissMangaInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
begin
  mangaInfo.url:= EncodeURL(KISSMANGA_ROOT + URL + '?confirm=yes');
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= KISSMANGA_NAME;

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString('~!@'+parse.Strings[i+1], '~!@', ' manga | ')));

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'width='))='190px') then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src'));

    // get summary
    if (Pos('Summary:', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+4;
      mangaInfo.summary:= '';
      while (Pos('</p>', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get chapter name and links
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (Pos('?id=', parse.Strings[i])<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), KISSMANGA_ROOT, '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))));
    end;

    // get authors
    if (Pos('Author:', parse.Strings[i])<>0) then
      mangaInfo.authors:= parse.Strings[i+4];

    // get artists
    if (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+4];

    // get genres
    if (Pos('Genres:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      s:= parse.Strings[i];
      if (Length(s)>0) AND (s[1]<>'<') then
        for j:= 0 to 38 do
          if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from batoto
function   GetBatotoInfoFromURL: Byte;
var
  count   : Cardinal = 0;
  patchURL,
  s: String;
  isGoOn          : Boolean = FALSE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
  myParser: THTMLParser;
label
  reload;

  function IEUp(const s: String): String;
  begin
    if OptionBatotoUseIEChecked then
      Result:= UpCase(s)
    else
      Result:= s;
  end;

begin
 // patchURL:= UTF8ToANSI(URL);
  patchURL:= URL;
  Insert('comics/', patchURL, 10);
  mangaInfo.url:= BATOTO_ROOT + patchURL;

reload:
  source.Clear;
  {$IFDEF WINDOWS}
  if isGetByUpdater then
  begin
    if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
  end
  else
  if NOT OptionBatotoUseIEChecked then
  begin
    if NOT bttGetPage(TObject(source), mangaInfo.url, Reconnect) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
  end
  else
  begin
    {$IFDEF DOWNLOADER}
    if NOT IEGetPage(TObject(source), mangaInfo.url, Reconnect) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    {$ENDIF}
  end;
  {$ELSE}
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;
  {$ENDIF}

  // parsing the HTML source using our own HTML parser
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.SlowExec;
  Parser.Free;

  {$IFDEF WINDOWS}
  if parse.Count > 0 then
  begin
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('Author:', parse.Strings[i])<>0) then
      begin
        isGoOn:= TRUE;
        break;
      end;
    end;
  end;
  if NOT isGoOn then
  begin
    Inc(count);
    Sleep(3000);
    if count < 16 then
      goto reload;
  end;
  {$ENDIF}

  source.Free;
  mangaInfo.website:= BATOTO_NAME;

  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover link
    if GetTagName(parse.Strings[i]) = IEUp('img') then
    begin
      if (NOT OptionBatotoUseIEChecked) AND (Pos('width:300px', parse.Strings[i-1]) <> 0) then
        mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')))
      else
      if (OptionBatotoUseIEChecked) AND (Pos('WIDTH: 300px', parse.Strings[i-1]) <> 0) then
        mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
    end;

    // get title
    if NOT OptionBatotoUseIEChecked then
    begin
      if (mangaInfo.title = '') AND
         (GetTagName(parse.Strings[i]) = '"og:title"') then
        mangaInfo.title:= StringFilter(GetString(parse.Strings[i], '"og:title" content="', ' - Scanlations'));
    end
    else
    begin
      if (mangaInfo.title = '') AND
         (GetTagName(parse.Strings[i]) = '"og:title"') then
        mangaInfo.title:= StringFilter(GetString(parse.Strings[i], 'META content="', ' - Scanlations'));
    end;

    // get summary
    if (Pos('Description:', parse.Strings[i]) <> 0) then
    begin
      j:= i+3;
      mangaInfo.summary:= '';
      while (Pos(IEUp('</tr>'), parse.Strings[j])=0) AND (j < parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + StringFilter(parse.Strings[j]);
        end;
        Inc(j);
      end;
    end;

      // get chapter name and links (bad code)
    if (NOT OptionBatotoUseIEChecked) AND
       (GetTagName(parse.Strings[i]) = IEUp('a')) AND
       (Pos('/read/_/', parse.Strings[i])<>0) AND
       (i+8 < parse.Count-1) AND
       (Pos('English', parse.Strings[i+8])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), BATOTO_ROOT, '', [rfReplaceAll])));
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2]))));
    end
    else
    if (OptionBatotoUseIEChecked) AND
       (GetTagName(parse.Strings[i]) = IEUp('a')) AND
       (Pos('/read/_/', parse.Strings[i])<>0) AND
       (i+2 < parse.Count-1) AND
       (Pos('English', parse.Strings[i-3])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), BATOTO_ROOT, '', [rfReplaceAll])));
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
      mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i+2])));
    end;

    // get authors
    if (i+5 < parse.Count-1) AND
       (Pos('Author:', parse.Strings[i])<>0) then
       mangaInfo.authors:= TrimLeft(parse.Strings[i+5]);

    // get artists
    if (i+5 < parse.Count-1) AND
       (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+5]);

    // get genres
    if (Pos('Genres:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      s:= parse.Strings[i];
      if s[1] <> '<' then
        for j:= 0 to 38 do
          if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
      if Pos(IEUp('</tr>'), parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if (i+4 < parse.Count-1) AND
         (Pos('Ongoing', parse.Strings[i+4])<>0) then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from Manga24h site
function   GetManga24hInfoFromURL: Byte;
var
 // patchURL,
  s: String;
  i, j: Cardinal;
  isExtractChapters: Boolean = FALSE;
  isExtractSummary : Boolean = FALSE;
begin
 // patchURL:= UTF8ToANSI(URL);
 // Insert('comics/', patchURL, 10);
  mangaInfo.url:= MANGA24H_ROOT + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGA24H_NAME;
  mangaInfo.summary:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover link
    if (Pos('class="img-rounded"', parse.Strings[i]) > 0) then
    begin
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
      s:= mangaInfo.coverLink;
    end;

    // get summary
    if (Pos('"clearfix"', parse.Strings[i])>0) AND
       (Pos('<p>', parse.Strings[i+3])>0) then
    begin
      j:= i+5;
      mangaInfo.summary:= '';
      while (Pos('$(document).ready(function()', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if (Length(s)>0) AND (s[1] <> '<') then
        begin
          parse.Strings[j]:= StringFilter(HTMLEntitiesFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + StringFilter(TrimRight(TrimLeft(parse.Strings[j])));
        end;
        Inc(j);
      end;
    end;

    if (Pos('<tbody>', parse.Strings[i])<>0) AND (NOT isExtractSummary) then
      isExtractChapters:= TRUE;

    if (Pos('</tbody>', parse.Strings[i])<>0) AND (isExtractSummary) then
      isExtractChapters:= FALSE;


      // get chapter name and links
    if (isExtractChapters) AND
       (Pos('<td>', parse.Strings[i])<>0) AND
       (GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')) <> '') AND
       (Pos('</a>', parse.Strings[i+3])<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(CorrectURL('/'+StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), MANGA24H_ROOT, '', [rfReplaceAll])));
      parse.Strings[i+2]:= HTMLEntitiesFilter(parse.Strings[i+2]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
      parse.Strings[i+2]:= TrimLeft(parse.Strings[i+2]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2]))));
    end;

    // get title
    if (Pos('"Truyen tranh ', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.authors:= TrimLeft(StringFilter(GetString(parse.Strings[i+1], '"Truyen tranh ', ',Doc truyen tranh')));

    // get authors
    if (Pos('Tác giả :', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(StringFilter(parse.Strings[i+3]));

    // get artists
    if (Pos('Họa sỹ :', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(StringFilter(parse.Strings[i+3]));

    // get genres
    if (Pos('Thể loại :', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 38 do
        if Pos(LowerCase(Genre[j]), LowerCase(parse.Strings[i+4]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
    end;

    // get status
    if (Pos('Tình Trạng:', parse.Strings[i])<>0) then
    begin
      if Pos('Hoàn Thành', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '0'   // ongoing
      else
        mangaInfo.status:= '1';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from VnSharing site
function   GetVnSharingInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= VNSHARING_ROOT + URL + '&confirm=yes';
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= VNSHARING_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('name="title"', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString(parse.Strings[i], '"Truyện ', ' | Đọc online')));

    // get cover
    if (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('img width="190px" height="250px"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('Sơ lược:', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+4;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

      // get chapter name and links
    if (i+1<parse.Count) AND
       (GetTagName(parse.Strings[i]) = 'a') AND
       (Pos('/Truyen/', parse.Strings[i])>0) AND
       (Pos('title="Đọc', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))));
      parse.Strings[i+1]:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(parse.Strings[i+1])));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Tác giả:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Họa sỹ:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]);

    // get genres
    if (Pos('Thể loại:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) <> 'a' then
        for j:= 0 to 38 do
          if LowerCase(Genre[j]) = LowerCase(parse.Strings[i]) then
            mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Tình trạng:', parse.Strings[i])<>0) then
    begin
      if Pos('Đang tiến hành', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  if mangaInfo.status = '1' then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.ChapterLinks.Delete(mangainfo.ChapterLinks.Count-1);
  end;
  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

function   GetHentai2ReadInfoFromURL: Byte;
var
  s: String;
  isExtractChapters: Boolean = FALSE;
  isExtractGenres  : Boolean = FALSE;
  isExtractSummary : Boolean = TRUE;
  i, j: Cardinal;
begin
  mangaInfo.url:= HENTAI2READ_ROOT + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= HENTAI2READ_NAME;

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get title
    if (mangaInfo.title = '') AND
       (Pos('meta name="description" content="', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i], 'meta name="description" content="', ' hentai chapters');

    // get cover link
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (i<parse.Count-3) then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cover') then
      begin
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
      end;

    // get chapter name and links
    if isExtractChapters then
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND (i < parse.Count-2) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]);
        parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
        parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
        parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
        parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
        s:= RemoveSymbols(parse.Strings[i+1]);
        mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))));
      end
      else
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='right') then
        isExtractChapters:= FALSE;
    end;

    // get summary
    if (Pos('Hentai Summary', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+5;
      mangaInfo.summary:= '';
      while (j<parse.Count) AND (Pos('<div class="box">', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary+parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    if Pos('Hentai Chapters', parse.Strings[i]) > 0 then
      isExtractChapters:= TRUE;

    // get authors
    if (Pos('Author(s):', parse.Strings[i])<>0) AND (i<parse.Count-6) then
      mangaInfo.authors:= parse.Strings[i+5];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) AND (i<parse.Count-6) then
      mangaInfo.artists:= parse.Strings[i+5];

    // get genres
    if (Pos('Genre(s):', parse.Strings[i])<>0) AND (i<parse.Count-6) then
    begin
      mangaInfo.genres:= '';
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) = 'a' then
        mangaInfo.genres:= TrimLeft(TrimRight(mangaInfo.genres+parse.Strings[i+1]))+', '
      else
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='box') then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) AND (i <= parse.Count-5) then
    begin
      if Pos('Ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

function   GetFakkuInfoFromURL: Byte;
var
  s: String;
  isExtractChapters: Boolean = TRUE;
  isExtractGenres  : Boolean = FALSE;
  isExtractSummary : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= FAKKU_ROOT + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= FAKKU_NAME;

  mangaInfo.status:= '0';

  {if Pos('-english', URL) > 0 then
    mangaInfo.genres:= 'English, '
  else
  if Pos('-japanese', URL) > 0 then
    mangaInfo.genres:= 'Japanese, '
  else}
    mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<a href="/manga" title="">', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(parse.Strings[i+5]));

    // get cover
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cover') then
        mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if isExtractSummary then
    begin
      s:= parse.Strings[i];
      if (Length(s) > 0) AND (s[1] <> '<') then
      begin
        parse.Strings[i]:= StringFilter(parse.Strings[i]);
        parse.Strings[i]:= StringReplace(parse.Strings[i], #10, '\n', [rfReplaceAll]);
        parse.Strings[i]:= StringReplace(parse.Strings[i], #13, '\r', [rfReplaceAll]);
        mangaInfo.summary:= mangaInfo.summary+parse.Strings[i]+'\n\r';
      end
      else
      if (GetTagName(parse.Strings[i]) = '/div') then
        isExtractSummary:= FALSE;
    end;

    if Pos('Language:', parse.Strings[i]) <> 0 then
    begin
      if Pos('english', parse.Strings[i+1]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + 'English, '
      else
      if Pos('japanese', parse.Strings[i+1]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + 'Japanese, '
    end;

    if (Pos('Description:', parse.Strings[i])) <> 0 then
    begin
      isExtractSummary:= TRUE;
      mangaInfo.summary:= '';
    end;

    // get chapter name and links
    if isExtractChapters then
    begin
      if (Pos('/read"', parse.Strings[i])>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), FAKKU_ROOT, '', [rfReplaceAll]));
        mangaInfo.chapterName.Add(mangaInfo.title);
        isExtractChapters:= FALSE;
      end ;
    end;

    // get authors
    if (Pos('Series:', parse.Strings[i])<>0) then
    begin
      mangaInfo.authors:= parse.Strings[i+2];
      mangaInfo.genres:= mangaInfo.genres + mangaInfo.authors + ', ';
    end;

    // get artists
    if (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+2];

    // get genres
    if (Pos('Tags:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) = 'a' then
        mangaInfo.genres:= TrimLeft(TrimRight(mangaInfo.genres+parse.Strings[i+1]))+', '
      else
      if (GetTagName(parse.Strings[i]) = '/div') then
        isExtractGenres:= FALSE;
    end;

    // get status - Fakku doesnt have status, in fact, it's always 'completed'
  end;
  Result:= NO_ERROR;
end;

// get manga infos from truyen18 site
function   GetTruyen18InfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= TRUYEN18_ROOT + URL;// + '&confirm=yes';
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= TRUYEN18_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('width="200px"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('Sơ lược:', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+4;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        mangaInfo.summary:= '';
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary+parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

      // get chapter name and links
    if (i+1<parse.Count) AND
       (GetTagName(parse.Strings[i]) = 'a') AND
       (Pos('/doctruyen/', parse.Strings[i])>0) AND
       (Pos('title="Đọc', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))));
      parse.Strings[i+1]:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(HTMLEntitiesFilter(parse.Strings[i+1]));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Tác giả:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Họa sỹ:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]);

    // get genres
    if (Pos('Thể loại:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) <> 'a' then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i])) + ', ';
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Tình trạng:', parse.Strings[i])<>0) then
    begin
      if Pos('Đang tiến hành', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  if mangaInfo.status = '1' then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.ChapterLinks.Delete(mangainfo.ChapterLinks.Count-1);
  end;
  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from mangareader site
function   GetMangaReaderInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= MANGAREADER_ROOT + URL;// + '&confirm=yes';
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGAREADER_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString(parse.Strings[i+1], ' Manga - Read ', ' Online For ')));

    // get cover
    if (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('alt=', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('<h2>', parse.Strings[i]) <> 0) AND
       (Pos('Read ', parse.Strings[i+1]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+4;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

      // allow get chapter name and links
    if (Pos('Chapter Name', parse.Strings[i])>0) AND
       (Pos('leftgap', parse.Strings[i-1])>0) then
      isExtractChapter:= TRUE;

      // get chapter name and links
    if (i+1<parse.Count) AND
       (isExtractChapter) AND
       (Pos('<a href=', parse.Strings[i])>0) AND
       (Pos(' : ', parse.Strings[i+3])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))));
      parse.Strings[i+1]:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1]))) + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(parse.Strings[i+1])));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Author:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]);

    // get genres
    if (Pos('Genre:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('"genretags"', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</tr>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if (Pos('Ongoing', parse.Strings[i+2])<>0) OR
         (Pos('Ongoing', parse.Strings[i+4])<>0) then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
 { if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end; }
  Result:= NO_ERROR;
end;

// get manga infos from mangapark site
function   GetMangaParkInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j, volumeCount: Cardinal;
begin
  volumeCount:= 0;
  mangaInfo.url:= MANGAPARK_ROOT + URL;// + '&confirm=yes';
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGAPARK_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString(parse.Strings[i+1], ' Manga - Read ', ' Online For ')));

    // get cover
    if (GetTagName(parse.Strings[i]) = 'meta') AND
       (Pos('property="og:image"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'content=')));

    // get summary
    if (Pos('<h2>', parse.Strings[i]) <> 0) AND
       (Pos('Summary', parse.Strings[i+1]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+3;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

      // allow get chapter name and links
    if (Pos(URL, parse.Strings[i])>0) AND
       (Pos('target="_blank"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

      // get chapter name and links
    if (isExtractChapter) AND
       (Pos(URL, parse.Strings[i])>0) AND
       (Pos('target="_blank"', parse.Strings[i])>0) AND
       (parse.Strings[i+1] <> '1') AND
       (parse.Strings[i+1] <> '3') AND
       (parse.Strings[i+1] <> '6') AND
       (parse.Strings[i+1] <> '10') AND
       (parse.Strings[i+1] <> 'All'){ AND
       (Pos('Vol.', parse.Strings[i+3]) = 0) }then
    begin
      if Pos('Vol.', parse.Strings[i+3]) <> 0 then
        Inc(volumeCount);
      Inc(mangaInfo.numChapter);
      s:= EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
      Delete(s, Length(s), 1);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3]))) + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+6])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+6]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+6]);

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('/genre/', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</td>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // check and delete duplicate links
  if (volumeCount < mangaInfo.numChapter) AND (volumeCount > 0) then
  begin
    i:= 0;
    while i < mangainfo.ChapterName.Count do
    begin
      if Pos('Vol.', mangainfo.ChapterName.Strings[i]) = 0 then
      begin
        mangainfo.ChapterName.Delete(i);
        mangainfo.ChapterLinks.Delete(i);
        Dec(mangaInfo.numChapter);
      end
      else
        Inc(i);
    end;
  end;
  {while i < mangainfo.ChapterName.Count-1 do
  begin
    j:= 0;
    if Pos('Vol.', mangainfo.ChapterName.Strings[i]) = 0 then
    while j < mangainfo.ChapterName.Count do
    begin
      if i=j then
      begin
        Inc(j);
        continue;
      end;
     // if Pos(GetString(' '+mangainfo.ChapterName.Strings[i]+' ', ' ', ' '), mangainfo.ChapterName.Strings[j]) <> 0 then
      if GetString(mangainfo.ChapterName.Strings[i]+' ', 'Ch.', ' ') =
         GetString(mangainfo.ChapterName.Strings[j]+' ', 'Ch.', ' ') then
      begin
        s:= GetString(' '+mangainfo.ChapterName.Strings[i]+' ', ' ', ' ');
        mangainfo.ChapterName.Delete(j);
        mangainfo.ChapterLinks.Delete(j);
        Dec(mangaInfo.numChapter);
        break;
      end
      else
        Inc(j);
    end;
    Inc(i);
  end;}

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from mangafox site
function   GetMangaFoxInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= URL;// + '&confirm=yes';
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGAFOX_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (Pos('class="cover"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src=')));

    // get summary
    if (Pos('<p class="summary">', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+1;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (Pos('<title>', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i+1], ' Manga - Read ', ' Manga Online for Free')));

      // allow get chapter name and links
    if (Pos('<h3>', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

      // get chapter name and links
    if (isExtractChapter) AND
       (Pos('title="Thanks for Contributing!', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i], 'href="', '/1.html"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+1<parse.Count) AND (Pos('/search/author/', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+1]);

    // get artists
    if (i+1<parse.Count) AND (Pos('/search/artist/', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

    // get genres
    if (Pos('<td valign="top">', parse.Strings[i])<>0) AND
       (Pos('/genres/', parse.Strings[i+2])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('/genres/', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</td>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+5<parse.Count) AND (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from mangatraders site
function   GetMangaTradersInfoFromURL: Byte;
var
  pages           : Cardinal = 1;
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j, k, tmp: Cardinal;

  procedure GetChapterNameAndLink(const apos: Integer);
  begin
    // get chapter name and links
    if (Pos('/view/file/', parse.Strings[apos])>0) AND
       (Pos('"linkFoot', parse.Strings[apos]) = 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[apos], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[apos+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;
  end;

begin
  mangaInfo.url:= MANGATRADERS_ROOT + URL;// + '&confirm=yes';
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  mangaInfo.website:= MANGATRADERS_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then
  begin
    Parser.Free;
    source.Free;
    exit;
  end;
  for i:= 0 to parse.Count-1 do
  begin
    // page counter (>1 = multi-page)
    if (Pos('/page/', parse.Strings[i]) > 0) then
    begin
      s:= parse.Strings[i];
      tmp:= StrToInt(TrimLeft(TrimRight(parse.Strings[i+1])));
      if tmp > pages then
        pages:= tmp;
    end;

    // get cover
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (Pos('"seriesInfo_image"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= MANGATRADERS_ROOT + CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src=')));

    // get summary
    if (Pos('id="summary"', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+9;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i+1]+'~!@', 'Manga Traders - ', '~!@')));

    GetChapterNameAndLink(i);

    // get authors
    if  (i+4<parse.Count) AND (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]);

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('/all', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Scanslation Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;
  if pages > 1 then
  begin
    for k:= 2 to pages do
    begin
      source.Clear;
      if NOT GetPage(TObject(source), MANGATRADERS_ROOT + URL + '/page/' + IntToStr(k) + '/', Reconnect) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
      Parser.Raw:= PChar(source.Text);
      Parser.Exec;
      if parse.Count=0 then
      begin
        Parser.Free;
        source.Free;
        exit;
      end;
     // mangaInfo.chapterLinks.Clear;
     // mangaInfo.chapterName.Clear;
     // mangaInfo.numChapter:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        GetChapterNameAndLink(i);
      end;
    end;
  end;

  // remove duplicate links
  i:= 0;
  while i < mangaInfo.chapterLinks.Count do
  begin
    j:= i+1;
    while j < mangaInfo.chapterLinks.Count do
    begin
      if mangaInfo.chapterLinks.Strings[i] = mangaInfo.chapterLinks.Strings[j] then
      begin
        mangaInfo.chapterLinks.Delete(j);
        mangaInfo.chapterName.Delete(j);
        Dec(mangaInfo.numChapter);
      end
      else
        Inc(j);
    end;
    Inc(i);
  end;

  Parser.Free;
  source.Free;
  Result:= NO_ERROR;
end;

// get manga infos from g.e-hentai site (dummy)
function   GetGEHentaiInfoFromURL_Dummy: Byte;
begin
  mangaInfo.url:= URL;
  source.Free;
  mangaInfo.website:= GEHENTAI_NAME;
  mangaInfo.title:= mangaInfo.title;
  mangaInfo.chapterLinks.Add(URL);
  mangaInfo.chapterName.Add(mangaInfo.title);
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.numChapter:= 0;
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  mangaInfo.status:= '0';
  Result:= NO_ERROR;
end;

// get manga infos from g.e-hentai site
function   GetGEHentaiInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;

  mangaInfo.website:= GEHENTAI_NAME;
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.numChapter:= 0;
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  mangaInfo.status:= '0';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get title and cover
    if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'id=')) = 'gd1') then
    begin
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));
      s:= GetString(parse.Strings[i+1], ' Gallery: ', '"');
      if s <> '' then
        mangaInfo.title:= StringFilter(s);
    end;
  end;

  mangaInfo.chapterLinks.Add(URL);
  mangaInfo.chapterName.Add(mangaInfo.title);
  Result:= NO_ERROR;
  Sleep(250);
end;

begin
  source:= TStringList.Create;
  mangaInfo.coverLink := '';
  mangaInfo.numChapter:= 0;
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;

  if website = ANIMEA_NAME then
    Result:= GetAnimeAInfoFromURL
  else
  if website = MANGAHERE_NAME then
    Result:= GetMangaHereInfoFromURL
  else
  if website = MANGAINN_NAME then
    Result:= GetMangaInnInfoFromURL
  else
  if website = OURMANGA_NAME then
    Result:= GetOurMangaInfoFromURL
  else
  if website = KISSMANGA_NAME then
    Result:= GetKissMangaInfoFromURL
  else
  if website = BATOTO_NAME then
    Result:= GetBatotoInfoFromURL
  else
  if website = MANGA24H_NAME then
    Result:= GetManga24hInfoFromURL
  else
  if website = VNSHARING_NAME then
    Result:= GetVnSharingInfoFromURL
  else
  if website = HENTAI2READ_NAME then
    Result:= GetHentai2ReadInfoFromURL
  else
  if website = FAKKU_NAME then
    Result:= GetFakkuInfoFromURL
  else
  if website = TRUYEN18_NAME then
    Result:= GetTruyen18InfoFromURL
  else
  if website = MANGAREADER_NAME then
    Result:= GetMangaReaderInfoFromURL
  else
  if website = MANGAPARK_NAME then
    Result:= GetMangaParkInfoFromURL
  else
  if website = MANGAFOX_NAME then
    Result:= GetMangaFoxInfoFromURL
  else
  if website = MANGATRADERS_NAME then
    Result:= GetMangaTradersInfoFromURL
  else
  if website = GEHENTAI_NAME then
  begin
    case isGetByUpdater of
      TRUE:  Result:= GetGEHentaiInfoFromURL_Dummy;
      FALSE: Result:= GetGEHentaiInfoFromURL;
    end;
  end;
end;

procedure   TMangaInformation.SyncInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
begin
  // sync info to data
  DataProcess.Data.Strings[index]:= SetParams(
            [DataProcess.Param[index, DATA_PARAM_NAME],
             DataProcess.Param[index, DATA_PARAM_LINK],
             mangaInfo.authors,
             mangaInfo.artists,
             mangaInfo.genres,
             mangaInfo.status,
             StringFilter(mangaInfo.summary),
             IntToStr(mangaInfo.numChapter),
             DataProcess.Param[index, DATA_PARAM_JDN],
            '0']);
  // then break it into parts
  dataProcess.BreakDataToParts(index);
end;

procedure   TMangaInformation.AddInfoToDataWithoutBreak(const name, link : String;
                                                        const DataProcess: TDataProcess);
begin
 { DataProcess.Data.Add(RemoveStringBreaks(
                       name+SEPERATOR+
                       link+SEPERATOR+
                       mangaInfo.authors   +SEPERATOR+
                       mangaInfo.artists   +SEPERATOR+
                       mangaInfo.genres    +SEPERATOR+
                       mangaInfo.status    +SEPERATOR+
                       mangaInfo.summary   +SEPERATOR+
                       IntToStr(mangaInfo.numChapter) +SEPERATOR+
                       IntToStr(GetCurrentJDN)+SEPERATOR+
                       '0'    +SEPERATOR));  }
  DataProcess.Data.Add(
             RemoveStringBreaks(
             SetParams(
             [name,
             link,
             mangaInfo.authors,
             mangaInfo.artists,
             mangaInfo.genres,
             mangaInfo.status,
             StringFilter(mangaInfo.summary),
             IntToStr(mangaInfo.numChapter),
             IntToStr(GetCurrentJDN),
            '0'])));
end;

procedure   TMangaInformation.AddInfoToData(const name, link : String;
                                            const DataProcess: TDataProcess);
var
  l: TStringList;
begin
  l:= TStringList.Create;
  DataProcess.Data.Add(
             RemoveStringBreaks(
             SetParams(
             [name,
             link,
             mangaInfo.authors,
             mangaInfo.artists,
             mangaInfo.genres,
             mangaInfo.status,
             StringFilter(mangaInfo.summary),
             IntToStr(mangaInfo.numChapter),
             IntToStr(GetCurrentJDN),
            '0'])));
  GetParams(l, DataProcess.Data.Strings[DataProcess.Data.Count-1]);
  DataProcess.title.Add  (l.Strings[DATA_PARAM_NAME]);
  DataProcess.link.Add   (l.Strings[DATA_PARAM_LINK]);
  DataProcess.authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
  DataProcess.artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
  DataProcess.genres.Add (l.Strings[DATA_PARAM_GENRES]);
  DataProcess.status.Add (l.Strings[DATA_PARAM_STATUS]);
  DataProcess.summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
  DataProcess.jdn.Add    (Pointer(StrToInt(l.Strings[DATA_PARAM_JDN])));
  l.Free;
end;

end.

