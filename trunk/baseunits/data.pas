{
        File: data.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
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
    isFilterAllSites,
    isFiltered: Boolean;

    site,
    filterMark: TByteList;
    // used by search
    searchPos,
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
    function    LoadFromAllFiles(const websiteList: TStringList): Boolean;
    procedure   SaveToFile(const website: String); overload;
    procedure   SaveToFile; overload;

    function    CanFilter(const checkedGenres, uncheckedGenres: TStringList;
                          const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                          const minusDay: Cardinal;
                          const haveAllChecked, searchNewManga: Boolean): Boolean;

    // en: Filter by genres, title, authors, ...
    // vi: Filter theo genre, tên, tác giả, ...
    function    Filter(const checkedGenres, uncheckedGenres: TStringList;
                       const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                       const minusDay: Cardinal;
                       const haveAllChecked, searchNewManga: Boolean): Boolean;
    // realtime search
    function    Search(AMangaName: String): Boolean;
    // get data position
    function    GetPos(const ANodePos: Integer): Integer;

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
    procedure   ClearInfo;
    function    GetDirectoryPage(var Page: Cardinal;
                                 const website: String): Byte;
    function    GetNameAndLink(const names, links: TStringList;
                               const website, URL: String): Byte;
    function    GetInfoFromURL(const website, URL: String; const Reconnect: Cardinal): Byte;
    procedure   SyncInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
    procedure   SyncMinorInfoToData(const DataProcess: TDataProcess; const index: Cardinal);

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
  synautil, HTMLParser, FastHTMLParser, LConvEncoding,
  HTMLUtil, HTTPSend, SynaCode{$IFDEF WINDOWS}{$IFDEF DOWNLOADER}, IECore{$ENDIF}{$ENDIF};

// ----- TDataProcess -----

constructor TDataProcess.Create;
begin
  inherited Create;
  isFilterAllSites:= FALSE;
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

  site:= TByteList.Create;
  filterMark:= TByteList.Create;
  filterPos := TCardinalList.Create;
  searchPos := TCardinalList.Create;
end;

destructor  TDataProcess.Destroy;
begin
  searchPos.Free;
  filterMark.Free;
  filterPos.Free;
  site.Free;

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
  id,
  i : Cardinal;
  l : TStringList;
  Filename: String;
begin
  Filename:= DATA_FOLDER+website;

  data.Clear;
  searchPos.Clear;
  filterMark.Clear;
  filterPos .Clear;
  site.Clear;

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
  id:= GetMangaSiteID(website);

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
      site.Add(id);

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

// TODO: load from all files - this function is for "Filter all sites"
function   TDataProcess.LoadFromAllFiles(const websiteList: TStringList): Boolean;
var
  id,
  j,
  i : Cardinal;
  l : TStringList;
  Filename: String;
begin
  if websiteList.Count = 0 then
    exit(FALSE);
  data.Clear;
  searchPos.Clear;
  filterMark.Clear;
  filterPos .Clear;
  site.Clear;

  title.Clear;
  authors.Clear;
  artists.Clear;
  genres.Clear;
  status.Clear;
  summary.Clear;
  jdn.Clear;

  l:= TStringList.Create;

  for i:= 0 to websiteList.Count-1 do
  begin
    Filename:= DATA_FOLDER+websiteList.Strings[i];
    id:= GetMangaSiteID(websiteList.Strings[i]);
    if NOT FileExists(Filename+DATA_EXT) then continue;
    l.Clear;
    l.LoadFromFile(Filename+DATA_EXT);

    if l.Count <> 0 then
    begin
      for j:= 0 to l.Count-1 do
      begin
        site.Add(id);
      end;
      data.Text:= data.Text + l.Text;
    end;
  end;

  if data.Count > 0 then
  begin
    QuickSortDataWithWebID(data, site);
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

// check if we need to filter or not
function    TDataProcess.CanFilter(const checkedGenres, uncheckedGenres: TStringList;
                                   const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                                   const minusDay: Cardinal;
                                   const haveAllChecked, searchNewManga: Boolean): Boolean;
begin
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
    Result:= FALSE
  else
    Result:= TRUE;
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
  searchPos.Clear;
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

function    TDataProcess.Search(AMangaName: String): Boolean;
var
  i: Cardinal;
begin
  searchPos.Clear;
  if filterPos.Count <= 0 then exit;
  AMangaName:= Upcase(AMangaName);
  for i:= 0 to filterPos.Count-1 do
  begin
    if Pos(AMangaName, Upcase(Title.Strings[filterPos.Items[i]])) > 0 then
    begin
      searchPos.Add(filterPos.Items[i]);
    end;
  end;
end;

// get data position
function    TDataProcess.GetPos(const ANodePos: Integer): Integer;
begin
  if searchPos.Count = 0 then
    Result:= filterPos.Items[ANodePos]
  else
    Result:= searchPos.Items[ANodePos];
end;

procedure   TDataProcess.RemoveFilter;
var
  i: Cardinal;
begin
  searchPos.Clear;
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
  ClearInfo;
  mangaInfo.chapterLinks.Free;
  mangaInfo.chapterName .Free;
  parse.Free;
  inherited Destroy;
end;

procedure   TMangaInformation.ClearInfo;
begin
  mangaInfo.artists:= '';
  mangaInfo.authors:= '';
  mangaInfo.genres := '';
  mangaInfo.summary:= '';
  mangaInfo.coverLink:= '';
  mangaInfo.numChapter:= 0;
  mangaInfo.status := '';
  mangaInfo.title  := '';
  mangaInfo.url    := '';
  mangaInfo.website:= '';
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
    if NOT GetPage(TObject(source), WebsiteRoots[ANIMEA_ID,1] + ANIMEA_BROWSER, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[KISSMANGA_ID,1] + KISSMANGA_BROWSER, 0) then
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
   { while NOT isFoundPage do
    begin
      Inc(Page);
      Result:= INFORMATION_NOT_FOUND;
      if NOT GetPage(TObject(source), WebsiteRoots[BATOTO_ID,1] + BATOTO_BROWSER + 'http://www.batoto.net/comic/_/comics/?per_page=750&st=%0' + IntToStr(Page*750), 0) then
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
    source.Free; }

    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[BATOTO_ID,1] + '/comic/_/comics/?per_page=750&st=0', 0) then
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
      if (Pos('Page 1 of ', parse.Strings[i]) > 0) then
      begin
        s:= GetString(parse.Strings[i]+'~!@', 'Page 1 of ', '~!@');
        Page:= StrToInt(TrimLeft(TrimRight(s)));
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetManga24hDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGA24H_ID,1] + MANGA24H_BROWSER, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[VNSHARING_ID,1] + VNSHARING_BROWSER, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[FAKKU_ID,1] + FAKKU_BROWSER, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAPARK_ID,1] + MANGAPARK_BROWSER + '1?az', 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[GEHENTAI_ID,1] + '/?page=0' + GEHENTAI_BROWSER, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAFOX_ID,1] + MANGAFOX_BROWSER + '?az', 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGATRADERS_ID,1] + MANGATRADERS_BROWSER, 0) then
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

  function   GetMangaEdenDirectoryPage(const root: String): Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), root + MANGAEDEN_BROWSER, 0) then
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
      if (GetTagName(parse.Strings[i]) = 'span') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='next') then
      begin
        s:= TrimRight(TrimLeft(parse.Strings[i-4]));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetBlogTruyenDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[BLOGTRUYEN_ID,1] + BLOGTRUYEN_BROWSER, 0) then
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
      if (Pos('[cuối]', parse.Strings[i]) > 0) then
      begin
        s:= TrimRight(TrimLeft(GetString(parse.Strings[i-1], 'trang(', ')')));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetRedHawkScansDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[REDHAWKSCANS_ID,1] + REDHAWKSCANS_BROWSER, 0) then
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
      if (Pos('Last ', parse.Strings[i]) > 0) then
      begin
        s:= TrimRight(TrimLeft(GetString(parse.Strings[i-1], '/list/', '/"')));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetS2scanDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[S2SCAN_ID,1] + S2SCAN_BROWSER, 0) then
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
      if (Pos('"gbutton fright"', parse.Strings[i]) > 0) then
      begin
        s:= TrimRight(TrimLeft(GetString(parse.Strings[i], '/list/', '/"')));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;

  function   GetMangaAeDirectoryPage: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAAE_ID,1] + MANGAAE_BROWSER, 0) then
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
    for i:= parse.Count-1 downto 2 do
    begin
      if (Pos('<li class=''active''>', parse.Strings[i]) > 0) then
      begin
        s:= TrimRight(TrimLeft(parse.Strings[i+10]));
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
  if website = MANGAEDEN_NAME then
    Result:= GetMangaEdenDirectoryPage(WebsiteRoots[MANGAEDEN_ID,1])
  else
  if website = PERVEDEN_NAME then
    Result:= GetMangaEdenDirectoryPage(WebsiteRoots[PERVEDEN_ID,1])
  else
  if website = BLOGTRUYEN_NAME then
    Result:= GetBlogTruyenDirectoryPage
  else
  if website = REDHAWKSCANS_NAME then
    Result:= GetRedHawkScansDirectoryPage
  else
  if website = S2SCAN_NAME then
    Result:= GetS2scanDirectoryPage
  else
  if website = MANGAAE_NAME then
    Result:= GetMangaAeDirectoryPage
  else
  if website = EATMANGA_NAME then
  begin
    Result:= NO_ERROR;
    Page:= 1;
  end
  else
  if website = TRUYEN18_NAME then
    Result:= GetTruyen18DirectoryPage
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
    if NOT GetPage(TObject(source), WebsiteRoots[ANIMEA_ID,1] + ANIMEA_BROWSER + URL, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAHERE_ID,1] + MANGAHERE_BROWSER, 0) then
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
        links.Add(StringReplace(GetString(parse.Strings[i], 'href="', '">'), WebsiteRoots[MANGAHERE_ID,1], '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Es.MangaHere
  function   EsMangaHereGetNameAndLink: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[ESMANGAHERE_ID,1] + ESMANGAHERE_BROWSER, 0) then
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
        links.Add(StringReplace(GetString(parse.Strings[i], 'href="', '">'), WebsiteRoots[ESMANGAHERE_ID,1], '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from AnimeExtremist
  function   AnimeExtremistGetNameAndLink: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[ANIMEEXTREMIST_ID,1] + ANIMEEXTREMIST_BROWSER, 0) then
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
      if Pos('id="manga" style="margin', parse.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        names.Add(TrimLeft(TrimRight(parse.Strings[i+4])));
        links.Add(StringReplace(GetString(parse.Strings[i+3], 'href="', '">'), WebsiteRoots[ANIMEEXTREMIST_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAINN_ID,1] + MANGAINN_BROWSER, 0) then
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
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGAINN_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[OURMANGA_ID,1] + OURMANGA_BROWSER, 0) then
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
            links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[OURMANGA_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[KISSMANGA_ID,1] + KISSMANGA_BROWSER + '?page=' + IntToStr(StrToInt(URL)+1), 0) then
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
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[KISSMANGA_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[BATOTO_ID,1] + '/comic/_/comics/?per_page=750&st=' + IntToStr(StrToInt(URL)*750), 0) then
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
         (Pos('/comic/', parse.Strings[i])>0) AND
         (Pos('/comics/''', parse.Strings[i])=0) AND
         (Pos('/comics/"', parse.Strings[i])=0) AND
         (Pos('/comics/?', parse.Strings[i])=0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        if (Pos('bloody-rose-r8162', parse.Strings[i]) = 0) AND
           (Pos('dragon-and-weed-origins-outbreak-r6901', parse.Strings[i]) = 0) AND
           (Pos('dragon-and-weed-origins-the-fallen-r8180', parse.Strings[i]) = 0) then
        begin
          names.Add(s);
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
          links.Add(StringReplace(s, WebsiteRoots[BATOTO_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGA24H_ID,1] + MANGA24H_BROWSER + IntToStr(StrToInt(URL)+1), 0) then
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
      if (Pos('<strong>', parse.Strings[i])<>0) AND
         (Pos('</strong>', parse.Strings[i+2])<>0) AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href=')) <> '') then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(s);
        links.Add('/'+StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href=')), WebsiteRoots[MANGA24H_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[VNSHARING_ID,1] + VNSHARING_BROWSER + '?page='+ IntToStr(StrToInt(URL)+1), 0) then
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
    if NOT GetPage(TObject(source), HENTAI2READ_ROOT + HENTAI2READ_BROWSER + IntToStr(StrToInt(URL)+1) + '/', 0) then
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
      if Pos('class="lst-anm-ifo"', parse.Strings[i])>0 then
      begin
        begin
          Result:= NO_ERROR;
          s:= TrimLeft(TrimRight(StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'title=')))));
          names.Add(s);
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'href=')), HENTAI2READ_ROOT, '', []));
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
      if NOT GetPage(TObject(source), WebsiteRoots[FAKKU_ID,1] + FAKKU_BROWSER, 0) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
    end
    else
    begin
      if NOT GetPage(TObject(source), WebsiteRoots[FAKKU_ID,1] + FAKKU_BROWSER + '/page/' + IntToStr(i+1), 0) then
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
          (Pos('-japanese', parse.Strings[i])>0) OR
          (Pos('class="content-title"', parse.Strings[i])>0)) then
        // ((GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Completed') OR
        //  (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Ongoing')) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')))));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[FAKKU_ID,1], '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAREADER_ID,1] + MANGAREADER_BROWSER, 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAPARK_ID,1] + MANGAPARK_BROWSER + URL + '?az', 0) then
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAFOX_ID,1] + MANGAFOX_BROWSER + IntToStr(StrToInt(URL)+1) + '.htm?az', 0) then
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
        s:= StringReplace(s, WebsiteRoots[MANGAFOX_ID,1], '', []);
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
    if NOT GetPage(TObject(source), WebsiteRoots[MANGATRADERS_ID,1] + MANGATRADERS_BROWSER + 'All/page/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
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

  // get name and link of the manga from TruyenTranhTuan
  function   TruyenTranhTuanGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[TRUYENTRANHTUAN_ID,1] + TRUYENTRANHTUAN_BROWSER, 0) then
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
      if (Pos('class="ch-subject"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from SubManga
  function   SubMangaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[SUBMANGA_ID,1] + SUBMANGA_BROWSER, 0) then
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
      if (Pos('class="xs"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+3])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href='));
        links.Add(StringReplace(s, WebsiteRoots[SUBMANGA_ID,1], '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Komikid
  function   KomikidGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[KOMIKID_ID,1], 0) then
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
      if (Pos('<option value="', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from PecintaKomik
  function   PecintaKomikGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[PECINTAKOMIK_ID,1] + PECINTAKOMIK_BROWSER, 0) then
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
      if (Pos('class=''screenshot''', parse.Strings[i]) > 0) OR
         (Pos('class="screenshot"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        if s[Length(s)] <> '/' then
          s:= s+ '/';
        links.Add(s);
      end;
      if (Pos('/manga/', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        if s[Length(s)] <> '/' then
          s:= s+ '/';
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Mabuns
  function   MabunsGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MABUNS_ID,1] + MABUNS_BROWSER, 0) then
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
      if (Pos('class="manga"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+3])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+6], 'href=')), WebsiteRoots[MABUNS_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Mabuns
  function   MangaEstaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAESTA_ID,1] + MANGAESTA_BROWSER, 0) then
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
      if (Pos('class="gallery-icon"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'title=')))));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href=')), WebsiteRoots[MANGAESTA_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from HugeManga
  function   HugeMangaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[HUGEMANGA_ID,1] + HUGEMANGA_BROWSER, 0) then
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
      if (Pos('option value="', parse.Strings[i]) > 0) AND
         (Pos('value="0"', parse.Strings[i]) = 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(parse.Strings[i+1]);
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaAr
  function   MangaArGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAAR_ID,1] + MANGAAR_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;

    // convert charset
    source.text:= CP1256ToUTF8(source.text);

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
      if (Pos('style="float', parse.Strings[i]) > 0) AND
         (Pos('"http://manga-ar', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAAR_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaAe
  function   MangaAeGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAAE_ID,1] + MANGAAE_BROWSER + IntToStr(StrToInt(URL)+1), 0) then
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
      if (Pos('class="imgf"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+3])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-2], 'href=')), WebsiteRoots[MANGAAE_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from CentralDeMangas
  function   CentralDeMangasGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[CENTRALDEMANGAS_ID,1] + CENTRALDEMANGAS_BROWSER, 0) then
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
      if (Pos('/online/', parse.Strings[i]) > 0) AND
         (Pos('title="', parse.Strings[i]) > 0) AND
         (Pos('<td>', parse.Strings[i-1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(parse.Strings[i+1]);
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetString(parse.Strings[i], 'href="', '"'), WebsiteRoots[CENTRALDEMANGAS_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from imanhua
  function   imanhuaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[IMANHUA_ID,1] + IMANHUA_BROWSER, 0) then
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
      if (Pos('href="/comic/', parse.Strings[i]) > 0) AND
         (Pos('/list_', parse.Strings[i]) = 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(parse.Strings[i+1]);
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Turkcraft
  function   TurkcraftGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[TURKCRAFT_ID,1] + TURKCRAFT_BROWSER, 0) then
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
      if (Pos('<option value="', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaVadisi
  function   MangaVadisiGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAVADISI_ID,1] + MANGAVADISI_BROWSER, 0) then
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
      if (Pos('<option value="', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Mangaframe
  function   MangaframeGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAFRAME_ID,1] + MANGAFRAME_BROWSER, 0) then
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
      if (Pos('class="title"', parse.Strings[i]) > 0) AND
         (Pos('class="group"', parse.Strings[i-1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'title=')));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGAFRAME_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from SenManga
  function   SenMangaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[SENMANGA_ID,1] + SENMANGA_BROWSER, 0) then
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
      if (Pos('<br/', parse.Strings[i]) > 0) AND
         (Pos('</a', parse.Strings[i-1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(parse.Strings[i-2]);
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-3], 'href=')), WebsiteRoots[SENMANGA_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from Starkana
  function   StarkanaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[STARKANA_ID,1] + STARKANA_BROWSER, 0) then
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
      if (Pos('style="float:right;', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+3])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href="'));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from EatManga
  function   EatMangaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[EATMANGA_ID,1] + EATMANGA_BROWSER, 0) then
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
      if (Pos(' Online">', parse.Strings[i]) > 0) AND
         (Pos('<th>', parse.Strings[i-1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href="'));
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaPanda
  function   MangaPandaGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
    isExtractInfo: Boolean = FALSE;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGAPANDA_ROOT + MANGAPANDA_BROWSER, 0) then
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
      if (NOT isExtractInfo) AND (Pos('ul class="series_alpha"', parse.Strings[i]) > 0) then
        isExtractInfo:= TRUE;
      if (isExtractInfo) AND
         (Pos('<li>', parse.Strings[i]) > 0) AND
         (Pos('<a', parse.Strings[i+1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href="'));
        links.Add(s);
      end
      else
      if (isExtractInfo) AND
         (Pos('div id="wrapper_footer"', parse.Strings[i]) > 0) then
        break;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaStream
  function   MangaStreamGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), MANGASTREAM_ROOT + MANGASTREAM_BROWSER, 0) then
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
      if (Pos('/manga/', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href="'));
        links.Add(StringReplace(s, MANGASTREAM_ROOT, '', []));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from RedHawkScans
  function   RedHawkScansGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[REDHAWKSCANS_ID,1] + REDHAWKSCANS_BROWSER + '/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
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
      if (Pos('class="title"', parse.Strings[i]) > 0) AND
         (Pos('<a', parse.Strings[i+1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href="')), WebsiteRoots[REDHAWKSCANS_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from S2scan
  function   S2scanGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[S2SCAN_ID,1] + S2SCAN_BROWSER + '/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
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
      if (Pos('class="title"', parse.Strings[i]) > 0) AND
         (Pos('<a', parse.Strings[i+1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href="')), WebsiteRoots[S2SCAN_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from S2scan
  function   EGScansGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[EGSCANS_ID,1] + EGSCANS_BROWSER + '/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
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
    for i:= parse.Count-1 downto 2 do
    begin
      if (Pos('<option value="', parse.Strings[i]) > 0) AND
         (Pos('="0"', parse.Strings[i]) = 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value="')), WebsiteRoots[S2SCAN_ID,1], '', []);
        links.Add(s);
      end;
      if Pos('<select name="manga"', parse.Strings[i]) > 0 then
        break;
    end;
    source.Free;
  end;

  // get name and link of the manga from blogtruyen
  function   BlogTruyenGetNameAndLink: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
    stream: TStringStream;
  begin
    Result:= INFORMATION_NOT_FOUND;
    stream:= TStringStream.Create('');
    s:= WebsiteRoots[BLOGTRUYEN_ID,1] + BLOGTRUYEN_JS_BROWSER;
    s:= BLOGTRUYEN_POST_FORM + IntToStr(StrToInt(URL)+1);
    while NOT HttpPostURL(WebsiteRoots[BLOGTRUYEN_ID,1] + BLOGTRUYEN_JS_BROWSER, BLOGTRUYEN_POST_FORM + IntToStr(StrToInt(URL)+1), stream) do ;
    source.Text:= stream.DataString;
    stream.Free;
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
      if (Pos('onmouseover="TagToTip', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(GetString(parse.Strings[i], 'title="đọc truyện ', '" onmouseover'))));
        names.Add(HTMLEntitiesFilter(s));
        links.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href="')));
      end;
    end;
    source.Free;
  end;

  // get name and link of the manga from MangaEden
  function   MangaEdenGetNameAndLink(const root: String): Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), root + MANGAEDEN_BROWSER + '?page=' + IntToStr(StrToInt(URL)+1), 0) then
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
      if ((Pos('class="openManga"', parse.Strings[i])>0) OR
          (Pos('class="closedManga"', parse.Strings[i])>0)) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+1])));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), root, '', []));
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
    if NOT GetPage(TObject(source), WebsiteRoots[GEHENTAI_ID,1] + '/?page=' + URL + GEHENTAI_BROWSER, 0) then
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
 { if website = OURMANGA_NAME then
    Result:= OurMangaGetNameAndLink
  else }
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
  if website = STARKANA_NAME then
    Result:= StarkanaGetNameAndLink
  else
  if website = EATMANGA_NAME then
    Result:= EatMangaGetNameAndLink
  else
  if website = MANGAPANDA_NAME then
    Result:= MangaPandaGetNameAndLink
  else
  if website = MANGASTREAM_NAME then
    Result:= MangaStreamGetNameAndLink
  else
  if website = REDHAWKSCANS_NAME then
    Result:= RedHawkScansGetNameAndLink
  else
  if website = S2SCAN_NAME then
    Result:= S2ScanGetNameAndLink
  else
  if website = EGSCANS_NAME then
    Result:= EGScansGetNameAndLink
  else
  if website = MANGAEDEN_NAME then
    Result:= MangaEdenGetNameAndLink(WebsiteRoots[MANGAEDEN_ID,1])
  else
  if website = PERVEDEN_NAME then
    Result:= MangaEdenGetNameAndLink(WebsiteRoots[PERVEDEN_ID,1])
  else
  if website = BLOGTRUYEN_NAME then
    Result:= BlogTruyenGetNameAndLink
  else
  if website = TRUYENTRANHTUAN_NAME then
    Result:= TruyenTranhTuanGetNameAndLink
  else
  if website = SUBMANGA_NAME then
    Result:= SubMangaGetNameAndLink
  else
  if website = ESMANGAHERE_NAME then
    Result:= EsMangaHereGetNameAndLink
  else
  if website = ANIMEEXTREMIST_NAME then
    Result:= AnimeExtremistGetNameAndLink
  else
  if website = KOMIKID_NAME then
    Result:= KomikidGetNameAndLink
  else
  if website = PECINTAKOMIK_NAME then
    Result:= PecintaKomikGetNameAndLink
  else
  if website = MABUNS_NAME then
    Result:= MabunsGetNameAndLink
  else
  if website = MANGAESTA_NAME then
    Result:= MangaEstaGetNameAndLink
  else
  if website = HUGEMANGA_NAME then
    Result:= HugeMangaGetNameAndLink
  else
  if website = MANGAAR_NAME then
    Result:= MangaArGetNameAndLink
  else
  if website = MANGAAE_NAME then
    Result:= MangaAeGetNameAndLink
  else
  if website = CENTRALDEMANGAS_NAME then
    Result:= CentralDeMangasGetNameAndLink
 // else
 // if website = MANGAKU_NAME then
 //   Result:= MangakuGetNameAndLink
  else
  if website = IMANHUA_NAME then
    Result:= imanhuaGetNameAndLink
  else
  if website = TURKCRAFT_NAME then
    Result:= TurkcraftGetNameAndLink
  else
  if website = MANGAVADISI_NAME then
    Result:= MangaVadisiGetNameAndLink
  else
  if website = MANGAFRAME_NAME then
    Result:= MangaframeGetNameAndLink
  else
  if website = SENMANGA_NAME then
    Result:= SenMangaGetNameAndLink
  else
  if website = GEHENTAI_NAME then
    Result:= GEHentaiGetNameAndLink
  else
  if website = TRUYEN18_NAME then
    Result:= Truyen18GetNameAndLink;
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
  mangaInfo.url:= WebsiteRoots[ANIMEA_ID,1] + URL + ANIMEA_SKIP;
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
        for j:= 0 to 37 do
          if Pos(LowerCase(defaultGenre[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(defaultGenre[j]+', ');
      if Pos('</li>', parse.Strings[i]) > 0 then
        isExtractGenres:= FALSE;
    end;

      // get summary
    if Pos('Upload a chapter', parse.Strings[i]) > 0 then
    begin
      j:= i+8;
      mangaInfo.summary:= '';
      while (j<parse.Count-4) AND (Pos('</p>', parse.Strings[j]) = 0) do
      begin
        mangaInfo.summary:= StringFilter(mangaInfo.summary+parse.Strings[j]);
        Inc(j);
      end;
      mangaInfo.summary:= StringFilter(mangaInfo.summary);
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
      mangaInfo.chapterName.Add(StringFilter(TrimRight(TrimLeft(RemoveSymbols(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title'))+' '+parse.Strings[i+3])))));
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
  mangaInfo.url:= WebsiteRoots[MANGAHERE_ID,1] + URL;
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
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAHERE_ID,1], '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      s:= StringFilter(TrimLeft(TrimRight(RemoveSymbols(parse.Strings[i+6]))));
      if (s <> '') AND ((s[1] = '<') OR (s = 'span')) then
        s:= ''
      else
        s:= ' ' + s;
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))) + s);
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
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenre[j]), LowerCase(parse.Strings[i+2]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenre[j]+', ');
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

// get manga infos from SubManga site
// due to its weird designs, this will take a lot of work (and time) for it to
// work property
function   GetSubMangaInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  s: String;
  i, j  : Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[SUBMANGA_ID,1] + URL;
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
  mangaInfo.website:= SUBMANGA_NAME;

  mangaInfo.genres:= '';
  mangaInfo.status:= '1';

  // using 1st parser (cover link, summary)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('content="Leer ', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i], 'content="Leer ', ' manga online');

    if Pos('class="suscripcion', parse.Strings[i]) > 0 then
    begin
      // get cover link
      if Pos('<img', parse.Strings[i+5]) > 0 then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i+5], 'src'));
      // get summary
      j:= i+8;
      while (j < parse.Count-1) AND (Pos('</p>', parse.Strings[j]) = 0) do
      begin
        Inc(j);
        s:= parse.Strings[j];
        if (s <> '') AND
           (s[1] <> '<') then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
      end;
    end;

    // get authors/artists
    if (Pos('Creado por', parse.Strings[i])<>0) then
    begin
      if Pos('/autor/', parse.Strings[i+1]) > 0 then
        mangaInfo.authors:= parse.Strings[i+2];
      if Pos('/mangaka/', parse.Strings[i+5]) > 0 then
        mangaInfo.authors:= parse.Strings[i+6];
    end;

    // get genres
    if (Pos('submanga.com/genero/', parse.Strings[i])<>0) then
      mangaInfo.genres:= mangaInfo.genres + parse.Strings[i+1] + ', ';
  end;

  source.Clear;
  if NOT GetPage(TObject(source), mangaInfo.url + '/completa', Reconnect) then
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

  // using 2nd parser (chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    if (Pos('class="s"', parse.Strings[i])>0) AND
       (Pos('</tr>', parse.Strings[i-2])>0) AND
      ((Pos('<tr class="u">', parse.Strings[i-1])>0) OR (Pos('<tr>', parse.Strings[i-1])>0)) AND
      ((Pos('</td>', parse.Strings[i-3])>0) OR (Pos('</th>', parse.Strings[i-3])>0)) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href')), WebsiteRoots[SUBMANGA_ID,1], '', []);
      for j:= Length(s) downto 1 do
      begin
        if s[j] = '/' then
          break;
      end;
      s:= '/c' + Copy(s, j, Length(s)-j+1);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

function   GetEsMangaHereInfoFromURL: Byte;
var
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[ESMANGAHERE_ID,1] + URL;
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
  mangaInfo.website:= ESMANGAHERE_NAME;
  mangaInfo.status:= '1';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'Manga - Leer ', ' manga en Español');

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
       (Pos('/manga/', GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')))<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[ESMANGAHERE_ID,1], '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))));
    end;

    // get authors
    if (Pos('Autor(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= parse.Strings[i+3];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+3];

    // get genres
    if (Pos('Género(s):', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenre[j]), LowerCase(parse.Strings[i+2]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenre[j]+', ');
    end;

   { // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end; }
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
 { if (mangaInfo.status = '1') AND (mangainfo.ChapterName.Count > 0) then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.chapterLinks.Delete(mangainfo.chapterLinks.Count-1);
  end; }
  Result:= NO_ERROR;
end;

function   GetAnimeExtremistInfoFromURL: Byte;
var
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[ANIMEEXTREMIST_ID,1] + URL;
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
  mangaInfo.website:= ANIMEEXTREMIST_NAME;
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos(' Manga - Animextremist', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString('~!@'+parse.Strings[i], '~!@', ' Manga - Animextremist');

    // get cover link
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'img') then
      if Pos('src="../', parse.Strings[i])>0 then
        mangaInfo.coverLink:= WebsiteRoots[ANIMEEXTREMIST_ID,1] + GetString(parse.Strings[i], 'src="..', '"');

      // get summary
    if (Pos('align="justify" class="style33"', parse.Strings[i])) <> 0 then
    begin
      j:= i+1;
      mangaInfo.summary:= '';
      while (Pos('<td height', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if (s<>'') AND (s[1] <> '<') then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j] + '\n\r';
        end;
        Inc(j);
      end;
    end;

      // get chapter name and links
    if (Pos('/mangas-online/', parse.Strings[i])<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[ANIMEEXTREMIST_ID,1], '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(HTMLEntitiesFilter(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1])))));
    end;

   { // get authors
    if (Pos('Autor(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= parse.Strings[i+3];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+3]; }

    // get genres
    if (Pos('ord=genero&id', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= mangaInfo.genres+(HTMLEntitiesFilter(TrimLeft(TrimRight(parse.Strings[i+1])))+', ');
    end;

    // get status
    if (Pos('class="manga_estado"', parse.Strings[i])<>0) then
    begin
      if Pos('Completo', parse.Strings[i+3])<>0 then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
    end;
  end;

  Result:= NO_ERROR;
end;

function   GetMangaInnInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractChapters: Boolean = FALSE;
begin
  mangaInfo.url:= WebsiteRoots[MANGAINN_ID,1] + URL;
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
        mangaInfo.chapterLinks.Add(CorrectURL(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAINN_ID,1], '', [rfReplaceAll])));
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
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenre[j]), LowerCase(parse.Strings[i+4]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenre[j]+', ');
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
  mangaInfo.url:= WebsiteRoots[OURMANGA_ID,1] + URL;
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
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[OURMANGA_ID,1], '', [rfReplaceAll]));
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
        for j:= 0 to 37 do
          if Pos(LowerCase(defaultGenre[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(defaultGenre[j]+', ');
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
  mangaInfo.url:= EncodeURL(WebsiteRoots[KISSMANGA_ID,1] + URL + '?confirm=yes');
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
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[KISSMANGA_ID,1], '', [rfReplaceAll]));
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
      if (i+1 < parse.Count) AND (Pos('"/Genre/', parse.Strings[i]) > 0) then
        mangaInfo.genres:= mangaInfo.genres+(TrimLeft(TrimRight(parse.Strings[i+1]))+', ');
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
 { zstream : TDecompressionStream;
  dstream,
  sstream : TMemoryStream; }
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
  if Pos('comic/_/comics', patchURL) = 0 then
    patchURL:= StringReplace(URL, 'comic/_', 'comic/_/comics', []);
  mangaInfo.url:= WebsiteRoots[BATOTO_ID,1] + patchURL;

reload:
  source.Clear;
  {$IFDEF WINDOWS}
  if isGetByUpdater then
  begin
    if NOT GetPage(TObject(source), TrimLeft(TrimRight(mangaInfo.url)), Reconnect) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
  end
  else
  if NOT OptionBatotoUseIEChecked then
  begin
    if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
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
 // parse.SaveToFile('error2.txt');

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
         (Pos('"og:title"', parse.Strings[i]) > 0) then
        mangaInfo.title:= StringFilter(GetString(parse.Strings[i], '"og:title" content="', ' - Scanlations'));
    end
    else
    begin
      if (mangaInfo.title = '') AND
         (Pos('<TITLE>', parse.Strings[i]) > 0) then
        mangaInfo.title:= StringFilter(GetString('~!@' + parse.Strings[i+1], '~!@', ' - Scanlations'));
    end;

    // get summary
    if (Pos('Description:', parse.Strings[i]) <> 0) then
    begin
      j:= i+3;
      mangaInfo.summary:= '';
      while (Pos(IEUp('</td>'), parse.Strings[j])=0) AND (j < parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]+#10#13);
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
      mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[BATOTO_ID,1], '', [rfReplaceAll])));
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
      s:= StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2])));
      if OptionShowBatotoSG then
        s:= s + ' [by ' + StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+15]))) + ']';
      mangaInfo.chapterName.Add(s);
    end
    else
    if (OptionBatotoUseIEChecked) AND
       (GetTagName(parse.Strings[i]) = IEUp('a')) AND
       (Pos('/read/_/', parse.Strings[i])<>0) AND
       (i+2 < parse.Count-1) AND
       (Pos('English', parse.Strings[i-3])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[BATOTO_ID,1], '', [rfReplaceAll])));
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
      if Pos('</span>', parse.Strings[i]) > 0 then
        mangaInfo.genres:= mangaInfo.genres+(TrimLeft(TrimRight(parse.Strings[i-1]))+', ');
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
  mangaInfo.url:= WebsiteRoots[MANGA24H_ID,1] + URL;
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
      mangaInfo.chapterLinks.Add(CorrectURL('/'+StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGA24H_ID,1], '', [rfReplaceAll])));
      parse.Strings[i+2]:= HTMLEntitiesFilter(parse.Strings[i+2]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
      parse.Strings[i+2]:= TrimLeft(parse.Strings[i+2]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2]))));
    end;

    // get title
    if (Pos('<title>', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(StringFilter(GetString('~!@'+parse.Strings[i+1], '~!@', ' - Truyen Tranh Online')));

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
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenre[j]), LowerCase(parse.Strings[i+4]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenre[j]+', ');
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
  mangaInfo.url:= WebsiteRoots[VNSHARING_ID,1] + URL + '&confirm=yes';
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
      if (i+1<parse.Count) AND (Pos('"/TheLoai/', parse.Strings[i]) > 0) then
        mangaInfo.genres:= mangaInfo.genres+(TrimLeft(TrimRight(parse.Strings[i+1]))+', ');
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

  if (mangaInfo.status = '1') AND (mangainfo.ChapterName.Count > 0) then
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
        if mangaInfo.coverLink = 'http://hentai2read.com/wp-content/hentai/cover/tbn/001_1535_233x0.jpg' then
          mangaInfo.coverLink:= '';
      end;

    // get chapter name and links
    if isExtractChapters then
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND (i < parse.Count-2) then
      begin
        Inc(mangaInfo.numChapter);
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]);
        s:= StringReplace(s, HENTAI2READ_MROOT, '', [rfReplaceAll]);
        mangaInfo.chapterLinks.Add(s);
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
  mangaInfo.url:= WebsiteRoots[FAKKU_ID,1] + URL;
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
       (Pos(' title="">', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(parse.Strings[i+5]));

    // get cover
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cover') then
      begin
        s:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
        if Pos('http://', s) = 0 then
          mangaInfo.coverLink:= WebsiteRoots[FAKKU_ID,1] + s
        else
          mangaInfo.coverLink:= s;
      end;

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
        mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[FAKKU_ID,1], '', [rfReplaceAll]));
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
  mangaInfo.url:= WebsiteRoots[MANGAREADER_ID,1] + URL;// + '&confirm=yes';
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
  mangaInfo.url:= WebsiteRoots[MANGAPARK_ID,1] + URL;// + '&confirm=yes';
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
      mangaInfo.title:= StringFilter(TrimLeft(TrimRight(GetString(parse.Strings[i+1], ' Manga - Read ', ' Online For '))));

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
  if Pos(WebsiteRoots[MANGAFOX_ID,1], URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MANGAFOX_ID,1] + URL
  else
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
      s:= StringReplace(GetString(parse.Strings[i], 'href="', '/1.html"'), WebsiteRoots[MANGAFOX_ID,1], '', []);
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
  mangaInfo.url:= WebsiteRoots[MANGATRADERS_ID,1] + URL;// + '&confirm=yes';
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
      mangaInfo.coverLink:= WebsiteRoots[MANGATRADERS_ID,1] + CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src=')));

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
      if NOT GetPage(TObject(source), WebsiteRoots[MANGATRADERS_ID,1] + URL + '/page/' + IntToStr(k) + '/', Reconnect) then
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

// get manga infos from mangastream site
function   GetMangaStreamInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = TRUE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= MANGASTREAM_ROOT + URL;
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
  mangaInfo.website:= MANGASTREAM_NAME;

  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  mangaInfo.status:= '1';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // do not allow to get chapter name and links
    if (isExtractChapter) AND
       (Pos('</table>', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('<td>', parse.Strings[i])>0) AND
       (Pos('</td>', parse.Strings[i+4])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), MANGASTREAM_ROOT2, '', []);
      Delete(s, Length(s)-1, 2);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

// get manga infos from mangaeden site
function   GetMangaEdenInfoFromURL(const root: String): Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= root + URL;// + '&confirm=yes';
  if Pos('/en-manga/', URL)>0 then
    mangaInfo.genres:= 'English, '
  else
    mangaInfo.genres:= 'Italian, ';
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
  if root = WebsiteRoots[MANGAEDEN_ID,1] then
    mangaInfo.website:= MANGAEDEN_NAME
  else
    mangaInfo.website:= PERVEDEN_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (Pos('class="mangaImage2"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));

    // get summary
    if (Pos('hr style="margin-top:0;', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+2;
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
    if ((Pos('enIcon', parse.Strings[i])<>0) OR (Pos('itIcon', parse.Strings[i])<>0)) AND
       (mangaInfo.title = '') then
    begin
      mangaInfo.title:= StringFilter(TrimRight(TrimLeft(parse.Strings[i+1])));
      mangaInfo.title:= GetString('~!@'+mangaInfo.title, '~!@', ' Manga');
    end;

    // get chapter name and links
    if (i+7<parse.Count) AND (Pos('class="chapterLink"', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i], 'href="', '1/"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+7])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if (i+1<parse.Count) AND (Pos('/?author', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+1]);

    // get artists
    if (i+1<parse.Count) AND (Pos('/?artist', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

    // get genres
    if (Pos('Genres', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('/?categories', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('<br />', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if parse.Strings[i] = 'Status' then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
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

// get manga infos from Starkana site
function   GetStarkanaInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[STARKANA_ID,1] + URL + '?mature_confirm=1';
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
  mangaInfo.website:= STARKANA_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('/upload/covers/', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'content=')));

    // get summary
    if (Pos('Summary:', parse.Strings[i]) <> 0) then
    begin
      j:= i+5;
      while (j<parse.Count) AND (Pos('</td>', parse.Strings[j])=0) do
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
    end;

    // get title
    if (Pos('Title(s):', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= StringFilter(TrimRight(TrimLeft(parse.Strings[i+5])));

    // get chapter name and links
    if (NOT isExtractChapter) AND
      ((Pos('class="zxz episode c_h2"', parse.Strings[i])>0) OR
       (Pos('class="zxz episode c_h2b"', parse.Strings[i])>0)) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
      ((Pos('class="zxz episode c_h2"', parse.Strings[i])>0) OR
       (Pos('class="zxz episode c_h2b"', parse.Strings[i])>0)) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i+3], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+6]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+10])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+7<parse.Count) AND (Pos('Creator(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimRight(TrimLeft(parse.Strings[i+7]));

    // get genres
    if (Pos('Genres:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('/manga/search?g', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('Start Date:', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+7<parse.Count) AND (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+7])<>0 then
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

// get manga infos from eatmanga site
function   GetEatMangaInfoFromURL: Byte;
var
  s: String;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[EATMANGA_ID,1] + URL;// + '&confirm=yes';
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
  mangaInfo.website:= EATMANGA_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('border="0" align="center"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos(' manga about ?', parse.Strings[i]) <> 0) then
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
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
    end;

    // get title
    if (Pos(', Read ', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i], ', Read ', ' English Scan Online')));

    // get chapter name and links
    if (Pos('<th class="title">', parse.Strings[i])>0) AND
       (Pos('/upcoming/', parse.Strings[i+1])=0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i+1], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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
      mangaInfo.genres:= '';
      s:= parse.Strings[i+4];
      if s[1] <> '<' then
        mangaInfo.genres:= s;
    end;

    // get status
    if (i+4<parse.Count) AND (Pos('Chapters:', parse.Strings[i])<>0) then
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

// get manga infos from redhawkscans site
function   GetRedHawkScansInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[REDHAWKSCANS_ID,1] + URL;// + '&confirm=yes';
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
  mangaInfo.website:= REDHAWKSCANS_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('class="thumbnail"', parse.Strings[i-2])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('Description', parse.Strings[i]) <> 0) AND
       (Pos('<b', parse.Strings[i-1]) <> 0) then
    begin
      j:= i+2;
      while (j<parse.Count) AND (Pos('</li>', parse.Strings[j])=0) do
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
    end;

    // get title
    if (Pos('Title', parse.Strings[i])<>0) AND
       (Pos('<b', parse.Strings[i-1]) <> 0) AND
       (mangaInfo.title = '') then
      mangaInfo.title:= StringReplace(TrimLeft(StringFilter(parse.Strings[i+2])), ': ', '', []);

    if (NOT isExtractChapter) AND
       (Pos('All chapters available for', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('class="element"', parse.Strings[i]) > 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetString(parse.Strings[i+3], 'href="', '"'), WebsiteRoots[REDHAWKSCANS_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if (i+2<parse.Count) AND (Pos('Author', parse.Strings[i])<>0) AND
       (Pos('<b', parse.Strings[i-1]) <> 0) then
      mangaInfo.authors:= StringReplace(TrimLeft(parse.Strings[i+2]), ': ', '', []);

    // get artists
    if (i+2<parse.Count) AND (Pos('Artist', parse.Strings[i])<>0) AND
       (Pos('<b', parse.Strings[i-1]) <> 0) then
      mangaInfo.artists:= StringReplace(TrimLeft(parse.Strings[i+2]), ': ', '', []);

    // get genres
    if (Pos('Genre', parse.Strings[i])<>0) AND
       (Pos('<b', parse.Strings[i-1]) <> 0) then
      mangaInfo.genres:= StringReplace(TrimLeft(parse.Strings[i+2]), ': ', '', []);

    // get status
    if (i+2<parse.Count) AND (Pos('Status', parse.Strings[i])<>0) AND
       (Pos('<b', parse.Strings[i-1]) <> 0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
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

// get manga infos from s2scan site
function   GetS2scanInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
 // stream: TStringStream;
 // HTTP: THTTPSend;
  R   : Boolean;
begin
  mangaInfo.url:= WebsiteRoots[S2SCAN_ID,1] + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;
 { stream:= TStringStream.Create('');
  while NOT HttpPostURL(mangaInfo.url, 'name=""&value="true"', stream) do ;

  HTTP := THTTPSend.Create;
  try
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.Cookies.Add('ci_session=VW5QZlw1BG0AfVd%2BADwDZwUwV2kHIAApUmJTJFUnU2sCbVwyDFVRaVNtV3kOaAAgBjpXPgYyAW4HIFBgAmRdPlJmUmBVYlQ1BmQBZwZlBDZVOFBuXDkEZwBjVzUAYgNjBTFXYgdhADxSMlNvVWRTZwI0XG0MaVExUztXeQ5oACAGOlc8BjABbgcgUD0Cd11YUmZSMlUwVHcGYwFyBnMEd1U0UC9cOwRmADJXNwAkA2cFMFdrBywAa1IxU2VVelMwAjBcaQwkUThTO1d5DmgAIAY6VzwGMAFuByBQIQJ0XWJSdVIJVTVUYgZjAW8GdAR3VTRQL1w7BGAANFc3ACQDGwVvVykHawA2UmtTNlV7UzcCLFxsDCpRKFNeVzIOPQA3Bm9XegZzAXQHTFAAAiddMVIpUmRVb1QlBlEBTgZXBGNVO1BnXCEEJQBwVzcANANkBS5XYwcrAHpSQFMyVTdTaQJtXHMMOFEwUzhXaw5jAGIGMFc8BiABEgdrUCYCYl1hUmhSLlV7VDcGNAEvBjAEd1U0UC9cOwRmADVXNwAkAzoFYVcgB3YABVJmUzRVIFNrAnRcNQx%2BUXlTK1dgDjoAaQYxVz4GNwFhBzdQZwIwXT9SP1JjVW9UeA%3D%3D');
    R:= HTTP.HTTPMethod('GET', mangaInfo.url);
    if R then
      stream.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
  source.Text:= stream.DataString;
  stream.Free;                     }

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= S2SCAN_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('class="thumbnail"', parse.Strings[i-2])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get title
    if (Pos('h1 class="title"', parse.Strings[i])<>0) AND
       (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+2])));

    if (NOT isExtractChapter) AND
       (Pos('All chapters available for', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('class="element"', parse.Strings[i]) > 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetString(parse.Strings[i+3], 'href="', '"'), WebsiteRoots[S2SCAN_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

// get manga infos from EGScans site
function   GetEGScansInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[EGSCANS_ID,1] + '/' + URL;
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

  mangaInfo.website:= EGSCANS_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (NOT isExtractChapter) AND
       (Pos('<span>', parse.Strings[i])>0) AND
       (Pos('Chapter', parse.Strings[i+1])>0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('</span>', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('content="Read ', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i], '~!@content="Read ', ' Manga Online"');

   { if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break; }

    if (isExtractChapter) AND
       (Pos('<option value="', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= '/' + URL + '/' + StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value=')), WebsiteRoots[EGSCANS_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;
  end;

  Result:= NO_ERROR;
end;

// get manga infos from eatmanga site
function   GetMangaPandaInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
begin
  mangaInfo.url:= MANGAPANDA_ROOT + URL;// + '&confirm=yes';
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
  mangaInfo.website:= MANGAPANDA_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('div id="mangaimg"', parse.Strings[i-1])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('div id="readmangasum"', parse.Strings[i]) <> 0) then
    begin
      j:= i+7;
      mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i+3], 'Read ', ' Manga Online')));
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
    end;

    // get title
   // if (Pos(', Read ', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
   //   mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i], ', Read ', ' English Scan Online')));

    if (NOT isExtractChapter) AND
       (Pos('Chapter Name', parse.Strings[i]) > 0) AND
       (Pos('class="leftgap"', parse.Strings[i-1]) > 0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('class="chico_manga"', parse.Strings[i]) > 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i+3], 'href="', '"');
      if (Pos('.html', s) > 0) AND (Pos('-1/', s) > 0) then
        s:= StringReplace(s, '-1/', SEPERATOR, []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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
    end;

    if isExtractGenres then
    begin
      if Pos('class="genretags"', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</tr>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+4<parse.Count) AND (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from truyentranhtuan site
function   GetTruyenTranhTuanInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[TRUYENTRANHTUAN_ID,1] + URL;// + '&confirm=yes';
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
  mangaInfo.website:= TRUYENTRANHTUAN_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (Pos('class="title-logo1"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));

    // get summary
    if (Pos('Tóm tắt truyện', parse.Strings[i]) <> 0) AND
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
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (Pos('Tên truyện:', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(StringFilter(parse.Strings[i+2]));

    // get chapter name and links
    if (Pos('class="tbl_body">', parse.Strings[i])>0) OR
       (Pos('class="tbl_body2">', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i+1], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+1<parse.Count) AND (Pos('Tác Giả:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+2]);

    // get artists
    //if (i+1<parse.Count) AND (Pos('/search/artist/', parse.Strings[i])<>0) then
    //  mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

    // get genres
    if (Pos('Thể loại:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('<a href=', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</span>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+5<parse.Count) AND (Pos('Chương mới nhất', parse.Strings[i])<>0) then
    begin
      if Pos('Đang tiến hành', parse.Strings[i+2])<>0 then
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

// get manga infos from Komikid site
function   GetKomikidInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[KOMIKID_ID,1] + URL;
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

  mangaInfo.website:= KOMIKID_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (Pos('select name="chapter"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('<title>', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i-2], 'content="', ' Chapter');

    if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break;

    if (isExtractChapter) AND (Pos('option value=', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= URL + '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

// get manga infos from PecintaKomik site
// this site have 2 kind of cover page
function   GetPecintaKomikInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
  k   : Integer;
begin
  mangaInfo.url:= WebsiteRoots[PECINTAKOMIK_ID,1] + URL;
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
  mangaInfo.website:= PECINTAKOMIK_NAME;
  mangaInfo.status:= '1';
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  if Pos('/manga/', URL) = 0 then
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('id="mangaimg"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= WebsiteRoots[PECINTAKOMIK_ID,1] + CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));

    // get summary
    if (Pos('Himbauan:', parse.Strings[i]) <> 0) then
    begin
      j:= i+4;
      while (j<parse.Count) AND (Pos('</tr>', parse.Strings[j])=0) do
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
    end;

    // get title
    if (Pos('Nama:', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= StringFilter(TrimRight(TrimLeft(parse.Strings[i+6])));

    // get chapter name and links
    if (NOT isExtractChapter) AND
       (Pos('id="chapterlist"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('/manga/', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Autor:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimRight(TrimLeft(parse.Strings[i+4]));

    // get artists
    if  (i+4<parse.Count) AND (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimRight(TrimLeft(parse.Strings[i+4]));

    // get genres
    if (Pos('Genre:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('class="genretags"', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</tr>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;
  end
  else
  for i:= 0 to parse.Count-1 do
  begin
    if Pos('name="chapter"', parse.Strings[i]) > 0 then
    begin
      if TryStrToInt(TrimRight(TrimLeft(parse.Strings[i+2])), k) then
        for j:= k downto 1 do
        begin
          Inc(mangaInfo.numChapter);
          s:= URL + IntToStr(j);
          mangaInfo.chapterLinks.Add(s);
          s:= 'Chapter ' + IntToStr(j);
          mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
        end;
      break;
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

// get manga infos from Mabuns site
function   GetMabunsInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  if Pos('http://', URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MABUNS_ID,1] + URL
  else
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
  mangaInfo.website:= MABUNS_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('rel="image_src"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));

    // get title
    if (Pos('Judul :', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(TrimRight(HTMLEntitiesFilter(StringFilter(GetString(parse.Strings[i]+'~!@', 'Judul :', '~!@')))));

    if (NOT isExtractChapter) AND (Pos('Baca Online:', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('<a href', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MABUNS_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('</table>', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get authors
    if  (i+8<parse.Count) AND (Pos('Author :', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(GetString(parse.Strings[i]+'~!@', 'Author :', '~!@')));

    // get artists
    if (i+1<parse.Count) AND (Pos('Artist :', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(GetString(parse.Strings[i]+'~!@', 'Artist :', '~!@')));

    // get genres
    if (Pos('Genre :', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= TrimLeft(TrimRight(GetString(parse.Strings[i]+'~!@', 'Genre :', '~!@')));
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

// get manga infos from MangaEsta site
function   GetMangaEstaInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  if Pos('http://', URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MANGAESTA_ID,1] + URL
  else
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
  mangaInfo.website:= MANGAESTA_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('imageanchor="1"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));

    // get title
    if (Pos('Nama :', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(TrimRight(HTMLEntitiesFilter(parse.Strings[i+2])));

    if (NOT isExtractChapter) AND (Pos('ONLINE INDONESIAN', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('href="http://www.mangaesta.net/', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAESTA_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('class=''comments''', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get summary
    if (Pos('Sinopsis :', parse.Strings[i]) <> 0) then
    begin
      j:= i+6;
      while (j<parse.Count) AND (Pos('</div>', parse.Strings[j])=0) do
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
    end;

    // get authors
    if  (i+8<parse.Count) AND (Pos('Author :', parse.Strings[i])<>0) then
      mangaInfo.authors:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));

    // get artists
    if (i+1<parse.Count) AND (Pos('Artist :', parse.Strings[i])<>0) then
      mangaInfo.artists:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));

    // get genres
    if (Pos('Genre :', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= StringReplace(StringFilter(TrimLeft(TrimRight(parse.Strings[i+2]))), ' |', ',', [rfReplaceAll]);
      mangaInfo.genres:= StringReplace(mangaInfo.genres, '|', ',', [rfReplaceAll]);
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Status :', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
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

// get manga infos from HugeManga site
function   GetHugeMangaInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[HUGEMANGA_ID,1] + HUGEMANGA_BROWSER + URL;
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

  mangaInfo.website:= HUGEMANGA_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (Pos('select name="chapter"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('<title>', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'indonesia online - ', ' - Chapter');

    if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break;

    if (isExtractChapter) AND (Pos('option value=', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= '/' + URL + '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

// get manga infos from Turkcraft site
function   GetTurkcraftInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[TURKCRAFT_ID,1] + URL;
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

  mangaInfo.website:= TURKCRAFT_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (Pos('select name="chapter"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('Mangaturk - ', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i], 'Mangaturk - ', ' - Chapter');

    if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break;

    if (isExtractChapter) AND (Pos('option value=', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= URL + '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

// get manga infos from MangaVadisi site
function   GetMangaVadisiInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAVADISI_ID,1] + MANGAVADISI_BROWSER + URL;
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

  mangaInfo.website:= MANGAVADISI_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (Pos('select name="chapter"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('Manga Vadisi - ', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i], 'Manga Vadisi - ', ' - Chapter');

    if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break;

    if (isExtractChapter) AND (Pos('option value=', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= URL + '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  Result:= NO_ERROR;
end;

// get manga infos from Mangaframe site
function   GetMangaframeInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAFRAME_ID,1] + URL;
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

  mangaInfo.website:= MANGAFRAME_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (Pos('<div class="list">', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos(' :: Mangaoku', parse.Strings[i])>0) then
      mangaInfo.title:= GetString('~!@'+parse.Strings[i], '~!@', ' :: Mangaoku');


   { if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break; }

    if (isExtractChapter) AND
       (Pos('<div class="title">', parse.Strings[i])>0) AND
       (Pos('</div>', parse.Strings[i+2])=0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGAFRAME_ID,1], '', []) + 'page/';
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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

// get manga infos from MangaAr site
function   GetMangaArInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAAR_ID,1] + URL;
  if NOT GetPage(TObject(source), EncodeURL(mangaInfo.url), Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // convert charset
  source.Text:= CP1256ToUTF8(source.Text);

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= MANGAAR_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('class="manga-pic"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('القصة :', parse.Strings[i]) <> 0) AND
       (Pos('</font>', parse.Strings[i+1])<>0)  AND
       (isExtractSummary) then
    begin
      j:= i+6;
      while (j<parse.Count) AND (Pos('</td>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(TrimLeft(parse.Strings[j])));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
          break;
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (mangaInfo.title = '') AND
       (Pos(' - ARAB MANGA Online', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i], '~!@', ' - عرب مانجا أونلاين - مشاهدة مباشرة دون عناء التحميل'))));

    // get chapter name and links
    if (Pos('vertical-align: middle; margin: 5px', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href=')), WebsiteRoots[MANGAAR_ID,1], '', []);
      s:= EncodeURL(StringReplace(s+'~!@', '/1~!@', '', []));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    // get authors
    if (i+6<parse.Count) AND
       (Pos('المؤلف :', parse.Strings[i])<>0) AND
       (Pos('</font>', parse.Strings[i+1])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+8]));

    // get artists
    if (i+6<parse.Count) AND
       (Pos('الرسام :', parse.Strings[i])<>0) AND
       (Pos('</font>', parse.Strings[i+1])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(parse.Strings[i+8]));

    // get genres
    if (Pos('&Genres=', parse.Strings[i])<>0) then
      mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';

    // get status
    if (i+6<parse.Count) AND
       (Pos('الحالة :', parse.Strings[i])<>0) AND
       (Pos('</font>', parse.Strings[i+1])<>0) then
    begin
      if (Pos('غير مكتمله', parse.Strings[i+6])<>0) then
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

// get manga infos from MangaAe site
function   GetMangaAeInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAAE_ID,1] + URL;
  if NOT GetPage(TObject(source), EncodeURL(mangaInfo.url), Reconnect) then
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
  mangaInfo.website:= MANGAAE_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('id="photo_1"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));

    // get summary
    if (Pos('<p class="sumry">', parse.Strings[i]) <> 0) then
    begin
      j:= i+1;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(TrimLeft(parse.Strings[j])));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
          break;
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (mangaInfo.title = '') AND
       (Pos(' -  مانجا العرب ● Manga Al-arab', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i], '~!@', ' -  مانجا العرب ● Manga Al-arab'))));

    if (NOT isExtractChapter) AND
       (Pos('class="mangachapters"', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos(WebsiteRoots[MANGAAE_ID,1], parse.Strings[i])>0) AND
       (Pos('<li>', parse.Strings[i-2])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAAE_ID,1], '', []);
      s:= StringReplace(s+'~!@', '/1/~!@', '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('class="ads"', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get authors
    if (i+6<parse.Count) AND
       (Pos('المؤلف', parse.Strings[i])<>0) AND
       (Pos('</h2>', parse.Strings[i+1])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+4]));

    // get genres
    if (Pos('/mn_name/', parse.Strings[i])<>0) then
      mangaInfo.genres:= mangaInfo.genres + parse.Strings[i+1] + ', ';

    // get status
    if (i+3<parse.Count) AND
       (Pos('حالة الترجمة', parse.Strings[i])<>0) AND
       (Pos('</h2>', parse.Strings[i+1])<>0) then
    begin
      if (Pos('غير مكتملة', parse.Strings[i+3])<>0) then
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

// get manga infos from centraldemangas site
function   GetCentralDeMangasInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[CENTRALDEMANGAS_ID,1] + URL;
  if NOT GetPage(TObject(source), EncodeURL(mangaInfo.url), Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // convert charset
  source.Text:= ISO_8859_1ToUTF8(source.Text);

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= CENTRALDEMANGAS_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('class="capa_sinopse"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('Sinopse', parse.Strings[i]) <> 0) AND
       (Pos('<h2>', parse.Strings[i-1])<>0)  AND
       (isExtractSummary) then
    begin
      j:= i+2;
      while (j<parse.Count) AND (Pos('</div>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(TrimLeft(parse.Strings[j])));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
          break;
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (mangaInfo.title = '') AND
       (Pos('/online/', parse.Strings[i])<>0) AND
       (Pos('<td>', parse.Strings[i-1])<>0) then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString(parse.Strings[i], 'title=', '">'))));

    if (NOT isExtractChapter) AND
       (Pos('Cap', parse.Strings[i]) > 0) AND
       (Pos('ulos', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('/online/', parse.Strings[i])>0) AND
       (Pos('<span>', parse.Strings[i-1])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[CENTRALDEMANGAS_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('</div>', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get authors
    if (i+6<parse.Count) AND
       (Pos('Autor', parse.Strings[i])<>0) AND
       (Pos('<h2>', parse.Strings[i-1])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+6]));

    // get artists
    if (i+6<parse.Count) AND
       (Pos('Arte', parse.Strings[i])<>0) AND
       (Pos('<h2>', parse.Strings[i-1])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(parse.Strings[i+6]));

    // get genres
    if (i+6<parse.Count) AND
       (Pos('Gen', parse.Strings[i])<>0) AND
       (Pos('</h2>', parse.Strings[i+1])<>0) AND
       (Pos('<h2>', parse.Strings[i-1])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      s:= TrimLeft(TrimRight(parse.Strings[i]));
      if Pos('</div>', s) <> 0 then
        isExtractGenres:= FALSE
      else
      if (Pos('class="rotulo"', s)<>0) then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
    end;

    // get status
    if (i+6<parse.Count) AND
       (Pos('Status', parse.Strings[i])<>0) AND
       (Pos('<h2>', parse.Strings[i-1])<>0) then
    begin
      if (Pos('Completo', parse.Strings[i+6])<>0) then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
    end;
  end;
  Result:= NO_ERROR;
end;

// get manga infos from SenManga site
function   GetSenMangaInfoFromURL: Byte;
var
  s: String;
  isRepeated      : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
label
  Again;
begin
  mangaInfo.url:= WebsiteRoots[SENMANGA_ID,1] + URL;
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

Again:
  Parser.Free;
  source.Free;

  mangaInfo.website:= SENMANGA_NAME;
  mangaInfo.status:= '1';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  isExtractChapter:= FALSE;

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get chapter name and links
    if (Pos('name="chapter"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('<title>', parse.Strings[i])>0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString(parse.Strings[i+1], 'Raw |', '|')));

   { if (isExtractChapter) AND (Pos('</select>', parse.Strings[i])>0) then
      break; }

    if (isExtractChapter) AND
       (Pos('<option', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= URL + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value=')) + '/';
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    if (isExtractChapter) AND
       (Pos('</select>', parse.Strings[i])>0) then
    begin
      isExtractChapter:= FALSE;
      break;
    end;
  end;

  if (mangainfo.ChapterLinks.Count = 0) AND NOT (isRepeated) then
  begin
    isRepeated:= TRUE;
    for i:= 0 to parse.Count-1 do
    begin
      if Pos('class="t-ch-list"', parse.Strings[i]) > 0 then
      begin
        s:= WebsiteRoots[SENMANGA_ID,1] + GetAttributeValue(GetTagAttribute(parse.Strings[i+5], 'href='));
        break;
      end;
    end;
    source:= TStringList.Create;
    if NOT GetPage(TObject(source), s, Reconnect) then
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
    goto Again;
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

// get manga infos from blogtruyen site
function   GetBlogTruyenInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[BLOGTRUYEN_ID,1] + URL;
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
  mangaInfo.website:= BLOGTRUYEN_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('img style=''max-width:500px;', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('sơ lược', parse.Strings[i]) <> 0) AND
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
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
          break;
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (Pos(' - Blog truyện tranh online', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i], '~!@', ' - Blog truyện tranh online'))));

    if (NOT isExtractChapter) AND (Pos('Tổng hợp (', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('title=''Đọc truyện', parse.Strings[i])>0) AND
       (Pos('/truyen/', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('để xem truyện nhanh hơn', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get authors
    if  (i+8<parse.Count) AND (Pos('Tác giả:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+3]));

    // get artists
    //if (i+1<parse.Count) AND (Pos('/search/artist/', parse.Strings[i])<>0) then
    //  mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

    // get genres
    if (Pos('Thể loại:', parse.Strings[i])<>0) AND (Pos('truyện tranh', parse.Strings[i])=0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      s:= TrimLeft(TrimRight(parse.Strings[i]));
      if Pos('Đăng bởi:', s) <> 0 then
        isExtractGenres:= FALSE
      else
      if (s<>'') AND (s[1] <> '<') AND (s[1] <> ',') AND (Pos('Thể loại:', s)=0) then
        mangaInfo.genres:= mangaInfo.genres + s + ', ';
    end;

    // get status
    if (i+12<parse.Count) AND (Pos('Trạng thái:', parse.Strings[i])<>0) then
    begin
      if (Pos('Đang tiến hành', parse.Strings[i+2])<>0) OR
         (Pos('Tạm ngưng', parse.Strings[i+2])<>0) then
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
 { if website = OURMANGA_NAME then
    Result:= GetOurMangaInfoFromURL
  else }
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
  if website = STARKANA_NAME then
    Result:= GetStarkanaInfoFromURL
  else
  if website = EATMANGA_NAME then
    Result:= GetEatMangaInfoFromURL
  else
  if website = MANGAPANDA_NAME then
    Result:= GetMangaPandaInfoFromURL
  else
  if website = MANGASTREAM_NAME then
    Result:= GetMangaStreamInfoFromURL
  else
  if website = REDHAWKSCANS_NAME then
    Result:= GetRedHawkScansInfoFromURL
  else
  if website = S2SCAN_NAME then
    Result:= GetS2scanInfoFromURL
  else
  if website = EGSCANS_NAME then
    Result:= GetEGScansInfoFromURL
  else
  if website = TRUYENTRANHTUAN_NAME then
    Result:= GetTruyenTranhTuanInfoFromURL
  else
  if website = MANGAEDEN_NAME then
    Result:= GetMangaEdenInfoFromURL(WebsiteRoots[MANGAEDEN_ID,1])
  else
  if website = PERVEDEN_NAME then
    Result:= GetMangaEdenInfoFromURL(WebsiteRoots[PERVEDEN_ID,1])
  else
  if website = SUBMANGA_NAME then
    Result:= GetSubMangaInfoFromURL
  else
  if website = ESMANGAHERE_NAME then
    Result:= GetEsMangaHereInfoFromURL
  else
  if website = ANIMEEXTREMIST_NAME then
    Result:= GetAnimeExtremistInfoFromURL
  else
  if website = KOMIKID_NAME then
    Result:= GetKomikidInfoFromURL
  else
  if website = PECINTAKOMIK_NAME then
    Result:= GetPecintaKomikInfoFromURL
  else
  if website = MABUNS_NAME then
    Result:= GetMabunsInfoFromURL
  else
  if website = MANGAESTA_NAME then
    Result:= GetMangaEstaInfoFromURL
  else
  if website = HUGEMANGA_NAME then
    Result:= GetHugeMangaInfoFromURL
  else
  if website = TURKCRAFT_NAME then
    Result:= GetTurkcraftInfoFromURL
  else
  if website = MANGAFRAME_NAME then
    Result:= GetMangaframeInfoFromURL
  else
  if website = MANGAVADISI_NAME then
    Result:= GetMangaVadisiInfoFromURL
  else
  if website = MANGAAR_NAME then
    Result:= GetMangaArInfoFromURL
  else
  if website = MANGAAE_NAME then
    Result:= GetMangaAeInfoFromURL
  else
  if website = CENTRALDEMANGAS_NAME then
    Result:= GetCentralDeMangasInfoFromURL
  else
  if website = SENMANGA_NAME then
    Result:= GetSenMangaInfoFromURL
  else
  if website = BLOGTRUYEN_NAME then
    Result:= GetBlogTruyenInfoFromURL
  else
  if website = GEHENTAI_NAME then
  begin
    case isGetByUpdater of
      TRUE:  Result:= GetGEHentaiInfoFromURL_Dummy;
      FALSE: Result:= GetGEHentaiInfoFromURL;
    end;
  end
  else
  if website = TRUYEN18_NAME then
    Result:= GetTruyen18InfoFromURL;

  s:= mangaInfo.artists;
  if (s <> '') AND (s[1] = '<') then
    mangaInfo.artists:= '';
  s:= mangaInfo.authors;
  if (s <> '') AND (s[1] = '<') then
    mangaInfo.authors:= '';

  // check everything one more
  mangaInfo.authors:= StringReplace(mangaInfo.authors, #10, '', [rfReplaceAll]);
  mangaInfo.authors:= StringReplace(mangaInfo.authors, #13, '', [rfReplaceAll]);
  mangaInfo.artists:= StringReplace(mangaInfo.artists, #10, '', [rfReplaceAll]);
  mangaInfo.artists:= StringReplace(mangaInfo.artists, #13, '', [rfReplaceAll]);
  mangaInfo.genres := StringReplace(mangaInfo.genres , #10, '', [rfReplaceAll]);
  mangaInfo.genres := StringReplace(mangaInfo.genres , #13, '', [rfReplaceAll]);
end;

procedure   TMangaInformation.SyncMinorInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
begin
  // sync info to data
  {$IFDEF DOWNLOADER}
  if NOT dataProcess.isFilterAllSites then
  {$ENDIF}
  begin
    DataProcess.Data.Strings[index]:= SetParams(
              [DataProcess.Param[index, DATA_PARAM_NAME],
               DataProcess.Param[index, DATA_PARAM_LINK],
               DataProcess.Param[index, DATA_PARAM_AUTHORS],
               DataProcess.Param[index, DATA_PARAM_ARTISTS],
               DataProcess.Param[index, DATA_PARAM_GENRES],
               mangaInfo.status,
               DataProcess.Param[index, DATA_PARAM_SUMMARY],
               IntToStr(mangaInfo.numChapter),
               {$IFDEF DOWNLOADER}
               DataProcess.Param[index, DATA_PARAM_JDN]
               {$ELSE}
               '0'
               {$ENDIF},
              '0']);
  end;
  // then break it into parts
  dataProcess.BreakDataToParts(index);
end;

procedure   TMangaInformation.SyncInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
begin
  // sync info to data
  {$IFDEF DOWNLOADER}
  if NOT dataProcess.isFilterAllSites then
  {$ENDIF}
  begin
    DataProcess.Data.Strings[index]:= SetParams(
              [DataProcess.Param[index, DATA_PARAM_NAME],
               DataProcess.Param[index, DATA_PARAM_LINK],
               mangaInfo.authors,
               mangaInfo.artists,
               mangaInfo.genres,
               mangaInfo.status,
               StringFilter(mangaInfo.summary),
               IntToStr(mangaInfo.numChapter),
               {$IFDEF DOWNLOADER}
               DataProcess.Param[index, DATA_PARAM_JDN]
               {$ELSE}
               '0'
               {$ENDIF},
              '0']);
  end;
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
             {$IFDEF DOWNLOADER}
             IntToStr(GetCurrentJDN)
             {$ELSE}
             '0'
             {$ENDIF},
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
  {$IFDEF DOWNLOADER}
  DataProcess.jdn.Add    (Pointer(StrToInt(l.Strings[DATA_PARAM_JDN])))
  {$ELSE}
  DataProcess.jdn.Add    (Pointer(StrToInt('0')))
  {$ENDIF}
  ;
  l.Free;
end;

end.

