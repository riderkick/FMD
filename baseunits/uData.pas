{
        File: uData.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uData;

{$mode delphi}
{$DEFINE DOWNLOADER}

// This unit contains all necessary functions for data processing

interface

uses
  Classes, SysUtils, fgl, uBaseUnit;

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
    function    Filter(const checkedGenres, uncheckedGenres: TStringList;
                       const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                       const minusDay: Cardinal;
                       const haveAllChecked, searchNewManga: Boolean): Boolean;
    // realtime search
    function    Search(AMangaName: String): Boolean;
    // get data position
    function    GetPos(const ANodePos: Integer): Integer;

    // en: Remove filter
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
  Dialogs,
  fpJSON, JSONParser,
  synautil,
  FastHTMLParser, LConvEncoding,
  HTMLUtil,
  HTTPSend, SynaCode;

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

  // get directory page number from AnimeA
  {$I includes/AnimeA/directory_page_number.inc}

  // get directory page number from KissManga
  {$I includes/KissManga/directory_page_number.inc}

  // get directory page number from Batoto
  {$I includes/Batoto/directory_page_number.inc}

  // get directory page number from Manga24h
  {$I includes/Manga24h/directory_page_number.inc}

  // get directory page number from VnSharing
  {$I includes/VnSharing/directory_page_number.inc}

  // get directory page number from Hentai2Read
  {$I includes/Hentai2Read/directory_page_number.inc}

  // get directory page number from Fakku
  {$I includes/Fakku/directory_page_number.inc}

  //get directory Pururin
  {$I includes/Pururin/directory_page_number.inc}

  // get directory page number from MangaPark
  {$I includes/MangaPark/directory_page_number.inc}

  function   GetGEHentaiDirectoryPageNumber: Byte;
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

  // get directory page number from MangaFox
  {$I includes/MangaFox/directory_page_number.inc}

  // get directory page number from MangaTraders
  {$I includes/MangaTraders/directory_page_number.inc}

  // get directory page number from MangaGo
  {$I includes/MangaGo/directory_page_number.inc}

  // get directory page number from MangaEden
  {$I includes/MangaEden/directory_page_number.inc}

  // get directory page number from BlogTruyen
  {$I includes/BlogTruyen/directory_page_number.inc}

  // get directory page number from RedHawkScans
  {$I includes/RedHawkScans/directory_page_number.inc}

  // get directory page number from S2scans
  {$I includes/S2scans/directory_page_number.inc}

  // get directory page number from LectureEnLigne
  {$I includes/LectureEnLigne/directory_page_number.inc}

  // get directory page number from MangaAe
  {$I includes/MangaAe/directory_page_number.inc}

  // get directory page number from CentralDeMangas
  {$I includes/CentralDeMangas/directory_page_number.inc}
  
    // get directory page number from Manga2u
  {$I includes/Manga2u/directory_page_number.inc}

  function   GetDM5DirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[DM5_ID,1] + DM5_BROWSER + '/', 0) then
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
      if (Pos('/mangas/list/*/', parse.Strings[i]) > 0) then
      begin
        s:= TrimRight(TrimLeft(GetString(parse.Strings[i], '/mangas/list/*/', '">')));
        page:= StrToInt(s);
        Result:= NO_ERROR;
        exit;
      end;
    end;
    source.Free;
  end;

begin
  source:= TStringList.Create;
  if website = WebsiteRoots[ANIMEA_ID,0] then
    Result:= GetAnimeADirectoryPageNumber
  else
  if website = WebsiteRoots[KISSMANGA_ID,0] then
    Result:= GetKissMangaDirectoryPageNumber
  else
  if website = WebsiteRoots[BATOTO_ID,0] then
    Result:= GetBatotoDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGA24H_ID,0] then
    Result:= GetManga24hDirectoryPageNumber
  else
  if website = WebsiteRoots[VNSHARING_ID,0] then
    Result:= GetVnSharingDirectoryPageNumber
  else
  if website = WebsiteRoots[HENTAI2READ_ID,0] then
    Result:= GetHentai2ReadDirectoryPageNumber
  else
  if website = WebsiteRoots[FAKKU_ID,0] then
    Result:= GetFakkuDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGAPARK_ID,0] then
    Result:= GetMangaParkDirectoryPageNumber
  else
  if website = WebsiteRoots[GEHENTAI_ID,0] then
    Result:= GetGEHentaiDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGAFOX_ID,0] then
    Result:= GetMangaFoxDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGATRADERS_ID,0] then
    Result:= GetMangaTradersDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGAGO_ID,0] then
    Result:= GetMangaGoDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGAEDEN_ID,0] then
    Result:= GetMangaEdenDirectoryPageNumber(WebsiteRoots[MANGAEDEN_ID,1])
  else
  if website = WebsiteRoots[PERVEDEN_ID,0] then
    Result:= GetMangaEdenDirectoryPageNumber(WebsiteRoots[PERVEDEN_ID,1])
  else
  if website = WebsiteRoots[BLOGTRUYEN_ID,0] then
    Result:= GetBlogTruyenDirectoryPageNumber
  else
  if website = WebsiteRoots[REDHAWKSCANS_ID,0] then
    Result:= GetRedHawkScansDirectoryPageNumber
 // else
 // if website = S2SCAN_ID,0] then
 //   Result:= GetS2scanDirectoryPageNumber
 // else
 // if website = LEE_ID,0] then
 //   Result:= GetLEEDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGAAE_ID,0] then
    Result:= GetMangaAeDirectoryPageNumber
  else
  if website = WebsiteRoots[CENTRALDEMANGAS_ID,0] then
    Result:= GetCentralDeMangasDirectoryPageNumber
  else
  if website = WebsiteRoots[MANGA2U_ID,0] then
    Result:= GetManga2uDirectoryPageNumber
  else
  if website = WebsiteRoots[EATMANGA_ID,0] then
  begin
    Result:= NO_ERROR;
    Page:= 1;
  end
  else
  if website = WebsiteRoots[DM5_ID,0] then
    Result:= GetDM5DirectoryPageNumber
  else
  if website = WebsiteRoots[PURURIN_ID,0] then
    Result:= GetPururinDirectoryPageNumber
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

  // get names and links of the manga from Manga2u
  {$I includes/Manga2u/names_and_links.inc}
  
  // get names and links of the manga from AnimeA
  {$I includes/AnimeA/names_and_links.inc}

  // get names and links of the manga from MangaHere
  {$I includes/MangaHere/names_and_links.inc}

  // get names and links of the manga from Es.MangaHere
  {$I includes/EsMangaHere/names_and_links.inc}

  // get names and links of the manga from AnimeExtremist
  {$I includes/AnimExtremist/names_and_links.inc}

  // get names and links of the manga from MangaInn
  {$I includes/MangaInn/names_and_links.inc}

  // get names and links of the manga from KissManga
  {$I includes/KissManga/names_and_links.inc}

  // get names and links of the manga from Batoto
  {$I includes/Batoto/names_and_links.inc}

  // get names and links of the manga from Manga24h
  {$I includes/Manga24h/names_and_links.inc}

  // get names and links of the manga from VnSharing
  {$I includes/VnSharing/names_and_links.inc}

  // get names and links of the manga from Hentai2Read
  {$I includes/Hentai2Read/names_and_links.inc}

  // get names and links of the manga from Fakku
  {$I includes/Fakku/names_and_links.inc}

  // get names and links of the manga from MangaReader
  {$I includes/MangaReader/names_and_links.inc}

  // get names and links of the manga from MangaPark
  {$I includes/MangaPark/names_and_links.inc}

  // get names and links of the manga from MangaFox
  {$I includes/MangaFox/names_and_links.inc}

  // get names and links of the manga from MangaTraders
  {$I includes/MangaTraders/names_and_links.inc}

  // get names and links of the manga from TruyenTranhTuan
  {$I includes/TruyenTranhTuan/names_and_links.inc}

  // get names and links of the manga from SubManga
  {$I includes/SubManga/names_and_links.inc}

  // get names and links of the manga from Komikid
  {$I includes/Komikid/names_and_links.inc}

  // get names and links of the manga from PecintaKomik
  {$I includes/PecintaKomik/names_and_links.inc}

  // get names and links of the manga from Mabuns
  {$I includes/Mabuns/names_and_links.inc}

  // get names and links of the manga from MangaEsta
  {$I includes/MangaEsta/names_and_links.inc}
  
  // get names and links of the manga from Pururin
  {$I includes/Pururin/names_and_links.inc}

  // get names and links of the manga from HugeManga
  {$I includes/HugeManga/names_and_links.inc}

  // get names and links of the manga from AnimeStory
  {$I includes/AnimeStory/names_and_links.inc}

  // get names and links of the manga from LectureEnLigne
  {$I includes/LectureEnLigne/names_and_links.inc}

  // get names and links of the manga from ScanManga
  {$I includes/ScanManga/names_and_links.inc}

  // get names and links of the manga from MangaAr
  {$I includes/MangaAr/names_and_links.inc}

  // get names and links of the manga from MangaAe
  {$I includes/MangaAe/names_and_links.inc}

  // get names and links of the manga from CentralDeMangas
  {$I includes/CentralDeMangas/names_and_links.inc}

  // get names and links of the manga from imanhua
  function   imanhuaGetNamesAndLinks: Byte;
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

  // get names and links of the manga from Turkcraft
  {$I includes/Turkcraft/names_and_links.inc}

  // get names and links of the manga from MangaVadisi
  {$I includes/MangaVadisi/names_and_links.inc}

  // get names and links of the manga from Mangaframe
  {$I includes/MangaFrame/names_and_links.inc}
  
  // get names and links of the manga from Mangacow
  {$I includes/Mangacow/names_and_links.inc}

  // get names and links of the manga from SenManga
  {$I includes/SenManga/names_and_links.inc}

  // get names and links of the manga from Starkana
  {$I includes/Starkana/names_and_links.inc}

  // get names and links of the manga from EatManga
  {$I includes/EatManga/names_and_links.inc}

  // get names and links of the manga from MangaPanda
  {$I includes/MangaPanda/names_and_links.inc}

  // get names and links of the manga from MangaGo
  {$I includes/MangaGo/names_and_links.inc}

  // get names and links of the manga from MangaStream
  {$I includes/MangaStream/names_and_links.inc}

  // get names and links of the manga from RedHawkScans
  {$I includes/RedHawkScans/names_and_links.inc}

  // get names and links of the manga from S2scan
  {$I includes/S2scans/names_and_links.inc}

  // get names and links of the manga from EGScans
  {$I includes/EGScans/names_and_links.inc}

  // get names and links of the manga from blogtruyen
  {$I includes/BlogTruyen/names_and_links.inc}

  // get names and links of the manga from MangaEden
  {$I includes/MangaEden/names_and_links.inc}
  
  // get names and links of the manga from Kivmanga
  {$I includes/Kivmanga/names_and_links.inc}
  
  // get names and links of the manga from Mangacan
  {$I includes/Mangacan/names_and_links.inc}

  // get names and links of the manga from MeinManga
  {$I includes/MeinManga/names_and_links.inc}
    
  // get names and links of the manga from MangasPROJECT
  {$I includes/MangasPROJECT/names_and_links.inc}
  
  // get names and links of the manga from MANGAREADER_POR
  {$I includes/MangaREADER_POR/names_and_links.inc}

  // get names and links of the manga from g.e-hentai
  function   GEHentaiGetNamesAndLinks: Byte;
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
  if website = WebsiteRoots[ANIMEA_ID,0] then
    Result:= AnimeAGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAHERE_ID,0] then
    Result:= MangaHereGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAINN_ID,0] then
    Result:= MangaInnGetNamesAndLinks
  else
  if website = WebsiteRoots[KISSMANGA_ID,0] then
    Result:= KissMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[BATOTO_ID,0] then
    Result:= BatotoGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGA24H_ID,0] then
    Result:= Manga24hGetNamesAndLinks
  else
  if website = WebsiteRoots[VNSHARING_ID,0] then
    Result:= VnSharingGetNamesAndLinks
  else
  if website = WebsiteRoots[HENTAI2READ_ID,0] then
    Result:= Hentai2ReadGetNamesAndLinks
  else
  if website = WebsiteRoots[FAKKU_ID,0] then
    Result:= FakkuGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAREADER_ID,0] then
    Result:= MangaReaderGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAFOX_ID,0] then
    Result:= MangaFoxGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGATRADERS_ID,0] then
    Result:= MangaTradersGetNamesAndLinks
  else
  if website = WebsiteRoots[STARKANA_ID,0] then
    Result:= StarkanaGetNamesAndLinks
  else
  if website = WebsiteRoots[EATMANGA_ID,0] then
    Result:= EatMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAPANDA_ID,0] then
    Result:= MangaPandaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAGO_ID,0] then
    Result:= MangaGoGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGASTREAM_ID,0] then
    Result:= MangaStreamGetNamesAndLinks
  else
  if website = WebsiteRoots[REDHAWKSCANS_ID,0] then
    Result:= RedHawkScansGetNamesAndLinks
  else
  if website = WebsiteRoots[S2SCAN_ID,0] then
    Result:= S2ScanGetNamesAndLinks
  else
  if website = WebsiteRoots[EGSCANS_ID,0] then
    Result:= EGScansGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAEDEN_ID,0] then
    Result:= MangaEdenGetNamesAndLinks(WebsiteRoots[MANGAEDEN_ID,1])
  else
  if website = WebsiteRoots[PERVEDEN_ID,0] then
    Result:= MangaEdenGetNamesAndLinks(WebsiteRoots[PERVEDEN_ID,1])
  else
  if website = WebsiteRoots[MEINMANGA_ID,0] then
    Result:= MeinMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[BLOGTRUYEN_ID,0] then
    Result:= BlogTruyenGetNamesAndLinks
  else
  if website = WebsiteRoots[TRUYENTRANHTUAN_ID,0] then
    Result:= TruyenTranhTuanGetNamesAndLinks
  else
  if website = WebsiteRoots[SUBMANGA_ID,0] then
    Result:= SubMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[ESMANGAHERE_ID,0] then
    Result:= EsMangaHereGetNamesAndLinks
  else
  if website = WebsiteRoots[ANIMEEXTREMIST_ID,0] then
    Result:= AnimeExtremistGetNamesAndLinks
  else
  if website = WebsiteRoots[KOMIKID_ID,0] then
    Result:= KomikidGetNamesAndLinks
  else
  if website = WebsiteRoots[PECINTAKOMIK_ID,0] then
    Result:= PecintaKomikGetNamesAndLinks
  else
  if website = WebsiteRoots[MABUNS_ID,0] then
    Result:= MabunsGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAESTA_ID,0] then
    Result:= MangaEstaGetNamesAndLinks
  else
  if website = WebsiteRoots[PURURIN_ID,0] then
    Result:= PururinGetNamesAndLinks
  else
  if website = WebsiteRoots[HUGEMANGA_ID,0] then
    Result:= HugeMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[ANIMESTORY_ID,0] then
    Result:= AnimeStoryGetNamesAndLinks
  else
  if website = WebsiteRoots[LEE_ID,0] then
    Result:= LEEGetNamesAndLinks
  else
  if website = WebsiteRoots[SCANMANGA_ID,0] then
    Result:= ScanMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAAR_ID,0] then
    Result:= MangaArGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAAE_ID,0] then
    Result:= MangaAeGetNamesAndLinks
  else
  if website = WebsiteRoots[CENTRALDEMANGAS_ID,0] then
    Result:= CentralDeMangasGetNamesAndLinks
 // else
 // if website = WebsiteRoots[MANGAKU_ID,0] then
 //   Result:= MangakuGetNamesAndLinks
  else
  if website = WebsiteRoots[IMANHUA_ID,0] then
    Result:= imanhuaGetNamesAndLinks
  else
  if website = WebsiteRoots[TURKCRAFT_ID,0] then
    Result:= TurkcraftGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAVADISI_ID,0] then
    Result:= MangaVadisiGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAFRAME_ID,0] then
    Result:= MangaframeGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGACOW_ID,0] then
    Result:= MangaCowGetNamesAndLinks
  else
  if website = WebsiteRoots[SENMANGA_ID,0] then
    Result:= SenMangaGetNamesAndLinks
  else
  if website = WebsiteRoots[GEHENTAI_ID,0] then
    Result:= GEHentaiGetNamesAndLinks
  else
  if website = WebsiteRoots[KIVMANGA_ID,0] then
    Result:= KivmangaGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGACAN_ID,0] then
    Result:= MangacanGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGASPROJECT_ID,0] then
    Result:= MangasPROJECTGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGAREADER_POR_ID,0] then
    Result:= MangaREADER_PORGetNamesAndLinks
  else
  if website = WebsiteRoots[MANGA2U_ID,0] then
    Result:= Manga2uGetNamesAndLinks
end;

function    TMangaInformation.GetInfoFromURL(const website, URL: String; const Reconnect: Cardinal): Byte;
var
  source: TStringList;
  Parser: TjsFastHTMLParser;
  s     : String;

// Get manga information from AnimeA
{$I includes/AnimeA/manga_information.inc}

// Get manga information from MangaHere
{$I includes/MangaHere/manga_information.inc}

// get manga infos from SubManga site
// due to its weird designs, this will take a lot of work (and time) for it to
// work property
{$I includes/SubManga/manga_information.inc}

// Get manga information from EsMangHere
{$I includes/EsMangaHere/manga_information.inc}

// Get manga information from AnimExtremist
{$I includes/AnimExtremist/manga_information.inc}

// Get manga information from MangaInn
{$I includes/MangaInn/manga_information.inc}

// Get manga information from KissManga
{$I includes/KissManga/manga_information.inc}

// get manga infos from batoto
{$I includes/Batoto/manga_information.inc}

// get manga infos from Manga24h site
{$I includes/Manga24h/manga_information.inc}

// get manga infos from VnSharing site
{$I includes/VnSharing/manga_information.inc}

// Get h-manga information from Hentai2Read
{$I includes/Hentai2Read/manga_information.inc}

// Get h-manga information from Fakku
{$I includes/Fakku/manga_information.inc}

// get manga infos from mangareader site
{$I includes/MangaReader/manga_information.inc}

// get manga infos from mangapark site
{$I includes/MangaPark/manga_information.inc}

// get manga infos from mangafox site
{$I includes/MangaFox/manga_information.inc}

// get manga infos from mangatraders site
{$I includes/MangaTraders/manga_information.inc}

// get manga infos from mangastream site
{$I includes/MangaStream/manga_information.inc}

// get manga infos from mangaeden site
{$I includes/MangaEden/manga_information.inc}

// get manga infos from Starkana site
{$I includes/Starkana/manga_information.inc}

// get manga infos from eatmanga site
{$I includes/EatManga/manga_information.inc}

// get manga infos from redhawkscans site
{$I includes/RedHawkScans/manga_information.inc}

// get manga infos from s2scan site
{$I includes/S2scans/manga_information.inc}

// get manga infos from EGScans site
{$I includes/EGScans/manga_information.inc}

// get manga infos from mangapanda site
{$I includes/MangaPanda/manga_information.inc}

// get manga infos from mangago site
{$I includes/MangaGo/manga_information.inc}

// get manga infos from truyentranhtuan site
{$I includes/TruyenTranhTuan/manga_information.inc}

// get manga infos from Komikid site
{$I includes/Komikid/manga_information.inc}

// get manga infos from PecintaKomik site
// this site have 2 kind of cover page
{$I includes/PecintaKomik/manga_information.inc}

// get manga infos from Mabuns site
{$I includes/Mabuns/manga_information.inc}

// get manga infos from MangaEsta site
{$I includes/MangaEsta/manga_information.inc}

// get manga infos from Pururin site
{$I includes/Pururin/manga_information.inc}

// get manga infos from HugeManga site
{$I includes/HugeManga/manga_information.inc}

// get manga infos from AnimeStory site
{$I includes/AnimeStory/manga_information.inc}

// get manga infos from LectureEnLigne site
{$I includes/LectureEnLigne/manga_information.inc}

// get manga infos from ScanManga site
{$I includes/ScanManga/manga_information.inc}

// get manga infos from Turkcraft site
{$I includes/Turkcraft/manga_information.inc}

// get manga infos from MangaVadisi site
{$I includes/MangaVadisi/manga_information.inc}

// get manga infos from Mangaframe site
{$I includes/MangaFrame/manga_information.inc}

// get manga infos from MangaAr site
{$I includes/MangaAr/manga_information.inc}

// get manga infos from MangaAe site
{$I includes/MangaAe/manga_information.inc}

// get manga infos from centraldemangas site
{$I includes/CentralDeMangas/manga_information.inc}

// get manga infos from Mangacow site
{$I includes/Mangacow/manga_information.inc}

// get manga infos from SenManga site
{$I includes/SenManga/manga_information.inc}

// get manga infos from blogtruyen site
{$I includes/BlogTruyen/manga_information.inc}

// get manga infos from MeinManga site
{$I includes/MeinManga/manga_information.inc}

// get manga infos from Manga2u site
{$I includes/Manga2u/manga_information.inc}

// get manga infos from g.e-hentai site (dummy)
function   GetGEHentaiInfoFromURL_Dummy: Byte;
begin
  mangaInfo.url:= URL;
  source.Free;
  mangaInfo.website:= WebsiteRoots[GEHENTAI_ID,0];
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

// get manga infos from Kivmanga site
function   GetKivmangaInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[KIVMANGA_ID,1] + URL;
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

  mangaInfo.website:= WebsiteRoots[KIVMANGA_ID,0];
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
      mangaInfo.title:= TrimLeft(TrimRight(GetString(parse.Strings[i+1], 'Read Free Manga Online - ', '-')));

    if (isExtractChapter) AND (Pos('option value=', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= URL + '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
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

// get manga infos from MANGACAN site
{$I includes/Mangacan/manga_information.inc}

// get manga infos from MangasPROJECT site
{$I includes/MangasPROJECT/manga_information.inc}

// get manga infos from MANGAREADER_POR site
{$I includes/MangaREADER_POR/manga_information.inc}

// get manga infos from g.e-hentai site
function   GetGEHentaiInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[GEHENTAI_ID,1] + URL;
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

  mangaInfo.website:= WebsiteRoots[GEHENTAI_ID,0];
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

  mangaInfo.chapterLinks.Add(mangaInfo.url);
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

  if website = WebsiteRoots[ANIMEA_ID,0] then
    Result:= GetAnimeAInfoFromURL
  else
  if website = WebsiteRoots[MANGAHERE_ID,0] then
    Result:= GetMangaHereInfoFromURL
  else
  if website = WebsiteRoots[MANGAINN_ID,0] then
    Result:= GetMangaInnInfoFromURL
  else
  if website = WebsiteRoots[KISSMANGA_ID,0] then
    Result:= GetKissMangaInfoFromURL
  else
  if website = WebsiteRoots[BATOTO_ID,0] then
    Result:= GetBatotoInfoFromURL
  else
  if website = WebsiteRoots[MANGA24H_ID,0] then
    Result:= GetManga24hInfoFromURL
  else
  if website = WebsiteRoots[VNSHARING_ID,0] then
    Result:= GetVnSharingInfoFromURL
  else
  if website = WebsiteRoots[HENTAI2READ_ID,0] then
    Result:= GetHentai2ReadInfoFromURL
  else
  if website = WebsiteRoots[FAKKU_ID,0] then
    Result:= GetFakkuInfoFromURL
  else
  if website = WebsiteRoots[MANGAREADER_ID,0] then
    Result:= GetMangaReaderInfoFromURL
  else
  if website = WebsiteRoots[MANGAPARK_ID,0] then
    Result:= GetMangaParkInfoFromURL
  else
  if website = WebsiteRoots[MANGAFOX_ID,0] then
    Result:= GetMangaFoxInfoFromURL
  else
  if website = WebsiteRoots[MANGATRADERS_ID,0] then
    Result:= GetMangaTradersInfoFromURL
  else
  if website = WebsiteRoots[STARKANA_ID,0] then
    Result:= GetStarkanaInfoFromURL
  else
  if website = WebsiteRoots[EATMANGA_ID,0] then
    Result:= GetEatMangaInfoFromURL
  else
  if website = WebsiteRoots[MANGAPANDA_ID,0] then
    Result:= GetMangaPandaInfoFromURL
  else
  if website = WebsiteRoots[MANGAGO_ID,0] then
    Result:= GetMangaGoInfoFromURL
  else
  if website = WebsiteRoots[MANGASTREAM_ID,0] then
    Result:= GetMangaStreamInfoFromURL
  else
  if website = WebsiteRoots[REDHAWKSCANS_ID,0] then
    Result:= GetRedHawkScansInfoFromURL
  else
  if website = WebsiteRoots[S2SCAN_ID,0] then
    Result:= GetS2scanInfoFromURL
  else
  if website = WebsiteRoots[EGSCANS_ID,0] then
    Result:= GetEGScansInfoFromURL
  else
  if website = WebsiteRoots[TRUYENTRANHTUAN_ID,0] then
    Result:= GetTruyenTranhTuanInfoFromURL
  else
  if website = WebsiteRoots[MANGAEDEN_ID,0] then
    Result:= GetMangaEdenInfoFromURL(WebsiteRoots[MANGAEDEN_ID,1])
  else
  if website = WebsiteRoots[PERVEDEN_ID,0] then
    Result:= GetMangaEdenInfoFromURL(WebsiteRoots[PERVEDEN_ID,1])
  else
  if website = WebsiteRoots[MEINMANGA_ID,0] then
    Result:= GetMeinMangaInfoFromURL
  else
  if website = WebsiteRoots[MANGA2U_ID,0] then
    Result:= GetManga2uInfoFromURL
  else
  if website = WebsiteRoots[SUBMANGA_ID,0] then
    Result:= GetSubMangaInfoFromURL
  else
  if website = WebsiteRoots[ESMANGAHERE_ID,0] then
    Result:= GetEsMangaHereInfoFromURL
  else
  if website = WebsiteRoots[ANIMEEXTREMIST_ID,0] then
    Result:= GetAnimeExtremistInfoFromURL
  else
  if website = WebsiteRoots[KOMIKID_ID,0] then
    Result:= GetKomikidInfoFromURL
  else
  if website = WebsiteRoots[PECINTAKOMIK_ID,0] then
    Result:= GetPecintaKomikInfoFromURL
  else
  if website = WebsiteRoots[MABUNS_ID,0] then
    Result:= GetMabunsInfoFromURL
  else
  if website = WebsiteRoots[MANGAESTA_ID,0] then
    Result:= GetMangaEstaInfoFromURL
  else
  if website = WebsiteRoots[PURURIN_ID,0] then
    Result:= GetPururinInfoFromURL
  else
  if website = WebsiteRoots[HUGEMANGA_ID,0] then
    Result:= GetHugeMangaInfoFromURL
  else
  if website = WebsiteRoots[ANIMESTORY_ID,0] then
    Result:= GetAnimeStoryInfoFromURL
  else
  if website = WebsiteRoots[LEE_ID,0] then
    Result:= GetLEEInfoFromURL
  else
  if website = WebsiteRoots[SCANMANGA_ID,0] then
    Result:= GetScanMangaInfoFromURL
  else
  if website = WebsiteRoots[TURKCRAFT_ID,0] then
    Result:= GetTurkcraftInfoFromURL
  else
  if website = WebsiteRoots[MANGAFRAME_ID,0] then
    Result:= GetMangaframeInfoFromURL
  else
  if website = WebsiteRoots[MANGAVADISI_ID,0] then
    Result:= GetMangaVadisiInfoFromURL
  else
  if website = WebsiteRoots[MANGAAR_ID,0] then
    Result:= GetMangaArInfoFromURL
  else
  if website = WebsiteRoots[MANGAAE_ID,0] then
    Result:= GetMangaAeInfoFromURL
  else
  if website = WebsiteRoots[CENTRALDEMANGAS_ID,0] then
    Result:= GetCentralDeMangasInfoFromURL
  else
  if website = WebsiteRoots[MANGACOW_ID,0] then
    Result:= GetMangaCowInfoFromURL
  else
  if website = WebsiteRoots[SENMANGA_ID,0] then
    Result:= GetSenMangaInfoFromURL
  else
  if website = WebsiteRoots[BLOGTRUYEN_ID,0] then
    Result:= GetBlogTruyenInfoFromURL
  else
  if website = WebsiteRoots[KIVMANGA_ID,0] then
    Result:= GetKivmangaInfoFromURL
  else
  if website = WebsiteRoots[MANGACAN_ID,0] then
    Result:= GetMangacanInfoFromURL
  else
  if website = WebsiteRoots[MANGASPROJECT_ID,0] then
    Result:= GetMangasPROJECTInfoFromURL
  else
   if website = WebsiteRoots[MANGAREADER_POR_ID,0] then
    Result:= GetMangaREADER_PORInfoFromURL
  else
  if website = WebsiteRoots[GEHENTAI_ID,0] then
  begin
    case isGetByUpdater of
      TRUE:  Result:= GetGEHentaiInfoFromURL_Dummy;
      FALSE: Result:= GetGEHentaiInfoFromURL;
    end;
  end;

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

