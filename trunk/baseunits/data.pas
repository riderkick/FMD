{
        File: data.pas
        License: GPLv3
        This unit is part of Free Manga Downloader
}

unit data;

{$mode delphi}

// EN: This unit contains all necessary functions for data processing
// VI: Unit chứa tất cả các hàm liên quan tới xử lý dữ liệu

interface

uses
  Classes, SysUtils, baseunit;

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
                       const haveAllChecked: Boolean): Boolean;

    // en: Remove filter
    // vi: Xóa bỏ filter
    procedure   RemoveFilter;
    procedure   Sort;
    property    Info [index: Cardinal]: TStringList read GetInfo;
    property    Param[index, paramNo: Cardinal]: String read GetParam;
  end;

  TMangaInformation = class(TObject)
  public
    mangaInfo: TMangaInfo;
    parse    : TStringList;
    procedure   OnTag (tag : String);
    procedure   OnText(text: String);
    constructor Create;
    destructor  Destroy; override;
    function    GetDirectoryPage(out Page: Cardinal;
                                 const website: String): Byte;
    function    GetNameAndLink(const names, links: TStringList;
                               const website, URL: String): Byte;
    function    GetInfoFromURL(const website, URL: String): Byte;
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

uses FastHTMLParser, HTMLUtil, HTTPSend, SynaCode;

// ----- TDataProcess -----

constructor TDataProcess.Create;
begin
  isFiltered:= FALSE;
  Data      := TStringList.Create;

  Title     := TStringList.Create;
  Link      := TStringList.Create;
  Authors   := TStringList.Create;
  Artists   := TStringList.Create;
  Genres    := TStringList.Create;
  Status    := TStringList.Create;
  Summary   := TStringList.Create;

  filterMark:= TByteList.Create;
  filterPos := TCardinalList.Create;
  inherited Create;
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

  l.Free;
end;

function   TDataProcess.LoadFromFile(const website: String): Boolean;
var
  i: Cardinal;
  l: TStringList;
  Filename: String;
begin
  Filename:= DATA_FOLDER+website;
  if NOT FileExists(Filename+DATA_EXT) then exit(FALSE);
  l:= TStringList.Create;
  data.Clear;
  filterMark.Clear;
  filterPos .Clear;

  title.Clear;
  authors.Clear;
  artists.Clear;
  genres.Clear;
  status.Clear;
  summary.Clear;

  Filename:= DATA_FOLDER+website;

  self.Filename:= Filename;

  data.LoadFromFile(Filename+DATA_EXT);
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
  end;
  l.Free;
  Result:= TRUE;
end;

procedure   TDataProcess.SaveToFile(const website: String);
begin
  QuickSortData(data);
  data.SaveToFile(DATA_FOLDER+website+DATA_EXT);
end;

procedure   TDataProcess.SaveToFile;
begin
  QuickSortData(data);
  data.SaveToFile(Filename+DATA_EXT);
end;

function    TDataProcess.Filter(const checkedGenres, uncheckedGenres: TStringList;
                                const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                                const haveAllChecked: Boolean): Boolean;
var
  i, j, fpos, count: Cardinal;
  s                : String;
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
      (uncheckedGenres.Count = 0)) then
    exit;

    // ugly filter code but quite fast
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

    if checkedGenres.Count <> 0 then
    begin
      for i:= 0 to filterPos.Count - 1 do
      begin
        fpos:= filterPos.Items[i];
        if (filterMark.Items[fpos] = FILTER_SHOW) then
        begin
          s:= LowerCase(Genres.Strings[fpos]);
          if haveAllChecked then
          begin
            count:= checkedGenres.Count;
            for j:= 0 to checkedGenres.Count-1 do
              if Pos(LowerCase(checkedGenres.Strings[j]+','), s) <> 0 then
                Dec(count);
                if count > 0 then
                  filterMark.Items[fpos]:= FILTER_HIDE;
          end
          else
          begin
            filterMark.Items[fpos]:= FILTER_HIDE;
            for j:= 0 to checkedGenres.Count-1 do
              if Pos(LowerCase(checkedGenres.Strings[j]+','), s) <> 0 then
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
              if Pos(LowerCase(uncheckedGenres.Strings[j]+','), s) = 0 then
                Dec(count);
            if count > 0 then
              filterMark.Items[fpos]:= FILTER_HIDE;
          end
          else
          begin
            for j:= 0 to uncheckedGenres.Count-1 do
              if Pos(LowerCase(uncheckedGenres.Strings[j]+','), s) <> 0 then
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
  parse:= TStringList.Create;
  mangaInfo.chapterName := TStringList.Create;
  mangaInfo.chapterLinks:= TStringList.Create;
  inherited Create;
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

function    TMangaInformation.GetDirectoryPage(out Page: Cardinal;
                                               const website: String): Byte;
var
  s     : String;
  source: TStringList;
  Parser: TjsFastHTMLParser;

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

begin
  source:= TStringList.Create;
  if website = ANIMEA_NAME then
    Result:= GetAnimeADirectoryPage
  else
  if website = VNSHARING_NAME then
    Result:= GetVnSharingDirectoryPage
  else
  if website = HENTAI2READ_NAME then
    Result:= GetHentai2ReadDirectoryPage
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

  function   VnSharingGetNameAndLink: Byte;
  var
    i: Cardinal;
    s: String;

   { function  Truncated(const s: String): String;
    var
      p: Cardinal;
    begin
      Result:= s;
      p:= Pos('?id', Result);
      if p > 0 then
        Delete(Result, p, Length(Result)-p+1);
     // Result:= UTF8ToANSI(Result);//StringReplace(Result, ''''+#226#128#153, '''', [rfReplaceAll]);
    end; }

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
        if s <> '/Truyen/Tenki-Yohou-no-Koibito?id=506' then
        begin
          links.Add(s);
          s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    source.Free;
  end;

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
  if website = VNSHARING_NAME then
    Result:= VnSharingGetNameAndLink
  else
  if website = HENTAI2READ_NAME then
    Result:= Hentai2ReadGetNameAndLink;
end;

function    TMangaInformation.GetInfoFromURL(const website, URL: String): Byte;
var
  source: TStringList;
  Parser: TjsFastHTMLParser;
  s     : String;

function   GetAnimeAInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractGenres: Boolean = FALSE;
begin
  if NOT GetPage(TObject(source), ANIMEA_ROOT + URL + ANIMEA_SKIP, 0) then
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
      mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title'))+' '+parse.Strings[i+3])));
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
  if NOT GetPage(TObject(source), MANGAHERE_ROOT + URL, 0) then
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
      mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i+1])));
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
  if NOT GetPage(TObject(source), MANGAINN_ROOT + URL, 0) then
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
        mangaInfo.chapterName.Add(RemoveSymbols(parse.Strings[i+2] + parse.Strings[i+4]));
      end;

    // get authors
    if (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= (parse.Strings[i+4]);

    // get artists
    if (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= (parse.Strings[i+4]);

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
  if NOT GetPage(TObject(source), OURMANGA_ROOT + URL, 0) then
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
    // get summary
    if (Pos('Summary:', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+2;
      while Pos('</p>', parse.Strings[j])=0 do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j];
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
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i+1])));
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

function   GetVnSharingInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  if NOT GetPage(TObject(source), VNSHARING_ROOT + URL + '&confirm=yes', 0) then
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
  isExtractSummary : Boolean = FALSE;
  i, j: Cardinal;
begin
  if NOT GetPage(TObject(source), HENTAI2READ_ROOT + URL, 0) then
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
    // get cover link
    if GetTagName(parse.Strings[i]) = 'div' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cover') then
      begin
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
        s:= mangaInfo.coverLink;
      end;

    // get summary
    if isExtractSummary then
    begin
      s:= parse.Strings[i];
      if s[1] <> '<' then
      begin
        parse.Strings[i]:= StringFilter(parse.Strings[i]);
        parse.Strings[i]:= StringReplace(parse.Strings[i], #10, '\n', [rfReplaceAll]);
        parse.Strings[i]:= StringReplace(parse.Strings[i], #13, '\r', [rfReplaceAll]);
        mangaInfo.summary:= mangaInfo.summary+parse.Strings[i]+'\n\r';
      end
      else
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='box') then
        isExtractSummary:= FALSE;
    end;

    if (Pos('Hentai Summary', parse.Strings[i])) <> 0 then
    begin
      isExtractSummary:= TRUE;
      mangaInfo.summary:= '';
    end;

    // get chapter name and links
    if isExtractChapters then
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND (i+1 < parse.Count-1) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]);
        parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
        parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
        parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
        parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
        s:= RemoveSymbols(parse.Strings[i+1]);
        mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i+1])));
      end
      else
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='right') then
        isExtractChapters:= FALSE;
    end;

    if Pos('Hentai Chapters', parse.Strings[i]) > 0 then
      isExtractChapters:= TRUE;

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
    if (Pos('Status:', parse.Strings[i])<>0) AND (i+4 <= parse.Count-1) then
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
  if website = VNSHARING_NAME then
    Result:= GetVnSharingInfoFromURL
  else
  if website = HENTAI2READ_NAME then
    Result:= GetHentai2ReadInfoFromURL;
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
  l.Free;
end;

end.

