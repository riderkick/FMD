{
        File: data.pas
        License: GPL/LGPL
        This unit is part of Free Manga Downloader
}

unit data;

{$mode delphi}

// EN: This unit contains all necessary functions for data processing
// VI: Unit chứa tất cả các hàm liên quan tới xử lý dữ liệu

interface

uses
  Classes, SysUtils, baseunit, HTTPSend, Dialogs;

type
  TDataProcess = class(TObject)
  private
    function    GetInfo(const index: Cardinal): TStringList;
    function    GetParam(const index, paramNo: Cardinal): AnsiString;
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
    destructor  Destroy;
    function    FirstParam(const index: Cardinal): AnsiString;

    // en: Break data into parts... This may be considered as bad coding, but
    //     it's for faster filter
    // vi: Thử tục này sẽ break data nhằm tăng tốc cho filter
    procedure   BreakDataToParts(const i: Cardinal);

    procedure   LoadFromFile(const website: String);
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
    property    Param[index, paramNo: Cardinal]: AnsiString read GetParam;
  end;

  TMangaInformation = class(TObject)
    mangaInfo: TMangaInfo;
    parse    : TStringList;
    procedure   OnTag (tag : String);
    procedure   OnText(text: String);
    constructor Create;
    destructor  Destroy;
    function    GetNameAndLink(const names, links: TStringList;
                               const website, URL: AnsiString): Byte;
    function    GetInfoFromURL(const website, URL: AnsiString): Byte;
    procedure   SyncInfoToData(const DataProcess: TDataProcess; const index: Cardinal);
    // Only use this function for update manga list
    procedure   AddInfoToData(const name, link : AnsiString;
                              const DataProcess: TDataProcess);
  end;

var
  options: TStringList;

implementation

uses FastHTMLParser, HTMLUtil;

// ----- TDataProcess -----

constructor TDataProcess.Create;
begin
  isFiltered:= FALSE;
  Data      := TStringList.Create;

  Title     := TStringList.Create;
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
  Title.Clear;
  Authors.Clear;
  Artists.Clear;
  Genres.Clear;
  Status.Clear;
  Summary.Clear;
  Data.Clear;

  Title.Free;
  Authors.Free;
  Artists.Free;
  Genres.Free;
  Status.Free;
  Summary.Free;

  Data.Free;
  inherited Destroy;
end;

function    TDataProcess.FirstParam(const index: Cardinal): AnsiString;
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

function    TDataProcess.GetParam(const index, paramNo: Cardinal): AnsiString;
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
  Authors.Strings[i]:= l.Strings[DATA_PARAM_AUTHORS];
  Artists.Strings[i]:= l.Strings[DATA_PARAM_ARTISTS];
  Genres.Strings [i]:= l.Strings[DATA_PARAM_GENRES];
  Status.Strings [i]:= l.Strings[DATA_PARAM_STATUS];
  Summary.Strings[i]:= l.Strings[DATA_PARAM_SUMMARY];

  l.Free;
end;

procedure   TDataProcess.LoadFromFile(const website: String);
var
  i: Cardinal;
  l: TStringList;
  Filename: String;
begin
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
    authors.Add(l.Strings[DATA_PARAM_AUTHORS]);
    artists.Add(l.Strings[DATA_PARAM_ARTISTS]);
    genres.Add (l.Strings[DATA_PARAM_GENRES]);
    status.Add (l.Strings[DATA_PARAM_STATUS]);
    summary.Add(l.Strings[DATA_PARAM_SUMMARY]);
  end;
  l.Free;
end;

procedure   TDataProcess.SaveToFile(const website: String);
begin
  QuickSortData(data);
  data.SaveToFile(DATA_FOLDER+website+DATA_EXT);
end;

procedure   TDataProcess.SaveToFile;
begin
  //QuickSortData(data);
  data.SaveToFile(Filename+DATA_EXT);
end;

function    TDataProcess.Filter(const checkedGenres, uncheckedGenres: TStringList;
                                const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
                                const haveAllChecked: Boolean): Boolean;
var
  i, j, fpos, count: Cardinal;
  s                : AnsiString;
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

function    TMangaInformation.GetNameAndLink(const names, links: TStringList;
                                             const website, URL: AnsiString): Byte;
var
  source: TStringList;

  function   AnimeAGetNameAndLink: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    for i:= 0 to source.Count-1 do
    begin
      if Pos('manga_img', source.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        links.Add(GetString(source.Strings[i], '"', '"'));
        names.Add(GetString(source.Strings[i], 'title="', ' Manga"'));
      end;
    end;
  end;
begin
  source:= TStringList.Create;
  if website = ANIMEA_NAME then
  begin
    if NOT GetPage(TObject(source), ANIMEA_ROOT + ANIMEA_BROWSER + URL, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    Result:= AnimeAGetNameAndLink;
  end;
  source.Free;
end;

function    TMangaInformation.GetInfoFromURL(const website, URL: AnsiString): Byte;
var
  source: TStringList;
  Parser: TjsFastHTMLParser;
  s     : String;

function   GetAnimeAInfoFromURL: Byte;
var
  i, j: Cardinal;
begin
  if NOT GetPage(TObject(source), ANIMEA_ROOT + URL + ANIMEA_SKIP, 1) then
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

  mangaInfo.website:= 'AnimeA';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then begin exit end;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='manga_img_big') then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src'));

      // get summary
    if (parse.Strings[i]='<p>') then
    begin
      j:= 1;
      mangaInfo.summary:= '';
      while parse.Strings[i+j] <> '</p>' do
      begin
        mangaInfo.summary:= mangaInfo.summary+parse.Strings[i+j];
        Inc(j);
      end;
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

  // using regular expressions (authors, artists, status, genres)
  for i:= 0 to source.Count-1 do
  begin
    // get authors
    if (Pos('Author(s):', source.Strings[i])<>0) then
      mangaInfo.authors:= GetString(source.Strings[i], '''s">', '<');

    // get artists
    if (Pos('Artist(s):', source.Strings[i])<>0) then
      mangaInfo.artists:= GetString(source.Strings[i], '</strong>', '<');

    // get genres
    if (Pos('/genre/', source.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 38 do
        if Pos(LowerCase(Genre[j]), LowerCase(source.Strings[i]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(Genre[j]+', ');
    end;

    // get status
    if (Pos('Status:', source.Strings[i])<>0) then
    begin
      if Pos('Ongoing', source.Strings[i])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
      if Pos('Completed', source.Strings[i])<>0 then
        mangaInfo.status:= '0';  // completed
      break;
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
  source:= TStringList.Create; source.Clear;
  mangaInfo.coverLink := '';
  mangaInfo.numChapter:= 0;
  mangaInfo.chapterName.Clear;
  mangaInfo.chapterLinks.Clear;

  if website = ANIMEA_NAME then
    Result:= GetAnimeAInfoFromURL;
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

procedure   TMangaInformation.AddInfoToData(const name, link : AnsiString;
                                            const DataProcess: TDataProcess);
begin
  DataProcess.Data.Add(SetParams(
             [name,
             link,
             mangaInfo.authors,
             mangaInfo.artists,
             mangaInfo.genres,
             mangaInfo.status,
             StringFilter(mangaInfo.summary),
             IntToStr(mangaInfo.numChapter),
             ConvertInt32ToStr(GetCurrentJDN),
            '0']));
end;

end.

