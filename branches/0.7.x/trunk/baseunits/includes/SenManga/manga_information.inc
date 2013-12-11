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
