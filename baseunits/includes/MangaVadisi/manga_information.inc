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

  mangaInfo.website:= WebsiteRoots[MANGAVADISI_ID,0];
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
