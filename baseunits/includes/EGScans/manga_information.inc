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

  mangaInfo.website:= WebsiteRoots[EGSCANS_ID,0];
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
