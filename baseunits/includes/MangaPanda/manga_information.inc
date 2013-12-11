function   GetMangaPandaInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
begin
  if Pos(WebsiteRoots[MANGAPANDA_ID,1], URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MANGAPANDA_ID,1] + URL
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
  mangaInfo.website:= WebsiteRoots[MANGAPANDA_ID,0];
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
        s:= StringReplace(s, '-1/', SEPERATOR2, []);
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
