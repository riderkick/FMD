function   GetMangaFoxInfoFromURL: Byte;
var
  s, s2: String;
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
  mangaInfo.website:= WebsiteRoots[MANGAFOX_ID,0];
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
      // ((Pos('class="title nowrap"', parse.Strings[i])>0 OR
        (Pos('class="tips"', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetString(parse.Strings[i], 'href="', '/1.html"'), WebsiteRoots[MANGAFOX_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s2:= TrimLeft(TrimRight(parse.Strings[i+5]));
      if (s2<>'') AND (s2[1]='<') then
        s2:= '';
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1]))+ ' '+s2);
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
