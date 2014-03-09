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
  mangaInfo.website:= WebsiteRoots[MANGAREADER_ID,0];
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
