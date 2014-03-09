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
  mangaInfo.website:= WebsiteRoots[REDHAWKSCANS_ID,0];
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
