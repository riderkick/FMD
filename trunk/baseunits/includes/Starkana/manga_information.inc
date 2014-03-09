function   GetStarkanaInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = TRUE;
  s: String;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[STARKANA_ID,1] + URL + '?mature_confirm=1';
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
  mangaInfo.website:= WebsiteRoots[STARKANA_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('/upload/covers/', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'content=')));

    // get summary
    if (Pos('Summary:', parse.Strings[i]) <> 0) then
    begin
      j:= i+5;
      while (j<parse.Count) AND (Pos('</td>', parse.Strings[j])=0) do
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
    if (Pos('Title(s):', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= StringFilter(TrimRight(TrimLeft(parse.Strings[i+5])));

    // get chapter name and link
    if (Pos('class="download-link"', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+7])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+7<parse.Count) AND (Pos('Creator(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimRight(TrimLeft(parse.Strings[i+7]));

    // get genres
    if (Pos('Genres:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('/manga/search?g', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('Start Date:', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+7<parse.Count) AND (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+7])<>0 then
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

