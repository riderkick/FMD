function   GetPecintaKomikInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
  k   : Integer;
begin
  mangaInfo.url:= WebsiteRoots[PECINTAKOMIK_ID,1] + URL;
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
  mangaInfo.website:= PECINTAKOMIK_NAME;
  mangaInfo.status:= '1';
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  if Pos('/manga/', URL) = 0 then
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('id="mangaimg"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= WebsiteRoots[PECINTAKOMIK_ID,1] + CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));

    // get summary
    if (Pos('Himbauan:', parse.Strings[i]) <> 0) then
    begin
      j:= i+4;
      while (j<parse.Count) AND (Pos('</tr>', parse.Strings[j])=0) do
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
    if (Pos('Nama:', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= StringFilter(TrimRight(TrimLeft(parse.Strings[i+6])));

    // get chapter name and links
    if (NOT isExtractChapter) AND
       (Pos('id="chapterlist"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('/manga/', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Autor:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimRight(TrimLeft(parse.Strings[i+4]));

    // get artists
    if  (i+4<parse.Count) AND (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimRight(TrimLeft(parse.Strings[i+4]));

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
  end
  else
  for i:= 0 to parse.Count-1 do
  begin
    if Pos('name="chapter"', parse.Strings[i]) > 0 then
    begin
      if TryStrToInt(TrimRight(TrimLeft(parse.Strings[i+2])), k) then
        for j:= k downto 1 do
        begin
          Inc(mangaInfo.numChapter);
          s:= URL + IntToStr(j);
          mangaInfo.chapterLinks.Add(s);
          s:= 'Chapter ' + IntToStr(j);
          mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
        end;
      break;
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
