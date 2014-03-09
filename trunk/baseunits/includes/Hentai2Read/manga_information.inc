function   GetHentai2ReadInfoFromURL: Byte;
var
  s: String;
  isExtractChapters: Boolean = FALSE;
  isExtractGenres  : Boolean = FALSE;
  isExtractSummary : Boolean = TRUE;
  i, j: Cardinal;
begin
  mangaInfo.url:= HENTAI2READ_ROOT + URL;
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
  mangaInfo.website:= WebsiteRoots[HENTAI2READ_ID,0];

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get title
    if (mangaInfo.title = '') AND
       (Pos('meta name="description" content="', parse.Strings[i])>0) then
      mangaInfo.title:= GetString(parse.Strings[i], 'meta name="description" content="', ' hentai chapters');

    // get cover link
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (i<parse.Count-3) then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cover') then
      begin
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src='));
        if mangaInfo.coverLink = 'http://hentai2read.com/wp-content/hentai/cover/tbn/001_1535_233x0.jpg' then
          mangaInfo.coverLink:= '';
      end;

    // get chapter name and links
    if isExtractChapters then
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND (i < parse.Count-2) then
      begin
        Inc(mangaInfo.numChapter);
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]);
        s:= StringReplace(s, HENTAI2READ_MROOT, '', [rfReplaceAll]);
        mangaInfo.chapterLinks.Add(s);
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), HENTAI2READ_ROOT, '', [rfReplaceAll]);
        parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
        parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
        parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
        parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
        s:= RemoveSymbols(parse.Strings[i+1]);
        mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))));
      end
      else
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='right') then
        isExtractChapters:= FALSE;
    end;

    // get summary
    if (Pos('Hentai Summary', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+5;
      mangaInfo.summary:= '';
      while (j<parse.Count) AND (Pos('<div class="box">', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary+parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    if Pos('Hentai Chapters', parse.Strings[i]) > 0 then
      isExtractChapters:= TRUE;

    // get authors
    if (Pos('Author(s):', parse.Strings[i])<>0) AND (i<parse.Count-6) then
      mangaInfo.authors:= parse.Strings[i+5];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) AND (i<parse.Count-6) then
      mangaInfo.artists:= parse.Strings[i+5];

    // get genres
    if (Pos('Genre(s):', parse.Strings[i])<>0) AND (i<parse.Count-6) then
    begin
      mangaInfo.genres:= '';
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) = 'a' then
        mangaInfo.genres:= TrimLeft(TrimRight(mangaInfo.genres+parse.Strings[i+1]))+', '
      else
      if (GetTagName(parse.Strings[i]) = 'div') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='box') then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) AND (i <= parse.Count-5) then
    begin
      if Pos('Ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
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
