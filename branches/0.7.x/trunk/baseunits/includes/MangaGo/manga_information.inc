function   GetMangaGoInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  s: String;
  i, j: Cardinal;
begin
  if Pos(WebsiteRoots[MANGAGO_ID,1], URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MANGAGO_ID,1] + URL
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
  mangaInfo.website:= WebsiteRoots[MANGAGO_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'meta') AND
       (Pos('property="og:image"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'content=')));

    // get title
    if (Pos('name="description"', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= StringReplace(TrimLeft(StringFilter(parse.Strings[i+3])), ' manga - Mangago', '', []);

    // get summary
    if (Pos('class="left manga_summary"', parse.Strings[i]) <> 0) then
    begin
      j:= i+1;
      while (j<parse.Count) AND (Pos('</div>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= TrimLeft(TrimRight(StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll])));
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
    end;

    if (NOT isExtractChapter) AND
       (Pos('Chapter Name', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('class="chico"', parse.Strings[i]) > 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[i], 'href="', '"');
      s:= StringReplace(s, WebsiteRoots[MANGAGO_ID,1], '', []);
      if Pos('/1.html', s) > 0 then
        s:= StringReplace(s+'~!@', '/1.html~!@', '/', []);
      if s[Length(s)] <> '/' then
        s:= s+'/';
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

   { // get authors
    if  (i+4<parse.Count) AND
        (Pos('Author', parse.Strings[i])<>0) AND
        (Pos('<th>', parse.Strings[i-1])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if  (i+4<parse.Count) AND
        (Pos('Author', parse.Strings[i])<>0) AND
        (Pos('<th>', parse.Strings[i-1])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]); }

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('/genre/', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</td>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if  (i+4<parse.Count) AND
        (Pos('Status', parse.Strings[i])<>0) AND
        (Pos('<th>', parse.Strings[i-1])<>0) then
    begin
      if Pos('Completed', parse.Strings[i+6])<>0 then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
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
