function   GetScanMangaInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = TRUE;
  i, j: Cardinal;
begin
  if Pos('http://', URL) = 0 then
    mangaInfo.url:= WebsiteRoots[SCANMANGA_ID,1] + URL
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
  mangaInfo.website:= WebsiteRoots[SCANMANGA_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('rel="image_src"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));

    // get title
    if (Pos('id=''ambubble''', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(TrimRight(HTMLEntitiesFilter(parse.Strings[i])));

    // get chapter name and links
    if (Pos('class=''chapitre_nom''', parse.Strings[i])>0) AND
       (Pos('<strong>', parse.Strings[i+1])>0) then
    begin
    //  Inc(mangaInfo.numChapter);
    //  s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+18], 'href=')), WebsiteRoots[SCANMANGA_ID,1], '', []);
    //  mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (Pos('chapitre_lectureonline', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[SCANMANGA_ID,1], '', []);
      s:= StringReplace(TrimLeft(TrimRight(s)), '"', '', [rfReplaceAll]);
      mangaInfo.chapterLinks.Add(s);
    //  s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2])));
    //  mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    // get summary
    if (Pos('itemprop="description"', parse.Strings[i]) <> 0) then
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
    end;

    // get authors
    if  (i+2<parse.Count) AND
        (Pos('itemprop="author"', parse.Strings[i])<>0) then
      mangaInfo.authors:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));

    // get artists
    //if (i+5<parse.Count) AND (Pos('Illustrateur :', parse.Strings[i])<>0) then
    //  mangaInfo.artists:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+5])));

    // get genres
    if (Pos('itemprop="genre"', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= mangaInfo.genres + HTMLEntitiesFilter(TrimLeft(TrimRight(parse.Strings[i+1]))) + ', ';
    end;

    if isExtractGenres then
    begin
      if Pos('class=''tTip''>', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + HTMLEntitiesFilter(TrimLeft(TrimRight(parse.Strings[i+1]))) + ', ';
      if Pos('itemprop="editor"', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+11<parse.Count) AND (Pos('itemprop="editor"', parse.Strings[i])<>0) then
    begin
      if (Pos('Termin', parse.Strings[i+5])<>0) OR
         (Pos('One Shot', parse.Strings[i+5])<>0) then
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
