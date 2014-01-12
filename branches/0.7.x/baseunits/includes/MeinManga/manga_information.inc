function   GetMeinMangaInfoFromURL: Byte;
var
  s, s2: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
  URLs: TStringList;

  procedure GetChapterTitleAndChapterURL;
  begin
    if (Pos('/default/images/book.jpg', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetString(parse.Strings[i+2], 'href="', '">'), WebsiteRoots[MEINMANGA_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(Trim(parse.Strings[i+3]));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;
  end;

begin
  if Pos(WebsiteRoots[MEINMANGA_ID,1], URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MEINMANGA_ID,1] + URL
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
  mangaInfo.website:= MEINMANGA_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  URLs:= TStringList.Create;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('/pic/logo/', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('Kurzbeschreibung', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+6;
      while (j<parse.Count) AND (Pos('</div>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if (s[1] <> '<') then
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
    if (mangaInfo.title = '') AND (Pos('class="chrname"', parse.Strings[i])<>0) then
      mangaInfo.title:= Trim(StringFilter(parse.Strings[i+1]));

    // Get chapter title and url
    GetChapterTitleAndChapterURL;

    // get authors
    if (i+4<parse.Count) AND
       (Pos('Autor', parse.Strings[i])<>0) AND
       (Pos('valign="top"', parse.Strings[i-1])<>0) then
       mangaInfo.authors:= Trim(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND
       (Pos('Zeichner', parse.Strings[i])<>0) AND
       (Pos('valign="top"', parse.Strings[i-1])<>0) then
       mangaInfo.artists:= Trim(parse.Strings[i+4]);

    // get genres
    if (i+4<parse.Count) AND
       (Pos('Genre', parse.Strings[i])<>0) AND
       (Pos('valign="top"', parse.Strings[i-1])<>0) then
       mangaInfo.genres:= Trim(parse.Strings[i+4]);

    // get status
    if (i+4<parse.Count) AND
       (Pos('Status', parse.Strings[i])<>0) AND
       (Pos('valign="top"', parse.Strings[i-1])<>0) then
    begin
      if Pos('ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;

    // Get number of page
    if (URLs.Count = 0) AND
       (Pos('class="gotopage"', parse.Strings[i]) <> 0) AND
       (Pos('class="hover"', parse.Strings[i+2]) <> 0) then
    begin
      j:= i+3;
      while (j<parse.Count) AND
            ((Pos('class="next"', parse.Strings[j])=0) AND
             (Pos('</div>', parse.Strings[j])=0)) do
      begin
        if (Pos('<a', parse.Strings[j]) <> 0) then
        begin
          URLs.Add(GetString(parse.Strings[j], 'href="', '">'));
        end;
        Inc(j);
      end;
    end;
  end;

  // Repeat until we get all chapter information
  if URLs.Count > 0 then
  begin
    for j:= 0 to URLs.Count-1 do
    begin
      source:= TStringList.Create;
      if NOT GetPage(TObject(source), URLs.Strings[j], Reconnect) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;

      parse.Clear;
      Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText:= OnText;
      Parser.Exec;

      Parser.Free;
      source.Free;
      if parse.Count=0 then exit;
      for i:= 0 to parse.Count-1 do
        GetChapterTitleAndChapterURL;
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
  URLs.Free;
  Result:= NO_ERROR;
end;
