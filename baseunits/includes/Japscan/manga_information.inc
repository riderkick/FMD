  function GetJapscanInfoFromURL: Byte;
  var
    s: String;
    i: Cardinal;
    row: TStringList;
    isRow: Boolean = False;
    isExtractChapter: Boolean = False;
    regx: TRegExpr;

  begin
    mangaInfo.url := FillMangaSiteHost(JAPSCAN_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;

    Parser.Free;
    Source.Free;
    mangaInfo.website := WebsiteRoots[JAPSCAN_ID, 0];

    mangaInfo.coverLink := '';
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    row := TStringList.Create;
    for i := 0 to parse.Count - 1 do
    begin
      //get title
      if Pos('<h1 class="bg-header"', parse[i]) > 0 then
      begin
        regx := TRegExpr.Create;
        regx.ModifierI := True;
        regx.Expression := '^Manga\s(.+)\sVF$';
        s := regx.Replace(Trim(parse[i + 2]), '$1', True);
        mangaInfo.title := s;
        regx.Free;
      end;

      // author, year, genres, fansubs and status respectively
      if (Pos('<div class="row"', parse[i]) > 0) then
        isRow := True;
      if isRow and (Pos('<h2', parse[i]) > 0) then
        isRow := False;

      if isRow and (Pos('class="cell"', parse[i]) > 0) then
      begin
        if Pos('href="', parse[i + 1]) > 0 then
          row.Add(Trim(parse[i + 2]))
        else
          row.Add(Trim(parse[i + 1]));
      end;

      //get status
      if isRow and (Pos('class="cell"', parse[i]) > 0) then
      begin
        if Trim(parse[i + 1]) = 'En Cours' then
          mangaInfo.status := '1'
        else
        if Trim(parse[i + 1]) = 'Terminé' then
          mangaInfo.status := '0';
      end;

      //get summary
      if Pos('id="synopsis"', parse[i]) > 0 then
        mangaInfo.summary := Trim(parse[i + 1]);

      // get chapter name and links
      if Pos('id="liste_chapitres"', parse[i]) > 0 then
        isExtractChapter := True;
      if isExtractChapter and (Pos('</div', parse[i]) > 0) then
        isExtractChapter := False;

      if (isExtractChapter) and (Pos('<a href="', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetVal(parse[i], 'href');
        if Pos('.htm', s) > 0 then
          s := ReplaceRegExpr('/\d+\.html?$', s, '', False);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(Trim(parse[i + 1]));
        mangaInfo.chapterName.Add(CommonStringFilter(s));
      end;
    end;

    //author, year, genres, fansubs and status respectively
    if row.Count >= 5 then
    begin
      mangaInfo.authors := row[0];
      mangaInfo.genres := row[2];
      if mangaInfo.genres = '' then
        mangaInfo.genres := row[3]
      else
        mangaInfo.genres := mangaInfo.genres + ', ' + row[3];
    end;
    row.Free;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterLinks.Count > 1 then
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);

    Result := NO_ERROR;
  end;
