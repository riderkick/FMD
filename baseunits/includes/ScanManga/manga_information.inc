  function GetScanMangaInfoFromURL: Byte;
  var
    s: String;
    isExtractGenres: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(SCANMANGA_ID, AURL);
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
    mangaInfo.website := WebsiteRoots[SCANMANGA_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('<img itemprop="image"', parse[i]) > 0) then
      begin
        mangaInfo.coverLink :=
          CorrectURL(GetVal(parse[i], 'src'));
        //mangaInfo.coverLink:= StringReplace(mangaInfo.coverLink, ':8080/', '/', []);
      end;

      // get title
      if (Pos('id=''ambubble''', parse[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(TrimRight(HTMLEntitiesFilter(parse[i])));

      // get chapter name and links
      if (Pos('class=''chapitre_nom''', parse[i]) > 0) and
        (Pos('<strong>', parse[i + 1]) > 0) then
      begin
        s := Trim(RemoveSymbols(parse[i + 2]));
        if (Pos('<', parse[i + 6]) = 0) and (Pos('>', parse[i + 6]) = 0) then
          s := s + ' ' + Trim(RemoveSymbols(parse[i + 6]));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;
      if (Pos('/lecture-en-ligne/', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[SCANMANGA_ID, 1], '', []);
        s := StringReplace(TrimLeft(TrimRight(s)), '"', '', [rfReplaceAll]);
        mangaInfo.chapterLinks.Add(s);
      end;

      // get summary
      if (Pos('itemprop="description">', parse[i]) <> 0) then
      begin
        j := i + 1;
        while (j < parse.Count) and (Pos('</p>', parse[j]) = 0) do
        begin
          s := parse[j];
          if s[1] <> '<' then
          begin
            parse[j] := HTMLEntitiesFilter(StringFilter(parse[j]));
            parse[j] := StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
            parse[j] := StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse[j];
          end;
          Inc(j);
        end;
      end;

      // get authors
      if (i + 2 < parse.Count) and
        (Pos('itemprop="author"', parse[i]) <> 0) then
        mangaInfo.authors := StringFilter(TrimLeft(TrimRight(parse[i + 2])));

      // get artists
      //if (i+5<parse.Count) AND (Pos('Illustrateur :', parse[i])<>0) then
      //  mangaInfo.artists:= StringFilter(TrimLeft(TrimRight(parse[i+5])));

      // get genres
      if (Pos('itemprop="genre"', parse[i]) <> 0) then
      begin
        isExtractGenres := True;
        mangaInfo.genres := mangaInfo.genres + HTMLEntitiesFilter(
          TrimLeft(TrimRight(parse[i + 1]))) + ', ';
      end;

      if isExtractGenres then
      begin
        if Pos('class=''tTip''>', parse[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            HTMLEntitiesFilter(TrimLeft(TrimRight(parse[i + 1]))) + ', ';
        if Pos('itemprop="editor"', parse[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 11 < parse.Count) and (Pos('itemprop="editor"', parse[i]) <> 0) then
      begin
        if (Pos('Termin', parse[i + 5]) <> 0) or
          (Pos('One Shot', parse[i + 5]) <> 0) then
          mangaInfo.status := '0'   // completed
        else
          mangaInfo.status := '1';  // ongoing
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterLinks.Count > 1 then
    begin
      i := 0;
      j := mangainfo.ChapterLinks.Count - 1;
      while (i < j) do
      begin
        mangainfo.ChapterName.Exchange(i, j);
        mangainfo.chapterLinks.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    end;
    Result := NO_ERROR;
  end;
