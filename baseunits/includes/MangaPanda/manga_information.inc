  function GetMangaPandaInfoFromURL: Byte;
  var
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
    s: String;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGAPANDA_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
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
    mangaInfo.website := WebsiteRoots[MANGAPANDA_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (GetTagName(parse.Strings[i]) = 'img') and
        (Pos('div id="mangaimg"', parse.Strings[i - 1]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i], 'src=')));

      // get summary
      if (Pos('div id="readmangasum"', parse.Strings[i]) <> 0) then
      begin
        j := i + 7;
        mangaInfo.title := TrimLeft(StringFilter(GetString(parse.Strings[i + 3],
          'Read ', ' Manga Online')));
        while (j < parse.Count) and (Pos('</p>', parse.Strings[j]) = 0) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
          end;
          Inc(j);
        end;
      end;

      // get title
      // if (Pos(', Read ', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      //   mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i], ', Read ', ' English Scan Online')));

      if (not isExtractChapter) and
        (Pos('Chapter Name', parse.Strings[i]) > 0) and
        (Pos('class="leftgap"', parse.Strings[i - 1]) > 0) then
        isExtractChapter := True;

      if (isExtractChapter) and
        (Pos('class="chico_manga"', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetString(parse.Strings[i + 3], 'href="', '"');
        if (Pos('.html', s) > 0) and (Pos('-1/', s) > 0) then
          s := StringReplace(s, '-1/', SEPERATOR2, []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 4])) +
          Trim(parse.Strings[i + 6]));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;

      // get authors
      if (i + 4 < parse.Count) and (Pos('Author:', parse.Strings[i]) <> 0) then
        mangaInfo.authors := TrimLeft(parse.Strings[i + 4]);

      // get artists
      if (i + 4 < parse.Count) and (Pos('Artist:', parse.Strings[i]) <> 0) then
        mangaInfo.artists := TrimLeft(parse.Strings[i + 4]);

      // get genres
      if (Pos('Genre:', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if Pos('class="genretags"', parse.Strings[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
        if Pos('</tr>', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 4 < parse.Count) and (Pos('Status:', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 4]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
      end;
    end;
    Result := NO_ERROR;
  end;
