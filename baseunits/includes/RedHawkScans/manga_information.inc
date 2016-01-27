  function GetRedHawkScansInfoFromURL: Byte;
  var
    isExtractChapter: Boolean = False;
    s: String;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(REDHAWKSCANS_ID, URL);// + '&confirm=yes';
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
    mangaInfo.website := WebsiteRoots[REDHAWKSCANS_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (GetTagName(parse[i]) = 'img') and
        (Pos('class="thumbnail"', parse[i - 2]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetVal(parse[i], 'src'));

      // get summary
      if (Pos('Description', parse[i]) <> 0) and
        (Pos('<b', parse[i - 1]) <> 0) then
      begin
        j := i + 2;
        while (j < parse.Count) and (Pos('</li>', parse[j]) = 0) do
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

      // get title
      if (Pos('Title', parse[i]) <> 0) and
        (Pos('<b', parse[i - 1]) <> 0) and
        (mangaInfo.title = '') then
        mangaInfo.title := StringReplace(TrimLeft(StringFilter(parse[i + 2])),
          ': ', '', []);

      if (not isExtractChapter) and
        (Pos('All chapters available for', parse[i]) > 0) then
        isExtractChapter := True;

      if (isExtractChapter) and
        (Pos('class="element"', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetString(parse[i + 3], 'href="', '"'),
          WebsiteRoots[REDHAWKSCANS_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse[i + 4])));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;

      // get authors
      if (i + 2 < parse.Count) and (Pos('Author', parse[i]) <> 0) and
        (Pos('<b', parse[i - 1]) <> 0) then
        mangaInfo.authors := StringReplace(TrimLeft(parse[i + 2]), ': ', '', []);

      // get artists
      if (i + 2 < parse.Count) and (Pos('Artist', parse[i]) <> 0) and
        (Pos('<b', parse[i - 1]) <> 0) then
        mangaInfo.artists := StringReplace(TrimLeft(parse[i + 2]), ': ', '', []);

      // get genres
      if (Pos('Genre', parse[i]) <> 0) and
        (Pos('<b', parse[i - 1]) <> 0) then
        mangaInfo.genres := StringReplace(TrimLeft(parse[i + 2]), ': ', '', []);

      // get status
      if (i + 2 < parse.Count) and (Pos('Status', parse[i]) <> 0) and
        (Pos('<b', parse[i - 1]) <> 0) then
      begin
        if Pos('Ongoing', parse[i + 2]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
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
