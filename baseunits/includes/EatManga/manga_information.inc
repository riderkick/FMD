  function GetEatMangaInfoFromURL: Byte;
  var
    s: String;
    i, j: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[EATMANGA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(EATMANGA_ID, URL);// + '&confirm=yes';
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

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (GetTagName(parse.Strings[i]) = 'img') and
        (Pos('border="0" align="center"', parse.Strings[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i], 'src=')));

      // get summary
      if (Pos(' manga about ?', parse.Strings[i]) <> 0) then
      begin
        j := i + 4;
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
      if (Pos(', Read ', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(StringFilter(GetString(parse.Strings[i],
          ', Read ', ' English Scan Online')));

      // get chapter name and links
      if (Pos('<th class="title">', parse.Strings[i]) > 0) and
        (Pos('/upcoming/', parse.Strings[i + 1]) = 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetString(parse.Strings[i + 1], 'href="', '"');
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 2])));
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
        mangaInfo.genres := '';
        s := parse.Strings[i + 4];
        if s[1] <> '<' then
          mangaInfo.genres := s;
      end;

      // get status
      if (i + 4 < parse.Count) and (Pos('Chapters:', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 4]) <> 0 then
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
