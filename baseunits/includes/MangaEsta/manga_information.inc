  function GetMangaEstaInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGAESTA_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGAESTA_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('imageanchor="1"', parse.Strings[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i], 'href=')));

      // get title
      if (Pos('Nama :', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(TrimRight(HTMLEntitiesFilter(parse.Strings[i + 2])));

      if (not isExtractChapter) and (Pos('ONLINE INDONESIAN', parse.Strings[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('href="http://www.mangaesta.net/', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
          WebsiteRoots[MANGAESTA_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 1])));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('class=''comments''', parse.Strings[i]) > 0) then
        isExtractChapter := False;

      // get summary
      if (Pos('Sinopsis :', parse.Strings[i]) <> 0) then
      begin
        j := i + 6;
        while (j < parse.Count) and (Pos('</div>', parse.Strings[j]) = 0) do
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

      // get authors
      if (i + 8 < parse.Count) and (Pos('Author :', parse.Strings[i]) <> 0) then
        mangaInfo.authors := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 2])));

      // get artists
      if (i + 1 < parse.Count) and (Pos('Artist :', parse.Strings[i]) <> 0) then
        mangaInfo.artists := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 2])));

      // get genres
      if (Pos('Genre :', parse.Strings[i]) <> 0) then
      begin
        mangaInfo.genres := StringReplace(
          StringFilter(TrimLeft(TrimRight(parse.Strings[i + 2]))), ' |', ',', [rfReplaceAll]);
        mangaInfo.genres := StringReplace(mangaInfo.genres, '|', ',', [rfReplaceAll]);
      end;

      // get status
      if (i + 2 < parse.Count) and (Pos('Status :', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 2]) <> 0 then
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
