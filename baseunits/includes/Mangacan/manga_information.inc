  function GetMangacanInfoFromURL: Byte;
  var
    s: String;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGACAN_ID, '/' + URL);
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
    mangaInfo.website := WebsiteRoots[MANGACAN_ID, 0];
    mangaInfo.status := '1';
    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
    {if (mangaInfo.coverLink = '') AND
       (Pos('class="cvr', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));}

      // get title
      if (Pos('<title>', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(
          TrimRight(HTMLEntitiesFilter(GetString('~!@' + parse.Strings[i + 1],
          '~!@', ' - Indonesia Online Terbaru|Baca Manga Komik Indonesia|Mangacan!'))));

      if (not isExtractChapter) and (Pos('latestchapters', parse.Strings[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('class="chaptersrec', parse.Strings[i]) > 0) then //asli class="lng
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetString(parse.Strings[i], 'href="', '"'),
          WebsiteRoots[MANGACAN_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 1]))); //ASLI 5
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('seemore', parse.Strings[i]) > 0) then
        isExtractChapter := False; //bermasalah

      // get summary
      if (Pos('<div class="det">', parse.Strings[i]) <> 0) then
      begin
        j := i + 2;
        while (j < parse.Count) and (Pos('<b>', parse.Strings[j]) = 0) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := HTMLEntitiesFilter(
              StringFilter(TrimLeft(parse.Strings[j])));
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
            Break;
          end;
          Inc(j);
        end;
      end;

      // get authors
      if (Pos('Author', parse.Strings[i]) <> 0) then
        mangaInfo.authors := TrimLeft(StringFilter(parse.Strings[i + 2]));

      // get artists
      if (Pos('Artist', parse.Strings[i]) <> 0) then
        mangaInfo.artists := TrimLeft(StringFilter(parse.Strings[i + 2]));

      // get genres
      if (Pos('Category', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if Pos('manga-list/category/', parse.Strings[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
        if Pos('</p>', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 2 < parse.Count) and (Pos('Status', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 3]) <> 0 then
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
