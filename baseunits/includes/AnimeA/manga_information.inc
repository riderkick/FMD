  function GetAnimeAInfoFromURL: Byte;
  var
    i, j: Cardinal;
    isExtractGenres: Boolean = False;
    s: String;
  begin
    mangaInfo.website := WebsiteRoots[ANIMEA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(ANIMEA_ID, AURL + ANIMEA_SKIP);
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

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos('<title>', parse[i]) > 0) then
        mangaInfo.title := GetString(parse[i + 1], 'Manga - Read ', ' Manga Scans');

      // get cover link
      if Pos('class="cover_mp"', parse[i]) > 0 then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      // get authors
      if (Pos('Author(s):', parse[i]) <> 0) then
        mangaInfo.authors := TrimRight(TrimLeft(parse[i + 3]));

      // get artists
      if (Pos('Artist(s):', parse[i]) <> 0) then
        mangaInfo.artists := TrimRight(TrimLeft(parse[i + 2]));

      // get genres
      if (Pos('Genre(s):', parse[i]) <> 0) then
      begin
        mangaInfo.genres := '';
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if GetTagName(parse[i]) <> 'a' then
          for j := 0 to 37 do
            if Pos(LowerCase(defaultGenres[j]), LowerCase(parse[i])) <> 0 then
              mangaInfo.genres := mangaInfo.genres + (defaultGenres[j] + ', ');
        if Pos('</li>', parse[i]) > 0 then
          isExtractGenres := False;
      end;

      // get summary
      if Pos('Upload a chapter', parse[i]) > 0 then
      begin
        j := i + 8;
        mangaInfo.summary := '';
        while (j < parse.Count - 4) and (Pos('</p>', parse[j]) = 0) do
        begin
          mangaInfo.summary := StringFilter(mangaInfo.summary + parse[j]);
          Inc(j);
        end;
        mangaInfo.summary := StringFilter(mangaInfo.summary);
      end;

      // get status
      if (Pos('Status:', parse[i]) <> 0) then
      begin
        if Pos('Ongoing', parse[i + 2]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
      end;

      // get chapter name and links
      if (Pos('<a', parse[i]) > 0) and (Pos('id="ch_', parse[i]) > 0) and
        (i + 3 < parse.Count - 1) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
        s := '';
        if (parse[i + 3] <> '') and (Pos('<', parse[i + 3]) = 0) then
        begin
          s := Trim(TrimLeftChar(parse[i + 3], [':']));
        end;
        s := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]) + ' ' + s)));
        mangaInfo.chapterName.Add(s);
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterName.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
