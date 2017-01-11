  function GetMangaGoInfoFromURL: Byte;
  var
    i: Integer;
    isExtractGenres: Boolean = False;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGAGO_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAGO_ID, AURL);
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
      // get title
      if (i + 3 < parse.Count - 1) then
        if (Pos('name="description"', parse[i]) > 0) and (mangaInfo.title = '') then
          mangaInfo.title := StringReplace(TrimLeft(StringFilter(parse[i + 3])),
            ' manga - Mangago', '', []);

      // get status
      if (i + 5 < parse.Count - 1) then
        if (Pos('<label>', parse[i]) > 0) and
          (Pos('Status:', parse[i + 1]) > 0) and
          (Pos('Completed', parse[i + 5]) > 0) then
            mangaInfo.status := '0'   // completed
          else
            mangaInfo.status := '1';  // ongoing

      // get cover
      if (Pos('<meta', parse[i]) > 0) and
        (Pos('property="og:image"', parse[i]) > 0) then
        mangaInfo.coverLink :=
          (GetVal(parse[i], 'content'));

      // get authors
      if (i + 1 < parse.Count - 1) then
        if (Pos('<label>', parse[i]) > 0) and
          (Pos('Author:', parse[i + 1]) > 0) then
            mangaInfo.authors := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 5])));

      // get genres
      if (i + 1 < parse.Count - 1) then
        if (Pos('<label>', parse[i]) > 0) and
          (Pos('Genre(s)', parse[i + 1]) > 0) then
            isExtractGenres := True;

      if (i + 1 < parse.Count - 1) then
        if isExtractGenres then
        begin
          if Pos('</td>', parse[i]) > 0 then
            isExtractGenres := False;
          if Pos('/genre/', parse[i]) > 0 then
          begin
            if mangaInfo.genres = '' then
              mangaInfo.genres := Trim(parse[i + 1])
            else
              mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);
          end;
        end;

      // get summary
      if (i + 1 < parse.Count - 1) then
        if Pos('class="manga_summary"', parse[i]) > 0 then
          mangaInfo.summary := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));

      //get chapter links and names
      if (GetTagName(parse[i]) = 'table') and
        ((GetVal(parse[i], 'id') = 'chapter_table') or (GetVal(parse[i], 'id') = 'raws_table')) then
        isExtractChapters := True;
      if isExtractChapters then
      begin
        if GetTagName(parse[i]) = '/table' then
          isExtractChapters := False;
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'chico') then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.ChapterLinks.Add(GetVal(parse[i], 'href'));
          mangaInfo.ChapterName.Add(CommonStringFilter(Trim(parse[i + 1]) + ' ' +
            Trim(parse[i + 3]) + ' ' + Trim(parse[i + 5])));
        end;
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterLinks.Count > 1 then
      InvertStrings([mangaInfo.ChapterName, mangaInfo.ChapterLinks]);
    Result := NO_ERROR;
  end;
