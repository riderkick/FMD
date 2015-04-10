  function GetMangaSeeInfoFromURL: Byte;
  var
    i: LongInt;
    s: String;
    isExtractGenres  : Boolean = False;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGASEE_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGASEE_ID, URL);
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

    mangaInfo.genres := '';

    if parse.Count = 0 then
      Exit;

    for i := 0 to parse.Count - 1 do
    begin
      //title
      if Pos('<h1', parse[i]) <> 0 then
        mangaInfo.title := CommonStringFilter(parse[i + 1]);

      //cover
      if (Pos('<img', parse[i]) <> 0) and (Pos('/cover/', parse[i]) <> 0) then
        mangaInfo.coverLink := GetMangaSiteRoot(MANGASEE_ID) + TrimLeftChar(GetVal(parse[i], 'src'), ['.']);

      if i + 2 < parse.Count - 1 then
        if Pos('</b', parse[i + 1]) <> 0 then
        begin
          //author
          if (Pos('Author:', parse[i]) <> 0) then
            mangaInfo.authors := CommonStringFilter(parse[i + 3]);

          //status
          if (Pos('Scanlation Status:', parse[i]) <> 0) then
            if Trim(LowerCase(parse[i + 2])) = 'ongoing' then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';

          //summary
          if (Pos('Description:', parse[i]) <> 0) then
            mangaInfo.summary := CommonStringFilter(parse[i + 4]);

          //genres
          if (Pos('Genre:', parse[i]) <> 0) then
            isExtractGenres := True;
        end;

      //genres
      if isExtractGenres and (Pos('</div', parse[i]) <> 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('<a', parse[i]) <> 0) then
        if (Pos('<', parse[i + 1]) = 0) then
          if mangaInfo.genres = '' then
            mangaInfo.genres := parse[i + 1]
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + parse[i + 1];

      //chapters
      if (not isExtractChapters) and (Pos('<!--Chapter Row-->', parse[i]) <> 0) then
        isExtractChapters := True;
      if isExtractChapters and (Pos('<!-- Divider -->', parse[i]) <> 0) then
        Break;
      if isExtractChapters and
        (Pos('<a', parse[i]) <> 0) and (Pos('chapter_link', parse[i]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := TrimLeftChar(GetVal(parse[i], 'href'), ['.']);
        if RightStr(s, 7) = '&page=1' then
          SetLength(s, Length(s) - 7);
        mangaInfo.chapterLinks.Add(s);
        s := Trim(parse[i + 1]);
        if Pos('<gray', parse[i + 3]) <> 0 then
          s := s + ' ' + Trim(parse[i + 4]);
        mangaInfo.chapterName.Add(CommonStringFilter(s));
      end;
    end;

    //invert
    if mangainfo.ChapterLinks.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterLinks);
      InvertStrings(mangaInfo.chapterName);
    end;
    Result := NO_ERROR;
  end;
