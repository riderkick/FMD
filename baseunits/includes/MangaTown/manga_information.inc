  function GetMangaTownInfoFromURL: Byte;
  var
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
    s: String;
    i: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[MANGATOWN_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGATOWN_ID, AURL);
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

    mangaInfo.genres := '';
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (i + 2 < parse.Count - 1) then
        if (Pos('class="detail_info clearfix"', parse[i]) > 0) and (Pos('<img', parse[i + 2]) > 0) then
          mangaInfo.coverLink := GetVal(parse[i + 2], 'src');

      // get title
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="title-top"', parse[i]) > 0) and (mangaInfo.title = '') then
          mangaInfo.title := Trim(StringFilter(parse[i + 1]));

      // get genres
      if (i + 1 < parse.Count - 1) then
        if ((Pos('Demographic:', parse[i]) > 0) or (Pos('Genre(s):', parse[i]) > 0)) and
          (Pos('</b', parse[i + 1]) > 0) then
          isExtractGenres := True;
      if isExtractGenres and
        (Pos('</li', parse[i]) > 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('<a ', parse[i]) > 0) then
        if mangaInfo.genres = '' then
          mangaInfo.genres := Trim(parse[i + 1])
        else
          mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);
      if (i + 3 < parse.Count - 1) then
        if (Pos('Type:', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          if mangaInfo.genres = '' then
            mangaInfo.genres := Trim(parse[i + 3])
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 3]);
        end;

      // get authors
      if (i + 3 < parse.Count - 1) then
        if (Pos('Author(s):', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          mangaInfo.authors := Trim(parse[i + 3]);
          if Pos(';', mangaInfo.authors) > 0 then
          begin
            mangaInfo.authors := StringReplace(mangaInfo.authors, ' ; ', ', ', [rfReplaceAll]);
            mangaInfo.authors := StringReplace(mangaInfo.authors, '; ', ', ', [rfReplaceAll]);
          end;
        end;

      // get summary
      if (i + 1 < parse.Count - 1) then
        if (Pos('<span', parse[i]) > 0) and (Pos('id="show"', parse[i]) > 0) then
          mangaInfo.summary := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));

      //get chapter links and names
      if (Pos('<ul', parse[i]) > 0) and
        (Pos('class="chapter_list"', parse[i]) > 0) then
        isExtractChapter := True;
      if isExtractChapter and (Pos('</ul', parse[i]) > 0) then
        isExtractChapter := False;
      if isExtractChapter and
        (i + 9 < parse.Count - 1) then
        if (Pos('<a ', parse[i]) > 0) then
        begin
          s := GetVal(parse[i], 'href');
          s := StringReplace(s, WebsiteRoots[MANGATOWN_ID, 1], '', [rfIgnoreCase]);
          mangaInfo.chapterLinks.Add(s);
          s := Trim(RemoveSymbols(parse[i + 1]));
          //if Pos('class="', parse[i + 4]) = 0 then
          if Pos('<span>', parse[i + 4]) > 0 then
            s := s + ' ' + Trim(RemoveSymbols(parse[i + 5]));
          //if Pos('class="', parse[i + 8]) = 0 then
          if Pos('<span>', parse[i + 8]) > 0 then
            s := s + ' ' + Trim(RemoveSymbols(parse[i + 9]));
          mangaInfo.chapterName.Add(Trim(s));
        end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    InvertStrings(mangaInfo.chapterName);
    InvertStrings(mangaInfo.chapterLinks);
    Result := NO_ERROR;
  end;
