  function GetOneMangaInfoFromURL: Byte;
  var
    i: Integer;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[ONEMANGA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(ONEMANGA_ID, AURL);
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

    mangaInfo.title := '';
    mangaInfo.authors := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      //get title
      if (Pos('<h1 ', parse[i]) > 0) and (Pos('class="ttl"', parse[i]) > 0) then
        mangaInfo.title := Trim(parse[i + 1]);

      //get cover
      if (Pos('<img ', parse[i]) > 0) and (Pos('class="cvr"', parse[i]) > 0) then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      //get authors
      if Pos('Author', parse[i]) > 0 then
        if Pos('</b>', parse[i + 1]) > 0 then
          mangaInfo.authors := Trim(RemoveSymbols(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2])))));

      //get summary
      if Pos('class="det"', parse[i]) > 0 then
        if Pos('<p>', parse[i + 2]) > 0 then
          mangaInfo.summary := Trim(BreaksString(HTMLEntitiesFilter(StringFilter(parse[i + 3]))));

      //get genres
      if Pos('Category', parse[i]) > 0 then
        if Pos('</b>', parse[i + 1]) > 0 then
          isExtractGenres := True;
      if isExtractGenres and (Pos('</p>', parse[i]) > 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('<a ', parse[i]) > 0) then
      begin
        if mangaInfo.genres = '' then
          mangaInfo.genres := Trim(RemoveSymbols(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))))
        else
          mangaInfo.genres := mangaInfo.genres + ', ' + Trim(RemoveSymbols(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
      end;

      //get status
      if Pos('Status', parse[i]) > 0 then
        if Pos('</b>', parse[i + 1]) > 0 then
        begin
          if Pos('Ongoing', parse[i + 2]) > 0 then
            mangaInfo.status := '1'
          else
            mangaInfo.status := '0';
        end;

      //get chapters name and link
      if (Pos('<ul ', parse[i]) > 0) and (Pos('class="lst"', parse[i]) > 0) then
        isExtractChapter := True;
      if isExtractChapter and (Pos('</ul>', parse[i]) > 0) then
        isExtractChapter := False;
      if isExtractChapter and
        ((Pos('<a ', parse[i]) > 0) and (Pos('class="lst"', parse[i]) > 0)) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[ONEMANGA_ID, 1], '', [rfIgnoreCase]));
        mangaInfo.chapterName.Add(Trim(RemoveSymbols(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 3]))))));
      end;
    end;

    //invert chapter list
    InvertStrings(mangaInfo.chapterName);
    InvertStrings(mangaInfo.chapterLinks);
    Result := NO_ERROR;
  end;
