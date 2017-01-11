  function GetFakkuInfoFromURL: Byte;
  var
    s: String;
    isExtractSummary: Boolean = False;
    i: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(FAKKU_ID, AURL);
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
    mangaInfo.website := WebsiteRoots[FAKKU_ID, 0];
    if parse.Count = 0 then
      Exit;

    mangaInfo.status := '0';
    mangaInfo.genres := '';
    // using parser (cover link, summary, chapter name and link)
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if GetVal(parse[i], 'property') = 'og:title' then
        mangaInfo.title := CommonStringFilter(GetVal(parse[i], 'content'));

      // get cover
      if (Pos('<img ', parse[i]) > 0) and (Pos('class="cover"', parse[i]) > 0) then
      begin
        s := GetVal(parse[i], 'src');
        if ExecRegExpr('^//', s) then
          mangaInfo.coverLink := 'https:' + s
        else
          mangaInfo.coverLink := s
      end;

      // get read/chapter
      if GetVal(parse[i], 'class') = 'button green' then
      begin
        Inc(mangaInfo.numChapter);
        if mangaInfo.title = '' then
          mangaInfo.title := 'Unknown';
        mangaInfo.chapterName.Add(mangaInfo.title);
        mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
      end;

      // get series/authors
      if (Pos('div class="left"', parse[i]) > 0) and
        (Pos('Series', parse[i + 1]) > 0) then
      begin
        mangaInfo.authors := parse[i + 6];
        mangaInfo.genres := mangaInfo.authors;
      end;

      // get artists
      if (Pos('div class="left"', parse[i]) > 0) and
        (Pos('Artist', parse[i + 1]) > 0) then
      begin
        mangaInfo.artists := parse[i + 6];
        mangaInfo.genres := mangaInfo.genres + ', ' + mangaInfo.artists;
      end;

      // get language
      if (Pos('div class="left"', parse[i]) > 0) and
        (Pos('Language', parse[i + 1]) > 0) then
        mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 7]);

      // get descriptions
      if (Pos('div class="left"', parse[i]) > 0) and
        (Pos('Description', parse[i + 1]) > 0) then
        isExtractSummary := True;

      if isExtractSummary then
      begin
        if (Pos('</div>', parse[i + 5]) > 0) then
          isExtractSummary := False
        else if (not (Pos('<', parse[i + 5]) > 0)) then
          mangaInfo.summary := Trim(mangaInfo.summary) + ' ' + Trim(parse[i + 5]);
      end;

      // get genres
      if (Pos('"/tags/', parse[i]) > 0) then
      begin
        mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);
      end;
    end;

    //last correction
    mangaInfo.summary := BreaksString(StringFilter(mangaInfo.summary));
    Result := NO_ERROR;
  end;
