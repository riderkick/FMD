  function GetMangaHereInfoFromURL: Byte;
  var
    i, j: LongInt;
    isGetChapters: Boolean = False;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGAHERE_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGAHERE_ID, 0];

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos('<title>', parse[i]) > 0) then
        mangaInfo.title := GetString(parse[i + 1], 'Manga - Read ', ' Online at ');

      // get cover link
      if GetTagName(parse[i]) = 'img' then
        if (GetAttributeValue(GetTagAttribute(parse[i], 'class=')) = 'img') then
          mangaInfo.coverLink :=
            GetAttributeValue(GetTagAttribute(parse[i], 'src'));

      // get summary
      if (Pos('id="show"', parse[i])) <> 0 then
      begin
        parse[i + 1] := StringFilter(parse[i + 1]);
        parse[i + 1] := StringReplace(parse[i + 1], #10, '\n', [rfReplaceAll]);
        parse[i + 1] := StringReplace(parse[i + 1], #13, '\r', [rfReplaceAll]);
        mangaInfo.summary := parse[i + 1];
      end;

      // get authors
      if (Pos('Author(s):', parse[i]) <> 0) then
        mangaInfo.authors := parse[i + 3];

      // get artists
      if (Pos('Artist(s):', parse[i]) <> 0) then
        mangaInfo.artists := parse[i + 3];

      // get genres
      if (Pos('Genre(s):', parse[i]) <> 0) then
        mangaInfo.genres := Trim(parse[i + 2]);

      // get status
      if (Pos('Status:', parse[i]) <> 0) then
      begin
        if Pos('Ongoing', parse[i + 2]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
        if Pos('Completed', parse[i + 2]) <> 0 then
          mangaInfo.status := '0';  // completed
      end;

      // get chapters
      if (Pos('<div', parse[i]) <> 0) and (Pos('class="detail_list', parse[i]) <> 0) then
        isGetChapters := True;
      if isGetChapters and (Pos('class="tab_comment clearfix', parse[i]) <> 0) then
        isGetChapters := False;
      if isGetChapters and 
        (Pos('<a', parse[i]) <> 0) and (Pos('class="color_', parse[i]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
        for j := i + 2 to parse.Count - 1 do
        begin
          if (Pos('class="right', parse[j]) <> 0) or (Pos('class="new', parse[j]) <> 0) then
            Break;
          if Pos('<', parse[j]) = 0 then
            parse[i + 1] := Trim(parse[i + 1]) + ' ' + Trim(parse[j]);
        end;
        mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
      end;
    end;

    // invert chapters
    if mangainfo.ChapterName.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
