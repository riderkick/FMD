  function GetMangaFrameInfoFromURL: Byte;
  var
    i: Integer;
  begin
    mangaInfo.website := WebsiteRoots[MANGAFRAME_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAFRAME_ID, URL);
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

    if parse.Count = 0 then
      Exit(INFORMATION_NOT_FOUND);

    mangaInfo.coverLink := '';
    mangaInfo.title := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.status := '1';

    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if (i + 2 < parse.Count - 1) then
        if (Pos('class="thumbnail"', parse[i]) > 0) and (Pos('<img', parse[i + 2]) > 0) then
          mangaInfo.coverLink := GetVal(parse[i + 2], 'src');

      //title
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="title"', parse[i]) > 0) and (Pos('<h1', parse[i]) > 0) then
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))));

      //author
      if (i + 3 < parse.Count - 1) then
        if (Pos('Author', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0)
          and (Pos('<', parse[i + 2]) = 0) then
          mangaInfo.authors := Trim(TrimLeftChar(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))), [':']));

      //artist
      if (i + 3 < parse.Count - 1) then
        if (Pos('Artist', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0)
          and (Pos('<', parse[i + 2]) = 0) then
          mangaInfo.artists := Trim(TrimLeftChar(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))), [':']));

      //synopsis
      if (i + 3 < parse.Count - 1) then
        if (Pos('Synopsis', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0)
          and (Pos('<', parse[i + 2]) = 0) then
          mangaInfo.summary := Trim(TrimLeftChar(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))), [':']));

      //chapters
      if (i + 2 < parse.Count - 1) then
        if (Pos('class="title"', parse[i]) > 0) and (Pos('<a', parse[i + 1]) > 0)
          and (Pos('/okuyucu/read', parse[i + 1]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(Trim(StringReplace(GetVal(parse[i + 1], 'href'), WebsiteRoots[MANGAFRAME_ID, 1], '', [rfIgnoreCase])));
          mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2])))));
        end;
    end;

    // invert chapters
    if mangaInfo.chapterName.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;

    Result := NO_ERROR;
  end;
