  function GetIKomikInfoFromURL: Byte;
  var
    s: String;
    i: Integer;
  begin
    mangaInfo.website := WebsiteRoots[IKOMIK_ID, 0];
    mangaInfo.url := FillMangaSiteHost(IKOMIK_ID, AURL);
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

    if parse.Count = 0 then
      Exit;

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    for i := 0 to parse.Count - 1 do
    begin
      //title
      if (i + 1 < parse.Count - 1) then
        if (Pos('<h1', parse[i]) > 0) and (Pos('class="title"', parse[i]) > 0) then
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));

      //cover
      if (i + 2 < parse.Count - 1) then
        if (Pos('class="cover_area"', parse[i]) > 0) and (Pos('<img', parse[i + 2]) > 0) then
          mangaInfo.coverLink := GetVal(parse[i + 2], 'src');

      //authors
      if (i + 2 < parse.Count - 1) then
        if (Pos('Author', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
          begin
            s := TrimLeftChar(parse[i + 2], [':']);
            s := Trim(HTMLEntitiesFilter(StringFilter(Trim(s))));
            mangaInfo.authors := s;
          end;

      //category/genre
      if (i + 2 < parse.Count - 1) then
        if (Pos('Category', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
          begin
            s := TrimLeftChar(parse[i + 2], [':']);
            s := Trim(HTMLEntitiesFilter(StringFilter(Trim(s))));
            mangaInfo.genres := s;
          end;

      //status
      if (i + 2 < parse.Count - 1) then
        if (Pos('Status', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
          begin
            if Pos('Completed', parse[i + 2]) > 0 then
              mangaInfo.status := '0'
            else
              mangaInfo.status := '1';
          end;

      //chapters
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="list"', parse[i]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[IKOMIK_ID, 1], '', [rfIgnoreCase]));
          mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        end;
    end;

    if mangaInfo.chapterName.Count > 1 then
    begin
      // invert chapter
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
