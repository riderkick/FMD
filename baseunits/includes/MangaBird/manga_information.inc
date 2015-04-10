  function GetMangaBirdInfoFromURL: Byte;
  var
    s: String;
    i, j, pageCount: Integer;
    isExtractGenres: Boolean = False;
    isExtractGenresItem: Boolean = False;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGABIRD_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGABIRD_ID, URL);
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

    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;

    pageCount := 0;
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    pageCount := 0;
    for i := 0 to parse.Count - 1 do
    begin
      //title
      if (i + 1 < parse.Count - 1) then
        if (Pos('<h1', parse[i]) > 0) and (Pos('class="ttitle"', parse[i]) > 0) then
        begin
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));
          mangaInfo.title := Trim(ReplaceRegExpr('\sManga$', mangaInfo.title, '', False));
        end;

      //cover
      if (Pos('class="imagefield', parse[i]) > 0) and (Pos('<img', parse[i]) > 0) then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      //authors
      if (i + 7 < parse.Count - 1) then
        if (Pos('class="field-label"', parse[i]) > 0) and (Pos('Author:', parse[i + 1]) > 0) then
          mangaInfo.authors := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 7]))));

      //authors
      if (i + 7 < parse.Count - 1) then
        if (Pos('class="field-label"', parse[i]) > 0) and (Pos('Artist:', parse[i + 1]) > 0) then
          mangaInfo.artists := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 7]))));

      //genres
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="field-label"', parse[i]) > 0) and (Pos('Genres:', parse[i + 1]) > 0) then
           isExtractGenres := True;
      if isExtractGenres and (Pos('class="field-item', parse[i]) > 0) then
        isExtractGenresItem := True;
      if isExtractGenresItem and (Pos('</div', parse[i]) > 0) then
      begin
        isExtractGenresItem := False;
        isExtractGenres := False;
      end;
      if (i + 1 < parse.Count - 1) then
        if isExtractGenresItem and ((Pos('/tag/', parse[i]) > 0) and (Pos('<a', parse[i]) > 0)) then
          if mangaInfo.genres = '' then
            mangaInfo.genres := Trim(parse[i + 1])
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);
      //type/genre
      if (i + 7 < parse.Count - 1) then
        if (Pos('class="field-label"', parse[i]) > 0) and (Pos('Type:', parse[i + 1]) > 0) then
        begin
          s := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 7]))));
          s := Trim(ReplaceRegExpr('\(.*\).*$', s, '', False));
          if mangaInfo.genres = '' then
            mangaInfo.genres := s
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + s;
        end;

      //status
      if (i + 7 < parse.Count - 1) then
        if (Pos('class="field-label"', parse[i]) > 0) and (Pos('Status:', parse[i + 1]) > 0) then
          if Pos('ongoing', LowerCase(parse[i + 7])) > 0 then
            mangaInfo.status := '1'
          else
            mangaInfo.status := '0';

      //summary
      if (i + 3 < parse.Count - 1) then
        if (Pos('</fieldset>', parse[i]) > 0) then
          mangaInfo.summary := Trim(HTMLEntitiesFilter(StringFilter(BreaksString(Trim(
            parse[i + 3])))));

      //chapters
      if (i + 2 < parse.Count - 1) then
        if (Pos('<tbody', parse[i]) > 0) and (Pos('views-row-first', parse[i + 2]) > 0) then
          isExtractChapters := True;
      if isExtractChapters and (Pos('</tbody', parse[i]) > 0) then
        isExtractChapters := False;
      if (i + 1 < parse.Count - 1) then
        if isExtractChapters and (Pos('<a', parse[i]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[MANGABIRD_ID, 1], '', [rfIgnoreCase]));
          mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        end;

      //chapter page count
      if (Pos('title="Go to last page"', parse[i]) > 0) and (Pos('<a', parse[i]) > 0) then
        pageCount := StrToIntDef(ReplaceRegExpr('^.*=(\d+).*$', GetVal(parse[i], 'href'), '$1', True), 0);
    end;

    //get the rest of chapters
    if pageCount > 0 then
    begin
      for j := 1 to pageCount do
      begin
        s := mangaInfo.url + '?page=' + IntToStr(j);
        Source.Clear;
        if not GetPage(TObject(Source), s, Reconnect) then
        begin
          Result := NET_PROBLEM;
          Break;
        end;

        parse.Clear;
        Parser := THTMLParser.Create(PChar(Source.Text));
        Parser.OnFoundTag := OnTag;
        Parser.OnFoundText := OnText;
        Parser.Exec;
        Parser.Free;

        if parse.Count > 0 then
        begin
          for i := 0 to parse.Count - 1 do
          begin
            if (i + 2 < parse.Count - 1) then
              if (Pos('<tbody', parse[i]) > 0) and (Pos('views-row-first', parse[i + 2]) > 0) then
                isExtractChapters := True;
            if isExtractChapters and (Pos('</tbody', parse[i]) > 0) then
              isExtractChapters := False;
            if (i + 1 < parse.Count - 1) then
              if isExtractChapters and (Pos('<a', parse[i]) > 0) then
              begin
                Inc(mangaInfo.numChapter);
                mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[MANGABIRD_ID, 1], '', [rfIgnoreCase]));
                mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
              end;
          end;
        end;
      end;
    end;

    // invert chapters
    if mangaInfo.chapterName.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Source.Free;
    Result := NO_ERROR;
  end;
