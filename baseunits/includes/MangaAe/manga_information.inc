  function GetMangaAeInfoFromURL: Byte;
  var
    i: LongInt;
    regx: TRegExpr;
    isExtractChapter: Boolean = False;
    isExtractGenres : Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGAAE_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAAE_ID, URL);
    if not GetPage(TObject(Source), EncodeURL(mangaInfo.url), Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count = 0 then
      Exit;

    mangaInfo.genres := '';

    regx := TRegExpr.Create;
    try
      regx.Expression := '/1/?$';
      for i := 0 to parse.Count - 1 do
      begin
        // cover
        if (Pos('<img', parse[i]) <> 0) and (Pos('class="manga-cover', parse[i]) <> 0) then
          mangaInfo.coverLink := GetVal(parse[i], 'src');

        // title
        if (mangaInfo.title = '') and (Pos('class="EnglishName', parse[i]) <> 0) then
        begin
          parse[i] := Trim(parse[i + 1]);
          if Length(parse[i]) > 2 then
          begin
            if (parse[i][1] = '(') and (parse[i][Length(parse[i])] = ')') then
              parse[i] := Copy(parse[i], 2, Length(parse[i]) - 2);
          end;
          mangaInfo.title := Trim(parse[i]);
        end;

        // author
        if (Pos('class="manga-details-author', parse[i]) <> 0) then
          if (Pos('<', parse[i + 21]) = 0) then
            mangaInfo.authors := Trim(parse[i + 21]);

        // status
        if (i + 4 < parse.Count) and
          (Pos('الحالة :', parse[i]) <> 0) and
          (Pos('</h3', parse[i + 1]) <> 0) then
        begin
          if (Pos('مستمرة', parse[i + 4]) <> 0) then
            mangaInfo.status := '1'   // ongoing
          else
            mangaInfo.status := '0';  // completed
        end;

        // genres
        if Pos('التصنيف:', parse[i]) <> 0 then
          isExtractGenres := True;
        if isExtractChapter and (Pos('</ul', parse[i]) <> 0) then
          isExtractGenres := False;
        if isExtractGenres and
          (Pos('<a', parse[i]) <> 0) and (Pos('/manga/cats', parse[i]) <> 0) then
        begin
          if mangaInfo.genres <> '' then
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1])
          else
            mangaInfo.genres := Trim(parse[i + 1]);
        end;

        // summary
        if (Pos('نبذة عن المانجا', parse[i]) <> 0) and (Pos('</h3', parse[i + 1]) <> 0) then
          if Pos('<', parse[i + 4]) = 0 then
            mangaInfo.summary := CommonStringFilter(parse[i + 4]);

        //chapters
        if (Pos('<ul', parse[i]) <> 0) and (Pos('class="new-manga-chapters', parse[i]) <> 0) then
          isExtractChapter := True;
        if isExtractChapter and (Pos('</ul', parse[i]) <> 0) then
          isExtractChapter := False;
        if isExtractChapter and
          (Pos('<a', parse[i]) <> 0) and (Pos('class="chapter', parse[i]) <> 0) then
        begin
          parse[i] := GetVal(parse[i], 'href');
          if regx.Exec(parse[i]) then
            parse[i] := regx.Replace(parse[i], '', False);
          mangaInfo.chapterLinks.Add(parse[i]);
          mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
        end;
      end;
    finally
      regx.Free;
    end;

    //invert chapters
    if mangaInfo.chapterLinks.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;

    Result := NO_ERROR;
  end;
