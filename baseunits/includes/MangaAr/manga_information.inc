  function GetMangaArInfoFromURL: Byte;
  var
    s: String;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGAAR_ID, AURL);
    if not GetPage(TObject(Source), EncodeURL(mangaInfo.url), AReconnect) then
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
    mangaInfo.website := WebsiteRoots[MANGAAR_ID, 0];

    mangaInfo.numChapter := 0;
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('class=" thumbnail"', parse[i]) > 0) then
        mangaInfo.coverLink :=
          CorrectURL(GetVal(parse[i + 2], 'src'));

      // get summary
      if (i + 4 < parse.Count) and (Pos('القصة', parse[i]) > 0) then
        if (Pos('</span>', parse[i - 1]) > 0) and
          (Pos('</dt>', parse[i + 1]) > 0) then
        begin
          mangaInfo.summary := '';
          j := i + 4;
          while (j < parse.Count) and (not (Pos('</dd>', parse[j]) > 0)) do
          begin
            s := Trim(parse[j]);
            s := HTMLEntitiesFilter(StringFilter(s));
            mangaInfo.summary := mangaInfo.summary + s;
            Inc(j);
          end;
          mangaInfo.summary := StringReplace(mangaInfo.summary, '<p>',
            '\r\n', [rfReplaceAll]);
          mangaInfo.summary := StringReplace(mangaInfo.summary, '</p>', '', [rfReplaceAll]);
          mangaInfo.summary := StringReplace(mangaInfo.summary, '\r\n\r\n',
            '\r\n', [rfReplaceAll]);
          mangaInfo.summary := StringReplace(mangaInfo.summary, '\r\n\r\n',
            '\r\n', [rfReplaceAll]);
          mangaInfo.summary := StringReplace(mangaInfo.summary, '\r\n\r\n',
            '\r\n', [rfReplaceAll]);
        end;

      // get title
      if (mangaInfo.title = '') and
        (Pos('http-equiv="description" content="', parse[i]) <> 0) then
        mangaInfo.title := Trim(GetVal(parse[i], 'content'));

      // get chapter name and links
      if (Pos('class="tit ajaxify {title:', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetVal(parse[i], 'href');
        s := StringReplace(s, WebsiteRoots[MANGAAR_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(Trim(parse[i + 1]));
        s := StringFilter(HTMLEntitiesFilter(s));
        mangaInfo.chapterName.Add(s);
      end;

      // get authors
      if (i + 4 < parse.Count) and (Pos('اسم المؤلف', parse[i]) > 0) then
        if (Pos('</span>', parse[i - 1]) > 0) and
          (Pos('</dt>', parse[i + 1]) > 0) then
          mangaInfo.authors := Trim(parse[i + 4]);

      // get artists
      if (i + 4 < parse.Count) and (Pos('اسم الرسام', parse[i]) > 0) then
        if (Pos('</span>', parse[i - 1]) > 0) and
          (Pos('</dt>', parse[i + 1]) > 0) then
          mangaInfo.artists := Trim(parse[i + 4]);

      // get genres
      if (i + 4 < parse.Count) and (Pos('التصنيف', parse[i]) > 0) then
        if (Pos('</span>', parse[i - 1]) > 0) and
          (Pos('</dt>', parse[i + 1]) > 0) then
        begin
          mangaInfo.genres := Trim(parse[i + 4]);
          mangaInfo.genres := StringReplace(mangaInfo.genres, sLineBreak,
            '', [rfReplaceAll]);
          mangaInfo.genres := StringReplace(mangaInfo.genres, '    ', '', [rfReplaceAll]);
          mangaInfo.genres := StringReplace(mangaInfo.genres, ',', ', ', [rfReplaceAll]);
        end;

      // get status
      if (i + 4 < parse.Count) and (Pos('حالة الترجمة', parse[i]) > 0) then
        if (Pos('</span>', parse[i - 1]) > 0) and
          (Pos('</dt>', parse[i + 1]) > 0) then
        begin
          if Trim(parse[i + 4]) = 'مكتملة' then
            mangaInfo.status := '1'  // Ongoing
          else
            mangaInfo.status := '0'; // Completed
        end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterLinks.Count > 1 then
    begin
      i := 0;
      j := mangainfo.ChapterLinks.Count - 1;
      while (i < j) do
      begin
        mangainfo.ChapterName.Exchange(i, j);
        mangainfo.chapterLinks.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    end;
    Result := NO_ERROR;
  end;
