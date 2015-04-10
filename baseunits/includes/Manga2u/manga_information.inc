  function GetManga2uInfoFromURL: Byte;
  var
    s: String;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
    i, j, n: Cardinal;
    numberOfPage: Cardinal = 1;

    procedure ExtractChapter;
    begin
      if (not isExtractChapter) and (Pos('name="IL_IN_TAG', parse.Strings[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('class="val', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i - 2], 'href')),
          WebsiteRoots[MANGA2U_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 1])));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('sct_wid_bot', parse.Strings[i]) > 0) then
        isExtractChapter := False;
    end;

  begin
    mangaInfo.url := FillMangaSiteHost(MANGA2U_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGA2U_ID, 0];
    mangaInfo.status := '';
    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      if Pos('class="sel', parse.Strings[i]) > 0 then
        numberOfPage := StrToInt(GetString(parse.Strings[i + 10], '-list/', '/">'));

      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('class="cvr_ara', parse.Strings[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i + 2], 'src=')));

      // get title
      if (Pos('class="cvr_ara', parse.Strings[i]) > 0) then
        mangaInfo.title := GetAttributeValue(GetTagAttribute(parse.Strings[i + 2], 'alt='));

      ExtractChapter;

      // get summary
      if (Pos('Author', parse.Strings[i]) <> 0) then
      begin
        j := i - 6;
        while (j < parse.Count) and (Pos('</p>', parse.Strings[j]) = 0) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := HTMLEntitiesFilter(
              StringFilter(TrimLeft(parse.Strings[j])));
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
            Break;
          end;
          Inc(j);
        end;
      end;

      // get authors
      if (Pos('Author', parse.Strings[i]) <> 0) then
        mangaInfo.authors := StringReplace(TrimLeft(StringFilter(parse.Strings[i + 2])),
          ': ', '', []);

      // get artists
      if (Pos('Author', parse.Strings[i]) <> 0) then
        mangaInfo.artists := StringReplace(TrimLeft(StringFilter(parse.Strings[i + 2])),
          ': ', '', []);

      // get genres
      if (Pos('Category', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if Pos('list/category/', parse.Strings[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
        if Pos('</p>', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 2 < parse.Count) and (Pos('Status', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 2]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
      end;
    end;

    if numberOfPage > 1 then
    begin
      for n := 2 to numberOfPage do
      begin
        Source.Clear;
        s := mangaInfo.url + 'chapter-list/' + IntToStr(n);
        if not GetPage(TObject(Source), mangaInfo.url + 'chapter-list/' +
          IntToStr(n), Reconnect) then
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
        isExtractChapter := False;
        if parse.Count = 0 then
          Exit;
        for i := 0 to parse.Count - 1 do
          ExtractChapter;
      end;
    end;
    Source.Free;

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
