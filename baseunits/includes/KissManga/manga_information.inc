  function GetKissMangaInfoFromURL: Byte;
  var
    i, j: Cardinal;
    isExtractSummary: Boolean = True;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.url := EncodeURL(FillMangaSiteHost(KISSMANGA_ID, URL + '?confirm=yes'));
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
    mangaInfo.website := WebsiteRoots[KISSMANGA_ID, 0];

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos('<title>', parse.Strings[i]) > 0) then
        mangaInfo.title := TrimLeft(
          TrimRight(GetString('~!@' + parse.Strings[i + 1], '~!@', ' manga | ')));

      // get cover link
      if GetTagName(parse.Strings[i]) = 'img' then
        if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'width=')) = '190px') then
          mangaInfo.coverLink :=
            EncodeURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src')));

      // get summary
      if (Pos('Summary:', parse.Strings[i]) <> 0) and
        (isExtractSummary) then
      begin
        j := i + 4;
        mangaInfo.summary := '';
        while (Pos('</p>', parse.Strings[j]) = 0) and (j < parse.Count - 1) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := StringFilter(parse.Strings[j]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
          end;
          Inc(j);
        end;
        isExtractSummary := False;
      end;

      if (not isExtractChapter) and (Pos('Chapter Name', parse.Strings[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (GetTagName(parse.Strings[i]) = 'a') and
        (Pos('?id=', parse.Strings[i]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(
          StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
          WebsiteRoots[KISSMANGA_ID, 1], '', [rfReplaceAll]));
        parse.Strings[i + 1] := StringReplace(parse.Strings[i + 1], #10, '', [rfReplaceAll]);
        parse.Strings[i + 1] := StringReplace(parse.Strings[i + 1], #13, '', [rfReplaceAll]);
        parse.Strings[i + 1] := TrimLeft(parse.Strings[i + 1]);
        parse.Strings[i + 1] := TrimRight(parse.Strings[i + 1]);
        mangaInfo.chapterName.Add(
          StringFilter(TrimRight(RemoveSymbols(parse.Strings[i + 1]))));
      end;

      if (isExtractChapter) and
        (Pos('fb-comments', parse.Strings[i]) > 0) then
        isExtractChapter := False;

      // get authors
      if (Pos('Author:', parse.Strings[i]) <> 0) then
        mangaInfo.authors := parse.Strings[i + 4];

      // get artists
      if (Pos('Artist:', parse.Strings[i]) <> 0) then
        mangaInfo.artists := parse.Strings[i + 4];

      // get genres
      if (Pos('Genres:', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
        mangaInfo.genres := '';
      end;

      if isExtractGenres then
      begin
        if (i + 1 < parse.Count) and (Pos('"/Genre/', parse.Strings[i]) > 0) then
          mangaInfo.genres := mangaInfo.genres +
            (TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ');
        if Pos('</p>', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (Pos('Status:', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 2]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterName.Count > 1 then
    begin
      i := 0;
      j := mangainfo.ChapterName.Count - 1;
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
