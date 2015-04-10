  function GetTruyenTranhTuanInfoFromURL: Byte;
  var
    s: String;
    isExtractSummary: Boolean = True;
    isExtractGenres: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(TRUYENTRANHTUAN_ID, URL);// + '&confirm=yes';
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
    mangaInfo.website := WebsiteRoots[TRUYENTRANHTUAN_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (GetTagName(parse.Strings[i]) = 'meta') and
        (Pos('property="og:image"', parse.Strings[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i], 'content=')));

      // get summary
      if (Pos('id="manga-summary"', parse.Strings[i]) <> 0) and
        (isExtractSummary) then
      begin
        j := i + 4;
        while (j < parse.Count) and (Pos('</p>', parse.Strings[j]) = 0) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
          end;
          Inc(j);
        end;
        isExtractSummary := False;
      end;

      // get title
      if (Pos('<title>', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := Trim(StringFilter(GetString('~!@' + parse.Strings[i + 1],
          '~!@', ' - Truyện tranh online - truyentranhtuan.com')));

      // get chapter name and links
      if (Pos('class="chapter-name"', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetString(parse.Strings[i + 2], 'href="', '"');
        s := StringReplace(s, WebsiteRoots[TRUYENTRANHTUAN_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(Trim(parse.Strings[i + 3]));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;

      // get authors
      if (i + 1 < parse.Count) and (Pos('Tác giả:', parse.Strings[i]) <> 0) then
        mangaInfo.authors := Trim(parse.Strings[i + 2]);

      // get artists
      //if (i+1<parse.Count) AND (Pos('/search/artist/', parse.Strings[i])<>0) then
      //  mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

      // get genres
      if (Pos('Thể loại:', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
        mangaInfo.genres := '';
      end;

      if isExtractGenres then
      begin
        if Pos('<a href=', parse.Strings[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
        if Pos('</span>', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 5 < parse.Count) and (Pos('Chương mới nhất', parse.Strings[i]) <> 0) then
      begin
        if Pos('dang-tien-hanh', parse.Strings[i + 1]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
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
