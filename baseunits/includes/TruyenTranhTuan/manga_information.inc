  function GetTruyenTranhTuanInfoFromURL: Byte;
  var
    s: String;
    isExtractSummary: Boolean = True;
    isExtractGenres: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(TRUYENTRANHTUAN_ID, AURL);// + '&confirm=yes';
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
    mangaInfo.website := WebsiteRoots[TRUYENTRANHTUAN_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (GetTagName(parse[i]) = 'meta') and
        (Pos('property="og:image"', parse[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetVal(parse[i], 'content'));

      // get summary
      if (Pos('id="manga-summary"', parse[i]) <> 0) and
        (isExtractSummary) then
      begin
        j := i + 4;
        while (j < parse.Count) and (Pos('</p>', parse[j]) = 0) do
        begin
          s := parse[j];
          if s[1] <> '<' then
          begin
            parse[j] := HTMLEntitiesFilter(StringFilter(parse[j]));
            parse[j] := StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
            parse[j] := StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse[j];
          end;
          Inc(j);
        end;
        isExtractSummary := False;
      end;

      // get title
      if (Pos('<title>', parse[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := Trim(StringFilter(GetString('~!@' + parse[i + 1],
          '~!@', ' - Truyện tranh online - truyentranhtuan.com')));

      // get chapter name and links
      if (Pos('class="chapter-name"', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetString(parse[i + 2], 'href="', '"');
        s := StringReplace(s, WebsiteRoots[TRUYENTRANHTUAN_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(Trim(parse[i + 3]));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;

      // get authors
      if (i + 1 < parse.Count) and (Pos('Tác giả:', parse[i]) <> 0) then
        mangaInfo.authors := Trim(parse[i + 2]);

      // get artists
      //if (i+1<parse.Count) AND (Pos('/search/artist/', parse[i])<>0) then
      //  mangaInfo.artists:= TrimLeft(parse[i+1]);

      // get genres
      if (Pos('Thể loại:', parse[i]) <> 0) then
      begin
        isExtractGenres := True;
        mangaInfo.genres := '';
      end;

      if isExtractGenres then
      begin
        if Pos('<a href=', parse[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse[i + 1])) + ', ';
        if Pos('</span>', parse[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 5 < parse.Count) and (Pos('Chương mới nhất', parse[i]) <> 0) then
      begin
        if Pos('dang-tien-hanh', parse[i + 1]) <> 0 then
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
