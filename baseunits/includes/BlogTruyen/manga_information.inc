  function GetBlogTruyenInfoFromURL: Byte;
  var
    s: String;
    isExtractSummary: Boolean = True;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = True;
    i, j: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[BLOGTRUYEN_ID, 0];
    mangaInfo.url := FillMangaSiteHost(BLOGTRUYEN_ID, URL);
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

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('div class="thumbnail"', parse.Strings[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i + 2], 'src=')));

      // get summary
      if (Pos('class="content"', parse.Strings[i]) <> 0) and
        (isExtractSummary) then
      begin
        j := i + 1;
        while (j < parse.Count) and (Pos('</div>', parse.Strings[j]) = 0) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
            Break;
          end;
          Inc(j);
        end;
        isExtractSummary := False;
      end;

      // get title
      if (Pos('class="entry-title"', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(parse.Strings[i + 3])));

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('class="publishedDate"', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetAttributeValue(GetTagAttribute(parse.Strings[i - 5], 'href='));
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(Trim(parse.Strings[i - 4]));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('class="al-c social-button"', parse.Strings[i]) > 0) then
        isExtractChapter := False;

      // get authors
      if (i + 2 < parse.Count) and (Pos('Tác giả:', parse.Strings[i]) <> 0) then
        mangaInfo.authors := TrimLeft(TrimRight(parse.Strings[i + 2]));

      // get artists
      //if (i+1<parse.Count) AND (Pos('/search/artist/', parse.Strings[i])<>0) then
      //  mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

      // get genres
      if (not isExtractGenres) and (Pos('class="category"', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
        mangaInfo.genres := '';
      end;

      if isExtractGenres then
      begin
        s := TrimLeft(TrimRight(parse.Strings[i]));
        if Pos('Đăng bởi:', s) <> 0 then
          isExtractGenres := False
        else
        if (s <> '') and (s[1] <> '<') and
          (Pos('class="category"', parse.Strings[i - 2]) <> 0) then
          mangaInfo.genres := mangaInfo.genres + s + ', ';
      end;

      // get status
      if (i + 2 < parse.Count) and (Pos('Trạng thái:', parse.Strings[i]) <> 0) then
      begin
        if (Pos('Đang tiến hành', parse.Strings[i + 2]) <> 0) or
          (Pos('Tạm ngưng', parse.Strings[i + 2]) <> 0) then
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
