  function GetMangasPROJECTInfoFromURL: Byte;
  var
    s: String;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
    i, j, n: Cardinal;
    numberOfPage: Cardinal = 1;

    procedure ExtractChapter;
    begin
      if (not isExtractChapter) and (Pos('class="chapter_cont', parse[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('class="nome', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetString(parse[i + 22], 'href="', '"'),
          WebsiteRoots[MANGASPROJECT_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse[i + 1])));
        s := StringFilter(HTMLEntitiesFilter(s));
        if Length(s) >= 3 then
        begin
          s := Copy(s, 1, Length(s) - 2);
          s := StringReplace(s, '- ' + #39, '- ', []);
        end;
        mangaInfo.chapterName.Add(s);
      end;

      if (isExtractChapter) and
        (Pos('paginacao', parse[i]) > 0) then
        isExtractChapter := False; //bermasalah
    end;

  begin
    mangaInfo.url := FillMangaSiteHost(MANGASPROJECT_ID, AURL);
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
    mangaInfo.website := WebsiteRoots[MANGASPROJECT_ID, 0];
    mangaInfo.status := '0';
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
      //Get number of pages
      //Don't know why unicode pos not work here
      if (Pos('tima', parse[i]) > 0) and
        (Pos('gina', parse[i]) > 0) then
        numberOfPage := StrToInt(GetString(parse[i - 1], '/page/', '">'));

      // get cover
      if Pos('class="side"', parse[i]) > 0 then
        mangaInfo.coverLink := CorrectURL(GetVal(parse[i + 2], 'src'));

      // get title
      if (Pos('class="text', parse[i]) > 0) then
        mangaInfo.title := parse[i + 3];

      // get summary
      if (Pos('class="text', parse[i]) <> 0) then
      begin
        j := i + 9;
        while (j < parse.Count) and (Pos('</div>', parse[j]) = 0) do
        begin
          s := parse[j];
          if s[1] <> '<' then
          begin
            parse[j] := HTMLEntitiesFilter(
              StringFilter(TrimLeft(parse[j])));
            parse[j] := StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
            parse[j] := StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse[j];
            Break;
          end;
          Inc(j);
        end;
      end;

      ExtractChapter;

      // get authors
      if (Pos('class="text', parse[i]) <> 0) then
        mangaInfo.authors := TrimLeft(StringFilter(parse[i + 7]));

      // get artists
      if (Pos('class="text', parse[i]) <> 0) then
        mangaInfo.artists := TrimLeft(StringFilter(parse[i + 7]));

      // get genres
      if (Pos('class="cat', parse[i]) <> 0) then
      begin
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if Pos('', parse[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse[i + 1])) + ', ';
        if Pos('</br>', parse[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 2 < parse.Count) and (Pos('Status', parse[i]) <> 0) then
      begin
        if Pos('Ongoing', parse[i + 3]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
      end;
    end;

    //Extract chapter
    if numberOfPage > 1 then
    begin
      for n := 2 to numberOfPage do
      begin
        Source.Clear;
        s := mangaInfo.url + '/' + IntToStr(n);
        if not GetPage(TObject(Source), mangaInfo.url + '/page/' +
          IntToStr(n), AReconnect) then
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
