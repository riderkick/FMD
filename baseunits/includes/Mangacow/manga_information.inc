  function GetMangaCowInfoFromURL: Byte;
  var
    s: String;
    i, j: Cardinal;
    isExtractInfo: Boolean = False;
    isExtractGenres: Boolean = False;
    chapterPage: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[MANGACOW_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGACOW_ID, URL);

    if mangaInfo.url[Length(mangaInfo.url)] <> '/' then
      mangaInfo.url := mangaInfo.url + '/';

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

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    chapterPage := 0;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (GetTagName(parse[i]) = 'img') and (GetVal(parse[i], 'class') = 'cvr') then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      // get title
      if (Pos('<h1 ', parse[i]) > 0) and
        (Pos('class="ttl"', parse[i]) > 0) then
        mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));

      // get chapter name and links
      if Pos('class="lst"', parse[i]) > 0 then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[MANGACOW_ID, 1],
          '', [rfIgnoreCase]);
        mangaInfo.chapterLinks.Add(s);
        s := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 3])));
        mangaInfo.chapterName.Add(s);
      end;

      if (Pos('<a ', parse[i]) > 0) and (Pos('/chapter-list/', parse[i]) > 0) then
        if Trim(parse[i + 1]) = 'Last' then
          chapterPage := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$',
            GetVal(parse[i], 'href'), '$1', True), 0);

      if (Pos('class="mng_ifo"', parse[i]) > 0) then
        isExtractInfo := True;
      if isExtractInfo and
        (Pos('class="lst mng_chp"', parse[i]) > 0) then
        isExtractInfo := False;

      if isExtractInfo then
      begin
        // get summary
        if (Pos('Subscribe', parse[i]) > 0) then
          if (Pos('</b>', parse[i + 1]) > 0) then
            mangaInfo.summary :=
              Trim(BreaksString(HTMLEntitiesFilter(StringFilter(parse[i + 12]))));

        // get authors
        if (mangaInfo.authors = '') and
          (Trim(parse[i]) = 'Author') then
        begin
          j := i + 1;
          while j < parse.Count - 1 do
          begin
            if Pos('</p', parse[j]) > 0 then
              Break;
            if Pos('<', parse[j]) = 0 then
            begin
              if Trim(parse[j]) = ',' then
                mangaInfo.authors := mangaInfo.authors + ', '
              else
                mangaInfo.authors := mangainfo.authors + Trim(parse[j]);
            end;
            Inc(j);
          end;
          mangaInfo.authors := Trim(RemoveStringBreaks(StringFilter(mangaInfo.authors)));
          mangaInfo.authors := Trim(ReplaceRegExpr('^[\:|\-]',
            Trim(mangaInfo.authors), '', False));
          mangaInfo.authors := Trim(ReplaceRegExpr('^[\:|\-]',
            Trim(mangaInfo.authors), '', False));
        end;

        // get artists
        if (mangaInfo.artists = '') and
          (Trim(parse[i]) = 'Artist') then
        begin
          j := i + 1;
          while j < parse.Count - 1 do
          begin
            if Pos('</p', parse[j]) > 0 then
              Break;
            if Pos('<', parse[j]) = 0 then
            begin
              if Trim(parse[j]) = ',' then
                mangaInfo.artists := mangaInfo.artists + ', '
              else
                mangaInfo.artists := mangainfo.artists + Trim(parse[j]);
            end;
            Inc(j);
          end;
          mangaInfo.artists := Trim(RemoveStringBreaks(StringFilter(mangaInfo.artists)));
          mangaInfo.artists := Trim(ReplaceRegExpr('^[\:|\-]',
            Trim(mangaInfo.artists), '', False));
          mangaInfo.artists := Trim(ReplaceRegExpr('^[\:|\-]',
            Trim(mangaInfo.artists), '', False));
        end;

        // get genres
        if (Pos('Category', parse[i]) <> 0) then
        begin
          isExtractGenres := True;
        end;

        if isExtractGenres then
        begin
          if Pos('manga-list/category/', parse[i]) <> 0 then
            mangaInfo.genres := mangaInfo.genres +
              TrimLeft(TrimRight(parse[i + 1])) + ', ';
          if Pos('</p>', parse[i]) <> 0 then
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
    end;

    // get the rest of chapter list
    if chapterPage > 1 then
    begin
      for j := 2 to chapterPage do
      begin
        if GetPage(TObject(Source), mangaInfo.url + 'chapter-list/' +
          IntToStr(j) + '/', Reconnect) then
        begin
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
              if Pos('class="lst"', parse[i]) > 0 then
              begin
                Inc(mangaInfo.numChapter);
                s := StringReplace(GetVal(parse[i], 'href'),
                  WebsiteRoots[MANGACOW_ID, 1], '', [rfIgnoreCase]);
                mangaInfo.chapterLinks.Add(s);
                s := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 3])));
                mangaInfo.chapterName.Add(s);
              end;
            end;
          end;
        end;
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
    Source.Free;
  end;
