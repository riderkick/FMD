  function GetMyReadingMangaInfoInfoFromURL: Byte;
  var
    s: String;
    i, j: Integer;
    isCategories: Boolean = False;
    isTags: Boolean = False;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MYREADINGMANGAINFO_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MYREADINGMANGAINFO_ID, AURL);
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

    if parse.Count = 0 then
      Exit;

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    for i := 0 to parse.Count - 1 do
    begin
      //title
      if (GetTagName(parse[i]) = 'h1') and
        (GetVal(parse[i], 'class') = 'entry-title') then
        mangaInfo.title := CommonStringFilter(parse[i + 1]);

      //author
      if (Pos('<strong>', parse[i]) <> 0) and (Pos('Author:', parse[i + 1]) <> 0) then
      begin
        s := StringReplace(parse[i + 1], 'Author:', '', []);
        s := StringReplace(s, #194#160, '', [rfReplaceAll]); //space
        mangaInfo.authors := Trim(s);
      end;

      //genres
      if (Pos('class="entry-categories"', parse[i]) > 0) and (isCategories = False) then
      begin
        j := i + 2;
        s := '';
        while (j < parse.Count - 1) do
        begin
          if (Pos('</span', parse[j]) > 0) then
            Break;
          if (Pos('<', parse[j]) = 0) then
            s := s + parse[j];
          Inc(j);
        end;
        s := StringFilter(Trim(s));
        if (mangaInfo.genres <> '') and (s <> '') then
          mangaInfo.genres := mangaInfo.genres + ', ' + Trim(s)
        else
        if s <> '' then
          mangaInfo.genres := Trim(s);
        isCategories := True;
      end;
      if (Pos('class="entry-tags"', parse[i]) > 0) and (isTags = False) then
      begin
        j := i + 2;
        s := '';
        while (j < parse.Count - 1) do
        begin
          if (Pos('</span', parse[j]) > 0) then
            Break;
          if (Pos('<', parse[j]) = 0) then
            s := s + parse[j];
          Inc(j);
        end;
        s := StringFilter(Trim(s));
        if (mangaInfo.genres <> '') and (s <> '') then
          mangaInfo.genres := mangaInfo.genres + ', ' + Trim(s)
        else
        if s <> '' then
          mangaInfo.genres := Trim(s);
        isTags := True;
      end;
      if (i + 1 < parse.Count - 1) then
        if (Pos('Pairing:', parse[i]) > 0) and (Pos('</strong', parse[i + 1]) > 0) then
        begin
          s := StringReplace(parse[i], 'Pairing:', '', []);
          s := StringReplace(s, #194#160, #32, [rfReplaceAll]);
          if (mangaInfo.genres <> '') and (s <> '') then
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(s)
          else
          if s <> '' then
            mangaInfo.genres := Trim(s);
        end;

      //chapters
      if Pos('class="entry-pagination pagination', parse[i]) <> 0 then
        isExtractChapters := True;
      if isExtractChapters and (Pos('</div', parse[i]) <> 0) then
        isExtractChapters := False;
      if isExtractChapters and (Pos('<a', parse[i]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(RemoveHostFromURL(GetVal(parse[i], 'href')));
        mangaInfo.chapterName.Add(mangaInfo.title + ' Ch. ' + IntToStr(mangaInfo.numChapter + 1));
      end;
    end;

    //chapter 1
    Inc(mangaInfo.numChapter);
    mangaInfo.chapterLinks.Insert(0, AURL);
    if mangaInfo.numChapter = 1 then
      mangaInfo.chapterName.Insert(0, mangaInfo.title)
    else
      mangaInfo.chapterName.Insert(0, mangaInfo.title + ' Ch. 1');

    //no status = completed
    mangaInfo.status := '0';

    Result := NO_ERROR;
  end;
