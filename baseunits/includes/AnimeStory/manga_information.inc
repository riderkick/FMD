  function GetAnimeStoryInfoFromURL: Byte;
  var
    s: String;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[ANIMESTORY_ID, 0];
    mangaInfo.url := FillMangaSiteHost(ANIMESTORY_ID, '/' + URL);
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
   { if (mangaInfo.coverLink = '') AND
       (Pos('imageanchor="1"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
    }
      // get title
      if (Pos('<title>', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(
          TrimRight(HTMLEntitiesFilter(GetString('~!@' + parse.Strings[i + 1],
          '~!@', ' | Anime-Story'))));

      if (not isExtractChapter) and (Pos('Liste des Chapitres', parse.Strings[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('href=', parse.Strings[i]) > 0) and
        (Pos('Lecture', parse.Strings[i + 1]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
          WebsiteRoots[ANIMESTORY_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(
          TrimRight(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')))));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('class=''comments''', parse.Strings[i]) > 0) then
        isExtractChapter := False;

      // get summary
      if (Pos('Synopsis', parse.Strings[i]) <> 0) and
        (Pos('<h2>', parse.Strings[i - 1]) <> 0) then
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
      end;

      // get authors
      if (i + 5 < parse.Count) and (Pos('Auteur :', parse.Strings[i]) <> 0) then
        mangaInfo.authors := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 5])));

      // get artists
      if (i + 5 < parse.Count) and (Pos('Illustrateur :', parse.Strings[i]) <> 0) then
        mangaInfo.artists := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 5])));

      // get genres
      if (Pos('Sous-genre(s) :', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if Pos('search?subgenre', parse.Strings[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
        if Pos('</td>', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if (i + 5 < parse.Count) and (Pos('Statut :', parse.Strings[i]) <> 0) then
      begin
        if (Pos('terminé', parse.Strings[i + 5]) <> 0) or
          (Pos('one shot', parse.Strings[i + 5]) <> 0) then
          mangaInfo.status := '0'   // completed
        else
          mangaInfo.status := '1';  // ongoing
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
