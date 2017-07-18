  function GetLectureEnLigneInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i, j: Integer;
  begin
    mangaInfo.url := FillMangaSiteHost(LECTUREENLIGNE_ID, AURL);
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
    mangaInfo.website := WebsiteRoots[LECTUREENLIGNE_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('class="imagemanga"', parse[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(WebsiteRoots[LECTUREENLIGNE_ID, 1] +
          '/' + GetVal(parse[i], 'src'));

      // get title
      if (Pos('<title>', parse[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(
          TrimRight(HTMLEntitiesFilter(GetString('~!@' + parse[i + 1],
          '~!@', ' - Lecture-en-ligne.com'))));

      // get chapter name and links
      if (Pos('class="table"', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetVal(parse[i], 'href'),
          '../..', '', []);
        if Pos('.htm', s) > 0 then
          s := ReplaceRegExpr('/\d+\.html?$', s, '', False);
        mangaInfo.chapterLinks.Add(s);
        j := i - 1;
        while (Pos('<', parse[j]) > 0) or (Trim(parse[j]) = '') do
          Dec(j);
        s := RemoveSymbols(Trim(parse[j]));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('class=''comments''', parse[i]) > 0) then
        isExtractChapter := False;

      // get summary
      if (Pos('Résumé :', parse[i]) <> 0) and
        (Pos('<p', parse[i - 1]) <> 0) then
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
      end;

      // get authors
      if (i + 3 < parse.Count) and (Pos('Auteur :', parse[i]) <> 0) then
        mangaInfo.authors := StringFilter(TrimLeft(TrimRight(parse[i + 3])));

      // get artists
      if (i + 3 < parse.Count) and (Pos('Dessinateur :', parse[i]) <> 0) then
        mangaInfo.artists := StringFilter(TrimLeft(TrimRight(parse[i + 3])));

      // get genres
      if (i + 3 < parse.Count) and (Pos('Genres :', parse[i]) <> 0) then
        mangaInfo.genres := StringFilter(TrimLeft(TrimRight(parse[i + 3])));


      // get status
      if (i + 5 < parse.Count) and (Pos('Statut :', parse[i]) <> 0) then
      begin
        if (Pos('Terminé', parse[i + 3]) <> 0) or
          (Pos('one shot', parse[i + 3]) <> 0) then
          mangaInfo.status := '0'   // completed
        else
          mangaInfo.status := '1';  // ongoing
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterLinks.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
