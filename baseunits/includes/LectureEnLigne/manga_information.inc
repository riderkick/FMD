  function GetLectureEnLigneInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i, j: Integer;
  begin
    mangaInfo.url := FillMangaSiteHost(LECTUREENLIGNE_ID, URL);
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
    mangaInfo.website := WebsiteRoots[LECTUREENLIGNE_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('class="imagemanga"', parse.Strings[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(WebsiteRoots[LECTUREENLIGNE_ID, 1] +
          '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

      // get title
      if (Pos('<title>', parse.Strings[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(
          TrimRight(HTMLEntitiesFilter(GetString('~!@' + parse.Strings[i + 1],
          '~!@', ' - Lecture-en-ligne.com'))));

      // get chapter name and links
      if (Pos('class="table"', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
          '../..', '', []);
        if Pos('.htm', s) > 0 then
          s := ReplaceRegExpr('/\d+\.html?$', s, '', False);
        mangaInfo.chapterLinks.Add(s);
        j := i - 1;
        while (Pos('<', parse.Strings[j]) > 0) or (Trim(parse.Strings[j]) = '') do
          Dec(j);
        s := RemoveSymbols(Trim(parse.Strings[j]));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('class=''comments''', parse.Strings[i]) > 0) then
        isExtractChapter := False;

      // get summary
      if (Pos('Résumé :', parse.Strings[i]) <> 0) and
        (Pos('<p', parse.Strings[i - 1]) <> 0) then
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
      if (i + 3 < parse.Count) and (Pos('Auteur :', parse.Strings[i]) <> 0) then
        mangaInfo.authors := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 3])));

      // get artists
      if (i + 3 < parse.Count) and (Pos('Dessinateur :', parse.Strings[i]) <> 0) then
        mangaInfo.artists := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 3])));

      // get genres
      if (i + 3 < parse.Count) and (Pos('Genres :', parse.Strings[i]) <> 0) then
        mangaInfo.genres := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 3])));


      // get status
      if (i + 5 < parse.Count) and (Pos('Statut :', parse.Strings[i]) <> 0) then
      begin
        if (Pos('Terminé', parse.Strings[i + 3]) <> 0) or
          (Pos('one shot', parse.Strings[i + 3]) <> 0) then
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
