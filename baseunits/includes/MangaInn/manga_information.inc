  function GetMangaInnInfoFromURL: Byte;
  var
    i, j: Cardinal;
    s: String;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGAINN_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGAINN_ID, 0];

    if parse.Count = 0 then
      Exit;

    // using parser
    mangaInfo.genres := '';
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos('<title>', parse.Strings[i]) > 0) then
        mangaInfo.title := GetString(parse.Strings[i + 1], ' - Read ', ' Online For Free');

      // get cover link
      if GetTagName(parse.Strings[i]) = 'img' then
        if Pos('/mangas/logos/', parse.Strings[i]) <> 0 then
          mangaInfo.coverLink :=
            CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

      // get summary
      if (Pos('Summary', parse.Strings[i])) <> 0 then
      begin
        j := i;
        while Pos('</td>', parse.Strings[j]) = 0 do
        begin
          Inc(j);
          if (GetTagName(parse.Strings[j]) = 'span') and
            (GetTagAttribute(parse.Strings[j], 'class=') <> '') then
          begin
            parse.Strings[j + 1] := StringFilter(parse.Strings[j + 1]);
            parse.Strings[j + 1] :=
              StringReplace(parse.Strings[j + 1], #10, '\n', [rfReplaceAll]);
            parse.Strings[j + 1] :=
              StringReplace(parse.Strings[j + 1], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := Trim(parse.Strings[j + 1]);
          end;
        end;
      end;

      // get authors
      if (Pos('Author(s)', parse.Strings[i]) <> 0) then
        mangaInfo.authors := TrimLeft(TrimRight(parse.Strings[i + 4]));

      // get artists
      if (Pos('Artist(s)', parse.Strings[i]) <> 0) then
        mangaInfo.artists := TrimLeft(TrimRight(parse.Strings[i + 4]));

      // get genres
      if (Pos('class="RedHeadLabel"', parse[i]) > 0) then
        if (Pos('Genre(s)', parse[i + 1]) > 0) then
          if mangaInfo.genres <> '' then
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 5])
          else
            mangaInfo.genres := Trim(parse[i + 5]);

      // get type(genres)
      if (Pos('class="RedHeadLabel"', parse[i]) > 0) then
        if (Pos('Type', parse[i + 1]) > 0) then
          if mangaInfo.genres <> '' then
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 6])
          else
            mangaInfo.genres := Trim(parse[i + 6]);

      // get status
      if (Pos('Status', parse.Strings[i]) <> 0) then
      begin
        if Pos('Ongoing', parse.Strings[i + 3]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
        if Pos('Completed', parse.Strings[i + 3]) <> 0 then
          mangaInfo.status := '0';  // completed
      end;

      if Pos('class="m"', parse[i]) > 0 then
        if Pos('Chapter Name', parse[i + 1]) > 0 then
          isExtractChapters := True;
      //if isExtractChapters and
      //(Pos('</table', parse[i]) > 0) then
      //isExtractChapters := False;
      // get chapter name and links
      if isExtractChapters and
        (Pos('/manga/chapter/', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(
          CorrectURL(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[MANGAINN_ID, 1],
          '', [rfReplaceAll])));
        s := Trim(parse[i + 2]);
        if Length(s) > 0 then
          if s[Length(s)] = ':' then
            Delete(s, Length(s), 1);
        s := Trim(s);
        if Trim(parse[i + 4]) <> '' then
          s := s + ' ' + Trim(parse[i + 4]);
        mangaInfo.chapterName.Add(Trim(StringFilter(RemoveSymbols(s))));
      end;
    end;
    Result := NO_ERROR;
  end;
