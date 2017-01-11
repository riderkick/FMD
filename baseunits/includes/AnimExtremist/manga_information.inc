  function GetAnimeExtremistInfoFromURL: Byte;
  var
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(ANIMEEXTREMIST_ID, AURL);
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
    mangaInfo.website := WebsiteRoots[ANIMEEXTREMIST_ID, 0];
    mangaInfo.genres := '';

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos(' Manga - Animextremist', parse[i]) > 0) then
        mangaInfo.title := GetString('~!@' + parse[i], '~!@',
          ' Manga - Animextremist');

      // get cover link
      if (mangaInfo.coverLink = '') and
        (GetTagName(parse[i]) = 'img') then
        if Pos('src="../', parse[i]) > 0 then
          mangaInfo.coverLink := WebsiteRoots[ANIMEEXTREMIST_ID, 1] +
            GetString(parse[i], 'src="..', '"');

      // get summary
      if (Pos('align="justify" class="style33"', parse[i])) <> 0 then
      begin
        j := i + 1;
        mangaInfo.summary := '';
        while (Pos('<td height', parse[j]) = 0) and (j < parse.Count - 1) do
        begin
          s := parse[j];
          if (s <> '') and (s[1] <> '<') then
          begin
            parse[j] := HTMLEntitiesFilter(StringFilter(parse[j]));
            parse[j] := StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
            parse[j] := StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse[j] + '\n\r';
          end;
          Inc(j);
        end;
      end;

      // get chapter name and links
      if (Pos('/mangas-online/', parse[i]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(
          StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[ANIMEEXTREMIST_ID, 1], '', [rfReplaceAll]));
        parse[i + 1] := StringReplace(parse[i + 1], #10, '', [rfReplaceAll]);
        parse[i + 1] := StringReplace(parse[i + 1], #13, '', [rfReplaceAll]);
        parse[i + 1] := TrimLeft(parse[i + 1]);
        parse[i + 1] := TrimRight(parse[i + 1]);
        mangaInfo.chapterName.Add(HTMLEntitiesFilter(
          StringFilter(TrimRight(RemoveSymbols(parse[i + 1])))));
      end;

   { // get authors
    if (Pos('Autor(s):', parse[i])<>0) then
      mangaInfo.authors:= parse[i+3];

    // get artists
    if (Pos('Artist(s):', parse[i])<>0) then
      mangaInfo.artists:= parse[i+3]; }

      // get genres
      if (Pos('ord=genero&id', parse[i]) <> 0) then
      begin
        mangaInfo.genres := mangaInfo.genres +
          (HTMLEntitiesFilter(TrimLeft(TrimRight(parse[i + 1]))) + ', ');
      end;

      // get status
      if (Pos('class="manga_estado"', parse[i]) <> 0) then
      begin
        if Pos('Completo', parse[i + 3]) <> 0 then
          mangaInfo.status := '0'   // completed
        else
          mangaInfo.status := '1';  // ongoing
      end;
    end;

    Result := NO_ERROR;
  end;
