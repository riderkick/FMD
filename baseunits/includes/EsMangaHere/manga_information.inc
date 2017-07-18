  function GetEsMangaHereInfoFromURL: Byte;
  var
    i, j: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[ESMANGAHERE_ID, 0];
    mangaInfo.url := FillMangaSiteHost(ESMANGAHERE_ID, AURL);
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

    mangaInfo.status := '1';

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos('<title>', parse[i]) > 0) then
        mangaInfo.title := GetString(parse[i + 1], 'Manga - Leer ',
          ' manga en Español');

      // get cover link
      if GetTagName(parse[i]) = 'img' then
        if (GetVal(parse[i], 'class') = 'img') then
          mangaInfo.coverLink :=
            GetVal(parse[i], 'src');

      // get summary
      if (Pos('id="show"', parse[i])) <> 0 then
      begin
        parse[i + 1] := StringFilter(parse[i + 1]);
        parse[i + 1] := StringReplace(parse[i + 1], #10, '\n', [rfReplaceAll]);
        parse[i + 1] := StringReplace(parse[i + 1], #13, '\r', [rfReplaceAll]);
        mangaInfo.summary := parse[i + 1];
      end;

      // get chapter name and links
      if (GetTagName(parse[i]) = 'a') and
        (GetVal(parse[i], 'class') = 'color_0077') and
        (Pos('/manga/', GetVal(parse[i],
        'href')) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(
          StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[ESMANGAHERE_ID, 1], '', [rfReplaceAll]));
        parse[i + 1] := StringReplace(parse[i + 1], #10, '', [rfReplaceAll]);
        parse[i + 1] := StringReplace(parse[i + 1], #13, '', [rfReplaceAll]);
        parse[i + 1] := TrimLeft(parse[i + 1]);
        parse[i + 1] := TrimRight(parse[i + 1]);
        mangaInfo.chapterName.Add(
          StringFilter(TrimRight(RemoveSymbols(parse[i + 1]))));
      end;

      // get authors
      if (Pos('Autor(s):', parse[i]) <> 0) then
        mangaInfo.authors := parse[i + 3];

      // get artists
      if (Pos('Artist(s):', parse[i]) <> 0) then
        mangaInfo.artists := parse[i + 3];

      // get genres
      if (Pos('Género(s):', parse[i]) <> 0) then
      begin
        mangaInfo.genres := '';
        for j := 0 to 37 do
          if Pos(LowerCase(defaultGenres[j]), LowerCase(parse[i + 2])) <> 0 then
            mangaInfo.genres := mangaInfo.genres + (defaultGenres[j] + ', ');
      end;

   { // get status
    if (Pos('Status:', parse[i])<>0) then
    begin
      if Pos('Ongoing', parse[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end; }
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterName.Count > 1 then
    begin
      i := 0;
      j := mangainfo.ChapterName.Count - 1;
      while (i < j) do
      begin
        mangainfo.ChapterName.Exchange(i, j);
        mangainfo.chapterLinks.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    end;

    // Delete 'latest' chapter because it isnt exist
 { if (mangaInfo.status = '1') AND (mangainfo.ChapterName.Count > 0) then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.chapterLinks.Delete(mangainfo.chapterLinks.Count-1);
  end; }
    Result := NO_ERROR;
  end;
