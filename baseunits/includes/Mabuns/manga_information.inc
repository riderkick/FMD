  function GetMabunsInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MABUNS_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MABUNS_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (Pos('rel="image_src"', parse[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetVal(parse[i], 'href'));

      // get title
      if (Pos('Judul :', parse[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(
          TrimRight(HTMLEntitiesFilter(StringFilter(GetString(parse[i] + '~!@',
          'Judul :', '~!@')))));

      if (not isExtractChapter) and (Pos('Baca Online:', parse[i]) > 0) then
        isExtractChapter := True;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('<a href', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[MABUNS_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse[i + 1])));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;

      if (isExtractChapter) and
        (Pos('</table>', parse[i]) > 0) then
        isExtractChapter := False;

      // get authors
      if (i + 8 < parse.Count) and (Pos('Author :', parse[i]) <> 0) then
        mangaInfo.authors := TrimLeft(
          TrimRight(GetString(parse[i] + '~!@', 'Author :', '~!@')));

      // get artists
      if (i + 1 < parse.Count) and (Pos('Artist :', parse[i]) <> 0) then
        mangaInfo.artists := TrimLeft(
          TrimRight(GetString(parse[i] + '~!@', 'Artist :', '~!@')));

      // get genres
      if (Pos('Genre :', parse[i]) <> 0) then
      begin
        mangaInfo.genres := TrimLeft(
          TrimRight(GetString(parse[i] + '~!@', 'Genre :', '~!@')));
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
