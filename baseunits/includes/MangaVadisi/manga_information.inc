  function GetMangaVadisiInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGAVADISI_ID, MANGAVADISI_BROWSER + URL);
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

    mangaInfo.website := WebsiteRoots[MANGAVADISI_ID, 0];
    mangaInfo.status := '1';
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
      // get chapter name and links
      if (Pos('select name="chapter"', parse[i]) > 0) then
        isExtractChapter := True;

      // get manga name
      if (mangaInfo.title = '') and (Pos('Manga Vadisi - ', parse[i]) > 0) then
        mangaInfo.title := GetString(parse[i], 'Manga Vadisi - ', ' - Chapter');

      if (isExtractChapter) and (Pos('</select>', parse[i]) > 0) then
        Break;

      if (isExtractChapter) and (Pos('option value=', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := URL + '/' + GetVal(parse[i], 'value');
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse[i + 1])));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    Result := NO_ERROR;
  end;
