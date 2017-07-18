  function GetKivmangaInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(KIVMANGA_ID, AURL);
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

    mangaInfo.website := WebsiteRoots[KIVMANGA_ID, 0];
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
      if (mangaInfo.title = '') and (Pos('<title>', parse[i]) > 0) then
        mangaInfo.title := TrimLeft(TrimRight(GetString(parse[i + 1],
          'Read Free Manga Online - ', '-')));

      if (isExtractChapter) and (Pos('option value=', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := AURL + '/' + GetVal(parse[i], 'value');
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse[i + 1])));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;

      if (isExtractChapter) and
        (Pos('</select>', parse[i]) > 0) then
      begin
        isExtractChapter := False;
        Break;
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
