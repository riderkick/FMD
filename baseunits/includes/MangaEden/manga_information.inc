  function GetMangaEdenInfoFromURL(const root: String): Byte;
  var
    s: String;
    isExtractSummary: Boolean = True;
    isExtractGenres: Boolean = False;
    i, j: Cardinal;
  begin
    if Pos('http', LowerCase(URL)) = 0 then
      mangaInfo.url := root + URL;// + '&confirm=yes';
    if Pos('/en-manga/', URL) > 0 then
      mangaInfo.genres := 'English, '
    else
      mangaInfo.genres := 'Italian, ';
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
    if root = WebsiteRoots[MANGAEDEN_ID, 1] then
      mangaInfo.website := WebsiteRoots[MANGAEDEN_ID, 0]
    else
      mangaInfo.website := WebsiteRoots[PERVEDEN_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (GetTagName(parse.Strings[i]) = 'div') and
        (Pos('class="mangaImage2"', parse.Strings[i]) > 0) then
      begin
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i + 1], 'src=')));
        if Copy(mangaInfo.coverLink, 1, 2) = '//' then
          mangaInfo.coverLink := 'http:' + mangaInfo.coverLink;
      end;

      // get summary
      if (Pos('hr style="margin-top:0;', parse.Strings[i]) <> 0) and
        (isExtractSummary) then
      begin
        j := i + 2;
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
        isExtractSummary := False;
      end;

      // get title
      if ((Pos('enIcon', parse.Strings[i]) <> 0) or (Pos('itIcon', parse.Strings[i]) <> 0)) and
        (mangaInfo.title = '') then
      begin
        mangaInfo.title := StringFilter(TrimRight(TrimLeft(parse.Strings[i + 1])));
        mangaInfo.title := GetString('~!@' + mangaInfo.title, '~!@', ' Manga');
      end;

      // get chapter name and links
      if (i + 7 < parse.Count) and (Pos('class="chapterLink"', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetString(parse.Strings[i], 'href="', '1/"');
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 3]))) +
          ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 7])));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
      end;

      // get authors
      if (i + 1 < parse.Count) and (Pos('/?author', parse.Strings[i]) <> 0) then
        mangaInfo.authors := TrimLeft(parse.Strings[i + 1]);

      // get artists
      if (i + 1 < parse.Count) and (Pos('/?artist', parse.Strings[i]) <> 0) then
        mangaInfo.artists := TrimLeft(parse.Strings[i + 1]);

      // get genres
      if (Pos('Genres', parse.Strings[i]) <> 0) then
      begin
        isExtractGenres := True;
      end;

      if isExtractGenres then
      begin
        if Pos('/?categories', parse.Strings[i]) <> 0 then
          mangaInfo.genres := mangaInfo.genres +
            TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
        if Pos('<br />', parse.Strings[i]) <> 0 then
          isExtractGenres := False;
      end;

      // get status
      if parse.Strings[i] = 'Status' then
      begin
        if Pos('Ongoing', parse.Strings[i + 2]) <> 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
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
