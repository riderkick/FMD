  function GetS2scanInfoFromURL: Byte;
  var
    isExtractChapter: Boolean = True;
    s: String;
    i, j: Cardinal;
    // stream: TStringStream;
    // HTTP: THTTPSend;
  begin
    mangaInfo.url := FillMangaSiteHost(S2SCAN_ID, URL);
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
    mangaInfo.website := WebsiteRoots[S2SCAN_ID, 0];
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and
        (GetTagName(parse.Strings[i]) = 'img') and
        (Pos('class="thumbnail"', parse.Strings[i - 2]) > 0) then
        mangaInfo.coverLink := CorrectURL(GetAttributeValue(
          GetTagAttribute(parse.Strings[i], 'src=')));

      // get title
      if (Pos('h1 class="title"', parse.Strings[i]) <> 0) and
        (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(TrimRight(StringFilter(parse.Strings[i + 2])));

      if (isExtractChapter) and
        (Pos('class="element"', parse.Strings[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetString(parse.Strings[i + 3], 'href="', '"'),
          WebsiteRoots[S2SCAN_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 4])));
        mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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
