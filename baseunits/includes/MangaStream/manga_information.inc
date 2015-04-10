  function GetMangaStreamInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = True;
    i, j: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGASTREAM_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGASTREAM_ID, 0];

    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.status := '1';

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // do not allow to get chapter name and links
      if (isExtractChapter) and
        (Pos('</table>', parse.Strings[i]) > 0) then
        isExtractChapter := False;

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('<td>', parse.Strings[i]) > 0) and
        (Pos('</td>', parse.Strings[i + 4]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i + 1], 'href=')),
          MANGASTREAM_ROOT2, '', []);
        Delete(s, Length(s) - 1, 2);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i + 2])));
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
