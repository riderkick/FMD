  function GetMangaOkuInfoFromURL: Byte;
  var
    s: String;
    i: Cardinal;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGAOKU_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAOKU_ID, AURL);
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
    if parse.Count = 0 then
      Exit;

    for i := 0 to parse.Count - 1 do
    begin
      //get chapters
      if (Pos('<select', parse[i]) > 0) and (Pos('name="chapter', parse[i]) > 0) then
        isExtractChapters := True;
      if isExtractChapters and (Pos('</select', parse[i]) > 0) then
      begin
        isExtractChapters := False;
        Break;
      end;
      if isExtractChapters and (Pos('<option', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));
        s := Trim(TrimChar(s, ['-']));
        mangaInfo.chapterName.Add(s);
        s := TrimChar(GetVal(parse[i], 'value'), ['/']);
        s := AURL + s + '/';
        mangaInfo.chapterLinks.Add(s);
      end;
    end;

    if mangaInfo.chapterName.Count > 1 then
    begin
      // invert chapter
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
