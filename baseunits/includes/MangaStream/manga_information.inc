  function GetMangaStreamInfoFromURL: Byte;
  var
    i: Integer;
    s: String;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGASTREAM_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGASTREAM_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source

    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count = 0 then
      Exit;

    mangaInfo.status := '1';
    for i := 0 to parse.Count - 1 do
    begin
      //title
      if mangaInfo.title = '' then
        if GetTagName(parse[i]) = 'h1' then
          mangaInfo.title := CommonStringFilter(parse[i + 1]);

      //chapters
      if (GetTagName(parse[i]) = 'table') and (GetVal(parse[i], 'class') = 'table table-striped') then
        isExtractChapter := True;
      if isExtractChapter then
      begin
        if  GetTagName(parse[i]) = '/table' then
          Break
        else
        if GetTagName(parse[i]) = 'a' then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterName.Add(parse[i + 1]);
          s := GetVal(parse[i], 'href');
          if Pos('http', LowerCase(s)) <> 1 then
            s := MANGASTREAM_ROOT2 + s;
          mangaInfo.chapterLinks.Add(s);
        end;
      end;
    end;

    // invert chapters
    InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
    Result := NO_ERROR;
  end;
