  function GetS2scanInfoFromURL: Byte;
  var
    i: Integer;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[S2SCAN_ID, 0];
    mangaInfo.url := FillMangaSiteHost(S2SCAN_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
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

    //no status = ongoing
    mangaInfo.status := '1';

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        //cover
        if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'class') = 'thumbnail') then
          if GetTagName(parse[i + 2]) = 'img' then
            mangaInfo.coverLink := GetVal(parse[i + 2], 'src');

        //title
        if mangaInfo.title = '' then
          if (GetTagName(parse[i]) = 'h1') and (GetVal(parse[i], 'class') = 'title') then
            mangaInfo.title := parse[i + 1];

        if GetTagName(parse[i]) = 'b' then
        begin
          //author
          if Trim(parse[i + 1]) = 'Author' then
            mangaInfo.authors := TrimLeft(TrimLeftChar(parse[i + 3], [':']));
          //artist
          if Trim(parse[i + 1]) = 'Artist' then
            mangaInfo.artists := TrimLeft(TrimLeftChar(parse[i + 3], [':']));
          //author
          if Trim(parse[i + 1]) = 'Synopsis' then
            mangaInfo.summary := TrimLeft(TrimLeftChar(parse[i + 3], [':']));
        end;

        //chapters
        if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'class') = 'list') then
          isExtractChapter := True;
        if isExtractChapter then
        begin
          if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'class') = 'title') then
            if GetTagName(parse[i + 1]) = 'a' then
            begin
              Inc(mangaInfo.numChapter);
              mangaInfo.chapterName.Add(GetVal(parse[i + 1], 'title'));
              mangaInfo.chapterLinks.Add(GetVal(parse[i + 1], 'href'));
            end;
        end;
      end;

    // invert chapters
    InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
    Result := NO_ERROR;
  end;
