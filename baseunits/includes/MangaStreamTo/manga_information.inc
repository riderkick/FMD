  function GetMangaStreamToInfoFromURL: Byte;
  var
    isExtractChapter: Boolean = False;
    i: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(MANGASTREAMTO_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGASTREAMTO_ID, 0];

    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.status := '';

    if parse.Count = 0 then
      Exit;

    // mangastream.to only has chapter list

    // get current_topic
    for i := 0 to parse.Count - 1 do
    begin
      //title
      if Pos('<h1>', parse[i]) > 0 then
        if Pos('</h1>', parse[i + 2]) > 0 then
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))));

      //names and links
      if (Pos('<td ', parse[i]) > 0) and (Pos('class="ch-subject"', parse[i]) > 0) then
        isExtractChapter := True;
      if isExtractChapter and (Pos('</td', parse[i]) > 0) then
        isExtractChapter := False;
      if isExtractChapter and (Pos('<a ', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(Trim(GetVal(parse[i], 'href')));
        mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
      end;
    end;
    Result := NO_ERROR;
  end;
