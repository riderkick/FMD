  function GetPornComixInfoFromURL(MangaID: Cardinal): Byte;
  var
    i: LongInt;
    isExtractCover: Boolean = False;
    isExtractGenres: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MangaID, 0];
    mangaInfo.url := FillMangaSiteHost(MangaID, AURL);
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

    // using parser
    if parse.Count = 0 then
      Exit;

    mangaInfo.coverLink := '';

    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if Pos('class="single-post', parse[i]) <> 0 then
        isExtractCover := True;
      if isExtractCover and (Pos('<img', parse[i]) <> 0) and
        (Pos('alt="download comic"', parse[i]) = 0) then
      begin
        mangaInfo.coverLink := GetVal(parse[i], 'src');
        isExtractCover := False;
      end;

      //title
      if (mangaInfo.title = '') and (Pos('class="posts', parse[i]) <> 0) then
        if (Pos('<h2' , parse[i + 2]) <> 0) and
          (Pos('class="post-title', parse[i + 2]) <> 0) then
          mangaInfo.title := CommonStringFilter(parse[i + 3]);

      //genres/tags
      if (Pos('class="video-tags', parse[i]) <> 0) then
        isExtractGenres := True;
      if isExtractGenres and (Pos('</div', parse[i]) <> 0) then
        isExtractGenres := False;
      if isExtractGenres and
        (Pos('<a', parse[i]) <> 0) and (Pos('/tag/', parse[i]) <> 0) then
      begin
        if mangaInfo.genres = '' then
          mangaInfo.genres := Trim(parse[i + 1])
        else
          mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1])
      end;
    end;
    if mangaInfo.title = '' then
      mangaInfo.title := 'Untitled_' + IntToStr(Random(99999));
    mangaInfo.status := '0';

    //single chapter
    mangaInfo.chapterLinks.Add(AURL);
    mangaInfo.chapterName.Add(mangaInfo.title);

    Result := NO_ERROR;
  end;
