  function GetNineMangaInfoFromURL: Byte;
  var
    isExtractGenres: Boolean = False;
    i, j, SiteID: Cardinal;
    s: String;
  begin
    SiteID := GetMangaSiteID(AWebsite);
    mangaInfo.website := WebsiteRoots[SiteID, 0];
    mangaInfo.url := StringReplace(FillMangaSiteHost(SiteID, AURL), '?waring=1', '',
      [rfIgnoreCase, rfReplaceAll]);

    if not GetPage(TObject(Source), mangaInfo.url + '?waring=1', AReconnect) then
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

    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.numChapter := 0;

    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;
    for i := 0 to parse.Count - 1 do
    begin
      //manga title
      if {(mangaInfo.title = '') and}
        (Pos('itemprop="name"', parse[i]) > 0) then
      begin
        mangaInfo.title := HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])));
        mangaInfo.title := ReplaceRegExpr('\sManga$', mangaInfo.title, '', False);
      end;

      //coverlink
      if Pos('itemprop="image"', parse[i]) > 0 then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      //genres
      if Pos('itemprop="genre"', parse[i]) > 0 then
        isExtractGenres := True;
      if isExtractGenres and (Pos('</li>', parse[i]) > 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('href="', parse[i]) > 0) then
        if mangaInfo.genres = '' then
          mangaInfo.genres := Trim(parse[i + 1])
        else
          mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);

      //author
      if Pos('itemprop="author"', parse[i]) > 0 then
        mangaInfo.authors := Trim(parse[i + 1]);

      //status
      if Pos('class="red" href="/category/', parse[i]) > 0 then
        if Pos('/category/updated', parse[i]) > 0 then
          mangaInfo.status := '1' //ongoing
        else
        if Pos('/category/completed', parse[i]) > 0 then
          mangaInfo.status := '0'; //completed

      //summary
      if Pos('itemprop="description"', parse[i]) > 0 then
        //mangaInfo.summary:= mangaInfo.summary+#13#10+Trim(parse[i+5]);
        mangaInfo.summary := HTMLEntitiesFilter(StringFilter(Trim(parse[i + 5])));

      ////chapter name and links
      //if Pos('class="chapter_list_', parse[i]) > 0 then
      //begin
      //  Inc(mangaInfo.numChapter);
      //  mangaInfo.chapterLinks.Add(
      //    StringReplace(GetVal(parse[i], 'href'),
      //    WebsiteRoots[SiteID, 1], '', [rfIgnoreCase]));
      //  mangaInfo.chapterName.Add(HTMLEntitiesFilter(
      //    StringFilter(Trim(parse[i + 1]))));
      //end;
    end;

    // chapter list direct
    if Source.Count > 1 then
    begin
      for i := 0 to Source.Count - 1 do
      begin
        if (Pos('<a ', Source[i]) > 0) and
          (Pos('class="chapter_list_', Source[i]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          s := StringReplace(GetVal(Source[i], 'href'), WebsiteRoots[SiteID, 1], '', [rfIgnoreCase]);
          mangaInfo.chapterLinks.Add(s);
          s := Trim(ReplaceRegExpr('^.*\>(.*)\<\/a.*$', Source[i], '$1', True));
          mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(s))));
        end;
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
    Source.Free;
  end;
