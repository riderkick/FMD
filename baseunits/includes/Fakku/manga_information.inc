  function GetFakkuInfoFromURL: Byte;
  var
    cht: String;
    s: String;
    isExtractChapters: Boolean = True;
    isExtractSummary: Boolean = False;
    i: Cardinal;
  begin
    mangaInfo.url := FillMangaSiteHost(FAKKU_ID, URL);
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
    mangaInfo.website := WebsiteRoots[FAKKU_ID, 0];
    if parse.Count = 0 then
      Exit;

    mangaInfo.status := '0';
    mangaInfo.genres := '';
    // using parser (cover link, summary, chapter name and link)
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if Pos('itemprop="name"', parse[i]) > 0 then
        mangaInfo.title := Trim(parse[i + 1]);

      // get cover
      if (Pos('<img ', parse[i]) > 0) and (Pos('class="cover"', parse[i]) > 0) then
      begin
        s := GetVal(parse[i], 'src');
        if ExecRegExpr('^//', s) then
          mangaInfo.coverLink := 'https:' + s
        else
          mangaInfo.coverLink := s
      end;
      //if GetTagName(parse.Strings[i]) = 'img' then
      //  if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class=')) = 'cover') then
      //  begin
      //    mangaInfo.coverLink :=
      //      CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
      //    //http seems working | not anymore?
      //    mangaInfo.coverLink :=
      //      StringReplace(mangaInfo.coverLink, 'https://', 'http://', [rfIgnoreCase]);
      //  end;

      // get manga chapter title, fakku only has 1 chapter
      if (Pos('<title>', parse.Strings[i]) > 0) then
        cht := Trim(parse.Strings[i + 1]);

      //get chapter name and links, fakku only has 1 chapter
      if isExtractChapters then
      begin
        if (Pos('<a ', parse[i]) > 0) and
          (Pos('class="button green"', parse[i]) > 0) then
        begin
          isExtractChapters := False;
          mangaInfo.numChapter := 1;
          mangaInfo.chapterName.Add(cht);
          mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'),
            WebsiteRoots[FAKKU_ID, 1], '', [rfIgnoreCase]));
        end;
        //if (Pos('/read#', parse.Strings[i])>0) then
        //begin
        //  Inc(mangaInfo.numChapter);
        //  s := GetVal(parse[i], 'href');
        //  s := StringReplace(s, WebsiteRoots[FAKKU_ID,1], '', []);
        //  //if Pos('#page=1', s)>0 then
        //    //s:=StringReplace(s, '#page=1', '', []);
        //  mangaInfo.chapterLinks.Add(s);
        //  mangaInfo.chapterName.Add(cht);
        //  isExtractChapters:= False;
        //end;
      end;

      // get series/authors
      if (Pos('div class="left"', parse.Strings[i]) > 0) and
        (Pos('Series', parse.Strings[i + 1]) > 0) then
      begin
        mangaInfo.authors := parse.Strings[i + 6];
        mangaInfo.genres := mangaInfo.authors;
      end;

      // get artists
      if (Pos('div class="left"', parse.Strings[i]) > 0) and
        (Pos('Artist', parse.Strings[i + 1]) > 0) then
      begin
        mangaInfo.artists := parse.Strings[i + 6];
        mangaInfo.genres := mangaInfo.genres + ', ' + mangaInfo.artists;
      end;

      // get language
      if (Pos('div class="left"', parse.Strings[i]) > 0) and
        (Pos('Language', parse.Strings[i + 1]) > 0) then
        mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse.Strings[i + 7]);

      // get descriptions
      if (Pos('div class="left"', parse.Strings[i]) > 0) and
        (Pos('Description', parse.Strings[i + 1]) > 0) then
        isExtractSummary := True;

      if isExtractSummary then
      begin
        if (Pos('</div>', parse.Strings[i + 5]) > 0) then
          isExtractSummary := False
        else if (not (Pos('<', parse.Strings[i + 5]) > 0)) then
          mangaInfo.summary := Trim(mangaInfo.summary) + ' ' + Trim(parse.Strings[i + 5]);
      end;

      // get genres
      if (Pos('"/tags/', parse.Strings[i]) > 0) then
      begin
        mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse.Strings[i + 1]);
      end;
    end;

    //last correction
    mangaInfo.summary := BreaksString(StringFilter(mangaInfo.summary));
    //mangaInfo.title := 'Cholif '+IntToStr(Random(1000));
    Result := NO_ERROR;
  end;
