  function GetSenMangaInfoFromURL: Byte;
  var
    i: LongInt;
    s: String;
    isExtractGenres  : Boolean = False;
    isExtractGenresP : Boolean = False;
    isExtractSummary : Boolean = False;
    isExtractSummaryP: Boolean = False;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.url := FillMangaSiteHost(SENMANGA_ID, URL);
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

    mangaInfo.website := WebsiteRoots[SENMANGA_ID, 0];
    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';

    if parse.Count = 0 then
      Exit;

    for i := 0 to parse.Count - 1 do
    begin
      //title 
      if (i + 7 < parse.Count - 1) then
        if GetVal(parse[i], 'id') = 'contentleft' then
          mangaInfo.title := CommonStringFilter(parse[i + 6]);

      //cover
      if (Pos('<img', parse[i]) <> 0) and
        (Pos('onerror="this.src =''/no_preview.jpg''', parse[i]) <> 0) then
      begin
        mangaInfo.coverLink := GetVal(parse[i], 'src');
        if Pos('/', mangaInfo.coverLink) = 1 then
          mangaInfo.coverLink := WebsiteRoots[SENMANGA_ID, 1] + mangaInfo.coverLink;
      end;

      if (i + 2 < parse.Count - 1) and
        (Pos('<strong', parse[i]) <> 0) and (GetVal(parse[i], 'class') = 'data') then
      begin
        //author
        if (Pos('Author:', parse[i + 1]) <> 0) then
          mangaInfo.authors := CommonStringFilter(parse[i + 5]);

        //artist
        if (Pos('Artist:', parse[i + 1]) <> 0) then
          mangaInfo.artists := CommonStringFilter(parse[i + 5]);

        //status
        if (Pos('Status:', parse[i + 1]) <> 0) then
        begin
          parse[i + 5] := Trim(LowerCase(parse[i + 5]));
          if parse[i + 5] = 'ongoing' then
            mangaInfo.status := '1'
          else
            mangaInfo.status := '0';
        end;

        //summary
        if (Pos('Summary:', parse[i + 1]) <> 0) then
          isExtractSummary := True;

        //genres
        if (Pos('Categorize in:', parse[i + 1]) <> 0) then
          isExtractGenres := True;
      end;

      //summary
      if isExtractSummary and (Pos('<p', parse[i]) <> 0) then
      begin
        isExtractSummary := False;
        isExtractSummaryP := True;
      end;
      if isExtractSummaryP and (Pos('</p', parse[i]) <> 0) then
        isExtractSummaryP := False;
      if isExtractSummaryP and
        (Pos('<', parse[i]) = 0) then
      begin
        if mangaInfo.summary <> '' then
          mangaInfo.summary := mangaInfo.summary + '\r\n' + CommonStringFilter(parse[i])
        else
          mangaInfo.summary := CommonStringFilter(parse[i]);
      end;

      //genres
      if isExtractGenres and (Pos('<p', parse[i]) <> 0) then
      begin
        isExtractGenres := False;
        isExtractGenresP := True;
      end;
      if isExtractGenresP and (Pos('</p', parse[i]) <> 0) then
        isExtractGenresP := False;
      if i + 2 < parse.Count - 1 then 
        if isExtractGenresP and
          (Pos('<a', parse[i]) <> 0) then
        begin
          if mangaInfo.genres <> '' then
            mangaInfo.genres := mangaInfo.genres + ', ' + CommonStringFilter(parse[i + 1])
          else
            mangaInfo.genres := CommonStringFilter(parse[i + 1]);
        end;

      //chapters
      if (Pos('<td', parse[i]) <> 0) and (Pos('class="chapter"', parse[i]) <> 0) then
        isExtractChapters := True;
      if isExtractChapters and (Pos('</table', parse[i]) <> 0) then
        isExtractChapters := False;
      if isExtractChapters and
        (Pos('<a', parse[i]) <> 0) and (Pos(' title="', parse[i]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetVal(parse[i], 'href');
        if (Length(s) > 4) and (RightStr(s, 2) = '/1') then
          SetLength(s, Length(s) - 2);
        mangaInfo.chapterLinks.Add(s);
        mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
      end;
    end;

    //invert
    if mangainfo.ChapterLinks.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterLinks);
      InvertStrings(mangaInfo.chapterName);
    end;
    Result := NO_ERROR;
  end;
