  function GetStarkanaInfoFromURL: Byte;
  var
    i: Integer;
    isExtractGenres: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[STARKANA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(STARKANA_ID, AURL) + '?mature_confirm=1';
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

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;

    mangaInfo.genres := '';

    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if (Pos('<a', parse[i]) > 0) and (Pos('class="olol', parse[i]) > 0) then
        if (Pos('<img', parse[i + 1]) > 0) then
          mangaInfo.coverLink := GetVal(parse[i + 1], 'src');

      //title
      if Pos('Title(s):', parse[i]) > 0 then
        mangaInfo.title := CommonStringFilter(parse[i + 5]);

      //author(s)
      if Pos('Author(s):', parse[i]) > 0 then
        mangaInfo.authors := CommonStringFilter(parse[i + 7]);

      //artist(s)
      if Pos('Artist(s):', parse[i]) > 0 then
        mangaInfo.artists := CommonStringFilter(parse[i + 7]);

      //status
      if Pos('Status:', parse[i]) > 0 then
        if (Trim(LowerCase(parse[i + 7])) = 'ongoing') then
          mangaInfo.status := '1'
        else
          mangaInfo.status := '0';

      //summary
      if Pos('Summary:', parse[i]) > 0 then
        if Pos('valign="top"', parse[i - 2]) > 0 then
          mangaInfo.summary := BreaksString(CommonStringFilter(parse[i + 7]));

      //genres
      if Pos('Genres:', parse[i]) > 0 then
        isExtractGenres := True;
      if isExtractGenres and (Pos('</tr', parse[i]) > 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('<', parse[i]) = 0) and
        (Pos('Genres:', parse[i]) = 0) then
      begin
        parse[i] := Trim(parse[i]);
        if parse[i] = ',' then
          parse[i] := ', ';
        mangaInfo.genres := mangaInfo.genres + parse[i];
      end;

      //chapter(s)
      if Pos('class="download-link', parse[i]) > 0 then
      begin
        Inc(mangaInfo.numChapter);     // 1, 3, 7
        mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[STARKANA_ID, 1], '', [rfIgnoreCase]));
        mangaInfo.chapterName.Add(CommonStringFilter(
          Trim(parse[i + 1]) + ' ' + Trim(parse[i + 3]) + ' ' + Trim(parse[i + 7])));
      end;
    end;

    //invert chapter(s)
    if mangaInfo.chapterLinks.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterLinks);
      InvertStrings(mangaInfo.chapterName);
    end;
    Result := NO_ERROR;
  end;
