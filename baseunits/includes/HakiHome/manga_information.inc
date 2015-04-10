  function GetHakiHomeInfoFromURL: Byte;
  var
    i: Integer;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[HAKIHOME_ID, 0];
    mangaInfo.url := FillMangaSiteHost(HAKIHOME_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    //replace ' with "
    Source.Text := FixHTMLTagQuote(Source.Text);

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

    mangaInfo.coverLink := '';
    mangaInfo.title := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.status := '0';

    for i := 0 to parse.Count - 1 do
    begin
      //cover
        if (Pos('<img', parse[i]) > 0) and (Pos('style="display', parse[i]) > 0) then
          mangaInfo.coverLink := GetVal(parse[i], 'src');

      //title
      if (i + 2 < parse.Count - 1) then
        if (Pos('class="tuade"', parse[i]) > 0) then
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))));

      //artist
      if (i + 1 < parse.Count - 1) then
        if (Pos('<a ', parse[i]) > 0) and (Pos('/art/', parse[i]) > 0) then
          if Pos('<', parse[i + 1]) = 0 then
            if mangaInfo.artists = '' then
              mangaInfo.artists := Trim(parse[i + 1])
            else
              mangaInfo.artists := mangaInfo.artists + ', ' + Trim(parse[i + 1]);

      //genres/tag
      if (i + 1 < parse.Count - 1) then
        if (Pos('/tag/', parse[i]) > 0) and
          (Pos('<a ', parse[i]) > 0) and (Pos('title=', parse[i]) > 0) then
          if Pos('<', parse[i + 1]) = 0 then
            if mangaInfo.genres = '' then
              mangaInfo.genres := Trim(parse[i + 1])
            else
              mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);
      //genres/series/language
      if (i + 2 < parse.Count - 1) then
        if ((Pos('Series:', parse[i]) > 0) or ((Pos('Language:', parse[i]) > 0))) and
          (Pos('<a ', parse[i + 1]) > 0) then
          if mangaInfo.genres = '' then
            mangaInfo.genres := Trim(parse[i + 2])
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 2]);

      //chapters
      if (Pos('<table', parse[i]) > 0) and (Pos('class="listing"', parse[i]) > 0) then
        isExtractChapters := True;
      if isExtractChapters and (Pos('</table', parse[i]) > 0) then
        isExtractChapters := False;
      if (i + 1 < parse.Count - 1) then
        if isExtractChapters and (Pos('class="readchap"', parse[i]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(Trim(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[HAKIHOME_ID, 1], '', [rfIgnoreCase])));
          mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        end;
    end;

    // invert chapters
    if mangaInfo.chapterName.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;

    if mangaInfo.chapterName.Count = 1 then
      mangaInfo.chapterName[0] := mangaInfo.title
    else
    if mangaInfo.chapterName.Count > 1 then
      for i := 0 to mangaInfo.chapterName.Count - 1 do
        if Pos(LowerCase(mangaInfo.title), LowerCase(mangaInfo.chapterName[i])) = 0 then
          mangaInfo.chapterName[i] := mangaInfo.title + ' ' + mangaInfo.chapterName[i];

    Result := NO_ERROR;
  end;
