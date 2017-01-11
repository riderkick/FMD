  function GetNHentaiInfoFromURL: Byte;
  var
    i: Integer;
  begin
    mangaInfo.website := WebsiteRoots[NHENTAI_ID, 0];
    mangaInfo.url := FillMangaSiteHost(NHENTAI_ID, AURL);
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

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if (Pos('/cover', parse[i]) > 0) and (Pos('<img', parse[i]) > 0) then
      begin
        mangaInfo.coverLink := GetVal(parse[i], 'src');
        if (mangaInfo.coverLink <> '') and (Pos('http', mangainfo.coverLink) = 0) then
        begin
          mangaInfo.coverLink := TrimLeftChar(mangaInfo.coverLink, ['/', ':']);
          mangaInfo.coverLink := 'http://' + mangaInfo.coverLink;
        end;
      end;

      //title
      if (i + 1 < parse.Count - 1) then
        if (Pos('<h1', parse[i]) > 0) then
          mangaInfo.title := Trim(StringFilter(parse[i + 1]));

      //artist
      if (i + 1 < parse.Count - 1) then
        if ((Pos('<a', parse[i]) > 0) and (Pos('class="tagbutton"', parse[i]) > 0)) and
          (Pos('/artist/', parse[i]) > 0) then
          if mangaInfo.artists = '' then
            mangaInfo.artists := Trim(parse[i + 1])
          else
            mangaInfo.artists := mangaInfo.artists + ', ' + Trim(parse[i + 1]);

      //tag/genre
      if (i + 1 < parse.Count - 1) then
        if ((Pos('<a', parse[i]) > 0) and (Pos('class="tagbutton"', parse[i]) > 0)) and
          ((Pos('/parody/', parse[i]) > 0) or
          (Pos('/character/', parse[i]) > 0) or
          (Pos('/tagged/', parse[i]) > 0) or
          (Pos('/group/', parse[i]) > 0)) then
          if mangaInfo.genres = '' then
            mangaInfo.genres := Trim(parse[i + 1])
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[i + 1]);

      //language/genre
      if (i + 1 < parse.Count - 1) then
        if ((Pos('class="field-name"', parse[i]) > 0) and (Pos('Language:', parse[i + 1]) > 0)) then
        begin
          if mangaInfo.genres = '' then
            mangaInfo.genres := Trim(StringReplace(parse[i + 1], 'Language:', '', [rfIgnoreCase]))
          else
            mangaInfo.genres := mangaInfo.genres + ', ' +
              Trim(StringReplace(parse[i + 1], 'Language:', '', [rfIgnoreCase]));;
        end;
    end;

    mangaInfo.status := '0';
    mangaInfo.numChapter := 1;
    mangaInfo.chapterLinks.Add(AURL);
    mangaInfo.chapterName.Add(mangaInfo.title);

    if mangaInfo.chapterName.Count > 1 then
    begin
      // invert chapter
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
