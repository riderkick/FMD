  function GetJapanShinInfoFromURL: Byte;
  var
    i, j: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[JAPANSHIN_ID, 0];
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), FillMangaSiteHost(JAPANSHIN_ID, AURL), AReconnect) then
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
    mangaInfo.numChapter := 0;
    for i := 0 to parse.Count - 1 do
    begin
      //get title
      if (Pos('h1 class="title"', parse[i]) > 0) then
        mangaInfo.title := Trim(parse[i + 1]);

      //get coverlink
      if (Pos('class="thumbnail"', parse[i]) > 0) then
      begin
        j := i + 1;
        while (Pos('<img', parse[j]) = 0) do
          Inc(j);
        mangaInfo.coverLink := GetVal(parse[j], 'src');
      end;

      //get authors
      if (Pos('Auteur :', parse[i]) > 0) then
        mangaInfo.authors := parse[i + 2];

      //get genre
      if (Pos('Genre :', parse[i]) > 0) then
        mangaInfo.genres := parse[i + 2];

      //get summary
      if (Pos('Synopsis :', parse[i]) > 0) then
        mangaInfo.summary := parse[i + 2];

      //get chapter name and links
      if (Pos('href="', parse[i]) > 0) and
        (Pos('title="', parse[i]) > 0) and
        (Pos('/reader/read/', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(
          StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[JAPANSHIN_ID, 1], '', [rfIgnoreCase]));
        mangaInfo.chapterName.Add(Trim(parse[i + 1]));
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
  end;
