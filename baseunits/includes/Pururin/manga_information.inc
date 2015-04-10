  function GetPururinInfoFromURL: Byte;
  var
    s, readURL: String;
    i, j: Integer;
    isExtractArtists: Boolean = False;
    isExtractParodies: Boolean = False;
    isExtractGenres: Boolean = False;
    isExtractCharacters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[PURURIN_ID, 0];
    mangaInfo.url := FillMangaSiteHost(PURURIN_ID, URL);
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

    mangaInfo.status := '0';
    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    readURL := '';

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    //for i:=0 to parse.Count-1 do
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (mangaInfo.coverLink = '') and (Pos('class="gallery-cover', parse[i]) > 0) then
        mangaInfo.coverLink := CorrectURL(WebsiteRoots[PURURIN_ID, 1] +
          GetVal(parse[i + 4], 'src'));

      // get title
      //if (Pos('class="otitle"', parse[i])<>0) AND (mangaInfo.title = '') then
      if Pos('<title>', parse[i]) > 0 then
      begin
        mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))));
        if Pos('- Pururin', mangaInfo.title) <> 0 then
          mangaInfo.title := ReplaceRegExpr(
            '^(.*)\s\-.*\s-\sPururin,\sFree\sOnline\sHentai\sManga.*$',
            mangaInfo.title, '$1', True);
      end;

      // get summary
      if (Pos('class="gallery-description', parse[i]) <> 0) then
      begin
        j := i + 1;
        while (j < parse.Count) and (Pos('</div>', parse[j]) = 0) do
        begin
          s := parse[j];
          if s[1] <> '<' then
          begin
            parse[j] := HTMLEntitiesFilter(
              StringFilter(TrimLeft(parse[j])));
            parse[j] := StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
            parse[j] := StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse[j];
            Break;
          end;
          Inc(j);
        end;
      end;

      // get authors
      if (Pos('Circle', parse[i]) <> 0) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        mangaInfo.authors := parse[i + 6];

      // get category
      if (Pos('Category', parse[i]) <> 0) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + parse[i + 6];

      // get artists
      if (Pos('Artist', parse[i]) <> 0) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        isExtractArtists := True;
      if isExtractArtists and (Pos('</ul', parse[i]) <> 0) then
        isExtractArtists := False;
      if isExtractArtists and (Pos('/browse/', parse[i]) <> 0) then
      begin
        if mangaInfo.artists = '' then
          mangaInfo.artists := Trim(parse[i + 1])
        else
          mangaInfo.artists := mangaInfo.artists + ', ' + Trim(parse[i + 1]);
      end;

      // get parody
      if ((Pos('Parody', parse[i]) <> 0) or
        (Pos('Parodies', parse[i]) <> 0)) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        isExtractParodies := True;
      if isExtractParodies and (Pos('</ul', parse[i]) <> 0) then
        isExtractParodies := False;
      if isExtractParodies and (Pos('/browse/', parse[i]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + Trim(parse[i + 1]) + ', ';

      // get language
      if (Pos('Language', parse[i]) <> 0) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + parse[i + 6] + ', ';

      // get convention
      if (Pos('Convention', parse[i]) <> 0) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + parse[i + 6] + ', ';

      // get character
      if (Pos('Character', parse[i]) <> 0) and
        (Pos('</td>', parse[i + 1]) <> 0) then
        isExtractCharacters := True;
      if isExtractCharacters and (Pos('</ul', parse[i]) <> 0) then
        isExtractCharacters := False;
      if isExtractCharacters and (Pos('/browse/', parse[i]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + Trim(parse[i + 1]) + ', ';

      // get genres
      if (Pos('Contents', parse[i]) <> 0) then
        isExtractGenres := True;
      if isExtractGenres and (Pos('</ul', parse[i]) <> 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('/browse/', parse[i]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + Trim(parse[i + 1]) + ', ';

      // get chapters
      if (Pos('<a', parse[i]) <> 0) and (Pos('class="btn link-next', parse[i]) <> 0) then
        readURL := GetVal(parse[i], 'href');
    end;
    mangaInfo.artists := StringReplace(mangaInfo.artists, '</tr>, ', '', []);
    mangaInfo.genres := StringReplace(mangaInfo.genres, '</tr>, ', '', []);
    //only 1 chapter
    if readURL <> '' then
    begin
      mangaInfo.numChapter := 1;
      if not ExecRegExpr('^https?\://', readURL) then
        readURL := WebsiteRoots[PURURIN_ID, 1] + readURL;
      mangaInfo.chapterName.Add(mangaInfo.title);
      mangaInfo.chapterLinks.Add(readURL);
    end;
    Result := NO_ERROR;
  end;
