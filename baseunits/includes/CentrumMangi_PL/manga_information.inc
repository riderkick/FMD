  function GetCentrumMangi_PLInfoFromURL: Byte;
  var
    s: String;
    i, j: Cardinal;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[CENTRUMMANGI_PL_ID, 0];
    mangaInfo.url := FillMangaSiteHost(CENTRUMMANGI_PL_ID, AURL);
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

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    //get infos
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        //get cover
        if Pos('class="image"', parse[i]) > 0 then
          if Pos('<img ', parse[i + 2]) > 0 then
            mangaInfo.coverLink := GetVal(parse[i + 2], 'src');

        //get title
        if Pos('Informacje ::', parse[i]) > 0 then
          if Pos('<h2', parse[i - 1]) > 0 then
            mangaInfo.title :=
              Trim(HTMLEntitiesFilter(StringFilter(
              Trim(ReplaceRegExpr('Informacje\s\:\:\s', parse[i], '', False)))));

        //get author
        if Pos('Autor:', parse[i]) > 0 then
          if Pos('</strong', parse[i + 1]) > 0 then
            mangaInfo.authors :=
              Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))));

        //get summary
        if Pos('Recenzja', parse[i]) > 0 then
          if Pos('</h3', parse[i + 1]) > 0 then
            mangaInfo.summary :=
              Trim(BreaksString(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 4])))));

        //get status
        if Pos('Status:', parse[i]) > 0 then
          if Pos('</strong', parse[i + 1]) > 0 then
            if Pos('Zlicencjonowana', parse[i + 4]) > 0 then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';

        //get genres
        if Pos('Gatunki:', parse[i]) > 0 then
          if Pos('</strong', parse[i + 1]) > 0 then
            isExtractGenres := True;
        if isExtractGenres and
          (Pos('</div', parse[i]) > 0) then
          isExtractGenres := False;
        if isExtractGenres then
        begin
          if Pos('<a ', parse[i]) > 0 then
            if mangaInfo.genres = '' then
              mangaInfo.genres :=
                Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))))
            else
              mangaInfo.genres :=
                Trim(mangaInfo.genres + ', ' +
                Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        end;

        //get chapters
        if Pos('class="chapter"', parse[i]) > 0 then
          isExtractChapter := True;
        if Pos('</tbody', parse[i]) > 0 then
          isExtractChapter := False;
        if isExtractChapter then
        begin
          if Pos('class="c"', parse[i]) > 0 then
          begin
            Inc(mangaInfo.numChapter);
            s := StringReplace(GetVal(parse[i + 1], 'href'),
              WebsiteRoots[CENTRUMMANGI_PL_ID, 1], '', [rfIgnoreCase]);
            mangaInfo.chapterLinks.Add(s);
            s := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))));
            mangaInfo.chapterName.Add(s);
          end;
          if (Pos('class="t"', parse[i]) > 0) and
            (mangaInfo.chapterName.Count > 0) then
          begin
            s := mangaInfo.chapterName[mangaInfo.chapterName.Count - 1] + ' ';
            mangaInfo.chapterName[mangaInfo.chapterName.Count - 1] :=
              Trim(s + (Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))))));
          end;
        end;
      end;
    end;

    // invert chapters
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
