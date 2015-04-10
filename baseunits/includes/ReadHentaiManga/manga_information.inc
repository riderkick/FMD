  function GetReadHentaiMangaInfoFromURL: Byte;
  var
    s: String;
    i, j: Cardinal;
  begin
    mangaInfo.website := WebsiteRoots[READHENTAIMANGA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(READHENTAIMANGA_ID, URL);
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


    if parse.Count = 0 then
      Exit;

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    mangaInfo.summary := '';
    for i := 0 to parse.Count - 1 do
    begin
      // get cover
      if (Pos('class="cvr"', parse[i]) > 0) and (Pos('<img', parse[i]) > 0) then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      // get title
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="ttl"', parse[i]) > 0) and (Pos('<h1 ', parse[i]) > 0) then
          mangaInfo.title := Trim(StringFilter(parse[i + 1]));

      //get authors
      if (i + 1 < parse.Count - 1) then
        if (Pos('Author', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          j := i + 1;
          while j < parse.Count do
          begin
            if (Pos('</p', parse[j]) > 0) then
              Break;
            if (Pos('<', parse[j]) = 0) and (Trim(parse[j]) <> ':') then
              mangaInfo.authors := mangaInfo.authors + parse[j];
            Inc(j);
          end;
        end;

      //get artists
      if (i + 1 < parse.Count - 1) then
        if (Pos('Artist', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          j := i + 1;
          while j < parse.Count do
          begin
            if (Pos('</p', parse[j]) > 0) then
              Break;
            if (Pos('<', parse[j]) = 0) and (Trim(parse[j]) <> ':') then
              mangaInfo.artists := mangaInfo.artists + parse[j];
            Inc(j);
          end;
        end;

      //get genres
      if (i + 1 < parse.Count - 1) then
        if (Pos('Category', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          j := i + 1;
          s := '';
          while j < parse.Count do
          begin
            if (Pos('</p', parse[j]) > 0) then
              Break;
            if (Pos('<', parse[j]) = 0) and (Trim(parse[j]) <> ':') then
              s := s + parse[j];
            Inc(j);
          end;
          if (mangaInfo.genres <> '') and (s <> '') then
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(s)
          else
          if s <> '' then
            mangaInfo.genres := Trim(s);
        end;
      if (i + 1 < parse.Count - 1) then
        if (Pos('Tagged as', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          j := i + 1;
          s := '';
          while j < parse.Count do
          begin
            if (Pos('</p', parse[j]) > 0) then
              Break;
            if (Pos('<', parse[j]) = 0) and (Trim(parse[j]) <> ':') then
              s := s + parse[j];
            Inc(j);
          end;
          if (mangaInfo.genres <> '') and (s <> '') then
            mangaInfo.genres := mangaInfo.genres + ', ' + Trim(s)
          else
          if s <> '' then
            mangaInfo.genres := Trim(s);
        end;

      //get status
      if (i + 4 < parse.Count - 1) then
        if (Pos('Status', parse[i]) > 0) and (Pos('</b', parse[i + 1]) > 0) then
        begin
          if Pos('Completed', parse[i + 4]) > 0 then
            mangaInfo.status := '0'
          else
            mangaInfo.status := '1';
        end;

      //get chapters
      if (Pos('<a ', parse[i]) > 0) and (Pos('class="lst"', parse[i]) > 0) then
      begin
        mangaInfo.chapterName.Add(Trim(GetVal(parse[i], 'title')));
        s := GetVal(parse[i], 'href');
        s := StringReplace(s, WebsiteRoots[READHENTAIMANGA_ID, 1], '', [rfIgnoreCase]);
        mangaInfo.chapterLinks.Add(s);
      end;
    end;

    if mangaInfo.chapterName.Count = 1 then
      mangaInfo.chapterName[0] := mangaInfo.title
    else if mangaInfo.chapterName.Count > 1 then
    begin
      // invert chapter
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
