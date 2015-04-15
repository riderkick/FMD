  function GetSubMangaInfoFromURL: Byte;
  var
    s: String;
    i, j: Integer;
    isGetChapters: Boolean = False;
  begin
    mangaInfo.url := FillMangaSiteHost(SUBMANGA_ID, URL);
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
    mangaInfo.website := WebsiteRoots[SUBMANGA_ID, 0];

    mangaInfo.genres := '';
    mangaInfo.status := '1';

    // using 1st parser (cover link, summary)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get manga title
      if (mangaInfo.title = '') and
        (Pos('content="Leer ', parse.Strings[i]) > 0) then
        mangaInfo.title := GetString(parse.Strings[i], 'content="Leer ', ' manga online');

      if Pos('class="suscripcion', parse.Strings[i]) > 0 then
      begin
        // get cover link
        if Pos('<img', parse.Strings[i + 5]) > 0 then
          mangaInfo.coverLink :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i + 5], 'src'));
        // get summary
        j := i + 8;
        while (j < parse.Count - 1) and (Pos('</p>', parse.Strings[j]) = 0) do
        begin
          Inc(j);
          s := parse.Strings[j];
          if (s <> '') and
            (s[1] <> '<') then
          begin
            parse.Strings[j] := StringFilter(parse.Strings[j]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + parse.Strings[j];
          end;
        end;
      end;

      // get authors/artists
      if (Pos('Creado por', parse.Strings[i]) <> 0) then
      begin
        if Pos('/autor/', parse.Strings[i + 1]) > 0 then
          mangaInfo.authors := parse.Strings[i + 2];
        if Pos('/mangaka/', parse.Strings[i + 5]) > 0 then
          mangaInfo.authors := parse.Strings[i + 6];
      end;

      // get genres
      if (Pos('submanga.com/genero/', parse.Strings[i]) <> 0) then
        mangaInfo.genres := mangaInfo.genres + parse.Strings[i + 1] + ', ';
    end;

    // get chapters
    Source.Clear;
    if not GetPage(TObject(Source), mangaInfo.url + '/completa', Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        // get chapters
        if (GetTagName(parse[i]) = 'table') and
          (GetVal(parse[i], 'class') = 'table table-striped table-hover') then
          isGetChapters := True;
        if isGetChapters and (GetTagName(parse[i]) = '/table') then
          Break;
        if isGetChapters and
          ((GetTagName(parse[i]) = 'a') and (GetTagName(parse[i - 1]) = 'td') and
          (GetTagName(parse[i - 2]) = 'tr')) then
            if Length(GetVal(parse[i], 'href')) > 3 then
            begin
              Inc(mangaInfo.numChapter);
              mangaInfo.chapterName.Add(CommonStringFilter(
                Trim(parse[i + 1]) + ' ' + Trim(parse[i + 3])));
              mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
            end;
      end;
    end;
    Source.Free;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterLinks.Count > 1 then
    begin
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
      // replace urls
      with TRegExpr.Create do
        try
          Expression := '^.*(/\d+)$';
          for i := 0 to mangaInfo.chapterLinks.Count - 1 do
            mangaInfo.chapterLinks[i] := Replace(mangaInfo.chapterLinks[i], '/c/$1', True);
        finally
          Free;
        end;
      Result := NO_ERROR;
    end;
  end;
