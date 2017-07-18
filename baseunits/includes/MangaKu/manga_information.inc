  function GetMangaKuInfoFromURL: Byte;
  var
    s: String;
    i: Integer;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGAKU_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAKU_ID, AURL);
    if mangaInfo.url <> '' then
      if mangaInfo.url[Length(mangaInfo.url)] <> '/' then
        mangaInfo.url := mangaInfo.url + '/';
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    s := FixCommonBrokenHTML(Source.Text);
    Source.Free;

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(s);
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
        //title
        if mangaInfo.title = '' then
          if GetTagName(parse[i]) = 'title' then
          begin
            s := Trim(parse[i + 1]);
            if s <> '' then
            begin
              s := StringReplace(s, '&#8211;', '-', [rfReplaceAll]);
              s := StringReplace(s, 'Baca', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Online Komik', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Komik -', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Komik', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Manga -', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Manga', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Online -', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Terbaru', '', [rfIgnoreCase]);
              s := StringReplace(s, 'Bahasa Indonesia', '', [rfIgnoreCase]);
              s := TrimLeftChar(Trim(s), ['-', ':', ' ']);
              mangaInfo.title := CommonStringFilter(s);
            end;
          end;

        //cover
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'imageanchor') = '1') then
          if GetTagName(parse[i + 1]) = 'img' then
            mangaInfo.coverLink := GetVal(parse[i + 1], 'src');

        //authors/artists/genre/status
        if GetTagName(parse[i]) = 'b' then
        begin
          if Pos('Author(s)', parse[i + 1]) > 0 then
            mangaInfo.authors := CommonStringFilter(TrimLeftChar(parse[i + 3], [':']));
          if Pos('Artist(s)', parse[i + 1]) > 0then
            mangaInfo.artists := CommonStringFilter(TrimLeftChar(parse[i + 3], [':']));
          if Pos('Genre', parse[i + 1]) > 0 then
            mangaInfo.genres := CommonStringFilter(TrimLeftChar(parse[i + 3], [':']));
          if Pos('Episodes', parse[i + 1]) > 0 then
          begin
            s := Trim(TrimLeftChar(parse[i + 3], [':']));
            if LowerCase(s) = 'ongoing' then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';
          end;
        end;

        //chapters
        if (GetTagName(parse[i]) = 'div') and (Pos('-moz-border-radius', parse[i]) > 0) then
          isExtractChapter := True;
        if isExtractChapter then
          if GetTagName(parse[i]) = '/div' then
            isExtractChapter := False
          else
          if GetTagName(parse[i]) = 'a' then
          begin
            Inc(mangaInfo.numChapter);
            mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
            mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
          end;
      end;
    end;

    //invert chapters
    if mangaInfo.chapterLinks.Count > 0 then
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
    Result := NO_ERROR;
  end;
