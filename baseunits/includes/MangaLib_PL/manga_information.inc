  function GetMangaLib_PLInfoFromURL: Byte;
  var
    s: String;
    i, j: Cardinal;
    cf: Boolean = False;
    isExtractGenres: Boolean = False;
    isExtractChapter: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGALIB_PL_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGALIB_PL_ID, AURL);
    s := mangaInfo.url;

    if not GetPage(TObject(Source), s, AReconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    //check confirm
    if Source.Count > 0 then
    begin
      for i := 0 to Source.Count - 1 do
      begin
        if (Pos('<form ', Source[i]) > 0) and
          (Pos('name="confirm_', Source[i]) > 0) then
        begin
          if (Length(AURL) > 1) and
            (AURL[1] = '/') then
            s := Copy(AURL, 2, Length(AURL) - 1)
          else
            s := AURL;
          s := WebsiteRoots[MANGALIB_PL_ID, 1] + '/page/' +
            GetVal(Source[i], 'name') + '?backlink=' + s;
          cf := True;
          Break;
        end;
      end;
      if cf then
      begin
        MANGALIB_PL_COOKIES := FHTTP.Cookies.Text;
        Source.Clear;
        if not GetPage(TObject(Source), s, AReconnect) then
        begin
          Result := NET_PROBLEM;
          Source.Free;
          Exit;
        end;
      end;
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
        if Pos('itemprop="image"', parse[i]) > 0 then
          mangaInfo.coverLink := GetVal(parse[i], 'src');

        //get title
        if (Pos('<strong', parse[i]) > 0) and
          (Pos('itemprop="name"', parse[i]) > 0) then
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))));

        //get author
        if (Pos('<td', parse[i]) > 0) and
          (Pos('itemprop="author"', parse[i]) > 0) then
          mangaInfo.authors :=
            Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 4]))));

        //get artist
        if (Pos('<td', parse[i]) > 0) and
          (Pos('itemprop="illustrator"', parse[i]) > 0) then
          mangaInfo.artists :=
            Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 4]))));

        //get summary
        if Pos('itemprop="description"', parse[i]) > 0 then
          mangaInfo.summary :=
            Trim(BreaksString(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));

        //get status
        if Pos('Status:', parse[i]) > 0 then
          if Pos('</th', parse[i + 1]) > 0 then
            if Pos('trwa', parse[i + 4]) > 0 then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';

        //get genres
        if Pos('Kategorie:', parse[i]) > 0 then
          if Pos('</th', parse[i + 1]) > 0 then
            isExtractGenres := True;
        if isExtractGenres and
          (Pos('</tr', parse[i]) > 0) then
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
        if (Pos('<table ', parse[i]) > 0) and
          (Pos('class="chapters_list"', parse[i]) > 0) then
          isExtractChapter := True;
        if Pos('</tbody', parse[i]) > 0 then
          isExtractChapter := False;
        if isExtractChapter then
        begin
          if (Pos('<a ', parse[i]) > 0) and
            (Pos('/manga/online/', parse[i]) > 0) then
            if Pos('<tr', parse[i - 3]) > 0 then
            begin
              Inc(mangaInfo.numChapter);
              s := StringReplace(GetVal(parse[i], 'href'),
                WebsiteRoots[MANGALIB_PL_ID, 1], '', [rfIgnoreCase]);
              mangaInfo.chapterLinks.Add(EncodeURL(s));
              s := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1]))));
              mangaInfo.chapterName.Add(s);
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
