  function GetExtremeMangasInfoFromURL: Byte;
  var
    i: Integer;
    isExtractChapters: Boolean = False;
    regxp: TRegExpr;
  begin
    mangaInfo.website := WebsiteRoots[EXTREMEMANGAS_ID, 0];
    mangaInfo.url := FillMangaSiteHost(EXTREMEMANGAS_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
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
    mangaInfo.summary := '';
    mangaInfo.status := '1';

    regxp := TRegExpr.Create;
    regxp.ModifierI := True;
    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if i + 1 < parse.Count - 1 then
        if (Pos('imageanchor="1"', parse[i]) > 0) and (Pos('<img', parse[i + 1]) > 0) then
          mangaInfo.coverLink := GetVal(parse[i + 1], 'src');

      //title
      if i + 3 < parse.Count - 1 then
        if Pos('class="post-title entry-title"', parse[i]) > 0 then
        begin
          mangaInfo.title := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 3]))));
          regxp.Expression := '\smang..?s$'; mangaInfo.title := Trim(regxp.Replace(mangaInfo.title, '', False));
        end;

      //sinopsis
      if i + 3 < parse.Count - 1 then
        if (Pos('<img border="0"', parse[i]) > 0) and (Pos('<', parse[i + 3]) = 0) then
          mangaInfo.summary := Trim(BreaksString(HTMLEntitiesFilter(StringFilter(
            Trim(parse[i + 3])))));

      //chapters
      if Pos('class="post-body entry-content"', parse[i]) > 0 then
        isExtractChapters := True;
      if isExtractChapters and (Pos('class="post-footer"', parse[i]) > 0) then
        isExtractChapters := False;
      if (i + 1 < parse.Count - 1) then
        if isExtractChapters and (Pos('<a', parse[i]) > 0) then
        begin
          regxp.Expression := '-\d+\.html?';
          if (regxp.Exec(parse[i])) then
          begin
            Inc(mangaInfo.numChapter);
            s := Trim(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[EXTREMEMANGAS_ID, 1], '', [rfIgnoreCase]));
            regxp.Expression := '^https?://.*\.com/'; s := Trim(regxp.Replace(s, '', False));
            mangaInfo.chapterLinks.Add(s);
            mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
          end;
        end;
    end;
    regxp.Free;
    Result := NO_ERROR;
  end;
