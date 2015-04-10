  function GetUnionMangasInfoFromURL: Byte;
  var
    i: Integer;
  begin
    mangaInfo.website := WebsiteRoots[UNIONMANGAS_ID, 0];
    mangaInfo.url := FillMangaSiteHost(UNIONMANGAS_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Source.Free;
      Exit;
    end;

    CleanHTMLComments(Source);
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
      //title
      if (i + 1 < parse.Count - 1) then
        if (Pos(' - Union Mang', parse[i]) > 0) then
          mangaInfo.title := CommonStringFilter(ReplaceRegExpr('\s-\sUnion\sMang.*$', parse[i], '', False));

      //cover
      if (Pos('<img', parse[i]) > 0) and (Pos('class="img-thumbnail', parse[i]) > 0) then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      //authors
      if (i + 2 < parse.Count - 1) then
        if (Pos('Autor:', parse[i]) > 0) and (Pos('</label', parse[i + 1]) > 0) then
            mangaInfo.authors := CommonStringFilter(parse[i + 2]);

      //artists
      if (i + 2 < parse.Count - 1) then
        if (Pos('Artista:', parse[i]) > 0) and (Pos('</label', parse[i + 1]) > 0) then
            mangaInfo.artists := CommonStringFilter(parse[i + 2]);

      //category/genre
      if (i + 2 < parse.Count - 1) then
        if (Pos('Gênero(s):', parse[i]) > 0) and (Pos('</label', parse[i + 1]) > 0) then
            mangaInfo.genres := CommonStringFilter(parse[i + 2]);

      //status
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="label label-info"', parse[i]) > 0) then
          if Trim(LowerCase(parse[i + 1])) = 'completo' then
            mangaInfo.status := '0'
          else
            mangaInfo.status := '1';

      //sinopsis
      if (i + 1 < parse.Count - 1) then
        if (Pos('class="panel-body"', parse[i]) > 0)then
            mangaInfo.summary := BreaksString(CommonStringFilter(parse[i + 1]));

      //chapters
      if (i + 7 < parse.Count - 1) then
        if Pos('class="row lancamento-linha', parse[i]) > 0 then
        begin
          mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i + 6], 'href'),
            WebsiteRoots[UNIONMANGAS_ID, 1], '', [rfIgnoreCase]));
          mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 7]));
        end;
    end;

    if mangaInfo.chapterName.Count > 1 then
    begin
      // invert chapter
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
