  function GetMangaHostInfoFromURL: Byte;
  var
    i: Integer;
    isExtractGenres: Boolean = False;
    isExtractSummary: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGAHOST_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAHOST_ID, AURL);
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

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;

    mangaInfo.genres := '';
    mangaInfo.summary := '';

    for i := 0 to parse.Count - 1 do
    begin
      //cover
      if (Pos('<img', parse[i]) > 0) and (Pos('class="pull-left thumbnail', parse[i]) > 0) then
        mangaInfo.coverLink := GetVal(parse[i], 'src');

      //title
      if (Pos('<h1' , parse[i]) > 0) and (Pos('class="entry-title', parse[i]) > 0) then
        mangaInfo.title := CommonStringFilter(parse[i + 1]);

      //author(s)
      if Pos('Autor:', parse[i]) > 0 then
        mangaInfo.authors := CommonStringFilter(parse[i + 2]);

      //artist(s)
      if Pos('Desenho (Art):', parse[i]) > 0 then
        mangaInfo.artists := CommonStringFilter(parse[i + 2]);

      //status
      if Pos('Status:', parse[i]) > 0 then
        if (Trim(LowerCase(parse[i + 2])) = 'completo') then
          mangaInfo.status := '0'
        else
          mangaInfo.status := '1';

      //genres
      if Pos('Categoria(s):', parse[i]) > 0 then
        isExtractGenres := True;
      if isExtractGenres and (Pos('</li', parse[i]) > 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('<', parse[i]) = 0) and
        (Pos('Categoria(s):', parse[i]) = 0) then
      begin
        parse[i] := Trim(parse[i]);
        if parse[i] = ',' then
          parse[i] := ', ';
        mangaInfo.genres := mangaInfo.genres + parse[i];
      end;

      //summary
      if (Pos('<div', parse[i]) > 0) and (Pos('id="divSpdInText', parse[i]) > 0) then
        isExtractSummary := True;
      if isExtractSummary and (Pos('</div', parse[i]) > 0) then
        isExtractSummary := False;
      if isExtractSummary then
      begin
        if Trim(parse[i]) = '<p>' then
          mangaInfo.summary := mangaInfo.summary + LineEnding;
        if (Pos('<', parse[i]) = 0) then
          mangaInfo.summary := mangaInfo.summary + Trim(parse[i]);
      end;

      //chapter(s)
      if Pos('class="capitulo"', parse[i]) > 0 then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'),
          WebsiteRoots[MANGAHOST_ID, 1], '', [rfIgnoreCase]));
        mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
      end;
    end;

    //invert chapter(s)
    if mangaInfo.chapterLinks.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterLinks);
      InvertStrings(mangaInfo.chapterName);
    end;
    Result := NO_ERROR;
  end;
