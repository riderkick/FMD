  function GetMangaREADER_PORInfoFromURL: Byte;
  var
    s: String;
    isExtractChapter: Boolean = False;
    i, j, n: Cardinal;
    numberOfPage: Cardinal = 1;

    procedure ExtractChapter;
    begin
      if (not isExtractChapter) and (Pos('id="listagemCaps', parse[i]) > 0) then
        isExtractChapter := True;

      if (isExtractChapter) and
        (Pos('paginacao', parse[i]) > 0) then
        isExtractChapter := False; //bermasalah

      // get chapter name and links
      if (isExtractChapter) and
        (Pos('</em>', parse[i]) > 0) and
        (i + 6 < parse.Count - 1) then
      begin
        Inc(mangaInfo.numChapter);
        //s:= StringReplace(GetString(parse[i+6], 'href="', '"'), WebsiteRoots[MANGAREADER_POR_ID,1], '', []);
        //mangaInfo.chapterLinks.Add(s);
        s := GetVal(parse[i + 6], 'href');
        s := StringReplace(s, WebsiteRoots[MANGAREADER_POR_ID, 1], '', []);
        mangaInfo.chapterLinks.Add(s);
        s := RemoveSymbols(TrimLeft(TrimRight(parse[i + 1])));
        mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
      end;
    end;

  begin
    mangaInfo.url := FillMangaSiteHost(MANGAREADER_POR_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    //weird stuff
    if Source.Count > 1 then
      Source.Text := StringReplace(Source.Text, '<3', '❤', [rfReplaceAll, rfIgnoreCase]);

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;

    Parser.Free;
    mangaInfo.website := WebsiteRoots[MANGAREADER_POR_ID, 0];
    mangaInfo.status := '0';
    mangaInfo.coverLink := '';
    mangaInfo.summary := '';
    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.genres := '';
    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;

    isExtractChapter := False;
    for i := 0 to parse.Count - 1 do
    begin
      // Get number of page.
      if Pos('Última Página', parse[i]) > 0 then
        numberOfPage := StrToInt(GetString(parse[i - 1], '/page/', '">'));

      // get cover
      //if (mangaInfo.coverLink='') AND
      //   (Pos('img src="', parse[i])>0) AND
      //   (Pos('class="imgClass"', parse[i])>0) then
      //   mangaInfo.coverLink:=GetVal(parse[i], 'src');

    {if (mangaInfo.coverLink = '') AND
       (Pos('class="cvr', parse[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetVal(parse[i], 'src'));}

      // get title
      if (Pos('Título:', parse[i]) > 0) then
        mangaInfo.title := parse[i + 2];

      ExtractChapter;

      //Not available
      // get summary
      //if (Pos('class="text', parse[i]) <> 0) then
      //begin
      //  j:= i+9;
      //  while (j<parse.Count) AND (Pos('</div>', parse[j])=0) do
      //  begin
      //    s:= parse[j];
      //    if s[1] <> '<' then
      //    begin
      //      parse[j]:= HTMLEntitiesFilter(StringFilter(TrimLeft(parse[j])));
      //      parse[j]:= StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
      //      parse[j]:= StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
      //      mangaInfo.summary:= mangaInfo.summary + parse[j];
      //      break;
      //    end;
      //    Inc(j);
      //  end;
      //  isExtractSummary:= FALSE;
      //end;

      // get authors
      if (Pos('Autor:', parse[i]) > 0) then
        mangaInfo.authors := Trim(StringFilter(parse[i + 2]));

      // get artists
      if (Pos('Artista:', parse[i]) > 0) then
        mangaInfo.artists := Trim(StringFilter(parse[i + 2]));

      // get genres
      if Pos('Categoria:', parse[i]) > 0 then
        mangaInfo.genres := parse[i + 4];
      //if (Pos('class="cat', parse[i])<>0) then
      //begin
      //  isExtractGenres:= TRUE;
      //end;

      //if isExtractGenres then
      //begin
      //  if Pos('', parse[i]) <> 0 then
      //    mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse[i+1])) + ', ';
      //  if Pos('</br>', parse[i]) <> 0 then
      //    isExtractGenres:= FALSE;
      //end;

      // not available
      // get status
      //if (i+2<parse.Count) AND (Pos('Status', parse[i])<>0) then
      //begin
      //  if Pos('Ongoing', parse[i+3])<>0 then
      //    mangaInfo.status:= '1'   // ongoing
      //  else
      //    mangaInfo.status:= '0';  // completed
      //end;
    end;

    // If there're more than 1 page, we need to continue to scrape for chapters ...
    if numberOfPage > 1 then
    begin
      for n := 2 to numberOfPage do
      begin
        Source.Clear;
        s := mangaInfo.url + '/' + IntToStr(n);
        if not GetPage(TObject(Source), mangaInfo.url + '/page/' +
          IntToStr(n), AReconnect) then
        begin
          Result := NET_PROBLEM;
          Source.Free;
          Exit;
        end;
        //weird stuff
        if Source.Count > 1 then
          Source.Text := StringReplace(Source.Text, '<3', '❤', [rfReplaceAll, rfIgnoreCase]);
        // parsing the HTML source
        parse.Clear;
        Parser := THTMLParser.Create(PChar(Source.Text));
        Parser.OnFoundTag := OnTag;
        Parser.OnFoundText := OnText;
        Parser.Exec;
        Parser.Free;
        isExtractChapter := False;
        if parse.Count = 0 then
          Exit;
        for i := 0 to parse.Count - 1 do
          ExtractChapter;
      end;
    end;
    Source.Free;

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
