  function GetManga24hInfoFromURL: Byte;
  var
    // patchURL,
    s: String;
    i, j: Cardinal;
    isExtractChapters: Boolean = False;
    isExtractSummary: Boolean = False;
  begin
    // patchURL:= UTF8ToANSI(URL);
    // Insert('comics/', patchURL, 10);
    mangaInfo.url := FillMangaSiteHost(MANGA24H_ID, URL);
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
    mangaInfo.website := WebsiteRoots[MANGA24H_ID, 0];
    mangaInfo.summary := '';

    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover link
      if (Pos('class="img-rounded', parse[i]) > 0) then
      begin
        mangaInfo.coverLink := EncodeURL(GetVal(parse[i], 'src'));
        s := mangaInfo.coverLink;
      end;

      // get summary
      if (Pos('"clearfix"', parse[i]) > 0) and
        (Pos('<p>', parse[i + 3]) > 0) then
      begin
        j := i + 5;
        mangaInfo.summary := '';
        while (Pos('$(document).ready(function()', parse[j]) = 0) and
          (j < parse.Count - 1) do
        begin
          s := parse[j];
          if (Length(s) > 0) and (s[1] <> '<') then
          begin
            parse[j] := StringFilter(HTMLEntitiesFilter(parse[j]));
            parse[j] := StringReplace(parse[j], #10, '\n', [rfReplaceAll]);
            parse[j] := StringReplace(parse[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary +
              StringFilter(TrimRight(TrimLeft(parse[j])));
          end;
          Inc(j);
        end;
      end;

      if (Pos('<tbody>', parse[i]) <> 0) and (not isExtractSummary) then
        isExtractChapters := True;

      if (Pos('</tbody>', parse[i]) <> 0) and (isExtractSummary) then
        isExtractChapters := False;


      // get chapter name and links
      if (isExtractChapters) and
        (Pos('<td>', parse[i]) <> 0) and
        (GetVal(parse[i + 1], 'href') <> '') and
        (Pos('</a>', parse[i + 3]) <> 0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(
          CorrectURL('/' + StringReplace(GetVal(parse[i + 1], 'href'), WebsiteRoots[MANGA24H_ID, 1],
          '', [rfReplaceAll])));
        parse[i + 2] := HTMLEntitiesFilter(parse[i + 2]);
        parse[i + 2] := StringReplace(parse[i + 2], #10, '', [rfReplaceAll]);
        parse[i + 2] := StringReplace(parse[i + 2], #13, '', [rfReplaceAll]);
        parse[i + 2] := TrimLeft(parse[i + 2]);
        mangaInfo.chapterName.Add(
          StringFilter(TrimRight(RemoveSymbols(parse[i + 2]))));
      end;

      // get title
      if (Pos('<title>', parse[i]) <> 0) and (mangaInfo.title = '') then
        mangaInfo.title := TrimLeft(
          StringFilter(GetString('~!@' + parse[i + 1], '~!@', ' - Truyen Tranh Online')));

      // get authors
      if (Pos('Tác giả :', parse[i]) <> 0) then
        mangaInfo.authors := TrimLeft(StringFilter(parse[i + 3]));

      // get artists
      if (Pos('Họa sỹ :', parse[i]) <> 0) then
        mangaInfo.artists := TrimLeft(StringFilter(parse[i + 3]));

      // get genres
      if (Pos('Thể loại :', parse[i]) <> 0) then
      begin
        mangaInfo.genres := '';
        for j := 0 to 37 do
          if Pos(LowerCase(defaultGenres[j]), LowerCase(parse[i + 4])) <> 0 then
            mangaInfo.genres := mangaInfo.genres + (defaultGenres[j] + ', ');
      end;

      // get status
      if (Pos('Tình Trạng:', parse[i]) <> 0) then
      begin
        if Pos('Hoàn Thành', parse[i + 4]) <> 0 then
          mangaInfo.status := '0'   // ongoing
        else
          mangaInfo.status := '1';  // completed
      end;
    end;

    // Since chapter name and link are inverted, we need to invert them
    if mangainfo.ChapterName.Count > 1 then
    begin
      i := 0;
      j := mangainfo.ChapterName.Count - 1;
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
