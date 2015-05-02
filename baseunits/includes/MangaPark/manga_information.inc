  function GetMangaParkInfoFromURL: Byte;
  var
    s, sver: String;
    isExtractSummary: Boolean = True;
    isExtractGenres: Boolean = False;
    i, j: Cardinal;
    tnames, tlinks: TStringList;
  begin
    mangaInfo.website := WebsiteRoots[MANGAPARK_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAPARK_ID, URL);// + '&confirm=yes';
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source
    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    // using parser (cover link, summary, chapter name and link)
    if parse.Count > 0 then
    begin
      tnames:= TStringList.Create;
      tlinks:= TStringList.Create;
      try
        sver := '';
        for i := 0 to parse.Count - 1 do
        begin
          // get manga title
          if (mangaInfo.title = '') and
            (Pos('property="og:title"', parse[i]) <> 0) then
          begin
            mangaInfo.title := CommonStringFilter(GetVal(parse[i], 'content'));
            mangaInfo.title := ReplaceRegExpr('\sManga$', mangaInfo.title, '', False);
          end;

          // get cover
          if (GetTagName(parse.Strings[i]) = 'meta') and
            (Pos('property="og:image"', parse.Strings[i]) > 0) then
            mangaInfo.coverLink := CorrectURL(GetAttributeValue(
              GetTagAttribute(parse.Strings[i], 'content=')));

          // get summary
          if (Pos('<h2>', parse.Strings[i]) <> 0) and
            (Pos('Summary', parse.Strings[i + 1]) <> 0) and
            (isExtractSummary) then
          begin
            j := i + 3;
            while (j < parse.Count) and (Pos('</p>', parse.Strings[j]) = 0) do
            begin
              s := parse.Strings[j];
              if s[1] <> '<' then
              begin
                parse.Strings[j] := HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
                parse.Strings[j] := StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
                parse.Strings[j] := StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
                mangaInfo.summary := parse.Strings[j];
              end;
              Inc(j);
            end;
            isExtractSummary := False;
          end;

          // get chapter name and links
          if (Pos('<a', parse[i]) <> 0) and (Pos('class="st st', parse[i]) <> 0) then
          begin
            if tlinks.Count > 0 then
            begin
              InvertStrings([tnames, tlinks]);
              mangaInfo.chapterName.AddStrings(tnames);
              mangaInfo.chapterLinks.AddStrings(tlinks);
              tnames.Clear;
              tlinks.Clear;
            end;
            sver := Trim(parse[i + 1]);
          end;
          if (Pos('<a', parse[i]) <> 0) and (Pos('class="ch sts sts', parse[i]) <> 0) then
          begin
            Inc(mangaInfo.numChapter);
            s := GetVal(parse[i], 'href');
            if RightStr(s, 2) = '/1' then
              SetLength(s, Length(s) - 2);
            tlinks.Add(s);
            tnames.Add(CommonStringFilter(
              Format('%s %s %s', [sver, Trim(parse[i + 1]), Trim(parse[i + 3])])));
          end;

          // get authors
          if (i + 4 < parse.Count) and (Pos('Author(s)', parse.Strings[i]) <> 0) then
            mangaInfo.authors := TrimLeft(parse.Strings[i + 6]);

          // get artists
          if (i + 4 < parse.Count) and (Pos('Artist(s)', parse.Strings[i]) <> 0) then
            mangaInfo.artists := TrimLeft(parse.Strings[i + 6]);

          // get genres
          if (Pos('Genre(s)', parse.Strings[i]) <> 0) then
          begin
            isExtractGenres := True;
            mangaInfo.genres := '';
          end;

          if isExtractGenres then
          begin
            if Pos('/genre/', parse.Strings[i]) <> 0 then
              mangaInfo.genres := mangaInfo.genres +
                TrimLeft(TrimRight(parse.Strings[i + 1])) + ', ';
            if Pos('</td>', parse.Strings[i]) <> 0 then
              isExtractGenres := False;
          end;

          // get status
          if (i + 2 < parse.Count) and (Pos('Status', parse.Strings[i]) <> 0) then
          begin
            if Pos('Ongoing', parse.Strings[i + 4]) <> 0 then
              mangaInfo.status := '1'   // ongoing
            else
              mangaInfo.status := '0';  // completed
          end;
        end;
      finally
        if tlinks.Count > 0 then
        begin
          InvertStrings([tnames, tlinks]);
          mangaInfo.chapterName.AddStrings(tnames);
          mangaInfo.chapterLinks.AddStrings(tlinks);
        end;
        tnames.Free;
        tlinks.Free;
      end;
    end;
    Result := NO_ERROR;
  end;
