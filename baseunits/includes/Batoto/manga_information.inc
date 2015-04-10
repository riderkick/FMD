  function GetBatotoInfoFromURL: Byte;
  var
    patchURL, s: String;
    i, j: Cardinal;

  begin
    patchURL := URL;
    //if Pos('comic/_/comics', patchURL) = 0 then
    //patchURL:= StringReplace(URL, 'comic/_', 'comic/_/comics', []);
    mangaInfo.url := FillMangaSiteHost(BATOTO_ID, patchURL);

    Source.Clear;
    if not GetPage(TObject(Source), TrimLeft(TrimRight(mangaInfo.url)), Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source using our own HTML parser
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    Source.Free;
    mangaInfo.website := WebsiteRoots[BATOTO_ID, 0];

    mangaInfo.genres := '';
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      // get cover link
      if (GetTagName(parse.Strings[i]) = 'img') and
        (Pos('max-height:500px', parse.Strings[i]) > 0) then
      begin
        mangaInfo.coverLink :=
          CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
      end;

      // get title
      if (Pos('ipsType_pagetitle', parse[i]) > 0) then
        mangaInfo.title := Trim(StringFilter(Trim(parse[i + 1])));

      // get summary
      if (Pos('Description:', parse.Strings[i]) <> 0) then
      begin
        j := i + 3;
        mangaInfo.summary := '';
        while (Pos('</td>', parse.Strings[j]) = 0) and (j < parse.Count - 1) do
        begin
          s := parse.Strings[j];
          if s[1] <> '<' then
          begin
            parse.Strings[j] := StringFilter(parse.Strings[j] + #10#13);
            parse.Strings[j] :=
              StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
            parse.Strings[j] :=
              StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
            mangaInfo.summary := mangaInfo.summary + StringFilter(parse.Strings[j]);
          end;
          Inc(j);
        end;
      end;

      // get chapter name and links (bad code)
      //mulai
      if OptionShowAllLang then
      begin
        if (not OptionBatotoUseIEChecked) and
          (GetTagName(parse.Strings[i]) = 'a') and
          (Pos('/read/_/', parse.Strings[i]) > 0) and
          (i + 8 < parse.Count - 1) and
          (Pos('title=', parse.Strings[i + 8]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(
            (StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
            WebsiteRoots[BATOTO_ID, 1], '', [rfReplaceAll])));
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #10, '', [rfReplaceAll]);
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #13, '', [rfReplaceAll]);
          parse.Strings[i + 2] := StringFilter(TrimLeft(parse.Strings[i + 2]));
          s := StringFilter(TrimRight(RemoveSymbols(parse.Strings[i + 2])));
          if OptionShowBatotoSG then
            s := s + ' [by ' + StringFilter(
              TrimRight(RemoveSymbols(parse.Strings[i + 15]))) + ']' + ' [' +
              GetAttributeValue(GetTagAttribute(parse.Strings[i + 8], 'title=')) + ']';
          mangaInfo.chapterName.Add(s);
        end
        else
        if (OptionBatotoUseIEChecked) and
          (GetTagName(parse.Strings[i]) = 'a') and
          (Pos('/read/_/', parse.Strings[i]) > 0) and
          (i + 2 < parse.Count - 1) and
          (Pos('title=', parse.Strings[i - 3]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(
            (StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
            WebsiteRoots[BATOTO_ID, 1], '', [rfReplaceAll])));
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #10, '', [rfReplaceAll]);
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #13, '', [rfReplaceAll]);
          parse.Strings[i + 2] := StringFilter(TrimLeft(parse.Strings[i + 2]));
          mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i + 2])));
        end;
      end
      else
      begin

        if (not OptionBatotoUseIEChecked) and
          (GetTagName(parse.Strings[i]) = 'a') and
          (Pos('/read/_/', parse.Strings[i]) > 0) and
          (i + 8 < parse.Count - 1) and
          (Pos('English', parse.Strings[i + 8]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(
            (StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
            WebsiteRoots[BATOTO_ID, 1], '', [rfReplaceAll])));
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #10, '', [rfReplaceAll]);
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #13, '', [rfReplaceAll]);
          parse.Strings[i + 2] := StringFilter(TrimLeft(parse.Strings[i + 2]));
          s := StringFilter(TrimRight(RemoveSymbols(parse.Strings[i + 2])));
          if OptionShowBatotoSG then
            s := s + ' [by ' + StringFilter(
              TrimRight(RemoveSymbols(parse.Strings[i + 15]))) + ']';
          mangaInfo.chapterName.Add(s);
        end
        else
        if (OptionBatotoUseIEChecked) and
          (GetTagName(parse.Strings[i]) = 'a') and
          (Pos('/read/_/', parse.Strings[i]) > 0) and
          (i + 2 < parse.Count - 1) and
          (Pos('English', parse.Strings[i - 3]) > 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(
            (StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')),
            WebsiteRoots[BATOTO_ID, 1], '', [rfReplaceAll])));
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #10, '', [rfReplaceAll]);
          parse.Strings[i + 2] :=
            StringReplace(parse.Strings[i + 2], #13, '', [rfReplaceAll]);
          parse.Strings[i + 2] := StringFilter(TrimLeft(parse.Strings[i + 2]));
          mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i + 2])));
        end;
      end;

      // get authors
      if (i + 5 < parse.Count - 1) and
        (Pos('Author:', parse.Strings[i]) > 0) then
        mangaInfo.authors := TrimLeft(parse.Strings[i + 5]);

      // get artists
      if (i + 5 < parse.Count - 1) and
        (Pos('Artist:', parse.Strings[i]) > 0) then
        mangaInfo.artists := TrimLeft(parse.Strings[i + 5]);

      // get genres
      if Pos('/search?genres=', parse.Strings[i]) > 0 then
      begin
        if Pos('</span', parse.Strings[i + 4]) > 0 then
          if mangaInfo.genres = '' then
            mangaInfo.genres := Trim(parse.Strings[i + 3])
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + parse.Strings[i + 3];
      end;

      // get status
      if (Pos('Status:', parse.Strings[i]) > 0) then
      begin
        if (i + 4 < parse.Count - 1) and
          (Pos('Ongoing', parse.Strings[i + 4]) > 0) then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
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
