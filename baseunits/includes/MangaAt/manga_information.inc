  function GetMangaAtInfoFromURL: Byte;
  var
    s: String;
    i, j: Integer;
  begin
    mangaInfo.website := WebsiteRoots[MANGAAT_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAAT_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
    begin
      mangaInfo.genres := '';
      mangaInfo.summary := '';
      for i := 0 to parse.Count - 1 do
      begin
        //title
        if mangaInfo.title = '' then
          if GetVal(parse[i], 'class') = 'EnglishName' then
          begin
            mangaInfo.title := CommonStringFilter(parse[i + 1]);
            if Length(mangaInfo.title) > 2 then
              if (mangaInfo.title[1] = '(') and
                (mangaInfo.title[Length(mangaInfo.title)] = ')') then
              begin
                Delete(mangaInfo.title, 1, 1);
                Delete(mangaInfo.title, Length(mangaInfo.title), 1);
              end;
          end;

        //cover
        if (GetTagName(parse[i]) = 'img') and
          (GetVal(parse[i], 'class') = 'manga-cover') then
          mangaInfo.coverLink := GetVal(parse[i], 'src');

        if GetTagName(parse[i]) = 'h3' then
        begin
          //genre
          if Pos('التصنيف:', parse[i + 1]) > 0 then
          begin
            for j := i + 2 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = '/ul' then
                Break;
              if Pos('<', parse[j]) = 0 then
                if mangaInfo.genres = '' then
                  mangaInfo.genres := Trim(parse[j])
                else
                  mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[j]);
            end;
            mangaInfo.genres := Trim(mangaInfo.genres);
          end;

          //author
          if Pos('اسم المؤلف بالأنجليزية', parse[i + 1]) > 0 then
            mangaInfo.authors := CommonStringFilter(TrimLeftChar(parse[i + 5], [':']));

          //status
          if Pos('الحالة :', parse[i + 1]) > 0 then
          begin
            s := Trim(TrimLeftChar(parse[i + 5], [':']));
            if s = 'مستمرة' then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';
          end;

          //summary
          if Pos('نبذة عن المانجا', parse[i + 1]) > 0 then
            mangaInfo.summary := CommonStringFilter(parse[i + 5]);
        end;

        //chapters
        if (GetTagName(parse[i]) = 'a') and
          (GetVal(parse[i], 'class') = 'chapter') then
        begin
          Inc(mangaInfo.numChapter);
          s := GetVal(parse[i], 'href');
          if Length(s) > 3 then
            if RightStr(s, 3) = '/1/' then
              SetLength(s, Length(s) - 3);
          mangaInfo.chapterLinks.Add(s);
          mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
        end;
      end;
      Result := NO_ERROR;
    end;

    //invert chapters
    if mangaInfo.chapterLinks.Count > 0 then
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
  end;
