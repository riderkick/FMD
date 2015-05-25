  function GetAcademyVNInfoFromURL: Byte;
  var
    s: String;
    i, j: Integer;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[ACADEMYVN_ID, 0];
    mangaInfo.url := FillMangaSiteHost(ACADEMYVN_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
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
          if GetVal(parse[i], 'class') = '__name' then
            mangaInfo.title := CommonStringFilter(parse[i + 1]);

        //cover
        if (GetTagName(parse[i]) = 'div') and
          (GetVal(parse[i], 'class') = '__image') then
          if GetTagName(parse[i + 2]) = 'img' then
            mangaInfo.coverLink := GetVal(parse[i + 2], 'src');

        if GetTagName(parse[i]) = 'strong' then
        begin
          //genre
          if Pos('Thể loại:', parse[i + 1]) > 0 then
          begin
            for j := i + 2 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = '/p' then
                Break;
              if Pos('<', parse[j]) = 0 then
                  mangaInfo.genres := mangaInfo.genres + parse[j];
            end;
            mangaInfo.genres := Trim(mangaInfo.genres);
          end;

          //author
          if Pos('Tác giả:', parse[i + 1]) > 0 then
            mangaInfo.authors := CommonStringFilter(TrimLeftChar(parse[i + 5], [':']));

          //status
          if Pos('Tình trạng:', parse[i + 1]) > 0 then
          begin
            s := Trim(TrimLeftChar(parse[i + 3], [':']));
            if (s = 'Đang tiến hành') or (s = 'Ngưng') then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';
          end;
        end;
        //summary
        if GetVal(parse[i], 'class') = '__description' then
          for j := i + 3 to parse.Count - 1 do
          begin
            if GetTagName(parse[j]) = '/div' then
              Break;
            if Pos('<', parse[j]) = 0 then
              mangaInfo.summary := mangaInfo.summary + CommonStringFilter(parse[j]);
          end;

        //chapters
        if GetVal(parse[i], 'class') = 'table-scroll' then
          isExtractChapters := True;
        if isExtractChapters then
        begin
          if GetTagName(parse[i]) = '/table' then
            isExtractChapters := False
          else
          if GetTagName(parse[i]) = 'a' then
          begin
            mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
            mangaInfo.chapterName.Add(Trim(parse[i + 2]));
          end;
        end;
      end;
      Result := NO_ERROR;
    end;

    //invert chapters
    if mangaInfo.chapterLinks.Count > 1 then
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
  end;
