  function GetSenMangaInfoFromURL: Byte;
  var
    i, j, p: Integer;

    procedure GetChapters(const ln: Integer);
    begin
      if (GetTagName(parse[ln]) = 'a') and (GetVal(parse[ln], 'class') = 'lst') then
      begin
        mangaInfo.chapterLinks.Add(GetVal(parse[ln], 'href'));
        mangaInfo.chapterName.Add(parse[ln + 3]);
      end;
    end;

  begin
    mangaInfo.website := WebsiteRoots[SENMANGA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(SENMANGA_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(Source.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      mangaInfo.genres := '';
      p := 0;
      for i := 0 to parse.Count - 1 do
      begin
        //title
        if mangaInfo.title = '' then
          if (GetTagName(parse[i]) = 'h1') and (GetVal(parse[i], 'class') = 'ttl') then
            mangaInfo.title := CommonStringFilter(parse[i + 1]);
        //cover
        if (GetTagName(parse[i]) = 'img') and (GetVal(parse[i], 'class') = 'cvr') then
          mangaInfo.coverLink := FixURL(GetVal(parse[i], 'src'));
        //summary
        if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'class') = 'det') then
          mangaInfo.summary := parse[i + 3];

        if (GetTagName(parse[i]) = 'b') then
        begin
          //author
          if Trim(parse[i + 1]) = 'Author' then
            mangaInfo.authors := parse[i + 5];
          //artist
          if Trim(parse[i + 1]) = 'Arstist' then
            mangaInfo.authors := parse[i + 5];
          //status
          if Trim(parse[i + 1]) = 'Status' then
          begin
            mangaInfo.status := Trim(parse[i + 5]);
            if mangaInfo.status = 'Completed' then
              mangaInfo.status := '0'
            else
              mangaInfo.status := '1';
          end;
          //genres
          if Trim(parse[i + 1]) = 'Category' then
            for j := i + 3 to parse.Count - 1 do
            begin
              if GetTagName(parse[j]) = '/p' then
                Break
              else
              if GetTagName(parse[j]) = 'a' then
              begin
                if mangaInfo.genres = '' then
                  mangaInfo.genres := Trim(parse[j + 1])
                else
                  mangaInfo.genres := mangaInfo.genres + ', ' + Trim(parse[j + 1]);
              end;
            end;
        end;

        //chapters
        GetChapters(i);

        //chapterpage
        if (GetTagName(parse[i]) = 'a') and (Pos('/chapter-list/', parse[i]) > 0) then
          if Trim(parse[i + 1]) = 'Last' then
          begin
            j := StrToIntDef(ReplaceRegExpr(
               '^.*/chapter-list/(\d+)/?$', GetVal(parse[i], 'href'), '$1', True), 0);
            if j > p then p := j;
          end;
      end;

      //get the rest of chapters from another page
      if p > 1 then
      begin
        for i := 2 to p do
        begin
          s := mangaInfo.url;
          if s[Length(s)] <> '/' then s := s + '/';
          s := s + 'chapter-list/' + IntToStr(i) + '/';
          if GetPage(TObject(Source), s, Reconnect) then
          begin
            Parser := THTMLParser.Create(Source.Text);
            try
              Parser.OnFoundTag := OnTag;
              Parser.OnFoundText := OnText;
              parse.Clear;
              Parser.Exec;
            finally
              Parser.Free;
            end;
            if parse.Count > 0 then
              for j := 0 to parse.Count - 1 do
                GetChapters(j);
          end;
        end;
      end;
      Source.Free;

      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
      Result := NO_ERROR;
    end
    else
      Result := INFORMATION_NOT_FOUND;
  end;
