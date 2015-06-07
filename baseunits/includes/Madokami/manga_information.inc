  function GetMadokamiInfoFromURL: Byte;
  var
    i, j: Integer;
    s: String = '';
    regx: TRegExpr;
  begin
    mangaInfo.website := WebsiteRoots[MADOKAMI_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MADOKAMI_ID, URL);
    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    ParseHTML(Source.Text, parse);
    Source.Free;
    if parse.Count > 0 then
    begin
      mangaInfo.genres := '';
      mangaInfo.authors := '';

      //title
      if mangaInfo.title = '' then
        for i := parse.Count-1 downto 0 do
          if GetVal(parse[i], 'itemprop') = 'title' then
          begin
            mangaInfo.title := CommonStringFilter(parse[i+1]);
            Break;
          end;

      regx := TRegExpr.Create;
      regx.Expression := '^(.+)\.\w+$';
      for i := 0 to parse.Count-1 do
      begin
        //cover
        if mangaInfo.coverLink = '' then
          if (GetTagName(parse[i]) = 'img') and
            (GetVal(parse[i], 'itemprop') = 'image') then
            mangaInfo.coverLink := GetVal(parse[i], 'src');

        //authors
        if GetVal(parse[i], 'itemprop') = 'author' then
          AddCommaString(mangaInfo.authors, CommonStringFilter(parse[i+1]));

        //genre
        if (GetTagName(parse[i]) = 'span') and
          (GetVal(parse[i], 'itemprop') = 'title') then
        begin
          s := CommonStringFilter(parse[i+1]);
          s := TrimLeftChar(s, ['_']);
          if AnsiIndexText(s, [mangaInfo.title, '/', 'Manga','# - F', 'G - M', 'N - Z',
            'Non-English', 'AutoUploads']) = -1 then
            AddCommaString(mangaInfo.genres, s);
        end;
        if AnsiIndexStr(GetVal(parse[i], 'class'), ['tag', 'tag tag-category']) > -1 then
          AddCommaString(mangaInfo.genres, CommonStringFilter(parse[i+1]));

        //summary
        if (GetTagName(parse[i]) = 'div') and
          (GetVal(parse[i], 'class') = 'description') then
        begin
          mangaInfo.summary := '';
          for j := i+1 to parse.Count-1 do
          begin
            if GetTagName(parse[j]) = '/div' then
              Break
            else
            if Pos('<', parse[j]) = 0 then
              mangaInfo.summary := mangaInfo.summary + LineEnding + CommonStringFilter(parse[j]);
          end;
          mangaInfo.summary := Trim(mangaInfo.summary);
        end;

        //chapters
        if (GetTagName(parse[i]) = 'a') and
          (Pos('/reader/', parse[i]) <> 0) then
        begin
          mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
          for j := i-10 downto 0 do
            if GetTagName(parse[j]) = 'a' then
            begin
              s := CommonStringFilter(parse[j+1]);
              s := regx.Replace(s, '$1', True);
              mangaInfo.chapterName.Add(s);
              Break;
            end;
        end;
      end;
      regx.Free;
      Result := NO_ERROR;
    end;
  end;
