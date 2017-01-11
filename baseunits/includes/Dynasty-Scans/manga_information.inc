  function GetDynastyScansInfoFromURL: Byte;
  var
    i, j: Integer;
    hs: String = '';
  begin
    mangaInfo.website := WebsiteRoots[DYNASTYSCANS_ID, 0];
    mangaInfo.url := FillMangaSiteHost(DYNASTYSCANS_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
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
      for i := 0 to parse.Count-1 do
      begin
        //title
        if mangaInfo.title = '' then
          if GetVal(parse[i], 'class') = 'tag-title' then
            mangaInfo.title := CommonStringFilter(parse[i+2]);

        //cover
        if mangaInfo.coverLink = '' then
          if (GetTagName(parse[i]) = 'img') and
            (GetVal(parse[i], 'class') = 'thumbnail') then
          begin
            mangaInfo.coverLink := GetVal(parse[i], 'src');
            if Length(mangaInfo.coverLink) > 0 then
              if mangaInfo.coverLink[1] = '/' then
                mangaInfo.coverLink := WebsiteRoots[DYNASTYSCANS_ID, 1] +
                  mangaInfo.coverLink;
          end;

        if (GetTagName(parse[i]) = 'a') then
        begin
          //authors
          if Pos('/authors/', parse[i]) <> 0 then
            mangaInfo.authors := CommonStringFilter(parse[i+1]);

          //genre/tags
          if GetVal(parse[i], 'class') = 'label' then
            AddCommaString(mangaInfo.genres, CommonStringFilter(parse[i+1]));
        end;

        //status
        if GetTagName(parse[i]) = 'small' then
          if Pos('ongoing', LowerCase(parse[i+1])) <> 0 then
            mangaInfo.status := '1'
          else
          if Pos('completed', LowerCase(parse[i+1])) <> 0 then
            mangaInfo.status := '0';

        //genre/tags
        if GetVal(parse[i], 'class') = 'doujin_tags' then
          AddCommaString(mangaInfo.genres, CommonStringFilter(parse[i+2]));

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
        if (GetTagName(parse[i]) = 'dl') and
          (GetVal(parse[i], 'class') = 'chapter-list') then
        begin
          for j := i+1 to parse.Count-1 do
          begin
            if GetTagName(parse[j]) = '/dl' then
              Break
            else
            if (GetTagName(parse[j]) = 'a') and
              (GetVal(parse[j], 'class') = 'name') then
            begin
              mangaInfo.chapterLinks.Add(GetVal(parse[j], 'href'));
              mangaInfo.chapterName.Add(Trim(hs + CommonStringFilter(parse[j+1])));
            end
            else
            if GetTagName(parse[j]) = 'dt' then
              hs := CommonStringFilter(parse[j+1]) + ' ';
          end;
        end;
      end;
      Result := NO_ERROR;
    end;
  end;
