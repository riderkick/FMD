  function GetReadMangaTodayInfoFromURL: Byte;
  var
    s: String;
    i, j: Integer;
  begin
    mangaInfo.website := WebsiteRoots[READMANGATODAY_ID, 0];
    mangaInfo.url := FillMangaSiteHost(READMANGATODAY_ID, AURL);
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
      for i := 0 to parse.Count-1 do
      begin
        //title
        if mangaInfo.title = '' then
          if GetTagName(parse[i]) = 'h1' then
            mangaInfo.title := CommonStringFilter(parse[i+1]);

        //cover
        if (GetTagName(parse[i]) = 'img') and
          (GetVal(parse[i], 'class') = 'img-responsive') then
            mangaInfo.coverLink := GetVal(parse[i], 'src');

        if GetTagName(parse[i]) = 'dt' then
        begin
          //genre
          if Pos('Categories:', parse[i+1]) > 0 then
          begin
            mangaInfo.genres := '';
            for j := i+2 to parse.Count-1 do
            begin
              if GetTagName(parse[j]) = '/dd' then
                Break
              else
              if GetTagName(parse[j]) = 'a' then
                if mangaInfo.genres = '' then
                  mangaInfo.genres := GetVal(parse[j], 'title')
                else
                  mangaInfo.genres := mangaInfo.genres + ', ' + GetVal(parse[j], 'title');
            end;
          end;

          //status
          if Pos('Status:', parse[i+1]) > 0 then
          begin
            s := LowerCase(Trim(parse[i+6]));
            if s = 'ongoing' then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';
          end;
        end;

        //summary
        if GetVal(parse[i], 'class') = 'list-group-item movie-detail' then
        begin
          mangaInfo.summary := '';
          for j := i + 1 to parse.Count-1 do
          begin
            if GetTagName(parse[j]) = '/li' then
              Break
            else
            if Pos('<', parse[j]) = 0 then
              mangaInfo.summary := mangaInfo.summary + LineEnding + CommonStringFilter(parse[j]);
          end;
          mangaInfo.summary := Trim(mangaInfo.summary);
        end;

        //author
        if Trim(parse[i]) = 'Author' then
          for j := i downto 0 do
            if GetTagName(parse[j]) = 'a' then
            begin
              mangaInfo.authors := CommonStringFilter(parse[j+1]);
              Break;
            end;

        //artist
        if Trim(parse[i]) = 'Artist' then
          for j := i downto 0 do
            if GetTagName(parse[j]) = 'a' then
            begin
              mangaInfo.artists := CommonStringFilter(parse[j+1]);
              Break;
            end;

        //chapters
        if (GetTagName(parse[i]) = 'ul') and
          (GetVal(parse[i], 'class') = 'chp_lst') then
          for j := i+1 to parse.Count-1 do
          begin
            if GetTagName(parse[j]) = '/ul' then
              Break
            else
            if GetTagName(parse[j]) = 'a' then
              mangaInfo.chapterLinks.Add(GetVal(parse[j], 'href'))
            else
            if GetVal(parse[j], 'class') = 'icon-arrow-2' then
              mangaInfo.chapterName.Add(Trim(parse[j+2]));
          end;
      end;
      Result := NO_ERROR;
    end;

    //invert chapters
    if mangaInfo.chapterLinks.Count > 1 then
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
  end;
