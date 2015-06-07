  function GetLoneMangaInfoFromURL: Byte;
  var
    s: String;
    i, j: Integer;
  begin
    mangaInfo.website := WebsiteRoots[LONEMANGA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(LONEMANGA_ID, URL);
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
      for i := 0 to parse.Count-1 do
      begin
        //title
        if mangaInfo.title = '' then
          if GetVal(parse[i], 'class') = 'kommiku-bread' then
            mangaInfo.title := CommonStringFilter(parse[i+2]);

        //cover
        if mangaInfo.coverLink = '' then
          if (GetTagName(parse[i]) = 'img') and
            (Pos('float:', parse[i]) <> 0) then
              mangaInfo.coverLink := GetVal(parse[i], 'src');

        if (GetTagName(parse[i]) = 'td') and
          (GetVal(parse[i], 'class') = 'infoTabOne') then
        begin
          //genre
          if Pos('Type:', parse[i+2]) > 0 then
            mangaInfo.genres := Trim(parse[i+6]);

          //status
          if Pos('Status:', parse[i+2]) > 0 then
          begin
            s := LowerCase(Trim(parse[i+6]));
            if s = 'ongoing' then
              mangaInfo.status := '1'
            else
              mangaInfo.status := '0';
          end;

          //summary
          if Pos('Summary:', parse[i+2]) <> 0 then
          begin
            mangaInfo.summary := '';
            for j := i + 5 to parse.Count-1 do
            begin
              if GetTagName(parse[j]) = '/td' then
                Break
              else
              if Pos('<', parse[j]) = 0 then
                mangaInfo.summary := mangaInfo.summary + LineEnding + CommonStringFilter(parse[j]);
            end;
            mangaInfo.summary := Trim(mangaInfo.summary);
          end;
        end;

        //chapters
        if (GetTagName(parse[i]) = 'div') and
          (GetVal(parse[i], 'class') = 'column') then
        begin
          for j := i+2 to parse.Count-1 do
          begin
            if GetTagName(parse[j]) = '/table' then
              Break
            else
            if GetTagName(parse[j]) = 'a' then
            begin
              mangaInfo.chapterLinks.Add(GetVal(parse[j], 'href'));
              mangaInfo.chapterName.Add(CommonStringFilter(parse[j+1]));
            end;
          end;
        end;
      end;
      Result := NO_ERROR;
    end;

    //invert chapters
    if mangaInfo.chapterLinks.Count > 1 then
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
  end;
