  function GetWPMangaInfoFromURL: Byte;
  var
    s: String;
    i, j, k: Integer;
    isSummaryDone: Boolean = False;

    procedure FindChapters(P: Integer);
    var
      x, y: Integer;
    begin
      for x := p+1 to parse.Count-1 do
      begin
        if GetTagName(parse[x]) = '/ul' then
          Break
        else
        if GetTagName(parse[x]) = 'a' then
        begin
          mangaInfo.chapterLinks.Add(GetVal(parse[x], 'href'));
          s := '';
          for y := x+1 to parse.Count-1 do
          begin
            s := Trim(parse[y]);
            if (s <> '') and (Pos('<', s) <> 1) then
              Break;
          end;
          mangaInfo.chapterName.Add(CommonStringFilter(s));
        end;
      end;
    end;

  begin
    mangaInfo.website := WebsiteRoots[WebsiteID, 0];
    mangaInfo.url := FillMangaSiteHost(WebsiteID, URL);

    if RightStr(mangaInfo.url, 1) <> '/' then
      mangaInfo.url += '/';

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
      mangaInfo.summary := '';
      for i := 0 to parse.Count-1 do
      begin
        //cover
        if mangaInfo.coverLink = '' then
          if (GetTagName(parse[i]) = 'img') and
            (GetVal(parse[i], 'class') = 'cvr') then
            mangaInfo.coverLink := GetVal(parse[i], 'src');

        //title
        if mangaInfo.title = '' then
          if (GetTagName(parse[i]) = 'h1') and
            (GetVal(parse[i], 'class') = 'ttl') then
            mangaInfo.title := CommonStringFilter(parse[i+1]);

        //details
        if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'class') = 'det') then
        begin
          for j := i+1 to parse.Count-1 do
          begin
            if GetTagName(parse[j]) = 'h2' then
              Break
            else
            begin
              if GetTagName(parse[j]) = 'b' then
              begin
                s := Trim(parse[j+1]);
                //author
                if s = 'Author' then
                  mangaInfo.authors := CommonStringFilter(
                    TrimLeftChar(parse[j+3], [':', ' ']))
                //artist
                else
                if s = 'Artist' then
                  mangaInfo.artists := CommonStringFilter(
                    TrimLeftChar(parse[j+3], [':', ' ']))
                //status
                else
                if s = 'Status' then
                begin
                  if Pos('completed', LowerCase(parse[j+3])) <> 0 then
                    mangaInfo.status := '0'
                  else
                    mangaInfo.status := '1';
                end
                //genres
                else
                if (s = 'Category') or (s = 'Genres') then
                begin
                  for k := j+4 to parse.Count-1 do
                  begin
                    if GetTagName(parse[k]) = '/p' then
                      Break
                    else
                    if Pos('<', parse[k]) = 0 then
                      AddCommaString(mangaInfo.genres, CommonStringFilter(parse[k]));
                  end;
                end
                else
                if s = 'Type' then
                  AddCommaString(mangaInfo.genres,
                    CommonStringFilter(TrimLeftChar(parse[j+3], [':', ' '])))
                else
                //summary
                if s = 'Summary' then
                begin
                  isSummaryDone := True;
                  for k := j+3 to parse.Count-1 do
                  begin
                    if GetTagName(parse[k]) = '/p' then
                      Break
                    else
                    if Pos('<', parse[k]) = 0 then
                      mangaInfo.summary := Trim(mangaInfo.summary + LineEnding +
                        CommonStringFilter(parse[k]));
                  end;
                end;
              end;
              //summary
              if (not isSummaryDone) and (GetTagName(parse[j]) = 'p') then
              begin
                s := Trim(parse[j+1]);
                if (s <> '') and (Pos('<', s) = 0) then
                begin
                  isSummaryDone := True;
                  for k := j+1 to parse.Count-1 do
                  begin
                    if GetTagName(parse[k]) = '/p' then
                      Break
                    else
                    if Pos('<', parse[k]) = 0 then
                      mangaInfo.summary := Trim(mangaInfo.summary + LineEnding +
                        CommonStringFilter(parse[k]));
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      //chapters
      for i := 0 to parse.Count do
      begin
        if (GetTagName(parse[i]) = 'ul') and
          (GetVal(parse[i],'class') = 'lst') then
        begin
          FindChapters(i);
          Break;
        end;
      end;

      //invert chapters
      InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
      Result := NO_ERROR;
    end;
  end;
