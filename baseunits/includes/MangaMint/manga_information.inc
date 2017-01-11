  function GetMangaMintInfoFromURL: Byte;
  var
    i, j, p: Integer;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[MANGAMINT_ID, 0];
    mangaInfo.url := FillMangaSiteHost(MANGAMINT_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(Source.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;


    if parse.Count > 0 then
    begin
      p := 0;
      for i := 0 to parse.Count - 1 do
      begin
        //title
        if mangaInfo.title = '' then
          if GetTagName(parse[i]) = 'h2' then
            mangaInfo.title := CommonStringFilter(
              ReplaceRegExpr('\sManga$', Trim(parse[i + 1]), '', False));

        //cover
        if (GetTagName(parse[i]) = 'img') and
          (GetVal(parse[i], 'class') = 'imagefield imagefield-field_image2') then
          mangaInfo.coverLink := GetVal(parse[i], 'src');

        if GetVal(parse[i], 'class') = 'field-label' then
        begin
          //status
          if Pos('Status:', parse[i + 1]) > 0 then
          begin
            if (LowerCase(Trim(parse[i + 7])) = 'complete') then
              mangaInfo.status := '0'
            else
              mangaInfo.status := '1';
          end;

          //author
          if Pos('Author:', parse[i + 1]) > 0 then
            mangaInfo.authors := CommonStringFilter(parse[i + 7]);

          //artits
          if Pos('Artist:', parse[i + 1]) > 0 then
            mangaInfo.artists := CommonStringFilter(parse[i + 7]);

          //genre
          if Pos('Type:', parse[i + 1]) > 0 then
            mangaInfo.genres := Trim(parse[i + 7]);
        end;

        //summary
        if GetTagName(parse[i]) = '/fieldset' then
          if GetTagName(parse[i + 2]) = 'p' then
            mangaInfo.summary := CommonStringFilter(parse[i + 3]);

        //chapters
        if (GetTagName(parse[i]) = 'thead') then
          isExtractChapters := True;
        if isExtractChapters then
        begin
          if GetTagName(parse[i]) = '/tbody' then
            isExtractChapters := False
          else
          if GetTagName(parse[i]) = 'a' then
          begin
            Inc(mangaInfo.numChapter);
            mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
            mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
          end;
        end;

        //morechapterspage
        if (p = 0) and
          (GetTagName(parse[i]) = 'li') and (GetVal(parse[i], 'class') = 'pager-last last') then
          if GetTagName(parse[i + 1]) = 'a' then
            p := StrToIntDef(ReplaceRegExpr('^.*\?page=(\d+)$', GetVal(parse[i + 1], 'href'), '$1', True), 0);
      end;
      Result := NO_ERROR;
    end;

    //fetch chapters pages
    if p > 0 then
      for j := 1 to p do
      begin
        isExtractChapters := False;
        Source.Clear;
        if GetPage(TObject(Source), mangaInfo.url + '?page=' + IntToStr(j), AReconnect) then
        begin
          parse.Clear;
          Parser := THTMLParser.Create(Source.Text);
          try
            Parser.OnFoundTag := OnTag;
            Parser.OnFoundText := OnText;
            Parser.Exec;
            if parse.Count > 0 then
              for i := 0 to parse.Count - 1 do
              begin
                if (GetTagName(parse[i]) = 'thead') then
                  isExtractChapters := True;
                if isExtractChapters then
                begin
                  if GetTagName(parse[i]) = '/tbody' then
                    isExtractChapters := False
                  else
                  if GetTagName(parse[i]) = 'a' then
                  begin
                    Inc(mangaInfo.numChapter);
                    mangaInfo.chapterLinks.Add(GetVal(parse[i], 'href'));
                    mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
                  end;
                end;
              end;
          finally
            Parser.Free;
          end;
          Result := NO_ERROR;
        end
        else
          Result := NET_PROBLEM;
      end;

    //invert chapters
    InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
    Source.Free;
  end;
