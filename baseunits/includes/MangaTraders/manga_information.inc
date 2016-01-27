  function GetMangaTradersInfoFromURL: Byte;
  var
    s: String;
    isExtractGenres: Boolean = False;
    i: Integer;
    regx: TRegExpr;
  begin
    mangaInfo.website := WebsiteRoots[MANGATRADERS_ID, 0];
    // fixing url
    mangaInfo.url := FillMangaSiteHost(MANGATRADERS_ID, URL);
    regx := TRegExpr.Create;
    try
      regx.Expression := '\/manga\/\?series\=(.*)$';
      if regx.Exec(mangaInfo.url) then
        mangaInfo.url := regx.Replace(mangaInfo.url, '/read-online/$1', True);
    finally
      regx.Free;
    end;

    if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;


    // using parser (cover link, summary, chapter name and link)
    if parse.Count = 0 then
    begin
      Parser.Free;
      Source.Free;
      Exit;
    end;

    mangaInfo.authors := '';
    mangaInfo.artists := '';
    mangaInfo.summary := '';

    regx := TRegExpr.Create;
    regx.Expression := '\/page\-\d+$';
    for i := 0 to parse.Count - 1 do
    begin
      //title
      if Pos('<h1', parse[i]) > 0 then
        mangaInfo.title := CommonStringFilter(parse[i + 1]);

      //cover
      if Pos('property="og:image:url"', parse[i]) > 0 then
        mangaInfo.coverLink := GetVal(parse[i], 'content');

      //author
      if Pos('<b', parse[i]) > 0 then
        if Pos('Author:', parse[i + 1]) > 0 then
          mangaInfo.authors := CommonStringFilter(parse[i + 4]);

      //status
      if Pos('<b', parse[i]) > 0 then
        if Pos('Publishing Status:', parse[i + 1]) > 0 then
        begin
          if LowerCase(Trim(parse[i + 3])) = 'ongoing' then
            mangaInfo.status := '1'
          else
            mangaInfo.status := '0';
        end;

      //genres
      if Pos('Genre:', parse[i]) > 0 then
      begin
        isExtractGenres := True;
        mangaInfo.genres := '';
      end;
      if isExtractGenres and (Pos('</div', parse[i]) > 0) then
        isExtractGenres := False;
      if isExtractGenres and (Pos('Genre:', parse[i]) = 0) and
        (Pos('<', parse[i]) = 0) then
          mangaInfo.genres := mangaInfo.genres + parse[i];

      //description
      if Pos('Description:', parse[i]) > 0 then
        mangaInfo.summary := BreaksString(CommonStringFilter(parse[i + 4]));

      //chapters
      if (Pos('<a', parse[i]) > 0) and (Pos('/chapter-', parse[i]) > 0) then
      begin
        Inc(mangaInfo.numChapter);
        s := GetVal(parse[i], 'href');
        if regx.Exec(s) then
          s := regx.Replace(s, '', False);
        mangaInfo.chapterLinks.Add(s);
        mangaInfo.chapterName.Add(CommonStringFilter(parse[i + 1]));
      end;
    end;
    regx.Free;

    //remove duplicate links
    RemoveDuplicateStrings([mangaInfo.chapterLinks, mangaInfo.chapterName]);
    //inverts
    InvertStrings(mangaInfo.chapterLinks);
    InvertStrings(mangaInfo.chapterName);

    Parser.Free;
    Source.Free;
    Result := NO_ERROR;
  end;
