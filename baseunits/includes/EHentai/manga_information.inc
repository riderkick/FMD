  function GetEHentaiInfoFromURL: Byte;
  var
    i: Cardinal;
    s: String;
    rgex: TRegExpr;
    ava: Boolean = False;
    cw: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[EHENTAI_ID, 0];
    mangaInfo.url := FillMangaSiteHost(EHENTAI_ID, URL);
    mangaInfo.url := ReplaceRegExpr('\?nw=.+$', mangaInfo.url, '', False);
    if not ExecRegExpr('/$', mangaInfo.url) then
      mangaInfo.url := mangaInfo.url + '/';
    s := mangaInfo.url;
    mangaInfo.summary := '';
    if not GetPage(TObject(Source), s, Reconnect) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    if Source.Count > 0 then
    begin
      // check if banned
      if Source.Count = 1 then
      begin
        //if mangaInfo.genres = '' then
        mangaInfo.summary := Source.Text;
        Result := NO_ERROR;
        Source.Free;
        Exit;
      end;

      // check if content warning
      cw := False;
      for i := 0 to Source.Count - 1 do
      begin
        if Pos('>Content Warning</', Source[i]) > 0 then
        begin
          cw := True;
          Break;
        end;
      end;
      if cw then
      begin
        s := s + '?nw=session';
        //s := s + '?nw=always';
        if not GetPage(TObject(Source), s, Reconnect) then
        begin
          Result := NET_PROBLEM;
          Source.Free;
          Exit;
        end;
      end;
    end;

    // parsing the HTML source
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    Source.Free;

    // using parser (cover link, summary, chapter name and link)
    if parse.Count < 1 then
    begin
      Result := INFORMATION_NOT_FOUND;
      Exit;
    end
    else
    begin
      // check availability
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('id="gn"', parse.Strings[i]) > 0 then
        begin
          ava := True;
          Break;
        end;
      end;
      // not available
      if not ava then
      begin
        for i := 0 to parse.Count - 1 do
          if Pos('<p>', parse.Strings[i]) > 0 then
          begin
            mangaInfo.summary := Trim(parse.Strings[i + 1]);
            Break;
          end;
        mangaInfo.status := '0';
        parse.Clear;
        Result := NO_ERROR;
        Exit;
      end;
    end;

    mangaInfo.title := '';
    mangaInfo.authors :=' ';
    mangaInfo.artists := '';
    mangaInfo.genres := '';

    if parse.Count > 1 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        // get cover
        if (mangaInfo.coverLink = '') and
          (Pos('id="gd1"', parse.Strings[i]) > 0) and
          (Pos('<img', parse.Strings[i + 1]) > 0) then
          mangaInfo.coverLink :=
            GetAttributeValue(GetTagAttribute(parse.Strings[i + 1], 'src='));

        // get title
        if (Pos('id="gn"', parse.Strings[i]) <> 0) then
        begin
          s := Trim(HTMLEntitiesFilter(StringFilter(parse.Strings[i + 1])));
          if s <> '' then
            mangaInfo.title := s;
        end;

        // get artists
        if (Pos('id="td_artist:', parse.Strings[i]) > 0) then
          if mangaInfo.artists = '' then
            mangaInfo.artists := parse.Strings[i + 2]
          else
            mangaInfo.artists := mangaInfo.artists + ', ' + parse.Strings[i + 2];

        // get misc genres  (language, parodi, character, etc)
        if (Pos('id="td_', parse.Strings[i]) > 0) and
          (Pos('id="td_artist:', parse.Strings[i]) = 0) then  //exclude artist
          if mangaInfo.genres = '' then
            mangaInfo.genres := parse.Strings[i + 2]
          else
            mangaInfo.genres := mangaInfo.genres + ', ' + parse.Strings[i + 2];
      end;
    end;
    parse.Clear;
    mangaInfo.numChapter := 1;
    mangaInfo.chapterName.Add(mangaInfo.title);
    mangaInfo.chapterLinks.Add(mangaInfo.url);

    //status
    rgex := TRegExpr.Create;
    rgex.ModifierI := True;
    rgex.Expression := '[\[\(\{](wip|ongoing)[\}\)\]]';
    if rgex.Exec(LowerCase(mangaInfo.title)) then
      mangaInfo.status := '1'
    else
      mangaInfo.status := '0';
    rgex.Free;

    Result := NO_ERROR;
  end;
