function GetMangaFoxInfoFromURL: Byte;
var
  i, j: Integer;
  s: String;
  isAvailable: Boolean = True;
begin
  mangaInfo.website := WebsiteRoots[MANGAFOX_ID, 0];
  mangaInfo.url := FillMangaSiteHost(MANGAFOX_ID, URL);
  if not GetPage(TObject(Source), mangaInfo.url, Reconnect) then
  begin
    Result := NET_PROBLEM;
    Source.Free;
    Exit;
  end;

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

  if parse.Count = 0 then
    Exit;

  for i := 0 to parse.Count - 1 do
  begin
    //title
    if mangaInfo.title = '' then
      if GetVal(parse[i], 'property') = 'og:title' then
        mangaInfo.title := CommonStringFilter(ReplaceRegExpr(
          '^(.+)\s(Manga|Manhwa)$', GetVal(parse[i], 'content'), '$1', True));

    //cover
    if (GetTagName(parse[i]) = 'div') and
      (GetVal(parse[i], 'class') = 'cover') then
      mangaInfo.coverLink := CorrectURL(GetVal(parse[i + 2], 'src'));

    //author
    if Pos('/search/author/', parse[i]) > 0 then
      mangaInfo.authors := Trim(parse[i + 1]);

    //artist
    if Pos('/search/artist/', parse[i]) > 0 then
      mangaInfo.artists := Trim(parse[i + 1]);

    //genres
    if (GetTagName(parse[i]) = 'td') and (GetVal(parse[i], 'valign') = 'top') then
      if Pos('/genres/', parse[i + 2]) > 0 then
      begin
        mangaInfo.genres := '';
        for j := i + 1 to parse.Count - 1 do
        begin
          if GetTagName(parse[j]) = '/td' then
            Break
          else
          if Pos('<', parse[j]) = 0 then
            mangaInfo.genres := mangaInfo.genres + parse[j];
        end;
      end;

    //summary
    if (GetTagName(parse[i]) = 'p') and (GetVal(parse[i], 'class') = 'summary') then
    begin
      mangaInfo.summary := '';
      for j := i + 1 to parse.Count - 1 do
      begin
        if GetTagName(parse[j]) = '/p' then
          Break
        else
        if Pos('<', parse[j]) = 0 then
          mangaInfo.summary := mangaInfo.summary + #13#10 + CommonStringFilter(parse[j]);
      end;
    end;

    //status
    if GetTagName(parse[i]) = 'h5' then
      if UpperCase(Trim(parse[i + 1])) = 'STATUS:' then
      begin
        if Pos('ONGOING', UpperCase(parse[i + 5])) > 0 then
          mangaInfo.status := '1'   // ongoing
        else
          mangaInfo.status := '0';  // completed
      end;


    //check if it's licensed
    if GetVal(parse[i], 'class') = 'warning' then
      if Pos('it is not available in', parse[i + 1]) > 0 then
      begin
        isAvailable := False;
        mangaInfo.numChapter := 0;
        mangaInfo.chapterName.Clear;
        mangaInfo.chapterLinks.Clear;
      end;

    //chapters
    if isAvailable then
      if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'tips') then
      begin
        Inc(mangaInfo.numChapter);
        s := Trim(parse[i + 1]) + ' ' + Trim(parse[i + 5]);
        mangaInfo.chapterName.Add(CommonStringFilter(s));
        s := GetVal(parse[i], 'href');
        if RightStr(s, 6) = '1.html' then
          SetLength(s, Length(s) - 6);
        mangaInfo.chapterLinks.Add(s);
      end;
  end;

  InvertStrings([mangaInfo.chapterName, mangaInfo.chapterLinks]);
  Result := NO_ERROR;
end;
