  function GetUnixMangaInfoFromURL: Byte;
  var
    i: Integer;
    isExtractChapters: Boolean = False;
  begin
    mangaInfo.website := WebsiteRoots[UNIXMANGA_ID, 0];
    mangaInfo.url := FillMangaSiteHost(UNIXMANGA_ID, AURL);
    if not GetPage(TObject(Source), mangaInfo.url, AReconnect) then
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
    Parser.Free;
	Source.Free;

    if parse.Count = 0 then
      Exit;

    for i := 0 to parse.Count - 1 do
    begin
      //chapters
      if (Pos('<table', parse[i]) > 0) and (Pos('class="snif"', parse[i]) > 0) then
        isExtractChapters := True;
      if isExtractChapters and (Pos('</table', parse[i]) > 0) then
        isExtractChapters := False;
      if (i + 1 < parse.Count - 1) then
        if isExtractChapters and (Pos('/onlinereading/', parse[i]) > 0) and
          (Pos('title=', parse[i]) > 0) and (Pos('<a ', parse[i]) > 0) and
          (Pos('/index.php', parse[i]) = 0) then
        begin
          Inc(mangaInfo.numChapter);
          mangaInfo.chapterLinks.Add(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[UNIXMANGA_ID, 1], '', [rfIgnoreCase]));
          mangaInfo.chapterName.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        end;
    end;

    // invert chapters
    if mangaInfo.chapterName.Count > 1 then
    begin
      InvertStrings(mangaInfo.chapterName);
      InvertStrings(mangaInfo.chapterLinks);
    end;
    Result := NO_ERROR;
  end;
