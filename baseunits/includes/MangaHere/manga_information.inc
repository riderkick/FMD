function   GetMangaHereInfoFromURL: Byte;
var
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAHERE_ID,1] + URL;
  if NOT GetPage(TObject(source), mangaInfo.url, Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= WebsiteRoots[MANGAHERE_ID,0];

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'Manga - Read ', ' Online at ');

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='img') then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src'));

      // get summary
    if (Pos('id="show"', parse.Strings[i])) <> 0 then
    begin
      parse.Strings[i+1]:= StringFilter(parse.Strings[i+1]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '\n', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '\r', [rfReplaceAll]);
      mangaInfo.summary:= parse.Strings[i+1];
    end;

      // get chapter name and links
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='color_0077') AND
       (Pos('http://www.mangahere.com/manga/', GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')))<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAHERE_ID,1], '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      s:= StringFilter(TrimLeft(TrimRight(RemoveSymbols(parse.Strings[i+6]))));
      if (s <> '') AND ((s[1] = '<') OR (s = 'span')) then
        s:= ''
      else
        s:= ' ' + s;
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1]))) + s);
    end;

    // get authors
    if (Pos('Author(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= parse.Strings[i+3];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+3];

    // get genres
    if (Pos('Genre(s):', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenres[j]), LowerCase(parse.Strings[i+2]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenres[j]+', ');
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
      if Pos('Completed', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;

  // Delete 'latest' chapter because it isnt exist
  if (mangaInfo.status = '1') AND (mangainfo.ChapterName.Count > 0) then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.chapterLinks.Delete(mangainfo.chapterLinks.Count-1);
  end;
  Result:= NO_ERROR;
end;
