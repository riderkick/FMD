function   GetAnimeAInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractGenres: Boolean = FALSE;
begin
  mangaInfo.url:= WebsiteRoots[ANIMEA_ID,1] + URL + ANIMEA_SKIP;
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
  mangaInfo.website:= WebsiteRoots[ANIMEA_ID,0];

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], 'Manga - Read ', ' Manga Scans');

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='manga_img_big') then
        mangaInfo.coverLink:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src'));

    // get authors
    if (Pos('Author(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimRight(TrimLeft(parse.Strings[i+3]));

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimRight(TrimLeft(parse.Strings[i+2]));

    // get genres
    if (Pos('Genre(s):', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) <> 'a' then
        for j:= 0 to 37 do
          if Pos(LowerCase(defaultGenres[j]), LowerCase(parse.Strings[i]))<>0 then
            mangaInfo.genres:= mangaInfo.genres+(defaultGenres[j]+', ');
      if Pos('</li>', parse.Strings[i]) > 0 then
        isExtractGenres:= FALSE;
    end;

      // get summary
    if Pos('Upload a chapter', parse.Strings[i]) > 0 then
    begin
      j:= i+8;
      mangaInfo.summary:= '';
      while (j<parse.Count-4) AND (Pos('</p>', parse.Strings[j]) = 0) do
      begin
        mangaInfo.summary:= StringFilter(mangaInfo.summary+parse.Strings[j]);
        Inc(j);
      end;
      mangaInfo.summary:= StringFilter(mangaInfo.summary);
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;

      // get chapter name and links
    if (GetTagName(parse.Strings[i]) = 'a') AND
       (GetTagAttribute(parse.Strings[i], 'href=')<>'') AND
       (GetTagAttribute(parse.Strings[i], 'id=')<>'') AND
       (GetTagAttribute(parse.Strings[i], 'title=')<>'') then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href')));
      mangaInfo.chapterName.Add(StringFilter(TrimRight(TrimLeft(RemoveSymbols(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title'))+' '+parse.Strings[i+3])))));
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
  Result:= NO_ERROR;
end;
