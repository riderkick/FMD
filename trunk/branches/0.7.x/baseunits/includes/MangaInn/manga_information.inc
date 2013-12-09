function   GetMangaInnInfoFromURL: Byte;
var
  i, j: Cardinal;
  isExtractChapters: Boolean = FALSE;
begin
  mangaInfo.url:= WebsiteRoots[MANGAINN_ID,1] + URL;
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
  mangaInfo.website:= MANGAINN_NAME;

  // using parser
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString(parse.Strings[i+1], ' - Read ', ' Online For Free');

    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
      if Pos('/mangas/logos/', parse.Strings[i]) <> 0 then
        mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

      // get summary
    if (Pos('Summary', parse.Strings[i])) <> 0 then
    begin
      j:= i;
      while Pos('</td>', parse.Strings[j]) = 0 do
      begin
        Inc(j);
        if (GetTagName(parse.Strings[j]) = 'span') AND
           (GetTagAttribute(parse.Strings[j], 'class=')<>'') then
        begin
          parse.Strings[j+1]:= StringFilter(parse.Strings[j+1]);
          parse.Strings[j+1]:= StringReplace(parse.Strings[j+1], #10, '\n', [rfReplaceAll]);
          parse.Strings[j+1]:= StringReplace(parse.Strings[j+1], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j+1];
        end;
      end;
    end;

    // get chapter name and links
    if isExtractChapters then
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('http://www.mangainn.com/manga/chapter', parse.Strings[i])<>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(CorrectURL(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAINN_ID,1], '', [rfReplaceAll])));
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
        parse.Strings[i+2]:= TrimLeft(parse.Strings[i+2]);
        parse.Strings[i+2]:= TrimRight(parse.Strings[i+2]);
        parse.Strings[i+4]:= TrimLeft(parse.Strings[i+4]);
        parse.Strings[i+4]:= TrimRight(parse.Strings[i+4]);
        mangaInfo.chapterName.Add(StringFilter(RemoveSymbols(parse.Strings[i+2] + parse.Strings[i+4])));
      end;

    // get authors
    if (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+4]));

    // get artists
    if (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(parse.Strings[i+4]));

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenres[j]), LowerCase(parse.Strings[i+4]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenres[j]+', ');
    end;

    // get status
    if (Pos('Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+3])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
      if Pos('Completed', parse.Strings[i+3])<>0 then
        mangaInfo.status:= '0';  // completed
    end;

    if Pos('Chapter Name', parse.Strings[i]) <> 0 then
      if GetTagAttribute(parse.Strings[i-1], 'class=') <> '' then
        isExtractChapters:= TRUE;
  end;
  Result:= NO_ERROR;
end;
