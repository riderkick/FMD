function   GetAnimeExtremistInfoFromURL: Byte;
var
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[ANIMEEXTREMIST_ID,1] + URL;
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
  mangaInfo.website:= WebsiteRoots[ANIMEEXTREMIST_ID,0];
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos(' Manga - Animextremist', parse.Strings[i]) > 0) then
      mangaInfo.title:= GetString('~!@'+parse.Strings[i], '~!@', ' Manga - Animextremist');

    // get cover link
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'img') then
      if Pos('src="../', parse.Strings[i])>0 then
        mangaInfo.coverLink:= WebsiteRoots[ANIMEEXTREMIST_ID,1] + GetString(parse.Strings[i], 'src="..', '"');

      // get summary
    if (Pos('align="justify" class="style33"', parse.Strings[i])) <> 0 then
    begin
      j:= i+1;
      mangaInfo.summary:= '';
      while (Pos('<td height', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if (s<>'') AND (s[1] <> '<') then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j] + '\n\r';
        end;
        Inc(j);
      end;
    end;

      // get chapter name and links
    if (Pos('/mangas-online/', parse.Strings[i])<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[ANIMEEXTREMIST_ID,1], '', [rfReplaceAll]));
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
      parse.Strings[i+1]:= StringReplace(parse.Strings[i+1], #13, '', [rfReplaceAll]);
      parse.Strings[i+1]:= TrimLeft(parse.Strings[i+1]);
      parse.Strings[i+1]:= TrimRight(parse.Strings[i+1]);
      mangaInfo.chapterName.Add(HTMLEntitiesFilter(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+1])))));
    end;

   { // get authors
    if (Pos('Autor(s):', parse.Strings[i])<>0) then
      mangaInfo.authors:= parse.Strings[i+3];

    // get artists
    if (Pos('Artist(s):', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+3]; }

    // get genres
    if (Pos('ord=genero&id', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= mangaInfo.genres+(HTMLEntitiesFilter(TrimLeft(TrimRight(parse.Strings[i+1])))+', ');
    end;

    // get status
    if (Pos('class="manga_estado"', parse.Strings[i])<>0) then
    begin
      if Pos('Completo', parse.Strings[i+3])<>0 then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
    end;
  end;

  Result:= NO_ERROR;
end;
