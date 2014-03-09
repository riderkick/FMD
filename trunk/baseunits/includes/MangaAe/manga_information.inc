function   GetMangaAeInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAAE_ID,1] + URL;
  if NOT GetPage(TObject(source), EncodeURL(mangaInfo.url), Reconnect) then
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
  mangaInfo.website:= WebsiteRoots[MANGAAE_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('id="photo_1"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'src=')));

    // get summary
    if (Pos('<p class="sumry">', parse.Strings[i]) <> 0) then
    begin
      j:= i+1;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(TrimLeft(parse.Strings[j])));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
          break;
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (mangaInfo.title = '') AND
       (Pos(' -  مانجا العرب ● Manga Al-arab', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i], '~!@', ' -  مانجا العرب ● Manga Al-arab'))));

    if (NOT isExtractChapter) AND
       (Pos('class="mangachapters"', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos(WebsiteRoots[MANGAAE_ID,1], parse.Strings[i])>0) AND
       (Pos('<li>', parse.Strings[i-2])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAAE_ID,1], '', []);
      s:= StringReplace(s+'~!@', '/1/~!@', '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+2]))) + ' ' + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('class="ads"', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get authors
    if (i+6<parse.Count) AND
       (Pos('المؤلف', parse.Strings[i])<>0) AND
       (Pos('</h2>', parse.Strings[i+1])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+4]));

    // get genres
    if (Pos('/mn_name/', parse.Strings[i])<>0) then
      mangaInfo.genres:= mangaInfo.genres + parse.Strings[i+1] + ', ';

    // get status
    if (i+3<parse.Count) AND
       (Pos('حالة الترجمة', parse.Strings[i])<>0) AND
       (Pos('</h2>', parse.Strings[i+1])<>0) then
    begin
      if (Pos('غير مكتملة', parse.Strings[i+3])<>0) then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;

  // remove duplicate links
  i:= 0;
  while i < mangaInfo.chapterLinks.Count do
  begin
    j:= i+1;
    while j < mangaInfo.chapterLinks.Count do
    begin
      if mangaInfo.chapterLinks.Strings[i] = mangaInfo.chapterLinks.Strings[j] then
      begin
        mangaInfo.chapterLinks.Delete(j);
        mangaInfo.chapterName.Delete(j);
        Dec(mangaInfo.numChapter);
      end
      else
        Inc(j);
    end;
    Inc(i);
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterLinks.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterLinks.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;

  Result:= NO_ERROR;
end;
