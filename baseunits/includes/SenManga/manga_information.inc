function   GetSenMangaInfoFromURL: Byte;
var
  s: String;
  isRepeated      : Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[SENMANGA_ID,1] + URL;
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
  Parser.SlowExec;

  Parser.Free;
  source.Free;

  mangaInfo.website:= WebsiteRoots[SENMANGA_ID,0];
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  isExtractChapter:= FALSE;

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('Series Cover:', parse.Strings[i])>0) then
      mangaInfo.coverLink:= WebsiteRoots[SENMANGA_ID,1] + CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'src=')));

    // get chapter name and links
    if (Pos('class="chapter"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

    // get summary
    if (isExtractSummary) AND
       (Pos('Summary:', parse.Strings[i]) <> 0) AND
       (Pos('<strong>', parse.Strings[i-1]) <> 0) then
    begin
      j:= i+7;
      while (j<parse.Count) AND (Pos('</table>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if (s[1] <> '<') then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get manga name
    if (mangaInfo.title = '') AND (Pos('<title>', parse.Strings[i])>0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString('~!@'+parse.Strings[i+1], '~!@', ' | Raw ')));

    if (isExtractChapter) AND
       ((Pos('''even'' style=''padding', parse.Strings[i])>0) OR
        (Pos('''odd'' style=''padding', parse.Strings[i])>0)) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+5], 'href=')), WebsiteRoots[SENMANGA_ID,1], '', []);
      SetLength(s, Length(s)-1);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+6])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get status
    if (i+4<parse.Count) AND
       (Pos('Status:', parse.Strings[i])<>0) AND
       (Pos('<strong>', parse.Strings[i-1])<>0) then
    begin
      if (Pos('Complete', parse.Strings[i+4])<>0)  then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
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
  Result:= NO_ERROR;
end;
