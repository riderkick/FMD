function   GetMangaParkInfoFromURL: Byte;
var
  s: String;
  isExtractChapter: Boolean = FALSE;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j, volumeCount: Cardinal;
begin
  volumeCount:= 0;
  mangaInfo.url:= WebsiteRoots[MANGAPARK_ID,1] + URL;// + '&confirm=yes';
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
  mangaInfo.website:= MANGAPARK_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i]) > 0) then
      mangaInfo.title:= StringFilter(TrimLeft(TrimRight(GetString(parse.Strings[i+1], ' Manga - Read ', ' Online For '))));

    // get cover
    if (GetTagName(parse.Strings[i]) = 'meta') AND
       (Pos('property="og:image"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'content=')));

    // get summary
    if (Pos('<h2>', parse.Strings[i]) <> 0) AND
       (Pos('Summary', parse.Strings[i+1]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+3;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= HTMLEntitiesFilter(StringFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= parse.Strings[j];
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

      // allow get chapter name and links
    if (Pos(URL, parse.Strings[i])>0) AND
       (Pos('target="_blank"', parse.Strings[i])>0) then
      isExtractChapter:= TRUE;

      // get chapter name and links
    if (isExtractChapter) AND
       (Pos(URL, parse.Strings[i])>0) AND
       (Pos('target="_blank"', parse.Strings[i])>0) AND
       (parse.Strings[i+1] <> '1') AND
       (parse.Strings[i+1] <> '3') AND
       (parse.Strings[i+1] <> '6') AND
       (parse.Strings[i+1] <> '10') AND
       (parse.Strings[i+1] <> 'All'){ AND
       (Pos('Vol.', parse.Strings[i+3]) = 0) }then
    begin
      if Pos('Vol.', parse.Strings[i+3]) <> 0 then
        Inc(volumeCount);
      Inc(mangaInfo.numChapter);
      s:= EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
      Delete(s, Length(s), 1);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3]))) + RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+6])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+6]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+6]);

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('/genre/', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</td>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  // check and delete duplicate links
  if (volumeCount < mangaInfo.numChapter) AND (volumeCount > 0) then
  begin
    i:= 0;
    while i < mangainfo.ChapterName.Count do
    begin
      if Pos('Vol.', mangainfo.ChapterName.Strings[i]) = 0 then
      begin
        mangainfo.ChapterName.Delete(i);
        mangainfo.ChapterLinks.Delete(i);
        Dec(mangaInfo.numChapter);
      end
      else
        Inc(i);
    end;
  end;
  {while i < mangainfo.ChapterName.Count-1 do
  begin
    j:= 0;
    if Pos('Vol.', mangainfo.ChapterName.Strings[i]) = 0 then
    while j < mangainfo.ChapterName.Count do
    begin
      if i=j then
      begin
        Inc(j);
        continue;
      end;
     // if Pos(GetString(' '+mangainfo.ChapterName.Strings[i]+' ', ' ', ' '), mangainfo.ChapterName.Strings[j]) <> 0 then
      if GetString(mangainfo.ChapterName.Strings[i]+' ', 'Ch.', ' ') =
         GetString(mangainfo.ChapterName.Strings[j]+' ', 'Ch.', ' ') then
      begin
        s:= GetString(' '+mangainfo.ChapterName.Strings[i]+' ', ' ', ' ');
        mangainfo.ChapterName.Delete(j);
        mangainfo.ChapterLinks.Delete(j);
        Dec(mangaInfo.numChapter);
        break;
      end
      else
        Inc(j);
    end;
    Inc(i);
  end;}

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
