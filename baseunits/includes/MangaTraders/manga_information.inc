function   GetMangaTradersInfoFromURL: Byte;
var
  pages           : Cardinal = 1;
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j, k, tmp: Cardinal;

  procedure GetChapterNamesAndLinks(const apos: Integer);
  begin
    // get chapter name and links
    if (Pos('/view/file/', parse.Strings[apos])>0) AND
       (Pos('"linkFoot', parse.Strings[apos]) = 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetString(parse.Strings[apos], 'href="', '"');
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[apos+1])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
    end;
  end;

begin
  mangaInfo.url:= WebsiteRoots[MANGATRADERS_ID,1] + URL;// + '&confirm=yes';
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

  mangaInfo.website:= MANGATRADERS_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then
  begin
    Parser.Free;
    source.Free;
    exit;
  end;
  for i:= 0 to parse.Count-1 do
  begin
    // page counter (>1 = multi-page)
    if (Pos('/page/', parse.Strings[i]) > 0) then
    begin
      s:= parse.Strings[i];
      tmp:= StrToInt(TrimLeft(TrimRight(parse.Strings[i+1])));
      if tmp > pages then
        pages:= tmp;
    end;

    // get cover
    if (GetTagName(parse.Strings[i]) = 'div') AND
       (Pos('"seriesInfo_image"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= WebsiteRoots[MANGATRADERS_ID,1] + CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src=')));

    // get summary
    if (Pos('id="summary"', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+9;
      while (j<parse.Count) AND (Pos('</p>', parse.Strings[j])=0) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
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

    // get title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(StringFilter(GetString(parse.Strings[i+1]+'~!@', 'Manga Traders - ', '~!@')));

    GetChapterNamesAndLinks(i);

    // get authors
    if  (i+4<parse.Count) AND (Pos('Author(s)', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Artist(s)', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]);

    // get genres
    if (Pos('Genre(s)', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('/all', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Scanslation Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;
  if pages > 1 then
  begin
    for k:= 2 to pages do
    begin
      source.Clear;
      if NOT GetPage(TObject(source), WebsiteRoots[MANGATRADERS_ID,1] + URL + '/page/' + IntToStr(k) + '/', Reconnect) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
      Parser.Raw:= PChar(source.Text);
      Parser.Exec;
      if parse.Count=0 then
      begin
        Parser.Free;
        source.Free;
        exit;
      end;
     // mangaInfo.chapterLinks.Clear;
     // mangaInfo.chapterName.Clear;
     // mangaInfo.numChapter:= 0;
      for i:= 0 to parse.Count-1 do
      begin
        GetChapterNamesAndLinks(i);
      end;
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

  Parser.Free;
  source.Free;
  Result:= NO_ERROR;
end;
