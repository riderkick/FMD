function   GetFakkuInfoFromURL: Byte;
var
  s: String;
  isExtractChapters: Boolean = TRUE;
  isExtractGenres  : Boolean = FALSE;
  isExtractSummary : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[FAKKU_ID,1] + URL;
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
  mangaInfo.website:= FAKKU_NAME;

  mangaInfo.status:= '0';
  mangaInfo.genres:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos(' title="">', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(parse.Strings[i+5]));

    // get cover
    if GetTagName(parse.Strings[i]) = 'img' then
      if (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='cover') then
      begin
        s:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
        if Pos('http://', s) = 0 then
          mangaInfo.coverLink:= WebsiteRoots[FAKKU_ID,1] + s
        else
          mangaInfo.coverLink:= s;
      end;

    // get summary
    if isExtractSummary then
    begin
      s:= parse.Strings[i];
      if (Length(s) > 0) AND (s[1] <> '<') then
      begin
        parse.Strings[i]:= StringFilter(parse.Strings[i]);
        parse.Strings[i]:= StringReplace(parse.Strings[i], #10, '\n', [rfReplaceAll]);
        parse.Strings[i]:= StringReplace(parse.Strings[i], #13, '\r', [rfReplaceAll]);
        mangaInfo.summary:= mangaInfo.summary+parse.Strings[i]+'\n\r';
      end
      else
      if (GetTagName(parse.Strings[i]) = '/div') then
        isExtractSummary:= FALSE;
    end;

    if Pos('Language:', parse.Strings[i]) <> 0 then
    begin
      if Pos('english', parse.Strings[i+1]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + 'English, '
      else
      if Pos('japanese', parse.Strings[i+1]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + 'Japanese, '
    end;

    if (Pos('Description:', parse.Strings[i])) <> 0 then
    begin
      isExtractSummary:= TRUE;
      mangaInfo.summary:= '';
    end;

    // get chapter name and links
    if isExtractChapters then
    begin
      if (Pos('/read"', parse.Strings[i])>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[FAKKU_ID,1], '', [rfReplaceAll]));
        mangaInfo.chapterName.Add(mangaInfo.title);
        isExtractChapters:= FALSE;
      end ;
    end;

    // get authors
    if (Pos('Series:', parse.Strings[i])<>0) then
    begin
      mangaInfo.authors:= parse.Strings[i+2];
      mangaInfo.genres:= mangaInfo.genres + mangaInfo.authors + ', ';
    end;

    // get artists
    if (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= parse.Strings[i+2];

    // get genres
    if (Pos('Tags:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if GetTagName(parse.Strings[i]) = 'a' then
        mangaInfo.genres:= TrimLeft(TrimRight(mangaInfo.genres+parse.Strings[i+1]))+', '
      else
      if (GetTagName(parse.Strings[i]) = '/div') then
        isExtractGenres:= FALSE;
    end;

    // get status - Fakku doesnt have status, in fact, it's always 'completed'
  end;
  Result:= NO_ERROR;
end;
