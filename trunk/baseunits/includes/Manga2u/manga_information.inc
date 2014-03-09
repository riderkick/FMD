function   GetManga2uInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i,j,n: Cardinal;
  numberOfPage: Cardinal = 1;
  
    procedure ExtractChapter;
  begin
    if (NOT isExtractChapter) AND (Pos('name="IL_IN_TAG', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('class="val', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
	  s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-2], 'href')), WebsiteRoots[MANGA2U_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('sct_wid_bot', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;
  end;
  
begin
  if Pos('http://', URL) = 0 then
    mangaInfo.url:= WebsiteRoots[MANGA2U_ID,1] + URL
  else
    mangaInfo.url:= URL;
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
  mangaInfo.website:= WebsiteRoots[MANGA2U_ID,0];
  mangaInfo.status:= '';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    if Pos('class="sel', parse.Strings[i])>0 then
      numberOfPage:= StrToInt(GetString(parse.Strings[i+10], '-list/', '/">'));
	  
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('class="cvr_ara', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'src=')));

    // get title
    if (Pos('class="cvr_ara', parse.Strings[i])>0) then
		mangaInfo.title:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'alt='));

    ExtractChapter;

    // get summary
    if (Pos('Author', parse.Strings[i]) <> 0) then
    begin
      j:= i-6;
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

    // get authors
    if (Pos('Author', parse.Strings[i])<>0) then
      mangaInfo.authors:= StringReplace(TrimLeft(StringFilter(parse.Strings[i+2])), ': ', '', []);

    // get artists
    if (Pos('Author', parse.Strings[i])<>0) then
      mangaInfo.artists:= StringReplace(TrimLeft(StringFilter(parse.Strings[i+2])), ': ', '', []);

// get genres
    if (Pos('Category', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('list/category/', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Status', parse.Strings[i])<>0) then
    begin
      if Pos('Ongoing', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  if numberOfPage > 1 then
  begin
    for n:= 2 to numberOfPage do
    begin
      source.Clear;
      s:= mangaInfo.url + 'chapter-list/' + IntToStr(n);
      if NOT GetPage(TObject(source), mangaInfo.url + 'chapter-list/' + IntToStr(n), Reconnect) then
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
      isExtractChapter:= FALSE;
      if parse.Count=0 then exit;
        for i:= 0 to parse.Count-1 do
          ExtractChapter;
    end;
  end;
  source.Free;

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
