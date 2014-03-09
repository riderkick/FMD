function   GetPururinInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  if Pos('http://', URL) = 0 then
    mangaInfo.url:= WebsiteRoots[PURURIN_ID,1] + URL
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
  source.Free;
  mangaInfo.website:= WebsiteRoots[PURURIN_ID,0];
  mangaInfo.title:= '';
  mangaInfo.status:= '';
  mangaInfo.coverLink:= '';
  mangaInfo.summary:= '';
  mangaInfo.authors:= '';
  mangaInfo.artists:= '';
  mangaInfo.genres:= '';
  
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  //for i:=0 to parse.Count-1 do
  for i:=0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('class="gallery-cover', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(WebsiteRoots[PURURIN_ID,1] + GetAttributeValue(GetTagAttribute(parse.Strings[i+4], 'src=')));

    // get title
    if (Pos('itemprop="title"', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= Trim(HTMLEntitiesFilter(parse.Strings[i+1]));

    if (NOT isExtractChapter) AND (Pos('loader block hidden', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    if (isExtractChapter) AND
       (Pos('class="gallery-cover', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= WebsiteRoots[PURURIN_ID,1] + GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(GetAttributeValue(GetTagAttribute(parse.Strings[i+4], 'alt=')))));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('Artist', parse.Strings[i])>0) then
      isExtractChapter:= FALSE; //bermasalah

    // get summary
    if (Pos('class="gallery-description', parse.Strings[i]) <> 0) then
    begin
      j:= i+1;
      while (j<parse.Count) AND (Pos('</div>', parse.Strings[j])=0) do
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
    if (Pos('Circle', parse.Strings[i])<>0) AND
       (Pos('</td>', parse.Strings[i+1])<>0) then
      mangaInfo.authors:= parse.Strings[i+6];

    // get artists
    if (Pos('Artist', parse.Strings[i])<>0) AND
       (Pos('</td>', parse.Strings[i+1])<>0) then
      mangaInfo.artists:= parse.Strings[i+6];

    // get parody
    if (Pos('Parody', parse.Strings[i])<>0) AND
       (Pos('</td>', parse.Strings[i+1])<>0) then
      mangaInfo.genres:= mangaInfo.genres + parse.Strings[i+6] + ', ';

    // get genres
    if (Pos('Contents', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
    end;

    if isExtractGenres then
    begin
      if Pos('/browse/', parse.Strings[i]) <> 0 then
        mangaInfo.genres:= mangaInfo.genres + Trim(parse.Strings[i+6]) + ', ';
      if Pos('</ul>', parse.Strings[i]) <> 0 then
      begin
        mangaInfo.genres:= StringReplace(mangaInfo.genres, '</tr>, ', '', []);
        isExtractGenres:= FALSE;
      end;
    end;
  end;
  Result:= NO_ERROR;
end;
