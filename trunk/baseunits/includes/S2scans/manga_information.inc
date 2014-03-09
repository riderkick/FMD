function   GetS2scanInfoFromURL: Byte;
var
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = TRUE;
  s: String;
  i, j: Cardinal;
 // stream: TStringStream;
 // HTTP: THTTPSend;
  R   : Boolean;
begin
  mangaInfo.url:= WebsiteRoots[S2SCAN_ID,1] + URL;
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
  mangaInfo.website:= WebsiteRoots[S2SCAN_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('class="thumbnail"', parse.Strings[i-2])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get title
    if (Pos('h1 class="title"', parse.Strings[i])<>0) AND
       (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+2])));

    if (isExtractChapter) AND
       (Pos('class="element"', parse.Strings[i]) > 0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetString(parse.Strings[i+3], 'href="', '"'), WebsiteRoots[S2SCAN_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+4])));
      mangaInfo.chapterName.Add(StringFilter(StringFilter(HTMLEntitiesFilter(s))));
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
