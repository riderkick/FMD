function   GetBlogTruyenInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[BLOGTRUYEN_ID,1] + URL;
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
  mangaInfo.website:= WebsiteRoots[BLOGTRUYEN_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('img style=''max-width:500px;', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('sơ lược', parse.Strings[i]) <> 0) AND
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
          mangaInfo.summary:= mangaInfo.summary + parse.Strings[j];
          break;
        end;
        Inc(j);
      end;
      isExtractSummary:= FALSE;
    end;

    // get title
    if (Pos(' - Blog truyện tranh online', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i], '~!@', ' - Blog truyện tranh online'))));

    if (NOT isExtractChapter) AND (Pos('Tổng hợp (', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (isExtractChapter) AND
       (Pos('title=''Đọc truyện', parse.Strings[i])>0) AND
       (Pos('/truyen/', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    if (isExtractChapter) AND
       (Pos('để xem truyện nhanh hơn', parse.Strings[i])>0) then
      isExtractChapter:= FALSE;

    // get authors
    if  (i+8<parse.Count) AND (Pos('Tác giả:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+3]));

    // get artists
    //if (i+1<parse.Count) AND (Pos('/search/artist/', parse.Strings[i])<>0) then
    //  mangaInfo.artists:= TrimLeft(parse.Strings[i+1]);

    // get genres
    if (Pos('Thể loại:', parse.Strings[i])<>0) AND (Pos('truyện tranh', parse.Strings[i])=0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      s:= TrimLeft(TrimRight(parse.Strings[i]));
      if Pos('Đăng bởi:', s) <> 0 then
        isExtractGenres:= FALSE
      else
      if (s<>'') AND (s[1] <> '<') AND (s[1] <> ',') AND (Pos('Thể loại:', s)=0) then
        mangaInfo.genres:= mangaInfo.genres + s + ', ';
    end;

    // get status
    if (i+12<parse.Count) AND (Pos('Trạng thái:', parse.Strings[i])<>0) then
    begin
      if (Pos('Đang tiến hành', parse.Strings[i+2])<>0) OR
         (Pos('Tạm ngưng', parse.Strings[i+2])<>0) then
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
  Result:= NO_ERROR;
end;
