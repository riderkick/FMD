function   GetVnSharingInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[VNSHARING_ID,1] + URL + '&confirm=yes';
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
  mangaInfo.website:= WebsiteRoots[VNSHARING_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get manga title
    if (mangaInfo.title = '') AND
       (Pos('name="title"', parse.Strings[i]) > 0) then
      mangaInfo.title:= TrimLeft(TrimRight(GetString(parse.Strings[i], '"Truyện ', ' | Đọc online')));

    // get cover
    if (GetTagName(parse.Strings[i]) = 'img') AND
       (Pos('img width="190px" height="250px"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('Sơ lược:', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+4;
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

      // get chapter name and links
    if (i+1<parse.Count) AND
       (GetTagName(parse.Strings[i]) = 'a') AND
       (Pos('/Truyen/', parse.Strings[i])>0) AND
       (Pos('title="Đọc', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(EncodeUrl(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))));
      parse.Strings[i+1]:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(parse.Strings[i+1])));
    end;

    // get authors
    if  (i+4<parse.Count) AND (Pos('Tác giả:', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(parse.Strings[i+4]);

    // get artists
    if (i+4<parse.Count) AND (Pos('Họa sỹ:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+4]);

    // get genres
    if (Pos('Thể loại:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if (i+1<parse.Count) AND (Pos('"/TheLoai/', parse.Strings[i]) > 0) then
        mangaInfo.genres:= mangaInfo.genres+(TrimLeft(TrimRight(parse.Strings[i+1]))+', ');
      if Pos('</p>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (i+2<parse.Count) AND (Pos('Tình trạng:', parse.Strings[i])<>0) then
    begin
      if Pos('Đang tiến hành', parse.Strings[i+2])<>0 then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
    end;
  end;

  if (mangaInfo.status = '1') AND (mangainfo.ChapterName.Count > 0) then
  begin
    Dec(mangaInfo.numChapter);
    mangainfo.ChapterName.Delete(mangainfo.ChapterName.Count-1);
    mangainfo.ChapterLinks.Delete(mangainfo.ChapterLinks.Count-1);
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
