function   GetManga24hInfoFromURL: Byte;
var
 // patchURL,
  s: String;
  i, j: Cardinal;
  isExtractChapters: Boolean = FALSE;
  isExtractSummary : Boolean = FALSE;
begin
 // patchURL:= UTF8ToANSI(URL);
 // Insert('comics/', patchURL, 10);
  mangaInfo.url:= WebsiteRoots[MANGA24H_ID,1] + URL;
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
  mangaInfo.website:= WebsiteRoots[MANGA24H_ID,0];
  mangaInfo.summary:= '';

  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover link
    if (Pos('class="img-rounded', parse.Strings[i]) > 0) then
    begin
      mangaInfo.coverLink:= EncodeURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));
      s:= mangaInfo.coverLink;
    end;

    // get summary
    if (Pos('"clearfix"', parse.Strings[i])>0) AND
       (Pos('<p>', parse.Strings[i+3])>0) then
    begin
      j:= i+5;
      mangaInfo.summary:= '';
      while (Pos('$(document).ready(function()', parse.Strings[j])=0) AND (j<parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if (Length(s)>0) AND (s[1] <> '<') then
        begin
          parse.Strings[j]:= StringFilter(HTMLEntitiesFilter(parse.Strings[j]));
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + StringFilter(TrimRight(TrimLeft(parse.Strings[j])));
        end;
        Inc(j);
      end;
    end;

    if (Pos('<tbody>', parse.Strings[i])<>0) AND (NOT isExtractSummary) then
      isExtractChapters:= TRUE;

    if (Pos('</tbody>', parse.Strings[i])<>0) AND (isExtractSummary) then
      isExtractChapters:= FALSE;


      // get chapter name and links
    if (isExtractChapters) AND
       (Pos('<td>', parse.Strings[i])<>0) AND
       (GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')) <> '') AND
       (Pos('</a>', parse.Strings[i+3])<>0) then
    begin
      Inc(mangaInfo.numChapter);
      mangaInfo.chapterLinks.Add(CorrectURL('/'+StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGA24H_ID,1], '', [rfReplaceAll])));
      parse.Strings[i+2]:= HTMLEntitiesFilter(parse.Strings[i+2]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
      parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
      parse.Strings[i+2]:= TrimLeft(parse.Strings[i+2]);
      mangaInfo.chapterName.Add(StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2]))));
    end;

    // get title
    if (Pos('<title>', parse.Strings[i])<>0) AND (mangaInfo.title = '') then
      mangaInfo.title:= TrimLeft(StringFilter(GetString('~!@'+parse.Strings[i+1], '~!@', ' - Truyen Tranh Online')));

    // get authors
    if (Pos('Tác giả :', parse.Strings[i])<>0) then
      mangaInfo.authors:= TrimLeft(StringFilter(parse.Strings[i+3]));

    // get artists
    if (Pos('Họa sỹ :', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(StringFilter(parse.Strings[i+3]));

    // get genres
    if (Pos('Thể loại :', parse.Strings[i])<>0) then
    begin
      mangaInfo.genres:= '';
      for j:= 0 to 37 do
        if Pos(LowerCase(defaultGenres[j]), LowerCase(parse.Strings[i+4]))<>0 then
          mangaInfo.genres:= mangaInfo.genres+(defaultGenres[j]+', ');
    end;

    // get status
    if (Pos('Tình Trạng:', parse.Strings[i])<>0) then
    begin
      if Pos('Hoàn Thành', parse.Strings[i+4])<>0 then
        mangaInfo.status:= '0'   // ongoing
      else
        mangaInfo.status:= '1';  // completed
    end;
  end;

  // Since chapter name and link are inverted, we need to invert them
  if mangainfo.ChapterName.Count > 1 then
  begin
    i:= 0; j:= mangainfo.ChapterName.Count - 1;
    while (i<j) do
    begin
      mangainfo.ChapterName.Exchange(i, j);
      mangainfo.chapterLinks.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  end;
  Result:= NO_ERROR;
end;
