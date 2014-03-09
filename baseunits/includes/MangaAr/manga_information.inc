function   GetMangaArInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[MANGAAR_ID,1] + URL;
  if NOT GetPage(TObject(source), EncodeURL(mangaInfo.url), Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // convert charset
  source.Text:= CP1256ToUTF8(source.Text);

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= WebsiteRoots[MANGAAR_ID,0];
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('class="manga-pic"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('القصة :', parse.Strings[i]) <> 0) AND
       (Pos('</font>', parse.Strings[i+1])<>0)  AND
       (isExtractSummary) then
    begin
      j:= i+6;
      while (j<parse.Count) AND (Pos('</td>', parse.Strings[j])=0) do
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

    // get title
    if (mangaInfo.title = '') AND
       (Pos(' - ARAB MANGA Online', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i], '~!@', ' - عرب مانجا أونلاين - مشاهدة مباشرة دون عناء التحميل'))));

    // get chapter name and links
    if (Pos('vertical-align: middle; margin: 5px', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href=')), WebsiteRoots[MANGAAR_ID,1], '', []);
      s:= EncodeURL(StringReplace(s+'~!@', '/1~!@', '', []));
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+3])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    // get authors
    if (i+6<parse.Count) AND
       (Pos('المؤلف :', parse.Strings[i])<>0) AND
       (Pos('</font>', parse.Strings[i+1])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+8]));

    // get artists
    if (i+6<parse.Count) AND
       (Pos('الرسام :', parse.Strings[i])<>0) AND
       (Pos('</font>', parse.Strings[i+1])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(parse.Strings[i+8]));

    // get genres
    if (Pos('&Genres=', parse.Strings[i])<>0) then
      mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';

    // get status
    if (i+6<parse.Count) AND
       (Pos('الحالة :', parse.Strings[i])<>0) AND
       (Pos('</font>', parse.Strings[i+1])<>0) then
    begin
      if (Pos('غير مكتمله', parse.Strings[i+6])<>0) then
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
