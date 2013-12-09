function   GetCentralDeMangasInfoFromURL: Byte;
var
  s: String;
  isExtractSummary: Boolean = TRUE;
  isExtractGenres : Boolean = FALSE;
  isExtractChapter: Boolean = FALSE;
  i, j: Cardinal;
begin
  mangaInfo.url:= WebsiteRoots[CENTRALDEMANGAS_ID,1] + URL;
  if NOT GetPage(TObject(source), EncodeURL(mangaInfo.url), Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // convert charset
  // source.Text:= ISO_8859_1ToUTF8(source.Text);

  // parsing the HTML source
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.Exec;

  Parser.Free;
  source.Free;
  mangaInfo.website:= CENTRALDEMANGAS_NAME;
  // using parser (cover link, summary, chapter name and link)
  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover
    if (mangaInfo.coverLink = '') AND
       (Pos('class="img-polaroid"', parse.Strings[i])>0) then
      mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')));

    // get summary
    if (Pos('class="img-polaroid"', parse.Strings[i]) <> 0) AND
       (isExtractSummary) then
    begin
      j:= i+6;
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

    // get title
    if (mangaInfo.title = '') AND
       (Pos('<title>', parse.Strings[i])<>0) then
      mangaInfo.title:= TrimLeft(HTMLEntitiesFilter(StringFilter(GetString('~!@'+parse.Strings[i+1], '~!@', ' || Central de Mangás'))));

    if (NOT isExtractChapter) AND
       (Pos('Capítulos', parse.Strings[i]) > 0) then
      isExtractChapter:= TRUE;

    // get chapter name and links
    if (Pos('/online/', parse.Strings[i])>0) AND
       (Pos('class="span1"', parse.Strings[i])>0) then
    begin
      Inc(mangaInfo.numChapter);
      s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[CENTRALDEMANGAS_ID,1], '', []);
      mangaInfo.chapterLinks.Add(s);
      s:= RemoveSymbols(TrimLeft(TrimRight(parse.Strings[i+1])));
      mangaInfo.chapterName.Add(StringFilter(HTMLEntitiesFilter(s)));
    end;

    // get authors
    if (i+12<parse.Count) AND
       (Pos('Autor', parse.Strings[i])<>0) AND
       (Pos('<h4>', parse.Strings[i-1])<>0) then
      mangaInfo.authors:= TrimLeft(TrimRight(parse.Strings[i+12]));

    // get artists
    if (i+12<parse.Count) AND
       (Pos('Arte', parse.Strings[i])<>0) AND
       (Pos('<h4>', parse.Strings[i-1])<>0) then
      mangaInfo.artists:= TrimLeft(TrimRight(parse.Strings[i+12]));

    // get genres
    if (i+1<parse.Count) AND
       (Pos('Genêro', parse.Strings[i])<>0) AND
       (Pos('</h4>', parse.Strings[i+1])<>0) AND
       (Pos('<h4>', parse.Strings[i-1])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      s:= TrimLeft(TrimRight(parse.Strings[i]));
      if Pos('<h4>', s) <> 0 then
        isExtractGenres:= FALSE
      else
      if (Pos('class="btn btn-mini', s)<>0) then
        mangaInfo.genres:= mangaInfo.genres + TrimLeft(TrimRight(parse.Strings[i+1])) + ', ';
    end;

    // get status
    if (i+12<parse.Count) AND
       (Pos('Status', parse.Strings[i])<>0) AND
       (Pos('<h4>', parse.Strings[i-1])<>0) then
    begin
      if (Pos('Completo', parse.Strings[i+12])<>0) then
        mangaInfo.status:= '0'   // completed
      else
        mangaInfo.status:= '1';  // ongoing
    end;
  end;
  Result:= NO_ERROR;
end;
