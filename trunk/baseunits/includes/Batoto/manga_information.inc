function   GetBatotoInfoFromURL: Byte;
var
  count   : Cardinal = 0;
  patchURL,
  s: String;
  isExtractGenres : Boolean = FALSE;
  i, j: Cardinal;
  myParser: THTMLParser;

begin
  patchURL:= URL;
  if Pos('comic/_/comics', patchURL) = 0 then
    patchURL:= StringReplace(URL, 'comic/_', 'comic/_/comics', []);
  mangaInfo.url:= WebsiteRoots[BATOTO_ID,1] + patchURL;

  source.Clear;
  if NOT GetPage(TObject(source), TrimLeft(TrimRight(mangaInfo.url)), Reconnect) then
  begin
    Result:= NET_PROBLEM;
    source.Free;
    exit;
  end;

  // parsing the HTML source using our own HTML parser
  parse.Clear;
  Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
  Parser.OnFoundTag := OnTag;
  Parser.OnFoundText:= OnText;
  Parser.SlowExec;
  Parser.Free;

  source.Free;
  mangaInfo.website:= WebsiteRoots[BATOTO_ID,0];

  if parse.Count=0 then exit;
  for i:= 0 to parse.Count-1 do
  begin
    // get cover link
    if GetTagName(parse.Strings[i]) = 'img' then
    begin
      if (Pos('width:300px', parse.Strings[i-1]) <> 0) then
        mangaInfo.coverLink:= CorrectURL(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'src=')))
    end;

    // get title
    if (mangaInfo.title = '') AND
       (Pos('"og:title"', parse.Strings[i]) > 0) then
      mangaInfo.title:= StringFilter(GetString(parse.Strings[i], '"og:title" content="', ' - Scanlations'));

    // get summary
    if (Pos('Description:', parse.Strings[i]) <> 0) then
    begin
      j:= i+3;
      mangaInfo.summary:= '';
      while (Pos('</td>', parse.Strings[j])=0) AND (j < parse.Count-1) do
      begin
        s:= parse.Strings[j];
        if s[1] <> '<' then
        begin
          parse.Strings[j]:= StringFilter(parse.Strings[j]+#10#13);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #10, '\n', [rfReplaceAll]);
          parse.Strings[j]:= StringReplace(parse.Strings[j], #13, '\r', [rfReplaceAll]);
          mangaInfo.summary:= mangaInfo.summary + StringFilter(parse.Strings[j]);
        end;
        Inc(j);
      end;
    end;

    // get chapter name and links (bad code)
    //mulai
    if OptionShowAllLang then
    begin
      if (NOT OptionBatotoUseIEChecked) AND
         (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('/read/_/', parse.Strings[i])<>0) AND
         (i+8 < parse.Count-1) AND
         (Pos('title=', parse.Strings[i+8])>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[BATOTO_ID,1], '', [rfReplaceAll])));
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
        s:= StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2])));
        if OptionShowBatotoSG then
          s:= s + ' [by ' + StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+15]))) + ']' + ' [' + GetAttributeValue(GetTagAttribute(parse.Strings[i+8], 'title=')) + ']';
        mangaInfo.chapterName.Add(s);
      end
      else
      if (OptionBatotoUseIEChecked) AND
         (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('/read/_/', parse.Strings[i])<>0) AND
         (i+2 < parse.Count-1) AND
         (Pos('title=', parse.Strings[i-3])>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[BATOTO_ID,1], '', [rfReplaceAll])));
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
        mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i+2])));
      end
    end
    else
    begin

      if (NOT OptionBatotoUseIEChecked) AND
         (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('/read/_/', parse.Strings[i])<>0) AND
         (i+8 < parse.Count-1) AND
         (Pos('English', parse.Strings[i+8])>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[BATOTO_ID,1], '', [rfReplaceAll])));
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
        s:= StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+2])));
        if OptionShowBatotoSG then
          s:= s + ' [by ' + StringFilter(TrimRight(RemoveSymbols(parse.Strings[i+15]))) + ']';
        mangaInfo.chapterName.Add(s);
      end
      else
      if (OptionBatotoUseIEChecked) AND
         (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('/read/_/', parse.Strings[i])<>0) AND
         (i+2 < parse.Count-1) AND
         (Pos('English', parse.Strings[i-3])>0) then
      begin
        Inc(mangaInfo.numChapter);
        mangaInfo.chapterLinks.Add((StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[BATOTO_ID,1], '', [rfReplaceAll])));
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #10, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringReplace(parse.Strings[i+2], #13, '', [rfReplaceAll]);
        parse.Strings[i+2]:= StringFilter(TrimLeft(parse.Strings[i+2]));
        mangaInfo.chapterName.Add(TrimRight(RemoveSymbols(parse.Strings[i+2])));
      end
    end;

    // get authors
    if (i+5 < parse.Count-1) AND
       (Pos('Author:', parse.Strings[i])<>0) then
       mangaInfo.authors:= TrimLeft(parse.Strings[i+5]);

    // get artists
    if (i+5 < parse.Count-1) AND
       (Pos('Artist:', parse.Strings[i])<>0) then
      mangaInfo.artists:= TrimLeft(parse.Strings[i+5]);

    // get genres
    if (Pos('Genres:', parse.Strings[i])<>0) then
    begin
      isExtractGenres:= TRUE;
      mangaInfo.genres:= '';
    end;

    if isExtractGenres then
    begin
      if Pos('</span>', parse.Strings[i]) > 0 then
        mangaInfo.genres:= mangaInfo.genres+(TrimLeft(TrimRight(parse.Strings[i-1]))+', ');
      if Pos('</tr>', parse.Strings[i]) <> 0 then
        isExtractGenres:= FALSE;
    end;

    // get status
    if (Pos('Status:', parse.Strings[i])<>0) then
    begin
      if (i+4 < parse.Count-1) AND
         (Pos('Ongoing', parse.Strings[i+4])<>0) then
        mangaInfo.status:= '1'   // ongoing
      else
        mangaInfo.status:= '0';  // completed
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
