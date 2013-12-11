function   GetMangaEdenDirectoryPageNumber(const root: String): Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), root + MANGAEDEN_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'span') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='next') then
      begin
        s:= TrimRight(TrimLeft(parse.Strings[i-4]));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end; 