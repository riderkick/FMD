  function GetMangaEdenDirectoryPageNumber(const root: String): Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), root + MANGAEDEN_BROWSER, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;
    for i := 0 to parse.Count - 1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'span') and
        (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class=')) = 'next') then
      begin
        s := TrimRight(TrimLeft(parse.Strings[i - 4]));
        Page := StrToInt(s);
        Result := NO_ERROR;
        Source.Free;
        Exit;
      end;
    end;
    Source.Free;
  end;