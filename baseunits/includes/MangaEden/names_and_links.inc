  function MangaEdenGetNamesAndLinks(const root: String): Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), root + MANGAEDEN_BROWSER + '?page=' +
      IntToStr(StrToInt(URL) + 1), 0) then
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
      if ((Pos('class="openManga"', parse.Strings[i]) > 0) or
        (Pos('class="closedManga"', parse.Strings[i]) > 0)) then
      begin
        Result := NO_ERROR;
        s := TrimLeft(TrimRight(StringFilter(parse.Strings[i + 1])));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(
          parse.Strings[i], 'href=')), root, '', []));
      end;
    end;
    Source.Free;
  end;