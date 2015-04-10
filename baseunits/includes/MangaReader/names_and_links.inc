  function MangaReaderGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAREADER_ID, 1] +
      MANGAREADER_BROWSER, 0) then
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
      if (Pos('<li>', parse.Strings[i]) > 0) and
        (Pos('</a>', parse.Strings[i + 3]) > 0) and
        (Length(GetAttributeValue(GetTagAttribute(parse.Strings[i + 1], 'href='))) >
        2) then
      begin
        Result := NO_ERROR;
        s := GetAttributeValue(GetTagAttribute(parse.Strings[i + 1], 'href='));
        links.Add(s);
        s := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 2])));
        names.Add(HTMLEntitiesFilter(s));
      end;
      if Pos('Network', parse.Strings[i]) > 0 then
        Break;
    end;
    Source.Free;
  end;