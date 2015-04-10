  function JapscanNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[JAPSCAN_ID, 1] +
      JAPSCAN_BROWSER, 0) then
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
    Source.Free;
    if parse.Count = 0 then
      Exit;

    links.Clear;
    names.Clear;
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('class="cell', parse[i]) > 0) then
        if (Pos(' href="/mangas/', parse[i + 1]) > 0) then
        begin
          Result := NO_ERROR;
          links.Add(GetAttributeValue(GetTagAttribute(parse[i + 1], 'href=')));
          names.Add(Trim(parse[i + 2]));
        end;
    end;
  end;