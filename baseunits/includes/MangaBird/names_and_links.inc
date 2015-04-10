  function MangaBirdNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[MANGABIRD_ID, 1] + MANGABIRD_BROWSER;
    if StrToIntDef(URL, 0) > 0 then
      s := s + '?page=' + URL;
    if not GetPage(TObject(Source), s, 0) then
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
    for i := 0 to parse.Count - 1 do
    begin
      if (i + 4 < parse.Count - 1) then
        if (Pos('class="views-field views-field-title"', parse[i]) > 0) and (Pos('<td', parse[i]) > 0) then
        begin
          Result := NO_ERROR;
          names.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 4])))));
          links.Add(Trim(StringReplace(GetVal(parse[i + 2], 'href'), WebsiteRoots[MANGABIRD_ID, 1], '', [rfIgnoreCase])));
        end;
    end;
  end;
