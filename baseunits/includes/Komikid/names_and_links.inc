  function KomikidGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    isFound: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[KOMIKID_ID, 1] + KOMIKID_BROWSER, 0) then
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
      if Pos('class="series_col"', LowerCase(parse.Strings[i])) > 0 then
        isFound := True;
      if isFound and (Pos('</html>', LowerCase(parse.Strings[i])) > 0) then
        isFound := False;
      if isFound and (Pos('<a ', parse.Strings[i]) > 0) then
        if (Pos('<li>', parse.Strings[i - 1]) > 0) then
        begin
          Result := NO_ERROR;
          links.Add('/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href="')));
          names.Add(Trim(RemoveStringBreaks(HTMLEntitiesFilter(
            StringFilter(parse.Strings[i + 1])))));
        end;
    end;
    parse.Clear;
    Source.Free;
  end;