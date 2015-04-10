  function MangaPandaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
    isExtractInfo: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAPANDA_ID, 1] +
      MANGAPANDA_BROWSER, 0) then
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
      if (not isExtractInfo) and
        (Pos('ul class="series_alpha"', parse.Strings[i]) > 0) then
        isExtractInfo := True;
      if (isExtractInfo) and
        (Pos('<li>', parse.Strings[i]) > 0) and
        (Pos('<a', parse.Strings[i + 1]) > 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 2])));
        names.Add(HTMLEntitiesFilter(s));
        s := GetAttributeValue(GetTagAttribute(parse.Strings[i + 1], 'href="'));
        s := StringReplace(s, WebsiteRoots[MANGAPANDA_ID, 1], '', []);
        links.Add(s);
      end
      else
      if (isExtractInfo) and
        (Pos('div id="wrapper_footer"', parse.Strings[i]) > 0) then
        Break;
    end;
    Source.Free;
  end;