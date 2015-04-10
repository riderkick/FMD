  function MangaStreamToGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    isExtractNamesAndLinks: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGASTREAMTO_ID, 1] +
      MANGASTREAMTO_BROWSER, 0) then
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

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<table', parse.Strings[i]) > 0) and
          (Pos('id="page-list"', parse.Strings[i]) > 0) then
          isExtractNamesAndLinks := True;

        if isExtractNamesAndLinks and
          (Pos('</table', parse.Strings[i]) > 0) then
          isExtractNamesAndLinks := False;

        if isExtractNamesAndLinks and
          (Pos('href="/', parse.Strings[i]) > 0) then
        begin
          names.Add(HTMLEntitiesFilter(StringFilter(Trim(parse.Strings[i + 1]))));
          links.Add(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
        end;
      end;
    Source.Free;
  end;