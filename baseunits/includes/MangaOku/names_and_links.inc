  function MangaOkuGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
    isExtractMangas: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAOKU_ID, 1] + '/', 0) then
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
      if (Pos('<select', parse[i]) > 0) and (Pos('name="manga"', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        isExtractMangas := True;
      end;
      if isExtractMangas and (Pos('</select', parse[i]) > 0) then
      begin
        isExtractMangas := False;
        Break;
      end;
      if isExtractMangas and (Pos('<option', parse[i]) > 0) then
      begin
        s := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));
        ANames.Add(s);
        s := TrimChar(GetVal(parse[i], 'value'), ['/']);
        s := '/' + s + '/';
        ALinks.Add(s);
      end;
    end;
    // index 0 isn't manga
    if ALinks.Count > 0 then
      if ALinks[0] = '/0/' then
      begin
        ALinks.Delete(0);
        ANames.Delete(0);
      end;
  end;
