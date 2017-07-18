  function MangaLib_PLGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGALIB_PL_ID, 1] +
      MANGALIB_PL_BROWSER, 0) then
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
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('itemprop="name"', parse[i]) > 0) then
        begin
          Result := NO_ERROR;
          ALinks.Add(StringReplace(GetVal(parse[i - 1], 'href'),
            WebsiteRoots[MANGALIB_PL_ID, 1], '', [rfIgnoreCase]));
          ANames.Add(Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1]))));
        end;
      end;
    end;
    Source.Free;
  end;
