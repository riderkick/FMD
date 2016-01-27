  function MangaEstaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAESTA_ID, 1] +
      MANGAESTA_BROWSER, 0) then
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
      if (Pos('class="gallery-icon"', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(
          TrimRight(GetVal(parse[i + 3], 'title'))));
        names.Add(HTMLEntitiesFilter(s));
        s := StringReplace(GetVal(parse[i + 2], 'href'),
          WebsiteRoots[MANGAESTA_ID, 1], '', []);
        links.Add(s);
      end;
    end;
    Source.Free;
  end;