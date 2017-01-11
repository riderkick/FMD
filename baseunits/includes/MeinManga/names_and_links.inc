  function MeinMangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MEINMANGA_ID, 1] +
      MEINMANGA_BROWSER, 0) then
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
      if (Pos('/manga/', parse[i]) > 0) and
        (Pos('<img', parse[i - 2]) > 0) then
      begin
        Result := NO_ERROR;
        s := GetVal(parse[i], 'href');
        ALinks.Add(s);
        s := StringFilter(Trim(parse[i + 1]));
        s := StringReplace(s, WebsiteRoots[MEINMANGA_ID, 1], '', []);
        ANames.Add(HTMLEntitiesFilter(s));
      end;
    end;
    Source.Free;
  end;