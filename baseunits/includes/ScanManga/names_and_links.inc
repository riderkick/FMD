  function ScanMangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[SCANMANGA_ID, 1] +
      SCANMANGA_BROWSER, 0) then
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
      if (i + 4 < parse.Count - 1) and
        (Pos('<li>', parse[i]) > 0) and
        (Pos('</a>', parse[i + 3]) > 0) and
        (Pos('</li>', parse[i + 4]) > 0) then
      begin
        Result := NO_ERROR;
        s := GetVal(parse[i + 1], 'href');
        if (Length(s) > 1) and
          (Pos('/scanlation/Shonen.html', s) = 0) and
          (Pos('/scanlation/Shojo.html', s) = 0) and
          (Pos('/scanlation/Josei.html', s) = 0) and
          (Pos('/scanlation/Seinen.html', s) = 0) then
        begin
          ALinks.Add(StringReplace(s, WebsiteRoots[SCANMANGA_ID, 1], '', []));
          s := StringFilter(parse[i + 2]);
          ANames.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    Source.Free;
  end;