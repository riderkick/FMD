  function KivmangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[KIVMANGA_ID, 1], 0) then
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
    for i := parse.Count - 1 downto 5 do
    begin
      if (Pos('option value="', parse[i]) > 0) and
        (Pos('value="0"', parse[i]) = 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(TrimRight(parse[i + 1])));
        ANames.Add(HTMLEntitiesFilter(s));
        s := '/' + GetVal(parse[i], 'value');
        ALinks.Add(s);
      end;
    end;
    Source.Free;
  end;
