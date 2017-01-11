  function UnixMangaNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[UNIXMANGA_ID, 1] + UNIXMANGA_BROWSER;
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
      if (Pos('/onlinereading/', parse[i]) > 0) and
        (Pos('title=', parse[i]) > 0) and (Pos('<a ', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        ANames.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        ALinks.Add(Trim(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[UNIXMANGA_ID, 1], '', [rfIgnoreCase])));
      end;
    end;
  end;
