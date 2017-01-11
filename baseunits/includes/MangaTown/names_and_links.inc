  function MangaTownGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGATOWN_ID, 1] +
      MANGATOWN_BROWSER + IntToStr(StrToInt(AURL) + 1) + '.htm?name.az', 0) then
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
      if (Pos('<P class="title"', parse[i]) > 0) and
        (i + 2 < parse.Count - 1) then
      begin
        Result := NO_ERROR;
        s := Trim(HTMLEntitiesFilter(StringFilter(GetVal(parse[i + 2], 'title'))));
        ANames.Add(s);
        s := GetVal(parse[i + 2], 'href');
        s := StringReplace(s, WebsiteRoots[MANGATOWN_ID, 1], '', [rfIgnoreCase]);
        ALinks.Add(s);
      end;
    end;
    Source.Free;
  end;
