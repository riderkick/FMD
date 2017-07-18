  function JapanShinGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source),
      WebsiteRoots[JAPANSHIN_ID, 1] + JAPANSHIN_BROWSER +
      IntToStr(StrToInt(AURL) + 1) + '/',
      0) then
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
      if (Pos('<a', parse[i]) > 0) and
        (Pos('/reader/series/', parse[i]) > 0) and
        (Pos('title="', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := GetVal(parse[i], 'href');
        ALinks.Add(StringReplace(s, WebsiteRoots[JAPANSHIN_ID, 1], '', [rfIgnoreCase]));
        s := Trim(HTMLEntitiesFilter(StringFilter(parse[i + 1])));
        ANames.Add(s);
      end;
    end;
    Source.Free;
  end;
