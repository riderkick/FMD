  function PecintaKomikGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[PECINTAKOMIK_ID, 1] +
      PECINTAKOMIK_BROWSER, 0) then
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
      if (Pos('class=''screenshot''', parse.Strings[i]) > 0) or
        (Pos('class="screenshot"', parse.Strings[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')));
        names.Add(HTMLEntitiesFilter(s));
        s := GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        if s[Length(s)] <> '/' then
          s := s + '/';
        links.Add(s);
      end;
      if (Pos('/manga/', parse.Strings[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 1])));
        names.Add(HTMLEntitiesFilter(s));
        s := GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        if s[Length(s)] <> '/' then
          s := s + '/';
        links.Add(s);
      end;
    end;
    Source.Free;
  end;