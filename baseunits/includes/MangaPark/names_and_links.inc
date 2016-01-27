  function MangaParkGetNamesAndLinks: Byte;
  var
    i: LongInt;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAPARK_ID, 1] +
      '/search?orderby=add&page=' + IntToStr(StrToInt(URL) + 1), 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'em') and (GetVal(parse[i], 'class') = 'icon') then
        begin
          links.Add(GetVal(parse[i + 3], 'href'));
          names.Add(CommonStringFilter(GetVal(parse[i + 3], 'title')));
        end;
    end;
    Source.Free;
  end;
