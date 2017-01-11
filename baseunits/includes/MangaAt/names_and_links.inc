  function MangaAtGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAAT_ID, 1] +
      '/manga/page:' + IntToStr(StrToInt(AURL) + 1), 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    parse.Clear;
    Parser := THTMLParser.Create(FixHTMLTagQuote(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
    finally
      Parser.Exec;
    end;
    Parser.Free;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'a') and
          (GetVal(parse[i], 'class') = 'manga') then
        begin
          ALinks.Add(GetVal(parse[i], 'href'));
          ANames.Add(CommonStringFilter(parse[i + 1]));
        end;
      end;

    Result := NO_ERROR;
    Source.Free;
  end;
