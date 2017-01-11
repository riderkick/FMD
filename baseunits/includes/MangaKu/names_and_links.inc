  function MangaKuGetNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAKU_ID, 1] +
      '/daftar-komik-bahasa-indonesia/', 3) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    s := FixCommonBrokenHTML(Source.Text);
    Source.Free;

    parse.Clear;
    Parser := THTMLParser.Create(s);
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
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'screenshot') then
        begin
          ALinks.Add(GetVal(parse[i], 'href'));
          ANames.Add(CommonStringFilter(parse[i + 1]));
        end;
      end;

    Result := NO_ERROR;
  end;
