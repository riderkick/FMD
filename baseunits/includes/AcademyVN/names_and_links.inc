  function AcademyVNGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ACADEMYVN_ID, 1] +
      '/manga/all?page=' + IntToStr(StrToInt(URL) + 1), 1) then
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
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'a') and
          (GetVal(parse[i], 'class') = 'ajax-title text-ellipsis') then
        begin
          links.Add(GetVal(parse[i], 'href'));
          names.Add(CommonStringFilter(GetVal(parse[i], 'title')));
        end;

    Result := NO_ERROR;
  end;
