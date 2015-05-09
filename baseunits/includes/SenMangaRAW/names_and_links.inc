  function SenMangaRAWGetNamesAndLinks: Byte;
  var
    i: Integer;
    isExtractNames: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[SENMANGARAW_ID, 1] +
      '/Manga/?order=text-version', 3) then
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
      begin
        if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'id') = 'content') then
          isExtractNames := True;
        if isExtractNames then
        begin
          if (GetTagName(parse[i]) = 'div') and
            (GetVal(parse[i], 'class') = 'col-xm-12 col-sm-4 col-md-4') then
            Break
          else
          if GetTagName(parse[i]) = 'a' then
            if GetTagName(parse[i + 4]) = 'td' then
            begin
              names.Add(parse[i + 1]);
              links.Add(GetVal(parse[i], 'href'));
            end;
        end;
      end;

    Result := NO_ERROR;
  end;
