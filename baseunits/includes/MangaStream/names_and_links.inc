  function MangaStreamGetNamesAndLinks: Byte;
  var
    i: Integer;
    isExtractNames: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), MANGASTREAM_ROOT + '/manga', 3) then
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
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'table') and (GetVal(parse[i], 'class') = 'table table-striped') then
          isExtractNames := True;
        if isExtractNames then
        begin
          if  GetTagName(parse[i]) = '/table' then
            Break
          else
          if GetTagName(parse[i]) = 'a' then
            if GetVal(parse[i], 'class') <> 'chapter-link' then
            begin
              links.Add(GetVal(parse[i], 'href'));
              names.Add(CommonStringFilter(parse[i + 1]));
            end;
        end;
      end;
    Source.Free;
  end;
