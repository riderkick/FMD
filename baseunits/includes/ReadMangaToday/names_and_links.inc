  function ReadMangaTodayGetNamesAndLinks: Byte;
  var
    i: Integer;
    s: string;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[READMANGATODAY_ID, 1] + '/manga-list';
    i := StrToIntDef(URL, 0);
    if i > 0 then
      s := s + '/' + ALPHA_LIST[i+1];
    if not GetPage(TObject(Source), s, 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    ParseHTML(Source.Text, parse);
    Source.Free;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'span') and
          (GetVal(parse[i], 'class') = 'manga-item') then
        begin
          links.Add(GetVal(parse[i+4], 'href'));
          names.Add(CommonStringFilter(parse[i+5]));
        end;

    Result := NO_ERROR;
  end;
