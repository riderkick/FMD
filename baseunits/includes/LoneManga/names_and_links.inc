  function LoneMangaGetNamesAndLinks: Byte;
  var
    i: Integer;
    s: string;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[LONEMANGA_ID, 1] + '/mangas/';
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
        if (GetVal(parse[i], 'class') = 'grid-entry-title entry-title') then
        begin
          links.Add(GetVal(parse[i+1], 'href'));
          names.Add(CommonStringFilter(parse[i+2]));
        end;

    Result := NO_ERROR;
  end;
