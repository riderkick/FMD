  function DynastyScansGetNamesAndLinks: Byte;
  var
    i: Integer;
    s: string;
  begin
    Result := INFORMATION_NOT_FOUND;
    i := StrToIntDef(AURL, 0);
    if i >= Length(DYNASTYSCANS_BROWSER) then
    begin
      Source.Free;
      Exit;
    end;

    s := WebsiteRoots[DYNASTYSCANS_ID, 1] + DYNASTYSCANS_BROWSER[i];
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
        if GetTagName(parse[i]) = 'dd' then
          if GetTagName(parse[i+1]) = 'a' then
          begin
            ALinks.Add(GetVal(parse[i+1], 'href'));
            ANames.Add(CommonStringFilter(parse[i+2]));
          end;

    Result := NO_ERROR;
  end;
