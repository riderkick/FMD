  function StarkanaGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[STARKANA_ID, 1] +
      STARKANA_BROWSER, 0) then
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
      if (Pos('style="float:right;', parse[i]) > 0) then
      begin
        ALinks.Add(GetVal(parse[i + 2], 'href'));
        ANames.Add(CommonStringFilter(parse[i + 3]));
      end;
    end;
    Result := NO_ERROR;
    Source.Free;
  end;
