  function GetIKomikDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    APage := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[IKOMIK_ID, 1] + IKOMIK_BROWSER, 0) then
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
      if i + 1 < parse.Count - 1 then
        if (Pos('/page-', parse[i]) > 0) and (Pos('last', parse[i + 1]) > 0) then
        begin
          Result := NO_ERROR;
          APage := StrToIntDef(ReplaceRegExpr('^.*\/page-(\d+)\/.*$', GetVal(parse[i], 'href'), '$1', True), 1);
          Break;
        end;
    end;
    Source.Free;
  end;
