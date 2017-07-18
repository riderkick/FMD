  function GetOneMangaDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ONEMANGA_ID, 1] +
      ONEMANGA_BROWSER, 0) then
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
      if Pos('Last', parse[i]) > 0 then
        if (Pos('<a ', parse[i - 1]) > 0) and (Pos('</a', parse[i + 1]) > 0) then
        begin
          APage := StrToIntDef(ReplaceRegExpr('^.*\/(\d+)\/$',
            GetVal(parse[i - 1], 'href'), '$1', True), 1);
          Result := NO_ERROR;
          Break;
        end;
    Source.Free;
  end;
