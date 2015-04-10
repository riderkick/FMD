  function GetManga2uDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGA2U_ID, 1] + MANGA2U_BROWSER, 0) then
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
    Source.Free;
    if parse.Count > 0 then
      for i := parse.Count - 1 downto 2 do
        if (Pos('<a', parse[i]) > 0) and (Pos(MANGA2U_BROWSER, parse[i]) > 0)
          and (Pos('Last', parse[i + 1]) > 0) then
        begin
          page := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$', GetVal(parse[i], 'href'), '$1', True), 1);
          Result := NO_ERROR;
          Break;
        end;
  end;
