  function GetMangaBirdDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Page := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGABIRD_ID, 1] + MANGABIRD_BROWSER, 0) then
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
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('title="Go to last page"', parse[i]) > 0) and (Pos('<a', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        Page := StrToIntDef(ReplaceRegExpr('^.*=(\d+).*$', GetVal(parse[i], 'href'), '$1', True), 0);
        Inc(Page); //from 0
        Break;
      end;
    end;
    BROWSER_INVERT := True;
  end;
