  function GetMangaCowDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGACOW_ID, 1] +
      MANGACOW_BROWSER, 0) then
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
      if (Pos('<a ', parse[i]) > 0) then
        if (Trim(parse[i + 1]) = 'Last') then
          if (Pos('</a', parse[i + 2]) > 0) then
          begin
            Result := NO_ERROR;
            Page := StrToIntDef(ReplaceRegExpr('^.*\/(\d+)\/$',
              GetVal(parse[i], 'href'), '$1', True), 1);
            Break;
          end;
    end;
    Source.Free;
  end;