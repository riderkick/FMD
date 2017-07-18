  function GetNHentaiDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    APage := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[NHENTAI_ID, 1] + '/', 0) then
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
      if (Pos('<a', parse[i]) > 0) and (Pos('?page=', parse[i]) > 0) and
        (Pos('class="last"', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        APage := StrToIntDef(ReplaceRegExpr('^.*\?page=(\d+).*$', GetVal(parse[i], 'href'), '$1', True), 1);
        Break;
      end;
    end;
    Source.Free;
  end;
