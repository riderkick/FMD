  function GetSenMangaDirectoryPageNumber: Byte;
  var
    i: Integer;
    s: string;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[SENMANGA_ID, 1] +
      SENMANGA_BROWSER, 0) then
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
    for i := parse.Count - 1 downto 0 do
      if (Pos('<a', parse[i]) <> 0) and (Pos('?order=title&page=', parse[i]) <> 0) then
      begin
        s := GetVal(parse[i], 'href');
        Page := StrToIntDef(ReplaceRegExpr('^.*page=(\d+)$', s, '$1', True), 1);
        Result := NO_ERROR;
        Break;
      end;
    Source.Free;
  end;
