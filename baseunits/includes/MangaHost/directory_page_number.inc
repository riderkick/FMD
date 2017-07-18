  function GetMangaHostDirectoryPageNumber: Byte;
  var
    i, p: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAHOST_ID, 1] +
      MANGAHOST_BROWSER, 0) then
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

    APage := 1;
    for i := parse.Count - 1 downto 2 do
    begin                      
      if (Pos('<a', parse[i]) > 0) and (Pos('class="last', parse[i]) > 0) then
      begin
        p := StrToIntDef(ReplaceRegExpr('^.*/(\d+)$', GetVal(parse[i], 'href'), '$1', True), 0);
        if APage < p then
          APage := p;
        Break;
      end;
    end;
    Source.Free;
  end;
