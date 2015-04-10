  function GetUnionMangasDirectoryPageNumber: Byte;
  var
    i, p: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[UNIONMANGAS_ID, 1] +
      UNIONMANGAS_BROWSER, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    CleanHTMLComments(Source);
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

    Page := 1;
    for i := parse.Count - 1 downto 2 do
    begin                      //http://unionmangas.com.br/mangas/a-z/2/*
      if (Pos('<a', parse[i]) > 0) and (Pos('/mangas/a-z/', parse[i]) > 0) then
      begin
        p := StrToIntDef(ReplaceRegExpr('^.*/a-z/(\d+)/.*$', GetVal(parse[i], 'href'), '$1', True), 0);
        if Page < p then
          Page := p;
      end;
    end;
    Source.Free;
  end;
