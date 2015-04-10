  function GetReadHentaiMangaDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Page := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[READHENTAIMANGA_ID, 1] +
      READHENTAIMANGA_BROWSER, 0) then
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
      if (Pos(READHENTAIMANGA_BROWSER, parse[i]) > 0) and (i + 1 < parse.Count -1) then
        if Pos('Last', parse[i + 1]) > 0 then
        begin
          s := GetVal(parse[i], 'href');
          s := ReplaceRegExpr('^.*\/(\d+)\/$', s, '$1', True);
          Page := StrToIntDef(s, 1);
          Result := NO_ERROR;
          Break;
        end;
    end;
    Source.Free;
  end;
