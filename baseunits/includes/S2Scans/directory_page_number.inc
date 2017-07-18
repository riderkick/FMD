  function GetS2ScanDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[S2SCAN_ID, 1] + '/directory/', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if Pos('Last »»', parse[i]) > 0 then
          if GetTagName(parse[i - 1]) = 'a' then
          begin
            APage := StrToIntDef(ReplaceRegExpr(
              '^.*/(\d+)/$', GetVal(parse[i - 1], 'href'), '$1', True), 1);
            Break;
          end;
    Source.Free;
  end;
