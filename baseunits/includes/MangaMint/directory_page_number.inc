  function GetMangaMintDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAMINT_ID, 1] +
      '/directory', 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
      for i := parse.Count - 1 downto 0 do
        if (GetTagName(parse[i]) = 'li') and (GetVal(parse[i], 'class') = 'pager-last last') then
        begin
          Page := StrToIntDef(
            ReplaceRegExpr('^.*\?page=(\d+)$', GetVal(parse[i + 1], 'href'), '$1', True), 1);
          Break;
        end;
    Result := NO_ERROR;
  end;
