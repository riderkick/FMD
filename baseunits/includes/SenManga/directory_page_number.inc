  function GetSenMangaDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[SENMANGA_ID, 1] +
      '/directory/all/any/last-added/', 3) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(Source.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
      for i := parse.Count - 1 downto 0 do
        if (GetTagName(parse[i]) = 'a') and
          (Pos('/directory/all/any/last-added/', parse[i]) > 0) then
        begin
          Page := StrToIntDef(ReplaceRegExpr('^.*/last-added/(\d+)/?$',
            GetVal(parse[i], 'href'), '$1', True), 1);
          Result := NO_ERROR;
          Break;
        end;
  end;
