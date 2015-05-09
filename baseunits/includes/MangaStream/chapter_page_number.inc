  function GetMangaStreamPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGASTREAM_ID, URL) + '/1',
      manager.container.manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free
    end;

    if parse.Count > 0 then
    begin
      manager.container.pageNumber := 0;
      for i := 0 to parse.Count - 1 do
        if Pos('Last Page (', parse[i]) > 0 then
        begin
          manager.container.PageNumber := StrToIntDef(ReplaceRegExpr(
            '^.*Last\s*Page\s*\((\d+)\).*$', parse[i], '$1', True), 1);
          Break;
        end;
    end;

    parse.Free;
    l.Free;
  end;
