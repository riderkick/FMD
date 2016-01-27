  function GetMangaGoPageNumber: Boolean;
  var
    i: Cardinal;
    s: String;
    l: TStringList;
    regx: TRegExpr;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGAGO_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
        if Pos('total_pages=', parse[i]) > 0 then
        begin
          regx := TRegExpr.Create;
          regx.Expression := '^.*total_pages=(\d+)\b.*$';
          manager.container.PageNumber :=
            StrToIntDef(regx.Replace(parse[i], '$1', True), 0);
          regx.Free;
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
