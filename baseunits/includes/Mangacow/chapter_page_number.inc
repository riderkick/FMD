  function GetMangaCowPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
    regx: TRegExpr;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MANGACOW_ID, URL) + '1/');
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      regx := TRegExpr.Create('^.*\.push\("([^"]+)"\);.*$');
      try
        regx.ModifierI := True;
        for i := 0 to parse.Count - 1 do
          if Pos('arr_img.push(', parse[i]) > 0 then
            manager.container.PageLinks.Add(regx.Replace(parse[i], '$1', True));
      finally
        regx.Free
      end;
    end;
    parse.Free;
    l.Free;
  end;
