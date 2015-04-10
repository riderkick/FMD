  function GetJapscanPageNumber: Boolean;
  var
    s, h: String;
    i: Cardinal;
    l: TStringList;
    regx: TRegExpr;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(JAPSCAN_ID, URL));
    Result := GetPage(TObject(l),
      s,
      manager.container.manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    manager.container.PageNumber := 0;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      manager.container.PageLinks.Clear;
      //get image-uri
      h := '';
      for i := 0 to parse.Count - 1 do
        if (Pos('<img', parse[i]) > 0) and (Pos('data-uri=', parse[i]) > 0) then
        begin
          h := GetAttributeValue(GetTagAttribute(parse[i], 'data-uri='));
          Break;
        end;

      regx := TRegExpr.Create;
      regx.Expression := '^.*data-image=''([^\'']+)''\s.*$';
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('data-image=', parse[i]) > 0 then
        begin
          Result := True;
          s := regx.Replace(parse[i], '$1', True);
          s := WebsiteRoots[JAPSCAN_ID, 1] + h + s;
          manager.container.PageLinks.Add(s);
        end;
      end;

      manager.container.PageNumber := manager.container.PageLinks.Count;
      regx.Free;
    end;
    parse.Free;
    l.Free;
  end;
