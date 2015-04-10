  function GetMangaSeePageNumber: Boolean;
  var
    s: String;
    i: LongInt;
    l: TStringList;
    regx: TRegExpr;
  begin
    l := TStringList.Create;

    s := DecodeUrl(URL);
    if RightStr(s, 7) = '&page=1' then
      SetLength(s, Length(s) - 7);
    s := FillMangaSiteHost(manager.container.MangaSiteID, s);

    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    if parse.Count > 0 then
    begin
      regx := TRegExpr.Create;
      try
        regx.Expression := '^.*src="([^"]+)".*$';
        for i := 0 to parse.Count - 1 do
          if (Pos('<img', parse[i]) <> 0) and (Pos('this.onerror=null', parse[i]) <> 0) then
          begin
            Inc(manager.container.PageNumber);
            manager.container.PageLinks.Add(regx.Replace(parse[i], '$1', True));
          end;
      finally
        regx.Free;
      end;
    end;
    parse.Free;
    l.Free;
  end;
