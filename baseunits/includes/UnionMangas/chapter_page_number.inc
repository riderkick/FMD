  function GetUnionMangasPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(UNIONMANGAS_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

    CleanHTMLComments(l);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      manager.container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<img', parse[i]) > 0) and (Pos('class="real', parse[i]) > 0) and
         (Pos('data-lazy', parse[i]) > 0) then
          manager.container.PageLinks.Add(GetVal(parse[i], 'data-lazy'));
      end;
      manager.container.PageNumber := manager.container.PageLinks.Count;
    end;
    parse.Free;
  end;
