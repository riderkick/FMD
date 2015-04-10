  function GetBatotoPageNumber: Boolean;
  var
    isWebtoon: Boolean = True;
    i, j: Cardinal;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;

    parse.Clear;
    l.Clear;
    s := FillMangaSiteHost(BATOTO_ID, URL);
    Result := GetPage(TObject(l), s + '/1', manager.container.manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    manager.container.PageNumber := 0;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('page_select', parse.Strings[i]) > 0) then
        begin
          isWebtoon := False;
          Break;
        end;
      end;

    if isWebtoon then
    begin
      //grab image directly
      if parse.Count > 0 then
      begin
        manager.container.PageLinks.Clear;
        for i := 0 to parse.Count - 1 do
        begin
          if Pos('<img ', parse.Strings[i]) > 0 then
            if Pos('<br/>', parse.Strings[i + 1]) > 0 then
              manager.container.pageLinks.Add(GetAttributeValue(
                GetTagAttribute(parse.Strings[i], 'src=')));
        end;
      end;
      if manager.container.PageLinks.Count > 0 then
        manager.container.PageNumber := manager.container.PageLinks.Count - 1;
    end
    else
    begin
      if parse.Count > 0 then
        for i := 0 to parse.Count - 1 do
        begin
          if Pos('page_select', parse.Strings[i]) <> 0 then
          begin
            j := i + 2;
            while GetTagName(parse.Strings[j]) = 'option' do
            begin
              Inc(manager.container.PageNumber);
              Inc(j, 3);
            end;
            Break;
          end;
        end;
    end;
    parse.Free;
    l.Free;
  end;
