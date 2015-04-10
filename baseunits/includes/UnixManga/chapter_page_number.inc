  function GetUnixMangaPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(UNIXMANGA_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

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
      manager.container.PageContainerLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('class="td2"', parse[i]) > 0) and (Pos('<A', parse[i]) > 0) then
          manager.container.PageContainerLinks.Add(EncodeURL(GetVal(parse[i], 'HREF')));
      end;
      manager.container.PageNumber := manager.container.PageContainerLinks.Count;
    end;
    parse.Free;
  end;
