  function GetMangaBirdPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGABIRD_ID, URL);
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
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('class="pageslink"', parse[i]) > 0) and (Pos('<a', parse[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
  end;
