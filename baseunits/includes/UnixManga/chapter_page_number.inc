  function GetUnixMangaPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
  begin
    Task.Container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(UNIXMANGA_ID, URL);
    Result := GetPage(TObject(l), s, Task.Container.Manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      Task.Container.PageLinks.Clear;
      Task.Container.PageContainerLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('class="td2"', parse[i]) > 0) and (Pos('<A', parse[i]) > 0) then
          Task.Container.PageContainerLinks.Add(EncodeURL(GetVal(parse[i], 'HREF')));
      end;
      Task.Container.PageNumber := Task.Container.PageContainerLinks.Count;
    end;
    parse.Free;
  end;
