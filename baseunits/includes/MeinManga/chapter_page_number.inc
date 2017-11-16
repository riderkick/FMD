  function GetMeinMangaPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(FillMangaSiteHost(MEINMANGA_ID, URL));
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      Task.container.PageLinks.Clear;
      for i := parse.Count - 1 downto 0 do
      begin
        if (Pos('</select>', parse[i]) > 0) then
        begin
          Task.Container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(parse[i - 3])));
          Break;
        end;
      end;
      if Task.Container.PageNumber > 0 then
        for i := 0 to Task.Container.PageNumber - 1 do
          Task.Container.pageLinks.Add(s + IntToStr(i + 1) + '.html');
    end;
    parse.Free;
    l.Free;
  end;
