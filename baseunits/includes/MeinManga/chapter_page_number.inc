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
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      manager.container.PageLinks.Clear;
      for i := parse.Count - 1 downto 0 do
      begin
        if (Pos('</select>', parse[i]) > 0) then
        begin
          manager.container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(parse[i - 3])));
          Break;
        end;
      end;
      if manager.container.PageNumber > 0 then
        for i := 0 to manager.container.PageNumber - 1 do
          manager.container.pageLinks.Add(s + IntToStr(i + 1) + '.html');
    end;
    parse.Free;
    l.Free;
  end;
