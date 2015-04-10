  function GetSubMangaPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(SUBMANGA_ID, URL),
      manager.container.manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := parse.Count - 1 downto 3 do
      begin
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(parse.Strings[i - 2])));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
