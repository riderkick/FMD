  function GetMangaInnPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAINN_ID, URL),
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
      for i := parse.Count - 1 downto 5 do
      begin
        if Pos('</select>', parse.Strings[i]) <> 0 then
        begin
          try
            manager.container.PageNumber := StrToInt(Trim(parse.Strings[i - 3]));
          except
            manager.container.PageNumber := 0;
          end;
          Break;
        end;
      end;
    parse.Free;
    l.Free;
  end;
