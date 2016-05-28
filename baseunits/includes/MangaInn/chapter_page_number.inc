  function GetMangaInnPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAINN_ID, URL),
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
      for i := parse.Count - 1 downto 5 do
      begin
        if Pos('</select>', parse[i]) <> 0 then
        begin
          try
            Task.Container.PageNumber := StrToInt(Trim(parse[i - 3]));
          except
            Task.Container.PageNumber := 0;
          end;
          Break;
        end;
      end;
    parse.Free;
    l.Free;
  end;
