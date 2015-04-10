  function GetEsMangaHerePageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(ESMANGAHERE_ID, URL),
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := parse.Count - 1 downto 4 do
      begin
        if Pos('</select>', parse.Strings[i]) > 0 then
        begin
          manager.container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(parse.Strings[i - 3])));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
