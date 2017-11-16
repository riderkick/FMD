  function GetEsMangaHerePageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(ESMANGAHERE_ID, URL),
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := parse.Count - 1 downto 4 do
      begin
        if Pos('</select>', parse[i]) > 0 then
        begin
          Task.Container.PageNumber :=
            StrToInt(TrimLeft(TrimRight(parse[i - 3])));
          Break;
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
