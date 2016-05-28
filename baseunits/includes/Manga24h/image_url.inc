  function GetManga24hImageURL: Boolean;
  var
    i: Cardinal;
    l: TStringList;
  begin
    l := TStringList.Create;
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGA24H_ID, URL),
      Task.Container.Manager.retryConnect);
    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageLinks.Clear;
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'img') and
          (Pos('style="border:3px', parse[i]) <> 0) then
          // (GetVal(parse[i], 'class') = 'm_picture') then
        begin
          Task.Container.PageLinks.Add(GetVal(parse[i], 'src'));
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
