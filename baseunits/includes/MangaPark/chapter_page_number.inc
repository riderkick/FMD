  function GetMangaParkPageNumber: Boolean;
  var
    i: LongInt;
    s: String;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := URL;
    if RightStr(s, 2) = '/1' then
      SetLength(s, Length(s) - 2);
    Result := GetPage(TObject(l),
      FillMangaSiteHost(MANGAPARK_ID, s),
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (Pos('<img', parse[i]) <> 0) and (Pos('id="img-', parse[i]) <> 0) then
        begin
          Inc(manager.container.PageNumber);
          manager.container.PageLinks.Add(GetVal(parse[i], 'src'));
        end;
    parse.Free;
    l.Free;
  end;
