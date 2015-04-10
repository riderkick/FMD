  function GetSenMangaPageNumber: Boolean;
  var
    s: String;
    i: LongInt;
    l: TStringList;
    isStartGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := DecodeUrl(URL);
    if RightStr(s, 2) <> '/1' then
      s := s + '/1';
    s := FillMangaSiteHost(SENMANGA_ID, s);
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;

    manager.container.PageNumber := 0;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<select', parse[i]) <> 0) and (Pos('name="page"', parse[i]) <> 0) then
          isStartGetPageNumber := True;
        if isStartGetPageNumber and (Pos('</select', parse[i]) > 0) then
          Break;
        if isStartGetPageNumber and (Pos('<option', parse[i]) > 0) then
        begin
          Inc(manager.container.PageNumber);
          manager.container.PageLinks.Add('G');
        end;
      end;
    end;
    parse.Free;
    l.Free;
  end;
