  function GetAnimeAPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    isPage: Boolean = False;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    Result := GetPage(TObject(l), FillMangaSiteHost(ANIMEA_ID,
      StringReplace(URL, '.html', '', [])) +
      '.html', manager.container.Manager.retryConnect);

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<select', parse[i]) > 0) and
          (Pos('class="mangaselecter pageselect"', parse[i]) > 0) then
          isPage := True;
        if isPage and (Pos('</select', parse[i]) > 0) then
        begin
          isPage := False;
          Break;
        end;
        if isPage and (Pos('<option', parse[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
  end;
