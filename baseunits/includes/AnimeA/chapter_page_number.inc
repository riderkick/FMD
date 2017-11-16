  function GetAnimeAPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    isPage: Boolean = False;
  begin
    Task.Container.PageNumber := 0;
    l := TStringList.Create;
    Result := GetPage(TObject(l), FillMangaSiteHost(ANIMEA_ID,
      StringReplace(URL, '.html', '', [])) +
      '.html', Task.Container.Manager.retryConnect);

    parse := TStringList.Create;
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
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
          Inc(Task.Container.PageNumber);
      end;
    end;
    parse.Free;
  end;
