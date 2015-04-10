  function GetMangaTradersPageNumber: Boolean;
  var
    isGetPageNumber: Boolean = False;
    i: Integer;
    l: TStringList;
    s: String;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGATRADERS_ID, URL);
    s := s + '/page-1';
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('<select', parse[i]) > 0 then
          isGetPageNumber := True;
        if isGetPageNumber and (Pos('</select', parse[i]) > 0) then
        begin
          isGetPageNumber := False;
          Break;
        end;
        if isGetPageNumber and (Pos('<option', parse[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
    l.Free;
  end;
