  function GetOneMangaPageNumber: Boolean;
  var
    s: String;
    i: Integer;
    l: TStringList;
    isGetPage: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(ONEMANGA_ID, URL);
    Result := GetPage(TObject(l),
      s,
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    Task.Container.PageNumber := 0;
    if parse.Count > 0 then
      for i := 1 to parse.Count - 1 do
      begin

        if (Pos('<select ', parse[i]) > 0) and
          (Pos('class="cbo_wpm_pag"', parse[i]) > 0) then
          isGetPage := True;
        if isGetPage and (Pos('</select', parse[i]) > 0) then
        begin
          isGetPage := False;
          Break;
        end;
        if isGetPage and (Pos('<option', parse[i]) > 0) then
          Inc(Task.Container.PageNumber);
      end;
    parse.Free;
    l.Free;
  end;
