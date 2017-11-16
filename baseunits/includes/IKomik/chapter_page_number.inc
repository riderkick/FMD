  function GetIKomikPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    isExtractPage: Boolean = False;
    s: String;
  begin
    Task.Container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(IKOMIK_ID, URL);
    Result := GetPage(TObject(l), s, Task.Container.Manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := @OnTag;
    Parser.OnFoundText := @OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<select', parse[i]) > 0) and (Pos('class="cbo_wp_manga_page"', parse[i]) > 0) then
          isExtractPage := True;
        if isExtractPage and (Pos('</select', parse[i]) > 0) then
        begin
          isExtractPage := False;
          Break;
        end;
        if isExtractPage and (Pos('<option', parse[i]) > 0) then
          Inc(Task.Container.PageNumber);
      end;
    end;
    parse.Free;
  end;
