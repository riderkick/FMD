  function GetStarkanaPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    isGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(STARKANA_ID, URL)),
      Task.Container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      Task.Container.PageNumber := 0;
      for i := 0 to  parse.Count - 1 do
      begin
        if (Pos('<select', parse[i]) > 0) and (Pos('id="page_switch', parse[i]) > 0) then
          isGetPageNumber := True;
        if isGetPageNumber and (Pos('</select', parse[i]) > 0) then
          Break;
        if isGetPageNumber and (Pos('<option', parse[i]) > 0) then
          Inc(Task.Container.PageNumber);
      end;
    end;
    parse.Free;
    l.Free;
  end;
