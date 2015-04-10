  function GetHakiHomePageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
    isExtractPageNumber: Boolean = False;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(HAKIHOME_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
    l.Text := FixHTMLTagQuote(l.Text);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<select', parse[i]) > 0) and (Pos('onchange="javascript:if', parse[i]) > 0) then
          isExtractPageNumber := True;
        if isExtractPageNumber and (Pos('</select', parse[i]) > 0) then
        begin
          isExtractPageNumber := False;
          Break;
        end;
        if isExtractPageNumber and (Pos('<option', parse[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
  end;
