  function GetMangaTownPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    s: String;
    isExtractPage: Boolean = False;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGATOWN_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);
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
          (Pos('onchange="javascript:location.href=this.value;"', parse[i]) > 0) then
          isExtractPage := True;
        if isExtractPage and
          (Pos('</select', parse[i]) > 0) then
        begin
          isExtractPage := False;
          Break;
        end;
        if isExtractPage and
          (Pos('<option', parse[i]) > 0) then
        begin
          Inc(manager.container.PageNumber);
        end;
      end;
    end;
    parse.Free;
  end;
