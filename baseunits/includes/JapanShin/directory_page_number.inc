  function GetJapanShinDirectoryPageNumber: Byte;
  var
    i, p: Cardinal;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[JAPANSHIN_ID, 1] +
      JAPANSHIN_BROWSER, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;

    APage := 0;
    regx := TRegExpr.Create;
    regx.Expression := '^.*/reader/list/(\d+)/.*$';
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('class="gbutton fright"', parse[i]) > 0) and
        (Pos('/reader/list/', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        p := StrToIntDef(regx.Replace(parse[i], '$1', True), 0);
        if p > APage then
          APage := p;
      end;
    end;
    regx.Free;
    Source.Free;
    Result := NO_ERROR;
  end;
