  function GetHakiHomeDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Page := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[HAKIHOME_ID, 1] + HAKIHOME_BROWSER, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    Source.Text := FixHTMLTagQuote(Source.Text);
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    Source.Free;
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      if i + 1 < parse.Count - 1 then
        if (Pos('class="inbut"', parse[i]) > 0) and (Pos('<a', parse[i]) > 0) and
          (Pos('Last', parse[i + 1]) > 0) then
        begin
          Result := NO_ERROR;
          Page := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$', Trim(GetVal(parse[i], 'href')), '$1', True), 1);
          Break;
        end;
    end;
  end;
