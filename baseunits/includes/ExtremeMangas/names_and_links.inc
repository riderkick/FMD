  function ExtremeMangasNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
    isExtractNames: Boolean = False;
    regxp: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[EXTREMEMANGAS_ID, 1] + EXTREMEMANGAS_BROWSER;
    if not GetPage(TObject(Source), s, 0) then
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

    regxp := TRegExpr.Create;
    regxp.ModifierI := True;
    regxp.Expression := '^https?://.*\.com/';
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('<div', parse[i]) > 0) and (Pos('id="content"', parse[i]) > 0) then
        isExtractNames := True;
      if isExtractNames and (Pos('<center', parse[i]) > 0) then
      begin
        isExtractNames := False;
        Break;
      end;
      if i + 1 < parse.Count - 1 then
        if isExtractNames and (Pos('<a', parse[i]) > 0) then
        begin
          Result := NO_ERROR;
          ANames.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
          s := Trim(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[EXTREMEMANGAS_ID, 1], '', [rfIgnoreCase]));
          s := Trim(regxp.Replace(s, '/', False));
          ALinks.Add(s);
        end;
    end;
    regxp.Free;
  end;
