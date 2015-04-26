  function GetAcademyDirectoryVNPageNumber: Byte;
  var
    i, p: Integer;
    s: String;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ACADEMYVN_ID, 1] +
      '/manga/all', 1) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      Page := 0;
      regx := TRegExpr.Create;
      try
        regx.Expression := '^.*/manga/all\?page=(\d+)$';
        regx.ModifierI := True;
        for i := 0 to parse.Count - 1 do
          if (GetTagName(parse[i]) = 'a') and (Pos('/manga/all?page=', parse[i]) > 0 ) then
          begin
            s := GetVal(parse[i], 'href');
            s := regx.Replace(s, '$1', True);
            p := StrToIntDef(s, 0);
            if p > Page then
              Page := p;
          end;
      finally
        regx.Free;
      end;
    end;
    Source.Free;
  end;
