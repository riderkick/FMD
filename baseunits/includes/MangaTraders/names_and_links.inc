  function MangaTradersGetNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGATRADERS_ID, 1] +
      MANGATRADERS_BROWSER , 0) then
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

    regx := TRegExpr.Create;
    try
      regx.Expression := '\/manga\/\?series\=(.*)$';
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('class="seriesList', parse[i]) > 0 then
        begin
          Result := NO_ERROR;
          names.Add(CommonStringFilter(parse[i + 3]));
          s := TrimLeftChar(GetVal(parse[i + 2], 'href'), ['.']);
          if regx.Exec(s) then
            s := regx.Replace(s, '/read-online/$1', True);
          links.Add(s);
        end;
      end;
    finally
      regx.Free;
    end;
    Source.Free;
  end;
