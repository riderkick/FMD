  function MangaAeGetNamesAndLinks: Byte;
  var
    i: LongInt;
    s: String;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAAE_ID, 1] +
      MANGAAE_BROWSER + 'page:' + IntToStr(StrToInt(AURL) + 1), 0) then
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
    if parse.Count > 0 then
    begin
      regx := TRegExpr.Create;
      try
        regx.Expression := '[^/]*\w+\.\w+(\:\d+)?/.+';
        for i := 0 to parse.Count - 1 do
        begin
          if (Pos('<a', parse[i]) <> 0) and (Pos('class="manga"', parse[i]) <> 0) then
          begin
            s := GetVal(parse[i], 'href');
            if regx.Exec(s) then
            begin
              ALinks.Add(s);
              ANames.Add(CommonStringFilter(parse[i + 1]));
            end;
          end;
        end;
      finally
        regx.Free;
      end;
    end;
    Result := NO_ERROR;
    Source.Free;
  end;
