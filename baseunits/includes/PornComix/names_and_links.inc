  function PornComixGetNamesAndLinks(MangaID: Cardinal): Byte;
  var
    i: LongInt;
    s: String;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[MangaID, 1] + '/';
    if MangaID = PORNCOMIXRE_ID then
      s := s + 'online/';
    if URL <> '0' then
      s := s + 'page/' + IntToStr(StrToInt(URL) + 1) + '/';
    if not GetPage(TObject(Source), s, 0) then
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
      regx.Expression := '\w+\.\w+/.+$';
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('id="post-', parse[i]) <> 0) then
        begin
          s := GetVal(parse[i + 2], 'href');
          if regx.Exec(s) then
          begin
            links.Add(s);
            names.Add(CommonStringFilter(GetVal(parse[i + 2], 'title')));
            if names[names.Count - 1] = '' then
              names[names.Count - 1] := 'Untitled_' + IntToStr(Random(99999));
          end;
        end;
      end;
    finally
      regx.Free;
    end;
    Result := NO_ERROR;
    Source.Free;
  end;
