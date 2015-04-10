  function MangaSeeGetNamesAndLinks: Byte;
  var
    i: LongInt;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[MANGASEE_ID, 1] + '/directory.php';
    i := StrToIntDef(URL, 0);
    if i > 0 then
      s := s + '?c=' + chr(i + 64);
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
    
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if (Pos('<a', parse[i]) <> 0) and (Pos('id=', parse[i]) <> 0) and
          (GetVal(parse[i], 'class') = 'directory_link') then
        begin
          s := GetVal(parse[i], 'href');
          s := TrimLeftChar(s, ['.']);
          links.Add(s);
          names.Add(CommonStringFilter(parse[i + 1]));
        end;
    Result := NO_ERROR;
    Source.Free;
  end;
