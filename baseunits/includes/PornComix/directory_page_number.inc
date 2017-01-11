  function GetPornComixDirectoryPageNumber(MangaID: Cardinal): Byte;
  var
    i, p: LongInt;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[MangaID, 1];
    if MangaID = PORNCOMIXRE_ID then
      s := s + '/online/';
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

    for i := 0 to parse.Count - 1 do
      if (Pos('<a', parse[i]) > 0) and (Pos('/page/', parse[i]) > 0) then
      begin
        p := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$', GetVal(parse[i], 'href'), '$1', True), 0);
        if APage < p then
          APage := p;
      end;
    Source.Free;
  end;
