  function GetAnimeADirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ANIMEA_ID, 1] + ANIMEA_BROWSER, 0) then
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
    begin
      if (GetTagName(parse[i]) = 'a') and
        (GetVal(parse[i], 'href') =
        'http://manga.animea.net/browse.html?page=1') and
        (Pos('Next', parse[i + 1]) > 0) then
      begin
        APage := StrToInt(TrimRight(TrimLeft(parse[i - 4])));
        Result := NO_ERROR;
        Source.Free;
        Exit;
      end;
    end;
    Source.Free;
  end;