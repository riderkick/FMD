  function MangaGoGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAGO_ID, 1] +
      MANGAGO_BROWSER + IntToStr(StrToInt(URL) + 1) + '/', 0) then
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
      if Pos('h3 class="title"', parse[i]) > 0 then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(TrimRight(parse[i + 2])));
        names.Add(HTMLEntitiesFilter(s));
        s := GetVal(parse[i + 1], 'href');
        s := StringReplace(s, WebsiteRoots[MANGAGO_ID, 1], '', []);
        links.Add(s);
      end;
    end;
    Source.Free;
  end;