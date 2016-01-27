  function MangaInnGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAINN_ID, 1] +
      MANGAINN_BROWSER, 0) then
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
      if (Pos('<li ', parse[i]) > 0) and
        (Pos('class="mangalistItems"', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := GetVal(parse[i + 1], 'href');
        s := StringReplace(s, WebsiteRoots[MANGAINN_ID, 1], '',
          [rfIgnoreCase, rfReplaceAll]);
        s := StringReplace(s, 'http://www.mangainn.com', '',
          [rfIgnoreCase, rfReplaceAll]);
        links.Add(s);
        names.Add(Trim(StringFilter(parse[i + 2])));
      end;
    end;
    Source.Free;
  end;