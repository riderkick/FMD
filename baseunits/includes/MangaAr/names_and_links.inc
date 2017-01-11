  function MangaArGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
    isExtractMangaList: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAAR_ID, 1] + MANGAAR_BROWSER, 0) then
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
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<tbody>', parse[i]) > 0) then
          isExtractMangaList := True;
        if (Pos('</tbody>', parse[i]) > 0) then
        begin
          isExtractMangaList := False;
          Break;
        end;

        if isExtractMangaList and (Pos('href=', parse[i]) > 0) then
        begin
          s := GetVal(parse[i], 'href');
          s := StringReplace(s, WebsiteRoots[MANGAAR_ID, 1], '', []);
          ALinks.Add(s);
          s := Trim(StringFilter(parse[i + 1]));
          s := HTMLEntitiesFilter(s);
          ANames.Add(s);
        end;
      end;
    end;
    Source.Free;
  end;