  function MyReadingMangaInfoNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MYREADINGMANGAINFO_ID, 1] +
      '/page/' + IntToStr(StrToInt(AURL) + 1) + '/', 0) then
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
      if Pos('class="entry-title"', parse[i]) > 0 then
      begin
        Result := NO_ERROR;
        s := Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2]))));
        ANames.Add(s);
        s := GetVal(parse[i + 1], 'href');
        s := StringReplace(s, WebsiteRoots[MYREADINGMANGAINFO_ID, 1], '', [rfIgnoreCase]);
        ALinks.Add(s);
      end;
    end;
    Source.Free;
  end;
