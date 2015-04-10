  function MangaFrameNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
    //isExtractNames: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[MANGAFRAME_ID, 1] + MANGAFRAME_BROWSER + IntToStr(StrToInt(URL) + 1) + '/';
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
    Source.Free;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if i + 2 < parse.Count - 1 then
          if (Pos('class="title"', parse[i]) > 0) and (Pos('<a', parse[i + 1]) > 0) and
            (Pos('/okuyucu/series/', parse[i + 1]) > 0) then
          begin
            names.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 2])))));
            links.Add(Trim(StringReplace(GetVal(parse[i + 1], 'href'), WebsiteRoots[MANGAFRAME_ID, 1], '', [rfIgnoreCase])));
          end;
    Result := NO_ERROR;
  end;
