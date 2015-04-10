  function MangaFoxGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    //if NOT GetPage(TObject(source), WebsiteRoots[MANGAFOX_ID,1] + MANGAFOX_BROWSER + IntToStr(StrToInt(URL)+1) + '.htm?az', 0) then
    if not GetPage(TObject(Source), WebsiteRoots[MANGAFOX_ID, 1] +
      MANGAFOX_BROWSER, 0) then
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
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<a ', parse[i]) > 0) and
          (Pos('class="series_preview manga_', parse[i]) > 0) then
        begin
          Result := NO_ERROR;
          links.Add(StringReplace(GetVal(parse[i], 'href'),
            WebsiteRoots[MANGAFOX_ID, 1], '', [rfIgnoreCase]));
          names.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
        end;
      end;
    end;

  end;