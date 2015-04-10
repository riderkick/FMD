  function Manga2uGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGA2U_ID, 1] +
      MANGA2U_BROWSER + IntToStr(StrToInt(URL) + 1), 0) then
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
        if i + 3 < parse.Count - 1 then
          if (Pos('class="det"', parse[i]) > 0) and (Pos('<a', parse[i + 2]) > 0)
            and (Pos('<', parse[i + 3]) = 0) then
          begin
            names.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 3])))));
            links.Add(Trim(StringReplace(GetVal(parse[i + 2], 'href'),
              WebsiteRoots[MANGA2U_ID, 1], '', [rfIgnoreCase])));
          end;
    Result := NO_ERROR;
  end;
