  function OneMangaGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ONEMANGA_ID, 1] +
      ONEMANGA_BROWSER + IntToStr(StrToInt(AURL) + 1) + '/', 0) then
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
      if Pos('class="det"', parse[i]) > 0 then
        if Pos('<a ', parse[i + 2]) > 0 then
        begin
          Result := NO_ERROR;
          ANames.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 3])))));
          ALinks.Add(StringReplace(GetVal(parse[i + 2], 'href'),
            WebsiteRoots[ONEMANGA_ID, 1], '', [rfIgnoreCase]));
        end;
    end;
    Source.Free;
  end;
