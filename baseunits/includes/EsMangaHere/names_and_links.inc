  function EsMangaHereGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[ESMANGAHERE_ID, 1] +
      ESMANGAHERE_BROWSER, 0) then
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
      if Pos('manga_info', parse[i]) <> 0 then
      begin
        Result := NO_ERROR;
        ANames.Add(StringFilter(GetString(parse[i], 'rel="', '" href')));
        ALinks.Add(StringReplace(GetString(parse[i], 'href="', '">'),
          WebsiteRoots[ESMANGAHERE_ID, 1], '', []));
      end;
    end;
    Source.Free;
  end;