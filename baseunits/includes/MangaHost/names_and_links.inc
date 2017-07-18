  function MangaHostGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAHOST_ID, 1] +
      MANGAHOST_BROWSER + '/page/' + IntToStr(StrToInt(AURL) + 1), 0) then
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
      if (Pos('<a', parse[i]) > 0) and (Pos('class="pull-left', parse[i]) > 0) and
        (Pos('title=', parse[i]) > 0) then
      begin
        ALinks.Add(GetVal(parse[i], 'href'));
        //ANames.Add(CommonStringFilter(GetVal(parse[i], 'title')));
        ANames.Add(CommonStringFilter(parse[i + 6]));
      end;
    end;
    Result := NO_ERROR;
    Source.Free;
  end;
