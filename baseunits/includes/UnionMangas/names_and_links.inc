  function UnionMangasNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source),
      WebsiteRoots[UNIONMANGAS_ID, 1] + UNIONMANGAS_BROWSER + '/a-z/' +
      IntToStr(StrToInt(URL) + 1) + '/*', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    CleanHTMLComments(Source);
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
      if (Pos('<a', parse[i]) > 0) and (Pos('class="capa_', parse[i]) > 0) and
       (i + 6 < parse.Count - 1) then
      begin
        names.Add(CommonStringFilter(parse[i + 6]));
        links.Add(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[UNIONMANGAS_ID, 1], '', [rfIgnoreCase]));
      end;
    end;
    Result := NO_ERROR;
    Source.Free;
  end;
