  function SenMangaGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[SENMANGA_ID, 1] +
      '/directory/all/any/last-added/' + IntToStr(StrToInt(URL) + 1) + '/', 3) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(Source.Text);
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;
    Source.Free;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
        if (GetTagName(parse[i]) = 'a') and (GetVal(parse[i], 'class') = 'lst mng_det_pop') then
        begin
          links.Add(GetVal(parse[i], 'href'));
          names.Add(CommonStringFilter(GetVal(parse[i], 'title')));
        end;
      Result := NO_ERROR;
    end;
  end;
