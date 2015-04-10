  function ReadHentaiMangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[READHENTAIMANGA_ID, 1] +
      READHENTAIMANGA_BROWSER + IntToStr(StrToInt(URL) + 1) + '/', 0) then
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
      if (Pos('<a ', parse[i]) > 0) and (Pos('class="lst mng_det_pop"', parse[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := Trim(HTMLEntitiesFilter(StringFilter(GetVal(parse[i], 'title'))));
        names.Add(s);
        s := GetVal(parse[i], 'href');
        s := StringReplace(s, WebsiteRoots[READHENTAIMANGA_ID, 1], '', [rfIgnoreCase]);
        links.Add(s);
      end;
    end;
    Source.Free;
  end;
