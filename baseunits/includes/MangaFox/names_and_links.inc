function   MangaFoxGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAFOX_ID,1] + MANGAFOX_BROWSER + IntToStr(StrToInt(URL)+1) + '.htm?az', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (Pos('"manga_text"', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+2], 'href='));
        links.Add(s);
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+3])));
        s:= StringReplace(s, WebsiteRoots[MANGAFOX_ID,1], '', []);
        names.Add(HTMLEntitiesFilter(s));
      end;
    end;
    source.Free;
  end; 