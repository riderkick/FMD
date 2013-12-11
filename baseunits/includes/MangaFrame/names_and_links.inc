function   MangaframeGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAFRAME_ID,1] + MANGAFRAME_BROWSER, 0) then
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
      if (Pos('class="title"', parse.Strings[i]) > 0) AND
         (Pos('class="group"', parse.Strings[i-1]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'title=')));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGAFRAME_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;