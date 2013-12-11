function   MangaParkGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAPARK_ID,1] + MANGAPARK_BROWSER + URL + '?az', 0) then
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
      if (Pos('"title"', parse.Strings[i])>0) AND
         (Pos('"_blank"', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        links.Add(s);
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
      end;
    //  if Pos('Network', parse.Strings[i])>0 then
    //    break;
    end;
    source.Free;
  end;     