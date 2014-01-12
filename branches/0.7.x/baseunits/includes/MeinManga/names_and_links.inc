function   MeinMangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MEINMANGA_ID,1] + MEINMANGA_BROWSER, 0) then
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
      if (Pos('/manga/', parse.Strings[i])>0) AND
         (Pos('<img', parse.Strings[i-2])>0) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        links.Add(s);
        s:= StringFilter(Trim(parse.Strings[i+1]));
        s:= StringReplace(s, WebsiteRoots[MEINMANGA_ID,1], '', []);
        names.Add(HTMLEntitiesFilter(s));
      end;
    end;
    source.Free;
  end; 
