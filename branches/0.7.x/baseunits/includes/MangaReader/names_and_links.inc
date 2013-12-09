function   MangaReaderGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAREADER_ID,1] + MANGAREADER_BROWSER, 0) then
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
      if (Pos('<li>', parse.Strings[i])>0) AND
         (Pos('</a>', parse.Strings[i+3])>0) AND
         (Length(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href='))) > 2) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href='));
        links.Add(s);
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+2])));
        names.Add(HTMLEntitiesFilter(s));
      end;
      if Pos('Network', parse.Strings[i])>0 then
        break;
    end;
    source.Free;
  end; 