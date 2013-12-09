function   PecintaKomikGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[PECINTAKOMIK_ID,1] + PECINTAKOMIK_BROWSER, 0) then
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
      if (Pos('class=''screenshot''', parse.Strings[i]) > 0) OR
         (Pos('class="screenshot"', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        if s[Length(s)] <> '/' then
          s:= s+ '/';
        links.Add(s);
      end;
      if (Pos('/manga/', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        if s[Length(s)] <> '/' then
          s:= s+ '/';
        links.Add(s);
      end;
    end;
    source.Free;
  end;     