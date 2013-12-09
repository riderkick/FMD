function   MangaTradersGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGATRADERS_ID,1] + MANGATRADERS_BROWSER + 'All/page/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
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
    for i:= parse.Count-1 downto 5 do
    begin
      if (Pos('/manga/series/', parse.Strings[i]) > 0) AND
         (TryStrToInt(GetString(parse.Strings[i], '/manga/series/', '"'), tmp)) AND
         (Pos('<img', parse.Strings[i+1]) = 0) AND
         (Pos('Anything without a category', parse.Strings[i+1]) = 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= GetString(parse.Strings[i], 'href="', '"');
        links.Add(s);
      end;
    end;
    source.Free;
  end; 