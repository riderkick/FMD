function   MangaEdenGetNamesAndLinks(const root: String): Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), root + MANGAEDEN_BROWSER + '?page=' + IntToStr(StrToInt(URL)+1), 0) then
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
      if ((Pos('class="openManga"', parse.Strings[i])>0) OR
          (Pos('class="closedManga"', parse.Strings[i])>0)) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+1])));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), root, '', []));
      end;
    end;
    source.Free;
  end; 