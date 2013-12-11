function   KissMangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[KISSMANGA_ID,1] + KISSMANGA_BROWSER + '?page=' + IntToStr(StrToInt(URL)+1), 0) then
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
      if (Pos('/Manga/', parse.Strings[i])>0) AND
         (Pos('title=', parse.Strings[i])>0) then
      begin
        Result:= NO_ERROR;
        s:= StringReplace(parse.Strings[i+1], #10, '', [rfReplaceAll]);
        s:= StringReplace(s, #13, '', [rfReplaceAll]);
        s:= TrimRight(TrimLeft(s));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[KISSMANGA_ID,1], '', []));
      end;
    end;
    source.Free;
  end;   