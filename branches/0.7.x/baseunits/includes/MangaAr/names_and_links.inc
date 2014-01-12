function   MangaArGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAAR_ID,1] + MANGAAR_BROWSER, 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;

    // convert charset
    source.text:= CP1256ToUTF8(source.text);

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
      if (Pos('style="float', parse.Strings[i]) > 0) AND
         (Pos('"http://manga-ar', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[MANGAAR_ID,1], '', []);
        links.Add(s);
      end;
    end;
    source.Free;
  end;   