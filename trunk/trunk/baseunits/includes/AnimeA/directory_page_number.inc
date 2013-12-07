function   GetAnimeADirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[ANIMEA_ID,1] + ANIMEA_BROWSER, 0) then
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
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))='http://manga.animea.net/browse.html?page=1') AND
         (Pos('Next', parse.Strings[i+1])>0) then
      begin
        Page:= StrToInt(TrimRight(TrimLeft(parse.Strings[i-4])));
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end; 