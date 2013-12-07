function   GetKissMangaDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[KISSMANGA_ID,1] + KISSMANGA_BROWSER, 0) then
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
      if //(GetTagName(parse.Strings[i]) = 'a') AND
         //(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='))='http://manga.animea.net/browse.html?page=1') AND
         (Pos('&raquo; Last', parse.Strings[i])>0) then
      begin
        Page:= StrToInt(StringReplace(TrimRight(TrimLeft(GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'page=')))), '"', '', [rfReplaceAll]));
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;