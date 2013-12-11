function   GetHentai2ReadDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), HENTAI2READ_ROOT + HENTAI2READ_BROWSER, 0) then
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
      if (GetTagName(parse.Strings[i]) = 'img') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'alt='))='Next Page') then
      begin
        s:= TrimRight(TrimLeft(parse.Strings[i-5]));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;
  