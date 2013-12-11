function   GetFakkuDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[FAKKU_ID,1] + FAKKU_BROWSER, 0) then
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
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title='))='Last Page') then
      begin
        s:= TrimRight(TrimLeft(GetString(parse.Strings[i], '/page/', '"')));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;
  