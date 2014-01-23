function   GetManga2uDirectoryPageNumber: Byte;
  var
    i : Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGA2U_ID,1] + MANGA2U_BROWSER, 0) then
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
    for i:= parse.Count-1 downto 2 do
    begin
      if (Pos('sct_wid_bot', parse.Strings[i]) > 0) then
      begin
        s:= GetString(GetAttributeValue(GetTagAttribute(parse.Strings[i-18], 'href=')), '-popular/', '/');
		Page:= StrToInt(s);
	Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end; 