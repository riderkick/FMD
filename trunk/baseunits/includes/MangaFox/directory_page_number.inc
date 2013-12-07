function   GetMangaFoxDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAFOX_ID,1] + MANGAFOX_BROWSER + '?az', 0) then
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
    for i:= parse.Count-2 downto 0 do
    begin
      if (GetTagName(parse.Strings[i]) = 'span') AND
         (i <= parse.Count) AND
         (Pos('span class="next"', parse.Strings[i])<>0)  then
      begin
        s:= GetString(parse.Strings[i-6], 'href="', '.htm');
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end; 