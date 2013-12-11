function   GetMangaAeDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGAAE_ID,1] + MANGAAE_BROWSER, 0) then
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
      if (Pos('<li class=''active''>', parse.Strings[i]) > 0) then
      begin
        s:= TrimRight(TrimLeft(parse.Strings[i+10]));
        Page:= StrToInt(s);
        Result:= NO_ERROR;
        source.Free;
        exit;
      end;
    end;
    source.Free;
  end;