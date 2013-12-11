function   AnimeExtremistGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[ANIMEEXTREMIST_ID,1] + ANIMEEXTREMIST_BROWSER, 0) then
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
      if Pos('id="manga" style="margin', parse.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        names.Add(TrimLeft(TrimRight(parse.Strings[i+4])));
        links.Add(StringReplace(GetString(parse.Strings[i+3], 'href="', '">'), WebsiteRoots[ANIMEEXTREMIST_ID,1], '', []));
      end;
    end;
    source.Free;
  end; 