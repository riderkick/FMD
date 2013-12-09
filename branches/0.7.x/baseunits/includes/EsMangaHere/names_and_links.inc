function   EsMangaHereGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[ESMANGAHERE_ID,1] + ESMANGAHERE_BROWSER, 0) then
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
      if Pos('manga_info', parse.Strings[i]) <> 0 then
      begin
        Result:= NO_ERROR;
        names.Add(StringFilter(GetString(parse.Strings[i], 'rel="', '" href')));
        links.Add(StringReplace(GetString(parse.Strings[i], 'href="', '">'), WebsiteRoots[ESMANGAHERE_ID,1], '', []));
      end;
    end;
    source.Free;
  end; 