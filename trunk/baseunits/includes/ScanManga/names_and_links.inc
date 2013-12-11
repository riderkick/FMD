function   ScanMangaGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[SCANMANGA_ID,1] + SCANMANGA_BROWSER, 0) then
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
      if (i+4 < parse.Count-1) AND
         (Pos('<li>', parse.Strings[i]) > 0) AND
         (Pos('</a>', parse.Strings[i+3]) > 0) AND
         (Pos('</li>', parse.Strings[i+4]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href='));
        if (Length(s) > 1) AND
           (Pos('/scanlation/Shonen.html', s) = 0) AND
           (Pos('/scanlation/Shojo.html', s) = 0) AND
           (Pos('/scanlation/Josei.html', s) = 0) AND
           (Pos('/scanlation/Seinen.html', s) = 0) then
        begin
          links.Add(StringReplace(s, WebsiteRoots[SCANMANGA_ID,1], '', []));
          s:= StringFilter(parse.Strings[i+2]);
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    source.Free;
  end;