function   Manga24hGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
    myParser: THTMLParser;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[MANGA24H_ID,1] + MANGA24H_BROWSER + IntToStr(StrToInt(URL)+1), 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    myParser:= THTMLParser.Create(PChar(source.Text));
    myParser.OnFoundTag := OnTag;
    myParser.OnFoundText:= OnText;
    myParser.Exec;
    myParser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-3 do
    begin
      if (Pos('<b>', parse.Strings[i])<>0) AND
         (Pos('</b>', parse.Strings[i+2])<>0) AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href=')) <> '') then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(s);
        links.Add('/'+StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i-1], 'href=')), WebsiteRoots[MANGA24H_ID,1], '', []));
      end;
    end;
    source.Free;
  end;  