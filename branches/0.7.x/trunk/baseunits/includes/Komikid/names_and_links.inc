function   KomikidGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[KOMIKID_ID,1], 0) then
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
    for i:= parse.Count-1 downto 5 do
    begin
      if (Pos('<option value="', parse.Strings[i]) > 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= '/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value='));
        links.Add(s);
      end;
    end;
    source.Free;
  end;    