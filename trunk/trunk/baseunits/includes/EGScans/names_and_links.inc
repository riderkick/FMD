function   EGScansGetNamesAndLinks: Byte;
  var
    tmp: Integer;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[EGSCANS_ID,1] + EGSCANS_BROWSER + '/' + IntToStr(StrToInt(URL)+1) + '/', 0) then
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
      if (Pos('<option value="', parse.Strings[i]) > 0) AND
         (Pos('="0"', parse.Strings[i]) = 0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        names.Add(HTMLEntitiesFilter(s));
        s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value="')), WebsiteRoots[S2SCAN_ID,1], '', []);
        links.Add(s);
      end;
      if Pos('<select name="manga"', parse.Strings[i]) > 0 then
        break;
    end;
    source.Free;
  end;  