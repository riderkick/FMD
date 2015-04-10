  function EGScansGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[EGSCANS_ID, 1] +
      EGSCANS_BROWSER + '/' + IntToStr(StrToInt(URL) + 1) + '/', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;
    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count = 0 then
    begin
      Source.Free;
      Exit;
    end;
    for i := parse.Count - 1 downto 2 do
    begin
      if (Pos('<option value="', parse.Strings[i]) > 0) and
        (Pos('="0"', parse.Strings[i]) = 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 1])));
        names.Add(HTMLEntitiesFilter(s));
        s := StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i],
          'value="')),
          WebsiteRoots[S2SCAN_ID, 1], '', []);
        links.Add(s);
      end;
      if Pos('<select name="manga"', parse.Strings[i]) > 0 then
        Break;
    end;
    Source.Free;
  end;