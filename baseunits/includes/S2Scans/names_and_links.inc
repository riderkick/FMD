  function S2ScanGetNamesAndLinks: Byte;
  var
    i: Integer;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[S2SCAN_ID, 1] +
      '/directory/' + '/' + IntToStr(StrToInt(AURL)+1) + '/', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'div') and (GetVal(parse[i], 'class') = 'group') then
          if GetTagName(parse[i + 5]) = 'a' then
          begin
            ANames.Add(GetVal(parse[i + 5], 'title'));
            ALinks.Add(GetVal(parse[i + 5], 'href'));
          end;
      end;
    Source.Free;
  end;
