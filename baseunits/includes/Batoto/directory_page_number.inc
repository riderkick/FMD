  function GetBatotoDirectoryPageNumber: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[BATOTO_ID, 1] +
      BATOTO_BROWSER + '?per_page=750&st=0', 0) then
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
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('Page 1 of ', parse.Strings[i]) > 0) then
      begin
        s := GetString(parse.Strings[i] + '~!@', 'Page 1 of ', '~!@');
        Page := StrToInt(TrimLeft(TrimRight(s)));
        Result := NO_ERROR;
        Source.Free;
        Exit;
      end;
    end;
    Source.Free;
  end;