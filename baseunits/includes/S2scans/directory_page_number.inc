  function GetS2scanDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[S2SCAN_ID, 1] + S2SCAN_BROWSER, 0) then
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
      if (Pos('"gbutton fright"', parse.Strings[i]) > 0) then
      begin
        s := TrimRight(TrimLeft(GetString(parse.Strings[i], '/list/', '/"')));
        Page := StrToInt(s);
        Result := NO_ERROR;
        Source.Free;
        Exit;
      end;
    end;
    Source.Free;
  end;