  function GetPururinDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[PURURIN_ID, 1], 0) then
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
      if (Pos('/browse/0/11/2.html', parse.Strings[i]) > 0) then
      begin
        Page := StrToInt(Trim(parse.Strings[i - 2]));
        Break;
      end;
    end;
    Source.Free;
  end;