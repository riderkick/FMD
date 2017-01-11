  function GetCentralDeMangasDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[CENTRALDEMANGAS_ID, 1] +
      CENTRALDEMANGAS_BROWSER, 0) then
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
      if (Pos('/mangas/list/*/', parse[i]) > 0) then
      begin
        s := TrimRight(TrimLeft(GetString(parse[i], '/mangas/list/*/', '">')));
        APage := StrToInt(s);
        Result := NO_ERROR;
        Exit;
      end;
    end;
    Source.Free;
  end;