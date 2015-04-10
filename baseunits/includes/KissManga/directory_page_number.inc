  function GetKissMangaDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[KISSMANGA_ID, 1] +
      KISSMANGA_BROWSER, 0) then
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
      if (Pos('&raquo; Last', parse.Strings[i]) > 0) then
      begin
        Page := StrToInt(StringReplace(
          TrimRight(TrimLeft(GetAttributeValue(GetTagAttribute(parse.Strings[i - 1], 'page=')))),
          '"', '', [rfReplaceAll]));
        Result := NO_ERROR;
        Break;
      end;
    end;
    Source.Free;
  end;
