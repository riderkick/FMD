  function GetMangaFoxDirectoryPageNumber: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAFOX_ID, 1] +
      MANGAFOX_BROWSER + '?az', 0) then
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
    if parse.Count > 0 then
    begin
      for i := parse.Count - 1 downto 0 do
      begin
        if (GetTagName(parse.Strings[i]) = 'span') and
          (i <= parse.Count) and
          (Pos('span class="next"', parse.Strings[i]) <> 0) then
        begin
          s := GetString(parse.Strings[i - 6], 'href="', '.htm');
          Page := StrToInt(s);
          Result := NO_ERROR;
          Break;
        end;
      end;
    end;
    Source.Free;
  end;