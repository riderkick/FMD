  function Manga24hGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGA24H_ID, 1] +
      MANGA24H_BROWSER + IntToStr(StrToInt(URL) + 1), 0) then
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
    for i := 0 to parse.Count - 3 do
    begin
      if (Pos('<b>', parse[i]) <> 0) and
        (Pos('</b>', parse[i + 2]) <> 0) and
        (GetVal(parse[i - 1], 'href') <> '') then
      begin
        Result := NO_ERROR;
        s := StringFilter(TrimLeft(TrimRight(parse[i + 1])));
        names.Add(s);
        links.Add('/' + StringReplace(GetVal(parse[i - 1], 'href'), WebsiteRoots[MANGA24H_ID, 1], '', []));
      end;
    end;
    Source.Free;
  end;