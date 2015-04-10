  function KissMangaGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[KISSMANGA_ID, 1] +
      KISSMANGA_BROWSER + '?page=' + IntToStr(StrToInt(URL) + 1), 0) then
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
      if (Pos('/Manga/', parse.Strings[i]) > 0) and
        (Pos('title=', parse.Strings[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := StringReplace(parse.Strings[i + 3], #10, '', [rfReplaceAll]);
        s := Trim(StringReplace(s, #13, '', [rfReplaceAll]));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(
          parse.Strings[i + 2], 'href=')), WebsiteRoots[KISSMANGA_ID, 1], '', []));
      end;
    end;
    Source.Free;
  end;