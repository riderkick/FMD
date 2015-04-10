  function PururinGetNamesAndLinks: Byte;
  var
    numPage: Integer;
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    numPage := StrToIntDef(URL, 0);
    if numPage = 0 then
      s := WebsiteRoots[PURURIN_ID, 1] + '/'
    else
      s := WebsiteRoots[PURURIN_ID, 1] + PURURIN_BROWSER + '0/1' +
        IntToStr(numPage) + '/' + IntToStr(numPage + 1) + '.html';

    if not GetPage(TObject(Source), s, 0) then
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
      if (Pos('class="overlay', parse.Strings[i]) > 0) then
      begin
        Result := NO_ERROR;
        s := StringFilter(parse.Strings[i + 7]);
        names.Add(HTMLEntitiesFilter(s));
        s := StringReplace(GetAttributeValue(GetTagAttribute(
          parse.Strings[i - 2], 'href=')),
          WebsiteRoots[PURURIN_ID, 1], '', []);
        links.Add(s);
      end;
    end;
    Source.Free;
  end;