  function FakkuGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    i := StrToInt(URL);
    if i = 0 then
    begin
      if not GetPage(TObject(Source), WebsiteRoots[FAKKU_ID, 1] + FAKKU_BROWSER, 0) then
      begin
        Result := NET_PROBLEM;
        Source.Free;
        Exit;
      end;
    end
    else
    begin
      if not GetPage(TObject(Source), WebsiteRoots[FAKKU_ID, 1] +
        FAKKU_BROWSER + '/page/' + IntToStr(i + 1), 0) then
      begin
        Result := NET_PROBLEM;
        Source.Free;
        Exit;
      end;
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
      if Pos('class="content-title"', parse[i]) > 0 then
      begin
        Result := NO_ERROR;
        names.Add(Trim(GetAttributeValue(GetTagAttribute(parse[i], 'title='))));
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse[i], 'href=')),
          WebsiteRoots[FAKKU_ID, 1], '', []));
      end;
    end;
    Source.Free;
  end;