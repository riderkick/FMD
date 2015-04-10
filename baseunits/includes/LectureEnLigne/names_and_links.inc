  function LectureEnLigneGetNamesAndLinks: Byte;
  var
    i: Cardinal;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[LECTUREENLIGNE_ID, 1] +
      LECTUREENLIGNE_BROWSER + '&p=' + IntToStr(StrToInt(URL) + 1), 0) then
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
      {
      if (Pos('option value=', parse[i]) > 0) then
      begin
        Result:= NO_ERROR;
        names.Add(Trim(HTMLEntitiesFilter(StringFilter(parse[i+1]))));
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'value=')), WebsiteRoots[LECTUREENLIGNE_ID,1], '', []));
      end;
      }
      if (Pos('class="infoImages"', parse.Strings[i]) > 0) and
        (Pos('href="', parse.Strings[i]) > 0) then
      begin
        Result := NO_ERROR;
        names.Add(Trim(HTMLEntitiesFilter(StringFilter(parse.Strings[i + 1]))));
        links.Add('/' + GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')));
      end;
    end;
    Source.Free;
  end;