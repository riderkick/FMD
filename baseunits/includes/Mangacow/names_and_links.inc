  function MangaCowGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result := INFORMATION_NOT_FOUND;
    //if Trim(URL) = '0' then
    //s := ''
    //else
    s := IntToStr(StrToInt(URL) + 1) + '/';
    if not GetPage(TObject(Source), WebsiteRoots[MANGACOW_ID, 1] +
      MANGACOW_BROWSER + s, 0) then
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
      if (Pos('<a ', parse[i]) > 0) and (Pos('class="mng_det_pop"', parse[i]) > 0) then
        if Pos('<br', parse[i + 3]) > 0 then
        begin
          Result := NO_ERROR;
          s := Trim(HTMLEntitiesFilter(StringFilter(GetVal(parse[i], 'title'))));
          names.Add(S);
          s := StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[MANGACOW_ID, 1],
            '', [rfIgnoreCase]);
          links.Add(S);
        end;
      //if (Pos('class="img_wrp', parse.Strings[i]) > 0) then
      //begin
      //  Result:= NO_ERROR;
      //  s:= StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'title=')));
      //  names.Add(HTMLEntitiesFilter(s));
      //  s:= StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+1], 'href=')), WebsiteRoots[MANGACOW_ID,1], '', []);
      //  links.Add(s);
      //end;
    end;
    Source.Free;
  end;