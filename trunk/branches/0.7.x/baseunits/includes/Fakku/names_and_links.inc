function   FakkuGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    i:= StrToInt(URL);
    if i = 0 then
    begin
      if NOT GetPage(TObject(source), WebsiteRoots[FAKKU_ID,1] + FAKKU_BROWSER, 0) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
    end
    else
    begin
      if NOT GetPage(TObject(source), WebsiteRoots[FAKKU_ID,1] + FAKKU_BROWSER + '/page/' + IntToStr(i+1), 0) then
      begin
        Result:= NET_PROBLEM;
        source.Free;
        exit;
      end;
    end;
    parse.Clear;
    Parser:= TjsFastHTMLParser.Create(PChar(source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText:= OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title='))<>'') AND
         ((Pos('-english', parse.Strings[i])>0) OR
          (Pos('-japanese', parse.Strings[i])>0) OR
          (Pos('class="content-title"', parse.Strings[i])>0)) then
        // ((GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Completed') OR
        //  (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'class='))='name Ongoing')) then
      begin
        Result:= NO_ERROR;
        s:= TrimLeft(TrimRight(StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'title=')))));
        names.Add(s);
        links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href=')), WebsiteRoots[FAKKU_ID,1], '', []));
      end;
    end;
    source.Free;
  end;
  