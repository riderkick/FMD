function   Hentai2ReadGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), HENTAI2READ_ROOT + HENTAI2READ_BROWSER + IntToStr(StrToInt(URL)+1) + '/', 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
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
      if Pos('class="lst-anm-ifo"', parse.Strings[i])>0 then
      begin
        begin
          Result:= NO_ERROR;
          s:= TrimLeft(TrimRight(StringFilter(GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'title=')))));
          names.Add(s);
          links.Add(StringReplace(GetAttributeValue(GetTagAttribute(parse.Strings[i+3], 'href=')), HENTAI2READ_ROOT, '', []));
        end;
      end;
    end;
    source.Free;
  end; 