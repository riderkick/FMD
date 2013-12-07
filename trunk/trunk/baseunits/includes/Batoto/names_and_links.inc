function   BatotoGetNamesAndLinks: Byte;
  var
    myParser: THTMLParser;
    i: Cardinal;
    s: String;
  begin
    Result:= INFORMATION_NOT_FOUND;
    if NOT GetPage(TObject(source), WebsiteRoots[BATOTO_ID,1] + '/comic/_/comics/?per_page=750&st=' + IntToStr(StrToInt(URL)*750), 0) then
    begin
      Result:= NET_PROBLEM;
      source.Free;
      exit;
    end;
    parse.Clear;
    myParser:= THTMLParser.Create(PChar(source.Text));
    myParser.OnFoundTag := OnTag;
    myParser.OnFoundText:= OnText;
    myParser.Exec;
    myParser.Free;
    if parse.Count=0 then
    begin
      source.Free;
      exit;
    end;
    for i:= 0 to parse.Count-1 do
    begin
      if (GetTagName(parse.Strings[i]) = 'a') AND
         (Pos('/comic/', parse.Strings[i])>0) AND
         (Pos('/comics/''', parse.Strings[i])=0) AND
         (Pos('/comics/"', parse.Strings[i])=0) AND
         (Pos('/comics/?', parse.Strings[i])=0) then
      begin
        Result:= NO_ERROR;
        s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
        if (Pos('bloody-rose-r8162', parse.Strings[i]) = 0) AND
           (Pos('dragon-and-weed-origins-outbreak-r6901', parse.Strings[i]) = 0) AND
           (Pos('dragon-and-weed-origins-the-fallen-r8180', parse.Strings[i]) = 0) then
        begin
          names.Add(s);
          s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
          links.Add(StringReplace(s, WebsiteRoots[BATOTO_ID,1], '', []));
        end;
      end;
    end;
    source.Free;
  end;   