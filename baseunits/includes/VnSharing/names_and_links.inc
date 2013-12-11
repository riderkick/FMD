function   VnSharingGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result:= INFORMATION_NOT_FOUND;
    // bad code
    if NOT GetPage(TObject(source), WebsiteRoots[VNSHARING_ID,1] + VNSHARING_BROWSER + '?page='+ IntToStr(StrToInt(URL)+1), 0) then
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
      if (Pos('/Truyen/', parse.Strings[i])>0) AND
         (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'width='))<>'') then
      begin
       { if NOT isGetNamesAndLinks then
          isGetNamesAndLinks:= TRUE
        else
        begin }
        Result:= NO_ERROR;
        s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
       // if s <> '/Truyen/Tenki-Yohou-no-Koibito?id=506' then
        if s <> '/Truyen/Bakuman-Fantasy-Weirdos?id=6238' then
        begin
          links.Add(s);
          s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    source.Free;
  end;     