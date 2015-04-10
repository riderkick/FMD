  function VnSharingGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    s: String;

  begin
    Result := INFORMATION_NOT_FOUND;
    // bad code
    if not GetPage(TObject(Source), WebsiteRoots[VNSHARING_ID, 1] +
      VNSHARING_BROWSER + '?page=' + IntToStr(StrToInt(URL) + 1), 0) then
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
      if (Pos('/Truyen/', parse.Strings[i]) > 0) and
        (GetAttributeValue(GetTagAttribute(parse.Strings[i], 'width=')) <> '') then
      begin
       { if NOT isGetNamesAndLinks then
          isGetNamesAndLinks:= TRUE
        else
        begin }
        Result := NO_ERROR;
        s := GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
        // if s <> '/Truyen/Tenki-Yohou-no-Koibito?id=506' then
        if s <> '/Truyen/Bakuman-Fantasy-Weirdos?id=6238' then
        begin
          links.Add(s);
          s := StringFilter(TrimLeft(TrimRight(parse.Strings[i + 1])));
          names.Add(HTMLEntitiesFilter(s));
        end;
      end;
    end;
    Source.Free;
  end;