  function BatotoGetNamesAndLinks: Byte;
  var
    i: Cardinal;
    //s   : String;
    regx: TRegExpr;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[BATOTO_ID, 1] +
      BATOTO_BROWSER + '?per_page=750&st=' + IntToStr(StrToInt(URL) * 750), 0) then
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
    regx := TRegExpr.Create;
    regx.ModifierI := True;
    //regx.Expression:='^.*(/comics?/_/\w+/[^''"\?]+).*$';
    regx.Expression := '^.*(/comics?/_/(sp|comics?)/[^''"\?]+).*$';
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('<a', parse[i]) > 0) then
        if regx.Exec(Trim(parse[i])) then
        begin
          links.Add(regx.Replace(Trim(parse[i]), '$1', True));
          names.Add(Trim(StringFilter(Trim(parse[i + 1]))));
        end;
      //if (GetTagName(parse.Strings[i]) = 'a') AND
      //   (Pos('/comic/', parse.Strings[i])>0) AND
      //   (Pos('/comics/''', parse.Strings[i])=0) AND
      //   (Pos('/comics/"', parse.Strings[i])=0) AND
      //   (Pos('/comics/?', parse.Strings[i])=0) then
      //begin
      //  Result:= NO_ERROR;
      //  s:= StringFilter(TrimLeft(TrimRight(parse.Strings[i+1])));
      //  if (Pos('bloody-rose-r8162', parse.Strings[i]) = 0) AND
      //     (Pos('dragon-and-weed-origins-outbreak-r6901', parse.Strings[i]) = 0) AND
      //     (Pos('dragon-and-weed-origins-the-fallen-r8180', parse.Strings[i]) = 0) then
      //  begin
      //    names.Add(s);
      //    s:= GetAttributeValue(GetTagAttribute(parse.Strings[i], 'href='));
      //    links.Add(StringReplace(s, WebsiteRoots[BATOTO_ID,1], '', []));
      //  end;
      //end;
    end;
    regx.Free;
    Source.Free;
  end;