  function HakiHomeNamesAndLinks: Byte;
  var
    i: Integer;
    s: String;
    isExtractNames: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    s := WebsiteRoots[HAKIHOME_ID, 1] + HAKIHOME_BROWSER + '/pagel/'+
      IntToStr(StrToInt(URL) + 1) + '/';
    if not GetPage(TObject(Source), s, 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Source.Text := FixHTMLTagQuote(Source.Text);
    //correcting some broken tag
    Source.Text := StringReplace(Source.Text, '>"  href=', '><a href=', [rfReplaceAll, rfIgnoreCase]);
    Source.Text := StringReplace(Source.Text, '="<', '=""><', [rfReplaceAll, rfIgnoreCase]);

    parse.Clear;
    Parser := THTMLParser.Create(PChar(Source.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    Source.Free;
    if parse.Count = 0 then
      Exit;
    for i := 0 to parse.Count - 1 do
    begin
      if (Pos('<table', parse[i]) > 0) and (Pos('class="listing"', parse[i]) > 0) then
        isExtractNames := True;
      if isExtractNames and (Pos('</table', parse[i]) > 0) then
      begin
        isExtractNames := False;
        Break;
      end;
      if i + 1 < parse.Count - 1 then
        if isExtractNames and (Pos('<a', parse[i]) > 0) and
          (Pos('class="tuade"', parse[i]) > 0) then
        begin
          Result := NO_ERROR;
          names.Add(Trim(HTMLEntitiesFilter(StringFilter(Trim(parse[i + 1])))));
          links.Add(Trim(StringReplace(GetVal(parse[i], 'href'), WebsiteRoots[HAKIHOME_ID, 1], '', [rfIgnoreCase])));
        end;
    end;
  end;
