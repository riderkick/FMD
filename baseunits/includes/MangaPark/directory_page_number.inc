  function GetMangaParkDirectoryPageNumber: Byte;
  var
    i: LongInt;
    isExtractPage: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAPARK_ID, 1] +
      MANGAPARK_BROWSER, 0) then
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
      if (not isExtractPage) and (Pos('<select', parse[i]) <> 0) and
        (Pos('this.options[this.selectedIndex].value', parse[i]) <> 0) then
        isExtractPage := True;
      if isExtractPage and (Pos('</select', parse[i]) <> 0 ) then
        Break;
      if isExtractPage and (Pos('<option', parse[i]) <> 0) then
        Inc(Page);
    end;
    Source.Free;
  end;
