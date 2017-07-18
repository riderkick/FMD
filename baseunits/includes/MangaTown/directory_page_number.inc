  function GetMangaTownDirectoryPageNumber: Byte;
  var
    i: Cardinal;
    isExtractPage: Boolean = False;
  begin
    APage := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGATOWN_ID, 1] +
      MANGATOWN_BROWSER + '?name.az', 0) then
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
      if Pos('<select onchange="javascript:location.href=this.value;">', parse[i]) > 0 then
        isExtractPage := True;
      if isExtractPage and
        (Pos('</select', parse[i]) > 0) then
        isExtractPage := False;
      if isExtractPage and
        (Pos('<option', parse[i]) > 0) then
      begin
        Inc(APage);
        Result := NO_ERROR;
      end;
    end;
    Source.Free;
  end;
