  function GetMangaParkDirectoryPageNumber: Byte;
  var
    i: LongInt;
    isExtractPage: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAPARK_ID, 1] +
      '/search?orderby=add', 0) then
    begin
      Result := NET_PROBLEM;
      Source.Free;
      Exit;
    end;

    Parser := THTMLParser.Create(PChar(Source.Text));
    try
      Parser.OnFoundTag := OnTag;
      Parser.OnFoundText := OnText;
      parse.Clear;
      Parser.Exec;
    finally
      Parser.Free;
    end;

    if parse.Count > 0 then
    begin
      for i := 0 to parse.Count - 1 do
      begin
        if (GetTagName(parse[i]) = 'select') and (GetVal(parse[i], 'onchange') =
          'javascript:window.location.href=this.options[this.selectedIndex].value;') then
          isExtractPage := True;
        if isExtractPage then
        begin
          if GetTagName(parse[i]) = '/select' then
            Break
          else if GetTagName(parse[i]) = 'option' then
            Inc(Page);
        end;
      end;
    end;
    Source.Free;
  end;
