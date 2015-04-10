  function GetMangaAeDirectoryPageNumber: Byte;
  var
    i, p: LongInt;
    isExtractPage: Boolean = False;
  begin
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAAE_ID, 1] + MANGAAE_BROWSER, 0) then
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
    Page := 1;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<div', parse[i]) <> 0 ) and (Pos('class="pagination', parse[i]) <> 0) then
          isExtractPage := True;
        if isExtractPage and (Pos('</div', parse[i]) <> 0) then
          Break;
        if isExtractPage and (Pos('<a', parse[i]) <> 0) then
        begin
          s := ReplaceRegExpr('^.*/page\:(\d+)$', GetVal(parse[i], 'href'), '$1', True);
          p := StrToIntDef(s, 0);
          if p > Page then
            Page := p;
        end;
      end;
    Result := NO_ERROR;
    Source.Free;
  end;
