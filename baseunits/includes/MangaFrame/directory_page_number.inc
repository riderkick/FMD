  function GetMangaFrameDirectoryPageNumber: Byte;
  var
    i: Integer;
  begin
    Page := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MANGAFRAME_ID, 1] + MANGAFRAME_BROWSER, 0) then
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
    Source.Free;
    if parse.Count > 0 then
      for i := 0 to parse.Count - 1 do
        if i + 1 < parse.Count - 1 then
          if (Pos('class="gbutton fright"', parse[i]) > 0) and (Pos('<a', parse[i]) > 0) and
            (Pos('Last', parse[i + 1]) > 0) then
          begin
            Page := StrToIntDef(ReplaceRegExpr('^.*/(\d+)/$', Trim(GetVal(parse[i], 'href')), '$1', True), 1);
            Break;
          end;
    Result := NO_ERROR;
  end;
