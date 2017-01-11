  function GetMyReadingMangaInfoDirectoryPageNumber: Byte;
  var
    i, j: Cardinal;
    isGetLastPage: Boolean = False;
  begin
    APage := 0;
    Result := INFORMATION_NOT_FOUND;
    if not GetPage(TObject(Source), WebsiteRoots[MYREADINGMANGAINFO_ID, 1] + '/', 0) then
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
      if Pos('class="pagination-omission"', parse[i]) > 0 then
      begin
        j := i + 1;
        while j < parse.Count - 1 do
        begin
          if (Pos('<a', parse[j]) > 0) and (Pos('/page/', parse[j]) > 0) then
          begin
            Result := NO_ERROR;
            APage := StrToIntDef(Trim(parse[j + 1]), 1);
            isGetLastPage := True;
            Break;
          end;
          if isGetLastPage then
            Break;
          Inc(j);
        end;
      end;
    end;
    Source.Free;
  end;
