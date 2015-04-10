  function GetNineMangaPageNumber: Boolean;
  var
    i: Cardinal;
    l: TStringList;
    isExtractPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;

    Result := GetPage(TObject(l),
      FillMangaSiteHost(manager.container.MangaSiteID, URL),
      manager.container.Manager.retryConnect);

    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if (Pos('<select', parse.Strings[i]) > 0) and
          (Pos('name="page"', parse.Strings[i]) > 0) and
          (Pos('id="page"', parse.Strings[i]) > 0) then
          isExtractPageNumber := True;
        if isExtractPageNumber and (Pos('</select', parse.Strings[i]) > 0) then
        begin
          isExtractPageNumber := False;
          Break;
        end;
        if isExtractPageNumber and (Pos('<option', parse.Strings[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
    l.Free;
  end;
