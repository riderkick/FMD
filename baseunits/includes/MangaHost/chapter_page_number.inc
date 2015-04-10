  function GetMangaHostPageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    isGetPageNumber: Boolean = False;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    Result := GetPage(TObject(l),
      DecodeUrl(FillMangaSiteHost(MANGAHOST_ID, URL)),
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to  parse.Count - 1 do
      begin
        if (Pos('<select', parse[i]) > 0) and (Pos('class="viewerPage', parse[i]) > 0) then
          isGetPageNumber := True;
        if isGetPageNumber and (Pos('</select', parse[i]) > 0) then
          Break;
        if isGetPageNumber and (Pos('<option', parse[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
    l.Free;
  end;
