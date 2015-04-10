  function GetMangaFramePageNumber: Boolean;
  var
    i: Integer;
    l: TStringList;
    s: String;
    isExtractPageNumber: Boolean = False;
  begin
    manager.container.PageNumber := 0;
    l := TStringList.Create;
    parse := TStringList.Create;
    s := FillMangaSiteHost(MANGAFRAME_ID, URL);
    Result := GetPage(TObject(l), s, manager.container.Manager.retryConnect);

    l.Text := FixHTMLTagQuote(l.Text);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    l.Free;
    if parse.Count > 0 then
    begin
      manager.container.PageNumber := 0;
      for i := 0 to parse.Count - 1 do
      begin
        if Pos('class="tbtitle dropdown_parent dropdown_right"', parse[i]) > 0 then
          isExtractPageNumber := True;
        if isExtractPageNumber and (Pos('</ul', parse[i]) > 0) then
        begin
          isExtractPageNumber := False;
          Break;
        end;
        if isExtractPageNumber and (Pos('<a', parse[i]) > 0) then
          Inc(manager.container.PageNumber);
      end;
    end;
    parse.Free;
  end;
