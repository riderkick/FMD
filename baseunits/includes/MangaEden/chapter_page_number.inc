  function GetMangaEdenPageNumber: Boolean;
  var
    s: String;
    i, j: Integer;
    l: TStringList;
  begin
    l := TStringList.Create;
    parse := TStringList.Create;
    if manager.container.MangaSiteID = MANGAEDEN_ID then
      s := FillMangaSiteHost(MANGAEDEN_ID, URL) + '1/'
    else
      s := FillMangaSiteHost(PERVEDEN_ID, URL) + '1/';
    Result := GetPage(TObject(l),
      s,
      manager.container.Manager.retryConnect);
    Parser := THTMLParser.Create(PChar(l.Text));
    Parser.OnFoundTag := OnTag;
    Parser.OnFoundText := OnText;
    Parser.Exec;
    Parser.Free;
    if parse.Count > 0 then
    begin
      manager.container.pageNumber := 0;
      for i := 0 to parse.Count - 1 do
        if (Pos('<select', parse[i]) > 0) and
          (GetVal(parse[i], 'id') = 'pageSelect') then
        begin
          for j := i to parse.Count - 1 do
          begin
            if Pos('</select', parse[j]) > 0 then
              Break
            else if Pos('<option', parse[j]) > 0 then
              Inc(manager.container.PageNumber);
          end;
          Break;
        end;
    end;
    parse.Free;
    l.Free;
  end;
